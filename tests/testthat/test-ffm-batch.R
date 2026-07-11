# Tests for the ffm_batch() tibble-driven runner. Compilation/validation tests
# are CI-safe (dry-run, no binaries); execution is binary-gated at the bottom.

test_that("ffm_batch() dry-run adds a command column per job, no success", {
  jobs <- tibble::tibble(input = c("a.mp4", "b.mp4"), output = c("a.mp3", "b.mp3"))
  res <- ffm_batch(
    jobs,
    function(input, output, ...) {
      p <- ffm_dry(input, output)
      ffm_codec(p, audio = "copy")
    },
    run = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true("command" %in% names(res))
  expect_false("success" %in% names(res))
  expect_match(res$command[[1]], '-i "a.mp4" -codec:a copy "a.mp3"', fixed = TRUE)
})

test_that("ffm_batch() passes columns to .f by name and forwards ...", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp4", start = 1, end = 2)
  res <- ffm_batch(
    jobs,
    function(input, output, start, end, reencode, ...) {
      ffm_seek(ffm_dry(input, output), start = start, end = end,
               reencode = reencode)
    },
    reencode = TRUE,
    run = FALSE
  )
  expect_match(res$command[[1]], "-ss 1 -to 2", fixed = TRUE)
})

test_that("ffm_batch() rejects a non-data-frame or empty jobs table", {
  expect_error(ffm_batch(list(a = 1), function(...) NULL), "data frame")
  expect_error(
    ffm_batch(tibble::tibble(input = character()), function(...) NULL),
    "at least one row"
  )
})

test_that("ffm_batch() errors when .f does not return an ffm pipeline", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp4")
  expect_error(
    ffm_batch(jobs, function(input, output, ...) "not a pipeline", run = FALSE),
    "ffm pipeline"
  )
})

test_that("ffm_batch() runs each job and reports success (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp3")
  out_b <- withr::local_tempfile(fileext = ".mp3")
  jobs <- tibble::tibble(input = c(a, b), output = c(out_a, out_b))
  res <- ffm_batch(jobs, function(input, output, ...) {
    ffm_files(input, output) |> ffm_drop("video")
  })
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
})

# M06: safe execution with hostile paths (binary-gated) --------------------------

test_that("ffm_batch() runs jobs whose paths contain hostile characters", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  dir <- withr::local_tempdir()
  hostile_in <- file.path(dir, "job one's $x `a`.mp4")
  expect_true(file.copy(src, hostile_in))
  jobs <- tibble::tibble(
    input  = hostile_in,
    output = file.path(dir, "out one's $y `b`.mp3")
  )
  res <- ffm_batch(jobs, .f = function(input, output, ...) {
    ffm_files(input, output) |>
      ffm_drop("video") |>
      ffm_codec(audio = "libmp3lame")
  })
  expect_true(all(res$success))
  expect_true(file.exists(jobs$output))
})
