# Tests for segment_videos(): a table-driven sibling of segment_video() that
# cuts segments across many input files from one jobs tibble. Command
# construction is tested purely (run = FALSE); execution and the ffm_batch
# forwarding paths (verify/manifest) are gated on the ffmpeg binary.

test_that("segment_videos() returns one command per job across multiple inputs", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input  = c(f1, f1, f2),
    output = c("a.mp4", "b.mp4", "c.mp4"),
    start  = c(0, 5, 0),
    end    = c(5, 10, 3)
  )
  res <- segment_videos(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_true("command" %in% names(res))
  # Each row's command reflects its own start/end and output (default
  # reencode = TRUE: accurate output-seek, -ss/-to after -i).
  expect_match(res$command[[1]], '-ss 0 -to 5 "a.mp4"', fixed = TRUE)
  expect_match(res$command[[2]], '-ss 5 -to 10 "b.mp4"', fixed = TRUE)
  expect_match(res$command[[3]], '-ss 0 -to 3 "c.mp4"', fixed = TRUE)
  # Each row uses its own input file.
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[3]], f2, fixed = TRUE)
})

test_that("segment_videos() default reencode = TRUE is accurate output-seek, no copy", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "clip.mp4", start = 0, end = 5)
  res <- segment_videos(jobs, run = FALSE)
  expect_match(res$command[[1]], '-ss 0 -to 5 "clip.mp4"', fixed = TRUE)
  expect_no_match(res$command[[1]], "-codec:v copy", fixed = TRUE)
})

test_that("segment_videos(reencode = FALSE) uses the fast copy path per row", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input = c(f1, f2), output = c("a.mp4", "b.mp4"),
    start = c(0, 2), end = c(5, 7)
  )
  res <- segment_videos(jobs, reencode = FALSE, run = FALSE)
  for (cmd in res$command) {
    expect_match(cmd, "-codec:v copy -codec:a copy", fixed = TRUE)
    expect_match(cmd, "-avoid_negative_ts make_zero", fixed = TRUE)
  }
  expect_match(res$command[[1]], "-ss 0 -to 5 -i", fixed = TRUE)
  expect_match(res$command[[2]], "-ss 2 -to 7 -i", fixed = TRUE)
})

test_that("segment_videos() rejects a non-data-frame jobs", {
  expect_error(segment_videos(list(input = "a"), run = FALSE), "data frame")
})

test_that("segment_videos() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character(), output = character(),
                         start = numeric(), end = numeric())
  expect_error(segment_videos(jobs, run = FALSE), "at least one row")
})

test_that("segment_videos() names the missing required column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", start = 0)  # no `end`
  expect_error(segment_videos(jobs, run = FALSE), "end")
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("segment_videos() writes segments and forwards verify (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input  = c(a, b),
    output = c(out_a, out_b),
    start  = c(0, 0),
    end    = c(1, 1)
  )
  # `verify` is a named ffm_batch argument reached only via `...`.
  res <- segment_videos(jobs, verify = list(width = 64, height = 64))
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))  # cuts keep the source 64x64
})

test_that("segment_videos() forwards manifest, read by ffm_manifest (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input  = c(a, b),
    output = c(out_a, out_b),
    start  = c(0, 0),
    end    = c(1, 1)
  )
  res <- segment_videos(jobs, manifest = TRUE)
  man <- ffm_manifest(res)
  expect_equal(nrow(man), 2)
  expect_true(all(man$output_size > 0))
})
