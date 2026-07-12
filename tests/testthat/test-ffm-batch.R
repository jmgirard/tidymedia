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

test_that("ffm_batch() adds no verified column on a dry run", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp4")
  res <- ffm_batch(
    jobs,
    function(input, output, ...) ffm_codec(ffm_dry(input, output), audio = "copy"),
    run = FALSE, verify = list(width = 320)
  )
  expect_false("verified" %in% names(res))
})

test_that("ffm_batch() rejects an invalid verify spec type", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp4")
  expect_error(
    ffm_batch(jobs, function(input, output, ...) ffm_dry(input, output),
              run = FALSE, verify = "width"),
    "named list or a function"
  )
})

test_that("resolve_batch_verify() replicates a list and maps a function", {
  jobs <- tibble::tibble(input = c("a", "b"), output = c("x", "y"),
                         w = c(10, 20))
  s1 <- resolve_batch_verify(list(width = 320), jobs)
  expect_length(s1, 2)
  expect_equal(s1[[1]], list(width = 320))
  s2 <- resolve_batch_verify(function(w, ...) list(width = w), jobs)
  expect_equal(s2[[1]], list(width = 10))
  expect_equal(s2[[2]], list(width = 20))
})

test_that("resolve_batch_verify() rejects an empty or unnamed spec", {
  jobs <- tibble::tibble(input = "a", output = "x")
  expect_error(
    resolve_batch_verify(function(...) list(), jobs), "non-empty named list"
  )
  expect_error(
    resolve_batch_verify(function(...) list(1, 2), jobs), "non-empty named list"
  )
})

test_that("ffm_batch() rejects an empty verify spec before running", {
  # resolved before any encode, so this aborts without needing ffmpeg
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp4")
  expect_error(
    ffm_batch(jobs, function(input, output, ...) ffm_dry(input, output),
              verify = function(...) list()),
    "non-empty named list"
  )
})

test_that("ffm_batch() rejects a non-logical progress flag", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp4")
  expect_error(
    ffm_batch(jobs, function(input, output, ...) ffm_dry(input, output),
              run = FALSE, progress = "yes"),
    "progress"
  )
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

test_that("ffm_batch(parallel = TRUE) warns when the future plan is sequential", {
  skip_if_not_installed("furrr")
  old <- future::plan(future::sequential)
  withr::defer(future::plan(old))
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp3")
  # run = FALSE (no execution) still warns: the guard fires before the run
  # branch, so the warning is independent of `run`. Muffle the benign furrr
  # global-export warning that only appears under devtools::load_all().
  withCallingHandlers(
    expect_warning(
      ffm_batch(jobs, function(input, output, ...) ffm_dry(input, output),
                run = FALSE, parallel = TRUE),
      class = "tidymedia_sequential_plan"
    ),
    warning = function(w) {
      if (grepl("may not be available", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
})

test_that("warn_if_sequential_plan() is silent under a non-sequential plan", {
  # Detection keys off the plan's class, not merely `parallel`. Stub
  # future::plan() to report a parallel strategy without spinning workers
  # (a real parallel plan would then need furrr to actually run jobs).
  local_mocked_bindings(
    plan = function(...) structure(function() NULL, class = "multisession"),
    .package = "future"
  )
  expect_no_warning(warn_if_sequential_plan())
})

test_that("ffm_batch(parallel = FALSE) never warns about the plan", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.mp3")
  expect_no_warning(
    ffm_batch(jobs, function(input, output, ...) ffm_dry(input, output),
              run = FALSE, parallel = FALSE)
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

# M08: verification wired into the batch (binary-gated) --------------------------

test_that("ffm_batch(verify =) adds a verified column without aborting", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(a, b), output = c(out_a, out_b))
  res <- ffm_batch(jobs, function(input, output, ...) {
    ffm_files(input, output) |>
      ffm_scale(width = 32, height = 32) |>
      ffm_codec(video = "libx264")
  }, verify = list(width = 32, height = 32))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
  expect_true(all(res$success))
})

test_that("ffm_batch(verify =) records FALSE for a mismatch, no abort", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = a, output = out_a)
  res <- ffm_batch(jobs, function(input, output, ...) {
    ffm_files(input, output) |>
      ffm_scale(width = 32, height = 32) |>
      ffm_codec(video = "libx264")
  }, verify = list(width = 999))
  expect_true(res$success[[1]])
  expect_false(res$verified[[1]])
})

test_that("ffm_batch(verify =) accepts a function of the job columns", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = a, output = out_a, w = 32)
  res <- ffm_batch(jobs, function(input, output, w, ...) {
    ffm_files(input, output) |>
      ffm_scale(width = w, height = w) |>
      ffm_codec(video = "libx264")
  }, verify = function(w, ...) list(width = w, height = w))
  expect_true(res$verified[[1]])
})

test_that("ffm_batch(progress = TRUE) completes without erroring", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(a, b), output = c(out_a, out_b))
  expect_no_error(
    res <- ffm_batch(jobs, function(input, output, ...) {
      ffm_files(input, output) |> ffm_scale(32, 32) |> ffm_codec(video = "libx264")
    }, progress = TRUE)
  )
  expect_true(all(res$success))
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
