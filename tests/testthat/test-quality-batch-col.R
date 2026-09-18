# M136 T1: check_batch_quality(), the front-door checker for the `quality`
# argument and column of a _batch verb. Direct tests because the helper's own
# logic -- the column's type, NA as the column form of NULL (D022), the row
# locator, the copy short-circuit -- is independent of the eight verbs that
# call it (test-quality-batch.R covers those).

quality_col_jobs <- function(quality, video_codec = c("libx264", "libx264")) {
  tibble::tibble(input = c("a.mp4", "b.mp4"), output = c("a1.mp4", "b1.mp4"),
                 video_codec = video_codec, quality = quality)
}

codec_rows_of <- function(jobs, video_codec = "libx264") {
  batch_arg_rows(jobs, "video_codec", video_codec, batch_codec_cell)
}

test_that("a numeric column with NA cells passes; NA is the NULL sentinel", {
  jobs <- quality_col_jobs(c(NA, 23))
  expect_invisible(check_batch_quality(jobs, NULL, codec_rows_of(jobs), "none"))
  # An all-NA column, which R types logical, passes too.
  jobs <- quality_col_jobs(c(NA, NA))
  expect_true(is.logical(jobs$quality))
  expect_invisible(check_batch_quality(jobs, NULL, codec_rows_of(jobs), "none"))
  # A whole-batch argument, no column.
  jobs <- quality_col_jobs(c(NA, NA))
  jobs$quality <- NULL
  expect_invisible(check_batch_quality(jobs, 23, codec_rows_of(jobs), "none"))
  expect_invisible(check_batch_quality(jobs, NULL, codec_rows_of(jobs), "none"))
})

test_that("a column that is not numeric is refused, naming the column and the row", {
  f <- function(jobs) check_batch_quality(jobs, NULL, codec_rows_of(jobs), "none")
  for (bad in list(c("23", "20"), c(TRUE, FALSE), c(NA, TRUE))) {
    jobs <- quality_col_jobs(bad)
    err <- rlang::catch_cnd(f(jobs))
    expect_s3_class(err, "rlang_error")
    expect_identical(rlang::call_name(err$call), "f")
    expect_match(conditionMessage(err), "quality", fixed = TRUE)
    expect_match(conditionMessage(err), "must be numeric", fixed = TRUE)
    # The first cell that is not NA is the row named.
    expect_match(conditionMessage(err),
                 sprintf("First offending jobs row: %d.", which(!is.na(bad))[1]),
                 fixed = TRUE)
  }
})

test_that("a cell is checked against its own row's resolved encoder, and names the row", {
  f <- function(jobs, video_codec = "libx264", hardware = "none") {
    check_batch_quality(jobs, NULL, codec_rows_of(jobs, video_codec), hardware)
  }
  # Out of range for libx264 (0 to 51) on row 2 only.
  err <- rlang::catch_cnd(f(quality_col_jobs(c(23, 52))))
  expect_identical(rlang::call_name(err$call), "f")
  expect_match(conditionMessage(err), "libx264", fixed = TRUE)
  expect_match(conditionMessage(err), "First offending jobs row: 2.", fixed = TRUE)
  # The same 52 is in range for h264_videotoolbox (1 to 100): row 2 resolves
  # under the backend, so a value the software row would refuse passes here.
  expect_invisible(f(quality_col_jobs(c(23, 52)), hardware = "videotoolbox"))
  # A copy row: refused for the copy, never for a family lookup, under a
  # backend too.
  jobs <- quality_col_jobs(c(NA, 23), video_codec = c("libx264", "copy"))
  err <- rlang::catch_cnd(f(jobs))
  expect_match(conditionMessage(err), "copy", fixed = TRUE)
  expect_match(conditionMessage(err), "First offending jobs row: 2.", fixed = TRUE)
  err <- rlang::catch_cnd(f(jobs, hardware = "nvenc"))
  expect_match(conditionMessage(err), "copy", fixed = TRUE)
  # An NA codec cell with the scalar video_codec = NULL under hardware = "none":
  # no encoder to apply to.
  jobs <- quality_col_jobs(c(NA, 23), video_codec = c("libx264", NA))
  err <- rlang::catch_cnd(f(jobs, video_codec = NULL))
  expect_match(conditionMessage(err), "video_codec", fixed = TRUE)
  expect_match(conditionMessage(err), "First offending jobs row: 2.", fixed = TRUE)
  # The same NA cell under a backend resolves to the H.264 encoder and passes.
  expect_invisible(f(jobs, video_codec = NULL, hardware = "nvenc"))
})

test_that("a wrong whole-batch argument is refused with no row locator", {
  jobs <- quality_col_jobs(c(NA, NA))
  jobs$quality <- NULL
  f <- function(quality) {
    check_batch_quality(jobs, quality, codec_rows_of(jobs), "none")
  }
  for (bad in list("23", c(20, 23), NA_real_, Inf, TRUE, 52)) {
    err <- rlang::catch_cnd(f(bad))
    expect_s3_class(err, "rlang_error")
    expect_identical(rlang::call_name(err$call), "f")
    expect_match(conditionMessage(err), "quality", fixed = TRUE)
    expect_no_match(conditionMessage(err), "offending jobs row", fixed = TRUE)
  }
})
