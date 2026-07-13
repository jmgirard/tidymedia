# Tests for sample_frames_batch(): the table-driven sibling of sample_frames()
# that samples many videos into numbered image sequences. Command construction
# is tested purely (run = FALSE, no binary); execution and ffm_batch forwarding
# (manifest) are gated on the binaries.

# Command parity + auto-naming (pure) -----------------------------------------

test_that("sample_frames_batch() per-row command matches sample_frames()", {
  f1 <- make_input()
  f2 <- make_input()
  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  jobs <- tibble::tibble(input = c(f1, f2), outdir = c(d1, d2))
  res <- sample_frames_batch(jobs, fps = 2, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true("command" %in% names(res))
  expect_identical(res$command[[1]], sample_frames(f1, d1, fps = 2, run = FALSE))
  expect_identical(res$command[[2]], sample_frames(f2, d2, fps = 2, run = FALSE))
})

test_that("sample_frames_batch() auto-derives a per-input output directory", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2))
  res <- sample_frames_batch(jobs, fps = 1, run = FALSE)
  expect_true("outdir" %in% names(res))
  expect_identical(res$outdir, derive_frames_dir(c(f1, f2)))
  # Each command samples into its own derived directory.
  expect_match(res$command[[1]], res$outdir[[1]], fixed = TRUE)
})

test_that("sample_frames_batch() applies a scalar outdir to every row", {
  f1 <- make_input()
  f2 <- make_input()
  d <- withr::local_tempdir()
  res <- sample_frames_batch(tibble::tibble(input = c(f1, f2)), fps = 1,
                             outdir = d, run = FALSE)
  expect_identical(res$outdir, c(d, d))
})

test_that("sample_frames_batch() lets a per-row rate column override the scalar", {
  f1 <- make_input()
  f2 <- make_input()
  d <- withr::local_tempdir()
  jobs <- tibble::tibble(input = c(f1, f2), outdir = c(d, d), fps = c(2, 5))
  res <- sample_frames_batch(jobs, run = FALSE)
  expect_identical(res$command[[1]], sample_frames(f1, d, fps = 2, run = FALSE))
  expect_identical(res$command[[2]], sample_frames(f2, d, fps = 5, run = FALSE))
})

test_that("sample_frames_batch() supports a per-row interval column", {
  f <- make_input()
  d <- withr::local_tempdir()
  res <- sample_frames_batch(tibble::tibble(input = f, outdir = d,
                                            interval = c(0.5)), run = FALSE)
  expect_identical(res$command[[1]], sample_frames(f, d, fps = 2, run = FALSE))
})

# Schema parity with the plain ffm_batch path (M19) ---------------------------

test_that("sample_frames_batch() returns the same schema as a direct ffm_batch", {
  f <- make_input()
  d <- withr::local_tempdir()
  jobs <- tibble::tibble(input = f, outdir = d)
  res <- sample_frames_batch(jobs, fps = 2, run = FALSE)
  # A hand-rolled ffm_batch over the same pipeline is the canonical schema.
  ref <- ffm_batch(jobs, run = FALSE, .f = function(input, outdir, ...) {
    sample_frames_pipeline(
      input, derive_frame_pattern(input, outdir, NULL, "png"), 2
    )
  })
  expect_identical(names(res), names(ref))
  expect_identical(vapply(res, class, character(1)),
                   vapply(ref, class, character(1)))
})

# Validation (pure) -----------------------------------------------------------

test_that("sample_frames_batch() rejects a non-data-frame jobs", {
  expect_error(sample_frames_batch(list(input = "a"), fps = 1, run = FALSE),
               "data frame")
})

test_that("sample_frames_batch() rejects a zero-row jobs", {
  jobs <- tibble::tibble(input = character())
  expect_error(sample_frames_batch(jobs, fps = 1, run = FALSE),
               "at least one row")
})

test_that("sample_frames_batch() requires an input column", {
  expect_error(sample_frames_batch(tibble::tibble(x = 1), fps = 1, run = FALSE),
               "input")
})

test_that("sample_frames_batch() requires exactly one rate source", {
  f <- make_input()
  # Neither argument nor column.
  expect_error(sample_frames_batch(tibble::tibble(input = f), run = FALSE),
               "exactly one")
  # Both an fps and an interval source.
  expect_error(
    sample_frames_batch(tibble::tibble(input = f, fps = 2), interval = 1,
                        run = FALSE),
    "exactly one"
  )
})

test_that("sample_frames_batch() rejects a bad rate column type", {
  f <- make_input()
  expect_error(
    sample_frames_batch(tibble::tibble(input = f, fps = list(1)), run = FALSE),
    "numeric or character"
  )
})

test_that("sample_frames_batch() rejects NA in a rate column", {
  f <- make_input()
  expect_error(
    sample_frames_batch(tibble::tibble(input = c(f, f), fps = c(2, NA)),
                        run = FALSE),
    "NA"
  )
})

test_that("sample_frames_batch() rejects a non-image format", {
  f <- make_input()
  expect_error(
    sample_frames_batch(tibble::tibble(input = f), fps = 1, format = "mkv",
                        run = FALSE),
    "image format"
  )
})

# Edge cases (pure) -----------------------------------------------------------

test_that("sample_frames_batch() accepts a factor input column", {
  f <- make_input()
  d <- withr::local_tempdir()
  jobs <- tibble::tibble(input = factor(f), outdir = d)
  res <- sample_frames_batch(jobs, fps = 1, run = FALSE)
  expect_match(res$command[[1]], f, fixed = TRUE)
})

test_that("sample_frames_batch() ignores extra jobs columns", {
  f <- make_input()
  d <- withr::local_tempdir()
  jobs <- tibble::tibble(input = f, outdir = d, note = "ignore me")
  res <- sample_frames_batch(jobs, fps = 1, run = FALSE)
  expect_equal(nrow(res), 1)
  expect_true(all(c("command", "note") %in% names(res)))
})

# Execution + forwarding (binary-gated) ---------------------------------------

test_that("sample_frames_batch() writes a sequence per input", {
  skip_if_no_ffmpeg()
  v <- make_test_video()  # 2 s at 10 fps
  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  jobs <- tibble::tibble(input = c(v, v), outdir = c(d1, d2))
  res <- sample_frames_batch(jobs, fps = 2)
  expect_true(all(res$success))
  expect_gte(length(list.files(d1, pattern = "\\.png$")), 3)
  expect_gte(length(list.files(d2, pattern = "\\.png$")), 3)
})

test_that("sample_frames_batch() forwards ffm_batch options (manifest)", {
  skip_if_no_ffmpeg()
  v <- make_test_video()
  d <- withr::local_tempdir()
  res <- sample_frames_batch(tibble::tibble(input = v, outdir = d), fps = 2,
                             manifest = TRUE)
  expect_true("success" %in% names(res))
  expect_false(is.null(ffm_manifest(res)))
})
