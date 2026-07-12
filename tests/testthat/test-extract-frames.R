# Tests for extract_frames(): a table-driven sibling of extract_frame() that
# grabs one still image per row across many inputs. Command construction is
# tested purely (run = FALSE, no binary); the frame-path conversion and the
# ffm_batch forwarding paths (manifest) are gated on the binaries.

# Command parity + auto-naming (pure) -----------------------------------------

test_that("extract_frames() timestamp path matches extract_frame() per row", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input     = c(f1, f1, f2),
    output    = c("a.png", "b.png", "c.png"),
    timestamp = c(0.5, 1.5, 2)
  )
  res <- extract_frames(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_true("command" %in% names(res))
  # Each row's command is byte-identical to the scalar verb's command.
  expect_identical(res$command[[1]],
                   extract_frame(f1, "a.png", timestamp = 0.5, run = FALSE))
  expect_identical(res$command[[2]],
                   extract_frame(f1, "b.png", timestamp = 1.5, run = FALSE))
  expect_identical(res$command[[3]],
                   extract_frame(f2, "c.png", timestamp = 2, run = FALSE))
})

test_that("extract_frames() auto-derives per-input frame names with the image ext", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f1, f2), timestamp = c(0, 1, 0))
  res <- extract_frames(jobs, run = FALSE)
  expect_true("output" %in% names(res))
  base1 <- tools::file_path_sans_ext(f1)
  base2 <- tools::file_path_sans_ext(f2)
  # Numbering restarts per input file; extension is the image format, not .mp4.
  expect_identical(
    res$output,
    c(paste0(base1, "_1.png"), paste0(base1, "_2.png"), paste0(base2, "_1.png"))
  )
})

test_that("extract_frames(format=) controls the derived image extension", {
  f <- make_input()
  res <- extract_frames(tibble::tibble(input = f, timestamp = 0), format = "jpg",
                        run = FALSE)
  expect_match(res$output, "\\.jpg$")
})

test_that("extract_frames() keeps an explicit output column untouched", {
  f <- make_input()
  res <- extract_frames(tibble::tibble(input = f, output = "shot.png",
                                       timestamp = 0.5), run = FALSE)
  expect_identical(res$output, "shot.png")
})

# Validation (pure) -----------------------------------------------------------

test_that("extract_frames() rejects a non-data-frame jobs", {
  expect_error(extract_frames(list(input = "a", timestamp = 0), run = FALSE),
               "data frame")
})

test_that("extract_frames() rejects a zero-row jobs", {
  jobs <- tibble::tibble(input = character(), timestamp = numeric())
  expect_error(extract_frames(jobs, run = FALSE), "at least one row")
})

test_that("extract_frames() requires an input column", {
  expect_error(extract_frames(tibble::tibble(timestamp = 0), run = FALSE),
               "input")
})

test_that("extract_frames() requires exactly one of timestamp/frame", {
  f <- make_input()
  expect_error(extract_frames(tibble::tibble(input = f), run = FALSE),
               "exactly one")
  expect_error(
    extract_frames(tibble::tibble(input = f, timestamp = 0, frame = 1),
                   run = FALSE),
    "exactly one"
  )
})

test_that("extract_frames() rejects a non-numeric/character selection column", {
  f <- make_input()
  expect_error(
    extract_frames(tibble::tibble(input = f, timestamp = list(1)), run = FALSE),
    "numeric or character"
  )
})

# Edge cases (pure) -----------------------------------------------------------

test_that("extract_frames() rejects NA in the selection column", {
  f <- make_input()
  expect_error(
    extract_frames(tibble::tibble(input = c(f, f), timestamp = c(0, NA)),
                   run = FALSE),
    "NA"
  )
})

test_that("extract_frames() rejects a non-whole frame (parity with extract_frame)", {
  f <- make_input()
  expect_error(
    extract_frames(tibble::tibble(input = f, frame = 2.5), run = FALSE),
    "whole"
  )
})

test_that("extract_frames() rejects a non-finite numeric timestamp", {
  f <- make_input()
  expect_error(
    extract_frames(tibble::tibble(input = f, timestamp = Inf), run = FALSE),
    "finite"
  )
})

test_that("extract_frames() accepts a factor input column", {
  f <- make_input()
  jobs <- tibble::tibble(input = factor(f), output = "a.png", timestamp = 0.5)
  res <- extract_frames(jobs, run = FALSE)
  expect_match(res$command[[1]], f, fixed = TRUE)
})

test_that("extract_frames() handles a single-row jobs", {
  f <- make_input()
  res <- extract_frames(tibble::tibble(input = f, timestamp = 0.5), run = FALSE)
  expect_equal(nrow(res), 1)
})

test_that("extract_frames() ignores extra jobs columns (no leak into .f)", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.png", timestamp = 0.5,
                         note = "ignore me")
  res <- extract_frames(jobs, run = FALSE)
  expect_equal(nrow(res), 1)
  expect_true("command" %in% names(res))
  expect_true("note" %in% names(res))
})

# Execution + framerate conversion (binary-gated) -----------------------------

test_that("extract_frames() writes one image per row", {
  skip_if_no_ffmpeg()
  v <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".png")
  out2 <- withr::local_tempfile(fileext = ".png")
  jobs <- tibble::tibble(input = c(v, v), output = c(out1, out2),
                         timestamp = c(0.2, 0.5))
  res <- extract_frames(jobs)
  expect_true(all(file.exists(c(out1, out2))))
  expect_true(all(res$success))
})

test_that("extract_frames() frame column converts via framerate like extract_frame()", {
  skip_if_no_ffmpeg()
  skip_if_no_mediainfo()
  v <- make_test_video()
  jobs <- tibble::tibble(input = v, output = "f.png", frame = 5)
  res <- extract_frames(jobs, run = FALSE)
  expect_identical(res$command[[1]],
                   extract_frame(v, "f.png", frame = 5, run = FALSE))
})

test_that("extract_frames() forwards ffm_batch options (manifest) to the runner", {
  skip_if_no_ffmpeg()
  v <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".png")
  out2 <- withr::local_tempfile(fileext = ".png")
  jobs <- tibble::tibble(input = c(v, v), output = c(out1, out2),
                         timestamp = c(0.2, 0.5))
  res <- extract_frames(jobs, manifest = TRUE)
  expect_true("success" %in% names(res))
  expect_false(is.null(ffm_manifest(res)))
})
