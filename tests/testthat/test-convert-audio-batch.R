# Tests for convert_audio_batch(): a table-driven sibling of convert_audio()
# that extracts/transcodes the audio of many files from one jobs tibble. Command
# construction is tested purely (run = FALSE); execution and verify forwarding
# are gated on the ffmpeg binary.

test_that("convert_audio_batch() returns one convert command per job", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.mp3", "b.mp3"))
  res <- convert_audio_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  # Default (format = NULL) is highest-VBR-quality, audio-only.
  expect_match(res$command[[1]], "-q:a 0 -map a", fixed = TRUE)
  expect_match(res$command[[1]], '"a.mp3"', fixed = TRUE)
})

test_that("convert_audio_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp3")
  res <- convert_audio_batch(jobs, run = FALSE)
  scalar <- convert_audio(f, "out.mp3", run = FALSE)
  expect_identical(res$command[[1]], scalar)
})

test_that("convert_audio_batch() parity holds with a pinned format", {
  f <- make_input()
  res <- convert_audio_batch(tibble::tibble(input = f, output = "out.m4a"),
                             format = "aac", run = FALSE)
  scalar <- convert_audio(f, "out.m4a", format = "aac", run = FALSE)
  expect_identical(res$command[[1]], scalar)
})

# Per-row override column ---------------------------------------------------

test_that("convert_audio_batch() honors a per-row format column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.m4a", "b.flac"),
                         format = c("aac", "flac"))
  res <- convert_audio_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
  expect_match(res$command[[2]], "-codec:a flac", fixed = TRUE)
})

test_that("convert_audio_batch() falls back to the format argument with no column", {
  f <- make_input()
  res <- convert_audio_batch(tibble::tibble(input = f, output = "a.m4a"),
                             format = "aac", run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
})

# Return schema -------------------------------------------------------------

test_that("convert_audio_batch() adds only a command column under run = FALSE", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp3")
  res <- convert_audio_batch(jobs, run = FALSE)
  expect_identical(setdiff(names(res), names(jobs)), "command")
})

# Front-door validation -----------------------------------------------------

test_that("convert_audio_batch() rejects a non-data-frame jobs", {
  expect_error(convert_audio_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("convert_audio_batch() rejects an empty jobs table", {
  expect_error(convert_audio_batch(tibble::tibble(input = character()), run = FALSE),
               "at least one row")
})

test_that("convert_audio_batch() names the missing input column", {
  expect_error(convert_audio_batch(tibble::tibble(output = "a.mp3"), run = FALSE),
               "input")
})

test_that("convert_audio_batch() rejects an NA input", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, NA), output = c("a.mp3", "b.mp3"))
  expect_error(convert_audio_batch(jobs, run = FALSE), "input")
})

test_that("convert_audio_batch() requires an output column", {
  f <- make_input()
  expect_error(convert_audio_batch(tibble::tibble(input = f), run = FALSE), "output")
})

test_that("convert_audio_batch() rejects a duplicated explicit output (M26)", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("same.mp3", "same.mp3"))
  expect_error(convert_audio_batch(jobs, run = FALSE), "same output path")
})

test_that("convert_audio_batch() rejects a non-character format column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp3", format = 1)
  expect_error(convert_audio_batch(jobs, run = FALSE), "format")
})

# Execution + ffm_batch forwarding (binary-gated) ---------------------------

test_that("convert_audio_batch() writes converted audio outputs (binary-gated)", {
  v1 <- make_test_video()
  v2 <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".m4a")
  out2 <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = c(v1, v2), output = c(out1, out2),
                         format = c("aac", "aac"))
  res <- convert_audio_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
})

test_that("convert_audio_batch() forwards verify (binary-gated)", {
  v <- make_test_video()
  out <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = v, output = out, format = "aac")
  res <- convert_audio_batch(jobs, verify = list(audio_codec = "aac"))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})
