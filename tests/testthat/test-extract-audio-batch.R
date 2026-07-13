# Tests for extract_audio_batch(): a table-driven sibling of extract_audio()
# that pulls the audio track out of many files from one jobs tibble. Command
# construction is tested purely (run = FALSE); execution and the ffm_batch
# forwarding paths (verify) are gated on the ffmpeg binary.

test_that("extract_audio_batch() returns one extract command per job", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.aac", "b.aac"))
  res <- extract_audio_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[1]], '"a.aac"', fixed = TRUE)
  expect_match(res$command[[1]], "-vn", fixed = TRUE)
  expect_match(res$command[[2]], f2, fixed = TRUE)
})

test_that("extract_audio_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.aac")
  res <- extract_audio_batch(jobs, run = FALSE)
  scalar <- extract_audio(f, "out.aac", run = FALSE)
  # Compile-parity via the shared extract_audio_pipeline() (M13).
  expect_identical(res$command[[1]], scalar)
})

# Per-row override column ---------------------------------------------------

test_that("extract_audio_batch() honors a per-row audio_codec column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.m4a", "b.aac"),
                         audio_codec = c("aac", "copy"))
  res <- extract_audio_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
  expect_match(res$command[[2]], "-codec:a copy", fixed = TRUE)
})

test_that("extract_audio_batch() falls back to the audio_codec argument with no column", {
  f <- make_input()
  res <- extract_audio_batch(tibble::tibble(input = f, output = "a.m4a"),
                             audio_codec = "aac", run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
})

# Return schema -------------------------------------------------------------

test_that("extract_audio_batch() adds only a command column under run = FALSE", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.aac")
  res <- extract_audio_batch(jobs, run = FALSE)
  expect_identical(setdiff(names(res), names(jobs)), "command")
  # Same added-column schema as another batch verb (M19 parity).
  strip <- strip_metadata_batch(tibble::tibble(input = f, output = "a.mp4"),
                                run = FALSE)
  expect_identical(setdiff(names(res), names(jobs)),
                   setdiff(names(strip), c("input", "output")))
})

# Front-door validation -----------------------------------------------------

test_that("extract_audio_batch() rejects a non-data-frame jobs", {
  expect_error(extract_audio_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("extract_audio_batch() rejects an empty jobs table", {
  expect_error(extract_audio_batch(tibble::tibble(input = character()), run = FALSE),
               "at least one row")
})

test_that("extract_audio_batch() names the missing input column", {
  expect_error(extract_audio_batch(tibble::tibble(output = "a.aac"), run = FALSE),
               "input")
})

test_that("extract_audio_batch() rejects an NA input", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, NA), output = c("a.aac", "b.aac"))
  expect_error(extract_audio_batch(jobs, run = FALSE), "input")
})

test_that("extract_audio_batch() requires an output column", {
  f <- make_input()
  expect_error(extract_audio_batch(tibble::tibble(input = f), run = FALSE), "output")
})

test_that("extract_audio_batch() rejects an NA output", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.aac", NA))
  expect_error(extract_audio_batch(jobs, run = FALSE), "output")
})

test_that("extract_audio_batch() rejects a duplicated explicit output (M26)", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("same.aac", "same.aac"))
  expect_error(extract_audio_batch(jobs, run = FALSE), "same output path")
})

test_that("extract_audio_batch() rejects a non-character audio_codec column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.aac", audio_codec = 1)
  expect_error(extract_audio_batch(jobs, run = FALSE), "audio_codec")
})

# Execution + ffm_batch forwarding (binary-gated) ---------------------------

test_that("extract_audio_batch() writes audio outputs (binary-gated)", {
  v1 <- make_test_video()
  v2 <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".m4a")
  out2 <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = c(v1, v2), output = c(out1, out2))
  res <- extract_audio_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
})

test_that("extract_audio_batch() forwards verify (binary-gated)", {
  v <- make_test_video()
  out <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = v, output = out)
  res <- extract_audio_batch(jobs, verify = list(audio_codec = "aac"))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})
