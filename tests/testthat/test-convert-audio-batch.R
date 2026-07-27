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
  # Default (audio_codec = NULL) is highest-VBR-quality, audio-only.
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

test_that("convert_audio_batch() parity holds with a pinned audio_codec", {
  f <- make_input()
  res <- convert_audio_batch(tibble::tibble(input = f, output = "out.m4a"),
                             audio_codec = "aac", run = FALSE)
  scalar <- convert_audio(f, "out.m4a", audio_codec = "aac", run = FALSE)
  expect_identical(res$command[[1]], scalar)
})

# Per-row override column ---------------------------------------------------

test_that("convert_audio_batch() honors a per-row audio_codec column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.m4a", "b.flac"),
                         audio_codec = c("aac", "flac"))
  res <- convert_audio_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
  expect_match(res$command[[2]], "-codec:a flac", fixed = TRUE)
})

test_that("convert_audio_batch() column overrides the audio_codec argument", {
  f1 <- make_input()
  f2 <- make_input()
  # The column must WIN over a non-default argument -- asserting only against a
  # NULL argument would pass even if the column were ignored (M39 lesson,
  # inverted): here the argument names flac and the column names aac.
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.m4a", "b.m4a"),
                         audio_codec = c("aac", NA))
  res <- convert_audio_batch(jobs, audio_codec = "flac", run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
  # NA is the column form of the NULL sentinel: back to the -q:a 0 default, NOT
  # the flac argument and NOT an emitted -codec:a.
  expect_match(res$command[[2]], "-q:a 0", fixed = TRUE)
  expect_no_match(res$command[[2]], "-codec:a", fixed = TRUE)
})

test_that("convert_audio_batch() accepts an all-NA (logical) audio_codec column", {
  # R types an all-NA column logical, which an is.character-only guard would
  # wrongly reject (M34 lesson). Every row falls back to the default.
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.mp3", "b.mp3"),
                         audio_codec = c(NA, NA))
  expect_true(is.logical(jobs$audio_codec))
  res <- convert_audio_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "-q:a 0", fixed = TRUE)
  expect_match(res$command[[2]], "-q:a 0", fixed = TRUE)
})

test_that("convert_audio_batch() falls back to the audio_codec argument with no column", {
  f <- make_input()
  res <- convert_audio_batch(tibble::tibble(input = f, output = "a.m4a"),
                             audio_codec = "aac", run = FALSE)
  expect_match(res$command[[1]], "-codec:a aac", fixed = TRUE)
})

# Retired `format` spelling (M40) -------------------------------------------

test_that("convert_audio_batch() aborts on the retired format argument", {
  # `...` forwards ffm_batch options and would otherwise swallow the retired
  # argument in silence, ignoring the codec the caller named (M37 lesson).
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.m4a")
  expect_error(
    convert_audio_batch(jobs, format = "aac", run = FALSE),
    "audio_codec"
  )
  expect_error(
    convert_audio_batch(jobs, format = "aac", run = FALSE),
    "argument"
  )
})

test_that("convert_audio_batch() aborts on the retired format jobs column", {
  # A stale column would otherwise fall through as one of the ignored columns.
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.m4a", format = "aac")
  expect_error(
    convert_audio_batch(jobs, run = FALSE),
    "audio_codec"
  )
  expect_error(
    convert_audio_batch(jobs, run = FALSE),
    "jobs column"
  )
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

test_that("convert_audio_batch() rejects a numeric audio_codec column", {
  # The other boundary of the all-NA-logical case above: a numeric column is
  # rejected up front rather than mid-batch (M34 lesson).
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp3", audio_codec = 1)
  expect_error(convert_audio_batch(jobs, run = FALSE), "audio_codec")
  # The hint must be true under the branch that fired it (M38 lesson): on THIS
  # verb NA selects -q:a 0, so the shared guard's default "leave the codec
  # unset" wording would contradict the verb's own docs.
  msg <- conditionMessage(
    tryCatch(convert_audio_batch(jobs, run = FALSE), error = function(e) e)
  )
  expect_match(msg, "highest-VBR-quality default", fixed = TRUE)
  expect_no_match(msg, "leave the codec unset", fixed = TRUE)
})

test_that("convert_audio_batch() rejects a non-string audio_codec argument", {
  # `audio_codec = NA` is the case that DISCRIMINATES: batch_codec_cell() maps
  # it to the NULL sentinel at the fan-out, so without the front-door check it
  # would quietly compile the default rather than erroring, and the pipeline's
  # own check_string() never sees it. Deleting that check must turn these red.
  # (`= 1` and `= c("aac","flac")` are caught by the pipeline's check_string()
  # either way, so they are not asserted here -- they would pass against the
  # pre-M40 code and so prove nothing about the new guard.)
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp3")
  expect_error(
    convert_audio_batch(jobs, audio_codec = NA, run = FALSE),
    "must be a single string"
  )
  # M37's lesson prescribes the same bad value with AND without the column
  # present: the front-door check runs unconditionally, but a guard placed after
  # the column resolution instead would pass this second case.
  with_col <- tibble::tibble(input = f, output = "a.mp3", audio_codec = "aac")
  expect_error(
    convert_audio_batch(with_col, audio_codec = NA, run = FALSE),
    "must be a single string"
  )
})

# Execution + ffm_batch forwarding (binary-gated) ---------------------------

test_that("convert_audio_batch() writes converted audio outputs (binary-gated)", {
  v1 <- make_test_video()
  v2 <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".m4a")
  out2 <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = c(v1, v2), output = c(out1, out2),
                         audio_codec = c("aac", "aac"))
  res <- convert_audio_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
})

test_that("convert_audio_batch() forwards verify (binary-gated)", {
  v <- make_test_video()
  out <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = v, output = out, audio_codec = "aac")
  res <- convert_audio_batch(jobs, verify = list(audio_codec = "aac"))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})
