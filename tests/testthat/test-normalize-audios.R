# Tests for normalize_audios(): a table-driven sibling of normalize_audio() that
# loudness-normalizes many input files from one jobs tibble. Command
# construction is tested purely (run = FALSE); execution and the ffm_batch
# forwarding paths (verify/manifest) are gated on the ffmpeg binary.

test_that("normalize_audios() returns one command per job across multiple inputs", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input  = c(f1, f1, f2),
    output = c("a.mp4", "b.mp4", "c.mp4")
  )
  res <- normalize_audios(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_true("command" %in% names(res))
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[1]], '"a.mp4"', fixed = TRUE)
  expect_match(res$command[[3]], f2, fixed = TRUE)
  expect_match(res$command[[3]], '"c.mp4"', fixed = TRUE)
})

test_that("normalize_audios() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4", target_loudness = -16)
  res <- normalize_audios(jobs, run = FALSE)
  scalar <- normalize_audio(f, "out.mp4", target_loudness = -16, run = FALSE)
  # Compile-parity: the shared normalize_audio_pipeline() makes the batch row's
  # command identical to the scalar call with the same parameters.
  expect_identical(res$command[[1]], scalar)
})

test_that("normalize_audios() default knobs match the scalar defaults", {
  f <- make_input()
  res <- normalize_audios(tibble::tibble(input = f, output = "out.mp4"),
                          run = FALSE)
  scalar <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_identical(res$command[[1]], scalar)
  # Sanity-check the default composition rides through: EBU R128 loudnorm and
  # video stream-copy.
  expect_match(res$command[[1]], 'loudnorm=I=-23:TP=-1:LRA=7', fixed = TRUE)
  expect_match(res$command[[1]], "-codec:v copy", fixed = TRUE)
})

# Single-pass characterization (guards the two_pass = FALSE default) -------

test_that("normalize_audios(run = FALSE) single-pass command column is unchanged (characterization)", {
  # Pin the exact single-pass command for every knob so the two_pass = FALSE
  # default is provably byte-for-byte unchanged as the two-pass path is added
  # (AC1). Scrub the temp input path to a stable token for a deterministic pin.
  f <- make_input()
  jobs <- tibble::tibble(
    input           = c(f, f),
    output          = c("a.mp4", "b.mp4"),
    target_loudness = c(-23, -16),
    true_peak       = c(-1, -1.5),
    loudness_range  = c(7, 11),
    channels        = c(2, 1),
    sample_rate     = c(48000, 44100)
  )
  res <- normalize_audios(jobs, run = FALSE)
  scrub <- function(cmd) gsub(f, "<in>", cmd, fixed = TRUE)
  expect_equal(
    scrub(res$command[[1]]),
    paste0('-y -i "<in>" -af "loudnorm=I=-23:TP=-1:LRA=7" ',
           '-codec:v copy -ac 2 -ar 48000 "a.mp4"')
  )
  expect_equal(
    scrub(res$command[[2]]),
    paste0('-y -i "<in>" -af "loudnorm=I=-16:TP=-1.5:LRA=11" ',
           '-codec:v copy -ac 1 -ar 44100 "b.mp4"')
  )
})

# Per-row override columns ------------------------------------------------

test_that("normalize_audios() honors per-row knob columns", {
  f <- make_input()
  jobs <- tibble::tibble(
    input          = c(f, f, f),
    output         = c("a.mp4", "b.mp4", "c.mp4"),
    target_loudness = c(-23, -16, -14),
    true_peak      = c(-1, -1.5, -2),
    loudness_range = c(7, 11, 9),
    channels       = c(1, 2, 1),
    sample_rate    = c(44100, 48000, 22050)
  )
  res <- normalize_audios(jobs, run = FALSE)
  expect_match(res$command[[1]], "loudnorm=I=-23:TP=-1:LRA=7", fixed = TRUE)
  expect_match(res$command[[1]], "-ac 1 -ar 44100", fixed = TRUE)
  expect_match(res$command[[2]], "loudnorm=I=-16:TP=-1.5:LRA=11", fixed = TRUE)
  expect_match(res$command[[2]], "-ac 2 -ar 48000", fixed = TRUE)
  expect_match(res$command[[3]], "loudnorm=I=-14:TP=-2:LRA=9", fixed = TRUE)
  expect_match(res$command[[3]], "-ac 1 -ar 22050", fixed = TRUE)
})

test_that("normalize_audios() knob column overrides the scalar arg per row", {
  f <- make_input()
  # A target_loudness column is present for both rows; the scalar
  # target_loudness = -14 applies only to rows lacking the column (none here),
  # so the column wins.
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    target_loudness = c(-23, -16)
  )
  res <- normalize_audios(jobs, target_loudness = -14, run = FALSE)
  expect_match(res$command[[1]], "loudnorm=I=-23:", fixed = TRUE)
  expect_match(res$command[[2]], "loudnorm=I=-16:", fixed = TRUE)
  expect_no_match(res$command[[1]], "loudnorm=I=-14:", fixed = TRUE)
})

test_that("normalize_audios() scalar arg applies to every row without a column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- normalize_audios(jobs, target_loudness = -16, channels = 1,
                          run = FALSE)
  for (cmd in res$command) {
    expect_match(cmd, "loudnorm=I=-16:", fixed = TRUE)
    expect_match(cmd, "-ac 1", fixed = TRUE)
  }
})

# Optional `output`: auto-naming + collision ------------------------------

test_that("normalize_audios() auto-names outputs when the column is absent", {
  f1 <- make_input()
  f2 <- make_input()
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(input = c(f1, f2))
  res <- normalize_audios(jobs, run = FALSE)
  # One-input -> one-output: <base>_normalized.<input-ext>, carried on the
  # returned tibble and used in the command.
  expect_equal(res$output, c(paste0(b1, "_normalized.mp4"),
                             paste0(b2, "_normalized.mp4")))
  expect_match(res$command[[1]], paste0('"', b1, '_normalized.mp4"'),
               fixed = TRUE)
})

test_that("normalize_audios() keeps the input extension when auto-naming", {
  f <- make_input(ext = "mkv")
  base <- tools::file_path_sans_ext(f)
  res <- normalize_audios(tibble::tibble(input = f), run = FALSE)
  expect_equal(res$output, paste0(base, "_normalized.mkv"))
})

test_that("normalize_audios() aborts on a duplicated input with no output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f))  # same input twice, would collide
  expect_error(normalize_audios(jobs, run = FALSE), "[Dd]uplicated")
})

test_that("normalize_audios() allows a duplicated input with an explicit output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- normalize_audios(jobs, run = FALSE)
  expect_equal(res$output, c("a.mp4", "b.mp4"))
})

# Front-door validation ---------------------------------------------------

test_that("normalize_audios() rejects a non-data-frame jobs", {
  expect_error(normalize_audios(list(input = "a"), run = FALSE), "data frame")
})

test_that("normalize_audios() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character())
  expect_error(normalize_audios(jobs, run = FALSE), "at least one row")
})

test_that("normalize_audios() names the missing input column", {
  jobs <- tibble::tibble(output = "a.mp4")
  expect_error(normalize_audios(jobs, run = FALSE), "input")
})

test_that("normalize_audios() rejects a non-numeric knob column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", target_loudness = "loud")
  expect_error(normalize_audios(jobs, run = FALSE), "target_loudness")
})

test_that("normalize_audios() rejects an NA in a knob column at the front door", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), channels = c(1, NA)
  )
  expect_error(normalize_audios(jobs, run = FALSE), "channels")
})

# Per-element value validation parity (inherited from the shared pipeline) -

test_that("normalize_audios() rejects an out-of-range loudness value per row (parity)", {
  f <- make_input()
  # -3 LUFS is outside the scalar verb's -70..-5 range; the shared pipeline's
  # ffm_loudnorm() guard must reject it for the batch sibling too (M11 parity).
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    target_loudness = c(-23, -3)
  )
  expect_error(normalize_audios(jobs, run = FALSE))
})

test_that("normalize_audios() rejects a non-whole channels value per row (parity)", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), channels = c(1, 1.5)
  )
  expect_error(normalize_audios(jobs, run = FALSE))
})

# ffm_batch forwarding ----------------------------------------------------

test_that("normalize_audios() forwards batch params after ... without leaking into .f", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  # `progress` is a named ffm_batch argument reached only via `...`; it must not
  # reach normalize_audio_pipeline() (which has no such parameter). A clean
  # compile proves the forwarding boundary holds (M09 lesson).
  res <- normalize_audios(jobs, run = FALSE, progress = FALSE)
  expect_equal(nrow(res), 2)
  expect_true("command" %in% names(res))
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("normalize_audios() writes non-empty, audio-decodable outputs (binary-gated)", {
  skip_if_no_ffprobe()
  a <- make_test_video()  # skips if ffmpeg absent; has a 440 Hz sine track
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input       = c(a, b),
    output      = c(out_a, out_b),
    channels    = c(1, 1),
    sample_rate = c(22050, 44100)
  )
  res <- normalize_audios(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  # Each output carries a decodable audio stream at its pinned sample rate.
  for (i in seq_len(nrow(res))) {
    probe <- ffprobe(sprintf(
      paste('-v error -select_streams a:0',
            '-show_entries stream=codec_type,sample_rate -of csv=p=0 "%s"'),
      res$output[[i]]
    ))
    expect_match(probe[[1]], "audio")
  }
  expect_match(ffprobe(sprintf(
    '-v error -select_streams a:0 -show_entries stream=sample_rate -of csv=p=0 "%s"',
    out_a
  ))[[1]], "22050")
})

test_that("normalize_audios() forwards verify (binary-gated)", {
  skip_if_no_ffprobe()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input       = c(a, b),
    output      = c(out_a, out_b),
    sample_rate = c(48000, 48000)
  )
  # `verify` is a named ffm_batch argument reached only via `...`.
  res <- normalize_audios(jobs, verify = list(sample_rate = 48000))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})
