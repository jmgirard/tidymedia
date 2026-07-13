# Tests for normalize_audio_batch(): a table-driven sibling of normalize_audio() that
# loudness-normalizes many input files from one jobs tibble. Command
# construction is tested purely (run = FALSE); execution and the ffm_batch
# forwarding paths (verify/manifest) are gated on the ffmpeg binary.

test_that("normalize_audio_batch() returns one command per job across multiple inputs", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input  = c(f1, f1, f2),
    output = c("a.mp4", "b.mp4", "c.mp4")
  )
  res <- normalize_audio_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_true("command" %in% names(res))
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[1]], '"a.mp4"', fixed = TRUE)
  expect_match(res$command[[3]], f2, fixed = TRUE)
  expect_match(res$command[[3]], '"c.mp4"', fixed = TRUE)
})

test_that("normalize_audio_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4", target_loudness = -16)
  res <- normalize_audio_batch(jobs, run = FALSE)
  scalar <- normalize_audio(f, "out.mp4", target_loudness = -16, run = FALSE)
  # Compile-parity: the shared normalize_audio_pipeline() makes the batch row's
  # command identical to the scalar call with the same parameters.
  expect_identical(res$command[[1]], scalar)
})

test_that("normalize_audio_batch() default knobs match the scalar defaults", {
  f <- make_input()
  res <- normalize_audio_batch(tibble::tibble(input = f, output = "out.mp4"),
                          run = FALSE)
  scalar <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_identical(res$command[[1]], scalar)
  # Sanity-check the default composition rides through: EBU R128 loudnorm and
  # video stream-copy.
  expect_match(res$command[[1]], 'loudnorm=I=-23:TP=-1:LRA=7', fixed = TRUE)
  expect_match(res$command[[1]], "-codec:v copy", fixed = TRUE)
})

# Single-pass characterization (guards the two_pass = FALSE default) -------

test_that("normalize_audio_batch(run = FALSE) single-pass command column is unchanged (characterization)", {
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
  res <- normalize_audio_batch(jobs, run = FALSE)
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

test_that("normalize_audio_batch() honors per-row knob columns", {
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
  res <- normalize_audio_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "loudnorm=I=-23:TP=-1:LRA=7", fixed = TRUE)
  expect_match(res$command[[1]], "-ac 1 -ar 44100", fixed = TRUE)
  expect_match(res$command[[2]], "loudnorm=I=-16:TP=-1.5:LRA=11", fixed = TRUE)
  expect_match(res$command[[2]], "-ac 2 -ar 48000", fixed = TRUE)
  expect_match(res$command[[3]], "loudnorm=I=-14:TP=-2:LRA=9", fixed = TRUE)
  expect_match(res$command[[3]], "-ac 1 -ar 22050", fixed = TRUE)
})

test_that("normalize_audio_batch() knob column overrides the scalar arg per row", {
  f <- make_input()
  # A target_loudness column is present for both rows; the scalar
  # target_loudness = -14 applies only to rows lacking the column (none here),
  # so the column wins.
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    target_loudness = c(-23, -16)
  )
  res <- normalize_audio_batch(jobs, target_loudness = -14, run = FALSE)
  expect_match(res$command[[1]], "loudnorm=I=-23:", fixed = TRUE)
  expect_match(res$command[[2]], "loudnorm=I=-16:", fixed = TRUE)
  expect_no_match(res$command[[1]], "loudnorm=I=-14:", fixed = TRUE)
})

test_that("normalize_audio_batch() scalar arg applies to every row without a column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- normalize_audio_batch(jobs, target_loudness = -16, channels = 1,
                          run = FALSE)
  for (cmd in res$command) {
    expect_match(cmd, "loudnorm=I=-16:", fixed = TRUE)
    expect_match(cmd, "-ac 1", fixed = TRUE)
  }
})

# Optional `output`: auto-naming + collision ------------------------------

test_that("normalize_audio_batch() auto-names outputs when the column is absent", {
  f1 <- make_input()
  f2 <- make_input()
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(input = c(f1, f2))
  res <- normalize_audio_batch(jobs, run = FALSE)
  # One-input -> one-output: <base>_normalized.<input-ext>, carried on the
  # returned tibble and used in the command.
  expect_equal(res$output, c(paste0(b1, "_normalized.mp4"),
                             paste0(b2, "_normalized.mp4")))
  expect_match(res$command[[1]], paste0('"', b1, '_normalized.mp4"'),
               fixed = TRUE)
})

test_that("normalize_audio_batch() keeps the input extension when auto-naming", {
  f <- make_input(ext = "mkv")
  base <- tools::file_path_sans_ext(f)
  res <- normalize_audio_batch(tibble::tibble(input = f), run = FALSE)
  expect_equal(res$output, paste0(base, "_normalized.mkv"))
})

test_that("normalize_audio_batch() aborts on a duplicated input with no output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f))  # same input twice, would collide
  expect_error(normalize_audio_batch(jobs, run = FALSE), "[Dd]uplicated")
})

test_that("normalize_audio_batch() allows a duplicated input with an explicit output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- normalize_audio_batch(jobs, run = FALSE)
  expect_equal(res$output, c("a.mp4", "b.mp4"))
})

# Front-door validation ---------------------------------------------------

test_that("normalize_audio_batch() rejects a non-data-frame jobs", {
  expect_error(normalize_audio_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("normalize_audio_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character())
  expect_error(normalize_audio_batch(jobs, run = FALSE), "at least one row")
})

test_that("normalize_audio_batch() names the missing input column", {
  jobs <- tibble::tibble(output = "a.mp4")
  expect_error(normalize_audio_batch(jobs, run = FALSE), "input")
})

test_that("normalize_audio_batch() rejects a non-numeric knob column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", target_loudness = "loud")
  expect_error(normalize_audio_batch(jobs, run = FALSE), "target_loudness")
})

test_that("normalize_audio_batch() rejects an NA in a knob column at the front door", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), channels = c(1, NA)
  )
  expect_error(normalize_audio_batch(jobs, run = FALSE), "channels")
})

# Per-element value validation parity (inherited from the shared pipeline) -

test_that("normalize_audio_batch() rejects an out-of-range loudness value per row (parity)", {
  f <- make_input()
  # -3 LUFS is outside the scalar verb's -70..-5 range; the shared pipeline's
  # ffm_loudnorm() guard must reject it for the batch sibling too (M11 parity).
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    target_loudness = c(-23, -3)
  )
  expect_error(normalize_audio_batch(jobs, run = FALSE))
})

test_that("normalize_audio_batch() rejects a non-whole channels value per row (parity)", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), channels = c(1, 1.5)
  )
  expect_error(normalize_audio_batch(jobs, run = FALSE))
})

# ffm_batch forwarding ----------------------------------------------------

test_that("normalize_audio_batch() forwards batch params after ... without leaking into .f", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  # `progress` is a named ffm_batch argument reached only via `...`; it must not
  # reach normalize_audio_pipeline() (which has no such parameter). A clean
  # compile proves the forwarding boundary holds (M09 lesson).
  res <- normalize_audio_batch(jobs, run = FALSE, progress = FALSE)
  expect_equal(nrow(res), 2)
  expect_true("command" %in% names(res))
})

# Two-pass fail-fast validation (pure: aborts before any analysis pass) ----

test_that("normalize_audio_batch(two_pass) rejects a fractional scalar channels before running FFmpeg", {
  f <- make_input()
  # A bad scalar channels must fail up front (before Phase 1 wastes an analysis
  # pass per row), so this needs no ffmpeg binary.
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  expect_error(
    normalize_audio_batch(jobs, two_pass = TRUE, channels = 1.5),
    "channels|whole"
  )
})

test_that("normalize_audio_batch(two_pass) rejects a fractional channels column before running FFmpeg", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), channels = c(1, 1.5)
  )
  expect_error(normalize_audio_batch(jobs, two_pass = TRUE), "channels|whole")
})

# Two-pass front door (binary-gated) --------------------------------------

test_that("normalize_audio_batch(two_pass, run = FALSE) runs analysis, returns correction cmds, writes nothing (AC4)", {
  skip_if_no_ffmpeg()
  src <- system.file("extdata", "sample.mp4", package = "tidymedia")
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input           = c(src, src),
    output          = c(out_a, out_b),
    target_loudness = c(-23, -16)
  )
  res <- normalize_audio_batch(jobs, two_pass = TRUE, run = FALSE)
  # Phase 1 ran: the five measured columns are populated (FFmpeg-arg names) and
  # the correction commands carry the measured values + linear=true.
  measured_cols <- c("measured_I", "measured_TP", "measured_LRA",
                     "measured_thresh", "offset")
  expect_true(all(measured_cols %in% names(res)))
  expect_false(anyNA(res$measured_I))
  expect_match(res$command[[1]], "measured_I=", fixed = TRUE)
  expect_match(res$command[[1]], "linear=true", fixed = TRUE)
  expect_match(res$command[[1]], "loudnorm=I=-23:", fixed = TRUE)
  expect_match(res$command[[2]], "loudnorm=I=-16:", fixed = TRUE)
  # run = FALSE gates only Phase 2: no correction executed, so no `success`
  # column and no output files written.
  expect_false("success" %in% names(res))
  expect_false(file.exists(out_a))
  expect_false(file.exists(out_b))
})

test_that("normalize_audio_batch(two_pass = TRUE) hits each per-row target within +/-1 LU (AC5)", {
  # The whole flow (Phase 1 analysis, correction pass, and the re-probe via
  # run_loudnorm_analysis) needs ffmpeg, not ffprobe.
  skip_if_no_ffmpeg()
  src <- system.file("extdata", "sample.mp4", package = "tidymedia")
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  # Two rows, two different per-row targets: the full two-phase fan-out measures
  # each input, then builds and runs a linear correction per row.
  jobs <- tibble::tibble(
    input           = c(src, src),
    output          = c(out_a, out_b),
    target_loudness = c(-23, -16)
  )
  res <- normalize_audio_batch(jobs, two_pass = TRUE)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  # Re-probe each output's integrated loudness with a fresh analysis pass (its
  # input_i is the output's measured loudness) and assert it lands within +/-1 LU
  # of that row's target. Source: EBU R 128 (2014); ITU-R BS.1770-4.
  for (i in seq_len(nrow(res))) {
    loud <- run_loudnorm_analysis(
      res$output[[i]], target_loudness = jobs$target_loudness[[i]]
    )$i
    expect_lt(abs(loud - jobs$target_loudness[[i]]), 1)
  }
})

test_that("normalize_audio_batch(two_pass = TRUE) marks silent rows and normalizes the rest (M18)", {
  skip_if_no_ffmpeg()
  real <- make_dynamic_audio()   # a real, non-silent clip
  silent <- make_silent_audio()  # digital silence (input_i = -inf)
  out_real <- withr::local_tempfile(fileext = ".mp4")
  out_silent <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input  = c(real, silent),
    output = c(out_real, out_silent)
  )
  # The silent row must not abort the batch; it is marked and the real row is
  # still normalized. A single warning names the silent row.
  expect_warning(
    res <- normalize_audio_batch(jobs, two_pass = TRUE),
    "silent"
  )
  # A silent column marks row 2; row 1 succeeded, row 2 did not.
  expect_equal(res$silent, c(FALSE, TRUE))
  expect_equal(res$success, c(TRUE, FALSE))
  # The non-silent row wrote a real output; the silent row wrote none.
  expect_true(file.exists(out_real))
  expect_gt(file.info(out_real)$size, 0)
  expect_false(file.exists(out_silent))
  # The silent row carries no correction command and NA measurements.
  expect_true(is.na(res$command[[2]]))
  expect_true(is.na(res$measured_I[[2]]))
  expect_false(is.na(res$measured_I[[1]]))
})

test_that("normalize_audio_batch(two_pass = TRUE) survives 2+ silent rows and keeps manifest one-per-job (M18)", {
  skip_if_no_ffmpeg()
  real <- make_dynamic_audio()
  s1 <- make_silent_audio()
  s2 <- make_silent_audio()
  out_real <- withr::local_tempfile(fileext = ".mp4")
  out_s1 <- withr::local_tempfile(fileext = ".mp4")
  out_s2 <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input  = c(s1, real, s2),
    output = c(out_s1, out_real, out_s2)
  )
  # Two silent rows must not crash the warning (regression: a scalar/vector cli
  # pluralization mix threw `length(object) == 1`), and the manifest must stay
  # one-row-per-job across the skipped rows.
  expect_warning(
    res <- normalize_audio_batch(jobs, two_pass = TRUE, manifest = TRUE),
    "silent"
  )
  expect_equal(res$silent, c(TRUE, FALSE, TRUE))
  expect_equal(res$success, c(FALSE, TRUE, FALSE))
  # Manifest: one row per job, aligned, silent inputs recorded with NA command.
  man <- ffm_manifest(res)
  expect_equal(nrow(man), nrow(res))
  expect_equal(man$input, jobs$input)
  expect_true(all(is.na(man$command[c(1, 3)])))
  expect_false(is.na(man$command[[2]]))
})

test_that("normalize_audio_batch(two_pass = TRUE) keeps the verify/manifest schema when every row is silent (M19)", {
  skip_if_no_ffmpeg()
  real <- make_dynamic_audio()   # a real, non-silent clip for the mixed baseline
  s1 <- make_silent_audio()
  s2 <- make_silent_audio()

  o1 <- withr::local_tempfile(fileext = ".mp4")
  o2 <- withr::local_tempfile(fileext = ".mp4")
  o3 <- withr::local_tempfile(fileext = ".mp4")
  o4 <- withr::local_tempfile(fileext = ".mp4")
  spec <- list(audio_codec = "aac")

  # All-silent batch with both opt-ins requested.
  all_silent <- tibble::tibble(input = c(s1, s2), output = c(o1, o2))
  expect_warning(
    res <- normalize_audio_batch(
      all_silent, two_pass = TRUE, verify = spec, manifest = TRUE,
      checksums = TRUE
    ),
    "silent"
  )
  # Mixed batch (same opt-ins) as the schema baseline.
  mixed <- tibble::tibble(input = c(real, s1), output = c(o3, o4))
  expect_warning(
    ref <- normalize_audio_batch(
      mixed, two_pass = TRUE, verify = spec, manifest = TRUE, checksums = TRUE
    ),
    "silent"
  )

  # AC1: the all-silent result carries the same columns as the mixed one, with a
  # logical `verified` that is all NA (nothing ran to verify).
  expect_identical(names(res), names(ref))
  expect_true(is.logical(res$verified))
  expect_true(all(is.na(res$verified)))

  # AC2/AC3: a manifest is still attached, one row per job, same columns as the
  # mixed batch's manifest (including the checksum columns), inputs recorded.
  man <- ffm_manifest(res)
  expect_identical(names(man), names(ffm_manifest(ref)))
  expect_equal(nrow(man), 2L)
  expect_equal(man$input, all_silent$input)
  expect_true(all(is.na(man$command)))
  expect_true(all(c("input_md5", "output_md5") %in% names(man)))
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("normalize_audio_batch() writes non-empty, audio-decodable outputs (binary-gated)", {
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
  res <- normalize_audio_batch(jobs)
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

test_that("normalize_audio_batch() forwards verify (binary-gated)", {
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
  res <- normalize_audio_batch(jobs, verify = list(sample_rate = 48000))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})
