# Tests for the M17 two-pass batch internals: the Phase 2 correction fan-out
# (run_normalize_correction) and the Phase 1 measured-table assembly
# (assemble_measured). Both are pure -- run_normalize_correction is exercised
# with run = FALSE (no binary), and assemble_measured parses recorded stderr --
# so no ffmpeg is required. Full two-pass execution is gated in
# test-normalize-audios.R.

# Phase 2: correction fan-out --------------------------------------------------

test_that("run_normalize_correction() builds one linear correction command per row (AC2)", {
  f <- make_input()
  # A jobs table pre-augmented with fixed measured columns (as Phase 1 would),
  # plus per-row knob columns, so no binary is touched.
  jobs <- tibble::tibble(
    input           = c(f, f),
    output          = c("a.mp4", "b.mp4"),
    target_loudness = c(-23, -16),
    channels        = c(1, 2),
    sample_rate     = c(44100, 48000),
    measured_I      = c(-27.61, -19.4),
    measured_TP     = c(-9.32, -3.1),
    measured_LRA    = c(5.90, 8.0),
    measured_thresh = c(-38.06, -29.0),
    offset          = c(0.30, -0.10)
  )
  res <- run_normalize_correction(
    jobs, target_loudness = -23, true_peak = -1, loudness_range = 7,
    channels = NULL, sample_rate = NULL, run = FALSE, parallel = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  # Row 1: per-row measured values, target, and knobs thread through, with
  # linear=true and the shared pipeline's -codec:v copy preserved.
  expect_match(
    res$command[[1]],
    paste0("loudnorm=I=-23:TP=-1:LRA=7:measured_I=-27.61:measured_TP=-9.32:",
           "measured_LRA=5.9:measured_thresh=-38.06:offset=0.3:linear=true"),
    fixed = TRUE
  )
  expect_match(res$command[[1]], "-codec:v copy -ac 1 -ar 44100", fixed = TRUE)
  # Row 2: a different per-row target and measured block.
  expect_match(
    res$command[[2]],
    paste0("loudnorm=I=-16:TP=-1:LRA=7:measured_I=-19.4:measured_TP=-3.1:",
           "measured_LRA=8:measured_thresh=-29:offset=-0.1:linear=true"),
    fixed = TRUE
  )
  expect_match(res$command[[2]], "-codec:v copy -ac 2 -ar 48000", fixed = TRUE)
})

# Phase 1: measured-table assembly ---------------------------------------------

test_that("assemble_measured() populates the five measured columns per row (AC3)", {
  fixture <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  # Two good rows (the recorded real loudnorm output).
  m <- assemble_measured(list(fixture, fixture))
  expect_s3_class(m, "tbl_df")
  expect_equal(names(m), c("measured_I", "measured_TP", "measured_LRA",
                           "measured_thresh", "offset"))
  expect_equal(nrow(m), 2)
  # Values match M16's parser on the same fixture.
  expect_equal(m$measured_I, c(-21.85, -21.85))
  expect_equal(m$measured_TP, c(-17.71, -17.71))
  expect_equal(m$measured_LRA, c(0.00, 0.00))
  expect_equal(m$measured_thresh, c(-31.85, -31.85))
  expect_equal(m$offset, c(-0.02, -0.02))
})

test_that("assemble_measured() aborts naming the offending row on a malformed block (AC3)", {
  fixture <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  malformed <- c(
    '\t"input_i" : "-21.85",',
    '\t"input_tp" : "n/a",',        # not a number
    '\t"input_lra" : "0.00",',
    '\t"input_thresh" : "-31.85",',
    '\t"target_offset" : "-0.02"'
  )
  # Row 2 is malformed; the abort must name row 2.
  expect_error(
    assemble_measured(list(fixture, malformed)),
    "2"
  )
})

test_that("assemble_measured() flags a row whose analysis pass exited non-zero", {
  fixture <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  # A non-zero FFmpeg exit surfaces as a `status` attr on the captured output.
  failed <- structure("some ffmpeg error", status = 1L)
  expect_error(assemble_measured(list(fixture, failed)), "2")
})

test_that("run_normalize_correction() falls back to scalar knobs without columns", {
  f <- make_input()
  jobs <- tibble::tibble(
    input           = f,
    output          = "out.mp4",
    measured_I      = -27.61,
    measured_TP     = -9.32,
    measured_LRA    = 5.90,
    measured_thresh = -38.06,
    offset          = 0.30
  )
  res <- run_normalize_correction(
    jobs, target_loudness = -16, true_peak = -1.5, loudness_range = 11,
    channels = 1, sample_rate = NULL, run = FALSE, parallel = FALSE
  )
  # Scalar target/knobs apply to the only row (no override columns present).
  expect_match(res$command[[1]], "loudnorm=I=-16:TP=-1.5:LRA=11:measured_I=-27.61",
               fixed = TRUE)
  expect_match(res$command[[1]], "-ac 1", fixed = TRUE)
})
