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
  res <- assemble_measured(list(fixture, fixture))
  m <- res$measured
  expect_s3_class(m, "tbl_df")
  expect_equal(names(m), c("measured_I", "measured_TP", "measured_LRA",
                           "measured_thresh", "offset"))
  expect_equal(nrow(m), 2)
  expect_equal(res$silent, c(FALSE, FALSE))
  # Values match M16's parser on the same fixture.
  expect_equal(m$measured_I, c(-21.85, -21.85))
  expect_equal(m$measured_TP, c(-17.71, -17.71))
  expect_equal(m$measured_LRA, c(0.00, 0.00))
  expect_equal(m$measured_thresh, c(-31.85, -31.85))
  expect_equal(m$offset, c(-0.02, -0.02))
})

# Silence handling: continue-and-mark, not abort (M18) -------------------------

test_that("assemble_measured() sets a silent row aside instead of aborting (M18)", {
  good <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  silent <- readLines(test_path("fixtures", "loudnorm-analysis-silent.txt"))
  # Row 2 is silent: no abort; the row is flagged and its measured cols are NA.
  res <- assemble_measured(list(good, silent))
  expect_equal(res$silent, c(FALSE, TRUE))
  expect_equal(res$measured$measured_I, c(-21.85, NA_real_))
  expect_true(anyNA(res$measured[2, ]))
  expect_false(anyNA(res$measured[1, ]))
})

test_that("assemble_measured() still aborts when a genuine failure accompanies silence (AC4)", {
  silent <- readLines(test_path("fixtures", "loudnorm-analysis-silent.txt"))
  # A non-zero FFmpeg exit is a genuine failure -- silence does not excuse it.
  failed <- structure("some ffmpeg error", status = 1L)
  expect_error(assemble_measured(list(silent, failed)), "2")
  # A non-silence unparseable block also still aborts (naming the bad row).
  malformed <- c(
    '\t"input_i" : "-21.85",',
    '\t"input_tp" : "n/a",',
    '\t"input_lra" : "0.00",',
    '\t"input_thresh" : "-31.85",',
    '\t"target_offset" : "-0.02"'
  )
  expect_error(assemble_measured(list(silent, malformed)), "2")
})

# Result reassembly in original row order (M18) --------------------------------

test_that("bind_two_pass_result() interleaves silent rows back in order (AC3)", {
  # jobs already augmented with measured cols (NA for the silent row 2).
  jobs <- tibble::tibble(
    input       = c("a.mp4", "b.mp4", "c.mp4"),
    output      = c("a.out", "b.out", "c.out"),
    measured_I  = c(-21.85, NA_real_, -19.4),
    measured_TP = c(-17.71, NA_real_, -3.1)
  )
  silent <- c(FALSE, TRUE, FALSE)
  # ok_res mimics run_normalize_correction() over the two non-silent rows.
  ok_res <- tibble::tibble(
    input       = c("a.mp4", "c.mp4"),
    output      = c("a.out", "c.out"),
    measured_I  = c(-21.85, -19.4),
    measured_TP = c(-17.71, -3.1),
    command     = c("cmd-a", "cmd-c"),
    success     = c(TRUE, TRUE)
  )
  res <- bind_two_pass_result(jobs, silent, ok_res, run = TRUE)
  expect_equal(nrow(res), 3)
  expect_equal(res$silent, c(FALSE, TRUE, FALSE))
  # Non-silent rows carry the correction fan-out's command/success in order.
  expect_equal(res$command, c("cmd-a", NA_character_, "cmd-c"))
  expect_equal(res$success, c(TRUE, FALSE, TRUE))
  # The silent row keeps its (NA) measured cols and gets no command.
  expect_true(is.na(res$measured_I[[2]]))
})

test_that("bind_two_pass_result() marks an all-silent batch with no correction (M18)", {
  jobs <- tibble::tibble(
    input      = c("a.mp4", "b.mp4"),
    output     = c("a.out", "b.out"),
    measured_I = c(NA_real_, NA_real_)
  )
  res <- bind_two_pass_result(jobs, c(TRUE, TRUE), ok_res = NULL, run = TRUE)
  expect_equal(res$silent, c(TRUE, TRUE))
  expect_equal(res$command, c(NA_character_, NA_character_))
  expect_equal(res$success, c(FALSE, FALSE))
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
