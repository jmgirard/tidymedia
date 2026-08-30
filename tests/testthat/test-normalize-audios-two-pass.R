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
  # linear=true and the shared pipeline's shaping knobs preserved (the
  # -codec:v copy this once checked went away with D030's audio-only contract).
  expect_match(
    res$command[[1]],
    paste0("loudnorm=I=-23:TP=-1:LRA=7:measured_I=-27.61:measured_TP=-9.32:",
           "measured_LRA=5.9:measured_thresh=-38.06:offset=0.3:linear=true"),
    fixed = TRUE
  )
  expect_match(res$command[[1]], "-ac 1 -ar 44100 -map \"0:a:0\"", fixed = TRUE)
  # Row 2: a different per-row target and measured block.
  expect_match(
    res$command[[2]],
    paste0("loudnorm=I=-16:TP=-1:LRA=7:measured_I=-19.4:measured_TP=-3.1:",
           "measured_LRA=8:measured_thresh=-29:offset=-0.1:linear=true"),
    fixed = TRUE
  )
  expect_match(res$command[[2]], "-ac 2 -ar 48000 -map \"0:a:0\"", fixed = TRUE)
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

test_that("assemble_measured() aborts cleanly (no cli crash) with 2+ failing rows (AC4)", {
  # Regression: the failure abort mixed a scalar count with a vector in one cli
  # message, which threw `length(object) == 1` for 2+ bad rows (M18 review).
  failed <- structure("some ffmpeg error", status = 1L)
  err <- tryCatch(assemble_measured(list(failed, failed)),
                  error = function(e) conditionMessage(e))
  expect_no_match(err, "length\\(object\\)")
  expect_no_match(err, "Could not evaluate")
  expect_match(err, "1")   # both offending rows are named
  expect_match(err, "2")
})

test_that("bind_two_pass_result() expands the manifest to one row per job (M18)", {
  # Regression: a manifest built over only the non-silent rows was attached to
  # the full-row result, breaking ffm_manifest()'s one-row-per-job contract
  # (D011). It must be padded back to full row order (M18 review).
  jobs <- tibble::tibble(
    input  = c("a.mp4", "b.mp4", "c.mp4"),
    output = c("a.out", "b.out", "c.out")
  )
  silent <- c(FALSE, TRUE, FALSE)
  ok_res <- tibble::tibble(
    input   = c("a.mp4", "c.mp4"),
    output  = c("a.out", "c.out"),
    command = c("cmd-a", "cmd-c"),
    success = c(TRUE, TRUE)
  )
  attr(ok_res, "manifest") <- tibble::tibble(
    command = c("cmd-a", "cmd-c"),
    input = c("a.mp4", "c.mp4"), output_size = c(10, 20)
  )
  res <- bind_two_pass_result(jobs, silent, ok_res, run = TRUE)
  man <- attr(res, "manifest")
  # One row per job, aligned to the result, with the silent input recorded.
  expect_equal(nrow(man), nrow(res))
  expect_equal(man$input, c("a.mp4", "b.mp4", "c.mp4"))
  expect_true(is.na(man$command[[2]]))
  expect_true(is.na(man$output_size[[2]]))
  expect_equal(man$command[c(1, 3)], c("cmd-a", "cmd-c"))
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
  # No verify/manifest requested: neither opt-in output is synthesized.
  expect_false("verified" %in% names(res))
  expect_null(attr(res, "manifest"))
})

test_that("bind_two_pass_result() adds an all-NA verified column for an all-silent verify batch (AC1)", {
  jobs <- tibble::tibble(
    input      = c("a.mp4", "b.mp4"),
    output     = c("a.out", "b.out"),
    measured_I = c(NA_real_, NA_real_)
  )
  res <- bind_two_pass_result(
    jobs, c(TRUE, TRUE), ok_res = NULL, run = TRUE, verify = TRUE
  )
  # Same shape a mixed batch gives silent rows: a logical `verified`, all NA,
  # ordered after `success`.
  expect_true(is.logical(res$verified))
  expect_true(all(is.na(res$verified)))
  expect_equal(
    tail(names(res), 3), c("command", "success", "verified")
  )
})

test_that("bind_two_pass_result() attaches a padded manifest for an all-silent manifest batch (AC2)", {
  jobs <- tibble::tibble(
    input      = c("a.mp4", "b.mp4"),
    output     = c("a.out", "b.out"),
    measured_I = c(NA_real_, NA_real_)
  )
  res <- bind_two_pass_result(
    jobs, c(TRUE, TRUE), ok_res = NULL, run = TRUE, manifest = TRUE
  )
  man <- attr(res, "manifest")
  # One row per job, input paths recorded, every other column NA (matching how a
  # mixed batch pads its silent rows), no md5 columns without checksums.
  expect_identical(names(man), names(manifest_schema(FALSE)))
  expect_equal(nrow(man), 2L)
  expect_equal(man$input, c("a.mp4", "b.mp4"))
  expect_true(all(is.na(man$command)))
  expect_true(all(is.na(man$output_size)))
})

test_that("bind_two_pass_result() manifest carries md5 columns under checksums (AC2)", {
  jobs <- tibble::tibble(
    input      = "a.mp4",
    output     = "a.out",
    measured_I = NA_real_
  )
  res <- bind_two_pass_result(
    jobs, TRUE, ok_res = NULL, run = TRUE, manifest = TRUE, checksums = TRUE
  )
  man <- attr(res, "manifest")
  expect_identical(names(man), names(manifest_schema(TRUE)))
  expect_true(all(c("input_md5", "output_md5") %in% names(man)))
  expect_equal(man$input, "a.mp4")
  expect_true(is.na(man$output_md5))
})

test_that("bind_two_pass_result() synthesizes no opt-in schema under run = FALSE (AC1/AC2)", {
  jobs <- tibble::tibble(input = "a.mp4", output = "a.out", measured_I = NA_real_)
  res <- bind_two_pass_result(
    jobs, TRUE, ok_res = NULL, run = FALSE, verify = TRUE, manifest = TRUE
  )
  # A mixed batch adds no verified/manifest under run = FALSE either; stay parallel.
  expect_false("verified" %in% names(res))
  expect_false("success" %in% names(res))
  expect_null(attr(res, "manifest"))
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


# audio_codec through the two-pass correction (M36) ---------------------------
#
# The batch two-pass path does NOT go through normalize_audio_batch()'s own
# ffm_batch() call -- it detours through run_normalize_correction(), so the
# codec has to reach this seam too or two-pass silently keeps the container
# default. Binary-free: the measured columns are supplied as Phase 1 would.

test_that("run_normalize_correction() honors audio_codec and a per-row column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input           = c(f, f),
    output          = c("a.mp4", "b.mp4"),
    measured_I      = c(-27.61, -19.4),
    measured_TP     = c(-9.32, -3.1),
    measured_LRA    = c(5.90, 8.0),
    measured_thresh = c(-38.06, -29.0),
    offset          = c(0.30, -0.10)
  )
  res <- run_normalize_correction(
    jobs, target_loudness = -23, true_peak = -1, loudness_range = 7,
    channels = NULL, sample_rate = NULL, audio_codec = "aac",
    run = FALSE, parallel = FALSE
  )
  expect_match(as.character(res$command[[1]]), "-codec:a aac", fixed = TRUE)
  expect_match(as.character(res$command[[2]]), "-codec:a aac", fixed = TRUE)
  # Still linear correction, not the single-pass filter.
  expect_match(as.character(res$command[[1]]), "linear=true", fixed = TRUE)

  # A per-row column overrides the argument; NA is the column form of NULL.
  jobs$audio_codec <- c("flac", NA_character_)
  res2 <- run_normalize_correction(
    jobs, target_loudness = -23, true_peak = -1, loudness_range = 7,
    channels = NULL, sample_rate = NULL, audio_codec = "aac",
    run = FALSE, parallel = FALSE
  )
  expect_match(as.character(res2$command[[1]]), "-codec:a flac", fixed = TRUE)
  expect_no_match(as.character(res2$command[[2]]), "-codec:a", fixed = TRUE)
})

test_that("normalize_audio(two_pass = TRUE) carries audio_codec into the correction", {
  # two_pass always runs the analysis pass, so this needs the binary even at
  # run = FALSE.
  skip_if_no_ffmpeg()
  src <- make_test_video()
  cmd <- normalize_audio(src, "out.mp4", audio_codec = "aac",
                         two_pass = TRUE, run = FALSE)
  expect_match(cmd, "-codec:a aac", fixed = TRUE)
  expect_match(cmd, "linear=true", fixed = TRUE)
})

# M086 AC4: the Phase 1 abort is catchable, and says which rows and why --------

# Drive the exported verb with recorded Phase 1 outputs. Phase 1 is the only
# phase that touches a binary before this abort, so mocking it makes the three
# batches deterministic and binary-free; the abort under test is raised by
# assemble_measured() on the way out of Phase 1, before any correction command
# is built.
local_analysis_outputs <- function(outputs, env = parent.frame()) {
  testthat::local_mocked_bindings(
    run_loudnorm_analysis_batch = function(...) outputs,
    .package = "tidymedia",
    .env = env
  )
}

two_pass_abort <- function(outputs) {
  local_analysis_outputs(outputs)
  f <- make_input()
  jobs <- tibble::tibble(input = rep(f, length(outputs)),
                         output = paste0("out", seq_along(outputs), ".m4a"))
  tryCatch(
    normalize_audio_batch(jobs, two_pass = TRUE, run = FALSE),
    tidymedia_loudnorm_no_measurement = function(e) e
  )
}

test_that("the two-pass analysis abort names its rows and their exit statuses", {
  good <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  malformed <- c(
    '\t"input_i" : "-21.85",',
    '\t"input_tp" : "n/a",',
    '\t"input_lra" : "0.00",',
    '\t"input_thresh" : "-31.85",',
    '\t"target_offset" : "-0.02"'
  )
  failed <- function(status) structure("some ffmpeg error", status = status)

  # Batch 1: exit failures only, with two DIFFERENT statuses, so a field pinned
  # to one number cannot pass.
  cnd <- two_pass_abort(list(failed(1L), good, failed(234L)))
  expect_s3_class(cnd, "tidymedia_loudnorm_no_measurement")
  expect_identical(cnd$tm_rows, c(1L, 3L))
  expect_identical(cnd$tm_row_status, c(1L, 234L))

  # Batch 2: unparseable only. These rows exited zero, so there is no status to
  # report and the field says so rather than inventing one.
  cnd <- two_pass_abort(list(good, malformed, malformed))
  expect_s3_class(cnd, "tidymedia_loudnorm_no_measurement")
  expect_identical(cnd$tm_rows, c(2L, 3L))
  expect_identical(cnd$tm_row_status, c(NA_integer_, NA_integer_))

  # Batch 3: one of each, which is the only batch that can show the two causes
  # are aligned to their own rows rather than to the row order.
  cnd <- two_pass_abort(list(malformed, good, failed(69L)))
  expect_s3_class(cnd, "tidymedia_loudnorm_no_measurement")
  expect_identical(cnd$tm_rows, c(1L, 3L))
  expect_identical(cnd$tm_row_status, c(NA_integer_, 69L))
  # `tm_rows` is 1-indexed on the CALLER's table and matches what the message
  # names, which is what a handler reading the field instead of the text needs.
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "Offending rows (1-indexed): 1 and 3", fixed = TRUE)
})

test_that("the two-pass analysis abort is not an FFmpeg-exit condition", {
  # It fires for rows that exited ZERO as well, so it names the analysis event
  # rather than the exit -- and a handler for a non-zero exit must not swallow
  # it even on a batch where every offending row happens to be an exit failure.
  good <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  cnd <- two_pass_abort(list(structure("boom", status = 1L), good))
  expect_false(inherits(cnd, "tidymedia_ffmpeg_exit"))
  expect_null(cnd$tm_status)
})


# M092 AC8: tm_row_status through a REAL failing Phase 1 row -----------------

test_that("a real failing Phase 1 row carries its exit status on tm_row_status", {
  # M086's F9: the AC4 grid above mocks run_loudnorm_analysis_batch() wholesale
  # and hand-builds `structure("some ffmpeg error", status = 1L)` fixtures, so
  # nothing ties assemble_measured()'s expected input to what run_program()
  # actually returns -- a change to that return shape would leave tm_row_status
  # silently all-NA in a real batch with the grid still green. This drives the
  # real Phase 1: no mock anywhere.
  skip_if_no_ffmpeg()
  dir <- withr::local_tempdir()
  # Exists and is readable but is not media, so check_file_readable() passes it
  # through and FFmpeg is the thing that refuses it.
  bad <- file.path(dir, "bad.wav")
  writeLines("this is text, not a RIFF header", bad)
  good <- make_dynamic_audio()

  cnd <- tryCatch(
    normalize_audio_batch(
      tibble::tibble(input = c(good, bad),
                     output = file.path(dir, c("g.m4a", "b.m4a"))),
      two_pass = TRUE, run = FALSE),
    tidymedia_loudnorm_no_measurement = function(e) e
  )
  expect_s3_class(cnd, "tidymedia_loudnorm_no_measurement")
  # The good row measured, so only the second is named -- without it the status
  # assertion below would hold just as well on a batch that failed wholesale.
  expect_identical(cnd$tm_rows, 2L)
  expect_type(cnd$tm_row_status, "integer")
  expect_length(cnd$tm_row_status, 1L)
  expect_false(is.na(cnd$tm_row_status))
})
