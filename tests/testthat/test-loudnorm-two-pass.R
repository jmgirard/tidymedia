# Tests for the M16 two-pass loudnorm internals: the analysis-pass command
# builder and the stderr measurement parser. Command construction and parsing
# are pure (no binary); the recorded fixture is real FFmpeg loudnorm output.

# Analysis-pass command --------------------------------------------------

test_that("loudnorm_analysis_pipeline() compiles a print_format=json null pass", {
  f <- make_input()
  cmd <- ffm_compile(loudnorm_analysis_pipeline(f))
  # print_format=json on the loudnorm filter, discarded output via -f null "-",
  # and no real output file is written.
  expect_match(cmd, "loudnorm=I=-23:TP=-1:LRA=7:print_format=json", fixed = TRUE)
  expect_match(cmd, '-f null "-"', fixed = TRUE)
  expect_no_match(cmd, "-codec", fixed = TRUE)
})

test_that("loudnorm_analysis_pipeline() carries custom targets", {
  f <- make_input()
  cmd <- ffm_compile(
    loudnorm_analysis_pipeline(f, target_loudness = -16, true_peak = -1.5,
                               loudness_range = 11)
  )
  expect_match(cmd, "loudnorm=I=-16:TP=-1.5:LRA=11:print_format=json", fixed = TRUE)
})

# Measurement parser -----------------------------------------------------

test_that("parse_loudnorm_measurements() extracts the five values from real output", {
  fixture <- readLines(test_path("fixtures", "loudnorm-analysis.txt"))
  m <- parse_loudnorm_measurements(fixture)
  expect_equal(m, list(i = -21.85, tp = -17.71, lra = 0.00,
                       thresh = -31.85, offset = -0.02))
})

test_that("parse_loudnorm_measurements() aborts when the block is absent", {
  # Stderr from a run that never printed a JSON measurement block.
  noise <- c("ffmpeg version 8.1.2", "Input #0, mov,mp4", "frame=  15 fps=0.0")
  expect_error(parse_loudnorm_measurements(noise), "parse")
})

test_that("parse_loudnorm_measurements() aborts on a malformed (non-numeric) value", {
  bad <- c(
    '\t"input_i" : "-21.85",',
    '\t"input_tp" : "n/a",',        # not a number
    '\t"input_lra" : "0.00",',
    '\t"input_thresh" : "-31.85",',
    '\t"target_offset" : "-0.02"'
  )
  expect_error(parse_loudnorm_measurements(bad), "parse")
})

test_that("parse_loudnorm_measurements() aborts when one key is missing", {
  # input_thresh omitted entirely.
  partial <- c(
    '\t"input_i" : "-21.85",',
    '\t"input_tp" : "-17.71",',
    '\t"input_lra" : "0.00",',
    '\t"target_offset" : "-0.02"'
  )
  expect_error(parse_loudnorm_measurements(partial), "parse")
})
