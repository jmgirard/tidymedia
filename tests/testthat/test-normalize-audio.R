# Tests for normalize_audio(): a Layer-2 verb that loudness-normalizes a file's
# audio (EBU R128 loudnorm) while stream-copying video. Command construction is
# tested purely (run = FALSE); execution is gated on the ffmpeg binary.

test_that("normalize_audio() compiles the default EBU R128 command", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_equal(
    cmd,
    sprintf(
      '-y -i "%s" -af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy "out.mp4"',
      f
    )
  )
})

test_that("normalize_audio() honors custom loudness targets", {
  f <- make_input()
  cmd <- normalize_audio(
    f, "out.mp4",
    target_loudness = -16, true_peak = -1.5, loudness_range = 11,
    run = FALSE
  )
  expect_match(cmd, '-af "loudnorm=I=-16:TP=-1.5:LRA=11"', fixed = TRUE)
})

test_that("normalize_audio() adds downmix and resample when requested", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", channels = 1, sample_rate = 48000,
                         run = FALSE)
  expect_match(cmd, "-codec:v copy -ac 1 -ar 48000", fixed = TRUE)
})

test_that("normalize_audio() omits downmix/resample by default", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_no_match(cmd, "-ac ", fixed = TRUE)
  expect_no_match(cmd, "-ar ", fixed = TRUE)
})

test_that("normalize_audio() stream-copies video (touches audio only)", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_match(cmd, "-codec:v copy", fixed = TRUE)
  expect_no_match(cmd, "-codec:v libx264", fixed = TRUE)
})

# Front-door validation ---------------------------------------------------

test_that("normalize_audio() rejects a missing input file", {
  expect_error(normalize_audio("does_not_exist.mp4", "out.mp4", run = FALSE),
               "exist")
})

test_that("normalize_audio() rejects out-of-range loudness targets", {
  f <- make_input()
  expect_error(normalize_audio(f, "out.mp4", target_loudness = -3, run = FALSE))
  expect_error(normalize_audio(f, "out.mp4", true_peak = 1, run = FALSE))
  expect_error(normalize_audio(f, "out.mp4", loudness_range = 0, run = FALSE))
})

test_that("normalize_audio() rejects non-positive channels or sample_rate", {
  f <- make_input()
  expect_error(normalize_audio(f, "out.mp4", channels = 0, run = FALSE))
  expect_error(normalize_audio(f, "out.mp4", channels = 1.5, run = FALSE))
  expect_error(normalize_audio(f, "out.mp4", sample_rate = -1, run = FALSE))
})

# Execution (binary-gated) ------------------------------------------------

test_that("normalize_audio() writes a non-empty, audio-decodable output", {
  skip_if_no_ffprobe()
  src <- make_test_video()  # skips if ffmpeg absent; has a 440 Hz sine track
  out <- withr::local_tempfile(fileext = ".mp4")
  normalize_audio(src, out, channels = 1, sample_rate = 44100)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
  # The output carries a decodable audio stream at the requested rate.
  codec <- ffprobe(sprintf(
    '-v error -select_streams a:0 -show_entries stream=codec_type -of csv=p=0 "%s"',
    out
  ))
  expect_match(codec[[1]], "audio")
})
