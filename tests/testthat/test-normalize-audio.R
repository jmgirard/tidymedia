# Tests for normalize_audio(): a Layer-2 verb that loudness-normalizes a file's
# audio (EBU R128 loudnorm). Its output is ONE audio stream and no video since
# D030 -- it no longer stream-copies video, and no longer names a video codec.
# Command construction is tested purely (run = FALSE); execution is gated on the
# ffmpeg binary.

test_that("normalize_audio() compiles the default EBU R128 command", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_equal(
    cmd,
    sprintf(
      paste0('-y -i "%s" -af "loudnorm=I=-23:TP=-1:LRA=7" ',
             '-map "0:a:0" "out.mp4"'),
      f
    )
  )
})

# Characterization: pin the full command for a fully-specified single-pass call
# so the `two_pass = FALSE` default (added in M16) stays byte-for-byte identical
# to today's behavior. This baseline must not drift when two-pass lands.
#
# Deliberately overwritten at M49, which is the one thing the paragraph above
# forbids doing by accident. The map pair is a stated behavior change, not
# drift: this verb emitted no map at all and let FFmpeg's DEFAULT-disposition
# heuristic pick one audio track (D028). The rest of the command is unchanged
# byte for byte, and that is what this baseline still pins.
test_that("normalize_audio() single-pass command is byte-for-byte stable (M16 baseline, re-based M49)", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", target_loudness = -16, true_peak = -1.5,
                         loudness_range = 11, channels = 1, sample_rate = 48000,
                         run = FALSE)
  expect_equal(
    cmd,
    sprintf(
      paste0('-y -i "%s" -af "loudnorm=I=-16:TP=-1.5:LRA=11" ',
             '-ac 1 -ar 48000 -map "0:a:0" "out.mp4"'),
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
  expect_match(cmd, "-ac 1 -ar 48000", fixed = TRUE)
})

test_that("normalize_audio() omits downmix/resample by default", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_no_match(cmd, "-ac ", fixed = TRUE)
  expect_no_match(cmd, "-ar ", fixed = TRUE)
})

test_that("normalize_audio() names no video codec and maps no video (D030)", {
  # Was "stream-copies video (touches audio only)". The verb no longer carries
  # video at all, so `-codec:v copy` named a stream that is never mapped; this
  # test is inverted rather than deleted, because what it originally guarded --
  # that the video is never RE-ENCODED -- is now guaranteed more strongly.
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_no_match(cmd, "-codec:v", fixed = TRUE)
  expect_no_match(cmd, "0:v", fixed = TRUE)
})

# Two-pass correction builder (M16) ---------------------------------------

test_that("normalize_audio_pipeline() threads measured values into a linear correction", {
  f <- make_input()
  measured <- list(i = -27.61, tp = -9.32, lra = 5.90,
                   thresh = -38.06, offset = 0.30)
  p <- normalize_audio_pipeline(f, "out.mp4", channels = 1, sample_rate = 48000,
                                measured = measured)
  cmd <- ffm_compile(p)
  expect_match(
    cmd,
    paste0("loudnorm=I=-23:TP=-1:LRA=7:measured_I=-27.61:measured_TP=-9.32:",
           "measured_LRA=5.9:measured_thresh=-38.06:offset=0.3:linear=true"),
    fixed = TRUE
  )
  # The correction pass still rides the shared pipeline's shaping: downmix,
  # resample, and the audio map (the video copy went with D030).
  expect_match(cmd, "-ac 1 -ar 48000", fixed = TRUE)
})

test_that("normalize_audio_pipeline() without measured is single-pass (no linear)", {
  f <- make_input()
  p <- normalize_audio_pipeline(f, "out.mp4")
  cmd <- ffm_compile(p)
  expect_match(cmd, '-af "loudnorm=I=-23:TP=-1:LRA=7"', fixed = TRUE)
  expect_no_match(cmd, "linear", fixed = TRUE)
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

test_that("normalize_audio(two_pass = FALSE) is identical to the single-pass default", {
  f <- make_input()
  # The new default arg must not perturb the single-pass command at all (AC1).
  expect_equal(
    normalize_audio(f, "out.mp4", two_pass = FALSE, run = FALSE),
    normalize_audio(f, "out.mp4", run = FALSE)
  )
})

test_that("normalize_audio() rejects a non-logical two_pass", {
  f <- make_input()
  expect_error(normalize_audio(f, "out.mp4", two_pass = "yes", run = FALSE))
})

# Two-pass execution (binary-gated) ---------------------------------------

test_that("normalize_audio(two_pass = TRUE) hits the target and beats single-pass", {
  skip_if_no_ffprobe()
  src <- make_dynamic_audio()  # skips if ffmpeg absent; high-LRA source
  target <- -23

  single <- withr::local_tempfile(fileext = ".mp4")
  normalize_audio(src, single, target_loudness = target)
  two <- withr::local_tempfile(fileext = ".mp4")
  normalize_audio(src, two, target_loudness = target, two_pass = TRUE)

  # Re-probe each output's integrated loudness with a fresh analysis pass
  # (its input_i is the output's measured loudness). Source: EBU R 128 (2014).
  loud_single <- run_loudnorm_analysis(single, target_loudness = target)$i
  loud_two <- run_loudnorm_analysis(two, target_loudness = target)$i

  # Two-pass lands within +/-1 LU of the target ...
  expect_lt(abs(loud_two - target), 1)
  # ... and is at least as close to target as single-pass (strictly closer on
  # this high-LRA material, where dynamic single-pass drifts).
  expect_lt(abs(loud_two - target), abs(loud_single - target))
})

test_that("normalize_audio(two_pass, run = FALSE) runs analysis, returns correction cmd", {
  skip_if_no_ffmpeg()
  src <- system.file("extdata", "sample.mp4", package = "tidymedia")
  out <- withr::local_tempfile(fileext = ".mp4")
  cmd <- normalize_audio(src, out, two_pass = TRUE, run = FALSE)
  # The returned value is the correction command, carrying the measured values
  # and linear=true ...
  expect_type(cmd, "character")
  expect_match(cmd, "measured_I=", fixed = TRUE)
  expect_match(cmd, "linear=true", fixed = TRUE)
  # ... and run = FALSE means the correction pass did not run: no output file.
  expect_false(file.exists(out))
})

test_that("normalize_audio(two_pass = TRUE) errors clearly on silent input (M18)", {
  src <- make_silent_audio()  # anullsrc silence; skips if ffmpeg absent
  out <- withr::local_tempfile(fileext = ".mp4")
  # The analysis pass measures input_i = -inf; the abort must name silence, not
  # the misleading "could not parse" measurement error.
  expect_error(
    normalize_audio(src, out, two_pass = TRUE),
    "silent"
  )
  expect_false(file.exists(out))
})

# Execution (binary-gated) ------------------------------------------------

test_that("normalize_audio() writes a non-empty, audio-decodable output", {
  skip_if_no_ffprobe()
  src <- make_test_video()  # skips if ffmpeg absent; has a 440 Hz sine track
  out <- withr::local_tempfile(fileext = ".mp4")
  normalize_audio(src, out, channels = 1, sample_rate = 22050)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
  # The output carries a decodable audio stream ...
  probe <- ffprobe(sprintf(
    paste('-v error -select_streams a:0',
          '-show_entries stream=codec_type,sample_rate -of csv=p=0 "%s"'),
    out
  ))
  expect_match(probe[[1]], "audio")
  # ... and an explicit sample_rate is honored (loudnorm otherwise resamples to
  # an encoder-capped rate, never the source rate -- so pinning it is the point).
  expect_match(probe[[1]], "22050")
})


# audio_codec (M36) -------------------------------------------------------
#
# normalize_audio() filters the audio (loudnorm), so it MUST re-encode it --
# until M36 it re-encoded to whatever the local FFmpeg build's container default
# was, the environment-dependence D001 exists to remove. audio_codec names that
# encoder. D017's "copy" default deliberately does NOT transfer here (a filtered
# stream cannot be copied), so the default is D016's NULL sentinel and every
# pre-M36 command is unchanged.

test_that("normalize_audio() emits no -codec:a by default (NULL sentinel)", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", run = FALSE)
  expect_no_match(cmd, "-codec:a", fixed = TRUE)
  # Byte-identical to the pre-M36 default command.
  expect_equal(
    cmd,
    sprintf(
      paste0('-y -i "%s" -af "loudnorm=I=-23:TP=-1:LRA=7" ',
             '-map "0:a:0" "out.mp4"'),
      f
    )
  )
})

test_that("normalize_audio(audio_codec = ) names the output audio encoder", {
  f <- make_input()
  cmd <- normalize_audio(f, "out.mp4", audio_codec = "aac", run = FALSE)
  # -codec:v is emitted before -codec:a (ffm_groups()); the loudnorm filter and
  # the video stream copy are untouched by the new argument.
  expect_equal(
    cmd,
    sprintf(
      paste0('-y -i "%s" -af "loudnorm=I=-23:TP=-1:LRA=7" ',
             '-codec:a aac -map "0:a:0" "out.mp4"'),
      f
    )
  )
})

test_that("normalize_audio(audio_codec = 'copy') aborts in the verb's vocabulary", {
  f <- make_input()
  # Layer 1 already refuses a copied stream that is also filtered (R/ffm.R,
  # M02 D-M02-5); this front-door check fails earlier and names `audio_codec`
  # rather than ffm_codec(), and spares two-pass a wasted analysis run.
  expect_error(
    normalize_audio(f, "out.mp4", audio_codec = "copy", run = FALSE),
    "audio_codec"
  )
})

test_that("normalize_audio() rejects a non-string audio_codec", {
  f <- make_input()
  expect_error(normalize_audio(f, "out.mp4", audio_codec = 1, run = FALSE))
})

test_that("normalize_audio(audio_codec = ) reaches the written output", {
  skip_if_no_ffprobe()
  src <- make_test_video()  # skips if ffmpeg absent; AAC audio in MP4
  named <- withr::local_tempfile(fileext = ".mp4")
  unset <- withr::local_tempfile(fileext = ".mp4")

  # Pin an encoder that is NOT the MP4 container default, so the argument's
  # effect is observable from the output alone (the M35 discrimination lesson,
  # applied in reverse: here the *named* codec must differ from the default).
  normalize_audio(src, named, audio_codec = "libmp3lame")
  expect_equal(probe_audio(infile = named)$codec_name, "mp3")

  # The NULL sentinel leaves the container default in place.
  normalize_audio(src, unset)
  expect_equal(probe_audio(infile = unset)$codec_name, "aac")
})


# Review findings F1/F3 (M36) -------------------------------------------------

test_that("the 'copy' abort points at NULL as code, not as a string to pass", {
  f <- make_input()
  msg <- tryCatch(
    normalize_audio(f, "out.mp4", audio_codec = "copy", run = FALSE),
    error = conditionMessage
  )
  # `{.val NULL}` would render "NULL" quoted, reading as a value to pass -- and
  # "NULL" is a valid token, so following that hint compiles `-codec:a NULL`.
  expect_no_match(msg, '"NULL"', fixed = TRUE)
  expect_match(msg, "NULL", fixed = TRUE)
})

test_that("two_pass validates the encoder token before running the analysis pass", {
  skip_if_no_ffmpeg()
  # A file that exists but is not decodable: with the token check hoisted, the
  # malformed encoder aborts before Phase 1 is attempted, so the error is about
  # the token. Without the hoist, ffmpeg would be run first and fail on the
  # unreadable input instead.
  bad <- withr::local_tempfile(fileext = ".mp4")
  writeLines("not a media file", bad)
  msg <- tryCatch(
    normalize_audio(bad, "out.mp4", audio_codec = "libmp3 lame",
                    two_pass = TRUE, run = FALSE),
    error = conditionMessage
  )
  expect_match(msg, "single clean token", fixed = TRUE)
})
