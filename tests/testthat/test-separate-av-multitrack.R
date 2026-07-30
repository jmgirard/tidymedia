# M45: `audio_stream` on separate_audio_video() / _batch, and the enriched abort
# a multi-track input gets when FFmpeg refuses the audio output.
#
# The `NULL` default of this verb's `audio_stream` means EVERY audio track --
# `-map 0:a`, what the verb has compiled since it shipped -- not the first track
# the extraction verbs' `NULL` selects (D023). So the baseline below is a pin
# against silently narrowing the Matroska callers who receive all their tracks
# today, and it is the pre-change form, recorded from commit b548902 (the last
# commit before this milestone; `git diff b548902 HEAD -- R/ffmpeg.R` touched no
# separation code, so the strings the working tree produced before T1 are that
# commit's). Verbatim, on inst/extdata/sample.mp4:
#
#   audio: -y -i "<...>/sample.mp4" -codec:a copy -map 0:a "audio.aac"
#   video: -y -i "<...>/sample.mp4" -codec:v copy -map 0:v "video.mp4"
#
# The tests pin that form as a template so they do not depend on a temp path.


# AC1: what each spelling compiles ------------------------------------------

baseline_pair <- function(infile, audiofile, videofile) {
  c(
    audio = sprintf('-y -i "%s" -codec:a copy -map 0:a "%s"', infile, audiofile),
    video = sprintf('-y -i "%s" -codec:v copy -map 0:v "%s"', infile, videofile)
  )
}

test_that("the default compiles the pre-change every-track pair", {
  infile <- make_input("mkv")
  expect_identical(
    separate_audio_video(infile, "audio.aac", "video.mp4", run = FALSE),
    baseline_pair(infile, "audio.aac", "video.mp4")
  )
})

test_that("an explicit NULL compiles the same pair as the absent argument", {
  infile <- make_input("mkv")
  expect_identical(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = NULL,
                         run = FALSE),
    baseline_pair(infile, "a.aac", "v.mp4")
  )
})

test_that("audio_stream narrows the audio map and leaves the video map alone", {
  infile <- make_input("mkv")
  out <- separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1,
                              run = FALSE)
  expect_match(out[["audio"]], "-map 0:a:1", fixed = TRUE)
  expect_match(out[["video"]], "-map 0:v", fixed = TRUE)
  # The every-track form must be GONE from the audio command, not merely joined
  # by the narrow one: `-map 0:a -map 0:a:1` would carry every track and pass a
  # containment-only assertion (M43 made ffm_map() append).
  expect_false(grepl("-map 0:a ", out[["audio"]], fixed = TRUE))
  expect_identical(out[["video"]], baseline_pair(infile, "a.aac", "v.mp4")[["video"]])
})

test_that("audio_stream = 1L compiles the identical pair to audio_stream = 1", {
  infile <- make_input("mkv")
  expect_identical(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1L, run = FALSE),
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1, run = FALSE)
  )
})

test_that("audio_stream = 0 selects the first track rather than every track", {
  # The discriminator between this verb's NULL and an explicit 0: on the
  # extraction verbs they compile the same map, here they must not.
  infile <- make_input("mkv")
  narrow <- separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 0,
                                 run = FALSE)
  expect_match(narrow[["audio"]], "-map 0:a:0", fixed = TRUE)
  expect_false(identical(narrow, baseline_pair(infile, "a.aac", "v.mp4")))
})

test_that("a non-whole or out-of-range audio_stream is rejected by name", {
  infile <- make_input("mkv")
  expect_error(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1.5,
                         run = FALSE),
    "audio_stream"
  )
  expect_error(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = -1,
                         run = FALSE),
    "audio_stream"
  )
  expect_error(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = NA,
                         run = FALSE),
    "audio_stream"
  )
})

test_that("the extraction verbs' NULL still means the first track", {
  # The other half of the split this milestone records: parameterizing
  # audio_stream_map()'s NULL resolution must not have moved D023's callers.
  infile <- make_input("mkv")
  expect_match(extract_audio(infile, "a.aac", run = FALSE), "-map 0:a:0",
               fixed = TRUE)
  expect_match(convert_audio(infile, "a.mp3", run = FALSE), "-map 0:a:0",
               fixed = TRUE)
})
