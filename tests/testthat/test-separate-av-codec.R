# M37: per-stream audio_codec / video_codec on separate_audio_video() and its
# _batch sibling, subsuming the old `reencode` switch. `"copy"` (the default)
# reproduces the pre-M37 `reencode = FALSE` command byte-for-byte, NULL is
# D016's sentinel reproducing `reencode = TRUE`, and a named encoder pins that
# stream's encoder. Command construction is tested purely (run = FALSE);
# execution is binary-gated.

# The pre-M37 commands, captured from the default branch before the swap. The
# input path is the only variable, so parity is asserted byte-for-byte rather
# than by substring (AC1, AC2).
pre_m37 <- function(input) {
  list(
    copy_audio  = sprintf('-y -i "%s" -codec:a copy -map 0:a "a.aac"', input),
    copy_video  = sprintf('-y -i "%s" -codec:v copy -map 0:v "v.mp4"', input),
    unset_audio = sprintf('-y -i "%s" -map 0:a "a.aac"', input),
    unset_video = sprintf('-y -i "%s" -map 0:v "v.mp4"', input)
  )
}

# AC1: the default call reproduces reencode = FALSE ------------------------

test_that("separate_audio_video() defaults reproduce the pre-M37 stream copy byte for byte", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", run = FALSE)
  ref <- pre_m37(f)
  expect_equal(unname(cmds[["audio"]]), ref$copy_audio)
  expect_equal(unname(cmds[["video"]]), ref$copy_video)
})

test_that("separate_audio_video(audio_codec = 'copy', video_codec = 'copy') is the default", {
  f <- make_input()
  expect_equal(
    separate_audio_video(f, "a.aac", "v.mp4", audio_codec = "copy",
                         video_codec = "copy", run = FALSE),
    separate_audio_video(f, "a.aac", "v.mp4", run = FALSE)
  )
})

# AC2: the NULL sentinel reproduces reencode = TRUE ------------------------

test_that("separate_audio_video(audio_codec = NULL, video_codec = NULL) emits no -codec", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", audio_codec = NULL,
                               video_codec = NULL, run = FALSE)
  ref <- pre_m37(f)
  expect_equal(unname(cmds[["audio"]]), ref$unset_audio)
  expect_equal(unname(cmds[["video"]]), ref$unset_video)
})

test_that("separate_audio_video() unsets one stream's codec independently", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", audio_codec = NULL,
                               run = FALSE)
  ref <- pre_m37(f)
  # Audio unset, video still on the default copy.
  expect_equal(unname(cmds[["audio"]]), ref$unset_audio)
  expect_equal(unname(cmds[["video"]]), ref$copy_video)
})

# AC3: a named encoder reaches its own stream and only that one ------------

test_that("separate_audio_video() routes each codec to its own stream's command", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.m4a", "v.mp4", audio_codec = "aac",
                               video_codec = "libx264", run = FALSE)
  expect_match(cmds[["audio"]], "-codec:a aac", fixed = TRUE)
  expect_match(cmds[["video"]], "-codec:v libx264", fixed = TRUE)
  # The audio choice never reaches the video command, nor the reverse: neither
  # the other stream's encoder name nor a cross-slot -codec flag appears.
  expect_no_match(cmds[["audio"]], "libx264", fixed = TRUE)
  expect_no_match(cmds[["audio"]], "-codec:v", fixed = TRUE)
  expect_no_match(cmds[["video"]], "aac", fixed = TRUE)
  expect_no_match(cmds[["video"]], "-codec:a", fixed = TRUE)
})

# AC4: `reencode` is gone from both verbs ---------------------------------

test_that("separate_audio_video() has no reencode argument", {
  f <- make_input()
  # No `...` on the scalar verb, so R rejects the retired argument itself.
  expect_error(
    separate_audio_video(f, "a.aac", "v.mp4", reencode = TRUE, run = FALSE),
    "unused argument"
  )
  expect_false("reencode" %in% names(formals(separate_audio_video)))
})

test_that("separate_audio_video_batch() aborts on the retired reencode argument", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4")
  # The batch verb's `...` forwards ffm_batch options, so R would swallow
  # `reencode` silently and stream-copy output the caller asked to re-encode.
  # The guard names the replacement instead (M37).
  expect_error(
    separate_audio_video_batch(jobs, reencode = TRUE, run = FALSE),
    "audio_codec"
  )
  expect_error(
    separate_audio_video_batch(jobs, reencode = FALSE, run = FALSE),
    "was removed"
  )
  expect_false("reencode" %in% names(formals(separate_audio_video_batch)))
})

test_that("separate_audio_video() rejects a non-string codec on either stream", {
  f <- make_input()
  expect_error(
    separate_audio_video(f, "a.aac", "v.mp4", audio_codec = 1, run = FALSE)
  )
  expect_error(
    separate_audio_video(f, "a.aac", "v.mp4", video_codec = 1, run = FALSE)
  )
  # check_token() rejects a value carrying shell metacharacters.
  expect_error(
    separate_audio_video(f, "a.aac", "v.mp4", audio_codec = "aac; rm -rf /",
                         run = FALSE)
  )
})
