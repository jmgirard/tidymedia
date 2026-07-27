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
  # the other stream's encoder name nor a cross-slot -codec flag appears. Assert
  # on the codec slot, not the bare encoder name — the command embeds a random
  # hex tempfile path, and "aac" is three hex digits (M37 review).
  expect_no_match(cmds[["audio"]], "libx264", fixed = TRUE)
  expect_no_match(cmds[["audio"]], "-codec:v", fixed = TRUE)
  expect_no_match(cmds[["video"]], "-codec:v aac", fixed = TRUE)
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

# AC6: execution — copy preserves the source codec, a name transcodes -------

test_that("separate_audio_video() copy preserves each source codec (binary-gated)", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  # MP3-in-MP4: the source audio codec is NOT the MP4 container's default, so a
  # stream copy is distinguishable from a re-encode by the output alone (M35).
  input <- make_mp3_audio_video()
  expect_equal(probe_audio(infile = input)$codec_name, "mp3")

  dir <- withr::local_tempdir()
  audiofile <- file.path(dir, "a.mp4")
  videofile <- file.path(dir, "v.mp4")
  separate_audio_video(input, audiofile, videofile)   # defaults: copy both
  expect_equal(probe_audio(infile = audiofile)$codec_name, "mp3")
  expect_equal(probe_video(infile = videofile)$codec_name,
               probe_video(infile = input)$codec_name)
})

test_that("separate_audio_video() transcodes the stream whose codec is named (binary-gated)", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  input <- make_mp3_audio_video()
  dir <- withr::local_tempdir()
  audiofile <- file.path(dir, "a.m4a")
  videofile <- file.path(dir, "v.mp4")
  # Naming an encoder on one stream re-encodes it; the other keeps its default
  # copy, so the two arguments are observably independent end to end.
  separate_audio_video(input, audiofile, videofile, audio_codec = "aac")
  expect_equal(probe_audio(infile = audiofile)$codec_name, "aac")
  expect_equal(probe_video(infile = videofile)$codec_name,
               probe_video(infile = input)$codec_name)
})

test_that("separate_audio_video() NULL hands the codec to the container (binary-gated)", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  input <- make_mp3_audio_video()
  dir <- withr::local_tempdir()
  audiofile <- file.path(dir, "a.mp4")
  videofile <- file.path(dir, "v.mp4")
  # The pre-M37 `reencode = TRUE` behavior: no -codec:a, so MP4's default
  # encoder decides and the source mp3 becomes aac.
  separate_audio_video(input, audiofile, videofile, audio_codec = NULL)
  expect_equal(probe_audio(infile = audiofile)$codec_name, "aac")
})

test_that("separate_audio_video_batch() honors a per-row codec column end to end (binary-gated)", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  input <- make_mp3_audio_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input       = c(input, input),
    audiofile   = file.path(dir, c("a1.mp4", "a2.m4a")),
    videofile   = file.path(dir, c("v1.mp4", "v2.mp4")),
    audio_codec = c("copy", "aac")
  )
  res <- separate_audio_video_batch(jobs)
  expect_true(all(res$success))
  # Row 1 copied the mp3 through; row 2 transcoded it to aac.
  expect_equal(probe_audio(infile = jobs$audiofile[[1]])$codec_name, "mp3")
  expect_equal(probe_audio(infile = jobs$audiofile[[2]])$codec_name, "aac")
})

# M37 review findings: stale spellings and argument validation -------------

test_that("separate_audio_video_batch() aborts on a stale reencode column", {
  f <- make_input()
  # The reshape builds its own table and would drop an unknown `jobs` column, so
  # a jobs table migrated from the pre-M37 API would stream-copy silently where
  # it asked to re-encode. The column form is guarded like the argument form.
  jobs <- tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4",
                         reencode = TRUE)
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "jobs column")
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "audio_codec")
})

test_that("separate_audio_video_batch() names both stale reencode spellings at once", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4",
                         reencode = TRUE)
  # Argument and column together: one message, both spellings named (a 2+ item
  # cli interpolation, which is where the M18 plural crash used to surface).
  expect_error(
    separate_audio_video_batch(jobs, reencode = TRUE, run = FALSE),
    "argument and jobs column"
  )
})

test_that("separate_audio_video_batch() rejects a codec argument through the dots", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4")
  # `codec` names the internal per-stream column; forwarded through `...` it
  # would reach pmap and set both streams' codec at once.
  expect_error(
    separate_audio_video_batch(jobs, codec = "libmp3lame", run = FALSE),
    "not an argument"
  )
})

test_that("separate_audio_video_batch() validates its codec args whether or not a column is present", {
  f <- make_input()
  bare <- tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4")
  withcol <- bare
  withcol$audio_codec <- "copy"
  # The same bad argument must be caught in both shapes: before the fix, a
  # present codec column materialized the argument straight into the reshaped
  # column and skipped the type check entirely.
  for (jobs in list(bare, withcol)) {
    expect_error(separate_audio_video_batch(jobs, video_codec = TRUE, run = FALSE))
    expect_error(separate_audio_video_batch(jobs, video_codec = 1, run = FALSE))
    expect_error(separate_audio_video_batch(jobs, video_codec = NA, run = FALSE))
    expect_error(
      separate_audio_video_batch(jobs, video_codec = c("a", "b"), run = FALSE)
    )
    expect_error(separate_audio_video_batch(jobs, audio_codec = TRUE, run = FALSE))
  }
  # NULL stays legal on both: it is the sentinel, not a bad value.
  expect_no_error(
    separate_audio_video_batch(withcol, video_codec = NULL, run = FALSE)
  )
})

# M38: hardware = "nvenc" on the video output ------------------------------
#
# nvenc encodes video, so `hardware`/`fallback` reach the video branch of
# separate_stream_pipeline() and never the audio one. Availability is simulated
# through the `tidymedia.nvenc_encoders` option seam has_nvenc() consults, so
# every compile test here is GPU-free; the real encode is skip-gated below.

test_that("separate_audio_video() defaults compile no hardware options", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", run = FALSE)
  ref <- pre_m37(f)
  # hardware = "none" is the default, so the pre-M38 commands are unchanged.
  expect_equal(unname(cmds[["audio"]]), ref$copy_audio)
  expect_equal(unname(cmds[["video"]]), ref$copy_video)
  expect_no_match(cmds[["video"]], "nvenc", fixed = TRUE)
})

test_that("separate_audio_video(hardware = 'none') matches the argument-free call", {
  f <- make_input()
  for (vc in list(NULL, "libx264")) {
    expect_equal(
      separate_audio_video(f, "a.aac", "v.mp4", video_codec = vc,
                           hardware = "none", run = FALSE),
      separate_audio_video(f, "a.aac", "v.mp4", video_codec = vc, run = FALSE)
    )
  }
})

test_that("separate_audio_video(hardware = 'nvenc') compiles to the nvenc encoder", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", video_codec = NULL,
                               hardware = "nvenc", run = FALSE)
  expect_match(cmds[["video"]], "-codec:v h264_nvenc", fixed = TRUE)
})

test_that("separate_audio_video(hardware = 'nvenc') respects the video_codec family", {
  withr::local_options(tidymedia.nvenc_encoders = c("h264_nvenc", "hevc_nvenc"))
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", video_codec = "libx265",
                               hardware = "nvenc", run = FALSE)
  expect_match(cmds[["video"]], "-codec:v hevc_nvenc", fixed = TRUE)
})

test_that("separate_audio_video() hardware never reaches the audio command", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  ref <- pre_m37(f)
  # nvenc encodes video only, so the audio output stays byte-identical across
  # every hardware/fallback combination -- neither argument reaches that branch.
  for (hw in c("none", "nvenc")) {
    for (fb in c(FALSE, TRUE)) {
      cmds <- separate_audio_video(f, "a.aac", "v.mp4", video_codec = NULL,
                                   hardware = hw, fallback = fb, run = FALSE)
      expect_equal(unname(cmds[["audio"]]), ref$copy_audio)
      expect_no_match(cmds[["audio"]], "nvenc", fixed = TRUE)
    }
  }
})

test_that("separate_audio_video(hardware = 'nvenc') aborts when unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_error(
    separate_audio_video(f, "a.aac", "v.mp4", video_codec = NULL,
                         hardware = "nvenc", run = FALSE),
    "not available"
  )
})

test_that("separate_audio_video() fallback keeps the sentinel, never injects a codec", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmds <- separate_audio_video(f, "a.aac", "v.mp4", video_codec = NULL,
                                 hardware = "nvenc", fallback = TRUE,
                                 run = FALSE),
    "container"
  )
  expect_equal(unname(cmds[["video"]]), pre_m37(f)$unset_video)
})

test_that("separate_audio_video() rejects an unknown hardware value", {
  f <- make_input()
  expect_error(
    separate_audio_video(f, "a.aac", "v.mp4", video_codec = NULL,
                         hardware = "gpu", run = FALSE),
    class = "rlang_error"
  )
})

test_that("separate_audio_video_batch(hardware = 'nvenc') applies nvenc to video rows only", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), audiofile = c("a1.aac", "a2.aac"),
                         videofile = c("v1.mp4", "v2.mp4"))
  res <- separate_audio_video_batch(jobs, video_codec = NULL,
                                    hardware = "nvenc", run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc",
                        res$command[res$stream == "video"], fixed = TRUE)))
  expect_false(any(grepl("nvenc", res$command[res$stream == "audio"],
                         fixed = TRUE)))
})

test_that("separate_audio_video_batch() ignores a per-row hardware column (batch-wide)", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), audiofile = c("a1.aac", "a2.aac"),
                         videofile = c("v1.mp4", "v2.mp4"),
                         hardware = c("nvenc", "nvenc"))
  # hardware is a property of the machine, not of a row (D016), so the column is
  # ignored like any other unrecognized one and the scalar default wins.
  res <- separate_audio_video_batch(jobs, run = FALSE)
  expect_false(any(grepl("nvenc", res$command, fixed = TRUE)))
})
