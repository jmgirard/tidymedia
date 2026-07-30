# Compile an ffm pipeline and replace each input path with a stable "<inN>"
# token, so full-command snapshots are deterministic across machines and CI
# (temp paths and Windows backslashes would otherwise leak in).
compile_scrubbed <- function(p) {
  cmd <- ffm_compile(p)
  for (i in seq_along(p$input)) {
    cmd <- gsub(p$input[[i]], sprintf("<in%d>", i), cmd, fixed = TRUE)
  }
  # The concat demuxer references a temp list-file path; scrub it too.
  if (length(p$concat_list)) {
    cmd <- gsub(p$concat_list, "<concatlist>", cmd, fixed = TRUE)
  }
  cmd
}

# Run an FFmpeg fixture command under a wall-clock timeout, and error if it
# reaches the limit. Fixture generation goes through here rather than through
# ffmpeg() because a hung FFmpeg would otherwise block the run forever with no
# output: `-shortest` beside a mapped subtitle stream deadlocked ~40% of runs on
# ffmpeg 8.1.2 (M46). A synthetic clip of a couple of seconds cannot legitimately
# take minutes, so reaching the limit is a defect and must be loudly red -- an
# error rather than a skip, which would go green on CI. base R's system()
# timeout kills the child at the limit and reports status 124, so no package
# dependency is involved. Skips the calling test if ffmpeg is unavailable.
# Returns FFmpeg's stdout invisibly.
run_ffmpeg_fixture <- function(command, timeout = 120) {
  skip_if_no_ffmpeg()
  location <- find_ffmpeg()
  out <- withCallingHandlers(
    system(paste(location, command), intern = TRUE, input = "",
           timeout = timeout),
    # Muffle only R's own timeout warning -- the failure is raised below with a
    # better message. Every other warning (a non-zero FFmpeg exit) still reaches
    # the reporter exactly as it did when these sites called ffmpeg().
    warning = function(w) {
      if (grepl("timed out", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  status <- attr(out, "status")
  if (!is.null(status) && identical(as.integer(status), 124L)) {
    # Name the binary and the limit only: the command string carries temp paths.
    stop(sprintf("%s fixture generation timed out after %g seconds.",
                 basename(location), timeout), call. = FALSE)
  }
  invisible(out)
}

# Generate a short test video (with an audio track) using ffmpeg's synthetic
# lavfi sources, so integration tests do not need a checked-in media fixture.
# Skips the calling test if ffmpeg is unavailable. Returns the file path.
make_test_video <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  command <- paste(
    "-y -f lavfi -i testsrc=duration=2:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=2",
    sprintf('-shortest -pix_fmt yuv420p "%s"', path)
  )
  run_ffmpeg_fixture(command)
  testthat::skip_if_not(file.exists(path), "test video could not be generated")
  path
}

# Generate a test video whose audio codec is NOT the output container's default:
# MP3 audio in an MP4, where FFmpeg would otherwise encode AAC. That makes
# copy-vs-re-encode observable from the output alone — a stream copy keeps mp3,
# an unset codec yields aac (M35). Skips the calling test if ffmpeg is
# unavailable. Returns the file path.
make_mp3_audio_video <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  command <- paste(
    "-y -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=1",
    "-c:v libx264 -c:a libmp3lame -shortest -pix_fmt yuv420p",
    sprintf('"%s"', path)
  )
  run_ffmpeg_fixture(command)
  testthat::skip_if_not(file.exists(path), "test video could not be generated")
  path
}

# Generate a longer H.264 test video with a *known* keyframe interval (a
# keyframe every `gop` frames at `rate` fps), so cut-accuracy tests can request
# a non-keyframe boundary and observe accurate vs keyframe-snapped behaviour.
make_keyframed_video <- function(duration = 12, rate = 24, gop = 48,
                                 env = parent.frame()) {
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  command <- paste(
    sprintf("-y -f lavfi -i testsrc=duration=%s:size=128x72:rate=%s", duration, rate),
    sprintf("-c:v libx264 -g %s -keyint_min %s -sc_threshold 0", gop, gop),
    sprintf('-pix_fmt yuv420p "%s"', path)
  )
  run_ffmpeg_fixture(command)
  testthat::skip_if_not(file.exists(path), "test video could not be generated")
  path
}

# Generate an audio clip with real loudness variation: a 440 Hz sine under a
# slow, deep tremolo (amplitude modulation), so its loudness swells and dips.
# Single-pass (dynamic) loudnorm drifts well off the target on such material
# while two-pass (linear) hits it, so the accuracy gap is observable (M16 AC5).
# tremolo takes no comma-bearing expression, so it survives the shell verbatim.
# Skips the calling test if ffmpeg is unavailable. Returns the file path.
make_dynamic_audio <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  command <- paste(
    "-y -f lavfi -i sine=frequency=440:duration=6:sample_rate=48000",
    "-af tremolo=f=0.2:d=0.9",
    sprintf('-c:a aac "%s"', path)
  )
  run_ffmpeg_fixture(command)
  testthat::skip_if_not(file.exists(path),
                        "dynamic test audio could not be generated")
  path
}

# Generate a digitally silent audio clip (anullsrc). FFmpeg's loudnorm analysis
# measures its integrated loudness as -inf, so two-pass normalization must treat
# it as silence (M18) rather than a parse failure. Skips the calling test if
# ffmpeg is unavailable. Returns the file path.
make_silent_audio <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  command <- paste(
    "-y -f lavfi -i anullsrc=r=44100:cl=mono -t 1",
    sprintf('-c:a aac "%s"', path)
  )
  run_ffmpeg_fixture(command)
  testthat::skip_if_not(file.exists(path),
                        "silent test audio could not be generated")
  path
}

# Generate a video carrying THREE audio tracks (aac, tagged eng/spa/fra, at
# distinct sine frequencies), so tests can observe which track a verb selects and
# whether it selects exactly one. A verb mapping every audio stream into a
# single-stream container (mp3) fails on this input and succeeds on a one-track
# file, which is the discriminator the hotfix regression test needs. Matroska
# rather than MP4 so the per-stream language tags survive the round trip. Skips
# the calling test if ffmpeg is unavailable. Returns the file path.
make_multitrack_video <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mkv", .local_envir = env)
  command <- paste(
    "-y -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=300:duration=1",
    "-f lavfi -i sine=frequency=600:duration=1",
    "-f lavfi -i sine=frequency=900:duration=1",
    "-map 0:v -map 1:a -map 2:a -map 3:a",
    "-c:v libx264 -c:a aac -b:a 32k -pix_fmt yuv420p",
    "-metadata:s:a:0 language=eng",
    "-metadata:s:a:1 language=spa",
    "-metadata:s:a:2 language=fra",
    sprintf('"%s"', path)
  )
  run_ffmpeg_fixture(command)
  testthat::skip_if_not(file.exists(path),
                        "multitrack test video could not be generated")
  path
}

# Generate a video carrying a subtitle track beside its video and audio, so
# tests can observe whether an explicit -map takes audio alone or lets FFmpeg's
# old implicit "one stream of each type" selection carry a subtitle along (M43).
# Matroska because the container has to accept a subtitle for the distinction to
# be visible at all.
#
# Deliberately NO -shortest, because beside a mapped subtitle stream it deadlocks
# FFmpeg intermittently: this command hung 10 times in 25 runs on ffmpeg
# 8.1.2/macOS, while the same command without the flag hung 0 in 15, and the flag
# WITH the subtitle map dropped hung 0 in 15 (M46).
#
# Removing it is not a no-op, though every consumer here is indifferent to what
# it changes. The flag was tracking the SHORTEST stream, which is the 1-second
# .srt and not the two 2-second lavfi sources, so the container duration goes
# 1.021 s -> 2.023 s (measured). The stream set is identical either way, and both
# consumers asserting on this fixture assert stream TYPES only -- so if you ever
# add a duration assertion here, that is the number to expect.
# Skips the calling test if ffmpeg is unavailable. Returns the file path.
make_subtitle_video <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  srt <- withr::local_tempfile(fileext = ".srt", .local_envir = env)
  writeLines(c("1", "00:00:00,000 --> 00:00:01,000", "hello", ""), srt)
  path <- withr::local_tempfile(fileext = ".mkv", .local_envir = env)
  run_ffmpeg_fixture(paste(
    "-y -f lavfi -i testsrc=duration=2:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=2",
    sprintf('-i "%s"', srt),
    "-map 0:v -map 1:a -map 2:s -c:v libx264 -c:a aac -c:s srt",
    sprintf('-pix_fmt yuv420p "%s"', path)
  ))
  testthat::skip_if_not(file.exists(path),
                        "subtitle test video could not be generated")
  path
}

# Probe a media file's stream types via ffprobe, in stream order: a character
# vector of "video"/"audio"/"subtitle". Skips if ffprobe is unavailable.
stream_types <- function(path) {
  skip_if_no_ffprobe()
  trimws(ffprobe(sprintf(
    '-v error -show_entries stream=codec_type -of csv=p=0 "%s"', path
  )))
}

# Build an ffm pipeline WITHOUT ffm_files()'s file-readability check, so pure
# (binary-free) tests can assert compiled commands for named-but-absent files.
ffm_dry <- function(input, output) {
  new_ffm(input = input, output = output, overwrite = TRUE)
}

# Probe a media file's container duration (seconds) via ffprobe. Skips if
# ffprobe is unavailable. Returns a numeric scalar.
probe_duration <- function(path) {
  skip_if_no_ffprobe()
  out <- ffprobe(sprintf(
    '-v error -show_entries format=duration -of csv=p=0 "%s"', path
  ))
  as.numeric(out[[1]])
}

# Generate a short video (with audio) carrying known global metadata tags and a
# 90-degree rotation display matrix, so de-identification tests can assert those
# tags clear while the streams and rotation survive a stream copy. Two passes:
# a lavfi source cannot both synthesize and carry a rotation matrix in one
# output, so generate a plain clip then remux it with the rotation applied as an
# input option and the tags written on output. Skips if ffmpeg is unavailable.
make_tagged_video <- function(env = parent.frame()) {
  skip_if_no_ffmpeg()
  plain <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  run_ffmpeg_fixture(paste(
    "-y -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=1",
    sprintf('-shortest -pix_fmt yuv420p "%s"', plain)
  ))
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  run_ffmpeg_fixture(paste(
    sprintf('-y -display_rotation:v:0 90 -i "%s" -c copy', plain),
    '-metadata title="Secret Study" -metadata comment="participant 007"',
    '-metadata location="+40.7128-074.0060/"',
    '-metadata creation_time="2020-01-02T03:04:05.000000Z"',
    # A per-stream (video) identifying tag too, so tests can confirm the scrub
    # clears stream-level tags, not just container-level ones. FFmpeg's mov muxer
    # surfaces a per-stream title as a `name` stream tag.
    '-metadata:s:v:0 title="CAM-OPERATOR-JANE"',
    sprintf('"%s"', path)
  ))
  testthat::skip_if_not(file.exists(path),
                        "tagged test video could not be generated")
  path
}

# Probe a media file's container (format-level) metadata tags via ffprobe,
# returning a character vector of "key=value" lines (empty if none). Used to
# assert which tags a scrub clears. Skips if ffprobe is unavailable.
probe_format_tags <- function(path) {
  skip_if_no_ffprobe()
  out <- ffprobe(sprintf(
    '-v error -show_entries format_tags -of default=noprint_wrappers=1 "%s"',
    path
  ))
  sub("^TAG:", "", out[nzchar(out)])
}

# Probe every video/audio stream's metadata tags via ffprobe, returning a
# character vector of "key=value" lines across all streams (empty if none).
# Used to assert a scrub clears stream-level tags, not just container-level ones.
# Skips if ffprobe is unavailable.
probe_stream_tags <- function(path) {
  skip_if_no_ffprobe()
  out <- ffprobe(sprintf(
    '-v error -show_entries stream_tags -of default=noprint_wrappers=1 "%s"',
    path
  ))
  sub("^TAG:", "", out[nzchar(out)])
}

# Probe the rotation (degrees) recorded in a video stream's display matrix side
# data, or NA if none. Skips if ffprobe is unavailable.
probe_rotation <- function(path) {
  skip_if_no_ffprobe()
  out <- ffprobe(sprintf(
    paste('-v error -select_streams v:0',
          '-show_entries stream_side_data=rotation',
          '-of default=noprint_wrappers=1:nokey=1 "%s"'),
    path
  ))
  out <- out[nzchar(out)]
  if (length(out) == 0) NA_real_ else as.numeric(out[[1]])
}
