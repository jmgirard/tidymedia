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
  ffmpeg(command)
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
  ffmpeg(command)
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
  ffmpeg(command)
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
  ffmpeg(command)
  testthat::skip_if_not(file.exists(path),
                        "silent test audio could not be generated")
  path
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
  ffmpeg(paste(
    "-y -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=1",
    sprintf('-shortest -pix_fmt yuv420p "%s"', plain)
  ))
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  ffmpeg(paste(
    sprintf('-y -display_rotation:v:0 90 -i "%s" -c copy', plain),
    '-metadata title="Secret Study" -metadata comment="participant 007"',
    '-metadata location="+40.7128-074.0060/"',
    '-metadata creation_time="2020-01-02T03:04:05.000000Z"',
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
