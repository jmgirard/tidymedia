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
