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
