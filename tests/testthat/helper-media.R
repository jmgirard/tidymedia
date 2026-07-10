# Compile an ffm pipeline and replace each input path with a stable "<inN>"
# token, so full-command snapshots are deterministic across machines and CI
# (temp paths and Windows backslashes would otherwise leak in).
compile_scrubbed <- function(p) {
  cmd <- ffm_compile(p)
  for (i in seq_along(p$input)) {
    cmd <- gsub(p$input[[i]], sprintf("<in%d>", i), cmd, fixed = TRUE)
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
