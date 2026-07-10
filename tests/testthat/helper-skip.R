# Skip helpers for tests that shell out to external CLIs. Execution tests must
# skip cleanly on machines (and CI images) where the binary is absent.

skip_if_no_ffmpeg <- function() {
  testthat::skip_if_not(
    nzchar(Sys.which("ffmpeg")),
    message = "ffmpeg binary not available"
  )
}

skip_if_no_ffprobe <- function() {
  testthat::skip_if_not(
    nzchar(Sys.which("ffprobe")),
    message = "ffprobe binary not available"
  )
}

skip_if_no_mediainfo <- function() {
  testthat::skip_if_not(
    nzchar(Sys.which("mediainfo")),
    message = "mediainfo binary not available"
  )
}

# Create an empty, readable temporary input file so builder functions that check
# file readability (e.g. ffm_files()) accept it. Registers cleanup on the given
# environment (default: the calling test).
make_input <- function(ext = "mp4", env = parent.frame()) {
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = env)
  file.create(path)
  path
}
