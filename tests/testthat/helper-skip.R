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

# Skip unless this FFmpeg can actually nvenc-encode at run time. has_nvenc() is
# only a cheap "is the encoder listed" check: a CI image can list h264_nvenc yet
# have no libcuda / GPU (the encode then dies mid-run). So probe with a tiny real
# encode and skip unless it exits 0 -- guarding execution tests against a listed-
# but-unusable encoder.
skip_if_no_nvenc <- function() {
  testthat::skip_if_not(
    nzchar(Sys.which("ffmpeg")),
    message = "ffmpeg binary not available"
  )
  testthat::skip_if_not(has_nvenc("h264"), message = "nvenc encoder not listed")
  probe <- suppressWarnings(tryCatch(
    system2(
      "ffmpeg",
      c("-hide_banner", "-loglevel", "error", "-f", "lavfi",
        "-i", "nullsrc=s=64x48:d=0.1", "-c:v", "h264_nvenc",
        "-frames:v", "1", "-f", "null", "-"),
      stdout = FALSE, stderr = FALSE
    ),
    error = function(e) 1L
  ))
  testthat::skip_if_not(
    identical(as.integer(probe), 0L),
    message = "nvenc listed but not usable at run time"
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

# M68 -- the gate in front of the unremovable-output case.
#
# A test whose fixture is "a file that cannot be deleted" must not establish
# that by TRYING to delete it: the attempt is the operation under test, and on a
# platform where it succeeds the fixture is gone and the evidence vanishes
# behind a green run (M63's review made the same call for unreadable inputs).
# Ask the filesystem instead -- write permission on the containing directory is
# what unlink() needs -- and skip only where the question cannot be posed:
# Windows, whose chmod reaches only the read-only bit, and a process running as
# root, which writes regardless. Anywhere else, a directory still writable after
# Sys.chmod("0500") means something is wrong with the run, not with the
# platform.
tm_require_unwritable_dir <- function(dir) {
  if (file.access(dir, mode = 2) != 0) return(invisible(dir))
  windows <- .Platform$OS.type == "windows"
  root <- !windows && identical(
    tryCatch(as.integer(system("id -u", intern = TRUE)),
             error = function(e) NA_integer_),
    0L
  )
  if (windows || root) {
    testthat::skip("this platform cannot express an undeletable file")
  }
  testthat::fail("the fixture directory is still writable after chmod 0500")
}
