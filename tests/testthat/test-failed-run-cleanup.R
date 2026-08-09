# M68: a run that fails removes the broken output it wrote.
#
# WHY THIS TRIGGER. The failure below is an AAC-to-MP3 STREAM COPY: the mp3
# muxer refuses a stream it cannot hold ("Invalid audio stream. Exactly one MP3
# audio stream is required", exit 234) and leaves a zero-byte file behind. No
# FFmpeg build can copy AAC into MP3, so the trigger is version-independent.
# The obvious alternative -- provoking the adts muxer's multi-stream refusal --
# is NOT: ffmpeg 6.1.1, which ubuntu-latest ships, writes that file and exits 0,
# which is how seven M45 tests went green on CI while catching no condition at
# all (see the portability note at the top of test-separate-av-multitrack.R).
#
# Both pre-states are covered because FFmpeg reaches them differently: with no
# file at the path it creates one and truncates it to zero on failure, and with
# a file already there it truncates THAT to zero before failing (measured
# 2026-08-09, ffmpeg 8.1.2 macOS). Either way the caller is left holding an
# empty file, so both must come back absent.

failing_copy <- function(infile, outfile) {
  ffm_codec(ffm_map(ffm_files(infile, outfile), "0:a:0"), audio = "copy")
}

test_that("a failed run removes an output path it created itself", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp3")
  expect_false(file.exists(outfile))

  expect_error(ffm_run(failing_copy(infile, outfile)), "FFmpeg exited")
  expect_false(file.exists(outfile))
})

test_that("a failed run removes an output path that already existed", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp3")
  writeLines("not a media file", outfile)
  expect_true(file.exists(outfile))

  expect_error(ffm_run(failing_copy(infile, outfile)), "FFmpeg exited")
  expect_false(file.exists(outfile))
})
