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


# AC8 / AC3: the helper's four cases, and what the abort says ----------------
#
# The `overwrite = FALSE` + pre-existing cell is unreachable through FFmpeg on
# this build -- `-n` against an existing output prints "already exists. Exiting."
# and exits 0 (measured 2026-08-09, ffmpeg 8.1.2 macOS), so ffm_run() never
# reaches its abort and the branch never runs. It is exercised directly here
# instead, which also keeps the guard covered on a build that reports that
# refusal as a failure -- the case the guard exists for.

test_that("the removal deletes a failed run's output in three of four cases", {
  cases <- list(
    list(overwrite = TRUE,  preexisting = TRUE),
    list(overwrite = TRUE,  preexisting = FALSE),
    list(overwrite = FALSE, preexisting = FALSE)
  )
  for (case in cases) {
    path <- withr::local_tempfile(fileext = ".mp3")
    writeLines("zero-byte stand-in", path)
    bullets <- remove_failed_output(path, case$overwrite, case$preexisting)
    expect_false(
      file.exists(path),
      info = sprintf("overwrite=%s preexisting=%s", case$overwrite,
                     case$preexisting)
    )
    expect_named(bullets, "i")
    expect_match(bullets, "was removed")
  }
})

test_that("the removal spares a pre-existing output under overwrite = FALSE", {
  path <- withr::local_tempfile(fileext = ".mp3")
  writeLines("the caller's own file", path)

  bullets <- remove_failed_output(path, overwrite = FALSE, preexisting = TRUE)

  expect_true(file.exists(path))
  expect_identical(readLines(path), "the caller's own file")
  expect_named(bullets, "i")
  expect_match(bullets, "left as it was")
})

test_that("the removal says so when the file cannot be removed", {
  skip_on_os("windows")  # chmod reaches only the read-only bit there
  skip_if(unname(Sys.info()["user"]) == "root", "root writes regardless")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "stuck.mp3")
  writeLines("cannot be unlinked", path)
  Sys.chmod(dir, "0500")
  withr::defer(Sys.chmod(dir, "0700"))
  skip_if(unlink(path) == 0L && !file.exists(path),
          "this platform removed the file from a read-only directory")

  bullets <- remove_failed_output(path, overwrite = TRUE, preexisting = FALSE)

  expect_true(file.exists(path))
  expect_named(bullets, "x")
  expect_match(bullets, "could not be removed")
})

test_that("nothing is claimed when the failed run wrote no file at all", {
  path <- withr::local_tempfile(fileext = ".mp3")
  expect_false(file.exists(path))
  expect_identical(
    remove_failed_output(path, overwrite = TRUE, preexisting = FALSE),
    character(0)
  )
})

test_that("the abort names the output it removed", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp3")
  cnd <- rlang::catch_cnd(ffm_run(failing_copy(infile, outfile)), "error")
  expect_match(conditionMessage(cnd), "was removed")
  expect_match(conditionMessage(cnd), basename(outfile), fixed = TRUE)
})
