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
    # A pre-existing output is one the snapshot saw before the run; either way
    # the run WROTE here, which is what makes it removable (D046). The written
    # content stands in for FFmpeg's zero-byte leftover.
    if (case$preexisting) writeLines("the caller's own file", path)
    before <- output_snapshot(path)
    Sys.sleep(0.01)
    writeLines("zero-byte stand-in", path)

    bullets <- remove_failed_output(path, case$overwrite, before)

    label <- sprintf("overwrite=%s preexisting=%s", case$overwrite,
                     case$preexisting)
    expect_false(file.exists(path), info = label)
    expect_named(bullets, "i")
    expect_match(bullets, "was removed")
  }
})

test_that("the removal spares a pre-existing output under overwrite = FALSE", {
  path <- withr::local_tempfile(fileext = ".mp3")
  writeLines("the caller's own file", path)
  before <- output_snapshot(path)
  # The guard holds even where FFmpeg DID write: it is a promise the package
  # made, not a behavior it observed (D046).
  Sys.sleep(0.01)
  writeLines("what a stray build might have written", path)

  bullets <- remove_failed_output(path, overwrite = FALSE, before = before)

  expect_true(file.exists(path))
  expect_named(bullets, "i")
  expect_match(bullets, "left as it was")
})

test_that("the removal says so when the file cannot be removed", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "stuck.mp3")
  writeLines("cannot be unlinked", path)
  before <- output_snapshot(path)
  Sys.sleep(0.01)
  writeLines("what the failed run wrote", path)
  Sys.chmod(dir, "0500")
  withr::defer(Sys.chmod(dir, "0700"))
  # The fixture is verified by ASKING whether the directory is writable, never
  # by attempting the unlink this test is about: a skip keyed on the outcome of
  # the operation under test destroys the fixture and hides a real regression
  # behind a green run (M63's lesson, and P1 of this milestone's review).
  tm_require_unwritable_dir(dir)

  bullets <- remove_failed_output(path, overwrite = TRUE, before = before)

  expect_true(file.exists(path))
  expect_named(bullets, "x")
  expect_match(bullets, "could not be removed")
})

test_that("nothing is claimed when the failed run wrote no file at all", {
  path <- withr::local_tempfile(fileext = ".mp3")
  expect_false(file.exists(path))
  expect_identical(
    remove_failed_output(path, overwrite = TRUE, before = character(0)),
    character(0)
  )
})

# AC9: a failure FFmpeg raises BEFORE it opens the output ---------------------
#
# An unknown encoder is refused at setup: FFmpeg prints "Error opening output
# file" and exits 8 with the output untouched -- not created, not truncated
# (measured 2026-08-09, ffmpeg 8.1.2 macOS: a 13-byte pre-existing file came
# back with the same md5 and the same mtime). Unknown filters and bad option
# values fail the same way. This is the case the milestone's first removal
# deleted a caller's file over, and the reason the rule is now "what this run
# wrote" rather than "the output path" (D046).

test_that("a failed run leaves an output FFmpeg never opened exactly as it was", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  writeLines("the caller's own file", outfile)
  before <- file.info(outfile)[c("size", "mtime")]

  cnd <- rlang::catch_cnd(
    ffm_run(ffm_codec(ffm_files(infile, outfile), video = "nosuchcodec")),
    "error"
  )

  expect_match(conditionMessage(cnd), "FFmpeg exited")
  expect_true(file.exists(outfile))
  expect_identical(readLines(outfile), "the caller's own file")
  expect_identical(file.info(outfile)[c("size", "mtime")], before)
  expect_match(conditionMessage(cnd), "left as it was")
})


# AC10: the removal deletes the named path and nothing beside it --------------
#
# unlink() expands wildcards by default, so the output's own NAME can reach a
# neighbour: unlink("a*.mp4") emptied a directory of aQQQ.mp4 and aXYZ.mp4, and
# unlink("out[1].mp4") deleted out1.mp4 and left out[1].mp4 (measured at M68's
# review). Both names are legal on every platform the package supports.

test_that("a wildcard in the output's name costs no neighbouring file", {
  dir <- withr::local_tempdir()
  target <- file.path(dir, "a*.mp4")
  neighbours <- file.path(dir, c("aQQQ.mp4", "aXYZ.mp4"))
  for (p in c(target, neighbours)) writeLines("content", p)

  bullets <- remove_failed_output(target, overwrite = TRUE,
                                  before = character(0))

  expect_false(file.exists(target))
  expect_true(all(file.exists(neighbours)))
  expect_match(bullets, "was removed")
})

test_that("a bracketed output name is the file that goes", {
  dir <- withr::local_tempdir()
  target <- file.path(dir, "out[1].mp4")
  neighbour <- file.path(dir, "out1.mp4")
  for (p in c(target, neighbour)) writeLines("content", p)

  remove_failed_output(target, overwrite = TRUE, before = character(0))

  expect_false(file.exists(target))
  expect_true(file.exists(neighbour))
})


# AC10: a frame sequence is one output, and the run owns only its own frames --
#
# sample_frames()' output is an image2 %0Nd PATTERN, not a path, so
# file.exists() is false of it and a failed sampling run left every frame it had
# written. The rule applies to the SET the pattern matches in its own directory,
# and only to the members this run wrote -- an earlier run's frames, and
# anything else sharing the directory, survive.

test_that("a failed frame run removes its own frames and no others", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()          # 2 s of video, sampled below at 5 fps
  dir <- withr::local_tempdir()

  # THE TRIGGER: a DIRECTORY where the third frame's file has to go. FFmpeg
  # writes frames 1 and 2, cannot open the third, and exits non-zero (measured
  # 2026-08-09, ffmpeg 8.1.2 macOS: exit 235 with two frames on disk). Opening a
  # directory as a file fails on every platform the package supports, so this
  # does not depend on the build the way a codec refusal does (M45's lesson).
  blocker <- file.path(dir, "f_000003.png")
  dir.create(blocker)
  # A previous run's frame -- matches the pattern, was not written by this run.
  earlier <- file.path(dir, "f_000010.png")
  bystander <- file.path(dir, "notes.txt")   # does not match the pattern
  writeLines("an earlier run's frame", earlier)
  writeLines("nothing to do with frames", bystander)
  earlier_before <- file.info(earlier)[c("size", "mtime")]

  expect_error(
    sample_frames(infile, dir, fps = 5, prefix = "f"),
    "FFmpeg exited"
  )

  # What this run wrote is gone...
  expect_false(file.exists(file.path(dir, "f_000001.png")))
  expect_false(file.exists(file.path(dir, "f_000002.png")))
  # ...and nothing else is touched: not the blocking directory, not an earlier
  # run's frame, not a file that never matched the pattern.
  expect_true(dir.exists(blocker))
  expect_true(file.exists(earlier))
  expect_identical(file.info(earlier)[c("size", "mtime")], earlier_before)
  expect_true(file.exists(bystander))
})


test_that("the abort names the output it removed", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp3")
  cnd <- rlang::catch_cnd(ffm_run(failing_copy(infile, outfile)), "error")
  expect_match(conditionMessage(cnd), "was removed")
  expect_match(conditionMessage(cnd), basename(outfile), fixed = TRUE)
})


# AC4/AC5: the disposition reaches the two paths that wrap ffm_run() ---------

test_that("the multi-track separation abort carries the removal in its parent", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  audiofile <- withr::local_tempfile(fileext = ".mp3")
  videofile <- withr::local_tempfile(fileext = ".mp4")

  cnd <- rlang::catch_cnd(
    separate_audio_video(infile, audiofile, videofile), "error"
  )

  # The verb REPLACES ffm_run()'s condition with its own multi-track diagnostic
  # and keeps the original only as `parent` (R/ffmpeg.R:636-653), so the removal
  # sentence reaches this caller through the chain rather than the top message.
  # Asserting the class as well as the text keeps this from passing on some
  # other error that happens to mention a removal (M54's lesson).
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  expect_match(conditionMessage(cnd$parent), "was removed")
  expect_false(file.exists(audiofile))
})

test_that("a failed batch row loses its output and a good row keeps its own", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  bad <- withr::local_tempfile(fileext = ".mp3")   # AAC copy into mp3: refused
  good <- withr::local_tempfile(fileext = ".m4a")  # AAC copy into m4a: fine
  jobs <- tibble::tibble(input = c(infile, infile), output = c(bad, good))

  res <- ffm_batch(jobs, function(input, output) failing_copy(input, output))

  expect_identical(res$success, c(FALSE, TRUE))
  expect_false(file.exists(bad))
  expect_true(file.exists(good))
})
