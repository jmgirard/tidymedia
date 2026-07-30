# Tests for the suite's own FFmpeg fixture runner (helper-media.R). The runner
# exists so that a fixture command which never terminates fails the run instead
# of stalling it (M46), and that guarantee needs a test of its own: no other
# test in the suite can show the limit is actually enforced.
#
# Note on what this can and cannot catch. A mutation that stops passing the
# limit through to system() makes this test HANG rather than go red -- the
# failure mode under test is non-termination, so there is no way to assert it
# from inside the same process. A hanging suite is itself the signal.

test_that("run_ffmpeg_fixture() errors when a command outruns its timeout", {
  skip_if_no_ffmpeg()
  # An unbounded 1080p60 lavfi encode: it cannot finish within the limit on any
  # machine, so reaching the limit is the only possible outcome.
  command <- paste(
    "-v error -y -f lavfi -i testsrc=duration=99999:size=1920x1080:rate=60",
    "-f null -"
  )
  start <- Sys.time()
  err <- expect_error(run_ffmpeg_fixture(command, timeout = 3))
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_lt(elapsed, 8)
  expect_match(conditionMessage(err), "ffmpeg", fixed = TRUE)
  expect_match(conditionMessage(err), "timed out after 3 seconds", fixed = TRUE)
  # And it must not leak WHAT ran: fixture commands carry temp paths, which an
  # error message is the wrong place for.
  expect_no_match(conditionMessage(err), "testsrc", fixed = TRUE)
  expect_no_match(conditionMessage(err), tempdir(), fixed = TRUE)
})

test_that("run_ffmpeg_fixture() returns FFmpeg's output on a command that finishes", {
  skip_if_no_ffmpeg()
  out <- run_ffmpeg_fixture("-version")
  expect_type(out, "character")
  expect_match(out[[1]], "^ffmpeg version")
})
