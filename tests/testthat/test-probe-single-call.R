# M52: probe_one() reads the container and every stream from ONE FFprobe call.
#
# The spawn count is asserted by a mock that COUNTS invocations and delegates to
# the real run_program(), never by timing and never by a mock that stop()s:
# probe_one()'s callers catch, so an erroring mock proves only that something
# caught something (M44's lesson). Delegating also keeps the returned tibbles
# real, so a test that counts one spawn cannot be counting one spawn of a call
# that returned nothing.

# Count run_program() invocations while `expr` runs, returning the count. The
# real binding is captured before the mock replaces it, so the delegate is the
# genuine function and not the mock recursing into itself.
count_spawns <- function(expr) {
  real <- run_program
  n <- 0L
  testthat::local_mocked_bindings(
    run_program = function(...) {
      n <<- n + 1L
      real(...)
    }
  )
  force(expr)
  n
}

test_that("probe_one() spawns exactly one FFprobe process on a 5-stream file", {
  skip_if_no_ffprobe()
  # Built before the mock is installed: the fixture helper probes the file
  # itself to assert its own stream count, and those probes are not the
  # subject of this count.
  infile <- make_multitrack_subtitle_video()
  res <- NULL
  n <- count_spawns(res <- probe_one(infile))
  # Pre-change this was nb_streams + 1 = 6, recorded in the T1 baseline.
  expect_equal(n, 1L)
  expect_equal(nrow(res$streams), 5L)
})

test_that("probe_one() spawns once on the early-return paths too", {
  skip_if_no_ffprobe()
  video_only <- make_silent_video()
  audio_only <- make_silent_audio()

  n_video <- count_spawns(v <- probe_one(video_only))
  expect_equal(n_video, 1L)

  n_audio <- count_spawns(a <- probe_one(audio_only))
  expect_equal(n_audio, 1L)
})

test_that("probe_one() spawns once on an unprobeable file and returns NULL", {
  skip_if_no_ffprobe()
  missing <- file.path(tempdir(), "tm-does-not-exist-m52.mkv")
  res <- "unset"
  n <- count_spawns(res <- probe_one(missing))
  expect_equal(n, 1L)
  expect_null(res)
})
