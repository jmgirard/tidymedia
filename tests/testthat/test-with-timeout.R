# M072: one call carries its own limit.
#
# The session-wide option (M69/D047) answers "how long may anything take in
# this session"; `with_timeout()` answers "how long may THIS take", without
# disturbing the answer to the first. Every test below therefore states what
# the session option was before and after, because the second half of the
# promise is that the call leaves it exactly as it found it.

# The seeds a per-call limit has to reach -------------------------------------
#
# `tm_timeout_domain()` (M70) is the set of exports a timeout can be seen
# THROUGH; this is the smaller set where a process is actually started, and it
# is the one a limit has to arrive at. Recorded so a new spawn site reddens it:
# a fifth site added without carrying the limit is exactly the silent hang the
# option seam was built to stop.

test_that("the package starts a process at exactly the four recorded sites", {
  expect_setequal(
    tm_spawn_sites(),
    c("ffmpeg", "ffprobe", "mediainfo", "run_program")
  )
})

test_that("spawn-site membership is read off the body, not off the record", {
  # An empty seed set must collapse the result. Without this the assertion
  # above could be satisfied by a helper that returned its own recorded list.
  expect_equal(tm_spawn_sites(seeds = character(0)), character(0))
  # And a seed no body names adds nobody.
  expect_equal(tm_spawn_sites(seeds = "no_such_primitive"), character(0))
})

# What the call carries, and what it hands back -------------------------------

test_that("expr is evaluated once, in the caller's frame, and its value returned", {
  withr::local_options(tidymedia.timeout = NULL)
  runs <- 0
  factor_here <- 3
  twice <- function() {
    runs <<- runs + 1
    factor_here * 2
  }
  expect_equal(with_timeout(twice(), 30), 6)
  # A promise is forced once, and the wrapper must not force it again on its
  # way out: a second evaluation would re-run whatever the caller wrapped.
  expect_equal(runs, 1)

  # Written where the caller wrote it, not inside the wrapper: this assignment
  # has to land in THIS frame, and the value it captures has to be the limit
  # in force while it ran.
  with_timeout(seen_inside <- getOption("tidymedia.timeout"), 30)
  expect_equal(seen_inside, 30)
})

test_that("the limit in force inside the call is as.numeric(seconds)", {
  withr::local_options(tidymedia.timeout = NULL)
  for (s in list(0, 1L, 60)) {
    expect_identical(
      with_timeout(getOption("tidymedia.timeout"), s),
      as.numeric(s)
    )
  }
})

test_that("a session limit is displaced for the call and restored after it", {
  withr::local_options(tidymedia.timeout = 99)
  expect_identical(with_timeout(getOption("tidymedia.timeout"), 2), 2)
  expect_equal(getOption("tidymedia.timeout"), 99)
})

# AC2 -- the three exits, against both prior states ---------------------------
#
# The erroring exits are the ones on.exit() exists for: a wrapper that restored
# on the returning path alone would leave the caller's session bounded by a
# limit they asked for one call, and a timeout is the abort most likely to be
# the exit taken.

tm_exit_paths <- list(
  returns = function() "value",
  aborts = function() rlang::abort("expr failed", class = "tm_test_failure"),
  times_out = function() abort_timeout("FFmpeg", 2)
)

test_that("a previously-set limit comes back on every exit", {
  for (path in names(tm_exit_paths)) {
    withr::local_options(tidymedia.timeout = 99)
    before <- getOption("tidymedia.timeout")
    f <- tm_exit_paths[[path]]
    if (identical(path, "returns")) {
      expect_equal(with_timeout(f(), 2), "value")
    } else {
      expect_error(with_timeout(f(), 2))
    }
    expect_equal(getOption("tidymedia.timeout"), before, info = path)
  }
})

test_that("a previously-unset limit is unset again on every exit", {
  for (path in names(tm_exit_paths)) {
    withr::local_options(tidymedia.timeout = NULL)
    f <- tm_exit_paths[[path]]
    if (identical(path, "returns")) {
      expect_equal(with_timeout(f(), 2), "value")
    } else {
      expect_error(with_timeout(f(), 2))
    }
    # getOption()'s default fires only when the NAME IS ABSENT, so this
    # distinguishes "unset" from "set to NULL" -- the distinction that makes
    # restoring an unset option a real claim rather than a coincidence.
    expect_equal(
      getOption("tidymedia.timeout", default = "absent"), "absent",
      info = path
    )
  }
})

test_that("each exit path signals what its name says it does", {
  # The loops above accept any error. Which error matters: a `times_out` cell
  # that aborted for some other reason would still restore the option and still
  # pass, certifying nothing about the timeout path.
  expect_equal(tm_exit_paths$returns(), "value")
  expect_error(tm_exit_paths$aborts(), class = "tm_test_failure")
  expect_error(tm_exit_paths$times_out(), class = "tidymedia_timeout")
})
