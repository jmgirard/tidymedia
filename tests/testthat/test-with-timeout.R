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

# What `seconds` may be -------------------------------------------------------
#
# The wrapper and the option must not disagree about what a usable limit is: a
# caller who can pass 0.5 to one and not the other has two rules to learn, and
# the one that accepted it would hand base R a value it reads as "no limit"
# (M69/D047). So the probe vector is scored against the option's own verdict
# rather than against a hand-written list of expectations.

tm_seconds_probes <- list(
  0, 1L, 60, 0.5, -1, NA, NA_real_, "2", c(1, 2), Inf, TRUE,
  integer(0), factor("2")
)

tm_probe_label <- function(v) paste(class(v)[[1]], format(v)[1], length(v))

tm_accepts <- function(f) {
  tryCatch({
    f()
    TRUE
  }, error = function(e) FALSE)
}

test_that("with_timeout() accepts exactly the values the option accepts", {
  verdicts <- vapply(tm_seconds_probes, function(v) {
    by_option <- tm_accepts(function() {
      withr::with_options(list(tidymedia.timeout = v), resolve_timeout())
    })
    by_call <- tm_accepts(function() with_timeout(NULL, v))
    expect_equal(by_call, by_option, info = tm_probe_label(v))
    by_option
  }, logical(1))
  # Not a vacuous agreement: the probe vector has to contain both verdicts, or
  # a wrapper that refused everything would agree with an option that refused
  # everything.
  expect_true(any(verdicts))
  expect_true(any(!verdicts))
})

test_that("a refused seconds stops the call before expr is evaluated", {
  dir <- withr::local_tempdir()
  refused <- Filter(
    function(v) !tm_accepts(function() with_timeout(NULL, v)),
    tm_seconds_probes
  )
  # The refusal set must not be empty, or this test asserts nothing at all.
  expect_gt(length(refused), 0)
  for (i in seq_along(refused)) {
    marker <- file.path(dir, sprintf("ran%d", i))
    expect_error(with_timeout(file.create(marker), refused[[i]]))
    expect_false(file.exists(marker))
  }
})

test_that("the refusal names seconds, not the option", {
  msg <- cli::ansi_strip(conditionMessage(
    expect_error(with_timeout(NULL, 0.5))
  ))
  expect_match(msg, "seconds", fixed = TRUE)
  # The caller wrote an argument, not an option: naming the option here would
  # send them to fix something they never set.
  expect_false(grepl("tidymedia.timeout", msg, fixed = TRUE))
})

test_that("seconds is required, and NULL is not a limit", {
  dir <- withr::local_tempdir()
  missing_marker <- file.path(dir, "missing")
  null_marker <- file.path(dir, "null")
  expect_error(with_timeout(file.create(missing_marker)))
  expect_error(with_timeout(file.create(null_marker), NULL))
  expect_false(file.exists(missing_marker))
  expect_false(file.exists(null_marker))
})

# Where the limit has to arrive ----------------------------------------------
#
# The wrapper writes one process-global option, so nothing is threaded and
# every reader picks it up by construction. "By construction" is exactly the
# kind of claim that stops being true when someone adds a fifth spawn site, so
# each recorded site is driven and asked what limit it was handed.
#
# `guard_timeout()` is mocked rather than a binary run: its `expr` argument is
# lazy, so the mock never forces the `system()`/`system2()` call underneath and
# the cell measures the value, not the runner's PATH.

tm_spawn_call_specs <- list(
  ffmpeg = function() ffmpeg("-version"),
  ffprobe = function() ffprobe("-version"),
  mediainfo = function() mediainfo("--Version"),
  run_program = function() run_program("/bin/echo", "hi", program = "FFmpeg")
)

test_that("every recorded spawn site has a call to drive it", {
  # A new site added to the package without a cell here fails rather than
  # being quietly skipped.
  expect_setequal(names(tm_spawn_call_specs), tm_spawn_sites())
})

test_that("each spawn site is handed the per-call limit", {
  withr::local_options(tidymedia.timeout = NULL)
  seen <- NULL
  testthat::local_mocked_bindings(
    guard_timeout = function(program, limit, expr, ...) {
      seen <<- limit
      character(0)
    },
    .package = "tidymedia"
  )
  for (nm in names(tm_spawn_call_specs)) {
    seen <- NULL
    with_timeout(tm_spawn_call_specs[[nm]](), 7)
    expect_identical(seen, 7, info = nm)

    # The control: with the session option unset and no wrapper, the same site
    # is handed the no-limit sentinel. Without it a site that hard-coded 7
    # would pass the assertion above.
    seen <- NULL
    tm_spawn_call_specs[[nm]]()
    expect_identical(seen, 0, info = nm)
  }
})

test_that("ffm_batch()'s up-front limit check reads the per-call value", {
  # ffm_batch() resolves the limit before it dispatches anything, so that a bad
  # value is refused once in the process that can name the caller (M071/D050).
  # That read is not a spawn site and would not be covered above.
  withr::local_options(tidymedia.timeout = NULL)
  real <- resolve_timeout
  seen <- numeric()
  testthat::local_mocked_bindings(
    resolve_timeout = function(...) {
      value <- real(...)
      seen <<- c(seen, value)
      value
    },
    .package = "tidymedia"
  )
  dir <- withr::local_tempdir()
  input <- file.path(dir, "in.mp4")
  file.create(input)
  jobs <- tibble::tibble(input = input, output = file.path(dir, "out.mp4"))
  build <- function(input, output, ...) ffm(input, output)

  with_timeout(ffm_batch(jobs, build, run = FALSE), 7)
  expect_equal(seen, 7)

  seen <- numeric()
  ffm_batch(jobs, build, run = FALSE)
  expect_equal(seen, 0)
})
