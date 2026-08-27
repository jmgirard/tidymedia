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
# The probe vector and the option's verdict on it live in
# `helper-timeout-probes.R`, shared with `local_timeout()`'s refusal test.

test_that("with_timeout() accepts exactly the values the option accepts", {
  verdicts <- vapply(tm_seconds_probes, function(v) {
    by_option <- tm_option_accepts(v)
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

# M073 AC1/AC2 -- both formals guarded by the package, not by base R --------
#
# `seconds` has always been checked eagerly; `expr` was left to base R, whose
# "argument \"expr\" is missing, with no default" names the package's internal
# parameter at a caller who wrote a call, not a definition. The cases below are
# derived from `formals()` rather than written out, so a third formal added
# later is guarded or reddens this.

# One valid value per formal. The map is checked against `formals()` below, so
# it cannot silently fall behind the signature.
tm_valid_formals <- list(expr = NULL, seconds = 2)

test_that("an omitted expr is refused by the package, not by base R", {
  err <- expect_error(with_timeout(seconds = 5), class = "rlang_error")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "expr", fixed = TRUE)
  expect_false(
    grepl("argument \"expr\" is missing, with no default", msg, fixed = TRUE)
  )
})

test_that("every formal of with_timeout() is guarded alike", {
  formal_names <- names(formals(with_timeout))
  # Non-vacuity, both ways: the map must cover the real signature, and there
  # must be more than one formal or "alike" compares nothing.
  expect_setequal(formal_names, names(tm_valid_formals))
  expect_gt(length(formal_names), 1)

  for (omitted in formal_names) {
    supplied <- tm_valid_formals[setdiff(formal_names, omitted)]
    # Against both prior states, because the regression clause below is a claim
    # about the caller's session and "unset" and "set" are different facts.
    for (prior in list(NULL, 99)) {
      withr::local_options(tidymedia.timeout = prior)
      before <- getOption("tidymedia.timeout", default = "absent")
      err <- expect_error(
        do.call(with_timeout, supplied),
        class = "rlang_error"
      )
      msg <- cli::ansi_strip(conditionMessage(err))
      expect_match(msg, omitted, fixed = TRUE, info = omitted)
      # Regression clause: holds today -- the refusals fire before the option
      # is written -- and is pinned here so it keeps holding.
      expect_equal(
        getOption("tidymedia.timeout", default = "absent"),
        before,
        info = paste(omitted, format(prior))
      )
    }
  }
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

# A real hung program, bounded by the call and not by the session -------------
#
# The cells above prove the limit is PASSED. This one proves it BITES when the
# session sets no limit at all, which is the case the session-wide option
# cannot cover.
#
# The hang is M69's FIFO nobody writes to: FFmpeg blocks on its header forever
# rather than racing the machine's encoding speed. Slow on Linux and the
# slowness is the feature -- base R escalates SIGINT -> SIGTERM -> SIGKILL
# across limit + 40 s, so this costs ~42 s there against ~2 s on macOS, and
# skips on CRAN with the rest of M69's execution cells.

# tm_release_fifo(): the cell's own outer bound.
#
# Without it, a `with_timeout()` that established nothing would leave FFmpeg
# blocked on a FIFO with no writer and no limit -- and the test would not fail,
# it would hang the runner forever. A background shell opens the FIFO for
# writing well past any limit this test allows and closes it again, so FFmpeg
# reaches EOF and exits. The failure is then an ordinary one: no
# `tidymedia_timeout` abort, or an elapsed time over budget.
#
# M073: the writer is CANCELLED when the frame that armed it ends. The first
# version slept `after` seconds unconditionally, so a suite that finished in two
# seconds still left a 90-second `sleep` behind -- one per cell, outliving the
# process that wanted it. Killing a recorded PID does not solve it: `system(wait
# = FALSE)` hands back the shell's exit status, not a PID, and killing the
# subshell orphans its `sleep` anyway (measured at plan time). So the shell polls
# for a cancel file instead and the arming frame touches that file on its way
# out, by any exit route. What is left running is at most one second-scale
# `sleep`, which ends on its own.
#
# `marker` is a token unique to this call, carried in the command line (via the
# cancel path) so a test can find the process with `pgrep -f` and watch it go.
# It is returned, invisibly, for exactly that purpose.
tm_release_fifo <- function(path, after = 90, envir = parent.frame()) {
  marker <- basename(tempfile("tm_fifo_"))
  cancel <- file.path(tempdir(), paste0(marker, ".cancel"))
  withr::defer(file.create(cancel), envir = envir)
  # No trailing `&` of our own: `wait = FALSE` is what backgrounds this, and
  # base R appends the `&` itself -- writing a second one is a shell syntax
  # error, which leaves no writer at all and the bound unarmed (measured
  # 2026-08-27: FFmpeg was still blocked seven minutes in). The enclosing
  # parentheses are load-bearing for the same reason: the appended `&` binds to
  # the LAST command of the string, so without them the poll loop runs in the
  # foreground and blocks R for the full `after` (measured 2026-08-27: 91.8 s
  # against 1.1 s with them).
  system(
    sprintf(
      paste(
        "(i=0; while [ $i -lt %d ]; do [ -f %s ] && exit 0;",
        "i=$((i+1)); sleep 1; done; [ -p %s ] && : > %s) >/dev/null 2>&1"
      ),
      after, shQuote(cancel), shQuote(path), shQuote(path)
    ),
    wait = FALSE
  )
  invisible(marker)
}

# tm_pgrep(): the process table, asked about one marker.
#
# `pgrep` exits 1 with no output when nothing matches, which system2() surfaces
# as a `status` attribute rather than an empty result, so the two are folded
# together here.
tm_pgrep <- function(marker) {
  out <- suppressWarnings(
    system2("pgrep", c("-f", marker), stdout = TRUE, stderr = FALSE)
  )
  if (!is.null(attr(out, "status"))) character(0) else out
}

tm_wait_for_pgrep <- function(marker, present, limit = 5) {
  start <- Sys.time()
  repeat {
    if ((length(tm_pgrep(marker)) > 0) == present) return(TRUE)
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > limit) {
      return(FALSE)
    }
    Sys.sleep(0.2)
  }
}

# AC3 -- the writer is reaped with its frame -----------------------------------
#
# Each case arms the helper inside a frame, checks the process is really there
# (without that the "gone" assertion below would pass against a helper that
# started nothing at all), then leaves the frame by a different route and waits
# for it to go.

test_that("no process tm_release_fifo() starts outlives the frame", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("pgrep")), "pgrep is not on the PATH")

  # Never created: the shell only looks at this path after `after` seconds, and
  # every case here cancels long before that.
  path <- file.path(withr::local_tempdir(), "no-such-fifo")

  armed <- function(marker) {
    expect_true(tm_wait_for_pgrep(marker, present = TRUE), info = "armed")
  }

  # Case 1: the frame returns.
  by_return <- function() {
    marker <- tm_release_fifo(path)
    armed(marker)
    marker
  }
  expect_true(tm_wait_for_pgrep(by_return(), present = FALSE))

  # Case 2: the frame aborts.
  aborted_marker <- NULL
  by_abort <- function() {
    aborted_marker <<- tm_release_fifo(path)
    armed(aborted_marker)
    rlang::abort("frame failed", class = "tm_test_failure")
  }
  expect_error(by_abort(), class = "tm_test_failure")
  expect_true(tm_wait_for_pgrep(aborted_marker, present = FALSE))

  # Case 3: one frame arms the helper twice. Two independent cancel files, both
  # deferred on the same frame, so neither writer may survive it.
  both_markers <- NULL
  twice <- function() {
    both_markers <<- c(tm_release_fifo(path), tm_release_fifo(path))
    for (marker in both_markers) armed(marker)
    invisible(NULL)
  }
  twice()
  expect_length(unique(both_markers), 2)
  for (marker in both_markers) {
    expect_true(tm_wait_for_pgrep(marker, present = FALSE), info = marker)
  }
})

test_that("a per-call limit kills a hung program with no session limit set", {
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = NULL)
  tm_release_fifo(blocked)

  start <- Sys.time()
  err <- expect_error(
    with_timeout(
      ffmpeg(paste("-y -i", shQuote(blocked), shQuote(out))),
      2
    ),
    class = "tidymedia_timeout"
  )
  # 2 + base R's 40-second escalation ladder, with room to spare; the outer
  # bound above is far outside this, so a cell that reaches it fails here.
  expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 60)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFmpeg")
  expect_match(msg, "2 seconds")
  # And the session is no more bounded afterwards than it was before.
  expect_equal(getOption("tidymedia.timeout", default = "absent"), "absent")
})
