# M073: bounding the rest of a frame, rather than a wrapped expression.
#
# `with_timeout(expr, seconds)` takes the expression; this takes the frame. The
# promise is the same in both halves -- the limit is in force from the call to
# the end of the frame, and the caller's prior state is back afterwards -- so
# every cell below states what the session option was before and after.

tm_local_priors <- list(unset = NULL, set = 99)

# What "before" reads as, distinguishing an unset option from one set to NULL:
# getOption()'s default fires only when the NAME IS ABSENT.
tm_limit_now <- function() getOption("tidymedia.timeout", default = "absent")

# AC5 -- the limit holds to the end of the frame, then the caller's is back ----

test_that("the limit holds for the rest of the frame and is undone on return", {
  for (prior in names(tm_local_priors)) {
    withr::local_options(tidymedia.timeout = tm_local_priors[[prior]])
    before <- tm_limit_now()

    seen <- NULL
    f <- function() {
      # Nothing before the call sees it; everything after does.
      early <- tm_limit_now()
      local_timeout(5)
      seen <<- list(early = early, late = getOption("tidymedia.timeout"))
      "value"
    }
    expect_equal(f(), "value", info = prior)
    expect_equal(seen$late, 5, info = prior)
    expect_equal(seen$early, before, info = prior)
    expect_equal(tm_limit_now(), before, info = prior)
  }
})

test_that("the caller's limit is back when the frame aborts", {
  for (prior in names(tm_local_priors)) {
    withr::local_options(tidymedia.timeout = tm_local_priors[[prior]])
    before <- tm_limit_now()

    seen <- NULL
    f <- function() {
      local_timeout(5)
      seen <<- getOption("tidymedia.timeout")
      rlang::abort("frame failed", class = "tm_test_failure")
    }
    # Which error matters: a frame that aborted for some other reason would
    # still restore the option and still pass.
    expect_error(f(), class = "tm_test_failure")
    expect_equal(seen, 5, info = prior)
    expect_equal(tm_limit_now(), before, info = prior)
  }
})

test_that("two calls in one frame stack, and unwind to the caller's state", {
  for (prior in names(tm_local_priors)) {
    withr::local_options(tidymedia.timeout = tm_local_priors[[prior]])
    before <- tm_limit_now()

    f <- function() {
      local_timeout(1)
      first <- getOption("tidymedia.timeout")
      local_timeout(2)
      c(first = first, second = getOption("tidymedia.timeout"))
    }
    # The second call displaces the first for the rest of the frame; both are
    # undone together at its end, back to what the CALLER had -- not to the
    # first call's value, which would be the wrong end of the stack.
    expect_equal(f(), c(first = 1, second = 2), info = prior)
    expect_equal(tm_limit_now(), before, info = prior)
  }
})

test_that("a non-default .local_envir bounds that frame, not the caller's", {
  for (prior in names(tm_local_priors)) {
    withr::local_options(tidymedia.timeout = tm_local_priors[[prior]])
    before <- tm_limit_now()

    outer <- function() {
      target <- environment()
      inner <- function() {
        local_timeout(5, .local_envir = target)
        getOption("tidymedia.timeout")
      }
      # `inner` has returned. Had the limit been bound to ITS frame it would
      # already be undone here; bound to `outer`'s, it is still in force.
      c(inside_inner = inner(), after_inner = getOption("tidymedia.timeout"))
    }
    expect_equal(outer(), c(inside_inner = 5, after_inner = 5), info = prior)
    expect_equal(tm_limit_now(), before, info = prior)
  }
})

test_that("the limit in force is as.numeric(seconds)", {
  withr::local_options(tidymedia.timeout = NULL)
  for (s in list(0, 1L, 60)) {
    f <- function() {
      local_timeout(s)
      getOption("tidymedia.timeout")
    }
    expect_identical(f(), as.numeric(s), info = format(s))
  }
})

test_that("the prior value comes back invisibly, as withr's local_* do", {
  withr::local_options(tidymedia.timeout = 99)
  f <- function() local_timeout(5)
  expect_equal(f(), list(tidymedia.timeout = 99))
  expect_invisible(local_timeout(5, .local_envir = environment()))
})

# AC6 -- refused by the rule with_timeout() applies ----------------------------

test_that("local_timeout() accepts exactly the values with_timeout() does", {
  # Scored against the option's own verdict, the same reference
  # `with_timeout()`'s refusal test uses, over the same probe vector
  # (`helper-timeout-probes.R`).
  verdicts <- vapply(tm_seconds_probes, function(v) {
    by_option <- tm_option_accepts(v)
    by_call <- tm_accepts(function() {
      f <- function() local_timeout(v)
      f()
    })
    expect_equal(by_call, by_option, info = tm_probe_label(v))
    by_option
  }, logical(1))
  # Not a vacuous agreement: both verdicts have to occur, or a function that
  # refused everything would agree with an option that refused everything.
  expect_true(any(verdicts))
  expect_true(any(!verdicts))
})

test_that("0 and 1 are accepted, and 0 means no limit", {
  withr::local_options(tidymedia.timeout = 99)
  for (s in list(0, 1)) {
    f <- function() {
      local_timeout(s)
      getOption("tidymedia.timeout")
    }
    expect_identical(f(), as.numeric(s), info = format(s))
  }
})

test_that("a refused seconds leaves the caller's limit exactly as it was", {
  refused <- Filter(
    function(v) {
      !tm_accepts(function() {
        f <- function() local_timeout(v)
        f()
      })
    },
    tm_seconds_probes
  )
  # The refusal set must not be empty, or this test asserts nothing at all.
  expect_gt(length(refused), 0)
  for (prior in names(tm_local_priors)) {
    for (v in refused) {
      withr::local_options(tidymedia.timeout = tm_local_priors[[prior]])
      before <- tm_limit_now()
      f <- function() local_timeout(v)
      expect_error(f(), class = "rlang_error")
      expect_equal(tm_limit_now(), before, info = tm_probe_label(v))
    }
  }
})

test_that("the refusal names seconds, not the option", {
  f <- function() local_timeout(0.5)
  msg <- cli::ansi_strip(conditionMessage(expect_error(f())))
  expect_match(msg, "seconds", fixed = TRUE)
  # The caller wrote an argument, not an option: naming the option here would
  # send them to fix something they never set.
  expect_false(grepl("tidymedia.timeout", msg, fixed = TRUE))
})

test_that("NULL is refused, the asymmetry with the option seam intact", {
  # `options(tidymedia.timeout = NULL)` REMOVES the name, so resolve_timeout()
  # reads its 0 default and accepts it; `local_timeout(NULL)` is a caller naming
  # no limit at all. Recorded behaviour (D051), stated in the roxygen and pinned
  # here rather than changed.
  f <- function() local_timeout(NULL)
  expect_error(f(), class = "rlang_error")
  expect_true(tm_option_accepts(NULL))
})
