# M69: the `tidymedia.timeout` seam.
#
# The resolver is the whole contract's front door: every spawn site passes what
# it returns straight to base R's `timeout=`. Two of its rules exist because
# base R's own handling is unsafe as a package default (measured on R 4.6.1):
#   - a value below 1 TRUNCATES to 0, which is base R's sentinel for "no
#     limit", so `0.5` would leave the call completely unbounded -- the exact
#     silent failure this milestone removes. Non-whole values are refused.
#   - `system2()` accepts `"2"` and `c(1, 2)` without complaint, so nothing
#     downstream will catch a malformed option for us.

test_that("resolve_timeout() returns 0 when the option is unset", {
  withr::local_options(tidymedia.timeout = NULL)
  expect_identical(resolve_timeout(), 0)
})

test_that("resolve_timeout() returns a whole positive limit unchanged", {
  withr::local_options(tidymedia.timeout = 30)
  expect_identical(resolve_timeout(), 30)
  withr::local_options(tidymedia.timeout = 1L)
  expect_identical(resolve_timeout(), 1)
})

test_that("resolve_timeout() accepts an explicit 0 as 'no limit'", {
  withr::local_options(tidymedia.timeout = 0)
  expect_identical(resolve_timeout(), 0)
})

test_that("resolve_timeout() refuses a fractional limit, naming whole seconds", {
  # 0.5 is the dangerous one: base R truncates it to 0 and the call runs
  # unbounded. 1.9 is refused for the same reason (it silently becomes 1).
  withr::local_options(tidymedia.timeout = 0.5)
  expect_error(resolve_timeout(), "whole number")
  withr::local_options(tidymedia.timeout = 1.9)
  expect_error(resolve_timeout(), "whole number")
})

test_that("resolve_timeout() names the option in its refusal", {
  withr::local_options(tidymedia.timeout = 0.5)
  expect_error(resolve_timeout(), "tidymedia.timeout")
})

test_that("resolve_timeout() refuses a negative, NA, string or vector limit", {
  withr::local_options(tidymedia.timeout = -1)
  expect_error(resolve_timeout())
  withr::local_options(tidymedia.timeout = NA_real_)
  expect_error(resolve_timeout())
  # base system2() ACCEPTS both of these, so only this guard catches them.
  withr::local_options(tidymedia.timeout = "2")
  expect_error(resolve_timeout())
  withr::local_options(tidymedia.timeout = c(1, 2))
  expect_error(resolve_timeout())
})

# timeout_status() ------------------------------------------------------------

# AC4: a timeout is recognized by the `status` attribute, never by matching R's
# warning text, whose wording is translated under a non-English locale (M46).
# AC6: status 124 is only a timeout when a limit was actually in force -- a
# program may exit 124 for its own reasons.

test_that("timeout_status() reports a timeout only when a limit was in force", {
  out <- structure(character(0), status = 124L)
  expect_true(is_timeout(out, limit = 2))
  expect_false(is_timeout(out, limit = 0))
})

test_that("timeout_status() does not read any other status as a timeout", {
  expect_false(is_timeout(structure(character(0), status = 1L), limit = 2))
  expect_false(is_timeout(structure(character(0), status = 234L), limit = 2))
  expect_false(is_timeout(character(0), limit = 2))
})

test_that("is_timeout() accepts the status however system2() types it", {
  # system() and system2() have both been seen returning the status as double
  # and as integer; the comparison must not depend on which.
  expect_true(is_timeout(structure(character(0), status = 124), limit = 2))
  expect_true(is_timeout(structure(character(0), status = 124L), limit = 2))
})

test_that("abort_timeout() names the program and the limit in seconds", {
  err <- expect_error(abort_timeout("FFmpeg", 30))
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFmpeg")
  expect_match(msg, "30")
  expect_match(msg, "second")
})

test_that("abort_timeout() signals a distinct condition class", {
  # The class is what lets a caller distinguish a timeout from any other
  # failure -- probe_all() and count_audio_streams() deliberately absorb both
  # today, and the class is what would let a later change separate them.
  err <- expect_error(abort_timeout("FFmpeg", 30))
  expect_s3_class(err, "tidymedia_timeout")
})

test_that("abort_timeout() names the option so the caller can raise it", {
  err <- expect_error(abort_timeout("FFmpeg", 30))
  expect_match(cli::ansi_strip(conditionMessage(err)), "tidymedia.timeout")
})
