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

# The four spawn sites (AC1, AC2) ---------------------------------------------

# The domain of AC1 is the four sites named in the milestone's Scope, not
# "every spawn site" -- a source regex cannot enumerate that, and the plan's
# criteria audit cut the universal for exactly that reason. These read the
# functions' own bodies, so a site that stops passing the limit reddens here.

timeout_site_bodies <- function() {
  list(
    ffmpeg    = tidymedia::ffmpeg,
    ffprobe   = tidymedia::ffprobe,
    mediainfo = tidymedia::mediainfo,
    run_program = get("run_program", envir = asNamespace("tidymedia"))
  )
}

test_that("each of the four spawn sites passes a resolved limit to timeout=", {
  for (nm in names(timeout_site_bodies())) {
    src <- paste(deparse(body(timeout_site_bodies()[[nm]])), collapse = "\n")
    expect_match(src, "resolve_timeout(", fixed = TRUE,
                 info = paste(nm, "must resolve the limit"))
    expect_match(src, "timeout = limit", fixed = TRUE,
                 info = paste(nm, "must pass it to timeout="))
    expect_match(src, "guard_timeout(", fixed = TRUE,
                 info = paste(nm, "must route through the shared guard"))
  }
})

test_that("with the option unset, each site resolves a limit of 0", {
  withr::local_options(tidymedia.timeout = NULL)
  expect_identical(resolve_timeout(), 0)
})

test_that("guard_timeout() re-raises a non-timeout warning but drops it on a timeout", {
  # Layer 0 has always let a non-zero exit warn; that must survive.
  expect_warning(
    guard_timeout("FFmpeg", 0, {warning("ordinary"); structure("x", status = 1L)}),
    "ordinary"
  )
  # On a timeout the held warning is dropped -- it carries the command line and
  # the input= temp path -- and replaced by the package's own message.
  expect_error(
    guard_timeout("FFmpeg", 2, {warning("secret /tmp/path"); structure("x", status = 124L)}),
    "timed out"
  )
})

test_that("guard_timeout(suppress = TRUE) discards warnings, as run_program() always has", {
  expect_no_warning(
    guard_timeout("x", 0, {warning("ordinary"); "ok"}, suppress = TRUE)
  )
})

test_that("no warning at all escapes a timed-out guard (AC7, locale-free)", {
  # Asserted as "no warning", never as a match on `timed out after`: R's warning
  # text is translated under a non-English locale, so a text match would pass
  # while the command line still leaked (M46).
  expect_no_warning(
    tryCatch(
      guard_timeout("FFmpeg", 2, {warning("timed out after 2s"); structure("x", status = 124L)}),
      error = function(e) NULL
    )
  )
})
