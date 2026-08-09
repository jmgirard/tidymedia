# check_batch_cell(): the batch row locator at its one site (M66). These are
# the mechanism's own unit tests; the per-verb behavior lives in the blame
# grid (test-builder-blame-front-door.R) and the value grids.

# The rendered locator line, as it appears at the end of a wrapped refusal.
# cli renders the bullet glyph per platform ("x" or U+2716), so the pattern
# accepts one arbitrary glyph before the text. Kept in sync with
# strip_row_locator() in helper-blame.R, which removes exactly this line.
locator_re <- "\n[^\n]{1,2} First offending jobs row: [0-9]+\\.$"

test_that("check_batch_cell passes a clean value through untouched", {
  expect_identical(check_batch_cell(1L, check_token("h264")), "h264")
  expect_no_error(check_batch_cell(5L, check_dim(320)))
})

test_that("a wrapped refusal keeps its head, class, and gains one locator", {
  plain <- rlang::catch_cnd(check_token("has space", arg = "video_codec"))
  wrapped <- rlang::catch_cnd(
    check_batch_cell(3L, check_token("has space", arg = "video_codec"))
  )
  expect_s3_class(wrapped, class(plain)[[1]])
  msg <- conditionMessage(wrapped)
  # Exactly one locator, naming the row that was passed, as the LAST line.
  expect_match(msg, "First offending jobs row: 3\\.$")
  expect_length(gregexpr("First offending jobs row", msg)[[1]], 1L)
  # Byte-identical head once the locator line is removed (AC2).
  expect_identical(sub(locator_re, "", msg), conditionMessage(plain))
})

test_that("the blamed call survives the wrapper", {
  verb_like <- function() {
    call <- rlang::current_env()
    check_token("has space", arg = "video_codec", call = call)
  }
  verb_wrapped <- function() {
    call <- rlang::current_env()
    check_batch_cell(
      2L, check_token("has space", arg = "video_codec", call = call)
    )
  }
  plain <- rlang::catch_cnd(verb_like())
  wrapped <- rlang::catch_cnd(verb_wrapped())
  expect_identical(deparse(conditionCall(wrapped)[[1]]),
                   "verb_wrapped")
  expect_identical(class(wrapped), class(plain))
})

test_that("a base-R error gains the locator too, class untouched", {
  cnd <- rlang::catch_cnd(check_batch_cell(2L, stop("boom")))
  expect_s3_class(cnd, "simpleError")
  expect_match(conditionMessage(cnd), "^boom\nx First offending jobs row: 2\\.$")
})

test_that("the row lands as an integer whatever numeric arrives", {
  cnd <- rlang::catch_cnd(check_batch_cell(4, check_token("has space")))
  expect_match(conditionMessage(cnd), "First offending jobs row: 4\\.")
})

test_that("the locator wording honors its constraints", {
  cnd <- rlang::catch_cnd(check_batch_cell(7L, check_token("has space")))
  locator <- regmatches(
    conditionMessage(cnd),
    regexpr("First offending jobs row: [0-9]+\\.", conditionMessage(cnd))
  )
  # No "index": test-separate-av-multitrack.R bans the substring outright on
  # that verb's messages, and one wording serves every verb.
  expect_no_match(locator, "index", ignore.case = TRUE)
  # First-offender, singular by construction: no cli pluralization to misfire
  # on a vector (M18), and no vector ever reaches the bullet.
  expect_no_match(locator, "\\{\\?")
})

test_that("strip_row_locator removes the locator and nothing else (AC4)", {
  refuse <- function(row) conditionMessage(rlang::catch_cnd(
    check_batch_cell(row, check_token("has space", arg = "video_codec"))
  ))
  # Under-removal direction: two messages differing ONLY in the row number
  # must compare equal once stripped.
  expect_identical(strip_row_locator(refuse(2L)), strip_row_locator(refuse(9L)))
  # Over-removal direction 1: a difference in the sentence immediately
  # preceding the locator must survive stripping.
  a <- "`x` must be clean.\nx First offending jobs row: 2."
  b <- "`x` must be tidy.\nx First offending jobs row: 2."
  expect_false(identical(strip_row_locator(a), strip_row_locator(b)))
  # Over-removal direction 2: a difference in the last PRE-EXISTING bullet
  # must survive stripping too.
  a2 <- "`x` must be clean.\ni Allowed: letters.\nx First offending jobs row: 2."
  b2 <- "`x` must be clean.\ni Allowed: digits.\nx First offending jobs row: 2."
  expect_false(identical(strip_row_locator(a2), strip_row_locator(b2)))
  # And an unwrapped message passes through untouched.
  plain <- conditionMessage(rlang::catch_cnd(check_token("has space")))
  expect_identical(strip_row_locator(plain), plain)
})
