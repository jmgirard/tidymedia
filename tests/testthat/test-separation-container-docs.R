# Guards the container enumeration on the two separation help pages (M93).
#
# `multi_audio_extensions` is the instrument the multi-track separation report
# is gated on (D069/D071), and both help pages describe it in prose. Before M93
# that prose was hand-written and went stale on each of the two occasions the
# vector grew inside M91. The blocks now paste the two renderers below in
# through inline `r` calls, and the second half of this file checks they are
# still wired that way -- generation prevents drift only while the blocks stay
# wired, and a block reverted to hand-written prose would otherwise ship green.
#
# rd_sources() comes from helper-rd.R.

# The renderers ---------------------------------------------------------------

test_that("multi_audio_rd_list() joins the dotted extensions with one `and`", {
  expect_identical(
    multi_audio_rd_list(c("a", "b", "c")),
    "\\code{.a}, \\code{.b} and \\code{.c}"
  )
  # Two members is the floor, and takes the `and` with no comma before it.
  expect_identical(multi_audio_rd_list(c("a", "b")),
                   "\\code{.a} and \\code{.b}")
})

test_that("multi_audio_rd_count() names the length in words", {
  expect_identical(multi_audio_rd_count(c("a", "b", "c")), "three")
  expect_identical(multi_audio_rd_count(c("a", "b")), "two")
  # The committed vector, so the word the help pages render is asserted here
  # too and not only through the Rd.
  expect_identical(multi_audio_rd_count(), "nine")
})

test_that("both renderers refuse a vector under two members", {
  # An emptied vector must fail document() rather than silently deleting the
  # enumeration from both blocks -- rd_verb_list()'s reason, unchanged.
  expect_error(multi_audio_rd_list(character(0)))
  expect_error(multi_audio_rd_list("mka"))
  expect_error(multi_audio_rd_count(character(0)))
  expect_error(multi_audio_rd_count("mka"))
})

test_that("multi_audio_rd_count() aborts on a length it cannot name", {
  # One past the last nameable length: the failure to catch is a silent NA
  # rendered into a help page, so the abort is asserted by its message rather
  # than by a bare error.
  last <- length(multi_audio_count_words) + 1L
  expect_identical(multi_audio_rd_count(rep("x", last)), "twelve")
  expect_error(multi_audio_rd_count(rep("x", last + 1L)),
               "cannot name a length of 13")
})
