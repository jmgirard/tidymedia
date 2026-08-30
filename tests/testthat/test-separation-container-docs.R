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

# The help pages -------------------------------------------------------------

# Whitespace-collapsed Rd text, keyed by topic. Collapsed because the
# enumeration is one long generated string that roxygen pastes in unbroken
# while the prose around it is wrapped: a verbatim match has to see the same
# single spaces the renderer emits, whatever the source wrapping.
collapsed_rd <- function() {
  rd <- rd_sources()
  if (is.null(rd)) return(NULL)
  out <- gsub("[[:space:]]+", " ", rd)
  names(out) <- names(rd)
  out
}

# The topic set, derived from the vector rather than recalled: a topic that
# names one member of `multi_audio_extensions` is claiming the list.
opus_token <- "\\code{.opus}"

# The clause the enumeration is embedded in, on both pages. Matched on the part
# both blocks share, so the leading "Those"/"The" can differ.
marker_clause <- "are an exclusion list and not a survey"

test_that("every topic naming a separation container carries the whole list", {
  rd <- collapsed_rd()
  skip_if(is.null(rd), "no Rd source available")
  topics <- rd[grepl(opus_token, rd, fixed = TRUE)]

  # A floor, not a count: it fails if the enumeration silently collapses (an
  # unreadable man/, an empty Rd_db) and reports a vacuous pass. Two is the
  # measured count at M93 -- the scalar verb and its batch sibling.
  expect_gte(length(topics), 2L)

  missing <- names(topics)[!grepl(multi_audio_rd_list(), topics, fixed = TRUE)]
  expect_identical(missing, character())
})

test_that("the list is not claimed where the enumeration is absent", {
  # The other direction, so the guard above cannot pass by saying nothing. What
  # both guards catch is a block whose enumeration stops matching the vector --
  # a faithful hand copy passes them, a stale one does not: dropping `.ts` from
  # the batch block's prose reddens both (M93). A copy that also loses the
  # `.opus` token drops out of the guard above's domain and is caught only
  # here.
  rd <- collapsed_rd()
  skip_if(is.null(rd), "no Rd source available")
  carrying <- names(rd)[grepl(marker_clause, rd, fixed = TRUE)]

  # The clause's own domain, shown non-empty for the same reason as above.
  expect_gte(length(carrying), 2L)

  listing <- names(rd)[grepl(multi_audio_rd_list(), rd, fixed = TRUE)]
  expect_identical(sort(setdiff(carrying, listing)), character())
})
