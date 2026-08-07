# Guards the documented consequence of D034: resolving hardware = "nvenc" runs
# the FFmpeg binary while the command is being built, so such a call is not
# binary-free even under run = FALSE (M54).
#
# The package has sixteen hand-written `@param hardware` blocks and no
# @inheritParams tying them together, so the docs themselves are exactly the
# hand-list that went stale in D024. This test is the procedure that replaces
# it: it enumerates the topics from the Rd rather than from anyone's memory, so
# a seventeenth verb gaining `hardware` without the sentence fails here.
#
# rd_sources() / rd_param_names() / topics_documenting() come from helper-rd.R.

# The claim, in the wording the roxygen carries. Matched on a distinctive clause
# rather than the whole sentence, so rewrapping the block cannot break the test
# while removing the sentence still does.
probe_sentence <- "asks this FFmpeg build which encoders it has"

test_that("every topic documenting `hardware` states that nvenc probes FFmpeg", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  topics <- topics_documenting(rd, "hardware")

  # A floor, not a count: it fails if the enumeration silently collapses (an
  # unreadable man/, an empty Rd_db) and reports a vacuous pass. Sixteen is the
  # measured count at M54; a verb gaining `hardware` later raises it.
  expect_gte(length(topics), 16L)

  missing <- names(topics)[!grepl(probe_sentence, topics, fixed = TRUE)]
  expect_identical(missing, character())
})

test_that("the nvenc probe sentence is not claimed where `hardware` is absent", {
  # Keeps the guard honest in the other direction: if the sentence were pasted
  # package-wide, the test above would pass while saying nothing. Every topic
  # carrying the sentence must be a topic that documents `hardware`.
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  carrying <- names(rd)[grepl(probe_sentence, rd, fixed = TRUE)]
  documenting <- names(topics_documenting(rd, "hardware"))
  expect_identical(sort(setdiff(carrying, documenting)), character())
})
