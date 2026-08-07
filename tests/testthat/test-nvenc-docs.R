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

# The stream-copy exception ---------------------------------------------------

# The sentence above is conditional -- it claims the probe for a call that
# RE-ENCODES the video. Four topics can be asked for nvenc on a stream copy,
# where the codec conflict each already documents aborts the call before the
# encoder is ever resolved. Those two facts are guarded together: the wording
# below, and the behavior it describes.

# Matched on a short clause: the sentence wraps at a different point on the
# `reencode` topics, and a wrap must not be able to break the guard.
copy_sentence <- "is caught first"
copy_topics <- c("segment_video", "segment_video_batch",
                 "separate_audio_video", "separate_audio_video_batch")

test_that("the stream-copy exception is stated on exactly the topics it applies to", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  carrying <- sub("\\.Rd$", "", names(rd)[grepl(copy_sentence, rd, fixed = TRUE)])
  expect_identical(sort(carrying), sort(copy_topics))
})

test_that("a stream-copy nvenc call aborts without probing, as documented", {
  # Counting at ffmpeg_encoders() for the reason the D034 purity blocks give:
  # it is the seam has_nvenc() reaches once the option seam is unset, and the
  # one place the binary would actually be consulted.
  f <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = NULL)
  probes <- 0L
  local_mocked_bindings(
    ffmpeg_encoders = function(...) {
      probes <<- probes + 1L
      tibble::tibble(name = "h264_nvenc")
    }
  )
  # Control: the re-encoding default DOES reach the resolver, so the sentence
  # the topics carry is not vacuous.
  segment_video(f, 0, 5, "out.mp4", hardware = "nvenc", run = FALSE)
  expect_gt(probes, 0L)

  probes <- 0L
  expect_error(
    segment_video(f, 0, 5, "out.mp4", reencode = FALSE, hardware = "nvenc",
                  run = FALSE),
    "re-encoding cut"
  )
  expect_error(
    segment_video_batch(
      tibble::tibble(input = f, start = 0, end = 5, outfiles = "out.mp4"),
      reencode = FALSE, hardware = "nvenc", run = FALSE
    ),
    "re-encoding cut"
  )
  expect_error(
    separate_audio_video(f, "v.mp4", "a.m4a", hardware = "nvenc", run = FALSE),
    "re-encoding `video_codec`"
  )
  expect_error(
    separate_audio_video_batch(
      tibble::tibble(input = f, videofile = "v.mp4", audiofile = "a.m4a"),
      hardware = "nvenc", run = FALSE
    ),
    "re-encoding `video_codec`"
  )
  expect_identical(probes, 0L)
})
