# Guards the cross-link between the two 0-based audio indices and the concept
# topic that disambiguates them (M51). The package exposes `audio_stream` (a
# 0-based index among ONE input's audio streams) and `audio_input` (a 0-based index
# among a verb's INPUTS, plus two unrelated Layer-1 meanings), and a reader who
# meets both needs one page saying so. These tests fail when a topic documents
# either parameter without linking to that page -- so a verb that gains the
# argument later and not the link is caught here rather than by a reader.

# TRUE when the Rd text links to the concept topic, in either of the two forms
# roxygen emits (`\link{audio_stream}` from Rd markup, `\link[=audio_stream]`
# from a markdown link).
links_to_topic <- function(txt) {
  grepl("\\\\link\\{audio_stream\\}", txt) ||
    grepl("\\\\link\\[=audio_stream\\]", txt)
}

test_that("the audio_stream concept topic exists and is not internal", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  txt <- rd[[grep("^audio_stream\\.Rd$", names(rd))[1]]]
  expect_type(txt, "character")
  # The precedent topic R/utils-tidy-eval.R carries `@keywords internal`, which
  # hides it from the reference index; this one is for users, so it must not.
  expect_false(grepl("\\\\keyword\\{internal\\}", txt))
})

test_that("every topic documenting audio_stream links to the concept topic", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  topics <- topics_documenting(rd, "audio_stream")
  # Eighteen exported verbs carry the argument after M49: seven scalar/_batch
  # pairs from the extraction and pass-through families, plus format_for_web,
  # normalize_audio, separate_audio_video and their _batch siblings.
  expect_gte(length(topics), 18L)
  missing <- names(topics)[!vapply(topics, links_to_topic, logical(1))]
  expect_equal(missing, character(0))
})

test_that("every topic documenting `audio_input` or `audio` links to the concept topic", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  # compare_videos() / picture_in_picture() and their _batch siblings carry the
  # input index as `audio_input`; ffm_codec() and ffm_copy() keep the bare
  # `audio` for a codec string and a logical. All three meanings are what the
  # topic disambiguates, so every one of them links to it -- an allowlist of
  # the non-index cases would just be a second place to keep in step.
  index <- topics_documenting(rd, "audio_input")
  expect_gte(length(index), 4L)
  bare <- topics_documenting(rd, "audio")
  expect_gte(length(bare), 2L)
  topics <- c(index, bare)
  missing <- names(topics)[!vapply(topics, links_to_topic, logical(1))]
  expect_equal(missing, character(0))
})

test_that("the generated @param text names both families and links the topic", {
  first <- audio_stream_param("take", "takes", "first")
  every <- audio_stream_param("carry into the output", "carries", "every")
  for (txt in list(first, every)) {
    expect_length(txt, 1L)
    # Every verb of both families is named, from the one set of vectors, so a
    # verb added to a family cannot reach the docs of only some of its siblings.
    for (v in c(audio_stream_families$first, audio_stream_families$every)) {
      expect_match(txt, sprintf("\\\\link\\{%s\\}", v), fixed = FALSE)
    }
    expect_match(txt, "\\\\link\\{audio_stream\\}")
    expect_match(txt, "FFmpeg error, not an R one", fixed = TRUE)
  }
  expect_match(first, "takes the \\strong{first} audio track", fixed = TRUE)
  expect_match(every, "carries \\strong{every} audio track", fixed = TRUE)
  # The batch form adds the column/NA rule; the scalar form must not claim it.
  expect_match(audio_stream_param("take", "takes", "first", batch = TRUE),
               "audio_stream} column", fixed = TRUE)
  expect_false(grepl("audio_stream} column", first, fixed = TRUE))
})

test_that("the generated @param audio text states the input basis", {
  scalar <- audio_input_param()
  batch <- audio_input_param(batch = TRUE, extra = "Row-checked.")
  expect_match(scalar, "counts the verb's inputs", fixed = TRUE)
  expect_match(scalar, "\\\\link\\{audio_stream\\}")
  # `audio = NULL` drops audio entirely -- the difference from audio_stream that
  # the whole concept topic exists to make legible.
  expect_match(scalar, "maps no audio at all", fixed = TRUE)
  expect_match(batch, "Row-checked.", fixed = TRUE)
  # Verb-specific text lands before the closing pointer, not after it.
  expect_lt(regexpr("Row-checked.", batch, fixed = TRUE),
            regexpr("(default = ", batch, fixed = TRUE))
})

test_that("the two family vectors are non-empty and disjoint", {
  # rd_verb_list() refuses a family under two members, so an emptied family
  # fails document() rather than silently deleting the sentence from 18 blocks.
  expect_gte(length(audio_stream_families$first), 2L)
  expect_gte(length(audio_stream_families$every), 2L)
  expect_equal(
    intersect(audio_stream_families$first, audio_stream_families$every),
    character(0)
  )
  expect_error(rd_verb_list(character(0)))
})
