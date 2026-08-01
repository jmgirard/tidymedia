# Guards the cross-link between the two 0-based audio indices and the concept
# topic that disambiguates them (M51). The package exposes `audio_stream` (a
# 0-based index among ONE input's audio streams) and `audio` (a 0-based index
# among a verb's INPUTS, plus two unrelated Layer-1 meanings), and a reader who
# meets both needs one page saying so. These tests fail when a topic documents
# either parameter without linking to that page -- so a verb that gains the
# argument later and not the link is caught here rather than by a reader.

# man/ ships in the package SOURCE, not in an installed package, so these tests
# run under devtools::test() and skip under R CMD check on a built tarball.
man_dir <- function() {
  for (p in c("../../man", "../../../man", "man")) {
    if (dir.exists(p)) return(normalizePath(p))
  }
  NULL
}

# Argument names documented by an .Rd file. roxygen renders a shared block as
# `\item{direction, resize, audio}{...}`, so each item is split on commas.
rd_param_names <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  items <- regmatches(txt, gregexpr("\\\\item\\{[^}]*\\}", txt))[[1]]
  names <- sub("^\\\\item\\{", "", sub("\\}$", "", items))
  unique(trimws(unlist(strsplit(names, ","))))
}

# TRUE when the .Rd links to the concept topic, in either of the two forms
# roxygen emits (`\link{audio_stream}` from Rd markup, `\link[=audio_stream]`
# from a markdown link).
links_to_topic <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  grepl("\\\\link\\{audio_stream\\}", txt) ||
    grepl("\\\\link\\[=audio_stream\\]", txt)
}

topics_documenting <- function(dir, param) {
  rd <- list.files(dir, pattern = "\\.Rd$", full.names = TRUE)
  Filter(function(p) param %in% rd_param_names(p), rd)
}

test_that("the audio_stream concept topic exists and is not internal", {
  dir <- man_dir()
  skip_if(is.null(dir), "man/ is not available (built package)")
  path <- file.path(dir, "audio_stream.Rd")
  expect_true(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # The precedent topic R/utils-tidy-eval.R carries `@keywords internal`, which
  # hides it from the reference index; this one is for users, so it must not.
  expect_false(grepl("\\\\keyword\\{internal\\}", txt))
})

test_that("every topic documenting audio_stream links to the concept topic", {
  dir <- man_dir()
  skip_if(is.null(dir), "man/ is not available (built package)")
  topics <- topics_documenting(dir, "audio_stream")
  # Eighteen exported verbs carry the argument after M49: seven scalar/_batch
  # pairs from the extraction and pass-through families, plus format_for_web,
  # normalize_audio, separate_audio_video and their _batch siblings.
  expect_gte(length(topics), 18L)
  missing <- basename(Filter(Negate(links_to_topic), topics))
  expect_equal(missing, character(0))
})

test_that("every topic documenting an `audio` argument links to the concept topic", {
  dir <- man_dir()
  skip_if(is.null(dir), "man/ is not available (built package)")
  topics <- topics_documenting(dir, "audio")
  # compare_videos() / picture_in_picture() and their _batch siblings, where
  # `audio` is an input index, plus ffm_codec() and ffm_copy(), where it means a
  # codec string and a logical. All four meanings are what the topic
  # disambiguates, so every one of them links to it -- an allowlist of the
  # non-index cases would just be a second place to keep in step.
  expect_gte(length(topics), 6L)
  missing <- basename(Filter(Negate(links_to_topic), topics))
  expect_equal(missing, character(0))
})
