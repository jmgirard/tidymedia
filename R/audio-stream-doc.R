# Documentation source for the two 0-based audio indices ---------------------
#
# The package exposes two arguments that both read as "0-based audio index" and
# count different things (D023). This file is the single source for the prose
# that says so: the user-facing concept topic below, and the shared `@param
# audio_stream` text every verb inherits through an inline `r` call in its
# roxygen block. Nothing here runs at package-use time -- roxygen evaluates it
# at document() time and pastes the result into man/*.Rd.
#
# Keeping the family enumerations in R data rather than in eighteen hand-written
# blocks is what makes a stale enumeration unrepresentable: before M51 the
# blocks on standardize_video() and anonymize_video() still named
# separate_audio_video() as their only fellow every-track verb, because M48
# added crop/segment without back-linking them.
#
# The generated sentences land on many help pages, so they follow the plain
# English rules in cairn/references/plain-docs.md (M127): at most 25 words a
# sentence, and no ` -- ` in the Rd source.

# The two verb families, by how each reads `audio_stream = NULL`. Every entry
# also has a `_batch` sibling reading it the same way, so the rendered lists say
# "and their _batch forms" rather than doubling in length.
audio_stream_families <- list(
  first = c("extract_audio", "convert_audio", "normalize_audio"),
  every = c("separate_audio_video", "standardize_video", "anonymize_video",
            "crop_video", "segment_video", "format_for_web")
)

# "\code{\link{a}}, \code{\link{b}} and \code{\link{c}}" -- Rd links in the
# register the rest of the package's roxygen uses.
rd_verb_list <- function(verbs) {
  # An emptied family would otherwise return character(0) and vanish silently
  # from every block that pastes it in, which is the one way this mechanism
  # could lose the enumeration it exists to keep correct.
  stopifnot(length(verbs) >= 2)
  links <- sprintf("\\code{\\link{%s}}", verbs)
  paste0(paste(links[-length(links)], collapse = ", "), " and ",
         links[length(links)])
}

# The sentence naming both families, told from the point of view of the family
# `reading` belongs to. Exists in exactly one place; both readings are rendered
# from the same two vectors, so neither list can drift from the other. Each
# sentence says what the family reads `NULL` as, rather than "this way", so it
# stands on its own as a bullet on ?audio_stream as well as after a verb's
# "`NULL` (default) takes ..." sentence.
audio_stream_family_sentence <- function(reading = c("first", "every")) {
  reading <- match.arg(reading)
  first <- rd_verb_list(audio_stream_families$first)
  every <- rd_verb_list(audio_stream_families$every)
  first_s <- paste0(" as the first audio track only: ", first,
                    ", and their \\code{_batch} forms.")
  every_s <- paste0(" as every audio track: ", every,
                    ", and their \\code{_batch} forms.")
  if (identical(reading, "first")) {
    paste0("The first-track family reads \\code{NULL}", first_s,
           " The every-track family reads it", every_s)
  } else {
    paste0("The every-track family reads \\code{NULL}", every_s,
           " The first-track family reads it", first_s)
  }
}

# The full `@param audio_stream` text for one verb. Callers supply only what is
# genuinely theirs: how the verb speaks of the track (`action` / `null_action`),
# whether it is a `_batch` verb, and any verb-specific caveat.
#
#   action      infinitive phrase completing "the audio track to ..."
#   null_action third-person verb completing "\code{NULL} (default) ..."
#   reading     which family this verb belongs to
#   batch       TRUE for a `_batch` verb (adds the column/NA sentence)
#   extra       verb-specific sentence(s), appended before the closing links
audio_stream_param <- function(action,
                               null_action,
                               reading = c("first", "every"),
                               batch = FALSE,
                               extra = NULL) {
  reading <- match.arg(reading)
  quantity <- if (identical(reading, "first")) {
    "the \\strong{first} audio track"
  } else {
    "\\strong{every} audio track"
  }
  parts <- c(
    sprintf(paste0("The audio track to %s, as a number that counts from ",
                   "\\code{0} among the \\emph{audio tracks} of %s. ",
                   "\\code{0} is the first audio track and \\code{1} is the ",
                   "second. Other streams in the file, such as video, do not ",
                   "count."),
            action,
            if (batch) "each row's input" else "the input"),
    sprintf("\\code{NULL} (default) %s %s.", null_action, quantity),
    if (batch) {
      paste0("Without an \\code{audio_stream} column, the argument applies to ",
             "every row. An \\code{NA} cell in that column means \\code{NULL} ",
             "for that row. It does not fall back to the argument.")
    },
    audio_stream_family_sentence(reading),
    extra,
    paste0("A track the input does not have gives an FFmpeg error, not an ",
           "R one. See \\code{\\link{audio_stream}} for how this differs from ",
           "\\code{audio_input}, the input index on ",
           "\\code{\\link{compare_videos}} and ",
           "\\code{\\link{picture_in_picture}}."),
    "(default = \\code{NULL})"
  )
  paste(parts, collapse = " ")
}

# The `@param audio_input` text for the two fan-in verbs, which count inputs rather
# than streams. Shared for the same reason the block above is.
audio_input_param <- function(batch = FALSE, extra = NULL) {
  paste(
    c(
      paste0("The input file whose audio to keep, as a number that counts ",
             "from \\code{0}. \\code{0} is the first file you pass and ",
             "\\code{1} is the second. This counts the function's inputs, ",
             "not the audio tracks of one input. So it is a different index ",
             "from \\code{audio_stream} on the functions that take one input."),
      # "still selects audio" is the plain form of "always emits a stream map":
      # audio_stream = NULL is a selection, never an absence.
      paste0("\\code{NULL} (default) selects no audio at all, so the output ",
             "is silent. This differs from \\code{audio_stream = NULL}, which ",
             "still selects audio. An input number the call does not have ",
             "gives an R error, before FFmpeg runs."),
      if (batch) {
        paste0("Without an \\code{audio_input} column, the argument applies ",
               "to every row. An \\code{NA} cell in that column means ",
               "\\code{NULL} for that row, so that output has no audio.")
      },
      extra,
      "See \\code{\\link{audio_stream}}. (default = \\code{NULL})"
    ),
    collapse = " "
  )
}

# Verb-specific sentences that nonetheless recur across a whole family, so they
# get one home here rather than ten copies in the blocks. Anything that is
# genuinely true of exactly one verb stays written out at that verb's block.
audio_stream_extras <- list(
  passthrough_subtitles = paste0(
    "The function does not carry subtitle or data streams in either case."
  ),
  separation_container = paste0(
    "A container that holds several audio streams (\\code{.mka}, ",
    "\\code{.m4a}) gets them all. A container for one stream only ",
    "(\\code{.aac}, \\code{.mp3}, \\code{.wav}) makes FFmpeg fail, so name a ",
    "track to write one of those. Count only the input's \\emph{audio} ",
    "streams. Do not use the \\code{index} column of ",
    "\\code{\\link{probe_audio}}, which counts every stream. An input with ",
    "no audio at all is an FFmpeg error here, because the product of this function is the audio file. ",
    "The functions that pass video through do not fail in that case. ",
    "\\code{videofile} is never affected."
  ),
  normalize_one_track = paste0(
    "This function reads \\code{NULL} as the first track only. The two-pass ",
    "analysis measures each audio track, but the correction uses one set of ",
    "measurements. Normalizing several tracks at once would apply one ",
    "track's measurements to all of them. Under \\code{two_pass = TRUE}, the ",
    "analysis pass measures this same track. Only the named track reaches ",
    "the output, and no video does, whatever the container. So an output ",
    "name with a video extension gives a video file that holds only audio. ",
    "An input with no audio at all is an FFmpeg error."
  )
)

#' Audio track and audio input indices
#'
#' @description
#' Two audio arguments in this package count different things: `audio_stream`
#' and `audio_input`. Both count from `0`, so `0` means the first one. This page
#' explains which is which.
#'
#' The glossary in `vignette("tidymedia")` explains media terms such as stream,
#' container and codec.
#'
#' @details
#' # The two indices
#'
#' `audio_stream` counts \strong{the audio tracks of one input file}. On
#' [extract_audio()], `audio_stream = 1` is the second audio track of the file.
#' Where that track sits among all the streams of the file does not matter. So
#' `audio_stream` is not the `index` column of [probe_audio()], which counts
#' every stream, audio or not.
#'
#' `audio_input` counts \strong{the input files of a function}. The functions
#' [compare_videos()] and [picture_in_picture()] combine several files into one
#' output, so they must choose whose sound to keep. On these functions,
#' `audio_input = 1` is the second \emph{file}. It says nothing about which
#' track of that file is used.
#'
#' You cannot work out one index from the other. So the package keeps two
#' names, rather than one argument whose meaning depends on how many inputs a
#' function takes.
#'
#' # What `NULL` means
#'
#' `audio_stream = NULL` still selects audio. It does not mean "no audio". How
#' much audio it selects depends on the function.
#'
#' * `r audio_stream_family_sentence("first")`
#' * The two readings have a reason. A function that writes one audio stream
#'   must pick one track when you name none. A function that carries audio
#'   through can keep all the tracks its container holds.
#' * On the functions that pass video through, an input with no audio gives an
#'   output with no audio, not an error. On [separate_audio_video()] and
#'   [normalize_audio()], whose output \emph{is} audio, that input gives an
#'   FFmpeg error.
#'
#' `audio_input = NULL` is different: it selects no audio at all, so the output
#' has \strong{no audio}. A silent output is the default for [compare_videos()]
#' and [picture_in_picture()]. With several inputs, no choice of which one to
#' hear is better than another.
#'
#' The two arguments also fail in different ways when a number is too large.
#' An `audio_input` that names an input you did not pass gives an R error,
#' before FFmpeg runs. An `audio_stream` that names a track the input does not
#' have gives an FFmpeg error. The reason is that the number of tracks is a fact
#' about the file, not about the call.
#'
#' # In a `_batch` jobs table
#'
#' On a `_batch` function, both arguments follow one rule. The argument you
#' pass is the default, and a `jobs` column with the same name overrides it row
#' by row.
#'
#' This rule is about these two arguments only. The arguments `hardware`,
#' `parallel` and `two_pass` apply to the whole batch, and the function reads
#' no column for them.
#'
#' If the column is absent, the argument applies to every row. If the column is
#' present, each row uses its own cell. An `NA` cell means `NULL` for that row.
#' It does not fall back to the argument. So `audio_stream = 2` with an `NA`
#' cell in an `audio_stream` column gives that row the `NULL` reading of its
#' family, not track 2.
#'
#' # The name `audio` alone is not an index
#'
#' The pipeline functions use `audio` for two things that are not counts:
#'
#' * an audio codec name on [ffm_codec()], where `audio = "aac"` names an
#'   encoder;
#' * a logical on [ffm_copy()], where `audio = TRUE` copies the audio stream
#'   without re-encoding it.
#'
#' The input index is called `audio_input`, so that its name says what it
#' counts, as `audio_stream` does.
#'
#' @seealso `r rd_verb_list(audio_stream_families$first)` read `NULL` as the
#'   first audio track. `r rd_verb_list(audio_stream_families$every)` read it
#'   as every audio track. [compare_videos()] and [picture_in_picture()] take
#'   the input index. [probe_audio()] shows which audio tracks a file has.
#' @family audio selection functions
#'
#' @aliases audio-tracks audio_indices
#' @name audio_stream
NULL
