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

# The two verb families, by how each reads `audio_stream = NULL`. Every entry
# also has a `_batch` sibling reading it the same way, so the rendered lists say
# "and their _batch siblings" rather than doubling in length.
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
# from the same two vectors, so neither list can drift from the other.
audio_stream_family_sentence <- function(reading = c("first", "every")) {
  reading <- match.arg(reading)
  first <- rd_verb_list(audio_stream_families$first)
  every <- rd_verb_list(audio_stream_families$every)
  if (identical(reading, "first")) {
    paste0("The first-track family reads \\code{NULL} this way -- ", first,
           ", plus their \\code{_batch} siblings. The every-track family ",
           "keeps them all instead: ", every, ", plus theirs.")
  } else {
    paste0("The every-track family reads \\code{NULL} this way -- ", every,
           ", plus their \\code{_batch} siblings. The first-track family takes ",
           "one track only: ", first, ", plus theirs.")
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
    sprintf(paste0("The 0-based index of the audio track to %s, counted ",
                   "\\emph{among %s audio streams} -- \\code{0} is the first ",
                   "audio track, \\code{1} the second, whatever their ",
                   "positions among the file's streams."),
            action,
            if (batch) "that row's input's" else "the input's"),
    sprintf("\\code{NULL} (default) %s %s.", null_action, quantity),
    if (batch) {
      paste0("The argument applies to every row lacking an ",
             "\\code{audio_stream} column; an \\code{NA} cell in that column ",
             "means the same as \\code{NULL} for that row, rather than ",
             "falling back to the argument.")
    },
    audio_stream_family_sentence(reading),
    extra,
    paste0("Naming a track the input does not have is an FFmpeg error, not an ",
           "R one. See \\code{\\link{audio_stream}} for how this differs from ",
           "\\code{audio_input}, the input index on \\code{\\link{compare_videos}} ",
           "and \\code{\\link{picture_in_picture}}."),
    "(default = \\code{NULL})"
  )
  paste(parts, collapse = " ")
}

# The `@param audio` text for the two fan-in verbs, which count inputs rather
# than streams. Shared for the same reason the block above is.
audio_input_param <- function(batch = FALSE, extra = NULL) {
  paste(
    c(
      paste0("The 0-based index of the \\emph{input} whose audio to keep -- ",
             "\\code{0} is the first file passed in, \\code{1} the second. ",
             "This counts the verb's inputs, not one input's audio streams, ",
             "so it is a different index from \\code{audio_stream} on the ",
             "single-input verbs."),
      paste0("\\code{NULL} (default) maps no audio at all, so the output is ",
             "silent -- unlike \\code{audio_stream = NULL}, which always maps ",
             "something. Naming an input the call does not have is an R ",
             "error, raised before FFmpeg runs."),
      if (batch) {
        paste0("Applied to every row lacking an \\code{audio_input} column; an ",
               "\\code{NA} cell in that column means the same as \\code{NULL} ",
               "for that row, dropping that output's audio.")
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
    "Subtitle and data streams are not carried either way."
  ),
  separation_container = paste0(
    "A container that holds several audio streams (\\code{.mka}, ",
    "\\code{.m4a}) receives them all, while a single-stream container ",
    "(\\code{.aac}, \\code{.mp3}, \\code{.wav}) makes FFmpeg fail -- name a ",
    "track to write one of those. Count among the input's \\emph{audio} ",
    "streams, not the \\code{index} column of \\code{\\link{probe_audio}}, ",
    "which counts every stream. Unlike the verbs that pass video through, an ",
    "input carrying no audio at all is an FFmpeg error here, because this ",
    "verb's product is the audio file. \\code{videofile} is never affected."
  ),
  normalize_one_track = paste0(
    "This verb reads \\code{NULL} the first-track way because the two-pass ",
    "analysis produces one measurement per audio track while the correction ",
    "takes a single set, so normalizing several tracks at once would apply ",
    "one track's measurements to all of them. Under \\code{two_pass = TRUE} ",
    "the analysis pass measures this same track. Only the named track reaches ",
    "the output, and no video does -- whatever the container, so an output ",
    "name that keeps a video extension yields a video file carrying audio ",
    "alone. An input with no audio at all is an FFmpeg error."
  )
)

#' Audio track and audio input indices
#'
#' @description
#' tidymedia has two 0-based audio arguments that count different things. This
#' page says which is which, so that meeting one after the other is not a trap.
#'
#' @details
#' # The two indices
#'
#' \code{audio_stream} counts \strong{one input's audio streams}. On
#' \code{\link{extract_audio}}, \code{audio_stream = 1} is that file's second
#' audio track, whatever position it holds among the file's streams overall (it
#' is not the \code{index} column of \code{\link{probe_audio}}, which counts
#' every stream, audio or not).
#'
#' \code{audio_input} counts \strong{a verb's inputs}. On
#' \code{\link{compare_videos}} and \code{\link{picture_in_picture}}, which
#' combine several files into one output and must choose whose sound to keep,
#' \code{audio_input = 1} is the second \emph{file}, and says nothing about which of
#' its tracks is taken.
#'
#' Neither can be computed from the other, which is why they stay separate
#' names rather than one argument meaning two things depending on the verb's
#' arity.
#'
#' # What `NULL` means, and it is not the same thing
#'
#' \code{audio_stream = NULL} is a selection rather than an absence: the verb
#' still emits a stream map. What differs is how much it selects.
#'
#' * `r audio_stream_family_sentence("first")`
#' * The two readings exist because a verb that writes one audio stream by
#'   construction must pick one track when you name none, while a verb that
#'   carries audio through can keep whatever its container holds.
#' * On the verbs that pass video through, the every-track map is written so
#'   that it matches nothing rather than failing, so an input with no audio at
#'   all simply yields an output with none. On
#'   \code{\link{separate_audio_video}} and \code{\link{normalize_audio}},
#'   whose product \emph{is} audio, that same case is an FFmpeg error.
#'
#' \code{audio_input = NULL} is different in kind: it emits no audio map at all, so
#' the output carries \strong{no audio}. A silent output is the default for
#' \code{\link{compare_videos}} and \code{\link{picture_in_picture}}, because
#' there is no non-arbitrary answer to which of several inputs should be heard.
#'
#' Out of range, the two also fail differently. An \code{audio_input} beyond the
#' inputs you passed is an R error raised before FFmpeg runs; an
#' \code{audio_stream} beyond the input's tracks is an FFmpeg error, because
#' the track count is a property of the file rather than of the call.
#'
#' # In a `_batch` jobs table
#'
#' Both arguments follow one rule on a \code{_batch} verb: the scalar argument
#' is the default, and a \code{jobs} column of the same name overrides it row by
#' row. (This is how these two behave; it is not a claim about every
#' \code{_batch} argument — \code{hardware}, \code{parallel} and \code{two_pass}
#' are batch-wide and read no column.) An \strong{absent column}
#' means the scalar argument applies to every row. A \strong{present column}
#' overrides it row by row, and an \code{NA} cell is that column's spelling of
#' \code{NULL} -- it does not fall back to the scalar argument. So
#' \code{audio_stream = 2} with an \code{audio_stream} column holding \code{NA}
#' puts that row on its family's \code{NULL} reading, not on track 2.
#'
#' # The bare name `audio` is not an index
#'
#' Layer 1 keeps \code{audio} for two things that count nothing:
#'
#' * an audio \emph{codec} string on \code{\link{ffm_codec}}, where
#'   \code{audio = "aac"} names an encoder;
#' * a \emph{logical} on \code{\link{ffm_copy}}, where \code{audio = TRUE}
#'   stream-copies the audio instead of re-encoding it.
#'
#' The input index is \code{audio_input}, so that its name says what it counts,
#' as \code{audio_stream} does.
#'
#' @seealso `r rd_verb_list(audio_stream_families$first)` for the first-track
#'   reading; `r rd_verb_list(audio_stream_families$every)` for the every-track
#'   one; \code{\link{compare_videos}} and \code{\link{picture_in_picture}} for
#'   the input index; \code{\link{probe_audio}} to see what tracks a file
#'   actually holds.
#' @family audio selection functions
#'
#' @aliases audio-tracks audio_indices
#' @name audio_stream
NULL
