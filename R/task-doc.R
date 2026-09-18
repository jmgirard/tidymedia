# Documentation source for text shared by the task function help pages --------
#
# Text that several task functions' roxygen blocks would otherwise each carry a
# copy of. An argument with the same name and the same text is written once, in
# one block, and the other blocks take it with `@inheritParams`. Everything else
# that is shared lives here: return values, references, and sentences that sit
# inside an argument or section whose other sentences differ from page to page.
# A block pastes the text in with an inline `r` call. Nothing here runs at
# package-use time. roxygen evaluates it at document() time and writes the
# result into man/*.Rd.
#
# The text follows the plain English rules in cairn/references/plain-docs.md: at
# most 25 words a sentence, and no ` -- ` in the Rd source. A rewrite of shared
# text keeps the claims the text made before (D093), so a change here changes
# how a fact is said, not what it says.

# Joins sentences into one Rd string, dropping any NULL.
rd_sentences <- function(...) {
  paste(c(...), collapse = " ")
}

# Arguments ------------------------------------------------------------------

# The path of the file a scalar task function writes.
write_path_param <- function(kind = c("video", "audio")) {
  kind <- match.arg(kind)
  sprintf("A string containing the path of the %s file to write.", kind)
}

# `video_codec` on the scalar functions whose default leaves the codec unset.
video_codec_unset_param <- function() {
  rd_sentences(
    "A string naming the output video codec, or \\code{NULL} (default) to leave it unset.",
    paste0("Then the output container's default encoder is used, and the ",
           "compiled command is the same as one that never named a codec.")
  )
}

# The start of `audio_codec` where `"copy"` is the default. `object` is how the
# sentence names the audio: "the audio", or "it" after a sentence naming the
# carried track.
audio_codec_copy_sentences <- function(object = c("the audio", "it")) {
  object <- match.arg(object)
  rd_sentences(
    sprintf("\\code{\"copy\"} (default) stream-copies %s through untouched.",
            object),
    "Name an encoder, such as \\code{\"aac\"}, to transcode it.",
    paste0("\\code{NULL} leaves the codec unset, so the output container's ",
           "default encoder is used.")
  )
}

# The `hardware` argument on a scalar function that re-encodes the video with a
# software `video_codec`. `null_default` is TRUE where `video_codec` defaults to
# NULL, and `video_only` TRUE where the function also takes an `audio_codec`.
hardware_param <- function(null_default = TRUE, video_only = FALSE) {
  rd_sentences(
    "The encoder backend. \\code{\"none\"} (default) uses the software \\code{video_codec}.",
    paste0("\\code{\"nvenc\"} uses NVIDIA GPU encoding (H.264, HEVC and AV1), ",
           "and \\code{\"videotoolbox\"} uses Apple GPU encoding (H.264 and HEVC)."),
    "A backend uses its own encoder for the family of \\code{video_codec}.",
    paste0("For example, \\code{\"libx264\"} becomes \\code{\"h264_nvenc\"} or ",
           "\\code{\"h264_videotoolbox\"}."),
    if (null_default) {
      c("With the default \\code{video_codec = NULL}, the H.264 family is assumed.",
        paste0("So a non-H.264 container, such as \\code{.webm}, needs an ",
               "explicit HEVC- or AV1-family \\code{video_codec} (AV1 only ",
               "under \\code{\"nvenc\"})."))
    },
    "See \\code{\\link{has_hardware_encoder}} for availability and its caveats.",
    if (video_only) {
      "This applies to video only. \\code{audio_codec} is never hardware-accelerated."
    },
    hardware_probe_sentences()
  )
}

# The `quality` argument on a scalar function that re-encodes video (M135).
# `fixed_h264` is TRUE where the function has no `video_codec` formal and the
# recipe fixes the H.264 family, so only the three H.264 encoders can apply.
quality_param <- function(fixed_h264 = FALSE) {
  rd_sentences(
    "A number, or \\code{NULL} (default) to leave the encoder's own default in place.",
    "It is the encoder's own rate-control value, passed through unchanged.",
    paste0("\\code{libx264} and \\code{libx265} read it as \\code{-crf} (0 to 51), ",
           "the nvenc encoders as \\code{-cq} (0 to 51), ",
           "and the videotoolbox encoders as \\code{-q:v} (1 to 100)."),
    "Each scale is its own: the same number means something different on each encoder.",
    "A value outside the encoder's range is refused.",
    if (fixed_h264) {
      paste0("The encoder is \\code{libx264}, \\code{h264_nvenc} or ",
             "\\code{h264_videotoolbox}, as \\code{hardware} chooses.")
    } else {
      paste0("An encoder outside those seven, such as \\code{libvpx-vp9}, ",
             "or a \\code{video_codec} of \\code{\"copy\"} or \\code{NULL} ",
             "under \\code{hardware = \"none\"}, is refused with \\code{quality} set.")
    },
    paste0("When \\code{fallback = TRUE} falls back to software, the value is ",
           "dropped and the message says so, because it belonged to the ",
           "hardware encoder's scale.")
  )
}

# What resolving a hardware backend costs. Every `hardware` argument of a task
# function carries it, and test-nvenc-docs.R checks that it does.
hardware_probe_sentences <- function() {
  rd_sentences(
    "Resolving a hardware backend asks this FFmpeg build which encoders it has.",
    paste0("So the first such call that re-encodes the video runs FFmpeg while ",
           "the command is built, even under \\code{run = FALSE}."),
    "The answer is remembered for the rest of the R session.",
    "See \\code{\\link{refresh_ffmpeg_capabilities}} to discard it."
  )
}

# A batch function checks encoder availability itself, before any row runs.
encoder_check_sentences <- function() {
  rd_sentences(
    "This function checks that the encoder is available before any row runs.",
    paste0("So an unavailable encoder aborts naming this function, not the ",
           "internal step that runs the rows.")
  )
}

# `hardware,fallback` on a batch function that passes both to its scalar form.
batch_hardware_param <- function(scalar) {
  rd_sentences(
    "The encoder backend and its fallback behavior, applied to the whole batch.",
    paste0("They are a property of the machine, not of a row, so neither is ",
           "read as a \\code{jobs} column."),
    sprintf("See \\code{\\link[=%s]{%s()}}.", scalar, scalar)
  )
}

# A call that is refused for contradicting itself before the encoder check.
# `example` names the one contradiction the function's page describes, and
# completes "contradict itself by ...".
contradiction_sentences <- function(example = c("audio_codec", "cut", "copy")) {
  example <- match.arg(example)
  how <- switch(example,
    audio_codec = "naming an \\code{audio_codec} with no audio carried into the output",
    cut = "asking for GPU encoding on a cut that stream-copies",
    copy = "asking for GPU encoding alongside a stream copy"
  )
  rd_sentences(
    sprintf("A call can also contradict itself by %s.", how),
    paste0("Such a call is refused for the contradiction first, whether or not ",
           "this machine has the encoder.")
  )
}

# The order of a value error and a contradiction on the two fan-in batch
# functions. test-front-door-ordering.R checks this wording.
value_error_order_sentences <- function() {
  rd_sentences(
    paste0("A value error and a contradiction resolve the same way whether the ",
           "value arrived as an argument or in a \\code{jobs} column."),
    "The contradiction reports first."
  )
}

# The `fallback` argument. `software` names what the fallback encodes with.
# `unset` adds what the fallback does under `video_codec = NULL`, `reproducible`
# the sentence on why the codec never changes silently, and `batch` says the
# value applies to every row.
fallback_param <- function(software = c("software", "video_codec", "libx264"),
                           unset = c("none", "picking", "injecting"),
                           reproducible = FALSE,
                           batch = FALSE) {
  software <- match.arg(software)
  unset <- match.arg(unset)
  fallback_to <- switch(software,
    software = "\\code{TRUE} encodes in software with a message",
    video_codec = "\\code{TRUE} re-encodes with the software \\code{video_codec} and a message",
    libx264 = "\\code{TRUE} re-encodes with software libx264 and a message"
  )
  rd_sentences(
    if (batch) "A logical applied to every row." else "A logical.",
    paste0("When a \\code{hardware} other than \\code{\"none\"} is requested ",
           "but its encoder is unavailable, ", fallback_to, "."),
    "\\code{FALSE} (default) aborts instead.",
    switch(unset,
      none = NULL,
      picking = paste0("With \\code{video_codec = NULL}, the fallback leaves the ",
                       "codec unset rather than picking one, so the codec never ",
                       "changes silently."),
      injecting = paste0("With \\code{video_codec = NULL}, the fallback leaves ",
                         "the codec unset rather than injecting one.")
    ),
    if (reproducible) {
      "This keeps output reproducible by never changing the codec silently."
    },
    if (batch) "It is batch-wide, not a per-row column.",
    paste0("A \\code{video_codec} in a family that the backend has no encoder ",
           "for is a wrong argument, not an absent encoder."),
    "So it aborts whatever \\code{fallback} says."
  )
}

# The `video_codec` default on the batch functions whose default is libx264.
batch_libx264_sentences <- function() {
  rd_sentences(
    "The default is \\code{\"libx264\"}.",
    paste0("\\code{NULL} emits no \\code{-codec:v} and lets the output ",
           "container's default encoder decide."),
    paste0("For a \\code{.webm} output, pass \\code{audio_codec = NULL} too, ",
           "because the default \\code{\"copy\"} would otherwise carry a codec ",
           "WebM cannot hold.")
  )
}

# A batch function that refuses two rows with the same output path.
duplicate_output_sentences <- function() {
  rd_sentences(
    "Two rows naming the same output path are refused before any row runs.",
    paste0("That is a path repeated in the \\code{output} column, or a repeated ",
           "\\code{input} when there is no \\code{output} column.")
  )
}

# What `NA` means in the columns of the two fan-in batch functions.
fan_in_na_sentences <- function() {
  rd_sentences(
    paste0("In an \\code{audio_input} column, \\code{NA} means \"drop audio\", ",
           "the column's way of writing the scalar's \\code{NULL}."),
    paste0("In a \\code{video_codec} or \\code{audio_codec} column, it means ",
           "\"leave the codec unset\".")
  )
}

# A function that checks every path of `infiles` itself.
infiles_check_sentences <- function() {
  rd_sentences(
    "This function checks every path itself.",
    paste0("A path that cannot be found or read aborts naming this function, ",
           "and the error lists every such path."),
    paste0("It is not reported against the internal builder that the path ",
           "would otherwise reach.")
  )
}

# The dropped-track check ------------------------------------------------------

# The paragraph on the dropped-track warning, for extract_audio(),
# convert_audio() and their batch forms. test-check-tracks-docs.R checks the
# cost phrase, and that only the batch forms say the probes run before the rows.
dropped_audio_paragraph <- function(batch = FALSE) {
  if (batch) {
    rd_sentences(
      paste0("When a row names no \\code{audio_stream} and its input has tracks ",
             "that the output will not carry, the function warns \\strong{once} ",
             "for the whole batch."),
      "The warning names every affected row.",
      dropped_audio_cost_sentences(batch = TRUE),
      paste0("The check never runs under \\code{run = FALSE}, never changes any ",
             "compiled command, and is skipped entirely when every row names a ",
             "track."),
      paste0("Suppress it by class with ",
             "\\code{suppressWarnings(classes = \"tidymedia_dropped_audio\")}.")
    )
  } else {
    rd_sentences(
      paste0("When no \\code{audio_stream} is named and the input has tracks that ",
             "the output will not carry, the function warns."),
      dropped_audio_cost_sentences(batch = FALSE),
      paste0("It never runs under \\code{run = FALSE}, and never changes the ",
             "compiled command."),
      paste0("Suppress it by naming a track with \\code{audio_stream}, or by ",
             "class with ",
             "\\code{suppressWarnings(classes = \"tidymedia_dropped_audio\")}.")
    )
  }
}

# What the dropped-track check costs, and when it is skipped.
dropped_audio_cost_sentences <- function(batch = FALSE) {
  rd_sentences(
    if (batch) {
      c(paste0("The check costs \\strong{one FFprobe call per distinct input} ",
               "it has to probe."),
        paste0("A repeated input is probed once, and a row that names a track ",
               "is not probed at all."))
    } else {
      paste0("The check costs \\strong{one FFprobe call per distinct input}, ",
             "which is one call here, because this function takes a single ",
             "\\code{infile}.")
    },
    paste0("The warning is given when FFprobe is available and the input can ",
           "be probed."),
    "Otherwise the check is skipped silently.",
    if (batch) {
      c(paste0("Those probes run \\strong{one at a time, before any row starts}, ",
               "so \\code{parallel} does not reach them."),
        "A sweep long enough to look like a hang reports its progress.")
    }
  )
}

# How to switch the dropped-track check off.
check_tracks_off_paragraph <- function(batch = FALSE) {
  rd_sentences(
    sprintf(paste0("To switch the check off and skip %s, use ",
                   "\\code{options(tidymedia.check_tracks = FALSE)} for the ",
                   "session."),
            if (batch) "the whole sweep" else "its FFprobe call"),
    paste0("Use \\code{withr::local_options(tidymedia.check_tracks = FALSE)} ",
           "for the rest of one function.")
  )
}

# Return values ----------------------------------------------------------------

# A scalar task function that returns its one compiled command.
command_return <- function() {
  "The compiled FFmpeg command (invisibly when \\code{run = TRUE})."
}

# A batch function whose result is the `jobs` table with added columns.
jobs_return <- function() {
  rd_sentences(
    "The \\code{jobs} tibble with an added \\code{command} column.",
    paste0("When \\code{run = TRUE}, it also has a \\code{success} column, plus ",
           "\\code{verified} or a provenance manifest, each when requested ",
           "through \\code{...}."),
    "See \\code{\\link{ffm_batch}}."
  )
}

# A batch function that returns what ffm_batch() returns. `derived` names the
# column the function can derive when `jobs` lacks it.
batch_return <- function(derived = c("output", "outdir")) {
  derived <- match.arg(derived)
  rd_sentences(
    paste0("The \\link[tibble:tibble-package]{tibble} returned by ",
           "\\code{\\link{ffm_batch}}: \\code{jobs} with an added ",
           "\\code{command} column."),
    sprintf("When \\code{%s} was derived, it also has the resolved \\code{%s} column.",
            derived, derived),
    paste0("When \\code{run = TRUE}, it has a \\code{success} column, plus any ",
           "columns the forwarded arguments add, such as \\code{verified}.")
  )
}

# References -------------------------------------------------------------------

time_duration_reference <- function() {
  "https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax"
}

ebu_r128_reference <- function() {
  paste0("EBU Recommendation R 128 (2014), \\emph{Loudness normalisation and ",
         "permitted maximum level of audio signals}; ITU-R BS.1770-4.")
}
