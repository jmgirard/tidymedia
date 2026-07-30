
# ffmpeg() ----------------------------------------------------------------

#' Run a raw FFmpeg command
#'
#' Send a raw argument string to the FFmpeg command-line program. This is the
#' Layer 0 escape hatch: the string is passed to FFmpeg verbatim (after the
#' executable path), so the caller is responsible for quoting and option order.
#'
#' @param command A string containing the arguments to pass to FFmpeg.
#' @return A character vector containing the text output by FFmpeg.
#' @seealso [ffmpeg_codecs()] and [ffmpeg_encoders()] for structured capability
#'   queries, and the `ffm_*` pipeline builders (e.g. [ffm_run()]) for a safer
#'   command layer.
#' @family escape hatch functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' # Layer 0 escape hatch: the string is passed to FFmpeg verbatim
#' ffmpeg("-version")
#' @export
ffmpeg <- function(command) {
  rlang::check_string(command)
  # Redirect FFmpeg's stdin from an empty input (the `input = ""` temp file) so
  # it cannot drain the parent process's stdin. FFmpeg reads stdin for
  # interactive control while encoding; without this it would swallow whatever
  # is feeding R's stdin (e.g. the example stream during R CMD check). This is
  # the equivalent of FFmpeg's -nostdin flag, applied without touching the
  # verbatim `command` string.
  out <- system(glue('{find_ffmpeg()} {command}'), intern = TRUE, input = "")
  out
}

# extract_frame_batch() --------------------------------------------------------

#' Extract a single frame from a video
#'
#' Save one frame of a video to an image file, selected either by timestamp or
#' by frame number. Provide exactly one of \code{timestamp} or \code{frame}.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the image file to write.
#' @param timestamp Either a number of seconds, a time-duration-syntax string,
#'   or \code{NULL}. Provide exactly one of \code{timestamp} or \code{frame}.
#' @param frame Either an integerish frame number or \code{NULL}. Provide
#'   exactly one of \code{timestamp} or \code{frame}.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_seek()], the builder it uses to grab the frame;
#'   [extract_frame_batch()] for the many-file (batch) form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # run = FALSE returns the reproducible command instead of executing it
#' extract_frame(video, "frame.png", timestamp = 0.5, run = FALSE)
#' @export
extract_frame <- function(infile, outfile, timestamp = NULL, frame = NULL,
                          run = TRUE) {
  check_file_exists(infile)
  rlang::check_string(outfile)
  if (!is.null(timestamp) &&
      !(rlang::is_double(timestamp, n = 1, finite = TRUE) ||
        rlang::is_string(timestamp))) {
    cli::cli_abort("{.arg timestamp} must be a single number, a string, or {.code NULL}.")
  }
  if (!is.null(frame)) rlang::check_number_whole(frame)
  if (is.null(timestamp) == is.null(frame)) {
    cli::cli_abort("Provide exactly one of {.arg timestamp} or {.arg frame}.")
  }

  if (rlang::is_null(timestamp)) timestamp <- frame / get_frame_rate(infile)

  ffm_finish(frame_pipeline(infile, outfile, timestamp), run)
}


# frame_pipeline() --------------------------------------------------------

# Shared single-frame grab for extract_frame() and extract_frame_batch(): a fast
# input seek to the (already resolved) timestamp plus the still-image quality
# flags, one frame out. Both verbs build identical commands from this helper;
# frame->timestamp resolution stays in the verb layer (scalar vs. per-row).
frame_pipeline <- function(input, output, timestamp) {
  p <- ffm_files(input, output)
  p <- ffm_seek(p, start = timestamp, reencode = FALSE)
  ffm_output_options(
    p, "-qmin 1", "-q:v 1", "-qscale:v 2", "-frames:v 1", "-huffman optimal"
  )
}


# sample_frames() ---------------------------------------------------------

#' Sample frames from a video at a fixed rate
#'
#' Sample a video at a fixed rate (\code{fps}) or interval (\code{interval},
#' seconds between frames) into a numbered image sequence — the front door to
#' per-frame coding and computer-vision feature pipelines. Provide exactly one
#' of \code{fps} or \code{interval}.
#'
#' Unlike \code{\link{extract_frame}} (one frame) and
#' \code{\link{extract_frame_batch}} (a caller-enumerated set of frames), this
#' verb emits a \emph{single} FFmpeg command whose output is a printf-style
#' pattern that FFmpeg's \code{image2} muxer fills — the frame count is decided
#' at decode time, not enumerated by the caller. Frames are written to
#' \code{outdir} as \code{<prefix>_<n>.<format>}, where \code{<n>} is a
#' zero-padded integer starting at 1.
#'
#' @param infile A string containing the path to a video file.
#' @param outdir A string naming the directory to write the image sequence to.
#'   It is created (recursively) if it does not exist.
#' @param fps The sampling rate, in frames per second: either a positive number
#'   or an FFmpeg framerate expression string (for example \code{"30000/1001"}).
#'   Provide exactly one of \code{fps} or \code{interval}.
#' @param interval The number of seconds between sampled frames (a positive
#'   number); the reciprocal is used as the frame rate. Provide exactly one of
#'   \code{fps} or \code{interval}.
#' @param format A string giving the output image file extension (one of
#'   \code{"png"}, \code{"jpg"}, \code{"jpeg"}, \code{"bmp"}, \code{"tif"},
#'   \code{"tiff"}, \code{"webp"}). (default = \code{"png"})
#' @param prefix A string used as the basename stem of each image, or
#'   \code{NULL} to derive it from \code{infile}'s basename. (default =
#'   \code{NULL})
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_fps()], the builder it uses to set the sampling rate;
#'   [extract_frame()] for a single frame and [extract_frame_batch()] for a
#'   caller-enumerated set; [sample_frames_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # run = FALSE returns the reproducible command instead of executing it
#' sample_frames(video, tempdir(), fps = 2, run = FALSE)
#' @export
sample_frames <- function(infile, outdir, fps = NULL, interval = NULL,
                          format = "png", prefix = NULL, run = TRUE) {
  check_file_exists(infile)
  rlang::check_string(outdir)
  if (!is.null(prefix)) rlang::check_string(prefix)
  format <- check_image_format(format)
  fps <- resolve_sample_fps(fps, interval)
  outdir <- ensure_dir(outdir)

  pattern <- derive_frame_pattern(infile, outdir, prefix, format)
  ffm_finish(sample_frames_pipeline(infile, pattern, fps), run)
}


# sample_frames_pipeline() ------------------------------------------------

# Shared fixed-rate sampling pipeline for sample_frames() and
# sample_frames_batch(): a constant-rate fps filter into an image2 printf
# pattern, with a quality flag for the still encoder. Both verbs build identical
# commands from this helper; the fps filter's own value check (check_dim() via
# ffm_fps()) is inherited here, so the batch sibling gets per-row parity for
# free (M13). `output` is a %0Nd pattern, so the single command fans out to many
# image files (D003: still one input chain, one output target).
sample_frames_pipeline <- function(input, output, fps) {
  p <- ffm_files(input, output)
  p <- ffm_fps(p, fps = fps)
  ffm_output_options(p, "-qscale:v 2")
}


# resolve_sample_fps() ----------------------------------------------------

# Resolve the exclusive fps/interval pair to a single frame-rate value for
# ffm_fps(): `fps` passes through (a positive number, coerced to double so it
# clears check_dim()'s integer rejection, M20; or an FFmpeg rate-expression
# string), while `interval` (seconds/frame) becomes its reciprocal. Enforces the
# exactly-one contract, mirroring extract_frame()'s timestamp/frame XOR.
resolve_sample_fps <- function(fps, interval,
                               call = rlang::caller_env()) {
  if (is.null(fps) == is.null(interval)) {
    cli::cli_abort("Provide exactly one of {.arg fps} or {.arg interval}.",
                   call = call)
  }
  if (!is.null(fps)) {
    if (rlang::is_string(fps)) return(fps)
    if (!(rlang::is_bare_numeric(fps, n = 1) && is.finite(fps) && fps > 0)) {
      cli::cli_abort(
        "{.arg fps} must be a single positive number or a string.", call = call
      )
    }
    return(as.double(fps))
  }
  if (!(rlang::is_bare_numeric(interval, n = 1) && is.finite(interval) &&
        interval > 0)) {
    cli::cli_abort("{.arg interval} must be a single positive number.",
                   call = call)
  }
  1 / as.double(interval)
}


# derive_frame_pattern() --------------------------------------------------

# Build the image2 output pattern for one input: `<outdir>/<prefix>_%0Nd.<fmt>`,
# with `prefix` defaulting to the input's basename (sans extension). A fixed pad
# width keeps the numbering zero-padded and lexically sortable; FFmpeg widens it
# automatically past the cap. Basename prefixes distinguish most batch sequences
# sharing one `outdir`, but same-basename inputs still collide — the batch verb
# guards that at the pattern level before running.
derive_frame_pattern <- function(input, outdir, prefix, format, digits = 6L) {
  if (is.null(prefix)) prefix <- tools::file_path_sans_ext(basename(input))
  file.path(outdir, paste0(prefix, "_%0", digits, "d.", format))
}


# check_image_format() ----------------------------------------------------

# Validate `format` as a supported still-image extension, returning it
# lower-cased. Keeps a non-image container (e.g. "mp4") from silently producing
# a broken sequence, and a clean token from reaching the output pattern.
check_image_format <- function(format, arg = rlang::caller_arg(format),
                               call = rlang::caller_env()) {
  rlang::check_string(format, arg = arg, call = call)
  allowed <- c("png", "jpg", "jpeg", "bmp", "tif", "tiff", "webp")
  format <- tolower(format)
  if (!format %in% allowed) {
    cli::cli_abort(
      c("{.arg {arg}} must be a supported image format.",
        "x" = "{.val {format}} is not one of {.val {allowed}}."),
      call = call
    )
  }
  format
}


# ensure_dir() ------------------------------------------------------------

# Create `dir` (recursively) if it is absent, then confirm it exists — so an
# uncreatable path (e.g. under an existing file) aborts here with a clear
# message rather than as an opaque FFmpeg write error.
ensure_dir <- function(dir, arg = rlang::caller_arg(dir),
                       call = rlang::caller_env()) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dir)) {
      cli::cli_abort(
        c("Can't create {.arg {arg}}.", "x" = "Not a creatable directory: {.file {dir}}."),
        call = call
      )
    }
  }
  dir
}

# audio_stream_map() ------------------------------------------------------

# Resolve an `audio_stream` selector to the FFmpeg stream specifier the audio
# verbs map: `0:a:<n>`, the n-th audio stream *within the input* (0-based), not
# the n-th stream overall. NULL is the documented default and, on the extraction
# verbs, resolves to the same `0:a:0` an explicit 0 does -- the argument's own
# sentinel for "no selection", which is what lets a batch column's NA cell say
# "leave this row on the default" (M43).
#
# `null_map` is what NULL resolves to, and it is a parameter because M45 added a
# caller whose no-selection default is a DIFFERENT MAP, not merely a different
# hint: on separate_audio_video() NULL stays `0:a` -- EVERY audio track -- which
# is what that verb has compiled since it shipped and what its Matroska callers
# receive today, so baking `0:a:0` in here would have silently narrowed them to
# one track. D023's NULL bullet reads the other way for the extraction verbs and
# the M45 D-entry records the split. Same shape as
# check_batch_audio_col(na_means =), parameterized at M43 when a new caller's NA
# meant something else (M40's lesson).
#
# The check lives here so every caller inherits it, including a batch row whose
# value arrives from an override column rather than the argument (M13/M32); the
# scalar verbs check again at their own front door so a bad argument blames the
# verb rather than aborting mid-fan-out (M41).
audio_stream_map <- function(audio_stream = NULL, null_map = "0:a:0",
                             call = rlang::caller_env()) {
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE,
                            arg = "audio_stream", call = call)
  if (is.null(audio_stream)) return(null_map)
  sprintf("0:a:%d", as.integer(audio_stream))
}

# warn_dropped_audio() ----------------------------------------------------

# Emit the single classed warning for inputs carrying audio tracks the output
# does not receive. `inputs` and `n` are parallel vectors (`n` = that input's
# audio-stream count from count_audio_streams(), NA where it could not be had);
# `rows` is the jobs-table row index per element, or NULL on the scalar path.
#
# ONE warning whatever the length. The batch form names every affected row
# instead of warning per row, so a large jobs table cannot bury the message
# under R's "There were 50 or more warnings" collapse (M44 gate). Scalar and
# batch share this builder so their wording cannot drift -- the divergence this
# repo has already fixed twice (M19, M35).
#
# NA `n` means the probe could not answer, and is skipped silently: D024 licenses
# this probe only while its outcome changes nothing but whether a diagnostic is
# signalled, so "could not check" must look exactly like "nothing to report".
# Callers gate on `run` and on audio_stream being NULL before they get here; this
# builder decides only whether there is anything to say.
warn_dropped_audio <- function(inputs, n, rows = NULL,
                               call = rlang::caller_env()) {
  keep <- !is.na(n) & n > 1
  if (!any(keep)) return(invisible(NULL))
  inputs <- inputs[keep]
  n <- as.integer(n[keep])
  dropped <- n - 1L
  bullets <- if (is.null(rows)) {
    sprintf("%s carries %d audio tracks; the output takes 1 and drops %d.",
            inputs, n, dropped)
  } else {
    sprintf("Row %d (%s) carries %d audio tracks; the output takes 1 and drops %d.",
            rows[keep], basename(inputs), n, dropped)
  }
  # cli_warn() glue-interpolates every bullet in this function's own frame, so a
  # file path carrying a brace is executed rather than printed: `my{video}.mkv`
  # ABORTS the verb ("could not evaluate cli expression"), and `{n}.mkv` --
  # naming a local of this very function -- silently prints a filename that does
  # not exist. Either one turns a diagnostic into something observable beyond the
  # diagnostic, which is exactly what D024 licenses this probe on NOT doing.
  # sprintf() has already built the line, so escape rather than route through a
  # cli field: doubling is what glue reads as a literal brace. Braces are legal
  # in filenames on every platform this package supports (M44 review F1).
  bullets <- gsub("}", "}}", gsub("{", "{{", bullets, fixed = TRUE), fixed = TRUE)
  cli::cli_warn(
    c(
      "Dropping {sum(dropped)} audio track{?s} from {length(dropped)} input{?s}.",
      rlang::set_names(bullets, rep("x", length(bullets))),
      "i" = "Name the track you want with {.arg audio_stream}: {.val {0}} is \\
             the first audio track, {.val {1}} the second.",
      "i" = "{.fn probe_audio} lists the tracks, but its {.field index} column \\
             counts ALL streams while {.arg audio_stream} counts audio streams \\
             from {.val {0}} -- on a video file with three audio tracks those \\
             read 1, 2, 3 there and 0, 1, 2 here."
    ),
    class = "tidymedia_dropped_audio",
    call = call
  )
  invisible(NULL)
}

# warn_dropped_audio_batch() ----------------------------------------------

# The batch form of the D024 diagnostic: probe the rows that named no track and
# emit ONE aggregated warning naming every affected row.
#
# Runs in the Layer-2 verb BEFORE ffm_batch(), never inside it. ffm_batch()'s
# contract is generic -- any verb, any pipeline -- and a track-drop diagnostic is
# these two verbs' semantics, so it does not belong in the runner (D011 settled
# that verb-agnostic verification may live there and verb-specific meaning may
# not, and an engine-signature change for one diagnostic inverts the thin-verb
# economy of IP1). Up front is also the better diagnostic: the warning lands
# before the fan-out spends its time encoding, while the caller can still stop
# and add audio_stream, and before FFmpeg's console output buries it.
#
# `audio_stream` is the scalar argument; an `audio_stream` column overrides it
# per row, where an NA cell is the column form of the NULL sentinel -- "leave
# this row on the first track" (D023) -- so an NA cell is a row that named NO
# track and is probed. A batch whose every row names one probes nothing at all.
# Unique inputs are probed once: a jobs table legitimately repeats an input (the
# package's own examples do) and the answer is per file, not per row.
warn_dropped_audio_batch <- function(jobs, audio_stream = NULL,
                                     call = rlang::caller_env()) {
  sel <- if ("audio_stream" %in% names(jobs)) {
    jobs$audio_stream
  } else {
    rep(if (is.null(audio_stream)) NA_real_ else as.numeric(audio_stream),
        nrow(jobs))
  }
  rows <- which(is.na(sel))
  if (length(rows) == 0) return(invisible(NULL))
  inputs <- jobs$input[rows]
  uniq <- unique(inputs)
  counts <- vapply(uniq, count_audio_streams, integer(1), USE.NAMES = FALSE)
  warn_dropped_audio(inputs, counts[match(inputs, uniq)], rows = rows,
                     call = call)
}

# extract_audio() ---------------------------------------------------------

# Shared recipe behind extract_audio() and extract_audio_batch(): map the audio
# stream out (dropping video), applying `audio_codec` (default "copy" =
# stream-copy, lossless). Holding it here gives the batch sibling per-row parity
# for free (M13); command assembly stays in Layer 1 (IP1/D002).
#
# The map is explicit (`0:a:0` by default) rather than absent. Emitting no -map
# left the choice to FFmpeg's default-stream heuristic, which prefers the track
# carrying the container's DEFAULT disposition -- so the extracted track depended
# on the file's flags rather than on anything the caller or this package said,
# and could differ across FFmpeg versions. `audio_stream` names the track
# instead (M43). The `-vn` from ffm_drop() is now redundant beside the map and is
# kept anyway: it costs nothing, keeps every existing compiled command a
# superset of what it was TOKEN-wise, and this verb is still documented as an
# ffm_drop() caller.
#
# A superset of tokens is NOT a superset of output streams, and the difference is
# a real behavior change. With no -map, FFmpeg's default selection carried one
# stream of EACH type, so a subtitle stream reached any container that accepts
# one; -vn removed only the video. Measured at M43 review: a video+audio+srt
# input written to .mkv gave audio+subtitle before and audio alone now.
# Audio-only containers never carried it, so .aac / .m4a / .mka are unaffected.
# Documented in NEWS rather than restored -- this verb extracts audio, and the
# subtitle was FFmpeg's default leaking through rather than anything the package
# chose.
extract_audio_pipeline <- function(input, output, audio_codec = "copy",
                                   audio_stream = NULL,
                                   call = rlang::caller_env()) {
  p <- ffm_files(input, output)
  p <- ffm_codec(p, audio = audio_codec)
  p <- ffm_map(p, audio_stream_map(audio_stream, call = call))
  ffm_drop(p, "video")
}

#' Extract the audio stream from a media file
#'
#' Pulls one audio track out of \code{infile}, dropping the video. When the
#' input carries more than one audio track, \code{audio_stream} names which one
#' to take; with no selector the \strong{first} audio track is taken.
#'
#' When no \code{audio_stream} is named and the input turns out to carry tracks
#' the output will not, the verb warns. That check is \strong{best-effort}: it
#' runs FFprobe, so it is emitted when FFprobe is available and the input can be
#' probed, and is skipped silently otherwise. It never runs under \code{run =
#' FALSE}, and never changes the compiled command. Suppress it by naming a track
#' with \code{audio_stream}, or by class with
#' \code{suppressWarnings(classes = "tidymedia_dropped_audio")}.
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the audio file to write.
#' @param audio_codec A string naming the audio codec for the output stream
#'   (default \code{"copy"}, i.e. remux without re-encoding), or \code{NULL} to
#'   emit no \code{-codec:a} and let the output container's default encoder
#'   decide — useful when the source codec cannot be copied into the extension
#'   you asked for.
#' @param audio_stream The 0-based index of the audio track to take, counted
#'   \emph{among the input's audio streams} — \code{0} is the first audio track,
#'   \code{1} the second, whatever their positions among the file's streams.
#'   \code{NULL} (default) takes the first audio track. Naming a track the input
#'   does not have is an FFmpeg error, not an R one.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_drop()] and [ffm_codec()], the builders it wraps;
#'   [convert_audio()] to re-encode the extracted audio;
#'   [extract_audio_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' extract_audio(video, "audio.aac", run = FALSE)
#' # Take the second audio track instead of the first
#' extract_audio(video, "audio.aac", audio_stream = 1, run = FALSE)
#' @export
extract_audio <- function(infile, outfile, audio_codec = "copy",
                          audio_stream = NULL, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  # allow_null is D022's family rule: NULL emits no -codec:a and lets the output
  # container's default encoder decide. This verb refused it until M42 while
  # extract_audio_batch() next door had always compiled the same call.
  rlang::check_string(audio_codec, allow_null = TRUE)
  # States the contract at the signature, where the repo puts scalar validation
  # (M32/M37/M41). It is deliberately NOT what produces the blame: deleting this
  # line leaves every test green, because audio_stream_map() carries the same
  # check and its `call` already resolves to this verb's frame. Kept as
  # defense-in-depth against that `call` chain being refactored away, not
  # because it currently changes any message -- the earlier comment here claimed
  # it did, which the M42 delete-and-run probe disproved at review.
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE)

  # D024's diagnostic probe. Gated on `run` because compilation and every
  # run = FALSE call stay binary-free, and on a NULL audio_stream because that
  # is the only case where the drop is implicit -- a caller who named a track
  # chose it. The probe changes nothing but this warning: the compiled command
  # below is byte-identical whether it runs, succeeds, or fails. isTRUE() rather
  # than a bare `run` so a non-logical value still gets ffm_finish()'s own
  # check_bool() message.
  if (isTRUE(run) && is.null(audio_stream)) {
    warn_dropped_audio(infile, count_audio_streams(infile))
  }

  ffm_finish(
    extract_audio_pipeline(infile, outfile, audio_codec, audio_stream),
    run
  )
}


# separate_stream_pipeline() ----------------------------------------------

# Shared recipe behind separate_audio_video() and separate_audio_video_batch():
# build one single-output pipeline for a single stream — map `0:a` (audio) or
# `0:v` (video) out of `input` into `output`, naming that stream's encoder via
# `codec`. The default `"copy"` stream-copies, the lossless path this verb has
# had since D-M06-4; `NULL` is D016's sentinel, emitting no `-codec` at all so
# the output container's default encoder decides (M37). `codec` applies to the
# audio or the video slot by `stream`, so a caller's audio choice can never
# reach the video command. `hardware`/`fallback` ride the same routing (M38):
# nvenc encodes video, so only the video branch forwards them and the audio
# command is byte-identical whatever the caller asked for -- the routing lives
# here once rather than at each call site. `audio_stream` rides the same routing
# in the other direction: it narrows the audio map to one track and the video
# branch never reads it, so the video command is byte-identical whatever the
# caller selected (M45).
# Splitting one input into audio + video is a fan-out,
# so each stream stays its own single-output pipeline (D003/D007); both verbs
# wrap this once per stream. Command assembly stays in Layer 1 (IP1/D002). Kept
# ABOVE the roxygen block below so document() does not re-target it (M28 lesson).
separate_stream_pipeline <- function(input, output, stream, codec = "copy",
                                     hardware = "none", fallback = FALSE,
                                     audio_stream = NULL,
                                     call = rlang::caller_env()) {
  # `null_map = "0:a"` keeps EVERY audio track when the caller named none, which
  # is this verb's map since it shipped -- audio_stream_map()'s own default is
  # the extraction verbs' `0:a:0` and would silently narrow to one track (M45).
  map <- if (stream == "audio") {
    audio_stream_map(audio_stream, null_map = "0:a", call = call)
  } else {
    "0:v"
  }
  p <- ffm_map(ffm_files(input, output), map)
  if (stream == "audio") {
    # nvenc encodes video, so `hardware`/`fallback` never reach this branch and
    # the audio command is byte-identical whatever the caller asked for (M38).
    apply_audio_codec(p, codec, call = call)
  } else {
    # A stream copy writes the source video bytes through untouched, so no
    # encoder -- GPU or software -- runs on that path (D008 keeps the copy
    # lossless and opt-in; D016 rules the same way for segment_video). Catch it
    # here rather than letting codec_family("copy") abort, which blames the
    # codec name instead of the copy. The guard sits in the shared recipe so
    # both verbs inherit it per stream; ffm_batch builds every row's pipeline
    # before running any (R/ffm_batch.R), so a batch fails before it encodes.
    if (identical(codec, "copy") && !identical(hardware, "none")) {
      cli::cli_abort(
        c(
          "{.arg hardware} needs a re-encoding {.arg video_codec}.",
          "x" = "{.code video_codec = \"copy\"} stream-copies the video, so no
                 encoder runs.",
          "i" = "Name an encoder (e.g. {.code video_codec = \"libx264\"}), or
                 pass {.code video_codec = NULL} to assume the H.264 family --
                 a non-H.264 container then needs an explicit HEVC- or
                 AV1-family codec.",
          "i" = "Or drop {.arg hardware} to keep stream-copying the video."
        ),
        call = call
      )
    }
    apply_video_codec(p, codec, hardware, fallback, call = call)
  }
}


# run_separation_audio() --------------------------------------------------

# Run separate_audio_video()'s AUDIO command and, when FFmpeg refuses it on a
# multi-track input, re-raise with the way out. Lives in Layer 2, never in
# ffm_run(): the message names `audio_stream`, a Layer-2 argument the engine has
# no business knowing (IP1/D002), and an engine hook for one verb's diagnostic is
# the same inversion D024/RR02 Q3 rejected for ffm_batch().
#
# Enrichment is deliberately NARROW -- it fires only when the caller named NO
# track. With `0:a:<n>` mapped the command carried exactly one stream, so a
# failure is something else (a codec the container will not hold, a bad path) and
# "take one track with audio_stream" would be false under the branch that fired
# it -- M38's lesson, which this repo has now paid for twice.
#
# The probe is D024's diagnostic licence at its narrowest: it runs only after
# FFmpeg has ALREADY failed, so the call aborts under every outcome and the probe
# decides only WHICH abort is signalled -- never whether execution proceeds
# (D024's third exclusion), never what was compiled, never a default, never a
# pipeline. It fails open to ffm_run()'s own abort, unchanged in text and class.
run_separation_audio <- function(pipeline, infile, outfile, audio_stream,
                                 call = rlang::caller_env()) {
  if (!is.null(audio_stream)) return(invisible(ffm_run(pipeline)))
  tryCatch(
    ffm_run(pipeline),
    error = function(cnd) {
      # Parsing the status out of ffm_run()'s own message is also what tells a
      # non-zero EXIT apart from every other way the run can fail: a missing
      # ffmpeg binary aborts in run_program() with no status at all, and a track
      # count would be a nonsense answer to that. NA therefore means "not the
      # failure this diagnostic is about" as well as "no status", and both fall
      # through. A test pins the coupling to that wording, so rewording
      # ffm_run()'s abort fails loudly instead of silently killing this branch.
      status <- ffmpeg_exit_status(cnd)
      n <- if (is.na(status)) NA_integer_ else count_audio_streams(infile)
      # Fail open: no status, no probe answer, or a single-track input all
      # re-raise the ORIGINAL condition object, so its message, class and trace
      # are the ones ffm_run() raises today (D024's fail-open consequence).
      if (is.na(status) || is.na(n) || n <= 1L) stop(cnd)
      cli::cli_abort(
        c(
          "Can't write {.file {outfile}}: FFmpeg exited with status {status}.",
          "x" = "{.file {infile}} carries {n} audio tracks and no
                 {.arg audio_stream} was named, so all {n} were mapped into one
                 output.",
          "i" = "Most audio containers hold exactly one stream ({.file .aac},
                 {.file .mp3}, {.file .wav}) and FFmpeg fails when asked to
                 write more.",
          "i" = "Take one track with {.arg audio_stream}: {.val {0}} is the
                 first audio track, {.val {1}} the second.",
          "i" = "Or keep all {n} by writing a container that holds several --
                 Matroska ({.file .mka}) or {.file .m4a}."
        ),
        class = "tidymedia_multitrack_separation",
        parent = cnd,
        call = call
      )
    }
  )
}

# warn_failed_separation() ------------------------------------------------

# The batch form of T2's abort. separate_audio_video_batch() cannot abort on a
# failed row -- ffm_batch() records `success = FALSE` and carries on, which is the
# batch contract (D007) -- so the same diagnostic arrives as ONE warning after the
# fan-out, naming every audio row that failed on a multi-track input without
# naming a track.
#
# ONE warning whatever the length, for M44's reason: R collapses many warnings
# into "There were 50 or more warnings" and a large jobs table would bury the
# message it exists to deliver. The wording carries the same three clauses the
# scalar abort does -- the count, `audio_stream`, and a container that holds
# several -- so scalar and batch cannot drift, the divergence this repo has fixed
# twice (M19, M35).
#
# `rows` are INPUT row numbers (the caller's jobs table), not row numbers of the
# 2N-row result: a caller reads the message to fix a row of the table they wrote.
warn_failed_separation <- function(rows, inputs, outputs, n,
                                   call = rlang::caller_env()) {
  keep <- !is.na(n) & n > 1
  if (!any(keep)) return(invisible(NULL))
  rows <- rows[keep]
  inputs <- inputs[keep]
  outputs <- outputs[keep]
  n <- as.integer(n[keep])
  bullets <- sprintf(
    "Input row %d (%s) carries %d audio tracks; FFmpeg would not write all %d to %s.",
    rows, basename(inputs), n, n, basename(outputs)
  )
  # Double every brace sprintf() has already interpolated: cli glue-evaluates
  # each bullet in THIS frame, so a path like `my{n}.aac` would otherwise print a
  # local of this function and `my{video}.aac` would abort the call outright.
  # Braces are legal in filenames everywhere this package runs (M44 review F1).
  bullets <- gsub("}", "}}", gsub("{", "{{", bullets, fixed = TRUE), fixed = TRUE)
  cli::cli_warn(
    c(
      "{length(rows)} audio output{?s} failed on a multi-track input.",
      rlang::set_names(bullets, rep("x", length(bullets))),
      "i" = "Most audio containers hold exactly one stream ({.file .aac}, \\
             {.file .mp3}, {.file .wav}) and FFmpeg fails when asked to write \\
             more.",
      "i" = "Take one track with {.arg audio_stream}, batch-wide or as a per-row \\
             {.field audio_stream} column: {.val {0}} is the first audio track, \\
             {.val {1}} the second.",
      "i" = "Or keep every track by writing a container that holds several -- \\
             Matroska ({.file .mka}) or {.file .m4a}."
    ),
    class = "tidymedia_multitrack_separation",
    call = call
  )
  invisible(NULL)
}

# Probe the failed audio rows of a finished separation batch and warn once.
#
# Runs AFTER ffm_batch() returns, unlike M44's up-front probe: this diagnostic is
# about rows that actually failed, which is not knowable before they run, and the
# probe therefore costs nothing on a batch where every row succeeds. Same D024
# licence as M44's (see this milestone's M45-D2): the outcome moves nothing but
# whether the warning is signalled, and an unanswerable count is skipped in
# silence rather than reported as a second failure.
#
# `out` is the 2N-row result; audio rows are the odd ones, so an input row number
# is (i + 1) %/% 2. A row that NAMED a track is excluded -- it mapped one stream,
# so a track count says nothing about why it failed (T2's narrowing).
warn_failed_separation_batch <- function(out, audio_stream = NULL,
                                        call = rlang::caller_env()) {
  if (!"success" %in% names(out)) return(invisible(NULL))
  sel <- if ("audio_stream" %in% names(out)) {
    out$audio_stream
  } else {
    rep(if (is.null(audio_stream)) NA_real_ else as.numeric(audio_stream),
        nrow(out))
  }
  bad <- which(out$stream == "audio" & !out$success & is.na(sel))
  if (length(bad) == 0) return(invisible(NULL))
  inputs <- out$input[bad]
  uniq <- unique(inputs)
  counts <- vapply(uniq, count_audio_streams, integer(1), USE.NAMES = FALSE)
  warn_failed_separation(
    rows = (bad + 1L) %/% 2L,
    inputs = inputs,
    outputs = out$output[bad],
    n = counts[match(inputs, uniq)],
    call = call
  )
}

# Pull FFmpeg's exit status out of the abort ffm_run() raises on a non-zero exit
# ("FFmpeg exited with status 234."), or NA when the condition is not that abort.
# cli styles the message, so strip the ANSI first.
ffmpeg_exit_status <- function(cnd) {
  msg <- cli::ansi_strip(conditionMessage(cnd))
  hit <- regmatches(msg, regexpr("exited with status -?[0-9]+", msg))
  if (length(hit) == 0L) return(NA_integer_)
  as.integer(sub("^exited with status ", "", hit[[1]]))
}


# separate_audio_video() --------------------------------------------------

#' Split a media file into separate audio and video files
#'
#' By default each stream is copied, not re-encoded (\code{audio_codec =
#' "copy"}, \code{video_codec = "copy"}): separation is lossless and fast, but
#' each output container must support the source codec (e.g. write AAC audio
#' from an MP4 to \code{.aac} or \code{.m4a}, not \code{.mp3}). Name an encoder
#' instead (\code{audio_codec = "libmp3lame"}) to transcode that stream, or pass
#' \code{NULL} to emit no codec option at all and let the output extension pick
#' the encoder. Each argument governs only its own output file. Where the video
#' is re-encoded, \code{hardware = "nvenc"} moves that encode onto an NVIDIA
#' GPU; the audio output is never affected.
#'
#' @param infile A string containing the path to a media file.
#' @param audiofile A string containing the path of the audio file to write.
#' @param videofile A string containing the path of the video file to write.
#' @param audio_codec A string naming the encoder for \code{audiofile}, passed
#'   to FFmpeg's \code{-codec:a}. The default \code{"copy"} stream-copies the
#'   audio losslessly; a codec name (e.g. \code{"libmp3lame"}) transcodes it;
#'   \code{NULL} emits no \code{-codec:a}, leaving the encoder to the
#'   \code{audiofile} extension.
#' @param video_codec A string naming the encoder for \code{videofile}, passed
#'   to FFmpeg's \code{-codec:v}. The default \code{"copy"} stream-copies the
#'   video losslessly; a codec name (e.g. \code{"libx264"}) transcodes it;
#'   \code{NULL} emits no \code{-codec:v}, leaving the encoder to the
#'   \code{videofile} extension.
#' @param hardware The encoder backend for \code{videofile}: \code{"none"}
#'   (default, the software \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU
#'   encoding, which uses the nvenc encoder for \code{video_codec}'s family
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}), assuming the H.264
#'   family when \code{video_codec = NULL}. Only video is encoded on the GPU, so
#'   this never affects \code{audiofile}. Because this verb's video default is a
#'   stream copy, which runs no encoder at all, \code{hardware = "nvenc"}
#'   alongside \code{video_codec = "copy"} is an error: name an encoder or pass
#'   \code{video_codec = NULL}. See \code{\link{has_nvenc}} for availability and
#'   its caveats.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, encode in software with a message (\code{TRUE}) instead of
#'   aborting (\code{FALSE}, default). With \code{video_codec = NULL} the
#'   fallback leaves the codec unset rather than injecting one.
#' @param audio_stream The 0-based index of the audio track to write to
#'   \code{audiofile}, counted among \code{infile}'s \emph{audio} streams only
#'   (\code{0} is the first audio track, \code{1} the second) — not the
#'   \code{index} column of \code{\link{probe_audio}}, which counts all streams.
#'   \code{NULL} (default) takes \strong{every} audio track, which is what this
#'   verb has always done: a container that holds several (\code{.mka},
#'   \code{.m4a}) receives them all, while a single-stream container
#'   (\code{.aac}, \code{.mp3}, \code{.wav}) makes FFmpeg fail — name a track to
#'   write one of those. This differs from \code{\link{extract_audio}} and
#'   \code{\link{convert_audio}}, whose \code{NULL} takes the first track only.
#'   \code{videofile} is never affected.
#' @param run A logical: run the commands through FFmpeg (\code{TRUE}, default)
#'   or return the compiled commands without running them (\code{FALSE}).
#' @return A named character vector of the two compiled commands
#'   (\code{audio}, \code{video}); invisible when \code{run = TRUE}.
#' @seealso [ffm_map()] and [ffm_codec()], the builders it wraps;
#'   [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [extract_audio()] to pull out just the audio;
#'   [probe_audio()] to list an input's audio tracks.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#' # transcode the audio to MP3 while copying the video through untouched
#' separate_audio_video(video, "audio.mp3", "video.mp4",
#'                      audio_codec = "libmp3lame", run = FALSE)
#' # write only the second audio track of a multi-track input
#' separate_audio_video(video, "audio.aac", "video.mp4",
#'                      audio_stream = 1, run = FALSE)
#' @export
separate_audio_video <- function(infile, audiofile, videofile,
                                 audio_codec = "copy", video_codec = "copy",
                                 hardware = c("none", "nvenc"),
                                 fallback = FALSE, audio_stream = NULL,
                                 run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(audiofile)
  rlang::check_string(videofile)
  # Resolve `hardware` at the front door: the copy guard below compares it to
  # "none", so the unresolved default vector would fire the guard on every
  # default call. Validating here also attributes the error to this verb (M37).
  hardware <- rlang::arg_match(hardware)
  rlang::check_bool(fallback)
  # Last of the front-door checks, so adding it cannot move the precedence of
  # the ones above (M41). It duplicates the check inside audio_stream_map(),
  # which the audio pipeline below always reaches with `call` resolving to this
  # frame -- so on the scalar verb it is defense-in-depth and parity with the
  # batch sibling, where the same call IS load-bearing because the reshape reads
  # NA as the NULL sentinel. No test is named after this line (M43's finding).
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE)

  # One input -> two outputs is a fan-out: emit two single-output pipelines
  # (D-M03-2) rather than a dual-`-map` command the linear engine can't model.
  # separate_stream_pipeline() carries the per-stream recipe shared with
  # separate_audio_video_batch(), and token-checks each codec there.
  # `audio_stream` is passed to the audio call only: the video command cannot
  # narrow its map even by mistake, because the value never reaches it.
  audio <- separate_stream_pipeline(infile, audiofile, "audio", audio_codec,
                                    hardware, fallback, audio_stream)
  video <- separate_stream_pipeline(infile, videofile, "video", video_codec,
                                    hardware, fallback)
  commands <- c(audio = ffm_compile(audio), video = ffm_compile(video))

  if (run) {
    # The audio command runs first and aborts the verb when it fails, so the
    # video file is not written either -- unchanged behavior, and a ROADMAP
    # candidate rather than this milestone's business (M45 Out).
    run_separation_audio(audio, infile, audiofile, audio_stream)
    ffm_run(video)
    invisible(commands)
  } else {
    commands
  }
}


# convert_audio() ---------------------------------------------------------

# Shared recipe behind convert_audio() and convert_audio_batch(): map the audio
# stream out and either encode at highest VBR quality (`audio_codec = NULL`, the
# extension picks the codec) or pin `-c:a` to `audio_codec`. The per-value
# check_string(audio_codec) lives here so the batch sibling inherits it per row
# (M13); command assembly stays in Layer 1 (IP1/D002).
#
# The map is `0:a:<n>` -- ONE audio stream of the input -- not `a`, which is
# every audio stream. On a multi-track input the unbounded form fed three
# streams to a single-stream muxer and FFmpeg aborted ("Exactly one MP3 audio
# stream is required", exit 65514) leaving a zero-byte output, against a
# documented contract that has always been singular. The hotfix made the track
# taken deterministic and one; `audio_stream` now lets the caller choose WHICH,
# and its default resolves to the same `0:a:0` the hotfix pinned (M43).
#
# The argument was spelled `format` until M40 renamed it to D014's `audio_codec`
# vocabulary; the NULL branch is unchanged, so every default command stays
# byte-identical. NULL here means "-q:a 0", NOT D016's emit-nothing sentinel --
# the departure is deliberate and recorded in D021.
convert_audio_pipeline <- function(input, output, audio_codec = NULL,
                                   audio_stream = NULL,
                                   call = rlang::caller_env()) {
  p <- ffm_files(input, output)
  p <- ffm_map(p, audio_stream_map(audio_stream, call = call))
  if (is.null(audio_codec)) {
    p <- ffm_output_options(p, "-q:a 0")
  } else {
    rlang::check_string(audio_codec)
    p <- ffm_codec(p, audio = audio_codec)
  }
  p
}

#' Extract or convert a media file's audio track
#'
#' Maps the audio stream of \code{infile} into \code{outfile}. By default
#' (\code{audio_codec = NULL}) the output format follows the \code{outfile} file
#' extension at highest VBR quality (\code{-q:a 0}) — e.g. an \code{.mp3}
#' extension yields an MP3. Pass \code{audio_codec} to pin the output audio
#' codec explicitly, regardless of the extension.
#'
#' When \code{infile} carries more than one audio track, \code{audio_stream}
#' names which one to take; with no selector the \strong{first} one is taken.
#'
#' When no \code{audio_stream} is named and the input turns out to carry tracks
#' the output will not, the verb warns. That check is \strong{best-effort}: it
#' runs FFprobe, so it is emitted when FFprobe is available and the input can be
#' probed, and is skipped silently otherwise. It never runs under \code{run =
#' FALSE}, and never changes the compiled command. Suppress it by naming a track
#' with \code{audio_stream}, or by class with
#' \code{suppressWarnings(classes = "tidymedia_dropped_audio")}.
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the audio file to write.
#' @param audio_codec An optional string naming the output audio codec (e.g.
#'   \code{"libmp3lame"}, \code{"aac"}, \code{"flac"}), passed to FFmpeg's
#'   \code{-c:a}. When \code{NULL} (default), the codec is inferred from the
#'   \code{outfile} extension and encoded at highest VBR quality. Unlike the
#'   other transform verbs, \code{NULL} here is \emph{not} the "leave the codec
#'   unset" sentinel — it selects \code{-q:a 0}.
#' @param audio_stream The 0-based index of the audio track to take, counted
#'   \emph{among the input's audio streams} — \code{0} is the first audio track,
#'   \code{1} the second, whatever their positions among the file's streams.
#'   \code{NULL} (default) takes the first audio track. Naming a track the input
#'   does not have is an FFmpeg error, not an R one.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_codec()] and [ffm_map()], the builders it wraps;
#'   [extract_audio()] to copy audio without re-encoding;
#'   [convert_audio_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' convert_audio(video, "audio.mp3", run = FALSE)
#' convert_audio(video, "audio.m4a", audio_codec = "aac", run = FALSE)
#' # Convert the second audio track instead of the first
#' convert_audio(video, "audio.mp3", audio_stream = 1, run = FALSE)
#' @export
convert_audio <- function(infile, outfile, audio_codec = NULL,
                          audio_stream = NULL, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  # Duplicates the check inside convert_audio_pipeline() on purpose. That helper
  # is shared with convert_audio_batch(), which relies on it for per-row
  # validation, so threading a `call` through it would also rewrite the batch
  # verb's per-row messages; checking here instead blames this verb and leaves
  # the batch path untouched (M41). NULL passes straight through: it is not the
  # emit-nothing sentinel here, it selects `-q:a 0` (D021).
  #
  # Spelled this way rather than check_string(allow_null = TRUE) so the message
  # matches convert_audio_batch()'s, which is the identical guard on the
  # identical value. The allow_null spelling said "must be a single string or
  # `NULL`" while the batch sibling said "must be a single string" -- a
  # divergence this milestone introduced, and one M41 has no business
  # introducing when it exists to make these messages agree (review A7).
  # Neither message mentions the `NULL` both verbs accept; that is a
  # pre-existing habit across the codec family (review A8) and M42's to settle.
  if (!is.null(audio_codec)) rlang::check_string(audio_codec)
  # States the contract at the signature, where the repo puts scalar validation
  # (M32/M37/M41). It is deliberately NOT what produces the blame: deleting this
  # line leaves every test green, because audio_stream_map() carries the same
  # check and its `call` already resolves to this verb's frame. Kept as
  # defense-in-depth against that `call` chain being refactored away, not
  # because it currently changes any message -- the earlier comment here claimed
  # it did, which the M42 delete-and-run probe disproved at review.
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE)

  # D024's diagnostic probe; see extract_audio() for why it is gated on `run`
  # and on a NULL audio_stream.
  if (isTRUE(run) && is.null(audio_stream)) {
    warn_dropped_audio(infile, count_audio_streams(infile))
  }

  # No `...` here, so a stale `format =` gets R's own `unused argument` error --
  # no guard needed (M37 lesson; the batch sibling, which has `...`, does need
  # one).
  ffm_finish(
    convert_audio_pipeline(infile, outfile, audio_codec, audio_stream),
    run
  )
}

# crop_video() ------------------------------------------------------------

# Shared recipe behind crop_video() and crop_video_batch(): a crop filter to the
# requested rectangle mapping every stream through. ffm_crop() carries the
# per-value dimension guards, so the batch sibling inherits them per row (M13);
# command assembly stays in Layer 1 (IP1/D002).
crop_video_pipeline <- function(input, output, width, height,
                                x = "(in_w-out_w)/2", y = "(in_h-out_h)/2",
                                video_codec = NULL, audio_codec = "copy",
                                hardware = "none",
                                fallback = FALSE, call = rlang::caller_env()) {
  p <- ffm_files(input, output)
  p <- ffm_crop(p, width = width, height = height, x = x, y = y)
  p <- ffm_map(p, "0")
  # -map 0 carries the audio through, and the default audio_codec = "copy"
  # stream-copies it rather than letting the container's default encoder
  # re-encode it (M35/D017).
  p <- apply_audio_codec(p, audio_codec, call = call)
  # The default video_codec = NULL emits no -codec:v, so the output keeps its
  # container's default *video* encoder. This no longer makes the whole command
  # byte-identical to the pre-M34 one -- M35's audio default added -codec:a copy
  # (M34/D016, M35/D017).
  apply_video_codec(p, video_codec, hardware, fallback, call = call)
}

#' Crop a video to a rectangular region
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param width The width of the output video, in pixels.
#' @param height The height of the output video, in pixels.
#' @param x The horizontal offset, in pixels, of the left edge of the crop.
#'   (default = centered)
#' @param y The vertical offset, in pixels, of the top edge of the crop.
#'   (default = centered)
#' @param video_codec A string naming the output video codec, or \code{NULL}
#'   (default) to leave it unset, so the output container's default encoder is
#'   used and the compiled command is unchanged from one that never named a
#'   codec.
#' @param audio_codec A string naming the output audio codec. \code{"copy"}
#'   (default) stream-copies the audio through untouched; name an encoder (e.g.
#'   \code{"aac"}) to transcode it, or pass \code{NULL} to leave the codec unset
#'   so the output container's default encoder is used. Stream-copying fails if
#'   the output container cannot hold the source audio codec (e.g. FLAC in
#'   \code{.mp4}) — name an encoder in that case.
#' @param hardware The encoder backend: \code{"none"} (default, the software
#'   \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU encoding. When
#'   \code{"nvenc"}, the nvenc encoder for \code{video_codec}'s family is used
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}); with the default
#'   \code{video_codec = NULL} the H.264 family is assumed, so a non-H.264
#'   container (e.g. \code{.webm}) needs an explicit HEVC- or AV1-family
#'   \code{video_codec}. See \code{\link{has_nvenc}} for availability and its
#'   caveats.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, encode in software with a message (\code{TRUE}) instead of
#'   aborting (\code{FALSE}, default). With \code{video_codec = NULL} the
#'   fallback leaves the codec unset rather than picking one, so the codec never
#'   changes silently.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_crop()], the builder it wraps; [has_nvenc()] for the
#'   \code{hardware = "nvenc"} toggle;
#'   [crop_video_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' crop_video(video, "cropped.mp4", width = 160, height = 120, run = FALSE)
#' @export
crop_video <- function(infile, outfile, width, height,
                       x = "(in_w-out_w)/2", y = "(in_h-out_h)/2",
                       video_codec = NULL, audio_codec = "copy",
                       hardware = c("none", "nvenc"),
                       fallback = FALSE, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  ffm_finish(
    crop_video_pipeline(infile, outfile, width, height, x, y,
                        video_codec, audio_codec, hardware, fallback),
    run
  )
}


# format_for_web() --------------------------------------------------------

# Shared recipe behind format_for_web() and format_for_web_batch(): the fixed
# web-delivery re-encode (H.264 + yuv420p + AAC + faststart), padding odd
# dimensions down to even as the codec requires. No per-row knobs — every input
# gets the same recipe. Command assembly stays in Layer 1 (IP1/D002).
format_for_web_pipeline <- function(input, output, hardware = "none",
                                    fallback = FALSE) {
  # The recipe stays H.264 (family fixed); hardware = "nvenc" swaps libx264 for
  # h264_nvenc when available. Layer 2 only computes the codec name (D009, IP1).
  video_codec <- resolve_hw_encoder("libx264", hardware, fallback)
  p <- ffm_files(input, output)
  p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")
  p <- ffm_codec(p, video = video_codec, audio = "aac")
  p <- ffm_pixel_format(p, "yuv420p")
  ffm_output_options(p, "-movflags +faststart")
}

#' Re-encode a video for web playback
#'
#' Re-encode a video into a widely compatible, web-friendly form (H.264 video
#' with \code{yuv420p} and \code{+faststart}, AAC audio), padding odd
#' dimensions down to even values as required by the codec.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param hardware The encoder backend: \code{"none"} (default, software
#'   libx264) or \code{"nvenc"} for NVIDIA GPU H.264 encoding
#'   (\code{"h264_nvenc"}) when available. See \code{\link{has_nvenc}}.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, re-encode with software libx264 and a message (\code{TRUE})
#'   instead of aborting (\code{FALSE}, default).
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_codec()] and [ffm_pixel_format()], among the builders it wraps;
#'   [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [standardize_video()] for a configurable re-encode;
#'   [format_for_web_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' format_for_web(video, "web.mp4", run = FALSE)
#' @export
format_for_web <- function(infile, outfile, hardware = c("none", "nvenc"),
                           fallback = FALSE, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  hardware <- rlang::arg_match(hardware)

  ffm_finish(format_for_web_pipeline(infile, outfile, hardware, fallback), run)
}


# strip_metadata() --------------------------------------------------------

# Shared recipe behind strip_metadata() and strip_metadata_batch(): a lossless
# stream copy that discards all container/global metadata and chapters, and
# muxes bit-exactly so FFmpeg does not re-stamp its own creation_time / encoder
# tag onto the output. Holding it here gives the batch sibling per-row parity
# for free (M13). Metadata scrubbing is pure command assembly, so compile stays
# binary-free (IP1/D002).
strip_metadata_pipeline <- function(input, output) {
  p <- ffm_files(input, output)
  # -c:v copy -c:a copy -map 0: carry every stream through untouched.
  p <- ffm_copy(p)
  # -map_metadata -1 drops global tags (creation_time, location/GPS, make/model,
  # title, comment); -map_chapters -1 drops chapters; -fflags +bitexact stops the
  # muxer writing a fresh creation_time and an encoder=Lavf... tag. Per-stream
  # tags (handler_name, language) and codec-embedded identifiers survive a copy.
  ffm_output_options(
    p, "-map_metadata -1", "-map_chapters -1", "-fflags +bitexact"
  )
}

#' Strip identifying metadata from a media file
#'
#' Remove a media file's container and global metadata tags (creation time,
#' GPS/location, device make and model, title, comment, and the like) together
#' with any chapters, writing a de-identified copy — the front door for
#' IRB/de-identification of research recordings. The audio and video streams are
#' **stream-copied**, not re-encoded, so the operation is lossless and fast and
#' the picture and sound are bit-for-bit unchanged (including any rotation
#' display matrix, which is stream side data, not a metadata tag).
#'
#' @details
#' The output is muxed bit-exactly (\code{-fflags +bitexact}) so FFmpeg does not
#' re-stamp the container with a fresh \code{creation_time} or an
#' \code{encoder} tag naming its own version — either of which would defeat
#' de-identification and reproducibility.
#'
#' Because the streams are copied rather than re-encoded, identifiers embedded
#' **inside** the encoded bitstream, and per-stream metadata such as
#' \code{handler_name} or \code{language}, are not removed. Removing those would
#' require re-encoding (out of scope; use the \code{\link{ffmpeg}} escape hatch)
#' or per-stream metadata mapping that must probe the file first.
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the de-identified file to
#'   write. Use the same container extension as \code{infile} so the copied
#'   streams remux cleanly.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [anonymize_video()] to remove faces or regions from the picture (the
#'   visual de-identification sibling); [probe_container()] and
#'   [mediainfo_query()] to inspect a file's metadata before and after;
#'   [ffm_copy()] and [ffm_output_options()], the builders it wraps;
#'   [strip_metadata_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' strip_metadata(video, "clean.mp4", run = FALSE)
#' @export
strip_metadata <- function(infile, outfile, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  p <- strip_metadata_pipeline(infile, outfile)
  ffm_finish(p, run)
}


# standardize_video() -----------------------------------------------------

#' Standardize a video to a reproducible format
#'
#' Re-encode a video to a consistent, reproducible format for analysis
#' pipelines: a single video codec, pixel format, and (optionally) resolution
#' and frame rate, with \code{+faststart} for smooth playback. Unlike
#' \code{\link{format_for_web}} (a fixed web-delivery recipe), every part of the
#' standard is a parameter, so a lab can pin its own house format once and apply
#' it across a dataset.
#'
#' @details
#' The default standard \code{standardize_video(infile, outfile)} re-encodes to
#' H.264 video (\code{video_codec = "libx264"}) with \code{pixel_format = "yuv420p"}
#' and \code{-movflags +faststart}, keeping the source resolution and frame
#' rate. Audio is stream-copied unchanged (\code{-c:a copy}) unless
#' \code{audio_codec} names an encoder; loudness standardization stays out of
#' scope (see \code{\link{normalize_audio}}). The same input therefore always
#' compiles to a byte-identical command.
#'
#' Resolution follows \code{width}/\code{height}: supplying both forces exact
#' output dimensions; supplying only one preserves the aspect ratio and rounds
#' the other to the nearest even number (FFmpeg's \code{-2}); supplying neither
#' keeps the source resolution but rounds odd dimensions down to the nearest
#' even value (a \code{yuv420p}/\code{libx264} requirement, and a no-op for
#' already-even input) so the output always encodes.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param width The output width in pixels (a positive number), or \code{NULL}
#'   (default) to leave the width unconstrained.
#' @param height The output height in pixels (a positive number), or \code{NULL}
#'   (default) to leave the height unconstrained.
#' @param fps The output frame rate (a positive number or FFmpeg framerate
#'   expression such as \code{"30000/1001"}), or \code{NULL} (default) to keep
#'   the input frame rate.
#' @param video_codec A string naming the output video codec (default
#'   \code{"libx264"}), or \code{NULL} to emit no \code{-codec:v} and let the
#'   output container's default encoder decide. \code{NULL} is how you opt out
#'   of the H.264 default for a container that does not hold it — for a
#'   \code{.webm} output, pass \code{video_codec = NULL} \emph{and}
#'   \code{audio_codec = NULL}, since the default \code{audio_codec = "copy"}
#'   would otherwise carry a codec WebM cannot hold.
#' @param audio_codec A string naming the output audio codec (default
#'   \code{"copy"}, i.e. stream-copy the source audio unchanged). Name a real
#'   encoder (e.g. \code{"aac"}) when the source audio codec cannot be copied
#'   into the output container, or \code{NULL} to emit no \code{-codec:a} and
#'   let the container's default encoder decide.
#' @param pixel_format A string naming the output pixel format (default
#'   \code{"yuv420p"}).
#' @param hardware The encoder backend: \code{"none"} (default, the software
#'   \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU encoding. When
#'   \code{"nvenc"}, the nvenc encoder for \code{video_codec}'s family is used
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}); see
#'   \code{\link{has_nvenc}} for availability and its caveats. Applies to video
#'   only: \code{audio_codec} is never hardware-accelerated.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, re-encode with the software \code{video_codec} and a message
#'   (\code{TRUE}) instead of aborting (\code{FALSE}, default). Keeps output
#'   reproducible by never changing the codec silently.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_scale()], [ffm_codec()], and [ffm_pixel_format()], among the
#'   builders it wraps; [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [standardize_video_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # The documented default standard (H.264 / yuv420p / +faststart)
#' standardize_video(video, "std.mp4", run = FALSE)
#' # Pin resolution and frame rate too
#' standardize_video(video, "std.mp4", width = 1280, height = 720, fps = 30,
#'                   run = FALSE)
#' @export
standardize_video <- function(infile, outfile,
                              width = NULL, height = NULL, fps = NULL,
                              video_codec = "libx264", audio_codec = "copy",
                              pixel_format = "yuv420p",
                              hardware = c("none", "nvenc"), fallback = FALSE,
                              run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  hardware <- rlang::arg_match(hardware)
  # Without this, a non-string video_codec reached ffm_codec() and aborted naming
  # Layer-1's `video` instead of the argument the caller actually passed (M41).
  # allow_null keeps NULL compiling as it does today: no -codec:v emitted.
  # audio_codec is already checked inside standardize_pipeline(), which blames
  # this verb because the pipeline is called from here.
  rlang::check_string(video_codec, allow_null = TRUE)

  ffm_finish(
    standardize_pipeline(infile, outfile, width, height, fps, video_codec,
                         audio_codec, pixel_format, hardware, fallback),
    run
  )
}


# standardize_pipeline() --------------------------------------------------

# Shared standardization pipeline for standardize_video() and
# standardize_video_batch(): build one single-output re-encode pipeline for a single
# input. Both verbs compile identical commands from this helper, so per-value
# validation (dimensions via check_dim, codec/pixfmt via check_token) and M12's
# guards (audio stream-copy, even-dimension safeguard, +faststart) live here
# once -- the batch sibling inherits them by construction (D002, D003, D007).
standardize_pipeline <- function(input, output, width, height, fps, video_codec,
                                 audio_codec = "copy", pixel_format,
                                 hardware = "none", fallback = FALSE,
                                 call = rlang::caller_env()) {
  video_codec <- resolve_hw_encoder(video_codec, hardware, fallback)
  p <- ffm_files(input, output)
  # Resolution: exact when both given; aspect-preserving with an even output
  # dimension (FFmpeg's -2) when only one. ffm_scale() validates each dimension
  # via check_dim(). When neither is given, still force even dimensions so
  # yuv420p/libx264 can encode odd-dimensioned sources -- floor-to-even is a
  # no-op for already-even input, mirroring format_for_web()'s guard.
  if (!is.null(width) || !is.null(height)) {
    p <- ffm_scale(
      p,
      width = if (is.null(width)) "-2" else width,
      height = if (is.null(height)) "-2" else height
    )
  } else {
    p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")
  }
  if (!is.null(fps)) {
    p <- ffm_fps(p, fps)
  }
  # Audio defaults to a stream copy, not a re-encode: standardization is
  # video-only, so "leave audio untouched" means copy the bytes (matching
  # extract_audio()). `audio_codec` is the escape hatch for the D017 trap --
  # copying a source codec the output container cannot hold -- and reuses M35's
  # apply_audio_codec() seam, so NULL emits no -codec:a (M39/D017). `call` is
  # threaded so a bad token names standardize_video(), not this internal helper
  # (parity with anonymize_pipeline(); M39 review F2).
  p <- ffm_codec(p, video = video_codec)
  p <- apply_audio_codec(p, audio_codec, call = call)
  p <- ffm_pixel_format(p, pixel_format)
  ffm_output_options(p, "-movflags +faststart")
}


# anonymize_video() -------------------------------------------------------

#' Cover fixed regions of a video with opaque boxes
#'
#' Anonymize a video by covering one or more fixed rectangular regions with
#' opaque filled boxes -- for example, to redact a face, a name badge, or a
#' screen that stays in one place for the whole clip. The regions are fixed
#' (there is no face or object tracking), so this suits footage where the areas
#' to cover do not move.
#'
#' @details
#' \code{regions} is a data frame with one row per box and the columns
#' \code{x}, \code{y}, \code{width}, and \code{height} (each a pixel number or an
#' FFmpeg expression such as \code{"in_w/2"}); \code{x}/\code{y} give the
#' top-left corner and \code{width}/\code{height} the size. An optional
#' \code{color} column overrides the \code{color} argument for that row. Every
#' box is a solid fill (FFmpeg's \code{drawbox} with \code{t=fill}); hollow
#' outlines are intentionally not offered.
#'
#' Because a filter is applied, the video is re-encoded (\code{video_codec} /
#' \code{pixel_format}, defaulting to H.264 / \code{yuv420p}); odd source
#' dimensions are floored to even so the output always encodes (a
#' \code{yuv420p}/\code{libx264} requirement, and a no-op for already-even
#' input). Audio is stream-copied unchanged (\code{-c:a copy}) unless
#' \code{audio_codec} names an encoder. The same input and regions therefore
#' always compile to a byte-identical command.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param regions A data frame with one row per box and columns \code{x},
#'   \code{y}, \code{width}, \code{height} (and optionally \code{color}); see
#'   Details.
#' @param color A string naming the default fill color in FFmpeg color syntax,
#'   used for any row without its own \code{color} (default \code{"black"}).
#' @param video_codec A string naming the output video codec (default
#'   \code{"libx264"}), or \code{NULL} to emit no \code{-codec:v} and let the
#'   output container's default encoder decide. \code{NULL} is how you opt out
#'   of the H.264 default for a container that does not hold it — for a
#'   \code{.webm} output, pass \code{video_codec = NULL} \emph{and}
#'   \code{audio_codec = NULL}, since the default \code{audio_codec = "copy"}
#'   would otherwise carry a codec WebM cannot hold.
#' @param audio_codec A string naming the output audio codec (default
#'   \code{"copy"}, i.e. stream-copy the source audio unchanged). Name a real
#'   encoder (e.g. \code{"aac"}) when the source audio codec cannot be copied
#'   into the output container, or \code{NULL} to emit no \code{-codec:a} and
#'   let the container's default encoder decide.
#' @param pixel_format A string naming the output pixel format (default
#'   \code{"yuv420p"}).
#' @param hardware The encoder backend: \code{"none"} (default, the software
#'   \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU encoding. When
#'   \code{"nvenc"}, the nvenc encoder for \code{video_codec}'s family is used
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}); see
#'   \code{\link{has_nvenc}} for availability and its caveats. Applies to video
#'   only: \code{audio_codec} is never hardware-accelerated.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, re-encode with the software \code{video_codec} and a message
#'   (\code{TRUE}) instead of aborting (\code{FALSE}, default). Keeps output
#'   reproducible by never changing the codec silently.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_drawbox()], the builder filter it wraps; [has_nvenc()] for the
#'   \code{hardware = "nvenc"} toggle; [anonymize_video_batch()]
#'   for the many-file (batch) form.
#' @references https://ffmpeg.org/ffmpeg-filters.html#drawbox
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Cover two fixed regions with black boxes
#' regions <- data.frame(
#'   x = c(10, 200), y = c(10, 150),
#'   width = c(120, 80), height = c(90, 60)
#' )
#' anonymize_video(video, "anon.mp4", regions, run = FALSE)
#' @export
anonymize_video <- function(infile, outfile, regions,
                            color = "black",
                            video_codec = "libx264", audio_codec = "copy",
                            pixel_format = "yuv420p",
                            hardware = c("none", "nvenc"), fallback = FALSE,
                            run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  hardware <- rlang::arg_match(hardware)

  ffm_finish(
    anonymize_pipeline(infile, outfile, regions, color, video_codec,
                       audio_codec, pixel_format, hardware, fallback),
    run
  )
}


# anonymize_pipeline() ----------------------------------------------------

# Shared anonymization pipeline for anonymize_video() and (M21) the batch
# sibling: build one single-output box-fill pipeline for a single input. Both
# verbs compile identical commands from this helper, so region-table validation
# and the encode guards (even-dimension safeguard, audio stream-copy) live here
# once -- the batch sibling inherits them by construction (D002, D003, D007;
# M13 extract-first lesson).
anonymize_pipeline <- function(input, output, regions, color, video_codec,
                               audio_codec = "copy", pixel_format,
                               hardware = "none", fallback = FALSE,
                               call = rlang::caller_env()) {
  check_regions(regions, call = call)
  rlang::check_string(color, call = call)
  # NULL is D016's "leave the codec alone" sentinel, and D022 makes it the
  # family-wide spelling of "unset": emit no -codec:v and let the output
  # container's default encoder decide. Refusing it here is the whole reason
  # anonymize_video() aborted on a call standardize_video() has always compiled
  # (measured at M42 T1).
  #
  # `allow_null = TRUE` rather than `if (!is.null(video_codec)) check_token(...)`:
  # the two accept identical values, but only this spelling makes the refusal
  # message say "must be a single string or `NULL`". The other one keeps
  # advertising NULL as illegal on the argument where it is now the documented
  # escape hatch (M42 review F1).
  #
  # The check stays at this position rather than moving into apply_video_codec()
  # below so it keeps reporting before pixel_format and the drawbox dimensions,
  # exactly as it did (the precedence M41's review twice caught moving).
  check_token(video_codec, allow_null = TRUE, call = call)
  check_token(pixel_format, call = call)

  # Integer coordinates are natural pixel values, but ffm_drawbox()'s check_dim()
  # accepts only doubles or expression strings; coerce numeric columns so an
  # integer/integerish table is not rejected.
  for (col in c("x", "y", "width", "height")) {
    if (is.numeric(regions[[col]])) regions[[col]] <- as.double(regions[[col]])
  }

  p <- ffm_files(input, output)
  # Force even output dimensions so yuv420p/libx264 can encode odd-dimensioned
  # sources (M12 guard); a no-op for already-even input. drawbox coordinates use
  # a top-left origin, so the <=1px floor never shifts a region's x/y.
  p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")

  colors <- if ("color" %in% names(regions)) {
    regions$color
  } else {
    rep(color, nrow(regions))
  }
  # One filled drawbox per region; ffm_drawbox() validates each x/y/w/h.
  for (i in seq_len(nrow(regions))) {
    p <- ffm_drawbox(
      p,
      x = regions$x[i], y = regions$y[i],
      width = regions$width[i], height = regions$height[i],
      color = colors[i], thickness = "fill"
    )
  }
  # Re-encode video (a filter is applied); audio defaults to a stream copy --
  # the same encode profile as standardize_video(). `audio_codec` is the escape
  # hatch for the D017 trap (a source codec the output container cannot hold),
  # and reuses M35's apply_audio_codec() seam, so NULL emits no -codec:a
  # (M39/D017).
  # hardware = "nvenc" swaps the software video_codec for its nvenc encoder;
  # Layer 2 computes the name here, Layer 1 assembles it unchanged (IP1; D-M31).
  video_codec <- resolve_hw_encoder(video_codec, hardware, fallback, call = call)
  p <- ffm_codec(p, video = video_codec)
  p <- apply_audio_codec(p, audio_codec, call = call)
  ffm_pixel_format(p, pixel_format)
}


# check_regions() ---------------------------------------------------------

# Validate the `regions` data frame for anonymize_video()/its batch sibling:
# structure and column type/NA only. Per-value dimension checks (positive size,
# valid expression) are inherited per row from ffm_drawbox()'s check_dim().
check_regions <- function(regions, call = rlang::caller_env()) {
  if (!is.data.frame(regions)) {
    cli::cli_abort("{.arg regions} must be a data frame with one row per box.",
                   call = call)
  }
  if (nrow(regions) == 0) {
    cli::cli_abort("{.arg regions} must have at least one row.", call = call)
  }
  required <- c("x", "y", "width", "height")
  missing <- setdiff(required, names(regions))
  if (length(missing)) {
    cli::cli_abort(
      c(
        "{.arg regions} is missing {length(missing)} required column{?s}: {.field {missing}}.",
        "i" = "Each row needs {.field x}, {.field y}, {.field width}, and {.field height}."
      ),
      call = call
    )
  }
  for (col in required) {
    v <- regions[[col]]
    if (!(is.numeric(v) || is.character(v))) {
      cli::cli_abort(
        "The {.field {col}} column of {.arg regions} must be numeric or character.",
        call = call
      )
    }
    if (anyNA(v)) {
      cli::cli_abort(
        "The {.field {col}} column of {.arg regions} must not contain {.val {NA}}.",
        call = call
      )
    }
  }
  if ("color" %in% names(regions)) {
    v <- regions$color
    if (!is.character(v) || anyNA(v)) {
      cli::cli_abort(
        "The {.field color} column of {.arg regions} must be character (no {.val {NA}}).",
        call = call
      )
    }
  }
  invisible(regions)
}


# derive_anonymized_names() -----------------------------------------------

# Derive one output path per input for anonymize_video_batch() when the `output`
# column is absent: `<base>_anonymized.<input-ext>` (box-fill re-encodes but
# keeps the source container). One input -> one output, so a duplicated input
# with no explicit `output` would collide; the caller (anonymize_video_batch)
# rejects that up front, so this helper assumes unique inputs and stays a pure
# name map (parity with derive_standardized_names()).
derive_anonymized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_anonymized.", tools::file_ext(input)
  )
}


# anonymize_video_batch() ------------------------------------------------------

#' Anonymize Many Videos From a Jobs Table
#'
#' Cover fixed rectangular regions of many input videos with opaque filled boxes
#' from a single jobs tibble — the **batch** (table-driven) sibling of
#' [anonymize_video()] for when you have more than one video to
#' redact. Each row is one input with its own regions; the required columns name
#' the source (\code{input}) and the boxes to cover (\code{regions}). This is a
#' thin wrapper over \code{\link{ffm_batch}}: one reproducible compiled command
#' per input, sharing the same box-fill pipeline (and per-region validation) as
#' the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) and a \code{regions} list-column. Each
#'   \code{regions} cell is itself a data frame of boxes for that input — the
#'   same \code{x}/\code{y}/\code{width}/\code{height} (and optional per-box
#'   \code{color}) shape \code{\link{anonymize_video}} takes. An optional
#'   \code{output} column names the destination; when absent, one is derived per
#'   row by appending \code{_anonymized} to each input's basename, keeping the
#'   input's extension (e.g. \code{clip.mkv} becomes \code{clip_anonymized.mkv}).
#'   Because anonymization is one-input-to-one-output, a duplicated \code{input}
#'   with no \code{output} column would collide and is rejected. Each of the
#'   four encode knobs — \code{color}, \code{video_codec}, \code{audio_codec},
#'   \code{pixel_format} — may
#'   also appear as a column to override the corresponding argument on a per-row
#'   basis; rows (or knobs) that omit the column fall back to the argument's
#'   value. In either codec column, \code{NA} leaves that row's codec unset (the
#'   column form of \code{video_codec = NULL} / \code{audio_codec = NULL}); in a
#'   \code{color} or \code{pixel_format} column it is an error, because those
#'   have no unset state. Any other columns are ignored.
#' @param color A string naming the default fill color (FFmpeg color syntax)
#'   applied to every row, unless \code{jobs} carries a \code{color} column or a
#'   box supplies its own \code{color}. (default = \code{"black"})
#' @param video_codec A string naming the output video codec applied to every
#'   row, unless \code{jobs} carries a \code{video_codec} column, in which case
#'   \code{NA} in a cell leaves that row's codec unset. Default
#'   \code{"libx264"}; \code{NULL} emits no \code{-codec:v} and lets the output
#'   container's default encoder decide (for a \code{.webm} output, pass
#'   \code{audio_codec = NULL} too — the default \code{"copy"} would otherwise
#'   carry a codec WebM cannot hold).
#' @param audio_codec A string naming the output audio codec applied to every
#'   row, unless \code{jobs} carries an \code{audio_codec} column, in which case
#'   \code{NA} in a cell leaves that row's codec unset. \code{"copy"} (default)
#'   stream-copies the audio through untouched; name an encoder (e.g.
#'   \code{"aac"}) when the source audio cannot be copied into the output
#'   container.
#' @param pixel_format A string naming the output pixel format applied to every
#'   row, unless \code{jobs} carries a \code{pixel_format} column.
#'   (default = \code{"yuv420p"})
#' @param hardware The encoder backend applied to every row: \code{"none"}
#'   (default, the software \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU
#'   encoding. Batch-wide (a machine property), not a per-row column; a
#'   \code{hardware} column in \code{jobs} is ignored. See \code{\link{has_nvenc}}.
#' @param fallback A logical applied to every row: when \code{hardware = "nvenc"}
#'   but nvenc is unavailable, re-encode with the software \code{video_codec} and
#'   a message (\code{TRUE}) instead of aborting (\code{FALSE}, default).
#'   Batch-wide, not a per-row column.
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: anonymize in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one input at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso [anonymize_video()] for the single-input form; [has_nvenc()] for the
#'   \code{hardware = "nvenc"} toggle; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [standardize_video_batch()] and [segment_video_batch()] for the other
#'   table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input   = c(video, video),
#'   output  = c("a.mp4", "b.mp4"),
#'   regions = list(
#'     data.frame(x = 10, y = 10, width = 120, height = 90),
#'     data.frame(x = 200, y = 150, width = 80, height = 60)
#'   )
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' anonymize_video_batch(jobs, run = FALSE)
#' @export
anonymize_video_batch <- function(jobs, color = "black", video_codec = "libx264",
                             audio_codec = "copy", pixel_format = "yuv420p",
                             hardware = c("none", "nvenc"), fallback = FALSE,
                             run = TRUE, parallel = FALSE, ...) {

  hardware <- rlang::arg_match(hardware)
  # NULL is legal (the "emit no -codec:a" escape hatch), so allow_null (M39).
  rlang::check_string(audio_codec, allow_null = TRUE)

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }
  if (!"regions" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have a {.field regions} column.",
      "x" = "Missing column: {.val regions}.",
      "i" = "Make it a list-column: one boxes data frame per input row."
    ))
  }
  # The regions column is a list-column (one boxes data frame per row); a flat
  # column can't hold per-row tables, so reject it here with a clear message
  # rather than as an opaque per-row abort. Each cell's structure is validated
  # per row by check_regions() inside anonymize_pipeline() (inherited, reported
  # by row index via purrr; M13 extract-first lesson).
  if (!is.list(jobs$regions) || is.data.frame(jobs$regions)) {
    cli::cli_abort(c(
      "The {.field regions} column of {.arg jobs} must be a list-column.",
      "i" = "Each element is a boxes data frame, one per input row."
    ))
  }


  # A factor input column carries paths as levels; treat them as strings
  # (parity with standardize_video_batch()).
  jobs$input <- as.character(jobs$input)

  # Validate present override columns up front so a bad column fails clearly
  # here rather than as an opaque FFmpeg error mid-batch (M11 parity lesson).
  # Value-level checks (valid color/codec/pixfmt tokens) are inherited per row
  # from anonymize_pipeline()'s guards.
  str_cols <- c("color", "pixel_format")
  for (col in intersect(str_cols, names(jobs))) {
    if (!is.character(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be character (no {.val {NA}}).")
    }
  }
  # Both codec columns take the codec guard, which admits NA and the all-NA
  # logical column R hands back (M34 lesson). `video_codec` sat in str_cols
  # above until M42, justified by a comment calling it "a literal libx264
  # default with no sentinel" -- false when written, since the argument accepts
  # NULL, and now the family rule: NA is the column form of that NULL (D022).
  # `color` and `pixel_format` stay in str_cols: not codec arguments, no
  # sentinel, so an NA cell there spells nothing.
  #
  # Checking video_codec here rather than in the loop's slot moves it after
  # `color` and `pixel_format` in the reporting order for a jobs table with two
  # bad columns. Named because M41's review twice caught a guard reassigning
  # precedence unremarked; the two codec columns now report together.
  check_batch_codec_col(jobs, "video_codec")
  check_batch_codec_col(jobs, "audio_codec")

  # Auto-name outputs when the column is absent. One input -> one output, so a
  # duplicated input with no explicit output would map to the same file; reject
  # that rather than silently overwrite (parity with standardize_video_batch()).
  if (!"output" %in% names(jobs)) {
    dupes <- unique(jobs$input[duplicated(jobs$input)])
    if (length(dupes) > 0) {
      cli::cli_abort(c(
        "{.arg jobs} has duplicated {.field input} paths but no {.field output} column.",
        "x" = "Duplicated input{?s}: {.val {dupes}}.",
        "i" = "Add an {.field output} column to name each row's destination."
      ))
    }
    jobs$output <- derive_anonymized_names(jobs$input)
  }

  # video_codec had no front-door check, so a non-string reached the per-row
  # pipeline and aborted inside purrr::pmap(), carrying `In index: <n>` and
  # blaming pmap rather than this verb (M41).
  #
  # M42 answered the question M41 left here: `NULL` IS legal, the family-wide
  # sentinel for "emit no -codec:v" (D022). So this takes allow_null = TRUE,
  # not the `if (!is.null(...))` shape M41 chose to avoid advertising NULL --
  # that shape accepts the same values but keeps saying NULL is illegal, which
  # is now false (M42 review F1). separate_audio_video_batch() still carries the
  # older spelling; its arguments' NULL semantics are D020's, not this
  # milestone's, so it is left alone rather than swept.
  #
  # Placed at the END of this verb's front-door validation, not beside the
  # other scalar checks: before M41 this argument was only read per row
  # inside the fan-out, so EVERY check above it reported first on a call
  # that was wrong about two things at once. Moving the guard up the
  # function silently reassigned that precedence -- first past the jobs
  # SHAPE block (review A6), then past its content checks too (review
  # A1r3). Here it changes nothing but the message a bad codec gets.
  rlang::check_string(video_codec, allow_null = TRUE)

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output box-fill
  # pipeline per row, sharing anonymize_pipeline() with anonymize_video(). A
  # per-row knob column (arriving via `...` from pmap) overrides the scalar arg
  # of the same name; `...` also forwards ffm_batch options (verify/manifest/...)
  # to the runner, never to the pipeline builder. The `regions` list-column
  # arrives unwrapped per row (pmap passes each cell's data frame by name).
  ffm_batch(
    jobs,
    function(input, output, regions, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      anonymize_pipeline(
        input, output, regions,
        color = pick("color", color),
        video_codec = batch_codec_cell(pick("video_codec", video_codec)),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        pixel_format = pick("pixel_format", pixel_format),
        # hardware/fallback are batch-wide (a machine property), never per-row
        # columns -- parity with standardize_video_batch (D-M31).
        hardware = hardware,
        fallback = fallback
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# normalize_audio() -------------------------------------------------------

#' Normalize a file's audio loudness (EBU R128)
#'
#' Normalize the perceived loudness of a file's audio toward an EBU R128 target
#' using FFmpeg's single-pass \code{loudnorm} filter, optionally downmixing the
#' channel count and resampling. The video stream is copied unchanged
#' (\code{-c:v copy}), so only the audio is touched -- the audio-side complement
#' to \code{\link{standardize_video}}, which leaves audio alone.
#'
#' @details
#' The default targets follow EBU Recommendation R 128 (2014) --
#' \code{target_loudness = -23} LUFS and \code{true_peak = -1} dBTP, loudness
#' measured per ITU-R BS.1770-4 -- with \code{loudness_range = 7}. This is
#' single-pass (dynamic) \code{loudnorm}: the same input and arguments always
#' compile to one reproducible command, with no separate measurement pass.
#' Because the audio is filtered it is re-encoded; set \code{audio_codec} to
#' name the output encoder, or leave it \code{NULL} to use the output
#' container's default. Leaving \code{channels} at \code{NULL} preserves the
#' source channel layout. Note that FFmpeg's \code{loudnorm} filter resamples its output (up to
#' 192 kHz, capped by the encoder), so the output sample rate is \emph{not} the
#' source rate unless you pin it: set \code{sample_rate} to control the output
#' rate.
#'
#' @param infile A string containing the path to a media file (with audio).
#' @param outfile A string containing the path of the file to write.
#' @param target_loudness The target integrated loudness, in LUFS (a number in
#'   \code{-70}..\code{-5}; default \code{-23}, the EBU R128 target).
#' @param true_peak The maximum true peak, in dBTP (a number in \code{-9}..\code{0};
#'   default \code{-1}, the EBU R128 ceiling).
#' @param loudness_range The target loudness range, in LU (a number in
#'   \code{1}..\code{50}; default \code{7}).
#' @param channels The output channel count, e.g. \code{1} to downmix to mono (a
#'   positive whole number), or \code{NULL} (default) to keep the source layout.
#' @param sample_rate The output sample rate in Hz, e.g. \code{48000} (a positive
#'   whole number), or \code{NULL} (default) to let \code{loudnorm} choose (it
#'   resamples, up to 192 kHz encoder-capped -- not the source rate). Set this to
#'   pin the output rate.
#' @param audio_codec An optional string naming the output audio encoder (e.g.
#'   \code{"aac"}, \code{"libmp3lame"}, \code{"flac"}), passed to FFmpeg's
#'   \code{-codec:a}. \code{NULL} (default) emits no \code{-codec:a}, leaving
#'   the output container's default encoder in place. \code{"copy"} is an error:
#'   loudness normalization filters the audio, so the stream must be re-encoded
#'   and cannot be copied.
#' @param two_pass A logical: when \code{TRUE}, use accurate two-pass
#'   (measured/linear) normalization instead of the default single-pass
#'   (\code{FALSE}). A first \emph{analysis pass} measures the input's loudness,
#'   and a second \emph{correction pass} feeds those measurements back with
#'   \code{linear=true} so the output hits the EBU R128 target precisely.
#'   Two-pass therefore \strong{always runs the analysis pass through FFmpeg}
#'   (it needs the binary and readable input), even when \code{run = FALSE}: in
#'   that case the analysis still runs and the returned value is the exact
#'   correction command, left unexecuted. The single-pass default touches no
#'   binary under \code{run = FALSE}. If the input is \strong{silent}, the
#'   analysis pass measures its loudness as \code{-inf}; normalizing silence to
#'   a target is undefined, so two-pass aborts with a clear error (the
#'   single-pass default leaves silence untouched).
#' @param run A logical: run the (correction) command through FFmpeg
#'   (\code{TRUE}, default) or return the compiled command without running it
#'   (\code{FALSE}). Under \code{two_pass = TRUE} this gates only the correction
#'   pass; the analysis pass runs regardless (see \code{two_pass}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}). Under
#'   \code{two_pass = TRUE} this is the correction command built from the
#'   measured values.
#' @seealso [ffm_loudnorm()], the builder it wraps; [normalize_audio_batch()]
#'   for the many-file form; [standardize_video()], its video-side complement.
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' normalize_audio(video, "normalized.mp4", run = FALSE)
#' # Normalize to a streaming target and downmix to mono
#' normalize_audio(video, "mono.mp4", target_loudness = -16, channels = 1,
#'                 run = FALSE)
#' # Name the output audio encoder instead of taking the container's default
#' normalize_audio(video, "aac.mp4", audio_codec = "aac", run = FALSE)
#' @export
normalize_audio <- function(infile, outfile,
                            target_loudness = -23,
                            true_peak = -1,
                            loudness_range = 7,
                            channels = NULL,
                            sample_rate = NULL,
                            audio_codec = NULL,
                            two_pass = FALSE,
                            run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  rlang::check_bool(two_pass)

  # Two-pass: measure the input first, then build a linear correction from the
  # measurements. Validate the shaping knobs up front so a bad channels/
  # sample_rate fails before the analysis pass runs, not after wasting it
  # (targets are validated when the analysis pipeline builds). Single-pass keeps
  # its pure, binary-free run = FALSE compile.
  measured <- NULL
  if (two_pass) {
    rlang::check_number_whole(channels, min = 1, allow_null = TRUE)
    rlang::check_number_whole(sample_rate, min = 1, allow_null = TRUE)
    check_audio_codec_not_copy(audio_codec)
    # Token-check here too, not only inside apply_audio_codec(): the whole point
    # of hoisting is to fail before the analysis pass runs, and a malformed
    # encoder name is as fatal as "copy".
    if (!is.null(audio_codec)) check_token(audio_codec)
    measured <- run_loudnorm_analysis(infile, target_loudness, true_peak,
                                      loudness_range)
  }

  # Duplicates the check apply_audio_codec() makes inside
  # normalize_audio_pipeline(); that pipeline is shared with the batch sibling
  # for per-row validation, so threading a `call` through it would rewrite the
  # batch verb's per-row messages instead (M41). allow_null because NULL is
  # this verb's documented sentinel (D019).
  #
  # Below the two_pass block on purpose: that block already type-checks this
  # argument via check_token(), and validates channels/sample_rate. Guarding
  # above it changed which complaint a two-pass call wrong about both gets
  # (review A3r3). Here the two-pass path is exactly as it was, and the
  # default path -- the one M41 is about -- is still fixed.
  rlang::check_string(audio_codec, allow_null = TRUE)

  ffm_finish(
    normalize_audio_pipeline(infile, outfile, target_loudness, true_peak,
                             loudness_range, channels, sample_rate,
                             audio_codec = audio_codec, measured = measured),
    run
  )
}

# check_audio_codec_not_copy(): refuse audio_codec = "copy" on the loudness
# verbs (M36). These filter the audio, so a stream copy is impossible and
# D017's "copy" default deliberately does not transfer. Layer 1 already refuses
# a filtered copied stream (ffm_groups(), M02 D-M02-5) and stays the
# enforcement point (IP1); this front door names the argument the caller
# actually passed, and -- called before run_loudnorm_analysis() -- fails before
# the two-pass path burns an analysis pass. One helper, three call sites
# (pipeline, scalar two-pass pre-check, batch column guard).
check_audio_codec_not_copy <- function(audio_codec, call = rlang::caller_env()) {
  if (any(audio_codec == "copy", na.rm = TRUE)) {
    cli::cli_abort(c(
      "{.arg audio_codec} can't be {.val copy}.",
      "x" = "Loudness normalization filters the audio, so it must be re-encoded.",
      "i" = "Name an encoder (e.g. {.val aac}), or use {.code NULL} to leave the
             encoder unset."
    ), call = call)
  }
  invisible(audio_codec)
}


# normalize_audio_pipeline() ----------------------------------------------

# Shared loudness-normalization pipeline for normalize_audio() and (M15)
# normalize_audio_batch(): build one single-output pipeline for a single input. Both
# verbs compile identical commands from this helper, so per-value validation
# (loudness targets via ffm_loudnorm(), channels/sample_rate here) lives once --
# the batch sibling inherits it by construction (D002, D007; M13 lesson).
normalize_audio_pipeline <- function(input, output,
                                     target_loudness = -23,
                                     true_peak = -1,
                                     loudness_range = 7,
                                     channels = NULL,
                                     sample_rate = NULL,
                                     audio_codec = NULL,
                                     measured = NULL) {
  rlang::check_number_whole(channels, min = 1, allow_null = TRUE)
  rlang::check_number_whole(sample_rate, min = 1, allow_null = TRUE)
  check_audio_codec_not_copy(audio_codec)

  p <- ffm_files(input, output)
  # Loudness: EBU R128 loudnorm; ffm_loudnorm() validates the target ranges. With
  # `measured` (the two-pass correction path), feed the analysis-pass values back
  # and switch to linear normalization so the target is hit precisely (M16).
  if (is.null(measured)) {
    p <- ffm_loudnorm(p, target_loudness = target_loudness,
                      true_peak = true_peak, loudness_range = loudness_range)
  } else {
    p <- ffm_loudnorm(p, target_loudness = target_loudness,
                      true_peak = true_peak, loudness_range = loudness_range,
                      measured_i = measured$i, measured_tp = measured$tp,
                      measured_lra = measured$lra, measured_thresh = measured$thresh,
                      offset = measured$offset, linear = TRUE)
  }
  # Touch audio only: stream-copy the video bytes unchanged (the inverse of
  # standardize_video()'s audio copy).
  p <- ffm_codec(p, video = "copy")
  # Name the audio encoder, if asked. NULL emits no -codec:a, leaving the
  # output container's default encoder in place -- the pre-M36 behavior.
  p <- apply_audio_codec(p, audio_codec)
  if (!is.null(channels)) {
    p <- ffm_output_options(p, paste0("-ac ", channels))
  }
  if (!is.null(sample_rate)) {
    p <- ffm_output_options(p, paste0("-ar ", sample_rate))
  }
  p
}


# ffmpeg_codecs() ------------------------------------------------------------

#' Get a data frame of all installed codecs
#'
#' Query a list of installed codecs from FFmpeg and construct a tidy data frame
#' containing information about these codecs.
#'
#' @param sort_by_type A logical indicating whether the tibble should be sorted
#'   by type and then by name (\code{TRUE}) or just by name (\code{FALSE}).
#'   (default = \code{TRUE})
#' @return A [tibble][tibble::tibble-package] with the following variables:
#'   \item{name}{A character vector including the name/code of each codec}
#'   \item{details}{A character vector including details about each codec}
#'   \item{type}{A factor vector indicating whether each codec supports
#'   \code{"Video"}, \code{"Audio"} or \code{"Subtitles"}} \item{decoding}{A
#'   logical vector indicating whether each codec supports decoding}
#'   \item{encoding}{A logical vector indicating whether each codec supports
#'   encoding} \item{intraframe}{A logical vector indicating whether each codec
#'   is an intra-frame-only codec} \item{lossy}{A logical vector indicating
#'   whether each codec supports lossy compression} \item{lossless}{A logical
#'   vector indicating whether each codec supports lossless compression}
#' @seealso [ffmpeg_encoders()] for the encoder list, [ffm_codec()] to set a
#'   codec in a pipeline, and [ffmpeg()] for the Layer 0 escape hatch.
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' head(ffmpeg_codecs())
#' ffmpeg_codecs(sort_by_type = FALSE)
#' @export
ffmpeg_codecs <- function(sort_by_type = TRUE) {
  output <- ffmpeg("-codecs")
  output2 <- output[-(1:which(output == " -------"))]
  key <- regmatches(
    output2, 
    regexpr("(?<=\\s)\\S+(?=\\s)", output2, perl = TRUE)
  )
  
  
  decoding <- substr(key, 1, 1) == "D"
  encoding <- substr(key, 2, 2) == "E"
  type <- substr(key, 3, 3)
  intraframe <- substr(key, 4, 4) == "I"
  lossy <- substr(key, 5, 5) == "L"
  lossless <- substr(key, 6, 6) == "S"
  
  abbrev <- regmatches(
    output2,
    regexpr("(?<=\\s\\S{6}\\s)\\S+(?=\\s+)", output2, perl = TRUE)
  )
  
  description <- regmatches(
    output2,
    regexpr("(?<=\\s{2})\\S[[:print:]]+$", output2, perl = TRUE)
  )
  
  out <- 
    tibble::tibble(
      name = abbrev,
      details = description,
      type = factor(
        type,
        levels = c("V", "A", "S"),
        labels = c("Video", "Audio", "Subtitles")
      ),
      decoding,
      encoding,
      intraframe,
      lossy,
      lossless
    )
  
  # Sort as requested
  if (sort_by_type) {
    out <- out[order(out$type, out$name), ]
  } else {
    out <- out[order(out$name), ]
  }
  
  out
}

# ffmpeg_encoders() ------------------------------------------------------------

#' Get a data frame of all installed encoders
#'
#' Query a list of installed encoders from FFmpeg and construct a tidy data
#' frame containing information about these encoders.
#'
#' @param sort_by_type A logical indicating whether the tibble should be sorted
#'   by type and then by name (\code{TRUE}) or just by name (\code{FALSE}).
#'   (default = \code{TRUE})
#' @return A [tibble][tibble::tibble-package] with the following variables:
#'   \item{name}{A character vector including the name/code of each encoder}
#'   \item{details}{A character vector including details about each encoder}
#'   \item{type}{A factor vector indicating whether each encoder supports
#'   \code{"Video"}, \code{"Audio"} or \code{"Subtitles"}} \item{frame_mt}{A
#'   logical vector indicating whether each encoder supports frame-level
#'   multithreading} \item{slice_mt}{A logical vector indicating whether each
#'   encoder supports slice-level multithreading} \item{experimental}{A logical
#'   vector indicating whether each encoder is experimental} \item{horiz_band}{A
#'   logical vector indicating whether each encoder supports draw_horiz_band}
#'   \item{direct_render}{A logical vector indicating whether each encoders
#'   supports direct rending method 1}
#' @seealso [ffmpeg_codecs()] for the codec list, [ffm_codec()] to set a codec
#'   in a pipeline, and [ffmpeg()] for the Layer 0 escape hatch.
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' head(ffmpeg_encoders())
#' ffmpeg_encoders(sort_by_type = FALSE)
#' @export
ffmpeg_encoders <- function(sort_by_type = TRUE) {

  rlang::check_bool(sort_by_type)

  output <- ffmpeg("-encoders")
  output2 <- output[-(1:which(output == " ------"))]
  key <- regmatches(
    output2, 
    regexpr("(?<=\\s)\\S+(?=\\s)", output2, perl = TRUE)
  )
  
  type <- substr(key, 1, 1)
  frame_mt <- substr(key, 2, 2) == "F"
  slice_mt <- substr(key, 3, 3) == "S"
  experimental <- substr(key, 4, 4) == "X"
  horiz_band <- substr(key, 5, 5) == "B"
  direct_render <- substr(key, 6, 6) == "D"
  
  abbrev <- regmatches(
    output2,
    regexpr("(?<=\\s\\S{6}\\s)\\S+(?=\\s+)", output2, perl = TRUE)
  )
  
  description <- regmatches(
    output2,
    regexpr("(?<=\\s{2})\\S[[:print:]]+$", output2, perl = TRUE)
  )
  
  out <- 
    tibble::tibble(
      name = abbrev,
      details = description,
      type = factor(
        type, 
        levels = c("V", "A", "S"), 
        labels = c("Video", "Audio", "Subtitles")
      ),
      frame_mt,
      slice_mt,
      experimental,
      horiz_band,
      direct_render
    )
  
  # Sort as requested
  if (sort_by_type) {
    out <- out[order(out$type, out$name), ]
  } else {
    out <- out[order(out$name), ]
  }
  
  out
}

# nvenc_encoder() / has_nvenc() ------------------------------------------------

#' NVIDIA nvenc hardware encoders
#'
#' Helpers for opt-in NVIDIA GPU (nvenc) video encoding. \code{nvenc_encoder()}
#' maps a codec family to its nvenc encoder name; \code{has_nvenc()} reports
#' whether that encoder is available in the local FFmpeg build.
#'
#' \code{has_nvenc()} is a \emph{cheap} check: it asks whether FFmpeg lists the
#' encoder (via \code{\link{ffmpeg_encoders}}), which reflects how FFmpeg was
#' built, not whether a working NVIDIA GPU and driver are present at run time. An
#' encode can still fail at run time on a machine with no capable GPU. To
#' override detection in a known environment (or in tests), set
#' \code{options(tidymedia.nvenc_encoders = )} to a character vector of encoder
#' names to treat as available.
#'
#' These back the \code{hardware = "nvenc"} toggle on
#' \code{\link{standardize_video}}, \code{\link{format_for_web}},
#' \code{\link{anonymize_video}}, \code{\link{crop_video}},
#' \code{\link{segment_video}}, \code{\link{compare_videos}},
#' \code{\link{picture_in_picture}}, and \code{\link{separate_audio_video}}
#' (and their \code{_batch} siblings). On the
#' verbs whose \code{video_codec} defaults to \code{NULL} (no codec named), the
#' H.264 family is assumed under \code{hardware = "nvenc"}, so a non-H.264
#' container (e.g. \code{.webm}) needs an explicit HEVC- or AV1-family
#' \code{video_codec}. Hardware
#' \emph{decoding} (\code{-hwaccel}) and GPU filter pipelines are out of scope;
#' use the \code{\link{ffmpeg}} escape hatch for those.
#'
#' @param codec The video codec family: one of \code{"h264"}, \code{"hevc"}, or
#'   \code{"av1"}.
#' @return \code{nvenc_encoder()} a single encoder-name string (e.g.
#'   \code{"h264_nvenc"}); \code{has_nvenc()} a length-one logical.
#' @seealso \code{\link{ffmpeg_encoders}} for the full encoder list,
#'   \code{\link{standardize_video}}, \code{\link{format_for_web}},
#'   \code{\link{anonymize_video}}, \code{\link{crop_video}},
#'   \code{\link{segment_video}}, \code{\link{compare_videos}},
#'   \code{\link{picture_in_picture}}, and
#'   \code{\link{separate_audio_video}} for the
#'   \code{hardware = "nvenc"} toggle that uses these.
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' nvenc_encoder("h264")
#' has_nvenc("h264")
#' @export
nvenc_encoder <- function(codec = c("h264", "hevc", "av1")) {
  codec <- rlang::arg_match(codec)
  paste0(codec, "_nvenc")
}

#' @rdname nvenc_encoder
#' @export
has_nvenc <- function(codec = c("h264", "hevc", "av1")) {
  enc <- nvenc_encoder(codec)
  pool <- getOption("tidymedia.nvenc_encoders", default = NULL)
  if (is.null(pool)) pool <- ffmpeg_encoders()$name
  enc %in% pool
}

# codec_family(): infer the nvenc codec family from a software codec name, so a
# user can flip hardware = "nvenc" while keeping a familiar video_codec (e.g.
# "libx264" -> "h264"). Aborts when no nvenc family matches.
codec_family <- function(video_codec, call = rlang::caller_env()) {
  if (grepl("264|avc", video_codec, ignore.case = TRUE)) return("h264")
  if (grepl("265|hevc", video_codec, ignore.case = TRUE)) return("hevc")
  if (grepl("av1", video_codec, ignore.case = TRUE)) return("av1")
  cli::cli_abort(
    c(
      "Cannot use {.code hardware = \"nvenc\"} with
       {.arg video_codec} = {.val {video_codec}}.",
      "x" = "No nvenc encoder maps to that codec.",
      "i" = "nvenc supports the h264, hevc, and av1 families (e.g.
             {.val libx264}, {.val libx265}, {.val libaom-av1})."
    ),
    call = call
  )
}

# resolve_hw_encoder(): pick the encoder name for a verb's hardware= choice.
# hardware = "none" returns the software video_codec unchanged; "nvenc" returns
# the nvenc encoder for video_codec's family when available, otherwise aborts
# (fallback = FALSE) or returns the software video_codec with a message
# (fallback = TRUE). Layer 2 only computes the argument here; Layer 1 assembles
# the command (D009, IP1).
#
# video_codec = NULL is the "leave the codec alone" sentinel carried by the
# codec-less re-encode verbs (M34/D016): no -codec:v is emitted, so the output
# keeps its container's default encoder. It is resolved here, in the one
# resolver seam, rather than in a second per-verb fork.
resolve_hw_encoder <- function(video_codec, hardware = c("none", "nvenc"),
                               fallback = FALSE, call = rlang::caller_env()) {
  hardware <- rlang::arg_match(hardware)
  rlang::check_bool(fallback, call = call)
  if (hardware == "none") {
    return(video_codec)
  }
  # The sentinel branch sits BEFORE codec_family(), which cannot infer a family
  # from nothing (it errors on NULL): under nvenc the sentinel assumes H.264,
  # the family every common container accepts.
  family <- if (is.null(video_codec)) {
    "h264"
  } else {
    codec_family(video_codec, call = call)
  }
  if (has_nvenc(family)) {
    return(nvenc_encoder(family))
  }
  if (fallback) {
    cli::cli_inform(c(
      "!" = if (is.null(video_codec)) {
        # Falling back from the sentinel keeps the sentinel -- never a silently
        # injected libx264, which would change the codec behind the user's back.
        "nvenc encoder {.val {nvenc_encoder(family)}} is not available;
         falling back to the output container's default video encoder."
      } else {
        "nvenc encoder {.val {nvenc_encoder(family)}} is not available;
         falling back to {.arg video_codec} = {.val {video_codec}}."
      }
    ))
    return(video_codec)
  }
  cli::cli_abort(
    c(
      "nvenc encoder {.val {nvenc_encoder(family)}} is not available.",
      "x" = "This FFmpeg build does not list it (see {.fn ffmpeg_encoders}).",
      "i" = "Use a machine with an nvenc-capable FFmpeg + NVIDIA GPU, or set
             {.code fallback = TRUE} to encode in software instead."
    ),
    call = call
  )
}

# apply_video_codec(): thread a verb's video_codec/hardware/fallback choice into
# a pipeline. The NULL sentinel (M34/D016) means "leave the codec alone", so no
# ffm_codec() call is made at all and the compiled command gains no -codec:v.
# Shared by the four codec-less re-encode verbs' pipelines, so the sentinel and
# the up-front token check are handled once. Not an ffm_* name: this is Layer 2
# computing an argument, not engine surface (D014, IP1).
apply_video_codec <- function(object, video_codec, hardware = "none",
                              fallback = FALSE, call = rlang::caller_env()) {
  # Validate the user's token before family inference so the error is the same
  # under hardware = "none" and "nvenc" (parity with anonymize_pipeline()).
  if (!is.null(video_codec)) check_token(video_codec, call = call)
  video_codec <- resolve_hw_encoder(video_codec, hardware, fallback, call = call)
  if (is.null(video_codec)) {
    return(object)
  }
  ffm_codec(object, video = video_codec)
}

# apply_audio_codec(): thread a verb's audio_codec choice into a pipeline. The
# default "copy" stream-copies the audio, matching the norm standardize_video()
# and anonymize_video() already document, so these verbs stop re-encoding audio
# to whatever the local build's container default is (M35/D017). NULL is the
# escape hatch: no ffm_codec() call at all, so the command gains no -codec:a.
# ffm_codec() token-checks the value too, but checking here attributes the error
# to the user-facing verb rather than to the engine (parity with
# apply_video_codec()).
apply_audio_codec <- function(object, audio_codec, call = rlang::caller_env()) {
  if (is.null(audio_codec)) {
    return(object)
  }
  check_token(audio_codec, call = call)
  ffm_codec(object, audio = audio_codec)
}


# segment_video() ---------------------------------------------------------

#' Segment Video
#'
#' Use FFmpeg to quickly break a single video file into multiple smaller video
#' files (with the same encoding) based on pairs of start and stop timestamps.
#' Segment video files will be named by taking the name of \code{infile} and
#' appending a suffix of an underscore (_) and an integer indicating which
#' segment (based on the order provided in \code{start} and \code{end}).
#'
#' @param infile A string containing the path to a video file.
#' @param start A vector containing one or more timestamps indicating the
#'   start of each segment to create. Can be either a numeric vector indicating
#'   seconds or a character vector with time duration syntax. Must have the same
#'   length as \code{end}.
#' @param end A vector containing one or more timestamps indicating the stop
#'   of each segment to create. Can be either a numeric vector indicating
#'   seconds or a character vector with time duration syntax. Must have the same
#'   length as \code{start}.
#' @param outfiles Either NULL or a character vector indicating the filename
#'   (with extension) for each segment to create. If NULL, will append a
#'   zero-padded integer to \code{infile}. If not NULL, must have the same
#'   length as \code{start}.
#' @param reencode A logical passed to \code{\link{ffm_seek}}: cut each segment
#'   frame-accurately by re-encoding (\code{TRUE}, default) or with a fast,
#'   lossless copy that snaps to keyframes (\code{FALSE}). See \code{ffm_seek}
#'   for the trade-off.
#' @param video_codec A string naming the output video codec, or \code{NULL}
#'   (default) to leave it unset, so the output container's default encoder is
#'   used and the compiled command is unchanged from one that never named a
#'   codec. A stream copy runs no encoder, so naming a codec (or a
#'   \code{hardware} backend) alongside \code{reencode = FALSE} is an error.
#' @param audio_codec A string naming the output audio codec. \code{"copy"}
#'   (default) stream-copies the audio through untouched; name an encoder (e.g.
#'   \code{"aac"}) to transcode it, or pass \code{NULL} to leave the codec unset
#'   so the output container's default encoder is used. A stream copy
#'   (\code{reencode = FALSE}) always copies the audio, so any other value is an
#'   error there. Stream-copying fails if the output container cannot hold the
#'   source audio codec (e.g. FLAC in \code{.mp4}) — name an encoder instead.
#' @param hardware The encoder backend: \code{"none"} (default, the software
#'   \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU encoding. When
#'   \code{"nvenc"}, the nvenc encoder for \code{video_codec}'s family is used
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}); with the default
#'   \code{video_codec = NULL} the H.264 family is assumed, so a non-H.264
#'   container (e.g. \code{.webm}) needs an explicit HEVC- or AV1-family
#'   \code{video_codec}. See \code{\link{has_nvenc}} for availability and its
#'   caveats.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, encode in software with a message (\code{TRUE}) instead of
#'   aborting (\code{FALSE}, default). With \code{video_codec = NULL} the
#'   fallback leaves the codec unset rather than picking one, so the codec never
#'   changes silently.
#' @param run A logical: run each segment's command (\code{TRUE}, default) or
#'   only compile them (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: cut segments in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one segment at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: one row per segment with its \code{command} (and,
#'   when \code{run = TRUE}, \code{success}).
#' @seealso [ffm_seek()], the builder it uses to cut; [ffm_batch()], the runner;
#'   [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [segment_video_batch()] for the many-file form.
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Two segments; run = FALSE compiles one command per segment
#' segment_video(video, start = c(0, 0.5), end = c(0.5, 1), run = FALSE)
#' @export
segment_video <- function(infile,
                          start,
                          end,
                          outfiles = NULL,
                          reencode = TRUE,
                          video_codec = NULL,
                          audio_codec = "copy",
                          hardware = c("none", "nvenc"),
                          fallback = FALSE,
                          run = TRUE,
                          parallel = FALSE) {

  check_file_exists(infile)
  if (!(is.numeric(start) || is.character(start))) {
    cli::cli_abort("{.arg start} must be a numeric or character vector.")
  }
  if (!(is.numeric(end) || is.character(end))) {
    cli::cli_abort("{.arg end} must be a numeric or character vector.")
  }
  if (length(start) != length(end)) {
    cli::cli_abort("{.arg start} and {.arg end} must have the same length.")
  }
  if (!is.null(outfiles) && length(outfiles) != length(start)) {
    cli::cli_abort("{.arg outfiles} must have the same length as {.arg start}.")
  }
  rlang::check_bool(reencode)
  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  # If no names are provided, derive per-segment names from the input file.
  if (is.null(outfiles)) {
    outfiles <- derive_segment_names(rep(infile, length(start)))
  }

  # Fan-out (one input -> many outputs) is a Layer 2 concern: build one
  # single-output seek pipeline per segment and run them through ffm_batch
  # (D-M03-2). The engine stays single-output (D003).
  jobs <- tibble::tibble(
    input = infile, output = outfiles, start = start, end = end
  )
  ffm_batch(
    jobs,
    function(input, output, start, end, ...) {
      segment_pipeline(input, output, start, end, reencode,
                       video_codec = video_codec, audio_codec = audio_codec,
                       hardware = hardware, fallback = fallback)
    },
    run = run,
    parallel = parallel
  )
}


# derive_segment_names() --------------------------------------------------

# Derive one output path per segment from its input path, appending
# `_<n>.<ext>` to each input's basename. Numbering restarts per input file (in
# row order) and is zero-padded to that input's own segment count, so the
# single-input case (segment_video) and the multi-input jobs table
# (segment_video_batch with no `output` column) share one naming rule.
derive_segment_names <- function(input) {
  out <- character(length(input))
  for (f in unique(input)) {
    sel <- input == f
    padded <- pad_integers(seq_len(sum(sel)))
    out[sel] <- paste0(
      tools::file_path_sans_ext(f), "_", padded, ".", tools::file_ext(f)
    )
  }
  out
}


# derive_frame_names() ----------------------------------------------------

# Derive one image path per frame from its input path, appending `_<n>.<format>`
# to each input's basename. Same per-input-restart, zero-padded rule as
# derive_segment_names(), but the extension is the image `format` (a frame is an
# image, not a copy of the source container) rather than the input's extension.
derive_frame_names <- function(input, format = "png") {
  out <- character(length(input))
  for (f in unique(input)) {
    sel <- input == f
    padded <- pad_integers(seq_len(sum(sel)))
    out[sel] <- paste0(tools::file_path_sans_ext(f), "_", padded, ".", format)
  }
  out
}


# segment_pipeline() ------------------------------------------------------

# Shared cut logic for segment_video() and segment_video_batch(): build one
# single-output seek pipeline for a single segment, stream-copying on the fast
# (non-reencode) path. Fan-out verbs stay single-output per job (D003, D007);
# both verbs wrap this in a closure that captures the scalar `reencode`.
segment_pipeline <- function(input, output, start, end, reencode,
                             video_codec = NULL, audio_codec = "copy",
                             hardware = "none",
                             fallback = FALSE, call = rlang::caller_env()) {
  # A stream copy writes the source video bytes through untouched, so naming an
  # encoder -- in software or on the GPU -- cannot mean anything on that path
  # (D008 keeps the copy lossless and opt-in). Abort rather than silently drop
  # the request; the check lives here so both callers inherit it per row
  # (M34/D016).
  if (!reencode && (!is.null(video_codec) || !identical(hardware, "none"))) {
    cli::cli_abort(
      c(
        "{.arg video_codec} and {.arg hardware} need a re-encoding cut.",
        "x" = "{.code reencode = FALSE} stream-copies each segment, so no
               encoder runs.",
        "i" = "Pass {.code reencode = TRUE} to cut by re-encoding, or drop
               {.arg video_codec} / {.arg hardware}."
      ),
      call = call
    )
  }
  # Same reasoning for the audio stream, with one wrinkle: the copy path's
  # ffm_copy() sets -codec:a copy itself, so "copy" is the one value that agrees
  # with it. Anything else -- a named encoder, or NULL asking for no -codec:a at
  # all -- would be silently overwritten by ffm_copy(), so it aborts (M35/D017).
  if (!reencode && !identical(audio_codec, "copy")) {
    cli::cli_abort(
      c(
        "{.arg audio_codec} needs a re-encoding cut.",
        "x" = "{.code reencode = FALSE} stream-copies every stream, so the
               audio is always copied.",
        "i" = "Pass {.code reencode = TRUE} to cut by re-encoding, or leave
               {.code audio_codec = \"copy\"}."
      ),
      call = call
    )
  }
  p <- ffm_seek(ffm_files(input, output), start = start, end = end,
                reencode = reencode)
  if (!reencode) p <- ffm_copy(p)
  p <- apply_audio_codec(p, audio_codec, call = call)
  apply_video_codec(p, video_codec, hardware, fallback, call = call)
}


# segment_video_batch() --------------------------------------------------------

#' Segment Many Videos From a Jobs Table
#'
#' Cut segments across many input files from a single jobs tibble — the
#' **batch** (table-driven) sibling of [segment_video()] for when your segments
#' span more than one input. Each row is one segment; the four required columns
#' name its source, destination, and cut points. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per segment.
#'
#' @param jobs A data frame with one row per segment and (at least) the columns
#'   \code{input} (source path), \code{start} and \code{end} (cut points; a
#'   numeric column of seconds or a character column with time-duration syntax).
#'   Two optional columns are recognized: \code{output} (destination path) and
#'   \code{reencode} (a logical; see the \code{reencode} argument). If
#'   \code{output} is absent, one is derived per row by appending
#'   \code{_<n>.<ext>} to each input's basename, with the segment number
#'   restarting at 1 for each input file (the same rule as
#'   \code{\link{segment_video}}). A \code{video_codec} or \code{audio_codec}
#'   column overrides that argument per row, with \code{NA} meaning "leave the
#'   codec unset" (the column's way of writing the argument's \code{NULL}). Any
#'   other columns are ignored.
#' @param reencode A logical passed to \code{\link{ffm_seek}}: cut each segment
#'   frame-accurately by re-encoding (\code{TRUE}, default) or with a fast,
#'   lossless copy that snaps to keyframes (\code{FALSE}). See \code{ffm_seek}
#'   for the trade-off. Applies to every row, unless \code{jobs} carries a
#'   \code{reencode} column, which overrides this argument on a per-row basis.
#' @param video_codec A string naming the output video codec, applied to every
#'   row lacking a \code{video_codec} column, or \code{NULL} (default) to leave
#'   it unset so each segment keeps its container's default encoder. A row that
#'   resolves to a codec while cutting by stream copy (\code{reencode = FALSE},
#'   as an argument or a column) is an error: no encoder runs on that path.
#' @param audio_codec A string naming the output audio codec, applied to every
#'   row lacking an \code{audio_codec} column. \code{"copy"} (default)
#'   stream-copies the audio; name an encoder to transcode it, or \code{NULL} to
#'   leave the codec unset. A row that resolves to anything but \code{"copy"}
#'   while cutting by stream copy (\code{reencode = FALSE}, as an argument or a
#'   column) is an error, so a jobs table mixing stream-copy rows with a
#'   transcoding \code{audio_codec} must be split into separate calls.
#' @param hardware,fallback The encoder backend and its fallback behavior,
#'   applied to the whole batch (a property of the machine, not of a row, so
#'   neither is read as a \code{jobs} column). See [segment_video()].
#'   Because \code{hardware} is batch-wide, \code{hardware = "nvenc"} conflicts
#'   with a stream-copy row on its own — even one naming no codec — so a jobs
#'   table mixing \code{reencode = FALSE} rows with GPU encoding must be split
#'   into separate calls.
#' @param run A logical: run each segment's command through FFmpeg
#'   (\code{TRUE}, default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: cut segments in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one segment at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso [segment_video()] for the single-input, parallel-vector form;
#'   [ffm_batch()] for the batch runner and the arguments forwarded through
#'   \code{...}; [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [ffm_seek()] for the cut trade-off.
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   output = c("a.mp4", "b.mp4"),
#'   start  = c(0, 0.5),
#'   end    = c(0.5, 1)
#' )
#' # run = FALSE compiles one command per segment without calling FFmpeg
#' segment_video_batch(jobs, run = FALSE)
#' @export
segment_video_batch <- function(jobs, reencode = TRUE, video_codec = NULL,
                           audio_codec = "copy",
                           hardware = c("none", "nvenc"), fallback = FALSE,
                           run = TRUE, parallel = FALSE, ...) {

  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per segment.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  required <- c("input", "start", "end")
  missing <- setdiff(required, names(jobs))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} must have columns {.val {required}}.",
      "x" = "Missing column{?s}: {.val {missing}}."
    ))
  }
  # Validate cut-point column types up front (parity with segment_video()), so a
  # bad column fails clearly here rather than as an opaque FFmpeg error.
  if (!(is.numeric(jobs$start) || is.character(jobs$start))) {
    cli::cli_abort("The {.field start} column of {.arg jobs} must be numeric or character.")
  }
  if (!(is.numeric(jobs$end) || is.character(jobs$end))) {
    cli::cli_abort("The {.field end} column of {.arg jobs} must be numeric or character.")
  }
  if ("reencode" %in% names(jobs) &&
      (!is.logical(jobs$reencode) || anyNA(jobs$reencode))) {
    cli::cli_abort(
      "The {.field reencode} column of {.arg jobs} must be {.val {TRUE}} or {.val {FALSE}} (no {.val {NA}})."
    )
  }
  check_batch_codec_col(jobs)
  check_batch_codec_col(jobs, "audio_codec")
  rlang::check_bool(reencode)

  # Auto-name outputs when the column is absent: derive per-input segment names
  # (numbering restarts per input file) and carry them on the returned tibble.
  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_segment_names(jobs$input)
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output seek pipeline
  # per row, sharing segment_pipeline() with segment_video(). A per-row
  # `reencode` column (arriving via `...` from pmap) overrides the scalar arg;
  # `...` also forwards ffm_batch options (verify/manifest/...) to the runner,
  # never to the pipeline builder.
  ffm_batch(
    jobs,
    function(input, output, start, end, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      segment_pipeline(
        input, output, start, end,
        reencode = pick("reencode", reencode),
        video_codec = batch_codec_cell(pick("video_codec", video_codec)),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        hardware = hardware,
        fallback = fallback
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# extract_frame_batch() --------------------------------------------------------

#' Extract Still Frames From Many Videos From a Jobs Table
#'
#' Grab one still image per row across many input files from a single jobs
#' tibble — the **batch** (table-driven) sibling of [extract_frame()] for when
#' your
#' frames span more than one input. Each row is one frame; the required columns
#' name its source and the moment to capture. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per frame.
#'
#' @param jobs A data frame with one row per frame and (at least) an
#'   \code{input} column (source path) plus \strong{exactly one} of a
#'   \code{timestamp} column (seconds, or \pkg{FFmpeg} time-duration strings) or
#'   a \code{frame} column (whole frame numbers, converted per row to a
#'   timestamp via the input's frame rate, as \code{\link{extract_frame}} does).
#'   An optional \code{output} column names the destination image; when absent,
#'   one is derived per row by appending \code{_<n>.<format>} to each input's
#'   basename, with the frame number restarting at 1 for each input file. Any
#'   other columns are ignored.
#' @param format A string giving the image file extension used when \code{output}
#'   is derived (ignored when \code{jobs} carries an \code{output} column).
#'   (default = \code{"png"})
#' @param run A logical: run each frame's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: grab frames in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one frame at a time
#'   and warns.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso [extract_frame()] for the single-frame form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [segment_video_batch()] for the segment-cutting sibling.
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input     = c(video, video),
#'   output    = c("a.png", "b.png"),
#'   timestamp = c(0.25, 0.75)
#' )
#' # run = FALSE compiles one command per frame without calling FFmpeg
#' extract_frame_batch(jobs, run = FALSE)
#' @export
extract_frame_batch <- function(jobs, format = "png", run = TRUE,
                           parallel = FALSE, ...) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per frame.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }
  rlang::check_string(format)

  # Table-level exclusivity: exactly one of the selection columns, mirroring
  # extract_frame()'s scalar timestamp/frame exclusive-or.
  has_ts <- "timestamp" %in% names(jobs)
  has_fr <- "frame" %in% names(jobs)
  if (has_ts == has_fr) {
    cli::cli_abort(c(
      "{.arg jobs} must have exactly one of a {.field timestamp} or {.field frame} column.",
      "x" = if (has_ts) "Both columns are present." else "Neither column is present."
    ))
  }

  # Validate the selection column's type + reject NA up front, so a bad column
  # fails clearly here rather than as an opaque FFmpeg (or framerate) error.
  if (has_ts) {
    if (!(is.numeric(jobs$timestamp) || is.character(jobs$timestamp))) {
      cli::cli_abort("The {.field timestamp} column of {.arg jobs} must be numeric or character.")
    }
    if (anyNA(jobs$timestamp)) {
      cli::cli_abort("The {.field timestamp} column of {.arg jobs} must not contain {.val {NA}}.")
    }
    # A numeric timestamp must be finite (parity with extract_frame()'s
    # finite = TRUE check); anyNA() above already caught NA/NaN, so this is Inf.
    if (is.numeric(jobs$timestamp) && any(!is.finite(jobs$timestamp))) {
      cli::cli_abort("The {.field timestamp} column of {.arg jobs} must be finite.")
    }
  } else {
    if (!is.numeric(jobs$frame)) {
      cli::cli_abort("The {.field frame} column of {.arg jobs} must be numeric.")
    }
    if (anyNA(jobs$frame)) {
      cli::cli_abort("The {.field frame} column of {.arg jobs} must not contain {.val {NA}}.")
    }
    # Whole numbers only (parity with extract_frame()'s check_number_whole() and
    # this verb's documented "whole frame numbers" contract).
    if (any(jobs$frame %% 1 != 0)) {
      cli::cli_abort("The {.field frame} column of {.arg jobs} must contain whole numbers.")
    }
  }

  # A factor input column carries paths as levels; treat them as the strings
  # they are (parity with the character case).
  jobs$input <- as.character(jobs$input)

  # Auto-name outputs when the column is absent: per-input frame names with the
  # image extension, carried on the returned tibble.
  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_frame_names(jobs$input, format = format)
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-frame pipeline per
  # row, sharing frame_pipeline() with extract_frame(). frame->timestamp
  # resolution happens per row (via the input's frame rate); `...` forwards
  # ffm_batch options (verify/manifest/...) to the runner, never to the builder.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      timestamp <- if (!is.null(dots$timestamp)) {
        dots$timestamp
      } else {
        dots$frame / get_frame_rate(input)
      }
      frame_pipeline(input, output, timestamp)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_frames_dir() -----------------------------------------------------

# Derive one output directory per input for sample_frames_batch() when neither
# an `outdir` column nor a scalar `outdir` is given: `<input-base>_frames` beside
# each input. Per-input directories keep each recording's sequence separate, so
# the batch never collides even when many inputs sample at once.
derive_frames_dir <- function(input) {
  file.path(
    dirname(input),
    paste0(tools::file_path_sans_ext(basename(input)), "_frames")
  )
}


# sample_frames_batch() ---------------------------------------------------

#' Sample frames from many videos at a fixed rate from a jobs table
#'
#' Sample many videos into numbered image sequences from a single jobs tibble —
#' the **batch** (table-driven) sibling of [sample_frames()]. Each row is one
#' input video sampled at a fixed rate into its own image sequence. This is a
#' thin wrapper over \code{\link{ffm_batch}}: one reproducible compiled command
#' per input.
#'
#' Supply the sampling rate once as the scalar \code{fps} or \code{interval}
#' argument (applied to every row), or per row as an \code{fps} or
#' \code{interval} column that overrides the scalar of the same name. Exactly one
#' of the two — fps \emph{or} interval — may be supplied across arguments and
#' columns.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). Optional columns: \code{outdir} (the
#'   output directory for that row's sequence; when absent, one is derived as
#'   \code{<input-base>_frames} beside each input), and \code{fps} /
#'   \code{interval} (per-row rate overrides). Any other columns are ignored.
#' @param fps,interval The sampling rate applied to every row, as in
#'   [sample_frames()]; a per-row column of the same name overrides it. Supply
#'   exactly one of the two (as an argument or a column). (default = \code{NULL})
#' @param outdir An optional single output directory for all rows (overridden by
#'   an \code{outdir} column); when both are absent, per-input directories are
#'   derived. (default = \code{NULL})
#' @param format A string giving the output image file extension, as in
#'   [sample_frames()]. (default = \code{"png"})
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: sample in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one at a time and
#'   warns.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and the resolved \code{outdir} column when it was derived; when
#'   \code{run = TRUE}, a \code{success} column, plus any columns the forwarded
#'   arguments add, e.g. \code{verified}).
#' @seealso [sample_frames()] for the single-video form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [extract_frame_batch()] for the enumerated-frame sibling.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   outdir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b"))
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' sample_frames_batch(jobs, fps = 2, run = FALSE)
#' @export
sample_frames_batch <- function(jobs, fps = NULL, interval = NULL,
                                outdir = NULL, format = "png", run = TRUE,
                                parallel = FALSE, ...) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }
  format <- check_image_format(format)

  # Table-level rate exclusivity: exactly one of an fps source or an interval
  # source (argument or column), mirroring sample_frames()' scalar XOR. The
  # per-row value checks are inherited from resolve_sample_fps() in the closure.
  fps_src <- !is.null(fps) || "fps" %in% names(jobs)
  interval_src <- !is.null(interval) || "interval" %in% names(jobs)
  if (fps_src == interval_src) {
    cli::cli_abort(c(
      "Provide exactly one of {.arg fps} or {.arg interval} (argument or column).",
      "x" = if (fps_src) "Both are present." else "Neither is present."
    ))
  }

  # Validate present override columns up front so a bad column fails clearly here
  # rather than as an opaque FFmpeg error mid-batch (M11 parity lesson). An `fps`
  # column may be character (an FFmpeg rate expression) but `interval` may not —
  # resolve_sample_fps() rejects a character interval, so type it numeric-only
  # here (parity with extract_frame_batch()'s per-column typing).
  if ("fps" %in% names(jobs) &&
      !(is.numeric(jobs$fps) || is.character(jobs$fps))) {
    cli::cli_abort("The {.field fps} column of {.arg jobs} must be numeric or character.")
  }
  if ("interval" %in% names(jobs) && !is.numeric(jobs$interval)) {
    cli::cli_abort("The {.field interval} column of {.arg jobs} must be numeric.")
  }
  for (col in intersect(c("fps", "interval"), names(jobs))) {
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with extract_frame_batch()).
  jobs$input <- as.character(jobs$input)

  # Resolve the per-row output directory: an explicit `outdir` column wins; else
  # the scalar `outdir` (one directory for every row); else one derived per
  # input. Carried on the returned tibble.
  if ("outdir" %in% names(jobs)) {
    if (!is.character(jobs$outdir) || anyNA(jobs$outdir)) {
      cli::cli_abort("The {.field outdir} column of {.arg jobs} must be character (no {.val {NA}}).")
    }
  } else if (!is.null(outdir)) {
    rlang::check_string(outdir)
    jobs$outdir <- outdir
  } else {
    jobs$outdir <- derive_frames_dir(jobs$input)
  }

  # Reject colliding output patterns before running: each row's pattern is
  # `<outdir>/<input-base>_%0Nd.<fmt>`, so two rows sharing a directory whose
  # inputs also share a basename (e.g. a duplicated input, or `cam1/rec.mp4` +
  # `cam2/rec.mp4` under one `outdir`) would silently overwrite each other's
  # frames. Fail clearly here rather than lose data mid-batch (the sibling
  # dup-input guard, adapted to the pattern level).
  patterns <- derive_frame_pattern(jobs$input, jobs$outdir, NULL, format)
  collisions <- unique(patterns[duplicated(patterns)])
  if (length(collisions) > 0) {
    cli::cli_abort(c(
      "Two or more jobs would write to the same image sequence.",
      "x" = "Colliding output pattern{?s}: {.file {collisions}}.",
      "i" = "Give colliding inputs distinct {.field outdir}s or rename them."
    ))
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one image2-pattern sampling
  # pipeline per input, sharing sample_frames_pipeline() with sample_frames().
  # A per-row rate column (arriving via `...` from pmap) overrides the scalar
  # arg of the same name; `...` also forwards ffm_batch options
  # (verify/manifest/...) to the runner, never to the pipeline builder.
  ffm_batch(
    jobs,
    function(input, outdir, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      rate <- resolve_sample_fps(pick("fps", fps), pick("interval", interval))
      dir_i <- ensure_dir(outdir)
      pattern <- derive_frame_pattern(input, dir_i, NULL, format)
      sample_frames_pipeline(input, pattern, rate)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_standardized_names() ---------------------------------------------

# Derive one output path per input for standardize_video_batch() when the `output`
# column is absent: `<base>_standardized.<input-ext>` (standardization keeps the
# source container, unlike a frame which becomes an image). Standardization is
# one-input -> one-output, so -- unlike the per-input-numbering siblings -- a
# duplicated input with no explicit `output` would collide; the caller
# (standardize_video_batch) rejects that up front, so this helper assumes unique
# inputs and stays a pure name map.
derive_standardized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_standardized.", tools::file_ext(input)
  )
}


# standardize_video_batch() ----------------------------------------------------

#' Standardize Many Videos From a Jobs Table
#'
#' Re-encode many input files to a reproducible format from a single jobs tibble
#' — the **batch** (table-driven) sibling of [standardize_video()] for when you
#' have
#' more than one video to standardize. Each row is one input; the only required
#' column names its source. This is a thin wrapper over \code{\link{ffm_batch}}:
#' one reproducible compiled command per input.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_standardized} to each input's basename, keeping the input's
#'   extension (e.g. \code{clip.mkv} becomes \code{clip_standardized.mkv}).
#'   Because standardization is one-input-to-one-output, a duplicated
#'   \code{input} with no \code{output} column would collide and is rejected.
#'   Each of the six standardization knobs — \code{width}, \code{height},
#'   \code{fps}, \code{video_codec}, \code{audio_codec}, \code{pixel_format} —
#'   may also appear as a
#'   column to override the corresponding argument on a per-row basis; rows (or
#'   knobs) that omit the column fall back to the argument's value. In either
#'   codec column, \code{NA} leaves that row's codec unset (the column form of
#'   \code{video_codec = NULL} / \code{audio_codec = NULL}); in a \code{width},
#'   \code{height}, \code{fps} or \code{pixel_format} column it is an error.
#'   \code{pixel_format} has no unset state to express; \code{width},
#'   \code{height} and \code{fps} do accept \code{NULL} as arguments, but their
#'   columns have no \code{NA} spelling for it. Any other columns are ignored.
#' @param width,height Optional target dimensions applied to every row, unless
#'   \code{jobs} carries a column of the same name (see \code{jobs}). When only
#'   one is given the other is derived to preserve aspect ratio; when neither is
#'   given the frame is floor-cropped to even dimensions so odd-sized sources
#'   encode. (default = \code{NULL})
#' @param fps Optional target frame rate applied to every row, unless
#'   \code{jobs} carries an \code{fps} column. (default = \code{NULL}, i.e.
#'   leave the frame rate unchanged)
#' @param video_codec A string naming the video codec applied to every row,
#'   unless \code{jobs} carries a \code{video_codec} column, in which case
#'   \code{NA} in a cell leaves that row's codec unset. Default
#'   \code{"libx264"}; \code{NULL} emits no \code{-codec:v} and lets the output
#'   container's default encoder decide (for a \code{.webm} output, pass
#'   \code{audio_codec = NULL} too — the default \code{"copy"} would otherwise
#'   carry a codec WebM cannot hold).
#' @param audio_codec A string naming the audio codec applied to every row,
#'   unless \code{jobs} carries an \code{audio_codec} column, in which case
#'   \code{NA} in a cell leaves that row's codec unset. \code{"copy"} (default)
#'   stream-copies the audio through untouched; name an encoder (e.g.
#'   \code{"aac"}) when the source audio cannot be copied into the output
#'   container.
#' @param pixel_format A string naming the pixel format applied to every row,
#'   unless \code{jobs} carries a \code{pixel_format} column.
#'   (default = \code{"yuv420p"})
#' @param hardware The encoder backend applied to every row: \code{"none"}
#'   (default) or \code{"nvenc"} for NVIDIA GPU encoding. Batch-wide (not a
#'   per-row column). See \code{\link{standardize_video}} and
#'   \code{\link{has_nvenc}}.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, re-encode with the software \code{video_codec} and a message
#'   (\code{TRUE}) instead of aborting (\code{FALSE}, default).
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: standardize in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one input at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso [standardize_video()] for the single-input form; [ffm_batch()] for
#'   the batch runner and the arguments forwarded through \code{...};
#'   [segment_video_batch()] and [extract_frame_batch()] for the other
#'   table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   output = c("a.mp4", "b.mp4"),
#'   width  = c(640, 320)
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' standardize_video_batch(jobs, run = FALSE)
#' @export
standardize_video_batch <- function(jobs, width = NULL, height = NULL, fps = NULL,
                               video_codec = "libx264", audio_codec = "copy",
                               pixel_format = "yuv420p",
                               hardware = c("none", "nvenc"), fallback = FALSE,
                               run = TRUE, parallel = FALSE, ...) {

  hardware <- rlang::arg_match(hardware)
  # NULL is legal (the "emit no -codec:a" escape hatch), so allow_null (M39).
  rlang::check_string(audio_codec, allow_null = TRUE)

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }


  # A factor input column carries paths as levels; treat them as strings
  # (parity with extract_frame_batch()).
  jobs$input <- as.character(jobs$input)

  # Validate present override columns up front so a bad column fails clearly
  # here rather than as an opaque FFmpeg error mid-batch (M11 parity lesson).
  # Value-level checks (positive dimensions, known codec/pixfmt) are inherited
  # per row from standardize_pipeline()'s check_dim/check_token guards.
  dim_cols <- c("width", "height", "fps")
  for (col in intersect(dim_cols, names(jobs))) {
    if (!(is.numeric(jobs[[col]]) || is.character(jobs[[col]]))) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric or character.")
    }
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  str_cols <- c("pixel_format")
  for (col in intersect(str_cols, names(jobs))) {
    if (!is.character(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be character (no {.val {NA}}).")
    }
  }
  # Both codec columns take the codec guard, which admits NA and the all-NA
  # logical column R hands back (M34 lesson). `video_codec` sat in str_cols
  # above until M42, justified by a comment calling it "a literal libx264
  # default with no sentinel" -- false when written, since the argument accepts
  # NULL, and now the family rule: NA is the column form of that NULL (D022).
  # `pixel_format` stays in str_cols: not a codec argument, no sentinel, so an
  # NA cell there spells nothing.
  #
  # Checking video_codec here rather than in the loop's slot moves it after
  # `pixel_format` in the reporting order for a jobs table with two bad columns.
  # Named because M41's review twice caught a guard reassigning precedence
  # unremarked; the two codec columns now report together.
  check_batch_codec_col(jobs, "video_codec")
  check_batch_codec_col(jobs, "audio_codec")

  # Auto-name outputs when the column is absent. One input -> one output, so a
  # duplicated input with no explicit output would map to the same file; reject
  # that rather than silently overwrite (the deliberate trade-off for readable
  # `_standardized` names over sibling-style per-input numbering).
  if (!"output" %in% names(jobs)) {
    dupes <- unique(jobs$input[duplicated(jobs$input)])
    if (length(dupes) > 0) {
      cli::cli_abort(c(
        "{.arg jobs} has duplicated {.field input} paths but no {.field output} column.",
        "x" = "Duplicated input{?s}: {.val {dupes}}.",
        "i" = "Add an {.field output} column to name each row's destination."
      ))
    }
    jobs$output <- derive_standardized_names(jobs$input)
  }

  # video_codec had no front-door check, so a non-string reached ffm_codec() per
  # row and aborted inside purrr::pmap() naming Layer-1's `video` -- the caller's
  # own argument name never appeared (M41). allow_null keeps NULL compiling
  # exactly as it does today: no -codec:v, the container's default encoder.
  #
  # Placed at the END of this verb's front-door validation, not beside the
  # other scalar checks: before M41 this argument was only read per row
  # inside the fan-out, so EVERY check above it reported first on a call
  # that was wrong about two things at once. Moving the guard up the
  # function silently reassigned that precedence -- first past the jobs
  # SHAPE block (review A6), then past its content checks too (review
  # A1r3). Here it changes nothing but the message a bad codec gets.
  rlang::check_string(video_codec, allow_null = TRUE)

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output re-encode
  # pipeline per row, sharing standardize_pipeline() with standardize_video().
  # A per-row knob column (arriving via `...` from pmap) overrides the scalar
  # arg of the same name; `...` also forwards ffm_batch options
  # (verify/manifest/...) to the runner, never to the pipeline builder.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      standardize_pipeline(
        input, output,
        width = pick("width", width),
        height = pick("height", height),
        fps = pick("fps", fps),
        video_codec = batch_codec_cell(pick("video_codec", video_codec)),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        pixel_format = pick("pixel_format", pixel_format),
        hardware = hardware,
        fallback = fallback
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_stripped_names() -------------------------------------------------

# Derive one output path per input for strip_metadata_batch() when the `output`
# column is absent: `<base>_stripped.<input-ext>` (a metadata scrub keeps the
# source container). The base keeps the input's directory, so inputs in
# different folders never collide; strip_metadata_batch() rejects any duplicated
# *resolved* output up front (M26), so this helper stays a pure name map.
derive_stripped_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_stripped.", tools::file_ext(input)
  )
}


# strip_metadata_batch() --------------------------------------------------------

#' Strip Metadata From Many Files From a Jobs Table
#'
#' De-identify many input files from a single jobs tibble — the **batch**
#' (table-driven) sibling of [strip_metadata()] for when you have more than one
#' file to scrub. Each row is one input; the only required column names its
#' source. This is a thin wrapper over \code{\link{ffm_batch}}: one reproducible
#' stream-copy strip command per input, sharing the same pipeline (and its
#' bit-exact, metadata-dropping behavior) as the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_stripped} to each input's basename, keeping the input's extension
#'   (e.g. \code{clip.mkv} becomes \code{clip_stripped.mkv}). Any two rows that
#'   resolve to the **same** output path — a duplicated \code{input} with no
#'   \code{output} column, or a repeated explicit \code{output} — are rejected
#'   so one file cannot silently overwrite another. Any other columns are
#'   ignored (the scrub has no per-row knobs).
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: scrub in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one input at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso [strip_metadata()] for the single-input form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [standardize_video_batch()] and [anonymize_video_batch()] for the other
#'   table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = video, output = "clean.mp4")
#' # run = FALSE compiles one command per input without calling FFmpeg
#' strip_metadata_batch(jobs, run = FALSE)
#' @export
strip_metadata_batch <- function(jobs, run = TRUE, parallel = FALSE, ...) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with the other *_batch verbs).
  jobs$input <- as.character(jobs$input)
  if (anyNA(jobs$input)) {
    cli::cli_abort("The {.field input} column of {.arg jobs} must not contain {.val {NA}}.")
  }

  # Resolve outputs (derive when absent), then reject any collision on the
  # *resolved* path — not just duplicated inputs — so an explicit `output`
  # column repeated across rows can't silently overwrite either (M26).
  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_stripped_names(jobs$input)
  } else {
    jobs$output <- as.character(jobs$output)
    if (anyNA(jobs$output)) {
      cli::cli_abort("The {.field output} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  dupes <- unique(jobs$output[duplicated(jobs$output)])
  if (length(dupes) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} has rows that resolve to the same output path.",
      "x" = "Colliding output{?s}: {.val {dupes}}.",
      "i" = "Give each row a distinct {.field output}, or de-duplicate the inputs."
    ))
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output strip pipeline
  # per row, sharing strip_metadata_pipeline() with strip_metadata(). `...`
  # forwards ffm_batch options (verify/manifest/...) to the runner; the scrub
  # itself has no per-row knobs, so extra job columns are ignored.
  ffm_batch(
    jobs,
    function(input, output, ...) strip_metadata_pipeline(input, output),
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_normalized_names() -----------------------------------------------

# Derive one output path per input for normalize_audio_batch() when the `output`
# column is absent: `<base>_normalized.<input-ext>` (loudness normalization
# keeps the source container). One input -> one output, so a duplicated input
# with no explicit `output` would collide; the caller (normalize_audio_batch)
# rejects that up front, so this helper assumes unique inputs and stays a pure
# name map (parity with derive_standardized_names()).
derive_normalized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_normalized.", tools::file_ext(input)
  )
}


# normalize_audio_batch() ------------------------------------------------------

#' Normalize Many Files' Audio Loudness From a Jobs Table
#'
#' Loudness-normalize the audio of many input files (EBU R128) from a single
#' jobs tibble — the **batch** (table-driven) sibling of [normalize_audio()] for
#' when you have more than one file to normalize. Each row is one input; the
#' only required column names its source. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same \code{loudnorm} pipeline (and per-value validation) as the scalar
#' verb. Set \code{two_pass = TRUE} for accurate measured/linear normalization
#' across the whole table (see \code{two_pass}).
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_normalized} to each input's basename, keeping the input's extension
#'   (e.g. \code{clip.mkv} becomes \code{clip_normalized.mkv}). Because
#'   normalization is one-input-to-one-output, a duplicated \code{input} with no
#'   \code{output} column would collide and is rejected. Each of the five
#'   loudness knobs — \code{target_loudness}, \code{true_peak},
#'   \code{loudness_range}, \code{channels}, \code{sample_rate} — may also appear
#'   as a column to override the corresponding argument on a per-row basis; rows
#'   (or knobs) that omit the column fall back to the argument's value. An
#'   optional \code{audio_codec} column (character) names each row's output
#'   audio encoder, with \code{NA} meaning "leave the encoder unset"; rows
#'   omitting it fall back to the \code{audio_codec} argument. Any other
#'   columns are ignored.
#' @param target_loudness,true_peak,loudness_range The EBU R128 loudness targets
#'   applied to every row, unless \code{jobs} carries a column of the same name
#'   (see \code{jobs}). Defaults follow EBU Recommendation R 128 (2014):
#'   \code{target_loudness = -23} LUFS, \code{true_peak = -1} dBTP,
#'   \code{loudness_range = 7} LU.
#' @param channels The output channel count applied to every row, unless
#'   \code{jobs} carries a \code{channels} column, e.g. \code{1} to downmix to
#'   mono. \code{NULL} (default) keeps each source's channel layout.
#' @param sample_rate The output sample rate in Hz applied to every row, unless
#'   \code{jobs} carries a \code{sample_rate} column. \code{NULL} (default) lets
#'   \code{loudnorm} choose (it resamples, up to 192 kHz encoder-capped — not the
#'   source rate); set this to pin the output rate.
#' @param audio_codec The output audio encoder applied to every row, unless
#'   \code{jobs} carries an \code{audio_codec} column, e.g. \code{"aac"}.
#'   \code{NULL} (default) emits no \code{-codec:a}, leaving the output
#'   container's default encoder in place. \code{"copy"} is an error: loudness
#'   normalization filters the audio, so it must be re-encoded. See
#'   \code{\link{normalize_audio}}.
#' @param two_pass A logical selecting the batch normalization mode for
#'   \emph{every} row (\code{two_pass} is a whole-table switch, not a per-row
#'   column). \code{FALSE} (default) keeps the single-pass \code{loudnorm}
#'   pipeline. \code{TRUE} runs the accurate two-pass (measured/linear) path as a
#'   two-phase fan-out: an \emph{analysis pass} first measures every input's
#'   loudness (honoring \code{parallel} and each row's targets), and a
#'   \emph{correction pass} then feeds those measurements back with
#'   \code{linear=true} so each output hits its EBU R128 target precisely — the
#'   table-wide sibling of \code{\link{normalize_audio}}'s \code{two_pass}. The
#'   five measured values are surfaced on the result as columns \code{measured_I},
#'   \code{measured_TP}, \code{measured_LRA}, \code{measured_thresh}, and
#'   \code{offset}. Because it must measure each input, two-pass
#'   \strong{always runs the analysis pass through FFmpeg} (it needs the binary
#'   and readable inputs), even when \code{run = FALSE}. If any row's analysis
#'   fails or yields no parseable measurement, the call aborts — naming the
#'   offending row(s) — before any correction command is built. \strong{Silent}
#'   rows are the exception: a silent input (analysis loudness \code{-inf})
#'   cannot be normalized to a target, but one silent row does not abort the
#'   batch — the non-silent rows are normalized, the silent rows are marked in a
#'   logical \code{silent} column (with \code{success = FALSE} and no output
#'   written), and a warning names them. The single-pass default touches no
#'   binary under \code{run = FALSE}.
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}). Under
#'   \code{two_pass = TRUE} this gates only the correction pass; the analysis
#'   pass runs regardless (see \code{two_pass}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: normalize in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one input at a time
#'   and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}). Under \code{two_pass = TRUE}
#'   the result also carries the five measured columns (\code{measured_I} etc.)
#'   and a logical \code{silent} column, and the \code{command} column holds the
#'   linear correction commands (\code{NA} for silent rows, which carry \code{NA}
#'   measurements and are not normalized). The two-pass result's schema is
#'   independent of how many rows are silent: the opt-in \code{verified} column
#'   (under \code{verify}) and provenance manifest (under \code{manifest}, read
#'   with \code{\link{ffm_manifest}}) are present whenever requested, even when
#'   \emph{every} row is silent -- silent rows simply carry \code{NA} for those
#'   outputs.
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' @seealso [normalize_audio()] for the single-input form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [standardize_video_batch()] for the video-side table-driven sibling.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input           = c(video, video),
#'   output          = c("a.mp4", "b.mp4"),
#'   target_loudness = c(-23, -16)
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' normalize_audio_batch(jobs, run = FALSE)
#' # Accurate two-pass (measured/linear) normalization across the whole table
#' # (runs FFmpeg to measure each input, so needs the binary):
#' \dontrun{
#' normalize_audio_batch(jobs, two_pass = TRUE)
#' }
#' @export
normalize_audio_batch <- function(jobs, target_loudness = -23, true_peak = -1,
                             loudness_range = 7, channels = NULL,
                             sample_rate = NULL, audio_codec = NULL,
                             two_pass = FALSE, run = TRUE,
                             parallel = FALSE, ...) {

  rlang::check_bool(two_pass)
  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with standardize_video_batch()).
  jobs$input <- as.character(jobs$input)

  # Validate present override columns up front so a bad column fails clearly
  # here rather than as an opaque FFmpeg error mid-batch (M11 parity lesson).
  # Value-level checks (loudness ranges, whole channels/sample_rate) are
  # inherited per row from normalize_audio_pipeline()'s ffm_loudnorm() and
  # check_number_whole() guards.
  knob_cols <- c("target_loudness", "true_peak", "loudness_range",
                 "channels", "sample_rate")
  for (col in intersect(knob_cols, names(jobs))) {
    if (!is.numeric(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric.")
    }
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  # audio_codec is a codec column, not a numeric knob: NA is legal and spells
  # the NULL sentinel, so it needs check_batch_codec_col(), never the numeric
  # guard above (M34/M35). Refuse "copy" from the argument and from any cell up
  # front, so two-pass fails before Phase 1 wastes an analysis pass per row.
  check_batch_codec_col(jobs, "audio_codec")
  check_audio_codec_not_copy(audio_codec)
  if ("audio_codec" %in% names(jobs)) check_audio_codec_not_copy(jobs$audio_codec)

  # Auto-name outputs when the column is absent. One input -> one output, so a
  # duplicated input with no explicit output would map to the same file; reject
  # that rather than silently overwrite (parity with standardize_video_batch()).
  if (!"output" %in% names(jobs)) {
    dupes <- unique(jobs$input[duplicated(jobs$input)])
    if (length(dupes) > 0) {
      cli::cli_abort(c(
        "{.arg jobs} has duplicated {.field input} paths but no {.field output} column.",
        "x" = "Duplicated input{?s}: {.val {dupes}}.",
        "i" = "Add an {.field output} column to name each row's destination."
      ))
    }
    jobs$output <- derive_normalized_names(jobs$input)
  }

  # Two-pass (measured/linear): the audio-side M16 analyze-then-build path fanned
  # across the jobs table (D013). Phase 1 measures every input (honoring
  # `parallel`) and appends the five measured columns; Phase 2 builds & runs one
  # linear correction per row from them. Fail-fast before Phase 2 if any row's
  # analysis did not yield a usable measurement (assemble_measured names the
  # row). Like the scalar verb, `run = FALSE` still runs Phase 1 (it needs the
  # binary and readable inputs) and gates only the Phase 2 correction commands.
  if (two_pass) {
    # Validate the shaping knobs up front (parity with the scalar two-pass verb)
    # so a bad channels/sample_rate fails before Phase 1 wastes an analysis pass
    # per row; per-value target checks stay per-row in the Phase 2 pipeline.
    rlang::check_number_whole(channels, min = 1, allow_null = TRUE)
    rlang::check_number_whole(sample_rate, min = 1, allow_null = TRUE)
    # Same reason, for the encoder name: a malformed token would otherwise abort
    # from apply_audio_codec() in Phase 2, after Phase 1 has already analyzed
    # every row. The argument and every non-NA cell are checked here.
    if (!is.null(audio_codec)) check_token(audio_codec)
    if ("audio_codec" %in% names(jobs)) {
      cells <- jobs$audio_codec[!is.na(jobs$audio_codec)]
      for (cell in cells) check_token(cell)
    }
    for (col in intersect(c("channels", "sample_rate"), names(jobs))) {
      if (any(jobs[[col]] %% 1 != 0) || any(jobs[[col]] < 1)) {
        cli::cli_abort(
          "The {.field {col}} column of {.arg jobs} must be whole numbers \\
           ({.val {1}} or greater) for two-pass normalization."
        )
      }
    }
    col_or <- function(nm, default) {
      if (nm %in% names(jobs)) jobs[[nm]] else rep(default, nrow(jobs))
    }
    outputs <- run_loudnorm_analysis_batch(
      jobs$input,
      col_or("target_loudness", target_loudness),
      col_or("true_peak", true_peak),
      col_or("loudness_range", loudness_range),
      parallel
    )
    # Continue-and-mark on silence (M18): a silent input (input_i = -inf) cannot
    # be normalized to a loudness target, but one silent row must not abort the
    # whole batch. assemble_measured() sets silent rows aside (measured cols NA)
    # and flags them; genuine failures still abort. Correct only the non-silent
    # rows, warn about the silent ones, and reassemble in original row order.
    measured <- assemble_measured(outputs)
    silent <- measured$silent
    for (nm in names(measured$measured)) jobs[[nm]] <- measured$measured[[nm]]
    if (any(silent)) {
      # Drive pluralization off the scalar {length(rows)} and list the row
      # indices without a `{?s}` marker: a `{?s}` governed by a `{.val
      # {vector}}` across cli_warn() message elements throws
      # `length(object) == 1` (M18 review).
      rows <- which(silent)
      cli::cli_warn(c(
        "Found {length(rows)} silent input{?s} that cannot be normalized to a \\
         loudness target.",
        "!" = "Affected rows (1-indexed): {.val {rows}}.",
        "i" = "Silent rows are marked in the {.field silent} column \\
               ({.field success} = {.val {FALSE}}, no output written)."
      ))
    }
    ok_res <- if (any(!silent)) {
      run_normalize_correction(
        jobs[!silent, , drop = FALSE], target_loudness, true_peak,
        loudness_range, channels, sample_rate, audio_codec, run = run,
        parallel = parallel, ...
      )
    } else {
      NULL
    }
    # Thread the opt-in intent (verify/manifest/checksums, forwarded via `...`)
    # so an all-silent batch synthesizes the same schema a mixed one produces
    # (D011); a mixed batch reads those from ok_res and ignores these.
    dots <- list(...)
    return(bind_two_pass_result(
      jobs, silent, ok_res, run,
      verify = !is.null(dots$verify),
      manifest = isTRUE(dots$manifest),
      checksums = isTRUE(dots$checksums)
    ))
  }

  # The scalar argument needs its own front-door check, and the column guard
  # above cannot stand in for it: batch_codec_cell() maps a scalar NA to the
  # NULL sentinel exactly as it maps an NA cell, so `audio_codec = NA` used to
  # compile the default command in silence rather than erroring -- the one
  # codec argument in the package that did (M41). allow_null because NULL is
  # this verb's documented sentinel (D019).
  #
  # Placed at the END of this verb's front-door validation, not beside the
  # other scalar checks: before M41 this argument was only read per row
  # inside the fan-out, so EVERY check above it reported first on a call
  # that was wrong about two things at once. Moving the guard up the
  # function silently reassigned that precedence -- first past the jobs
  # SHAPE block (review A6), then past its content checks too (review
  # A1r3). Here it changes nothing but the message a bad codec gets.
  rlang::check_string(audio_codec, allow_null = TRUE)

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output loudnorm
  # pipeline per row, sharing normalize_audio_pipeline() with normalize_audio().
  # A per-row knob column (arriving via `...` from pmap) overrides the scalar
  # arg of the same name; `...` also forwards ffm_batch options
  # (verify/manifest/...) to the runner, never to the pipeline builder (the
  # runner's params sit after `...` and bind by name — M09 lesson).
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      normalize_audio_pipeline(
        input, output,
        target_loudness = pick("target_loudness", target_loudness),
        true_peak = pick("true_peak", true_peak),
        loudness_range = pick("loudness_range", loudness_range),
        channels = pick("channels", channels),
        sample_rate = pick("sample_rate", sample_rate),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec))
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# Batch jobs-table guards (shared by the M28 single-in/out batch verbs) --------

# Validate the common jobs-table contract and return `jobs` with `input` (and,
# when present, `output`) coerced to character. `require_output = TRUE` demands
# an explicit `output` column (audio verbs, whose extension is the instruction
# and cannot be auto-named); `verb` names the operation in that error. A factor
# path column carries paths as levels, so coerce to character (parity with the
# other *_batch verbs). Value-level per-row checks stay in the shared pipelines.
check_batch_jobs <- function(jobs, require_output = FALSE, verb = NULL,
                             call = rlang::caller_env()) {
  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.", call = call)
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.", call = call)
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ), call = call)
  }
  jobs$input <- as.character(jobs$input)
  if (anyNA(jobs$input)) {
    cli::cli_abort("The {.field input} column of {.arg jobs} must not contain {.val {NA}}.", call = call)
  }

  if (require_output && !"output" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field output} column.",
      "x" = "Missing column: {.val output}.",
      "i" = "{verb} can't derive an output container; name each destination."
    ), call = call)
  }
  if ("output" %in% names(jobs)) {
    jobs$output <- as.character(jobs$output)
    if (anyNA(jobs$output)) {
      cli::cli_abort("The {.field output} column of {.arg jobs} must not contain {.val {NA}}.", call = call)
    }
  }
  jobs
}

# Reject any two rows that resolve to the same output path — not just duplicated
# inputs — so an explicit `output` repeated across rows can't silently overwrite
# either (M26). Assumes `jobs$output` is resolved (present or derived).
reject_duplicate_outputs <- function(jobs, call = rlang::caller_env()) {
  dupes <- unique(jobs$output[duplicated(jobs$output)])
  if (length(dupes) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} has rows that resolve to the same output path.",
      "x" = "Colliding output{?s}: {.val {dupes}}.",
      "i" = "Give each row a distinct {.field output}, or de-duplicate the inputs."
    ), call = call)
  }
  jobs
}

# Guard an optional per-row video_codec column (M34/D016). Unlike
# check_batch_string_col(), NA is legal: it is the column form of
# video_codec = NULL, the "leave the codec alone" sentinel. An all-NA column is
# typed logical by R, so accept that too (the audio-column pattern); any other
# type (e.g. numeric) is rejected up front rather than mid-batch.
#
# `na_means` states what NA spells on the CALLING verb, because the hint must be
# true under the branch that fired the guard (M38 lesson). It is "leave the
# codec unset" on every verb whose NULL is D016's emit-nothing sentinel, but
# convert_audio_batch() is the one caller where NULL/NA selects `-q:a 0`
# instead (D021), so that verb overrides the wording rather than shipping a hint
# that contradicts its own documentation.
check_batch_codec_col <- function(jobs, col = "video_codec",
                                  na_means = "leave the codec unset",
                                  call = rlang::caller_env()) {
  # Legal: a character column (NA cells allowed), or the all-NA column R types
  # as logical. Testing `all(is.na(.))` alone would also admit an all-NA numeric
  # or an all-NA Date, which the contract above says is rejected; testing
  # `is.logical(.)` alone would admit a c(TRUE, FALSE) column.
  ok <- function(x) is.character(x) || (is.logical(x) && all(is.na(x)))
  if (col %in% names(jobs) && !ok(jobs[[col]])) {
    cli::cli_abort(
      "The {.field {col}} column of {.arg jobs} must be character
       ({.val {NA}} to {na_means}).",
      call = call
    )
  }
  invisible(jobs)
}

# Resolve a per-row video_codec cell to the scalar the pipelines take: NA is the
# column form of the NULL sentinel (M34/D016).
batch_codec_cell <- function(value) {
  if (length(value) == 1L && is.na(value)) NULL else value
}

# check_batch_audio_col(): type-guard a numeric stream-index column up front.
# Legal: a numeric column (NA cells allowed), or the all-NA column R types as
# logical. The same spelled-out shape check_batch_codec_col() uses, and for the
# same reason: testing `all(is.na(.))` alone would admit an all-NA character or
# Date column, while testing `is.logical(.)` alone would admit c(TRUE, FALSE)
# (M35, M34 lesson). Shared by compare_videos_batch() and
# picture_in_picture_batch(), which drifted apart before M35 -- compare had no
# up-front guard at all -- and by the two audio verbs' `audio_stream` (M43).
#
# `col` and `na_means` are parameters because M43 added a caller whose NA means
# something else: on the composite verbs an NA `audio` cell drops audio, while
# an NA `audio_stream` cell leaves that row on the first audio track. The
# inherited wording would be false for the new caller -- the failure M40 hit by
# ADDING a caller to a shared guard rather than by writing a wrong branch.
check_batch_audio_col <- function(jobs, col = "audio",
                                  na_means = "drop audio",
                                  call = rlang::caller_env()) {
  ok <- function(x) is.numeric(x) || (is.logical(x) && all(is.na(x)))
  if (col %in% names(jobs) && !ok(jobs[[col]])) {
    cli::cli_abort(
      "The {.field {col}} column of {.arg jobs} must be numeric
       ({.val {NA}} to {na_means}).",
      call = call
    )
  }
  invisible(jobs)
}

# Resolve a per-row `audio_stream` cell to the scalar the pipelines take: NA is
# the column form of the NULL sentinel, i.e. "leave this row on the first audio
# track" -- NOT "fall back to the argument", which is what an ABSENT column
# means (M43; the same shape batch_codec_cell() gives the codec columns, kept
# separate so the codec helper stays about codecs).
batch_stream_cell <- function(value) {
  if (length(value) == 1L && is.na(value)) NULL else value
}

# Guard an optional string override column: present -> character, no NA.
check_batch_string_col <- function(jobs, col, call = rlang::caller_env()) {
  if (col %in% names(jobs)) {
    if (!is.character(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort(
        "The {.field {col}} column of {.arg jobs} must be character (no {.val {NA}}).",
        call = call
      )
    }
  }
  invisible(jobs)
}

# Validate a fan-in jobs table (D015): unlike the scalar-input `input` column of
# check_batch_jobs(), the many-inputs -> one-output verbs carry their per-row
# inputs in an `inputs` LIST-COLUMN (each cell a character vector of >= min_inputs
# paths) alongside a scalar `output` column. purrr::pmap passes each list cell to
# .f row-wise, so ffm_batch needs no change. Coerces `output` to character;
# leaves duplicate-path rejection to reject_duplicate_outputs (called after).
check_fanin_jobs <- function(jobs, min_inputs = 1L, verb = NULL,
                             call = rlang::caller_env()) {
  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per output.", call = call)
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.", call = call)
  }
  if (!"inputs" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field inputs} list-column.",
      "x" = "Missing column: {.val inputs}.",
      "i" = "Each cell is a character vector of input paths for that output."
    ), call = call)
  }
  if (!"output" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field output} column.",
      "x" = "Missing column: {.val output}.",
      "i" = "{verb} writes one output per row; name each destination."
    ), call = call)
  }
  if (!is.list(jobs$inputs) || is.data.frame(jobs$inputs)) {
    cli::cli_abort(c(
      "The {.field inputs} column of {.arg jobs} must be a list-column.",
      "i" = "Build it with e.g. {.code tibble::tibble(inputs = list(c(a, b)), output = o)}."
    ), call = call)
  }
  ok <- vapply(
    jobs$inputs,
    function(x) is.character(x) && length(x) >= min_inputs && !anyNA(x),
    logical(1)
  )
  if (!all(ok)) {
    # Drive pluralization off the scalar count and list the offending rows
    # without a `{?s}` governed by the numeric `{.val {vector}}`, which cli reads
    # as a quantity and throws `length(object) == 1` on with 2+ items (M18
    # review; see the loudnorm silent-input guard above for the same fix).
    bad <- which(!ok)
    cli::cli_abort(c(
      "Each {.field inputs} cell must be a character vector of {min_inputs} or \\
       more paths with no {.val {NA}}.",
      "x" = "Found {length(bad)} invalid cell{?s} at row{?s} (1-indexed): {.val {bad}}."
    ), call = call)
  }
  jobs$output <- as.character(jobs$output)
  if (anyNA(jobs$output)) {
    cli::cli_abort("The {.field output} column of {.arg jobs} must not contain {.val {NA}}.", call = call)
  }
  jobs
}


# extract_audio_batch() ---------------------------------------------------

#' Extract Audio From Many Files From a Jobs Table
#'
#' Pull the audio track out of many input files from a single jobs tibble — the
#' **batch** (table-driven) sibling of [extract_audio()] for when you have more
#' than one file. Each row is one input; \code{input} and \code{output} columns
#' are required. This is a thin wrapper over \code{\link{ffm_batch}}: one
#' reproducible compiled command per input, sharing the same map/drop-video
#' pipeline as the scalar verb.
#'
#' When a row names no \code{audio_stream} and its input turns out to carry
#' tracks the output will not, the verb warns \strong{once} for the whole batch,
#' naming every affected row. That check is \strong{best-effort}: it runs
#' FFprobe, so it is emitted when FFprobe is available and the input can be
#' probed, and is skipped silently otherwise. It never runs under \code{run =
#' FALSE}, never changes any compiled command, and is skipped entirely when
#' every row names a track. Suppress it by class with
#' \code{suppressWarnings(classes = "tidymedia_dropped_audio")}.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) and an \code{output} column (destination
#'   path). An \code{output} column is **required** — unlike the video batch
#'   verbs, an audio destination cannot be auto-named because its extension is
#'   the instruction (it picks the container, and with \code{audio_codec =
#'   "copy"} must match the source codec). An optional \code{audio_codec} column
#'   overrides the \code{audio_codec} argument per row; rows omitting it fall
#'   back to the argument, and \code{NA} in a cell leaves that row's codec unset
#'   (the column form of \code{audio_codec = NULL}). An optional
#'   \code{audio_stream} column likewise overrides the \code{audio_stream}
#'   argument per row, where \code{NA} keeps that row on the first audio track.
#'   Any other columns are ignored.
#' @param audio_codec The audio codec applied to every row unless \code{jobs}
#'   carries an \code{audio_codec} column, in which case \code{NA} in a cell
#'   leaves that row's codec unset. \code{"copy"} (default) stream-copies the
#'   audio losslessly; name an encoder (e.g. \code{"aac"}) to transcode; or pass
#'   \code{NULL} to emit no \code{-codec:a} and let the output container's
#'   default encoder decide.
#' @param audio_stream The 0-based index of the audio track to take, applied to
#'   every row unless \code{jobs} carries an \code{audio_stream} column, in
#'   which case \code{NA} in a cell keeps that row on the first audio track.
#'   The index counts \emph{among each input's audio streams}. \code{NULL}
#'   (default) takes the first audio track.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [extract_audio()], the scalar verb it wraps; [ffm_batch()], the batch
#'   runner; [convert_audio_batch()] to transcode audio in batch.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.aac", "b.aac"))
#' extract_audio_batch(jobs, run = FALSE)
#' @export
extract_audio_batch <- function(jobs, audio_codec = "copy",
                                audio_stream = NULL, run = TRUE,
                                parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, require_output = TRUE, verb = "Audio extraction")
  jobs <- reject_duplicate_outputs(jobs)
  # check_batch_codec_col(), never check_batch_string_col(), which rejects NA
  # and so leaves the column unable to spell the "unset" its own argument can
  # say with NULL (D022). This was the third and last codec column on the wrong
  # guard.
  check_batch_codec_col(jobs, "audio_codec")
  # Without this, a non-string audio_codec reached ffm_codec() per row and
  # aborted inside purrr::pmap() naming Layer-1's `audio` -- the only pair in the
  # package that leaked the engine's name, fired mid-fan-out, AND blamed pmap all
  # at once (M41).
  #
  # allow_null carries D022's family rule: NULL emits no -codec:a. The scalar
  # sibling now agrees; until M42 it aborted on this same call, which is the
  # split D021 recorded from the wrong side (it called extract_audio() the verb
  # that "accepts neither NULL nor NA" without noticing this one always had).
  rlang::check_string(audio_codec, allow_null = TRUE)
  # The stream-index column's own type guard, with a hint saying what NA means
  # HERE -- the shared default ("drop audio") belongs to the composite verbs and
  # would be false on this one (M40).
  check_batch_audio_col(jobs, "audio_stream",
                        na_means = "keep the first audio track")
  # And the scalar argument's front-door check: the column path resolves NA to
  # the NULL sentinel, so without this `audio_stream = NA` would quietly compile
  # track 0 instead of erroring (the M37/M41 shape). Per-row VALUES are checked
  # again inside audio_stream_map(), which every row's pipeline calls (M32).
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE)


  # D024's diagnostic probe, up front so it lands before the fan-out encodes;
  # ffm_batch() itself is untouched. isTRUE() rather than a bare `run` so a
  # non-logical value still gets ffm_batch()'s own check_bool() message.
  if (isTRUE(run)) warn_dropped_audio_batch(jobs, audio_stream)

  # Thin Layer-2 fan-out over ffm_batch (D007): one map/drop-video pipeline per
  # row, sharing extract_audio_pipeline() with extract_audio(). A per-row
  # `audio_codec` or `audio_stream` column (via `...` from pmap) overrides the
  # scalar arg; `...` also forwards ffm_batch options to the runner.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      extract_audio_pipeline(
        input, output,
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        audio_stream = batch_stream_cell(pick("audio_stream", audio_stream))
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# convert_audio_batch() ---------------------------------------------------

#' Convert the Audio of Many Files From a Jobs Table
#'
#' Extract or transcode the audio track of many input files from a single jobs
#' tibble — the **batch** (table-driven) sibling of [convert_audio()] for when
#' you have more than one file. Each row is one input; \code{input} and
#' \code{output} columns are required. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same audio-map pipeline (and per-value \code{audio_codec} validation) as
#' the scalar verb.
#'
#' When a row names no \code{audio_stream} and its input turns out to carry
#' tracks the output will not, the verb warns \strong{once} for the whole batch,
#' naming every affected row. That check is \strong{best-effort}: it runs
#' FFprobe, so it is emitted when FFprobe is available and the input can be
#' probed, and is skipped silently otherwise. It never runs under \code{run =
#' FALSE}, never changes any compiled command, and is skipped entirely when
#' every row names a track. Suppress it by class with
#' \code{suppressWarnings(classes = "tidymedia_dropped_audio")}.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) and an \code{output} column (destination
#'   path). An \code{output} column is **required** — an audio destination
#'   cannot be auto-named because its extension picks the output format. An
#'   optional \code{audio_codec} column overrides the \code{audio_codec}
#'   argument per row, where \code{NA} spells "use the highest-VBR-quality
#'   default"; rows omitting it fall back to the argument. An optional
#'   \code{audio_stream} column likewise overrides the \code{audio_stream}
#'   argument per row, where \code{NA} keeps that row on the first audio track.
#'   Any other columns are
#'   ignored — except a \code{format} column, retired with the argument of the
#'   same name, which is an error rather than a silent no-op.
#' @param audio_codec The output audio codec applied to every row unless
#'   \code{jobs} carries an \code{audio_codec} column. \code{NULL} (default)
#'   infers the codec from each \code{output} extension at highest VBR quality;
#'   name a codec (e.g. \code{"aac"}, \code{"flac"}) to pin \code{-c:a}.
#' @param audio_stream The 0-based index of the audio track to take, applied to
#'   every row unless \code{jobs} carries an \code{audio_stream} column, in
#'   which case \code{NA} in a cell keeps that row on the first audio track.
#'   The index counts \emph{among each input's audio streams}. \code{NULL}
#'   (default) takes the first audio track.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [convert_audio()], the scalar verb it wraps; [ffm_batch()], the batch
#'   runner; [extract_audio_batch()] to stream-copy audio in batch.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.mp3", "b.mp3"))
#' convert_audio_batch(jobs, run = FALSE)
#' @export
convert_audio_batch <- function(jobs, audio_codec = NULL,
                                audio_stream = NULL, run = TRUE,
                                parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, require_output = TRUE, verb = "Audio conversion")
  jobs <- reject_duplicate_outputs(jobs)

  # M40 renamed `format` to `audio_codec` in BOTH its spellings. The scalar
  # sibling has no `...`, so R rejects a stale call itself; here `...` forwards
  # ffm_batch options and would swallow a stale argument, while a stale `jobs`
  # column would fall through as one of the "other columns are ignored" -- either
  # way silently ignoring the codec the caller named. Name the replacement
  # instead of ignoring it -- a diagnostic, not a lifecycle shim (D014's clean
  # break stands; M37's precedent).
  stale <- c(
    if ("format" %in% names(list(...))) "argument",
    if ("format" %in% names(jobs)) "jobs column"
  )
  if (length(stale) > 0) {
    cli::cli_abort(c(
      "The {.arg format} {stale} was removed from {.fn convert_audio_batch}.",
      "i" = "Use {.arg audio_codec} instead; it takes the same encoder names.",
      "i" = "In {.arg jobs}, name it as an {.field audio_codec} column, where
             {.val {NA}} keeps that row on the highest-VBR-quality default."
    ))
  }

  # NA is legal in the column: it is the column form of audio_codec = NULL, so
  # this needs check_batch_codec_col(), never check_batch_string_col(), which
  # rejects NA and so cannot spell "unset" (M34/D016). The hint is overridden
  # because on THIS verb NA selects `-q:a 0` rather than leaving the codec unset
  # (D021) -- the shared default would be false here (M38 lesson).
  check_batch_codec_col(jobs, "audio_codec",
                        na_means = "use the highest-VBR-quality default")
  # The scalar argument is resolved through batch_codec_cell() below, which maps
  # NA to the NULL sentinel -- so without this front-door check `audio_codec =
  # NA` would quietly compile the default instead of erroring, the M37 shape
  # where a scalar arg reaching the pipeline by the column path skips its own
  # type check. NULL stays legal: it IS the sentinel.
  if (!is.null(audio_codec)) rlang::check_string(audio_codec)
  # The stream-index column's own type guard, with a hint saying what NA means
  # HERE -- the shared default ("drop audio") belongs to the composite verbs and
  # would be false on this one (M40).
  check_batch_audio_col(jobs, "audio_stream",
                        na_means = "keep the first audio track")
  # And the scalar argument's front-door check: the column path resolves NA to
  # the NULL sentinel, so without this `audio_stream = NA` would quietly compile
  # track 0 instead of erroring (the M37/M41 shape). Per-row VALUES are checked
  # again inside audio_stream_map(), which every row's pipeline calls (M32).
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE)


  # D024's diagnostic probe, up front so it lands before the fan-out encodes;
  # ffm_batch() itself is untouched. isTRUE() rather than a bare `run` so a
  # non-logical value still gets ffm_batch()'s own check_bool() message.
  if (isTRUE(run)) warn_dropped_audio_batch(jobs, audio_stream)

  # Thin Layer-2 fan-out over ffm_batch (D007): one audio-map pipeline per row,
  # sharing convert_audio_pipeline() with convert_audio(). A per-row
  # `audio_codec` or `audio_stream` column overrides the scalar arg; the
  # per-value check_string(audio_codec) and check_number_whole(audio_stream) are
  # inherited from the shared pipeline.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      convert_audio_pipeline(
        input, output,
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        audio_stream = batch_stream_cell(pick("audio_stream", audio_stream))
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_cropped_names() / derive_web_names() -----------------------------

# Derive one output path per input for the video batch verbs when the `output`
# column is absent. crop keeps the source container (`<base>_cropped.<ext>`);
# the web re-encode always writes H.264/mp4 (`<base>_web.mp4`). The base keeps
# the input's directory, so inputs in different folders never collide; the batch
# verbs reject any duplicated *resolved* output up front (M26), so these stay
# pure name maps.
derive_cropped_names <- function(input) {
  paste0(tools::file_path_sans_ext(input), "_cropped.", tools::file_ext(input))
}

derive_web_names <- function(input) {
  paste0(tools::file_path_sans_ext(input), "_web.mp4")
}


# crop_video_batch() ------------------------------------------------------

#' Crop Many Videos From a Jobs Table
#'
#' Crop many input videos to a rectangular region from a single jobs tibble —
#' the **batch** (table-driven) sibling of [crop_video()] for when you have more
#' than one file. Each row is one input. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same crop pipeline (and its per-value dimension guards) as the scalar
#' verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_cropped} to each input's basename, keeping the input's extension
#'   (e.g. \code{clip.mp4} becomes \code{clip_cropped.mp4}). Each crop dimension
#'   — \code{width}, \code{height}, \code{x}, \code{y} — may also appear as a
#'   column to override the corresponding argument per row; rows (or dimensions)
#'   omitting the column fall back to the argument. A \code{video_codec} column
#'   overrides that argument per row, with \code{NA} meaning "leave the codec
#'   unset" (the column's way of writing the argument's \code{NULL}); an
#'   \code{audio_codec} column works the same way. Any two
#'   rows that resolve to the same output path are rejected. Any other columns
#'   are ignored.
#' @param width,height The output crop size in pixels, applied to every row
#'   unless \code{jobs} carries a column of the same name. Required: pass each as
#'   an argument or supply the column (there is no default crop size).
#' @param x,y The offset in pixels of the crop's left/top edge, applied to every
#'   row unless \code{jobs} carries a column of the same name. Default: centered.
#' @param video_codec A string naming the output video codec, applied to every
#'   row lacking a \code{video_codec} column, or \code{NULL} (default) to leave
#'   it unset so each output keeps its container's default encoder.
#' @param audio_codec A string naming the output audio codec, applied to every
#'   row lacking an \code{audio_codec} column. \code{"copy"} (default)
#'   stream-copies the audio; name an encoder to transcode it, or \code{NULL} to
#'   leave the codec unset so each output keeps its container's default encoder.
#' @param hardware,fallback The encoder backend and its fallback behavior,
#'   applied to the whole batch (a property of the machine, not of a row, so
#'   neither is read as a \code{jobs} column). See [crop_video()].
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [crop_video()], the scalar verb it wraps; [ffm_batch()], the batch
#'   runner; [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [standardize_video_batch()] to re-encode in batch.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.mp4", "b.mp4"),
#'                        width = c(160, 80), height = c(120, 60))
#' crop_video_batch(jobs, run = FALSE)
#' @export
crop_video_batch <- function(jobs, width = NULL, height = NULL,
                             x = "(in_w-out_w)/2", y = "(in_h-out_h)/2",
                             video_codec = NULL, audio_codec = "copy",
                             hardware = c("none", "nvenc"), fallback = FALSE,
                             run = TRUE, parallel = FALSE, ...) {

  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  jobs <- check_batch_jobs(jobs, require_output = FALSE)

  # width/height have no default: each must be resolvable as an argument (applied
  # to every row) or a per-row column, else fail here rather than as an opaque
  # FFmpeg error mid-batch.
  for (dim in c("width", "height")) {
    if (is.null(get(dim)) && !dim %in% names(jobs)) {
      cli::cli_abort(c(
        "{.arg {dim}} is required.",
        "i" = "Pass {.arg {dim}} (applied to every row) or add a {.field {dim}} column."
      ))
    }
  }
  # Validate present override columns up front; per-value checks (positive
  # dimensions / valid expressions) are inherited per row from ffm_crop().
  for (col in intersect(c("width", "height", "x", "y"), names(jobs))) {
    if (!(is.numeric(jobs[[col]]) || is.character(jobs[[col]]))) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric or character.")
    }
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  check_batch_codec_col(jobs)
  check_batch_codec_col(jobs, "audio_codec")

  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_cropped_names(jobs$input)
  }
  jobs <- reject_duplicate_outputs(jobs)

  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      crop_video_pipeline(
        input, output,
        width = pick("width", width),
        height = pick("height", height),
        x = pick("x", x),
        y = pick("y", y),
        video_codec = batch_codec_cell(pick("video_codec", video_codec)),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        hardware = hardware,
        fallback = fallback
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# format_for_web_batch() --------------------------------------------------

#' Re-encode Many Videos for the Web From a Jobs Table
#'
#' Re-encode many input videos into a widely compatible, web-friendly form from
#' a single jobs tibble — the **batch** (table-driven) sibling of
#' [format_for_web()] for when you have more than one file. Each row is one
#' input. This is a thin wrapper over \code{\link{ffm_batch}}: one reproducible
#' compiled command per input, sharing the same fixed H.264/AAC/\code{+faststart}
#' pipeline as the scalar verb (no per-row knobs).
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_web} to each input's basename with an \code{.mp4} extension (the web
#'   re-encode always writes H.264/mp4), e.g. \code{clip.mkv} becomes
#'   \code{clip_web.mp4}. Any two rows that resolve to the same output path are
#'   rejected. Any other columns are ignored — including \code{video_codec} and
#'   \code{audio_codec}, which the sibling batch verbs read as per-row overrides
#'   but this one does not: the web recipe fixes both codecs by identity (H.264
#'   video, AAC audio). For per-row codecs use a verb that exposes them, such as
#'   \code{\link{standardize_video_batch}} or \code{\link{crop_video_batch}}.
#' @param hardware The encoder backend applied to every row: \code{"none"}
#'   (default, software libx264) or \code{"nvenc"} for NVIDIA GPU H.264 encoding.
#'   Batch-wide (not a per-row column). See \code{\link{has_nvenc}}.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, re-encode with software libx264 and a message (\code{TRUE})
#'   instead of aborting (\code{FALSE}, default).
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [format_for_web()], the scalar verb it wraps; [ffm_batch()], the
#'   batch runner; [standardize_video_batch()] for a configurable re-encode.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.mp4", "b.mp4"))
#' format_for_web_batch(jobs, run = FALSE)
#' @export
format_for_web_batch <- function(jobs, hardware = c("none", "nvenc"),
                                 fallback = FALSE, run = TRUE, parallel = FALSE,
                                 ...) {

  jobs <- check_batch_jobs(jobs, require_output = FALSE)
  hardware <- rlang::arg_match(hardware)

  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_web_names(jobs$input)
  }
  jobs <- reject_duplicate_outputs(jobs)

  # Thin Layer-2 fan-out over ffm_batch (D007): one web re-encode pipeline per
  # row, sharing format_for_web_pipeline() with format_for_web(). hardware/
  # fallback are batch-wide; other extra job columns are ignored.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      format_for_web_pipeline(input, output, hardware, fallback)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# separate_audio_video_batch() --------------------------------------------

#' Separate Audio and Video for Many Files From a Jobs Table
#'
#' Split the audio and video streams of many input files from a single jobs
#' tibble — the **batch** (table-driven) sibling of [separate_audio_video()] for
#' when you have more than one file. Each row is one input that fans out into
#' **two** outputs; \code{input}, \code{audiofile}, and \code{videofile} columns
#' are all required. This is a thin wrapper over \code{\link{ffm_batch}}: every
#' input row is reshaped into two single-output jobs (one per stream), so a jobs
#' table of \code{N} rows returns \code{2N} rows — one reproducible compiled
#' command per stream — sharing the same per-stream map/stream-copy pipeline as
#' the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) plus \code{audiofile} and \code{videofile}
#'   columns naming the two destinations. All three are **required** — like
#'   \code{\link{separate_audio_video}}, this verb derives no output paths,
#'   because a copied stream's container extension is the instruction (it must
#'   match the source codec). Optional \code{audio_codec} and \code{video_codec}
#'   columns (character; \code{NA} to emit no codec option for that stream)
#'   override the arguments of the same name per row; rows omitting a column fall
#'   back to that argument. An optional numeric \code{audio_stream} column
#'   (\code{NA} to keep every audio track in that row's \code{audiofile})
#'   likewise overrides the \code{audio_stream} argument per row. Any other
#'   columns are ignored — except a \code{reencode} column, retired with the
#'   argument of the same name, which is an error rather than a silent no-op.
#' @param audio_codec A string naming the encoder for every \code{audiofile}
#'   unless \code{jobs} carries an \code{audio_codec} column. The default
#'   \code{"copy"} stream-copies the audio losslessly; \code{NULL} emits no
#'   \code{-codec:a}. See \code{\link{separate_audio_video}}.
#' @param video_codec A string naming the encoder for every \code{videofile}
#'   unless \code{jobs} carries a \code{video_codec} column. The default
#'   \code{"copy"} stream-copies the video losslessly; \code{NULL} emits no
#'   \code{-codec:v}. See \code{\link{separate_audio_video}}.
#' @param hardware,fallback The encoder backend for every \code{videofile} and
#'   its fallback behavior, applied to the whole batch (a property of the
#'   machine, not of a row, so neither is read as a \code{jobs} column). See
#'   [separate_audio_video()]. Because \code{hardware} is batch-wide, and a
#'   stream copy runs no encoder, \code{hardware = "nvenc"} conflicts with any
#'   row whose video codec resolves to \code{"copy"} — including the default —
#'   so a jobs table mixing copied and re-encoded video must be split into
#'   separate calls.
#' @param audio_stream The 0-based index of the audio track to write to every
#'   \code{audiofile}, unless \code{jobs} carries an \code{audio_stream} column.
#'   \code{NULL} (default) keeps \strong{every} audio track, as
#'   \code{\link{separate_audio_video}} does; an \code{NA} cell in the column
#'   says the same for that row. Only \code{audiofile} is affected — a
#'   \code{videofile} always takes the input's video streams.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return A [tibble][tibble::tibble-package] with \strong{two rows per input}
#'   (one per stream): the reshaped \code{input}, a single \code{output} path, a
#'   \code{stream} marker (\code{"audio"} or \code{"video"}), and an added
#'   \code{command} column — plus, when \code{run = TRUE}, a \code{success}
#'   column (and \code{verified} / provenance manifest when requested via
#'   \code{...}). When \code{jobs} supplies either codec column, a single
#'   \code{codec} column carries each row's resolved encoder for its own stream
#'   (\code{NA} where none is emitted). When \code{audio_stream} is supplied as
#'   either the argument or a \code{jobs} column, an \code{audio_stream} column
#'   likewise carries each row's resolved track — the selected index on an audio
#'   row, \code{NA} on a video row, which takes no audio. The columns match the
#'   other \code{_batch} verbs' output plus the \code{stream} marker. See
#'   \code{\link{ffm_batch}}.
#' @seealso [separate_audio_video()], the scalar verb it wraps; [ffm_batch()],
#'   the batch runner; [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [segment_video_batch()] for the other fan-out batch verb.
#' @section Failed audio outputs:
#' A row whose \code{audiofile} FFmpeg refuses is recorded as \code{success =
#' FALSE} rather than aborting the batch. When such a row named no
#' \code{audio_stream} and its input carries more than one audio track, the verb
#' warns \strong{once} for the whole batch, naming every affected input row and
#' the ways out. That check runs FFprobe on the failed rows only, so it is
#' emitted when FFprobe is available and the input can be probed, and skipped
#' silently otherwise; it never runs under \code{run = FALSE} and never changes
#' any compiled command. Suppress it with \code{suppressWarnings(classes =
#' "tidymedia_multitrack_separation")}.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input     = c(video, video),
#'   audiofile = c("a1.aac", "a2.aac"),
#'   videofile = c("v1.mp4", "v2.mp4")
#' )
#' # run = FALSE compiles two commands per input without calling FFmpeg
#' separate_audio_video_batch(jobs, run = FALSE)
#' @export
separate_audio_video_batch <- function(jobs, audio_codec = "copy",
                                       video_codec = "copy",
                                       hardware = c("none", "nvenc"),
                                       fallback = FALSE, audio_stream = NULL,
                                       run = TRUE,
                                       parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, verb = "Audio/video separation")
  # Batch-wide, never a jobs column: nvenc availability is a property of the
  # machine, not of a file (D016). Resolved here for the same reason as the
  # scalar -- the copy guard compares against "none".
  hardware <- rlang::arg_match(hardware)
  rlang::check_bool(fallback)

  # Two required output columns; this verb derives nothing (parity with the
  # scalar, which requires both audiofile and videofile).
  outcols <- c("audiofile", "videofile")
  missing <- setdiff(outcols, names(jobs))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} must have {.field audiofile} and {.field videofile} columns.",
      "x" = "Missing column{?s}: {.val {missing}}."
    ))
  }
  for (col in outcols) {
    jobs[[col]] <- as.character(jobs[[col]])
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  # M37 removed `reencode` from this verb in BOTH its spellings. The scalar
  # sibling has no `...`, so R rejects a stale call itself; here `...` forwards
  # ffm_batch options and would swallow a stale argument, and the reshape below
  # builds `long` fresh and would drop a stale `jobs` column -- either way
  # silently stream-copying output the caller asked to have re-encoded. Name the
  # replacement instead of ignoring it -- a diagnostic, not a lifecycle shim
  # (D014's clean break stands).
  stale <- c(
    if ("reencode" %in% names(list(...))) "argument",
    if ("reencode" %in% names(jobs)) "jobs column"
  )
  if (length(stale) > 0) {
    cli::cli_abort(c(
      "The {.arg reencode} {stale} was removed from
       {.fn separate_audio_video_batch}.",
      "i" = "Use {.arg audio_codec} / {.arg video_codec} instead:
             {.val copy} replaces {.code reencode = FALSE} and {.code NULL}
             replaces {.code reencode = TRUE}.",
      "i" = "In {.arg jobs}, name them as {.field audio_codec} /
             {.field video_codec} columns, where {.val {NA}} leaves that
             stream's codec unset."
    ))
  }

  # `codec` is the name the reshape below gives its resolved per-stream column,
  # so a caller passing `codec =` would have it forwarded through `...` into the
  # same pmap slot and applied to BOTH streams -- the cross-stream leak the
  # per-stream split exists to make impossible. Every other _batch verb is immune
  # because its per-row column names are also formals; this one's is not, so it
  # is guarded here (M37 review).
  if ("codec" %in% names(list(...))) {
    cli::cli_abort(c(
      "{.arg codec} is not an argument of {.fn separate_audio_video_batch}.",
      "x" = "It names an internal per-stream column and would set the codec on
             the audio and the video output alike.",
      "i" = "Use {.arg audio_codec} / {.arg video_codec} to name each stream's
             encoder separately."
    ))
  }

  # The scalar arguments are materialized into the reshaped column below, which
  # bypasses the per-row token check in the pipeline for their *type*: without
  # this, `video_codec = TRUE` compiled `-codec:v TRUE` and `video_codec = NA`
  # silently emitted nothing whenever `jobs` happened to carry a codec column
  # (M37 review). NULL stays legal -- it is the sentinel.
  if (!is.null(audio_codec)) rlang::check_string(audio_codec)
  if (!is.null(video_codec)) rlang::check_string(video_codec)

  # NA is legal in either codec column: it is the column form of the NULL
  # sentinel, so these need check_batch_codec_col(), never a guard that rejects
  # NA (M34/D016).
  check_batch_codec_col(jobs, "audio_codec")
  check_batch_codec_col(jobs, "video_codec")
  # The stream-index column's own type guard, with a hint saying what NA means
  # HERE: on this verb it keeps EVERY track, where on the extraction verbs the
  # same cell keeps the first one (M40's stale-hint lesson, which is why the
  # wording is a parameter).
  check_batch_audio_col(jobs, "audio_stream",
                        na_means = "keep every audio track")
  # And the argument's front-door check. Load-bearing here, unlike on the scalar
  # sibling: the reshape below materializes this argument into a column whose NA
  # cells mean the NULL sentinel, so without it `audio_stream = NA` would quietly
  # keep every track instead of erroring (the M37/M41 shape).
  rlang::check_number_whole(audio_stream, min = 0, allow_null = TRUE)

  # Reshape N input rows -> 2N single-output rows (D003/D007): each input fans out
  # into an audio row (0:a -> audiofile) and a video row (0:v -> videofile),
  # tagged by a `stream` marker; interleaved audio,video per input. Melting both
  # output columns into one `output` lets a single duplicate-path guard pool
  # across audio and video — and catch within-row audiofile == videofile (M26).
  n <- nrow(jobs)
  long <- tibble::tibble(
    input  = rep(jobs$input, each = 2L),
    output = as.vector(rbind(jobs$audiofile, jobs$videofile)),
    stream = rep(c("audio", "video"), times = n)
  )
  # The two input-side codec columns collapse into ONE `codec` column on the
  # reshaped table, resolved per stream: an audio row carries the audio choice, a
  # video row the video one, so neither can reach the other's command. Each
  # stream falls back to its own argument where `jobs` supplies no column, and
  # the column is added only when `jobs` supplied one — a table naming no codec
  # keeps the pre-M37 shape.
  pick_codec <- function(col, arg) {
    if (col %in% names(jobs)) {
      # An all-NA column is typed logical by R (M34); as.character() carries the
      # NA cells through as the character sentinel the reshape needs.
      as.character(jobs[[col]])
    } else if (is.null(arg)) {
      rep(NA_character_, n)
    } else {
      rep(arg, n)
    }
  }
  if (any(c("audio_codec", "video_codec") %in% names(jobs))) {
    long$codec <- as.vector(rbind(
      pick_codec("audio_codec", audio_codec),
      pick_codec("video_codec", video_codec)
    ))
  }
  # `audio_stream` is one choice per INPUT that applies to the audio row only, so
  # the reshaped column carries each input's value on its audio row and NA on its
  # video row. NA is the no-selection sentinel, and the video branch of
  # separate_stream_pipeline() never reads the value anyway -- two independent
  # reasons a video output cannot be narrowed by this argument. as.numeric()
  # carries an all-NA column, which R types logical (M34).
  if ("audio_stream" %in% names(jobs) || !is.null(audio_stream)) {
    per_input <- if ("audio_stream" %in% names(jobs)) {
      as.numeric(jobs$audio_stream)
    } else {
      rep(as.numeric(audio_stream), n)
    }
    long$audio_stream <- as.vector(rbind(per_input, rep(NA_real_, n)))
  }
  long <- reject_duplicate_outputs(long)

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output pipeline per
  # reshaped row, sharing separate_stream_pipeline() with separate_audio_video().
  # The reshaped `codec` column (via `...` from pmap) already carries the
  # column-over-argument resolution; without it each row takes its stream's
  # argument. `...` also forwards ffm_batch options (verify/manifest/...) to the
  # runner.
  out <- ffm_batch(
    long,
    function(input, output, stream, ...) {
      dots <- list(...)
      codec <- if ("codec" %in% names(dots)) {
        batch_codec_cell(dots$codec)
      } else if (stream == "audio") {
        audio_codec
      } else {
        video_codec
      }
      # Passed on audio rows only: the video command never receives the value, so
      # no column or argument can narrow a video map even by mistake.
      sel <- if (identical(stream, "audio")) {
        batch_stream_cell(
          if ("audio_stream" %in% names(dots)) dots$audio_stream else audio_stream
        )
      }
      separate_stream_pipeline(input, output, stream, codec, hardware, fallback,
                               sel)
    },
    run = run,
    parallel = parallel,
    ...
  )

  # The failed-row diagnostic, after the fan-out rather than before it: which
  # rows failed is not knowable until they run, so a batch where every row
  # succeeds pays nothing (M45-D2).
  if (isTRUE(run)) warn_failed_separation_batch(out, audio_stream)
  out
}


# concatenate_videos() ----------------------------------------------------

# Build the concat-demuxer pipeline shared by concatenate_videos() and its
# _batch sibling (M32): warn on mixed extensions, then ffm_concat() writes the
# demuxer list file and sets copy + map 0. Kept ABOVE the roxygen block so
# document() does not re-target it (M28 lesson).
concatenate_pipeline <- function(infiles, outfile) {
  if (length(unique(tools::file_ext(infiles))) != 1) {
    cli::cli_warn("Not all {.arg infiles} have the same extension.")
  }
  ffm_concat(ffm_files(infiles, outfile))
}

#' Combine video files using the concat demuxer
#'
#' Combine multiple video files one after another without needing to re-encode
#' them by using the [concat
#' demuxer](https://ffmpeg.org/ffmpeg-formats.html#concat-1). This will be much
#' faster than re-encoding but requires that the files have the same parameters
#' (width, height, etc.) and formats/codecs. To concatenate videos using
#' re-encoding, see the [concat video
#' filter](https://ffmpeg.org/ffmpeg-filters.html#concat)
#'
#' @param infiles A character vector containing the file paths to video files.
#' @param outfile A string containing the desired file path to write the new,
#'   concatenated video file to.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_concat()], the builder it wraps.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' concatenate_videos(c(video, video), "joined.mp4", run = FALSE)
#' @export
concatenate_videos <- function(infiles, outfile, run = TRUE) {

  if (!rlang::is_character(infiles)) {
    cli::cli_abort("{.arg infiles} must be a character vector of file paths.")
  }
  rlang::check_string(outfile)

  ffm_finish(concatenate_pipeline(infiles, outfile), run)
}



# compare_videos() --------------------------------------------------------

# Build the side-by-side comparison pipeline shared by compare_videos() and its
# _batch sibling (M32): resize supports exactly two inputs, so guard it here;
# then stack (h/v) and optionally carry one input's audio. Assumes `resize`/
# `audio` already type-checked by the caller; `direction` is arg-matched here so
# both callers get a clean per-value error. ABOVE the roxygen block (M28 lesson).
compare_videos_pipeline <- function(infiles, outfile,
                                    direction = c("horizontal", "vertical"),
                                    resize = TRUE, audio = NULL,
                                    video_codec = NULL, audio_codec = "copy",
                                    hardware = "none",
                                    fallback = FALSE,
                                    call = rlang::caller_env()) {
  direction <- rlang::arg_match(direction)
  # audio_codec configures an encode; with no audio mapped there is no stream to
  # encode, so a named encoder is a contradiction rather than a no-op. NULL stays
  # legal -- it only ever means "emit no -codec:a", which is already the case
  # (M35/D017). IP3/D009 is untouched: the graph and its labels are unchanged.
  if (is.null(audio) && !is.null(audio_codec) &&
      !identical(audio_codec, "copy")) {
    cli::cli_abort(
      c(
        "{.arg audio_codec} needs an audio stream to encode.",
        "x" = "{.code audio = NULL} carries no audio into the output.",
        "i" = "Pass {.arg audio} the 0-based index of the input whose audio to
               keep, or drop {.arg audio_codec}."
      ),
      call = call
    )
  }

  if (resize && length(infiles) != 2) {
    cli::cli_abort(c(
      "{.arg resize} currently supports exactly two inputs.",
      "i" = "Pass {.code resize = FALSE} to compare more than two videos."
    ))
  }
  p <- ffm_files(infiles, outfile)
  p <- switch(
    direction,
    horizontal = ffm_hstack(p, resize = resize),
    vertical = ffm_vstack(p, resize = resize)
  )
  if (!is.null(audio)) {
    p <- ffm_map(p, paste0(audio, ":a"))
    # The carried track is mapped straight through, so the default audio_codec
    # stream-copies it instead of letting the container re-encode it (M35/D017).
    p <- apply_audio_codec(p, audio_codec, call = call)
  }
  # The stacked video is a filtered stream, so a -codec:v rides alongside the
  # -filter_complex … [vout] mapping the blessed stack verbs emit; the default
  # video_codec = NULL emits none (M34/D016).
  apply_video_codec(p, video_codec, hardware, fallback, call = call)
}

#' Build a side-by-side comparison video
#'
#' Stack two or more videos into a single comparison video — side-by-side
#' (\code{direction = "horizontal"}) or one above the other
#' (\code{direction = "vertical"}) — a common need when reviewing annotations or
#' before/after processing. Built on the blessed stacking verbs
#' (\code{\link{ffm_hstack}} / \code{\link{ffm_vstack}}).
#'
#' By default the two inputs are resized to share an edge (equal heights for a
#' horizontal stack, equal widths for a vertical one); resizing currently
#' supports exactly two inputs, so pass \code{resize = FALSE} to compare more.
#' Audio is dropped unless \code{audio} names an input to carry; a carried
#' track is stream-copied unless \code{audio_codec} names an encoder.
#'
#' @param infiles A character vector of two or more video file paths.
#' @param outfile A string giving the path to write the comparison video to.
#' @param direction Either \code{"horizontal"} (side-by-side, the default) or
#'   \code{"vertical"} (stacked top to bottom).
#' @param resize A logical indicating whether to resize the inputs to share an
#'   edge. Only supported for exactly two inputs. (default = \code{TRUE})
#' @param audio The 0-based index of the input whose audio to keep in the
#'   output, or \code{NULL} to drop audio entirely. (default = \code{NULL})
#' @param video_codec A string naming the output video codec, or \code{NULL}
#'   (default) to leave it unset, so the output container's default encoder is
#'   used and the compiled command is unchanged from one that never named a
#'   codec.
#' @param audio_codec A string naming the codec for the carried audio track.
#'   \code{"copy"} (default) stream-copies it through untouched; name an encoder
#'   (e.g. \code{"aac"}) to transcode it, or pass \code{NULL} to leave the codec
#'   unset so the output container's default encoder is used. Nothing is emitted
#'   when \code{audio} is \code{NULL}, since no audio reaches the output; naming
#'   an encoder in that case is an error.
#' @param hardware The encoder backend: \code{"none"} (default, the software
#'   \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU encoding. When
#'   \code{"nvenc"}, the nvenc encoder for \code{video_codec}'s family is used
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}); with the default
#'   \code{video_codec = NULL} the H.264 family is assumed, so a non-H.264
#'   container (e.g. \code{.webm}) needs an explicit HEVC- or AV1-family
#'   \code{video_codec}. See \code{\link{has_nvenc}} for availability and its
#'   caveats.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, encode in software with a message (\code{TRUE}) instead of
#'   aborting (\code{FALSE}, default). With \code{video_codec = NULL} the
#'   fallback leaves the codec unset rather than picking one, so the codec never
#'   changes silently.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_hstack()] and [ffm_vstack()], the builders it wraps;
#'   [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [picture_in_picture()] for insetting instead of stacking.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' compare_videos(c(video, video), "compare.mp4", run = FALSE)
#' @export
compare_videos <- function(infiles, outfile,
                           direction = c("horizontal", "vertical"),
                           resize = TRUE, audio = NULL, video_codec = NULL,
                           audio_codec = "copy",
                           hardware = c("none", "nvenc"), fallback = FALSE,
                           run = TRUE) {

  if (!rlang::is_character(infiles) || length(infiles) < 2) {
    cli::cli_abort("{.arg infiles} must name two or more video files.")
  }
  rlang::check_string(outfile)
  rlang::check_bool(resize)
  rlang::check_number_whole(
    audio, min = 0, max = length(infiles) - 1, allow_null = TRUE
  )
  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  p <- compare_videos_pipeline(infiles, outfile, direction, resize, audio,
                               video_codec = video_codec,
                               audio_codec = audio_codec,
                               hardware = hardware, fallback = fallback)
  ffm_finish(p, run)
}


# picture_in_picture() ----------------------------------------------------

# Build the picture-in-picture overlay pipeline shared by picture_in_picture()
# and its _batch sibling (M32): translate the corner/center choice into overlay
# x/y expressions, scale + position the inset, optionally carry one input's
# audio. Assumes `scale`/`margin`/`audio` already type-checked by the caller;
# `position` is arg-matched here so both callers get a clean per-value error.
# ABOVE the roxygen block (M28 lesson).
picture_in_picture_pipeline <- function(main, overlay, outfile,
                                        position = c("topright", "topleft",
                                                     "bottomright", "bottomleft",
                                                     "center"),
                                        scale = 0.25, margin = 16, audio = NULL,
                                        video_codec = NULL,
                                        audio_codec = "copy",
                                        hardware = "none",
                                        fallback = FALSE,
                                        call = rlang::caller_env()) {
  position <- rlang::arg_match(position)
  # Same contradiction as compare_videos(): no mapped audio means no stream for a
  # named encoder to act on, while NULL only ever means "emit no -codec:a"
  # (M35/D017). IP3/D009 untouched -- the graph and its labels are unchanged.
  if (is.null(audio) && !is.null(audio_codec) &&
      !identical(audio_codec, "copy")) {
    cli::cli_abort(
      c(
        "{.arg audio_codec} needs an audio stream to encode.",
        "x" = "{.code audio = NULL} carries no audio into the output.",
        "i" = "Pass {.arg audio} {.val {0}} for the main video's audio or
               {.val {1}} for the overlay's, or drop {.arg audio_codec}."
      ),
      call = call
    )
  }

  m <- as.integer(margin)
  pos <- switch(
    position,
    topleft     = list(x = as.character(m), y = as.character(m)),
    topright    = list(x = sprintf("main_w-overlay_w-%d", m),
                       y = as.character(m)),
    bottomleft  = list(x = as.character(m),
                       y = sprintf("main_h-overlay_h-%d", m)),
    bottomright = list(x = sprintf("main_w-overlay_w-%d", m),
                       y = sprintf("main_h-overlay_h-%d", m)),
    center      = list(x = "(main_w-overlay_w)/2",
                       y = "(main_h-overlay_h)/2")
  )
  p <- ffm_files(c(main, overlay), outfile)
  p <- ffm_overlay(p, x = pos$x, y = pos$y, scale = scale)
  if (!is.null(audio)) {
    p <- ffm_map(p, paste0(audio, ":a"))
    # The carried track is mapped straight through, so the default audio_codec
    # stream-copies it instead of letting the container re-encode it (M35/D017).
    p <- apply_audio_codec(p, audio_codec, call = call)
  }
  # The composited video is a filtered stream, so a -codec:v rides alongside the
  # -filter_complex … [vout] mapping ffm_overlay() emits; the default
  # video_codec = NULL emits none (M34/D016).
  apply_video_codec(p, video_codec, hardware, fallback, call = call)
}

#' Inset one video over another (picture-in-picture)
#'
#' Composite a smaller \code{overlay} video onto a \code{main} video in one
#' corner (or the center) — the classic picture-in-picture layout for pairing a
#' speaker with a screen recording, or a stimulus with a webcam. Built on the
#' blessed \code{\link{ffm_overlay}} verb, which resizes the overlay to a
#' fraction of the main video's width and positions it.
#'
#' Audio is dropped unless \code{audio} names an input to carry (\code{0} = the
#' main video, \code{1} = the overlay). A carried track is
#' stream-copied unless \code{audio_codec} names an encoder.
#'
#' @param main A string giving the path to the background (full-size) video.
#' @param overlay A string giving the path to the inset video.
#' @param outfile A string giving the path to write the result to.
#' @param position Where to place the inset: one of \code{"topright"} (default),
#'   \code{"topleft"}, \code{"bottomright"}, \code{"bottomleft"}, or
#'   \code{"center"}.
#' @param scale The inset's width as a fraction of the main video's width, aspect
#'   preserved (\code{0 < scale <= 1}). (default = \code{0.25})
#' @param margin The gap in pixels between the inset and the video edges (ignored
#'   for \code{position = "center"}). (default = \code{16})
#' @param audio The 0-based index of the input whose audio to keep, or
#'   \code{NULL} to drop audio. (default = \code{NULL})
#' @param video_codec A string naming the output video codec, or \code{NULL}
#'   (default) to leave it unset, so the output container's default encoder is
#'   used and the compiled command is unchanged from one that never named a
#'   codec.
#' @param audio_codec A string naming the codec for the carried audio track.
#'   \code{"copy"} (default) stream-copies it through untouched; name an encoder
#'   (e.g. \code{"aac"}) to transcode it, or pass \code{NULL} to leave the codec
#'   unset so the output container's default encoder is used. Nothing is emitted
#'   when \code{audio} is \code{NULL}, since no audio reaches the output; naming
#'   an encoder in that case is an error.
#' @param hardware The encoder backend: \code{"none"} (default, the software
#'   \code{video_codec}) or \code{"nvenc"} for NVIDIA GPU encoding. When
#'   \code{"nvenc"}, the nvenc encoder for \code{video_codec}'s family is used
#'   (e.g. \code{"libx264"} becomes \code{"h264_nvenc"}); with the default
#'   \code{video_codec = NULL} the H.264 family is assumed, so a non-H.264
#'   container (e.g. \code{.webm}) needs an explicit HEVC- or AV1-family
#'   \code{video_codec}. See \code{\link{has_nvenc}} for availability and its
#'   caveats.
#' @param fallback A logical: when \code{hardware = "nvenc"} but nvenc is
#'   unavailable, encode in software with a message (\code{TRUE}) instead of
#'   aborting (\code{FALSE}, default). With \code{video_codec = NULL} the
#'   fallback leaves the codec unset rather than picking one, so the codec never
#'   changes silently.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_overlay()], the builder it wraps; [has_nvenc()] for the
#'   \code{hardware = "nvenc"} toggle; [compare_videos()] for
#'   side-by-side stacking.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' picture_in_picture(video, video, "pip.mp4", run = FALSE)
#' @export
picture_in_picture <- function(main, overlay, outfile,
                               position = c("topright", "topleft",
                                            "bottomright", "bottomleft",
                                            "center"),
                               scale = 0.25, margin = 16, audio = NULL,
                               video_codec = NULL, audio_codec = "copy",
                               hardware = c("none", "nvenc"), fallback = FALSE,
                               run = TRUE) {

  check_file_exists(main)
  check_file_exists(overlay)
  rlang::check_string(outfile)
  rlang::check_number_decimal(scale)
  rlang::check_number_whole(margin, min = 0)
  rlang::check_number_whole(audio, min = 0, max = 1, allow_null = TRUE)
  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  p <- picture_in_picture_pipeline(
    main, overlay, outfile, position, scale, margin, audio,
    video_codec = video_codec, audio_codec = audio_codec,
    hardware = hardware, fallback = fallback
  )
  ffm_finish(p, run)
}


# concatenate_videos_batch() ----------------------------------------------

#' Concatenate Many Videos From a Jobs Table
#'
#' Join clips end to end for many outputs from a single jobs tibble — the
#' **batch** (table-driven) sibling of [concatenate_videos()] for when you have
#' more than one concatenation to produce. Unlike the single-input batch verbs,
#' each row's inputs are **many**, so \code{jobs} carries an \code{inputs}
#' list-column (each cell a character vector of source paths) plus an
#' \code{output} column (D015). This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible concat-demuxer command per row,
#' sharing the copy + map-0 pipeline with the scalar verb.
#'
#' @param jobs A data frame with one row per output and (at least) an
#'   \code{inputs} list-column — each cell a character vector of the source
#'   paths to join, in order — and an \code{output} column (destination path).
#'   An \code{output} column is required; this verb derives no destination. Any
#'   two rows resolving to the same output path are rejected. Any other columns
#'   are ignored.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [concatenate_videos()], the scalar verb it wraps; [ffm_batch()], the
#'   batch runner; [compare_videos_batch()] and [picture_in_picture_batch()],
#'   the other fan-in batch siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(inputs = list(c(video, video)), output = "joined.mp4")
#' concatenate_videos_batch(jobs, run = FALSE)
#' @export
concatenate_videos_batch <- function(jobs, run = TRUE, parallel = FALSE, ...) {

  jobs <- check_fanin_jobs(jobs, verb = "Concatenation")
  jobs <- reject_duplicate_outputs(jobs)

  # Thin Layer-2 fan-in over ffm_batch (D007/D015): one concat-demuxer pipeline
  # per row, sharing concatenate_pipeline() with concatenate_videos(). pmap
  # passes each `inputs` list cell as a character vector; `...` forwards
  # ffm_batch options (verify/manifest/...) to the runner.
  ffm_batch(
    jobs,
    function(inputs, output, ...) concatenate_pipeline(inputs, output),
    run = run,
    parallel = parallel,
    ...
  )
}


# compare_videos_batch() ---------------------------------------------------

#' Build Many Comparison Videos From a Jobs Table
#'
#' Stack videos side by side for many outputs from a single jobs tibble — the
#' **batch** (table-driven) sibling of [compare_videos()] for when you have more
#' than one comparison to produce. Each row carries an \code{inputs} list-column
#' (each cell two or more video paths) plus an \code{output} column (D015).
#' This is a thin wrapper over \code{\link{ffm_batch}}: one reproducible stacking
#' command per row, sharing the pipeline with the scalar verb.
#'
#' @param jobs A data frame with one row per output and (at least) an
#'   \code{inputs} list-column — each cell a character vector of **two or more**
#'   video paths — and an \code{output} column (destination path). Optional
#'   \code{direction}, \code{resize}, \code{audio}, \code{video_codec}, and
#'   \code{audio_codec} columns override the
#'   like-named arguments per row (a row omitting one falls back to the
#'   argument). In an \code{audio} column, \code{NA} means "drop audio" (the
#'   column's way of writing the scalar's \code{NULL}); in a \code{video_codec}
#'   or \code{audio_codec} column it means "leave the codec unset". Any two rows
#'   resolving to the same output path are rejected; other columns are ignored.
#' @param direction,resize,audio Defaults applied to every row lacking the
#'   corresponding column. See [compare_videos()] for their meaning. \code{audio}
#'   is validated per row against that row's input count.
#' @param video_codec A string naming the output video codec, applied to every
#'   row lacking a \code{video_codec} column, or \code{NULL} (default) to leave
#'   it unset so each output keeps its container's default encoder.
#' @param audio_codec A string naming the codec for the carried audio track,
#'   applied to every row lacking an \code{audio_codec} column. \code{"copy"}
#'   (default) stream-copies it; name an encoder to transcode it, or \code{NULL}
#'   to leave the codec unset. A row carrying no audio emits no \code{-codec:a};
#'   naming an encoder on such a row is an error.
#' @param hardware,fallback The encoder backend and its fallback behavior,
#'   applied to the whole batch (a property of the machine, not of a row, so
#'   neither is read as a \code{jobs} column). See [compare_videos()].
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [compare_videos()], the scalar verb it wraps; [ffm_batch()], the
#'   batch runner; [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [concatenate_videos_batch()] and [picture_in_picture_batch()],
#'   the other fan-in batch siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(inputs = list(c(video, video)), output = "compare.mp4")
#' compare_videos_batch(jobs, run = FALSE)
#' @export
compare_videos_batch <- function(jobs, direction = c("horizontal", "vertical"),
                                 resize = TRUE, audio = NULL,
                                 video_codec = NULL, audio_codec = "copy",
                                 hardware = c("none", "nvenc"),
                                 fallback = FALSE,
                                 run = TRUE, parallel = FALSE, ...) {

  direction <- rlang::arg_match(direction)
  rlang::check_bool(resize)
  rlang::check_number_whole(audio, min = 0, allow_null = TRUE)
  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  jobs <- check_fanin_jobs(jobs, min_inputs = 2L, verb = "Comparison")
  jobs <- reject_duplicate_outputs(jobs)

  # Validate present override columns up front (types); per-value checks
  # (direction vocabulary, resize/length compatibility, audio range) are
  # inherited per row from compare_videos_pipeline() / the per-row audio check.
  check_batch_string_col(jobs, "direction")
  check_batch_audio_col(jobs)
  check_batch_codec_col(jobs)
  check_batch_codec_col(jobs, "audio_codec")
  if ("resize" %in% names(jobs) &&
      (!is.logical(jobs$resize) || anyNA(jobs$resize))) {
    cli::cli_abort(
      "The {.field resize} column of {.arg jobs} must be {.val {TRUE}} or {.val {FALSE}} (no {.val {NA}})."
    )
  }

  # Thin Layer-2 fan-in over ffm_batch (D007/D015): one stacking pipeline per
  # row, sharing compare_videos_pipeline() with compare_videos(). A per-row
  # override column (via `...` from pmap) wins over the scalar arg; an `audio`
  # cell of NA means "drop audio" (the column form of the scalar's NULL).
  ffm_batch(
    jobs,
    function(inputs, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      aud <- pick("audio", audio)
      if (length(aud) == 1L && is.na(aud)) aud <- NULL
      if (!is.null(aud)) {
        rlang::check_number_whole(aud, min = 0, max = length(inputs) - 1)
      }
      compare_videos_pipeline(
        inputs, output,
        direction = pick("direction", direction),
        resize = pick("resize", resize),
        audio = aud,
        video_codec = batch_codec_cell(pick("video_codec", video_codec)),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        hardware = hardware,
        fallback = fallback
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# picture_in_picture_batch() -----------------------------------------------

#' Inset One Video Over Another For Many Outputs From a Jobs Table
#'
#' Composite an inset (overlay) video onto a main video for many outputs from a
#' single jobs tibble — the **batch** (table-driven) sibling of
#' [picture_in_picture()] for when you have more than one to produce. Its two
#' inputs have distinct roles, so \code{jobs} carries fixed \code{main} and
#' \code{overlay} columns (not a list-column; D015) plus an \code{output} column.
#' This is a thin wrapper over \code{\link{ffm_batch}}: one reproducible overlay
#' command per row, sharing the pipeline with the scalar verb.
#'
#' @param jobs A data frame with one row per output and (at least) \code{main}
#'   (background path), \code{overlay} (inset path), and \code{output}
#'   (destination path) columns. Optional \code{position}, \code{scale},
#'   \code{margin}, \code{audio}, \code{video_codec}, and \code{audio_codec}
#'   columns override the
#'   like-named arguments
#'   per row (a row omitting one falls back to the argument). In an \code{audio}
#'   column, \code{NA} means "drop audio" (the column's way of writing the
#'   scalar's \code{NULL}); in a \code{video_codec} or \code{audio_codec} column
#'   it means "leave the codec unset". Any two rows resolving to the same output
#'   path are rejected; other columns are ignored.
#' @param position,scale,margin,audio Defaults applied to every row lacking the
#'   corresponding column. See [picture_in_picture()] for their meaning.
#' @param video_codec A string naming the output video codec, applied to every
#'   row lacking a \code{video_codec} column, or \code{NULL} (default) to leave
#'   it unset so each output keeps its container's default encoder.
#' @param audio_codec A string naming the codec for the carried audio track,
#'   applied to every row lacking an \code{audio_codec} column. \code{"copy"}
#'   (default) stream-copies it; name an encoder to transcode it, or \code{NULL}
#'   to leave the codec unset. A row carrying no audio emits no \code{-codec:a};
#'   naming an encoder on such a row is an error.
#' @param hardware,fallback The encoder backend and its fallback behavior,
#'   applied to the whole batch (a property of the machine, not of a row, so
#'   neither is read as a \code{jobs} column). See [picture_in_picture()].
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [picture_in_picture()], the scalar verb it wraps; [ffm_batch()], the
#'   batch runner; [has_nvenc()] for the \code{hardware = "nvenc"} toggle;
#'   [concatenate_videos_batch()] and [compare_videos_batch()],
#'   the other fan-in batch siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(main = video, overlay = video, output = "pip.mp4")
#' picture_in_picture_batch(jobs, run = FALSE)
#' @export
picture_in_picture_batch <- function(jobs,
                                     position = c("topright", "topleft",
                                                  "bottomright", "bottomleft",
                                                  "center"),
                                     scale = 0.25, margin = 16, audio = NULL,
                                     video_codec = NULL, audio_codec = "copy",
                                     hardware = c("none", "nvenc"),
                                     fallback = FALSE,
                                     run = TRUE, parallel = FALSE, ...) {

  position <- rlang::arg_match(position)
  rlang::check_number_decimal(scale)
  rlang::check_number_whole(margin, min = 0)
  rlang::check_number_whole(audio, min = 0, max = 1, allow_null = TRUE)
  rlang::check_string(video_codec, allow_null = TRUE)
  rlang::check_string(audio_codec, allow_null = TRUE)
  hardware <- rlang::arg_match(hardware)

  # Fixed two-input shape (D015): main/overlay are distinct roles, so named
  # columns rather than a list-column — validated inline (parity with the
  # scalar's two required inputs), not via check_fanin_jobs().
  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per output.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  cols <- c("main", "overlay", "output")
  missing <- setdiff(cols, names(jobs))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} must have {.field main}, {.field overlay}, and {.field output} columns.",
      "x" = "Missing column{?s}: {.val {missing}}."
    ))
  }
  for (col in cols) {
    jobs[[col]] <- as.character(jobs[[col]])
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  jobs <- reject_duplicate_outputs(jobs)

  # Validate present override columns up front. scale/margin are required values
  # (no NA); audio may be NA (means "drop audio"). Per-value checks (position
  # vocabulary, audio range) are inherited per row below / from the pipeline.
  check_batch_string_col(jobs, "position")
  for (col in intersect(c("scale", "margin"), names(jobs))) {
    if (!is.numeric(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric (no {.val {NA}}).")
    }
  }
  check_batch_audio_col(jobs)
  check_batch_codec_col(jobs)
  check_batch_codec_col(jobs, "audio_codec")

  # Thin Layer-2 fan-in over ffm_batch (D007/D015): one overlay pipeline per row,
  # sharing picture_in_picture_pipeline() with picture_in_picture(). A per-row
  # override column (via `...` from pmap) wins over the scalar arg; an `audio`
  # cell of NA means "drop audio" (the column form of the scalar's NULL).
  ffm_batch(
    jobs,
    function(main, overlay, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      # Re-check the resolved margin per row: a `margin` column bypasses the
      # scalar arg's check_number_whole(min = 0), so enforce it here (parity).
      mrg <- pick("margin", margin)
      rlang::check_number_whole(mrg, min = 0, arg = "margin")
      aud <- pick("audio", audio)
      if (length(aud) == 1L && is.na(aud)) aud <- NULL
      if (!is.null(aud)) rlang::check_number_whole(aud, min = 0, max = 1)
      picture_in_picture_pipeline(
        main, overlay, output,
        position = pick("position", position),
        scale = pick("scale", scale),
        margin = mrg,
        audio = aud,
        video_codec = batch_codec_cell(pick("video_codec", video_codec)),
        audio_codec = batch_codec_cell(pick("audio_codec", audio_codec)),
        hardware = hardware,
        fallback = fallback
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# Get volume levels -------------------------------------------------------

get_volume <- function(infile) {
  rlang::check_string(infile)
  
  command <- glue::glue('-i {infile} -af "volumedetect" -vn -sn -dn -f null NUL')
  
  output <- ffmpeg(command)
  
  #TODO: Clean up output
  
  mean_volumes <- regmatches(
    output,
    regexpr("^\\[Parsed_volumedetect.*mean_volume.*", output, perl=TRUE)
  )

  max_volumes <- regmatches(
    output,
    regexpr("^\\[Parsed_volumedetect.*max_volume.*", output, perl=TRUE)
  )
  
  output
}
