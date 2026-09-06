# ffm_files() ------------------------------------------------------------------

#' Specify Files in an FFmpeg Pipeline
#'
#' Start an FFmpeg pipeline by specifying input and output files.
#'
#' @param input A character vector containing strings that indicate the input
#'   media file for the pipeline (provide more than one for stacking).
#' @param output A string indicating the output media file for the pipeline.
#' @param overwrite A logical indicating whether the output media file should be
#'   overwritten if it already exists. (default = \code{TRUE})
#' @return An FFmpeg pipeline object.
#' @seealso [ffm_compile()] to render the pipeline and [ffm_run()] to execute
#'   it; the Layer-2 task verbs (e.g. [standardize_video()], [segment_video()])
#'   wrap this engine.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_compile()
#' @export
ffm_files <- function(input, output, overwrite = TRUE) {
  
  if (!rlang::is_character(input) || length(input) == 0) {
    cli::cli_abort(
      "{.arg input} must be a character vector naming at least one input file."
    )
  }
  rlang::check_string(output)
  rlang::check_bool(overwrite)
  # The pipeline's own input refusal, reaching the same site every verb's front
  # door reaches (M63). Its predicate WAS this function's, tested here and
  # nowhere else, so a front door refusing existence and a pipeline refusing
  # readability disagreed on a file that is there but cannot be opened. One site
  # now holds the predicate and the wording, so the two cannot disagree.
  # `multiple = TRUE` follows the ARGUMENT's contract -- `input` admits several
  # paths for stacking -- not the count this call happened to pass.
  check_paths_readable(input, arg = "input", multiple = TRUE)

  new_ffm(
    input = input,
    output = output,
    overwrite = overwrite,
    drop = vector("character", 0),
    codec_video = vector("character", 0),
    codec_audio = vector("character", 0),
    pixel_format = vector("character", 0),
    filter_video = vector("character", 0),
    filter_audio = vector("character", 0),
    map = vector("character", 0),
    complex = FALSE
  )
}

# ffm_trim() --------------------------------------------------------------

#' Trim the Duration of the FFmpeg Pipeline
#'
#' Trim the input so that the output contains one continuous subpart of the
#' input. Note that, if \code{start=NULL}, then the kept section will start at
#' the beginning of the input. If both \code{end=NULL} and \code{duration=NULL},
#' the kept section will end at the end of the input.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param start The time of the start of the kept section (i.e., this will be
#'   the first frame in the output) given in \code{units}.
#' @param end The time of the first frame that will be dropped (i.e., the frame
#'   immediately preceding this will be the last frame in the output), given in
#'   \code{units}.
#' @param duration The maximum duration of the output given in time duration
#'   syntax.
#' @param units A string indicating whether the \code{start} and/or \code{end}
#'   are given time duration syntax ("tds"), timebase units ("pts"), or frame
#'   number ("frame"). default = \code{"tds"}
#' @param setpts A logical indicating whether the output timestamps should be
#'   modified to start at zero. If TRUE, will add a setpts filter after trim.
#' @return \code{object} but will added instructions to trim the duration.
#' @seealso [ffm_seek()], the faster seek-based cut that can stream-copy (this
#'   is the frame-exact *filter*).
#' @references https://ffmpeg.org/ffmpeg-filters.html#trim
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_trim(start = 1, end = 5) |>
#'   ffm_compile()
#' @export
ffm_trim <- function(object,
                     start = NULL,
                     end = NULL,
                     duration = NULL,
                     units = c("tds", "pts", "frame"),
                     setpts = TRUE) {
  
  # Validate arguments
  check_ffm(object)
  if (!is.null(start) && length(start) != 1) {
    cli::cli_abort("{.arg start} must be a single value or {.code NULL}.")
  }
  if (!is.null(end) && length(end) != 1) {
    cli::cli_abort("{.arg end} must be a single value or {.code NULL}.")
  }
  if (!is.null(duration) && length(duration) != 1) {
    cli::cli_abort("{.arg duration} must be a single value or {.code NULL}.")
  }
  units <- rlang::arg_match(units)
  rlang::check_bool(setpts)
  
  # select arguments based on units
  if (units == "tds") {
    s_arg <- "start"
    e_arg <- "end"
  } else if (units == "pts") {
    s_arg <- "start_pts"
    e_arg <- "end_pts"
  } else if (units == "frame") {
    s_arg <- "start_frame"
    e_arg <- "end_frame"
  }
  
  # create filter command
  trim_args <- c(
    glue('{s_arg}={start}'),
    glue('{e_arg}={end}'),
    glue('duration={duration}')
  )
  cmd <- paste0("trim=", paste(trim_args, collapse = ":"))
  
  # append filter command
  object$filter_video <- c(object$filter_video, cmd)

  # add setpts only when requested (resets output timestamps to start at zero)
  if (setpts) {
    object$filter_video <- c(object$filter_video, "setpts=PTS-STARTPTS")
  }

  object
}

# ffm_seek() -------------------------------------------------------------------

#' Cut a Continuous Section from an FFmpeg Pipeline by Seeking
#'
#' Keep one continuous section of the input using FFmpeg's fast \code{-ss}/
#' \code{-to} seek options, rather than the \code{trim} *filter* (see
#' \code{\link{ffm_trim}}). Unlike the filter, seeking can stream-copy, so it is
#' the tool for fast, lossless cutting.
#'
#' The \code{reencode} argument trades accuracy against speed:
#' \itemize{
#'   \item \code{reencode = TRUE} (default) is \strong{frame-accurate}: the
#'     section is re-encoded so it begins and ends on the exact requested
#'     frames. This is the safe default.
#'   \item \code{reencode = FALSE} is a \strong{fast, lossless copy}, but the cut
#'     points snap to the nearest keyframes, so the output duration can differ
#'     from the request by up to one group-of-pictures. Pair it with
#'     \code{\link{ffm_copy}} for the fastest path.
#' }
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param start The start of the kept section, in seconds or FFmpeg time
#'   duration syntax. \code{NULL} keeps from the beginning.
#' @param end The end of the kept section, in seconds or FFmpeg time duration
#'   syntax. \code{NULL} keeps to the end.
#' @param reencode A logical: re-encode for a frame-accurate cut (\code{TRUE},
#'   default) or fast copy-safe seek that snaps to keyframes (\code{FALSE}).
#' @return \code{object} with the added instruction to seek-cut the input.
#' @seealso [ffm_trim()] for the filter-based alternative, [ffm_copy()] for the
#'   fast copy path, and [segment_video()], the task verb built on it.
#' @references https://ffmpeg.org/ffmpeg.html#Main-options
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Fast, lossless copy cut (snaps to keyframes)
#' ffm_files(video, "output.mp4") |>
#'   ffm_seek(start = 1, end = 5, reencode = FALSE) |>
#'   ffm_copy() |>
#'   ffm_compile()
#' @export
ffm_seek <- function(object, start = NULL, end = NULL, reencode = TRUE) {

  check_ffm(object)
  if (!is.null(start) && length(start) != 1) {
    cli::cli_abort("{.arg start} must be a single value or {.code NULL}.")
  }
  if (!is.null(end) && length(end) != 1) {
    cli::cli_abort("{.arg end} must be a single value or {.code NULL}.")
  }
  if (is.null(start) && is.null(end)) {
    cli::cli_abort("Provide at least one of {.arg start} or {.arg end}.")
  }
  rlang::check_bool(reencode)

  if (!is.null(start)) object$seek_start <- as.character(start)
  if (!is.null(end)) object$seek_end <- as.character(end)
  object$seek_reencode <- reencode

  object
}

# ffm_drop() -------------------------------------------------------------------

#' Drop Steams from an FFmpeg Pipeline
#'
#' Remove one or more specified streams from the media file. For example, remove
#' the video, audio, subtitles, or data stream from a media file.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param streams A character vector containing one or more of the following
#'   strings: \code{"video"}, \code{"audio"}, \code{"subtitles"}, \code{"data"}
#' @return \code{object} but with the added instruction to drop one or more
#'   streams from the output file when run.
#' @seealso [extract_audio()], the task verb that drops the video stream via
#'   this builder.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Drop the audio stream (keep video only)
#' ffm_files(video, "output.mp4") |>
#'   ffm_drop(streams = "audio") |>
#'   ffm_compile()
#' @export
ffm_drop <- function(object,
                     streams = c("video", "audio", "subtitles", "data")) {
  
  check_ffm(object)
  streams <- rlang::arg_match(streams, multiple = TRUE)

  # Store the stream names; ffm_compile() renders the -vn/-an/-sn/-dn output
  # options in the correct position (after -i, before the output file).
  object$drop <- streams

  object
}

# ffm_crop() -------------------------------------------------------------------

#' Crop Frames in an FFmpeg Pipeline
#'
#' Decrease the size of the video's frames by cropping it.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param width The width of the output video (in pixels). Either a positive
#'   real number or a string that contains an FFMPEG expression.
#' @param height The height of the output video (in pixels). Either a positive
#'   real number or a string that contains an FFMPEG expression.
#' @param x The horizontal position, in the input video, of the left edge of the
#'   output video (in pixels). Either a positive real number or a string that
#'   contains an FFMPEG expression. (default = \code{"(in_w-out_w)/2"})
#' @param y The vertical position, in the input video, of the top edge of the
#'   output video (in pixels). Either a positive real number or a string that
#'   contains an FFMPEG expression. (default = \code{"(in_h-out_h)/2"})
#' @return \code{object} but with the added instruction to crop the image(s).
#' @seealso [ffm_scale()] to resize instead of crop; [crop_video()] and
#'   [format_for_web()] are the task verbs built on it.
#' @references https://ffmpeg.org/ffmpeg-filters.html#toc-crop
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Crop to a centered 160x120 region
#' ffm_files(video, "output.mp4") |>
#'   ffm_crop(width = 160, height = 120) |>
#'   ffm_compile()
#' @export
ffm_crop <- function(object,
                     width,
                     height,
                     x = "(in_w-out_w)/2",
                     y = "(in_h-out_h)/2") {
  
  check_ffm(object)
  check_dim(width)
  check_dim(height)
  check_dim(x, inclusive = TRUE)
  check_dim(y, inclusive = TRUE)

  cmd <- glue('crop=w={width}:h={height}:x={x}:y={y}')
  object$filter_video <- c(object$filter_video, cmd)

  object
}

# ffm_scale() ------------------------------------------------------------------

#' Scale (Resize) Frames in a FFmpeg Pipeline
#'
#' Scale (resize) the input video's frames to either a specific width and height
#' (in pixels) or using an FFmpeg expression.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param width The width of the output video (in pixels). Either (1) a positive
#'   real number or (2) a string that contains an FFmpeg expression.
#' @param height The height of the output video (in pixels). Either (1) a
#'   positive real number or (2) a string that contains an FFmpeg expression.
#' @return \code{object} but with the added instruction to resize the image(s).
#' @seealso [ffm_crop()] to crop instead of resize; [standardize_video()] is the
#'   task verb built on it.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_scale(width = 160, height = 120) |>
#'   ffm_compile()
#' @export
ffm_scale <- function(object, width, height) {

  check_ffm(object)
  check_dim(width)
  check_dim(height)

  cmd <- glue('scale=w={width}:h={height}')
  object$filter_video <- c(object$filter_video, cmd)

  object
}

# ffm_fps() --------------------------------------------------------------------

#' Set the Frame Rate in an FFmpeg Pipeline
#'
#' Resample the video to a constant frame rate via FFmpeg's \code{fps} filter,
#' duplicating or dropping frames as needed. Appended to the video filter chain
#' like the other single-input sequential filters.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param fps The target frame rate. Either (1) a positive real number of
#'   frames per second or (2) a string that contains an FFmpeg framerate
#'   expression (for example \code{"30000/1001"} for NTSC).
#' @return \code{object} but with the added instruction to resample the frame
#'   rate.
#' @seealso [standardize_video()], the task verb that sets frame rate via this
#'   builder.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_fps(fps = 30) |>
#'   ffm_compile()
#' @export
ffm_fps <- function(object, fps) {

  check_ffm(object)
  check_dim(fps)

  cmd <- glue('fps={fps}')
  object$filter_video <- c(object$filter_video, cmd)

  object
}

# ffm_loudnorm() ---------------------------------------------------------------

#' Normalize Loudness in an FFmpeg Pipeline
#'
#' Append FFmpeg's \code{loudnorm} (EBU R128) audio filter, normalizing the
#' input's perceived loudness toward a target integrated loudness, true-peak
#' ceiling, and loudness range. This is the first builder function to write the
#' pipeline's audio filter chain, so it compiles to \code{-af} (or joins an
#' existing audio filter chain in application order).
#'
#' @details
#' This is single-pass (dynamic) \code{loudnorm}: one reproducible command, no
#' measurement pass. The defaults follow EBU Recommendation R 128 (2014) —
#' \code{target_loudness = -23} LUFS and \code{true_peak = -1} dBTP, loudness
#' measured per ITU-R BS.1770-4 — with \code{loudness_range = 7} (FFmpeg's own
#' \code{loudnorm} default, EBU R128 not prescribing a single value).
#'
#' Two filters are appended, not one: \code{loudnorm} is followed by
#' \code{asetnsamples}, which re-chunks the filtered audio into 4096-sample
#' frames without padding the last one. Dynamic \code{loudnorm} resamples to
#' 192 kHz and emits 192000-sample frames, which encoders that accept whatever
#' frame they are handed — FLAC and Vorbis among them — refuse to open at all.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param target_loudness The target integrated loudness, in LUFS
#'   (`r loudnorm_bounds_rd("target_loudness")`; default \code{-23}, the EBU
#'   R128 target).
#' @param true_peak The maximum true peak, in dBTP
#'   (`r loudnorm_bounds_rd("true_peak")`; default \code{-1}, the EBU R128
#'   ceiling).
#' @param loudness_range The target loudness range, in LU
#'   (`r loudnorm_bounds_rd("loudness_range")`; default \code{7}).
#' @param measured_i,measured_tp,measured_lra,measured_thresh Measured input
#'   values from a prior \code{loudnorm} analysis pass (integrated loudness,
#'   true peak, loudness range, and threshold). Supplied together to drive an
#'   accurate two-pass (linear) correction; all five of these plus \code{offset}
#'   must be given as a set, or none (\code{NULL}, default, for single-pass
#'   dynamic normalization). These map to FFmpeg's \code{measured_I},
#'   \code{measured_TP}, \code{measured_LRA}, and \code{measured_thresh} options.
#' @param offset The \code{target_offset} (offset gain) reported by the analysis
#'   pass, part of the measured set (see \code{measured_i}). \code{NULL} by
#'   default.
#' @param linear A logical: when \code{TRUE}, request linear normalization
#'   (\code{linear=true}), which needs the measured values to hit the target
#'   precisely. \code{FALSE} (default) omits the option entirely, leaving
#'   single-pass dynamic behavior untouched.
#' @param print_format The measurement report format for an analysis pass, one
#'   of \code{"json"}, \code{"summary"}, or \code{"none"}. \code{NULL} (default)
#'   omits the option. Use \code{"json"} for a machine-parseable analysis pass.
#' @return \code{object} but with the added instruction to normalize loudness.
#' @seealso [normalize_audio()], the task verb built on this filter.
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' \url{https://ffmpeg.org/ffmpeg-filters.html#loudnorm}
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_loudnorm() |>
#'   ffm_compile()
#' @export
ffm_loudnorm <- function(object,
                         target_loudness = -23,
                         true_peak = -1,
                         loudness_range = 7,
                         measured_i = NULL,
                         measured_tp = NULL,
                         measured_lra = NULL,
                         measured_thresh = NULL,
                         offset = NULL,
                         linear = FALSE,
                         print_format = NULL) {

  check_ffm(object)
  # The three target ranges live in one binding each (loudnorm_range_*), read
  # here and at normalize_audio()'s / _batch's front doors (M65).
  check_loudnorm_targets(target_loudness, true_peak, loudness_range)
  # Measured values are observed (not user targets), so they are range-free but
  # must be finite real numbers.
  rlang::check_number_decimal(measured_i, allow_null = TRUE, allow_infinite = FALSE)
  rlang::check_number_decimal(measured_tp, allow_null = TRUE, allow_infinite = FALSE)
  rlang::check_number_decimal(measured_lra, allow_null = TRUE, allow_infinite = FALSE)
  rlang::check_number_decimal(measured_thresh, allow_null = TRUE, allow_infinite = FALSE)
  rlang::check_number_decimal(offset, allow_null = TRUE, allow_infinite = FALSE)
  rlang::check_bool(linear)
  if (!is.null(print_format)) {
    rlang::arg_match(print_format, c("json", "summary", "none"))
  }

  # The four measured values plus offset are one coherent set from a single
  # analysis pass: require all or none, so a half-specified correction can't
  # silently produce a wrong filter (FFmpeg would ignore the orphans).
  measured <- list(measured_I = measured_i, measured_TP = measured_tp,
                   measured_LRA = measured_lra, measured_thresh = measured_thresh,
                   offset = offset)
  present <- !vapply(measured, is.null, logical(1))
  if (any(present) && !all(present)) {
    cli::cli_abort(c(
      "The measured {.code loudnorm} values must be supplied together.",
      "x" = "Missing: {.field {names(measured)[!present]}}.",
      "i" = "Provide all of {.field measured_i/tp/lra/thresh} and {.field offset}, or none."
    ))
  }

  cmd <- glue("loudnorm=I={target_loudness}:TP={true_peak}:LRA={loudness_range}")
  if (all(present)) {
    cmd <- paste0(
      cmd,
      ":measured_I=", measured_i, ":measured_TP=", measured_tp,
      ":measured_LRA=", measured_lra, ":measured_thresh=", measured_thresh,
      ":offset=", offset
    )
  }
  # Emit `linear=true` only when requested; the default omits it so single-pass
  # commands stay byte-for-byte unchanged.
  if (linear) cmd <- paste0(cmd, ":linear=true")
  if (!is.null(print_format)) cmd <- paste0(cmd, ":print_format=", print_format)
  # Re-chunk on the way out. Dynamic `loudnorm` resamples to 192 kHz and emits
  # 192000-sample frames; an encoder with a fixed frame size is re-framed by
  # FFmpeg on the way in, but flac and vorbis take whatever frame they are
  # handed and 192000 is past flac's 65535-sample block ceiling -- measured on
  # FFmpeg 9.0.1, both die at `Could not open encoder before EOF` (exit 234)
  # leaving a zero-byte file. Unconditional rather than gated on `linear`,
  # because FFmpeg falls back from linear to dynamic on its own whenever the
  # linear correction would breach the true-peak target. `p=0` so the final
  # frame is not padded: padding would lengthen the output by up to one frame.
  object$filter_audio <- c(object$filter_audio, cmd, "asetnsamples=n=4096:p=0")

  object
}

# ffm_codec() ------------------------------------------------------------------

#' Set Codecs in an FFmpeg Pipeline
#'
#' Set the audio and/or video codecs for the output file. Note that you can use
#' the command \code{ffmpeg_codecs()} to see a list of the codecs included in your
#' FFmpeg version.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param audio A string indicating which audio codec to use or \code{NULL} to
#'   only set the video codec (default = \code{NULL}). See
#'   \code{\link{audio_stream}} for the two things the bare name \code{audio}
#'   means at Layer 1, and for the input index \code{audio_input}.
#' @param video A string indicating which video codec to use or \code{NULL} to
#'   only set the audio codec. default = \code{NULL}
#' @return \code{object} but with the added instruction to change the codec(s).
#' @seealso [ffm_copy()] for the stream-copy shortcut, [ffmpeg_codecs()] to list
#'   available codecs, and [standardize_video()], a task verb built on it.
#' @references https://ffmpeg.org/ffmpeg-codecs.html
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_codec(video = "libx264", audio = "aac") |>
#'   ffm_compile()
#' @export
ffm_codec <- function(object,
                      audio = NULL,
                      video = NULL) {
  
  check_ffm(object)
  # Cheap sanity check only (D-M06-3): whether the token names a real codec
  # stays FFmpeg's call, so compile behavior never depends on the binary.
  if (!is.null(audio)) check_token(audio)
  if (!is.null(video)) check_token(video)

  if (is.null(audio) == FALSE) {
    object$codec_audio <- audio
  }
  if (is.null(video) == FALSE) {
    object$codec_video <- video
  }

  object
}


# ffm_map() ---------------------------------------------------------------

#' Set the Stream Mapping in an FFmpeg Pipeline
#'
#' Select which input streams are included in the output via FFmpeg's
#' \code{-map} option. The default (\code{"0"}) maps every stream from the first
#' input. \code{mapping} may be a character vector, which emits one \code{-map}
#' per element in the order given — for example
#' \code{ffm_map(object, c("0:v", "0:a:1"))} keeps the video and the input's
#' \emph{second} audio track.
#'
#' Chaining \strong{appends}: a second \code{ffm_map()} call adds to the maps
#' already set rather than replacing them. Pass \code{replace = TRUE} to discard
#' them instead, which is how you narrow the all-streams map that
#' \code{\link{ffm_copy}} sets — appending to that one would duplicate the
#' stream in the output rather than select it.
#'
#' This is the only builder verb that accumulates; every other \code{ffm_*}
#' setter, \code{\link{ffm_copy}} included, assigns. The exception is earned by
#' this function's arguments being \emph{partial} selections that genuinely
#' compose (keep the video, then name one audio track).
#'
#' When the pipeline uses a multi-input verb (e.g.
#' \code{\link{ffm_hstack}}), the explicit mapping is added \emph{alongside}
#' the automatic \code{-map "[vout]"} of the filtered stream — for example,
#' \code{ffm_map(object, "0:a")} keeps the first input's audio next to the
#' stacked video.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param mapping A character vector of one or more stream specifiers, one
#'   \code{-map} each.
#' @param replace A logical: discard any mapping already set on \code{object}
#'   (\code{TRUE}) or append to it (\code{FALSE}, default).
#' @return \code{object} with the added stream mapping instruction.
#' @seealso [ffm_copy()], which maps all streams; [separate_audio_video()] is a
#'   task verb built on it.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_map(mapping = "0") |>
#'   ffm_compile()
#'
#' # Keep the video and the second audio track only
#' ffm_files(video, "output.mkv") |>
#'   ffm_map(mapping = c("0:v", "0:a:1")) |>
#'   ffm_compile()
#' @export
ffm_map <- function(object, mapping = "0", replace = FALSE) {
  check_ffm(object)
  # Not check_string(): a vector is the point (M43). Spelled out rather than
  # deferred to rlang because check_character() is unexported.
  if (!is.character(mapping) || length(mapping) == 0L || anyNA(mapping)) {
    cli::cli_abort(
      "{.arg mapping} must be a character vector of one or more stream \\
       specifiers (no {.val {NA}})."
    )
  }
  rlang::check_bool(replace)

  # Append by default. Overwriting was the old behavior and it silently
  # discarded the earlier call, so a pipeline could not keep the video and then
  # name one audio track; `replace` keeps overwriting reachable for the one case
  # that needs it -- narrowing ffm_copy()'s all-streams "0" (M43). ffm_copy()
  # now sets that map through `replace` itself (M48/D027), so the two in-package
  # callers of this branch are it and segment_pipeline().
  #
  # No de-duplication here, deliberately: that was considered and rejected at
  # M48 (RR03). It would change this contract to fix a defect that is not this
  # function's -- `c(existing, "0")` is wrong because "0" SUBSUMES whatever it
  # is appended to, which is a fact about ffm_copy()'s specifier, not about
  # appending -- and it would leave `ffm_map("0:v") |> ffm_copy()` duplicating
  # the video stream regardless.
  object$map <- if (replace) mapping else c(object$map, mapping)

  object
}


# ffm_copy() --------------------------------------------------------------

#' Copy the codecs and map all streams
#'
#' Stream-copy the audio and/or video (no re-encoding) and, optionally, map all
#' streams from the input. This is the fast, lossless path when you only need to
#' remux or cut on keyframes.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param audio A logical indicating whether to copy the audio codec
#'   (default = \code{TRUE}). See \code{\link{audio_stream}} for the two
#'   things the bare name \code{audio} means at Layer 1, and for the input
#'   index \code{audio_input}.
#' @param video A logical indicating whether to copy the video codec.
#'   (default = \code{TRUE})
#' @param streams A logical indicating whether to map all streams from the
#'   input. This \strong{sets} the mapping to the all-streams specifier
#'   \code{"0"} rather than adding to it, so calling \code{ffm_copy()} twice
#'   compiles one \code{-map "0"}, not two. If the pipeline already states a
#'   \emph{different} mapping, that is a
#'   conflict and \code{ffm_copy()} aborts rather than discard it silently:
#'   pass \code{streams = FALSE} to keep the mapping you set, or call
#'   \code{ffm_copy()} first and narrow afterwards with
#'   \code{ffm_map(replace = TRUE)}. (default = \code{TRUE})
#' @return \code{object} with the added instruction to copy codecs and/or map
#'   all streams.
#' @seealso [ffm_codec()] and [ffm_map()], which it wraps; [segment_video()]
#'   uses it for fast copy cuts.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_copy() |>
#'   ffm_compile()
#' @export
ffm_copy <- function(object, audio = TRUE, video = TRUE, streams = TRUE) {
  
  check_ffm(object)
  rlang::check_bool(audio)
  rlang::check_bool(video)
  rlang::check_bool(streams)
  if (audio) {
    object <- ffm_codec(object, audio = "copy")
  }
  if (video) {
    object <- ffm_codec(object, video = "copy")
  }
  if (streams) {
    # ASSIGN, never append (M48/D027). "0" subsumes every other specifier the
    # linear builder can address (one input chain, IP2/D003), so appending it
    # has no composition in which it is what the caller wanted: beside an
    # existing "0" it duplicates every stream, and beside anything narrower
    # ("0:v") it duplicates that selection's streams. An operation whose
    # right-hand side subsumes any possible left-hand side is an assignment,
    # and ffm_copy() executing it as an increment is what M43's append change
    # accidentally made it do.
    check_copy_map_conflict(object$map, call = rlang::caller_env())
    object <- ffm_map(object, mapping = "0", replace = TRUE)
  }

  object
}

# Refuse to assign the all-streams map over a DIFFERENT stated mapping. Without
# this, ffm_copy()'s assignment would silently discard a selection the caller
# wrote -- the precise flaw D023 was written to remove ("a second call silently
# discarded the first"), reintroduced for one verb. The package already decided
# this shape one field over: segment_pipeline() aborts on an `audio_codec` that
# ffm_copy() would silently overwrite (M35/D017). RR03 chose an abort over a
# warning because the compiled command is the product (D001), and because an
# abort relaxed to a warning later is backward-compatible where the reverse
# breaks running code.
#
# The identical("0") carve-out is load-bearing, not a convenience: it is what
# keeps ffm_copy() |> ffm_copy() and ffm_concat() |> ffm_copy() silent no-op
# restatements instead of errors. It is literal, so c("0", "0") -- a map already
# doubled by hand -- is a conflict and aborts.
#
# Worded around the PIPELINE's map rather than the caller's frame, because
# ffm_concat() calls ffm_copy() internally: a user chain
# `ffm_map(...) |> ffm_concat()` trips this from a function they never called,
# where "you passed streams = TRUE" would be a lie and `streams = FALSE` is not
# an argument they can reach. That chain compiled `-map 0:v -map 0` before M48
# and duplicated a stream, so erroring is a fix rather than a regression.
check_copy_map_conflict <- function(map, call = rlang::caller_env()) {
  if (length(map) == 0L || identical(map, "0")) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "This pipeline already sets a stream mapping ({.val {map}}), which
       copying every stream would discard.",
      "i" = "Keep it with {.code ffm_copy(streams = FALSE)}, which copies the
             codecs and leaves the mapping alone.",
      "i" = "Or copy first and narrow after, with
             {.code ffm_map(..., replace = TRUE)}."
    ),
    class = "tidymedia_copy_map_conflict",
    call = call
  )
}

# ffm_pixel_format() ------------------------------------------------------

#' Set the Pixel Format in an FFmpeg Pipeline
#'
#' Set the output pixel format via FFmpeg's \code{-pix_fmt} option (for example
#' \code{"yuv420p"} for broad player compatibility).
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param format A string indicating the pixel format for the output file.
#' @return \code{object} with the added pixel-format instruction.
#' @seealso [standardize_video()] and [format_for_web()], the task verbs that
#'   set the pixel format via this builder.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm_files(video, "output.mp4") |>
#'   ffm_pixel_format("yuv420p") |>
#'   ffm_compile()
#' @export
ffm_pixel_format <- function(object, format) {
  
  check_ffm(object)
  check_token(format)

  object$pixel_format <- format

  object
}


# ffm_hstack() -----------------------------------------------------------------

#' Horizontally Stack Multiple Videos in an FFmpeg Pipeline
#'
#' Add a complex video filter to stack multiple videos horizontally
#' (side-by-side) and, optionally, resize them to have the same height.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param shortest A logical indicating whether to trim the duration of all
#'   videos to that of the shortest video (default = \code{FALSE})
#' @param resize A logical indicating whether to resize the height of the input
#'   videos to match (takes longer and currently only works with two inputs).
#'   Resizing conforms both inputs to the same aspect ratio, so it assumes the
#'   inputs share one.
#' @return \code{object} but with the added instruction to apply horizontal
#'   stacking.
#' @seealso [ffm_vstack()] for vertical stacking and [compare_videos()], the
#'   task verb built on both.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Stack two inputs side-by-side (pass more than one input to ffm_files())
#' ffm_files(c(video, video), "output.mp4") |>
#'   ffm_hstack() |>
#'   ffm_compile()
#' @export
ffm_hstack <- function(object,
                       shortest = FALSE,
                       resize = FALSE) {
  
  check_ffm(object)
  rlang::check_bool(shortest)
  rlang::check_bool(resize)
  inputs_n <- length(object$input)
  shortest_int <- as.integer(shortest)
  if (inputs_n <= 1) {
    cli::cli_abort("Stacking requires more than one input file.")
  }
  if (resize && inputs_n != 2) {
    cli::cli_abort("{.arg resize} currently only works with exactly two inputs.")
  }
  check_multi_input_ordering(object, "Stacking")

  # hstack is a blessed multi-input verb: it forces the -filter_complex path
  # (see ffm_compile()). The resize graph manages its own stream labels (it
  # starts with "[..]"), so ffm_compile() emits it verbatim; the plain hstack
  # token is label-free and ffm_compile() prepends the input labels. The graph
  # must be a single line (embedded newlines would leak into the command).
  if (resize == TRUE) {
    cmd <- paste0(
      "[0:v][1:v]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[0s][1s];",
      "[1s][0s]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[1s][0s];",
      "[0s][1s]hstack,setsar=1"
    )
  } else {
    cmd <- glue('hstack=inputs={inputs_n}:shortest={shortest_int}')
  }

  object$filter_video <- c(object$filter_video, cmd)
  object$complex <- TRUE

  object
}

# ffm_vstack() -----------------------------------------------------------------

#' Vertically Stack Multiple Videos in an FFmpeg Pipeline
#'
#' Add a complex video filter to stack multiple videos vertically (one above the
#' other) and, optionally, resize them to have the same width. This is the
#' vertical companion to \code{\link{ffm_hstack}}; both are blessed multi-input
#' verbs that force the \code{-filter_complex} path and manage their own stream
#' labels internally.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param shortest A logical indicating whether to trim the duration of all
#'   videos to that of the shortest video (default = \code{FALSE})
#' @param resize A logical indicating whether to resize the width of the input
#'   videos to match (takes longer and currently only works with two inputs).
#'   Resizing conforms both inputs to the same aspect ratio, so it assumes the
#'   inputs share one.
#' @return \code{object} but with the added instruction to apply vertical
#'   stacking.
#' @seealso [ffm_hstack()] for horizontal stacking and [compare_videos()], the
#'   task verb built on both.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Stack two inputs one above the other (pass more than one input to ffm_files())
#' ffm_files(c(video, video), "output.mp4") |>
#'   ffm_vstack() |>
#'   ffm_compile()
#' @export
ffm_vstack <- function(object,
                       shortest = FALSE,
                       resize = FALSE) {

  check_ffm(object)
  rlang::check_bool(shortest)
  rlang::check_bool(resize)
  inputs_n <- length(object$input)
  shortest_int <- as.integer(shortest)
  if (inputs_n <= 1) {
    cli::cli_abort("Stacking requires more than one input file.")
  }
  if (resize && inputs_n != 2) {
    cli::cli_abort("{.arg resize} currently only works with exactly two inputs.")
  }
  check_multi_input_ordering(object, "Stacking")

  # vstack mirrors hstack (see ffm_hstack()) but equalises *widths* instead of
  # heights: the scale2ref graph grows each input to the larger of the two
  # widths, preserving aspect via ow/mdar, then vertically stacks. Label-free
  # plain-vstack token is completed by ffm_compile() with input labels + [vout].
  if (resize == TRUE) {
    cmd <- paste0(
      "[0:v][1:v]scale2ref='if(lt(main_w,iw),iw,main_w)':'ow/mdar'[0s][1s];",
      "[1s][0s]scale2ref='if(lt(main_w,iw),iw,main_w)':'ow/mdar'[1s][0s];",
      "[0s][1s]vstack,setsar=1"
    )
  } else {
    cmd <- glue('vstack=inputs={inputs_n}:shortest={shortest_int}')
  }

  object$filter_video <- c(object$filter_video, cmd)
  object$complex <- TRUE

  object
}

# ffm_overlay() ----------------------------------------------------------------

#' Overlay One Video on Another in an FFmpeg Pipeline
#'
#' Composite the second input (the overlay) on top of the first (the main
#' video) at position \code{x}/\code{y}. This is a blessed multi-input verb (like
#' \code{\link{ffm_hstack}}): it forces the \code{-filter_complex} path and
#' manages its own stream labels internally. Exactly two inputs are required —
#' the first is the background, the second is drawn over it.
#'
#' \code{x} and \code{y} accept plain numbers (pixels from the top-left of the
#' main video) or FFmpeg overlay expressions, where \code{main_w}/\code{main_h}
#' are the main video's dimensions and \code{overlay_w}/\code{overlay_h} are the
#' overlay's. For example, \code{x = "main_w-overlay_w-16"} pins the overlay 16
#' pixels from the right edge. When \code{scale} is set, the overlay is first
#' resized to a fraction of the main video's width (aspect preserved), which is
#' what the Layer-2 \code{\link{picture_in_picture}} verb uses. Otherwise, to
#' resize the overlay yourself, filter it in a separate pipeline first.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()} with exactly two input files.
#' @param x The horizontal position of the overlay's left edge, as a number of
#'   pixels or an FFmpeg expression. (default = \code{0})
#' @param y The vertical position of the overlay's top edge, as a number of
#'   pixels or an FFmpeg expression. (default = \code{0})
#' @param shortest A logical indicating whether to end the output when the
#'   shorter input ends (default = \code{FALSE}).
#' @param scale An optional fraction (\code{0 < scale <= 1}) to resize the
#'   overlay to \code{scale} times the main video's width before compositing
#'   (aspect preserved); \code{NULL} (default) overlays at native size. When set,
#'   \code{overlay_w}/\code{overlay_h} in \code{x}/\code{y} refer to the resized
#'   overlay.
#' @return \code{object} with the added instruction to overlay the second input
#'   on the first.
#' @seealso [picture_in_picture()], the task verb built on this verb.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Draw the second input over the first, 16px in from the top-right corner
#' ffm_files(c(video, video), "output.mp4") |>
#'   ffm_overlay(x = "main_w-overlay_w-16", y = 16) |>
#'   ffm_compile()
#' @export
ffm_overlay <- function(object,
                        x = 0,
                        y = 0,
                        shortest = FALSE,
                        scale = NULL) {

  check_ffm(object)
  check_dim(x, inclusive = TRUE)
  check_dim(y, inclusive = TRUE)
  rlang::check_bool(shortest)
  rlang::check_number_decimal(scale, allow_null = TRUE)
  # The range rule lives in one binding (overlay_scale_range), read here and at
  # picture_in_picture()'s / _batch's front doors (M65).
  check_overlay_scale(scale)
  if (length(object$input) != 2) {
    cli::cli_abort("Overlaying requires exactly two input files.")
  }
  check_multi_input_ordering(object, "Overlaying")

  shortest_int <- as.integer(shortest)
  if (is.null(scale)) {
    # Label-free token: ffm_compile() prepends the two input pads ([0:v][1:v],
    # main then overlay) and appends [vout].
    cmd <- glue('overlay=x={x}:y={y}:shortest={shortest_int}')
  } else {
    # Self-labelled graph (starts with "["), so ffm_compile() emits it verbatim
    # and only appends [vout]. scale2ref resizes the overlay ([1:v]) using the
    # main ([0:v]) as reference: width = main_w*scale, height preserves the
    # overlay's own aspect (ih/iw). Must stay a single line (no newlines).
    cmd <- glue(
      "[1:v][0:v]scale2ref=w='main_w*{scale}':h='main_w*{scale}*ih/iw'",
      "[pip][bg];[bg][pip]overlay=x={x}:y={y}:shortest={shortest_int}"
    )
  }

  object$filter_video <- c(object$filter_video, cmd)
  object$complex <- TRUE

  object
}

# ffm_concat() -----------------------------------------------------------------

#' Concatenate Multiple Inputs in an FFmpeg Pipeline
#'
#' Join the pipeline's input files one after another using FFmpeg's
#' [concat demuxer](https://ffmpeg.org/ffmpeg-formats.html#concat-1). This is a
#' blessed multi-input verb (like \code{\link{ffm_hstack}}): it stream-copies,
#' so it is fast and lossless but requires that every input share the same
#' parameters (codec, resolution, frame rate, ...). To concatenate inputs with
#' differing parameters you must re-encode via the concat filter (not yet
#' wrapped; use the Layer 0 escape hatch).
#'
#' The demuxer needs a list file naming the inputs; \code{ffm_concat()} writes
#' one to a temporary path immediately and stores it in the pipeline, so the
#' compiled command can reference it. It also copies codecs and maps all
#' streams (as \code{\link{ffm_copy}} would).
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()} with more than one input file.
#' @return \code{object} with the added instruction to concatenate the inputs.
#' @seealso [concatenate_videos()], the task verb built on this verb.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Join two inputs end-to-end (they must share codec/resolution/frame rate)
#' ffm_files(c(video, video), "output.mp4") |>
#'   ffm_concat() |>
#'   ffm_compile()
#' @export
ffm_concat <- function(object) {

  check_ffm(object)
  if (length(object$input) <= 1) {
    cli::cli_abort("Concatenation requires more than one input file.")
  }
  if (length(object$filter_video) || length(object$filter_audio)) {
    cli::cli_abort(c(
      "Concatenation must come before other filters.",
      "i" = "The concat demuxer copies whole files; filter the result after."
    ))
  }

  # The demuxer reads a list file of `file '<path>'` lines. Write it now so the
  # compiled command is self-contained; -safe 0 permits absolute paths, and the
  # single quotes are escaped per the concat format's rules.
  listfile <- tempfile("ffm-concat", fileext = ".txt")
  lines <- paste0("file '", gsub("'", "'\\\\''", object$input), "'")
  writeLines(lines, listfile)

  object$concat <- TRUE
  object$concat_list <- listfile
  object <- ffm_copy(object)

  object
}

# ffm_drawbox() -----------------------------------------------------------

#' Draw a Colored Box on the Videos in an FFmpeg Pipeline
#'
#' Add a video filter to draw a colored rectangle on the input video.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param x The horizontal position, in the input video, of the left edge of the
#'   box (in pixels). Either a nonnegative real number or a string that contains
#'   an FFMPEG expression. (default = 0)
#' @param y The vertical position, in the input video, of the top edge of the
#'   box (in pixels). Either a nonnegative real number or a string that contains
#'   an FFMPEG expression. (default = 0)
#' @param width The width of the box (in pixels). Either a positive real number
#'   or a string that contains an FFmpeg expression. (default = \code{"in_w"})
#' @param height The height of the box (in pixels). Either a positive real
#'   number or a string that contains an FFmpeg expression. (default =
#'   \code{"in_h"})
#' @param color A string containing the color of the box in FFmpeg color syntax,
#'   see reference link below for more details. If the special value
#'   \code{"invert"} is used, the box color is the same as the video with
#'   inverted luma. (default = \code{"black"})
#' @param thickness A thickness of the box edge (in pixels). A value of
#'   \code{"fill"} will create a filled box. (default = \code{"fill"})
#' @return \code{object} but with the added instruction to apply the drawbox
#'   filter.
#' @seealso [anonymize_video()], the task verb that fills regions via this
#'   builder.
#' @references https://ffmpeg.org/ffmpeg-filters.html#drawbox
#' @references https://ffmpeg.org/ffmpeg-utils.html#color-syntax
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Draw a filled red box covering the top-left quarter of the frame
#' ffm_files(video, "output.mp4") |>
#'   ffm_drawbox(width = "in_w/2", height = "in_h/2", color = "red") |>
#'   ffm_compile()
#' @export
ffm_drawbox <- function(object,
                       x = 0,
                       y = 0,
                       width = "in_w",
                       height = "in_h",
                       color = "black",
                       thickness = "fill") {
  
  check_ffm(object)
  check_dim(x, inclusive = TRUE)
  check_dim(y, inclusive = TRUE)
  check_dim(width)
  check_dim(height)
  rlang::check_string(color)
  check_dim(thickness)

  cmd <- glue('drawbox=x={x}:y={y}:w={width}:h={height}:c={color}:t={thickness}')
  object$filter_video <- c(object$filter_video, cmd)
  
  object
}

# ffm_output_options() ---------------------------------------------------------

#' Add Raw Output Options to an FFmpeg Pipeline
#'
#' Append one or more raw FFmpeg output options (the flags that sit after the
#' input and before the output file) to the pipeline. This is a controlled
#' escape hatch for options that lack a dedicated verb: \code{ffm_compile()}
#' still owns where they are placed and how the rest of the command is quoted,
#' so this is not the same as gluing a command string yourself.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param ... One or more strings, each a whitespace-separated option group
#'   (e.g. \code{"-q:v 1"}, \code{"-frames:v 1"}). Added in the order given.
#'   At execution time each whitespace-separated token becomes one FFmpeg
#'   argument, so option values themselves must not contain spaces.
#' @return \code{object} with the added output options.
#' @seealso [ffmpeg()] for the full Layer 0 escape hatch, and [ffm_compile()],
#'   which places these options.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Extract a single frame by adding a raw output option
#' ffm_files(video, "frame.png") |>
#'   ffm_output_options("-frames:v 1") |>
#'   ffm_compile()
#' @export
ffm_output_options <- function(object, ...) {

  check_ffm(object)
  opts <- c(...)
  if (!rlang::is_character(opts) || length(opts) == 0) {
    cli::cli_abort("Provide at least one output option as a string.")
  }
  # Each whitespace-separated token becomes one FFmpeg argument at execution
  # (no shell parsing), so quoted values with spaces cannot work — reject them
  # loudly rather than emit a command that means something else than printed.
  if (any(grepl("[\"']", opts))) {
    cli::cli_abort(c(
      "Output options can't contain quote characters.",
      "x" = "Options are split on whitespace into FFmpeg arguments verbatim;
             quoting does not group tokens.",
      "i" = "Use values without spaces, or the {.fn ffmpeg} escape hatch."
    ))
  }

  object$output_opts <- c(object$output_opts, opts)

  object
}

# ffm_compile() ----------------------------------------------------------------

#' Compile the tidymedia pipeline into FFmpeg command
#'
#' Compile all the instructions into a string representing the FFmpeg command
#' needed to run it.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @return A string containing the FFmpeg command needed to execute all the
#'   instructions provided to the tidymedia pipeline.
#' @seealso [ffm_run()] to compile and execute in one step, and [ffm_batch()] to
#'   compile over many files.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # ffm_compile() returns the reproducible FFmpeg command as a string
#' ffm_files(video, "output.mp4") |>
#'   ffm_trim(start = 1, end = 5) |>
#'   ffm_crop(width = 160, height = 120) |>
#'   ffm_codec(video = "libx264") |>
#'   ffm_compile()
#' @export
ffm_compile <- function(object) {
  groups <- ffm_groups(object)
  paste(vapply(groups, `[[`, character(1), "display"), collapse = " ")
}

# ffm_args() ---------------------------------------------------------------

# Render the pipeline as an argument vector: one element per CLI argument,
# never shell-quoted. This is what actually gets executed (via run_program()'s
# system2 call), so paths containing spaces, quotes, `$`, or backticks reach
# FFmpeg verbatim. Internal by decision (D-M06-2): the exported surface stays
# `ffm_compile()`'s display string.
ffm_args <- function(object) {
  groups <- ffm_groups(object)
  unlist(lapply(groups, `[[`, "args"), use.names = FALSE)
}

# ffm_groups() -------------------------------------------------------------

# Shared assembly for the two renderings of a pipeline: the display string
# (`ffm_compile()`, the reproducibility artifact) and the argument vector
# (`ffm_args()`, what gets executed). Each group holds `args` (one element per
# CLI argument, unquoted) and `display` (the string fragment, with `quote`d
# elements wrapped in double quotes). Deriving both renderings from one
# structure is what keeps the printed command and the executed command from
# drifting apart (M06).
ffm_group <- function(args, quote = integer(), display = NULL) {
  args <- as.character(args)
  if (is.null(display)) {
    shown <- args
    shown[quote] <- paste0('"', shown[quote], '"')
    display <- paste(shown, collapse = " ")
  }
  list(args = args, display = display)
}

ffm_groups <- function(object) {

  check_ffm(object)

  # Guard: a copied stream is passed through untouched, so it cannot also be
  # filtered. Catch it here with a clear error instead of a cryptic ffmpeg
  # failure at run time (M02 D-M02-5).
  if (length(object$codec_video) && object$codec_video == "copy" &&
      length(object$filter_video)) {
    cli::cli_abort(c(
      "Can't apply a video filter while the video codec is set to {.val copy}.",
      "x" = "{.code copy} passes the stream through without re-encoding.",
      "i" = "Re-encode with a real codec via {.fn ffm_codec}, or drop the filter."
    ))
  }
  if (length(object$codec_audio) && object$codec_audio == "copy" &&
      length(object$filter_audio)) {
    cli::cli_abort(c(
      "Can't apply an audio filter while the audio codec is set to {.val copy}.",
      "x" = "{.code copy} passes the stream through without re-encoding.",
      "i" = "Re-encode with a real codec via {.fn ffm_codec}, or drop the filter."
    ))
  }

  # Guard: the concat demuxer stream-copies whole files, so it cannot also run
  # a filtergraph (that path is the concat *filter*, deferred).
  if (isTRUE(object$concat) &&
      (length(object$filter_video) || length(object$filter_audio))) {
    cli::cli_abort(c(
      "Can't apply a filter while concatenating with the concat demuxer.",
      "i" = "The demuxer copies whole files; filter the result in a second pass."
    ))
  }

  # Seek-based cut (distinct from the trim *filter*): a frame-accurate seek
  # re-encodes, so it cannot ride a copied stream.
  seek_reencode <- length(object$seek_reencode) && isTRUE(object$seek_reencode)
  if (seek_reencode && length(object$codec_video) &&
      object$codec_video == "copy") {
    cli::cli_abort(c(
      "Can't make a frame-accurate seek while the video codec is {.val copy}.",
      "x" = "Accurate seeking must re-encode to cut on an exact frame.",
      "i" = "Use {.code reencode = FALSE} for a fast copy cut (snaps to a keyframe)."
    ))
  }

  # Global options (before the inputs).
  overwrite <- list(ffm_group(if (isTRUE(object$overwrite)) "-y" else "-n"))

  # Seek options. A frame-accurate seek is placed *after* -i (output seeking,
  # re-encoded). A fast copy-safe seek is placed *before* -i (input seeking)
  # with -avoid_negative_ts so the copied segment starts cleanly at a keyframe
  # instead of the broken output-seek-copy path (see M03 D-M03-5).
  seek_pre <- list()
  seek_post <- list()
  if (length(object$seek_start) || length(object$seek_end)) {
    seeks <- c(
      if (length(object$seek_start)) list(ffm_group(c("-ss", object$seek_start))),
      if (length(object$seek_end)) list(ffm_group(c("-to", object$seek_end)))
    )
    if (seek_reencode) {
      seek_post <- seeks
    } else {
      seek_pre <- seeks
      seek_post <- list(ffm_group(c("-avoid_negative_ts", "make_zero")))
    }
  }

  # Inputs. The concat demuxer replaces the per-file -i list with a single -i
  # pointing at the list file that ffm_concat() wrote.
  inputs <- if (isTRUE(object$concat)) {
    list(ffm_group(
      c("-f", "concat", "-safe", "0", "-i", object$concat_list),
      quote = 6L
    ))
  } else {
    lapply(object$input, function(inp) ffm_group(c("-i", inp), quote = 2L))
  }

  # Filters and stream mapping. Single-input sequential chains compile to
  # -vf/-af; any multi-input (blessed) verb sets `complex` and compiles to
  # -filter_complex with explicit labels plus an auto -map (M02 D-M02-2).
  filters <- list()
  map <- list()
  if (isTRUE(object$complex)) {
    body <- paste(object$filter_video, collapse = ",")
    # A verb that manages its own stream labels starts the graph with "[..]";
    # otherwise prepend one video pad per input.
    if (!startsWith(body, "[")) {
      labels <- paste0("[", seq_along(object$input) - 1L, ":v]", collapse = "")
      body <- paste0(labels, body)
    }
    filters <- list(ffm_group(
      c("-filter_complex", paste0(body, "[vout]")),
      quote = 2L
    ))
    # D-M06-1: explicit ffm_map() maps ride alongside the auto [vout] map
    # (e.g. keep 0:a audio next to stacked video) instead of being dropped.
    # Both are `quote = 2L`: the specifier is quoted in the DISPLAY string only
    # (D031), so a compiled command survives a paste into a shell, where
    # `-map 0:v?` and `-map [vout]` are both glob patterns. `args` is untouched.
    map <- c(
      list(ffm_group(c("-map", "[vout]"), quote = 2L)),
      lapply(object$map, function(m) ffm_group(c("-map", m), quote = 2L))
    )
  } else {
    if (length(object$filter_video)) {
      filters <- c(filters, list(ffm_group(
        c("-vf", paste(object$filter_video, collapse = ",")),
        quote = 2L
      )))
    }
    if (length(object$filter_audio)) {
      filters <- c(filters, list(ffm_group(
        c("-af", paste(object$filter_audio, collapse = ",")),
        quote = 2L
      )))
    }
    # Display-only quoting, as in the complex branch above (D031).
    map <- lapply(object$map, function(m) ffm_group(c("-map", m), quote = 2L))
  }

  # Output options (after the inputs, before the output file).
  codecs <- c(
    if (length(object$codec_video)) {
      list(ffm_group(c("-codec:v", object$codec_video)))
    },
    if (length(object$codec_audio)) {
      list(ffm_group(c("-codec:a", object$codec_audio)))
    },
    if (length(object$pixel_format)) {
      list(ffm_group(c("-pix_fmt", object$pixel_format)))
    }
  )
  drop_flags <- c(
    if ("video" %in% object$drop) "-vn",
    if ("audio" %in% object$drop) "-an",
    if ("subtitles" %in% object$drop) "-sn",
    if ("data" %in% object$drop) "-dn"
  )
  drops <- if (length(drop_flags)) list(ffm_group(drop_flags)) else list()

  # Raw output-option passthrough (e.g. "-q:v 1"): positioned here so verbs can
  # add specific flags without owning command layout. Each whitespace-separated
  # token within a group becomes one CLI argument; the display keeps the group
  # verbatim.
  output_opts <- lapply(object$output_opts, function(opt) {
    ffm_group(strsplit(trimws(opt), "[[:space:]]+")[[1]], display = opt)
  })

  c(
    overwrite,
    seek_pre,
    inputs,
    filters,
    codecs,
    seek_post,
    output_opts,
    drops,
    map,
    list(ffm_group(object$output, quote = 1L))
  )
}

# output_targets() -------------------------------------------------------------

# The files a pipeline's `output` designates, as they exist right now.
#
# Usually that is one path. Where the output is an image2 printf PATTERN --
# sample_frames()' "<outdir>/<prefix>_%06d.png" -- it is the numbered files that
# pattern names in its own directory: one FFmpeg command fans out to many files
# there (D003), so the run's output is the SET, and `file.exists()` is false of
# the pattern itself.
#
# The pattern is matched as a REGEX built from the pattern's own text, never as
# a glob: every character outside the `%0Nd` field is escaped, so a prefix
# containing `*` or `[` matches itself and nothing else.
#
# Directories are never targets. `unlink()` leaves one alone without
# `recursive = TRUE`, so reporting it as removed would be a lie, and a directory
# sitting where a frame's file must go is a failure the run hit rather than
# something it wrote.
output_targets <- function(output) {
  # A file sitting at the literal path IS the output, whatever its name looks
  # like: a caller may legally name one "100%d.mp4", and reading that as a
  # pattern would send the search after its neighbors. Only when nothing is
  # there does the printf reading get a turn.
  if (file.exists(output) && !dir.exists(output)) return(output)
  if (!grepl("%[0-9]*d", output)) return(character(0))
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", basename(output))
  rx <- paste0("^", gsub("%[0-9]*d", "[0-9]+", escaped), "$")
  found <- list.files(dirname(output), pattern = rx, full.names = TRUE)
  found[!dir.exists(found)]
}


# output_snapshot() ------------------------------------------------------------

# What each of those files looked like at one moment: a character vector of
# "<size> <mtime>", named by path. Comparing a snapshot taken BEFORE the run
# with one taken after a failure is how the removal below tells what this run
# wrote from what it merely found -- the pre-run one has to be taken by the
# caller, since afterwards the answer is no longer recoverable.
#
# Size alone is not enough: a pre-existing zero-byte output that FFmpeg opens
# and leaves at zero bytes is distinguished only by its mtime (measured
# 2026-08-09, ffmpeg 8.1.2 macOS). Where a filesystem's timestamp resolution
# hides even that, the run leaves a zero-byte file that was already zero bytes.
#
# The mtime is recorded as EPOCH SECONDS, never as a formatted local time: a
# formatted string carries the session's timezone, so a `TZ` change or a DST
# crossing between the two snapshots would make an untouched file compare
# unequal -- and an untouched file comparing unequal is a caller's file deleted
# (M68 review). `sprintf("%.6f")` because `paste()` on the double would round
# the sub-second part away.
output_snapshot <- function(output) {
  paths <- output_targets(output)
  if (!length(paths)) return(character(0))
  info <- file.info(paths, extra_cols = FALSE)
  rlang::set_names(
    paste(info$size, sprintf("%.6f", as.numeric(info$mtime))),
    paths
  )
}


# remove_failed_output() -------------------------------------------------------

# Delete what a failed run wrote, and report what happened as cli bullets for
# the abort that follows.
#
# WHY AT ALL. FFmpeg creates its output before it knows the command will work,
# and truncates an existing one to zero on the way, so a failed run left a file
# that is empty and looks like a result (measured 2026-08-09, ffmpeg 8.1.2
# macOS: an AAC-to-MP3 stream copy exits 234 with a zero-byte output). Removing
# it here rather than in each verb keeps execution in Layer 1 once (IP1/D002);
# every verb and ffm_batch() reach this one site.
#
# WHAT IT REMOVES is what THIS RUN wrote, never what it found (D046): a target
# goes only if it is absent from `before` or its size or mtime has moved. The
# rule is not "the output path", because FFmpeg refuses an unknown encoder,
# filter or option value BEFORE opening the output and exits 8 with a
# pre-existing file byte-for-byte intact -- deleting that file was M68's own
# review defect.
#
# THE ONE EXCEPTION is `overwrite = FALSE` against a path that ALREADY EXISTED.
# The rule above already spares it on every build measured, since FFmpeg does
# not touch it; the guard stays because that is a promise the package MADE
# rather than a behavior it observes, and a build that touched the file anyway
# must not cost the caller it.
#
# `unlink(expand = FALSE)` because unlink() globs by default: an output legally
# named `a*.mp4` otherwise takes its neighbors with it (measured at M68's
# review).
#
# Returns a named character vector of cli bullets, interpolated in the calling
# frame -- so `output` must be bound there, it is the only value they reference,
# and it goes through a cli field (`{.file {output}}`), which does not recurse
# into the value: a filename containing braces would otherwise abort the message
# itself (M44's lesson).
remove_failed_output <- function(output, overwrite, before) {
  if (isFALSE(overwrite) && output %in% names(before)) {
    return(c(
      "i" = "{.file {output}} was left as it was: {.arg overwrite} is
             {.code FALSE}, so FFmpeg was told not to replace it."
    ))
  }
  after <- output_snapshot(output)
  if (!length(after)) return(character(0))
  prior <- before[names(after)]
  written <- names(after)[is.na(prior) | prior != after]

  if (!length(written)) {
    # Everything at the output is exactly as the run found it.
    if (!identical(output, names(after))) return(character(0))
    return(c(
      "i" = "{.file {output}} was left as it was: FFmpeg never wrote to it."
    ))
  }

  unlink(written, expand = FALSE)
  # unlink() signals nothing and reports failure only through its return value;
  # a read-only directory is the ordinary way it fails. Say so rather than let
  # the caller believe in a cleanup that did not happen.
  stuck <- written[file.exists(written)]
  single <- identical(written, output)
  if (length(stuck)) {
    return(c("x" = if (single) {
      "{.file {output}} could not be removed and is still there."
    } else {
      sprintf(paste("%s this run wrote for {.file {output}} could not be",
                    "removed and %s still there."),
              n_files(stuck), if (length(stuck) == 1L) "is" else "are")
    }))
  }
  c("i" = if (single) {
    "The incomplete {.file {output}} was removed."
  } else {
    sprintf("The %s this run wrote for {.file {output}} %s removed.",
            n_files(written), if (length(written) == 1L) "was" else "were")
  })
}


# n_files() --------------------------------------------------------------------

# "1 file" / "3 files". The count is baked into the bullet here rather than left
# as a cli field, so a bullet still references only `output` in the frame that
# renders it (remove_failed_output()'s contract).
n_files <- function(x) {
  sprintf("%d file%s", length(x), if (length(x) == 1L) "" else "s")
}

# ffm_run() --------------------------------------------------------------------

#' Run the FFmpeg Pipeline
#' 
#' Compile the instructions in the pipeline and run them all through FFmpeg.
#' 
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param verify An optional named list of expected output properties, passed to
#'   \code{\link{verify_media}} (e.g. \code{list(width = 1920, video_codec =
#'   "h264")}). After a successful run the output is probed and, if any check
#'   fails, \code{ffm_run()} aborts with the failed checks (mirroring how it
#'   aborts on a non-zero FFmpeg exit). \code{NULL} (default) skips verification.
#' @return A character vector of FFmpeg's standard output (with a
#'   \code{status} attribute on a non-zero exit), invisibly; called for its
#'   side effect of writing the output file. The pipeline is executed as an
#'   argument vector (never through a shell), so paths containing spaces or
#'   special characters are safe.
#' @section When FFmpeg exits non-zero:
#' A run FFmpeg refuses aborts with a condition of class
#' \code{tidymedia_ffmpeg_exit}, so a caller can catch a failed run without
#' reading the error text:
#'
#' \preformatted{
#' tryCatch(
#'   ffm_run(pipeline),
#'   tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
#' )
#' }
#'
#' The \code{tm_status} field is a length-one integer holding the exit status
#' exactly as \code{system2()} reported it — including, for a
#' signal-terminated FFmpeg, the shell's 128-plus-signal number passed through
#' unchanged, which encodes the signal rather than anything FFmpeg chose to
#' return. Two other paths raise this class and carry this field, so one handler
#' covers all three: the \code{loudnorm} analysis pass behind
#' \code{normalize_audio(two_pass = TRUE)} when FFmpeg exits non-zero, and the
#' multi-track diagnostic \code{\link{separate_audio_video}} adds to a failed
#' audio output. Each of those two names a second, narrower class ahead of this
#' one — \code{tidymedia_loudnorm_no_measurement} and
#' \code{tidymedia_multitrack_separation} respectively — which is what to catch
#' when it is that failure in particular you want.
#'
#' Two paths in the same family do \strong{not} raise this class, each for its
#' own reason. \code{normalize_audio(two_pass = TRUE)} also
#' aborts when the analysis pass exits zero and prints no parseable measurement
#' block; no non-zero exit happened there, so that abort is
#' \code{tidymedia_loudnorm_no_measurement} alone, with no
#' \code{tm_status}. And \code{normalize_audio_batch(two_pass = TRUE)} reports
#' every offending row of its analysis phase in one error, firing for rows that
#' exited zero as well as for rows FFmpeg refused — so an exit is one of its
#' causes rather than the fact it reports, and no single status could stand for
#' the mix. It too raises
#' \code{tidymedia_loudnorm_no_measurement} alone — carrying \code{tm_rows},
#' the 1-indexed offending rows, and \code{tm_row_status}, their exit statuses
#' aligned to it, with \code{NA} where the row exited zero. That shared class is
#' therefore the one handler that covers the analysis pass in both forms.
#' @seealso [ffm_compile()] to get the command without running it, [ffm_batch()]
#'   for the many-file runner, and [verify_media()] for the \code{verify =} spec.
#' @family builder functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' out <- tempfile(fileext = ".mp4")
#' ffm_files(video, out) |>
#'   ffm_scale(width = 160, height = 120) |>
#'   ffm_codec(video = "libx264") |>
#'   ffm_run(verify = list(width = 160, height = 120))
#' @export
ffm_run <- function(object, verify = NULL) {
  check_ffm(object)
  if (!is.null(verify) && !(rlang::is_list(verify) && rlang::is_named(verify))) {
    cli::cli_abort("{.arg verify} must be a named list of expected properties.")
  }
  # D074, and the reason the site is HERE rather than left to run_program():
  # that helper refuses a missing binary (`Could not locate FFmpeg.`) before it
  # resolves the limit, so on a machine with no FFmpeg this abort used to be the
  # PATH's rather than the option's. A machine-independent refusal reports before
  # a machine-dependent one (D036), so it is read at the front door.
  resolve_timeout()
  # Execute the argument vector directly (one shell-free token per argument),
  # so paths containing spaces, quotes, `$`, or backticks reach FFmpeg
  # verbatim (M06). stdin is redirected from an empty input so FFmpeg cannot
  # drain the parent process's stdin (see ffmpeg()); stderr streams to the
  # console as before.
  # Snapshot the output BEFORE running: remove_failed_output() removes what this
  # run wrote and nothing it merely found, and the difference is only visible
  # against a pre-run size and mtime (D046).
  output <- object$output
  before <- output_snapshot(output)
  # A timeout ABORTS inside run_program() rather than returning a status, so the
  # cleanup below would never be reached without this handler -- a killed FFmpeg
  # leaves exactly the half-written output D046 exists to remove. The disposition
  # is appended to the timeout's own message, so which of D046's outcomes applied
  # is stated on this path too (M69 AC5). The rule itself is untouched: the same
  # remove_failed_output() call, with the same pre-run snapshot.
  out <- rlang::try_fetch(
    run_program(find_ffmpeg(), ffm_args(object), program = "FFmpeg",
                input = "", stderr = ""),
    tidymedia_timeout = function(cnd) {
      # Read the parts off the condition rather than reusing its formatted
      # message: re-interpolating that would re-run glue over whatever the
      # message already contains (M44's brace trap). `.envir` is this handler's
      # frame, so `{.file {output}}` inside `disposition` resolves against
      # ffm_run()'s `output` up the enclosure chain, while `program` and `limit`
      # resolve to the locals below.
      program <- cnd$tm_program
      limit <- cnd$tm_limit
      disposition <- remove_failed_output(output, object$overwrite, before)
      abort_timeout(program, limit, extra = disposition,
                    .envir = environment())
    }
  )
  status <- attr(out, "status")
  if (!is.null(status)) {
    disposition <- remove_failed_output(output, object$overwrite, before)
    cli::cli_abort(
      c(
        "FFmpeg exited with status {status}.",
        "i" = "FFmpeg's error output is printed above.",
        disposition,
        "i" = "The failing command was: {.code ffmpeg {ffm_compile(object)}}"
      ),
      class = "tidymedia_ffmpeg_exit",
      tm_status = as.integer(status)
    )
  }
  if (!is.null(verify)) verify_output(object$output, verify)
  invisible(out)
}

# verify_output() ---------------------------------------------------------

# Probe a just-written output against a `verify` spec (named list) and abort
# with the failing checks if any do not pass. Shared by ffm_run(verify=); the
# batch path records outcomes instead of aborting (see ffm_batch()).
verify_output <- function(file, verify, call = rlang::caller_env()) {
  report <- do.call(verify_media, c(list(file = file), verify))
  failed <- report[!report$pass, , drop = FALSE]
  if (nrow(failed) == 0) return(invisible(report))
  bullets <- rlang::set_names(
    sprintf(
      "%s: expected %s, got %s",
      failed$check, failed$expected,
      ifelse(is.na(failed$actual), "NA", failed$actual)
    ),
    rep("x", nrow(failed))
  )
  cli::cli_abort(
    c("Output failed {nrow(failed)} verification check{?s}.", bullets),
    call = call
  )
}

# ffm_finish() -----------------------------------------------------------------

# Shared tail of the Layer 2 task verbs: compile the pipeline and, when
# run = TRUE, execute it. Returns the compiled command (invisibly after
# running) so every verb yields its reproducible command (M03 D-M03-6).
ffm_finish <- function(object, run) {
  rlang::check_bool(run)
  command <- ffm_compile(object)
  if (run) {
    ffm_run(object)
    invisible(command)
  } else {
    command
  }
}

# https://ffmpeg.org/ffmpeg-filters.html#toc-drawbox
# https://ffmpeg.org/ffmpeg-filters.html#toc-drawgrid
# https://ffmpeg.org/ffmpeg-filters.html#toc-drawtext-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-fade
# https://ffmpeg.org/ffmpeg-filters.html#toc-fillborders
# https://ffmpeg.org/ffmpeg-filters.html#toc-framestep
# https://ffmpeg.org/ffmpeg-filters.html#toc-loop
# https://ffmpeg.org/ffmpeg-filters.html#toc-subtitles-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-zoompan


# https://ffmpeg.org/ffmpeg-filters.html#toc-format-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-fps-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-rotate
# https://ffmpeg.org/ffmpeg-filters.html#toc-scale-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-thumbnail
# https://ffmpeg.org/ffmpeg-filters.html#toc-tile-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-trim
# https://ffmpeg.org/ffmpeg-filters.html#toc-vstack
# https://ffmpeg.org/ffmpeg-filters.html#overlay
# https://ffmpeg.org/ffmpeg-filters.html#toc-pad-1
# https://ffmpeg.org/ffmpeg-filters.html#toc-concat

