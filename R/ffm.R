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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
  unreadable <- input[file.access(input, mode = 4) != 0]
  if (length(unreadable) > 0) {
    cli::cli_abort(c(
      "Can't find or read {length(unreadable)} input file{?s}.",
      "x" = "Not readable: {.file {unreadable}}."
    ))
  }

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

# ffm() ------------------------------------------------------------------------

#' @inherit ffm_files
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
#'   ffm_compile()
#' @export
ffm <- ffm_files

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
#' @references https://ffmpeg.org/ffmpeg-filters.html#trim
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
#' @references https://ffmpeg.org/ffmpeg.html#Main-options
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Fast, lossless copy cut (snaps to keyframes)
#' ffm(video, "output.mp4") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Drop the audio stream (keep video only)
#' ffm(video, "output.mp4") |>
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
#' @references https://ffmpeg.org/ffmpeg-filters.html#toc-crop
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Crop to a centered 160x120 region
#' ffm(video, "output.mp4") |>
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
#' @return \code{object} but with the added instruction to crop the image(s).
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param target_loudness The target integrated loudness, in LUFS (a number in
#'   \code{-70}..\code{-5}; default \code{-23}, the EBU R128 target).
#' @param true_peak The maximum true peak, in dBTP (a number in \code{-9}..\code{0};
#'   default \code{-1}, the EBU R128 ceiling).
#' @param loudness_range The target loudness range, in LU (a number in
#'   \code{1}..\code{50}; default \code{7}).
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
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' \url{https://ffmpeg.org/ffmpeg-filters.html#loudnorm}
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
  rlang::check_number_decimal(target_loudness, min = -70, max = -5)
  rlang::check_number_decimal(true_peak, min = -9, max = 0)
  rlang::check_number_decimal(loudness_range, min = 1, max = 50)
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
  object$filter_audio <- c(object$filter_audio, cmd)

  object
}

# ffm_codec() ------------------------------------------------------------------

#' Set Codecs in an FFmpeg Pipeline
#'
#' Set the audio and/or video codecs for the output file. Note that you can use
#' the command \code{get_codecs()} to see a list of the codecs included in your
#' FFmpeg version.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param audio A string indicating which audio codec to use or \code{NULL} to
#'   only set the video codec. default = \code{NULL}
#' @param video A string indicating which video codec to use or \code{NULL} to
#'   only set the audio codec. default = \code{NULL}
#' @return \code{object} but with the added instruction to change the codec(s).
#' @references https://ffmpeg.org/ffmpeg-codecs.html
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
#' input. When the pipeline uses a multi-input verb (e.g.
#' \code{\link{ffm_hstack}}), the explicit mapping is added \emph{alongside}
#' the automatic \code{-map "[vout]"} of the filtered stream — for example,
#' \code{ffm_map(object, "0:a")} keeps the first input's audio next to the
#' stacked video.
#'
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param mapping A string determining the stream mapping.
#' @return \code{object} with the added stream mapping instruction.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
#'   ffm_map(mapping = "0") |>
#'   ffm_compile()
#' @export
ffm_map <- function(object, mapping = "0") {
  check_ffm(object)
  rlang::check_string(mapping)

  object$map <- mapping
  
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
#' @param audio A logical indicating whether to copy the audio codec.
#'   (default = \code{TRUE})
#' @param video A logical indicating whether to copy the video codec.
#'   (default = \code{TRUE})
#' @param streams A logical indicating whether to map all streams from the
#'   input (via \code{ffm_map(mapping = "0")}). (default = \code{TRUE})
#' @return \code{object} with the added instruction to copy codecs and/or map
#'   all streams.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
    object <- ffm_map(object, mapping = "0")
  }
  
  object
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Stack two inputs side-by-side (pass more than one input to ffm())
#' ffm(c(video, video), "output.mp4") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Stack two inputs one above the other (pass more than one input to ffm())
#' ffm(c(video, video), "output.mp4") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Draw the second input over the first, 16px in from the top-right corner
#' ffm(c(video, video), "output.mp4") |>
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
  if (!is.null(scale) && (scale <= 0 || scale > 1)) {
    cli::cli_abort("{.arg scale} must be greater than 0 and at most 1.")
  }
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Join two inputs end-to-end (they must share codec/resolution/frame rate)
#' ffm(c(video, video), "output.mp4") |>
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
#' @references https://ffmpeg.org/ffmpeg-filters.html#drawbox
#' @references https://ffmpeg.org/ffmpeg-utils.html#color-syntax
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Draw a filled red box covering the top-left quarter of the frame
#' ffm(video, "output.mp4") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Extract a single frame by adding a raw output option
#' ffm(video, "frame.png") |>
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
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # ffm_compile() returns the reproducible FFmpeg command as a string
#' ffm(video, "output.mp4") |>
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
    map <- c(
      list(ffm_group(c("-map", "[vout]"), quote = 2L)),
      lapply(object$map, function(m) ffm_group(c("-map", m)))
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
    map <- lapply(object$map, function(m) ffm_group(c("-map", m)))
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
#' @family builder functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' out <- tempfile(fileext = ".mp4")
#' ffm(video, out) |>
#'   ffm_scale(width = 160, height = 120) |>
#'   ffm_codec(video = "libx264") |>
#'   ffm_run(verify = list(width = 160, height = 120))
#' @export
ffm_run <- function(object, verify = NULL) {
  check_ffm(object)
  if (!is.null(verify) && !(rlang::is_list(verify) && rlang::is_named(verify))) {
    cli::cli_abort("{.arg verify} must be a named list of expected properties.")
  }
  # Execute the argument vector directly (one shell-free token per argument),
  # so paths containing spaces, quotes, `$`, or backticks reach FFmpeg
  # verbatim (M06). stdin is redirected from an empty input so FFmpeg cannot
  # drain the parent process's stdin (see ffmpeg()); stderr streams to the
  # console as before.
  out <- run_program(find_ffmpeg(), ffm_args(object), program = "FFmpeg",
                     input = "", stderr = "")
  status <- attr(out, "status")
  if (!is.null(status)) {
    cli::cli_abort(c(
      "FFmpeg exited with status {status}.",
      "i" = "FFmpeg's error output is printed above.",
      "i" = "The failing command was: {.code ffmpeg {ffm_compile(object)}}"
    ))
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

