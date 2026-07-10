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
#' @export
ffm_scale <- function(object, width, height) {

  check_ffm(object)
  check_dim(width)
  check_dim(height)

  cmd <- glue('scale=w={width}:h={height}')
  object$filter_video <- c(object$filter_video, cmd)

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
#' @export
ffm_codec <- function(object,
                      audio = NULL,
                      video = NULL) {
  
  check_ffm(object)
  if (!is.null(audio)) rlang::check_string(audio)
  if (!is.null(video)) rlang::check_string(video)
  # TODO: Check codec against the list from get_codecs() and get_encoders()?
  
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
#' Description
#' 
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param mapping A string determining the stream mapping.
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
#' Description
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
#' 
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @param format A string indicating the pixel format for the output file.
#' @export
ffm_pixel_format <- function(object, format) {
  
  check_ffm(object)
  rlang::check_string(format)
  # TODO: Validate format argument
  
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
#'   videos to match (takes longer and currently only works with two inputs)
#' @return \code{object} but with the added instruction to apply horizontal
#'   stacking.
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
  # Stacking is a whole-frame operation that consumes the raw input pads, so it
  # must precede any single-input video filter — otherwise ffm_compile() would
  # feed two pads to a one-input filter and produce an invalid graph.
  if (length(object$filter_video) > 0) {
    cli::cli_abort(c(
      "Stacking must come before other video filters.",
      "i" = "Apply {.fn ffm_hstack} first, then filter the stacked result."
    ))
  }

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
#'   \code{"invert"} is used, the box color is teh same as tehv ideo with
#'   inverted luma. (default = \code{"black"})
#' @param thickness A thickness of the box edge (in pixels). A value of
#'   \code{"fill"} will create a filled box. (default = \code{"fill"})
#' @return \code{object} but with the added instruction to apply the drawbox
#'   filter.
#' @references https://ffmpeg.org/ffmpeg-filters.html#drawbox
#' @references https://ffmpeg.org/ffmpeg-utils.html#color-syntax
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
#' @export
ffm_compile <- function(object) {

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

  # Global options (before the inputs).
  overwrite <- if (isTRUE(object$overwrite)) "-y" else "-n"

  # Inputs.
  inputs <- paste0('-i "', object$input, '"')

  # Filters and stream mapping. Single-input sequential chains compile to
  # -vf/-af; any multi-input (blessed) verb sets `complex` and compiles to
  # -filter_complex with explicit labels plus an auto -map (M02 D-M02-2).
  filters <- character()
  map <- character()
  if (isTRUE(object$complex)) {
    body <- paste(object$filter_video, collapse = ",")
    # A verb that manages its own stream labels starts the graph with "[..]";
    # otherwise prepend one video pad per input.
    if (!startsWith(body, "[")) {
      labels <- paste0("[", seq_along(object$input) - 1L, ":v]", collapse = "")
      body <- paste0(labels, body)
    }
    filters <- paste0('-filter_complex "', body, '[vout]"')
    map <- '-map "[vout]"'
  } else {
    if (length(object$filter_video)) {
      filters <- c(
        filters,
        paste0('-vf "', paste(object$filter_video, collapse = ","), '"')
      )
    }
    if (length(object$filter_audio)) {
      filters <- c(
        filters,
        paste0('-af "', paste(object$filter_audio, collapse = ","), '"')
      )
    }
    if (length(object$map)) {
      map <- paste0("-map ", object$map)
    }
  }

  # Output options (after the inputs, before the output file).
  codec_video <- if (length(object$codec_video)) {
    paste0("-codec:v ", object$codec_video)
  } else {
    character()
  }
  codec_audio <- if (length(object$codec_audio)) {
    paste0("-codec:a ", object$codec_audio)
  } else {
    character()
  }
  pixel_format <- if (length(object$pixel_format)) {
    paste0("-pix_fmt ", object$pixel_format)
  } else {
    character()
  }
  drop_flags <- c(
    if ("video" %in% object$drop) "-vn",
    if ("audio" %in% object$drop) "-an",
    if ("subtitles" %in% object$drop) "-sn",
    if ("data" %in% object$drop) "-dn"
  )

  tokens <- c(
    overwrite,
    inputs,
    filters,
    codec_video,
    codec_audio,
    pixel_format,
    drop_flags,
    map,
    paste0('"', object$output, '"')
  )

  paste(tokens, collapse = " ")
}

# ffm_run() --------------------------------------------------------------------

#' Run the FFmpeg Pipeline
#' 
#' Compile the instructions in the pipeline and run them all through FFmpeg.
#' 
#' @param object An ffmpeg pipeline (\code{ffm}) object created by
#'   \code{ffm_files()}.
#' @export
ffm_run <- function(object) {
  check_ffm(object)
  command <- ffm_compile(object)
  ffmpeg(command)
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

