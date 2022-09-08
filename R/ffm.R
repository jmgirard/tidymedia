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
#' @aliases ffm
#' @export
ffm_files <- function(input, output, overwrite = TRUE) {
  
  assert_that(rlang::is_character(input), length(input) > 0)
  assert_that(rlang::is_character(output, n = 1))
  assert_that(all(file.access(input, mode = 4) == 0),
              msg = "One or more input files was not found or not readable")
  assert_that(rlang::is_logical(overwrite, n = 1))
  
  new_ffm(
    drop_streams = vector("character", 0),
    input = input, 
    overwrite = ifelse(overwrite, '-y ', '-n '),
    codec_video = vector("character", 0),
    codec_audio = vector("character", 0),
    pixel_format = vector("character", 0),
    filter_video = vector("character", 0),
    filter_audio = vector("character", 0),
    map = vector("character", 0),
    output = output
  )
}

# ffm() ------------------------------------------------------------------------

#' @inherit ffm_files
#' @export
ffm <- ffm_files

# ffm_trim <- function(object, 
#                      start_at = 0,
#                      stop_at = NULL, 
#                      duration = NULL) {
#   
#   # Validate arguments
#   assert_that(inherits(object, "tidymedia_ffm"))
#   assert_that(
#     rlang::is_character(start_at, n = 1) ||
#     (rlang::is_double(start_at, n = 1) && start_at >= 0)
#   )
#   assert_that(
#     is.null(stop_at) || 
#       rlang::is_character(stop_at, n = 1) ||
#       (rlang::is_double(stop_at, n = 1) && stop_at > start_at)
#   )
#   assert_that(
#     is.null(duration) || 
#       rlang::is_character(duration, n = 1) ||
#       (rlang::is_double(duration, n = 1) && duration > 0)
#   )
#   assert_that(is.null(stop_at) + is.null(duration) == 1,
#               msg = "Please enter either 'stop_at' or 'duration' but not both.")
#   
#   # Update object
#   object$trim_start <- glue('-ss {start_at} ')
#   if (!is.null(stop_at)) {
#     object$trim_end <- glue('-to {stop_at} ')
#   } else if (!is.null(duration)) {
#     object$trim_end <- glue('-t {duration} ')
#   }
#   
#   object
# }


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
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(is.null(start) || length(start) == 1)
  assert_that(is.null(end) || length(end) == 1)
  assert_that(is.null(duration) || length(duration) == 1)
  units <- match.arg(units)
  assert_that(rlang::is_logical(setpts, n = 1))
  
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

  # add setpts if requested
  object$filter_video <- c(object$filter_video, "setpts=PTS-STARTPTS")
  
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
  
  assert_that(inherits(object, "tidymedia_ffm"))
  streams <- match.arg(streams, several.ok = TRUE)

  # Update object
  vn <- ifelse("video" %in% streams, "-vn ", "")
  an <- ifelse("audio" %in% streams, "-an ", "")
  sn <- ifelse("subtitles" %in% streams, "-sn ", "")
  dn <- ifelse("data" %in% streams, "-dn ", "")
  object$drop_streams <- paste0(vn, an, sn, dn)
  
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
  
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(
    rlang::is_character(width, n = 1) || 
      (rlang::is_double(width, n = 1) && width > 0)
  )
  assert_that(
    rlang::is_character(height, n = 1) ||
      (rlang::is_double(height, n = 1) && height > 0)
  )
  assert_that(
    rlang::is_character(x, n = 1) ||
      (rlang::is_double(x, n = 1) && x >= 0)
  )
  assert_that(
    rlang::is_character(y, n = 1) ||
      (rlang::is_double(y, n = 1) && y >= 0)
  )

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

  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(
    rlang::is_character(width, n = 1) ||
      (rlang::is_double(width, n = 1) && width > 0)
  )
  assert_that(
    rlang::is_character(height, n = 1) ||
      (rlang::is_double(height, n = 1) && height > 0)
  )

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
  
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(is.null(audio) || rlang::is_character(audio, n = 1))
  assert_that(is.null(video) || rlang::is_character(video, n = 1))
  # TODO: Check codec against the list from get_codecs() and get_encoders()?
  
  if (is.null(audio) == FALSE) {
    object$codec_audio <- glue('-codec:a {audio} ')
  } 
  if (is.null(video) == FALSE) {
    object$codec_video <- glue('-codec:v {video} ')
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
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(rlang::is_character(mapping, n = 1))
  
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
#' @export
ffm_copy <- function(object, audio = TRUE, video = TRUE, streams = TRUE) {
  
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(rlang::is_logical(audio))
  assert_that(rlang::is_logical(video))
  assert_that(rlang::is_logical(streams))
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
  
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(rlang::is_character(format, n = 1))
  # TODO: Validate format argument
  
  object$pixel_format <- glue('-pix_fmt {format}')
  
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
  
  shortest_int <- as.integer(shortest)
  inputs_n <- length(object$input)
  
  # TODO: Validate arguments
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(rlang::is_logical(shortest, n = 1))
  assert_that(inputs_n > 1)
  assert_that(rlang::is_logical(resize, n = 1))
  assert_that(resize == FALSE || (resize == TRUE && inputs_n == 2))
  
  if (resize == TRUE) {
    cmd <- glue("[0][1]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[0s][1s];
        [1s][0s]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[1s][0s];
        [0s][1s]hstack,setsar=1")
  } else {
    cmd <- glue('hstack=inputs={inputs_n}:shortest={shortest_int}')
  }

  object$filter_video <- c(object$filter_video, cmd)

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
  
  assert_that(inherits(object, "tidymedia_ffm"))
  assert_that(rlang::is_character(x, n = 1) || 
                (rlang::is_double(x, n = 1) && x >= 0))
  assert_that(rlang::is_character(y, n = 1) || 
                (rlang::is_double(y, n = 1) && y >= 0))
  assert_that(rlang::is_character(width, n = 1) || 
                (rlang::is_double(width, n = 1) && width > 0))
  assert_that(rlang::is_character(height, n = 1) || 
                (rlang::is_double(height, n = 1) && height > 0))
  assert_that(rlang::is_character(color, n = 1))
  assert_that(rlang::is_character(thickness, n = 1) ||
                (rlang::is_double(thickness, n = 1) && thickness > 0))
  
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
  
  assert_that(inherits(object, "tidymedia_ffm"))
  
  if (length(object$filter_video)) {
    vf <- paste0(
      '-filter_complex:v "', 
      paste(object$filter_video, collapse = ','), 
      '" '
    )
  } else {
    vf <- ''
  }
  
  if (length(object$filter_audio)) {
    va <- paste0(
      '-filter_complex:a "', 
      paste(object$filter_audio, collapse = ','), 
      '" '
    )
  } else {
    va <- ''
  }
  
  if (length(object$map)) {
    map <- paste0('-map ', object$map, ' ')
  } else {
    map <- ''
  }
  
  input_string <- paste0(glue('-i "{object$input}"', sep = ""), collapse = " ")
  
  command <- paste0(
    object$drop_streams,
    input_string, " ", 
    object$overwrite,
    object$codec_video, 
    object$codec_audio, 
    object$pixel_format,
    vf, 
    va, 
    map,
    '"', object$output, '"'
  )
  
  command
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
  assert_that(inherits(object, "tidymedia_ffm"))
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

