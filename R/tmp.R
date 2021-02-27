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
  
  #TODO: Update to allow multiple inputs?
  assert_that(rlang::is_character(input), length(input) > 0)
  assert_that(rlang::is_character(output, n = 1))
  assert_that(all(file.access(input, mode = 4) == 0),
              msg = "One or more input files was not found or readable")
  assert_that(rlang::is_logical(overwrite, n = 1))
  
  new_tmp(
    trim_start = vector("character", 0),
    trim_end = vector("character", 0),
    drop_streams = vector("character", 0),
    input = input, 
    overwrite = ifelse(overwrite, '-y ', '-n '),
    codec_video = vector("character", 0),
    codec_audio = vector("character", 0),
    pixel_format = vector("character", 0),
    filter_video = vector("character", 0),
    filter_audio = vector("character", 0),
    output = output
  )
}

# ffm() ------------------------------------------------------------------------

#' @inherit ffm_files
#' @export
ffm <- ffm_files

# ffm_trim() -------------------------------------------------------------------

#' Trim Duration in an FFmpeg Pipeline
#'
#' Make the duration of a media file shorter by selecting a segment to keep.
#'
#' @param object A \code{tmp} (tidymedia pipeline) object.
#' @param start_at A timestamp indicating where in the media file to start
#'   trimming. Either (1) a nonnegative real number indicating the timestamp in
#'   seconds or (2) a string containing an FFMPEG timestamp. (default = 0)
#' @param stop_at A timestamp indicating where in the media file to stop
#'   trimming. Either (1) a positive real number indicating the timestamp in
#'   seconds, (2) a string containing an FFMPEG timestamp, or (3) \code{NULL} to
#'   use \code{duration} instead. (default = \code{NULL})
#' @param duration The duration of the output file. Either (1) a positive real
#'   number indicating the duration in seconds, (2) a string containing an
#'   FFMPEG timestamp, or (3) \code{NULL} to use \code{stop_at} instead.
#'   (default = \code{NULL})
#' @return \code{object} but with the added instructions to trim the duration of
#'   the output file when run.
#' @references https://ffmpeg.org/ffmpeg.html
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @export
ffm_trim <- function(object, 
                     start_at = 0,
                     stop_at = NULL, 
                     duration = NULL) {
  
  # Validate arguments
  assert_that(inherits(object, "tidymedia_tmp"))
  assert_that(
    rlang::is_character(start_at, n = 1) ||
    (rlang::is_double(start_at, n = 1) && start_at >= 0)
  )
  assert_that(
    is.null(stop_at) || 
      rlang::is_character(stop_at, n = 1) ||
      (rlang::is_double(stop_at, n = 1) && stop_at > start_at)
  )
  assert_that(
    is.null(duration) || 
      rlang::is_character(duration, n = 1) ||
      (rlang::is_double(duration, n = 1) && duration > 0)
  )
  assert_that(is.null(stop_at) + is.null(duration) == 1,
              msg = "Please enter either 'stop_at' or 'duration' but not both.")
  
  # Update object
  object$trim_start <- glue('-ss {start_at} ')
  if (!is.null(stop_at)) {
    object$trim_end <- glue('-to {stop_at} ')
  } else if (!is.null(duration)) {
    object$trim_end <- glue('-t {duration} ')
  }
  
  object
}

# set_input_offset() -----------------------------------------------------------
# 
# set_input_offset <- function(object, offset, silent = FALSE) {
#   
#   #TODO: Assert that object is of class tidymedia
#   assert_that(rlang::is_bare_double(offset, n = 1))
#   assert_that(rlang::is_logical(silent, n = 1))
#   
#   # Issue overwriting warning
#   if (!silent && length(object$input_offset)) {
#     print("Overwriting input_offset information.")
#   }
#   
#   # Update object
#   object$input_offset <- paste0('-itsoffset ', offset, ' ')
#   
#   object
# }

# ffm_drop() -------------------------------------------------------------------

#' Drop Steams from an FFmpeg Pipeline
#'
#' Remove one or more specified streams from the media file. For example, remove
#' the video, audio, subtitles, or data stream from a media file.
#'
#' @param object A tidymedia pipeline (\code{tmp}) object.
#' @param streams A character vector containing one or more of the following
#'   strings: \code{"video"}, \code{"audio"}, \code{"subtitles"}, \code{"data"}
#' @return \code{object} but with the added instruction to drop one or more
#'   streams from the output file when run.
#' @export
ffm_drop <- function(object,
                     streams = c("video", "audio", "subtitles", "data")) {
  
  assert_that(inherits(object, "tidymedia_tmp"))
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
#' @param object A tidymedia pipeline (\code{tmp}) object.
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
  
  assert_that(inherits(object, "tidymedia_tmp"))
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
      (rlang::is_double(x, n = 1) && x > 0)
  )
  assert_that(
    rlang::is_character(y, n = 1) ||
      (rlang::is_double(y, n = 1) && y > 0)
  )

  idx <- substr(object$filter_video, 1, 4) == "crop"
  cmd <- glue('crop=w={width}:h={height}:x={x}:y={y}')
  if (sum(idx) > 0) {
    object$filter_video[[idx]] <- cmd
  } else {
    object$filter_video <- c(object$filter_video, cmd)
  }

  object
}

# ffm_scale() ------------------------------------------------------------------

#' Scale (Resize) Frames in a FFmpeg Pipeline
#'
#' Scale (resize) the input video's frames to either a specific width and height
#' (in pixels) or using an FFmpeg expression.
#'
#' @param object A tidymedia pipeline (\code{tmp}) object.
#' @param width The width of the output video (in pixels). Either (1) a positive
#'   real number or (2) a string that contains an FFmpeg expression.
#' @param height The height of the output video (in pixels). Either (1) a
#'   positive real number or (2) a string that contains an FFmpeg expression.
#' @return \code{object} but with the added instruction to crop the image(s).
#' @export
ffm_scale <- function(object, width, height) {

  assert_that(inherits(object, "tidymedia_tmp"))
  assert_that(
    rlang::is_character(width, n = 1) ||
      (rlang::is_double(width, n = 1) && width > 0)
  )
  assert_that(
    rlang::is_character(height, n = 1) ||
      (rlang::is_double(height, n = 1) && height > 0)
  )

  idx <- substr(object$filter_video, 1, 5) == "scale"
  cmd <- glue('scale=w={width}:h={height}')
  if (sum(idx) > 0) {
    object$filter_video[[idx]] <- cmd
  } else {
    object$filter_video <- c(object$filter_video, cmd)
  }
  
  object
}

# ffm_codec() ------------------------------------------------------------------

#' Set Codecs in an FFmpeg Pipeline
#'
#' Set the audio and/or video codecs for the output file. Note that you can use
#' the command \code{get_codecs()} to see a list of the codecs included in your
#' FFmpeg version.
#'
#' @param object A tidymedia pipeline (\code{tmp}) object
#' @param audio A string indicating which audio codec to use.
#' @param codec A string indicating which video codec to use.
#' @return \code{object} but with the added instruction to change the codec(s).
#' @references
#' @export
ffm_codec <- function(object,
                      audio = NULL,
                      video = NULL) {
  
  assert_that(inherits(object, "tidymedia_tmp"))
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

# ffm_pixel_format() ------------------------------------------------------

#' Set the Pixel Format in an FFmpeg Pipeline
#' 
#' 
#' @param object A tidymedia pipeline object.
#' @param format A string indicating the pixel format for the output file.
#' @export
ffm_pixel_format <- function(object, format) {
  
  assert_that(inherits(object, "tidymedia_tmp"))
  assert_that(rlang::is_character(format, n = 1))
  # TODO: Validate format argument
  
  object$pixel_format <- paste0('-pix_fmt ', format)
  
  object
}


# ffm_hstack() -----------------------------------------------------------------

#' Horizontally Stack Multiple Videos in an FFmpeg Pipeline
#'
#' Add a complex video filter to stack multiple videos horizontally
#' (side-by-side) and, optionally, resize them to have the same height.
#'
#' @param object A tidymedia pipeline (\code{tmp}) object containing two or more
#'   inputs with the same height (or two inputs with different heights)
#' @param shortest A logical indicating whether to trim the duration of all
#'   videos to that of the shortest video (default = \code{FALSE})
#' @return \code{object} but with the added instruction to apply horizontal
#'   stacking.
#' @export
ffm_hstack <- function(object,
                       shortest = FALSE,
                       resize = FALSE) {
  
  shortest_int <- as.integer(shortest)
  inputs_n <- length(object$input)
  
  # TODO: Validate arguments
  assert_that(inherits(object, "tidymedia_tmp"))
  assert_that(rlang::is_logical(shortest, n = 1))
  assert_that(inputs_n > 1)
  assert_that(rlang::is_logical(resize, n = 1))
  assert_that(resize == FALSE || (resize == TRUE && inputs_n == 2))
  
  idx <- substr(object$filter_video, 1, 6) == "hstack"
  if (resize == TRUE) {
    cmd <- glue("[0][1]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[0s][1s];
        [1s][0s]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[1s][0s];
        [0s][1s]hstack,setsar=1")
  } else {
    cmd <- glue('hstack=inputs={inputs_n}:shortest={shortest_int}')
  }
  if (sum(idx) > 0) {
    object$filter_video[[idx]] <- cmd
  } else {
    object$filter_video <- c(object$filter_video, cmd)
  }
  
  object
}


# ffm_compile() ----------------------------------------------------------------

#' Compile the tidymedia pipeline into FFmpeg command
#'
#' Compile all the instructions into a string representing the FFmpeg command
#' needed to run it.
#'
#' @param object A tidymedia pipeline (\code{tmp}) object.
#' @return A string containing the FFmpeg command needed to execute all the
#'   instructions provided to the tidymedia pipeline.
#' @export
ffm_compile <- function(object) {
  
  assert_that(inherits(object, "tidymedia_tmp"))
  
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
  
  input_string <- paste0(glue('-i "{object$input}"', sep = ""), collapse = " ")
  
  command <- paste0(
    object$trim_start, 
    object$input_offset, 
    object$trim_end, 
    object$drop_streams,
    input_string, " ", 
    object$overwrite,
    object$codec_video, 
    object$codec_audio, 
    object$pixel_format,
    vf, 
    va, 
    '"', object$output, '"'
  )
  
  command
}

# ffm_run() --------------------------------------------------------------------

#' @export
ffm_run <- function(object) {
  assert_that(inherits(object, "tidymedia_tmp"))
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

