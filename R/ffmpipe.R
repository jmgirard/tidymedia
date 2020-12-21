#' @export
tidymedia <- function(input, output, overwrite = TRUE) {
  
  #TODO: Validate arguments
  
  out <- list(
    trim_start = vector("character", 0),
    input_offset = vector("character", 0),
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
  
  #TODO: set class
  
  out
}

# trim_duration() ---------------------------------------------------------

#' Trim Duration
#'
#' Make the duration of a media file shorter by selecting a segment to keep.
#'
#' @param object A \code{tidymedia} object.
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
#' @param silent A logical indicating whether to suppress messages to the
#'   console when overwriting a previously specified \code{trim_duration()}
#'   call. (default = \code{FALSE})
#' @return \code{object} but with the added instructions to trim the duration of
#'   the output file when run.
#' @references https://ffmpeg.org/ffmpeg.html
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @export
trim_duration <- function(object, 
                          start_at = 0,
                          stop_at = NULL, 
                          duration = NULL,
                          silent = FALSE) {
  
  # Validate arguments
  #TODO: Assert that object is of class tidymedia
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
      (rlang::is_double(duraiton, n = 1) && duration > 0)
  )
  assert_that(rlang::is_logical(silent, n = 1))
  assert_that(is.null(stop_at) + is.null(duration) == 1,
              msg = "Please enter either 'stop_at' or 'duration' but not both.")
  
  # Issue overwriting warning
  if (!silent && length(object$trim_start) == 1) {
    print("Overwriting 'trim_duration' instructions")
  }
  
  # Update object
  object$trim_start <- glue('-ss {start_at} ')
  if (!is.null(stop_at)) {
    object$trim_end <- glue('-to {stop_at} ')
  } else if (!is.null(duration)) {
    object$trim_end <- glue('-t {duration} ')
  }
  
  object
}

# # set_input_offset() ------------------------------------------------------
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

# drop_streams() ----------------------------------------------------------

#' Drop one of more streams
#'
#' Remove one or more specified streams from the media file. For example, remove
#' the video, audio, subtitles, or data stream from a media file.
#'
#' @param object A \code{tidymedia} object.
#' @param streams A character vector containing one or more of the following
#'   strings: \code{"video"}, \code{"audio"}, \code{"subtitles"}, \code{"data"}
#' @param silent A logical indicating whether to suppress messages to the
#'   console when overwriting a previously specified \code{drop_streams()} call.
#'   (default = \code{FALSE})
#' @return \code{object} but with the added instruction to drop one or more
#'   streams from the output file when run.
#' @export
drop_streams <- function(object, 
                         streams = c("video", "audio", "subtitles", "data"),
                         silent = FALSE) {
  
  #TODO: Assert that object is of class tidymedia
  streams <- match.arg(streams, several.ok = TRUE)
  assert_that(rlang::is_logical(silent, n = 1))
  
  # Issue overwriting warning
  if (!silent && length(object$drop_streams) > 0) {
    print("Overwriting 'drop_streams' instructions")
  }
  
  # Update object
  vn <- ifelse("video" %in% streams, "-vn ", "")
  an <- ifelse("audio" %in% streams, "-an ", "")
  sn <- ifelse("subtitles" %in% streams, "-sn ", "")
  dn <- ifelse("data" %in% streams, "-dn ", "")
  object$drop_streams <- paste0(vn, an, sn, dn)
  
  object
}

# crop_frames() -----------------------------------------------------------

#' Crop frames
#'
#' Decrease the size of the video's frames by cropping it.
#'
#' @param object A \code{tidymedia} object.
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
#' @param silent A logical indicating whether to suppress messages to the
#'   console when overwriting a previously specified \code{crop_frames()} call.
#'   (default = \code{FALSE})
#' @return \code{object} but with the added instruction to crop the image(s).
#' @references https://ffmpeg.org/ffmpeg-filters.html#toc-crop
#' @export
crop_frames <- function(object, 
                        width, 
                        height, 
                        x = "(in_w-out_w)/2", 
                        y = "(in_h-out_h)/2", 
                        silent = FALSE) {
  
  # TODO: Assert that object is of class tidymedia
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
  assert_that(rlang::is_logical(silent, n = 1))
  
  idx <- substr(object$filter_video, 1, 4) == "crop"
  cmd <- glue('crop=w={width}:h={height}:x={x}:y={y}')
  if (sum(idx) > 0) {
    object$filter_video[[idx]] <- cmd
    if (!silent) print("Overwriting 'crop_frames' instructions")
  } else {
    object$filter_video <- c(object$filter_video, cmd)
  }

  object
}

# resize_frames() ---------------------------------------------------------

#' Resize frames
#'
#' Resize or scale the input video.
#'
#' @param object A \code{tidymedia} object.
#' @param width The width of the output video (in pixels). Either (1) a positive
#'   real number or (2) a string that contains an FFMPEG expression.
#' @param height The height of the output video (in pixels). Either (1) a
#'   positive real number or (2) a string that contains an FFMPEG expression.
#' @param silent A logical indicating whether to suppress messages to the
#'   console when overwriting a previously specified \code{resize_frames()}
#'   call. (default = \code{FALSE})
#' @return \code{object} but with the added instruction to crop the image(s).
#' @aliases scale_frames
#' @export
resize_frames <- function(object, width, height, silent = FALSE) {

  #TODO: Assert that object is of class tidymedia
  assert_that(
    rlang::is_character(width, n = 1) ||
      (rlang::is_double(width, n = 1) && width > 0)
  )
  assert_that(
    rlang::is_character(height, n = 1) ||
      (rlang::is_double(height, n = 1) && height > 0)
  )
  assert_that(rlang::is_logical(silent, n = 1))
  
  idx <- substr(object$filter_video, 1, 5) == "scale"
  cmd <- glue('scale=w={width}:h={height}')
  if (sum(idx) > 0) {
    object$filter_video[[idx]] <- cmd
    if (!silent) print("Overwriting 'resize_frames' instructions")
  } else {
    object$filter_video <- c(object$filter_video, cmd)
  }
  
  object
}

# scale_frames() ----------------------------------------------------------

#' @inherit resize_frames
#' @export
scale_frames <- function(object, width, height, silent = FALSE) {
  resize_frames(object, width, height, silent)
}

# set_codec() -------------------------------------------------------------

#' @export
set_codec <- function(object, stream = c("audio", "video"), codec) {
  #TODO: Validate arguments
  
  stream <- match.arg(stream)
  
  if (stream == "audio") {
    object$codec_audio <- paste0('-codec:a ', codec, ' ')
  } else if (stream == "video") {
    object$codec_video <- paste0('-codec:v ', codec, ' ')
  }

  object
}

# set_pixel_format() ------------------------------------------------------

#' @export
set_pixel_format <- function(object, format) {
  
  # TODO: Validate arguments
  
  object$pixel_format <- paste0('-pix_fmt ', format)
  object
}

# compile_command() -------------------------------------------------------

#' @export
compile_command <- function(object) {
  
  if (length(object$filter_video)) {
    vf <- paste0(
      '-filter:v "', 
      paste(object$filter_video, collapse = ','), 
      '" '
    )
  } else {
    vf <- ''
  }
  
  if (length(object$filter_audio)) {
    va <- paste0(
      '-filter:a "', 
      paste(object$filter_audio, collapse = ','), 
      '" '
    )
  } else {
    va <- ''
  }
  
  command <- paste0(
    object$trim_start, 
    object$input_offset, 
    object$trim_end, 
    object$drop_streams,
    '-i "', object$input, '" ', 
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

# run_ffmpeg() ------------------------------------------------------------

#' @export
run_ffmpeg <- function(object) {
  command <- compile_command(object)
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

