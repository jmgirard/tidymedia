#' @export
set_files <- function(input, output, overwrite = TRUE) {
  
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

#' @export
trim_duration <- function(object, 
                       start_at = NULL, start_eof = NULL, 
                       stop_at = NULL, duration = NULL,
                       silent = FALSE) {
  
  #TODO: Validate arguments
  assert_that(rlang::is_logical(silent, n = 1))
  # Note that start_eof needs to be negative (0 = eof)
  
  if (!silent && length(object$trim_start)) {
    print("Overwriting trim_duration information.")
  }
  
  if (is.null(start_at) == FALSE) {
    object$trim_start <- paste0('-ss ', start_at, ' ')
  } else if (is.null(start_eof) == FALSE) {
    object$trim_start <- paste0('-sseof ', start_eof, ' ')
  }
  
  if (is.null(stop_at) == FALSE) {
    object$trim_end <- paste0('-to ', stop_at,  ' ')
  } else if (is.null(duration) == FALSE) {
    object$trim_end <- paste0('-t ', duration, ' ')
  }
  
  object
}

#' @export
set_input_offset <- function(object, offset, silent = FALSE) {
  
  #TODO: Validate arguments
  assert_that(rlang::is_logical(silent, n = 1))
  
  if (!silent && length(object$input_offset)) {
    print("Overwriting input_offset information.")
  }
  
  # TODO: Validate arguments
  object$input_offset <- paste0('-itsoffset ', offset, ' ')
  object
}

# drop_streams() ----------------------------------------------------------

#' @export
drop_streams <- function(object, 
                         streams = c("video", "audio", "subtitles", "data")) {
  
  #TODO: Validate object
  streams <- match.arg(streams, several.ok = TRUE)
  streams <- unique(streams)
  
  current <- object$drop_streams
  
  if (("video" %in% streams) && (!"-vn" %in% current)) {
    object$drop_streams <- c(object$drop_streams, "-vn ")
  }
  
  if (("audio" %in% streams) && (!"-an" %in% current)) {
    object$drop_streams <- c(object$drop_streams, "-an ")
  }
  
  if (("subtitles" %in% streams) && (!"-sn" %in% current)) {
    object$drop_streams <- c(object$drop_streams, "-sn ")
  }
  
  if (("data" %in% streams) && (!"-dn" %in% current)) {
    object$drop_streams <- c(object$drop_streams, "-dn ")
  }
  
  object
}

# crop_frames() -----------------------------------------------------------

#' @references https://ffmpeg.org/ffmpeg-filters.html#toc-crop
#' @export
crop_frames <- function(object, width, height, x, y) {
  # TODO: Validate arguments
  
  object$filter_video <- c(
    object$filter_video,
    paste0('crop=', width, ':', height, ':', x, ':', y)
  )
  
  object
}

#' @references https://ffmpeg.org/ffmpeg-filters.html#toc-cropdetect
#' @export
apply_autocrop <- function(object, limit = 24, round = 16, reset = 0) {
  # TODO: Validate arguments
  
  object$filter_video <- c(
    object$filter_video, 
    paste0('cropdetect=limit=', limit, ':round=', round, ':reset=', reset)
  )
  
  object
}

#' @export
resize_frames <- function(object, width, height) {
  # TODO: Validate arguments
  
  object$filter_video <- c(
    object$filter_video, 
    paste0('scale=', width, ':', height)
  )
  
  object
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
    object$trim_start, object$input_offset, object$trim_end,
    paste0(object$drop_streams),
    '-i "', object$input, '" ', object$overwrite,
    object$codec_video, object$codec_audio, object$pixel_format,
    vf, va, '"', object$output, '"'
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

