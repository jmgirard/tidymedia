
# ffmpeg() ----------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  assert_that(rlang::is_character(command, n = 1))
  out <- system(glue('"{find_ffmpeg()}" {command}'), intern = TRUE)
  out
}

# extract_frames() --------------------------------------------------------

#' @export
extract_frame <- function(infile, outfile, timestamp = NULL, frame = NULL) {
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile, n = 1))
  assert_that(rlang::is_null(timestamp) || 
                rlang::is_double(timestamp, n = 1, finite = TRUE))
  assert_that(rlang::is_null(frame) || 
                rlang::is_integerish(frame, n = 1, finite = TRUE))
  assert_that(sum(rlang::is_null(timestamp), rlang::is_null(frame)) == 1,
              msg = "Please provide either timestamp or frame.")
  
  if (rlang::is_null(timestamp)) timestamp <- frame / get_framerate(infile)
  
  pre <- glue('-ss {timestamp}')
  post <- glue('-qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal')
  command <- glue('{pre} -i "{input}" {post} "{output}"')
  ffmpeg(command)
}

# extract_audio() ---------------------------------------------------------

#' @export
extract_audio <- function(infile, outfile, options = "-acodec copy") {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile, n = 1))
  assert_that(rlang::is_character(options, n = 1))
  
  command <- glue('-i "{infile}" {options} -vn "{outfile}"')
  ffmpeg(command)
}

# crop_video() ------------------------------------------------------------

#' @export
crop_video <- function(infile, outfile, width, height, x, y, arg) {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile))
  assert_that(rlang::is_integerish(width, n = 1))
  assert_that(rlang::is_integerish(height, n = 1))
  assert_that(rlang::is_integerish(x, n = 1))
  assert_that(rlang::is_integerish(y, n = 1))
  
  command <- glue(
    '-i "{infile}" -filter:v "crop={width}:{height}:{x}:{y}" {arg} "{outfile}"'
  )
  
  ffmpeg(command)
}


# format_for_web() --------------------------------------------------------

#' @export
format_for_web <- function(infile, outfile) {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile, n = 1))
  
  command <- glue(
    '-i "{infile}" -pix_fmt yuv420p -c:v libx264 -movflags +faststart ',
    '-filter:v crop="floor(in_w/2)*2:floor(in_h/2)*2" -c:a aac "{outfile}"'
  )
  
  ffmpeg(command)

}


# get_codecs() ------------------------------------------------------------

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
#' @export
#' @examples
#' \dontrun{
#'
#' get_codecs()
#' get_codecs(sort_by_type = FALSE)
#' }
#' @export
get_codecs <- function(sort_by_type = TRUE) {
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

# get_encoders() ------------------------------------------------------------

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
#' @export
#' @examples 
#' \dontrun{
#' 
#' get_encoders()
#' get_encoders(sort_by_type = FALSE)
#' }
get_encoders <- function(sort_by_type = TRUE) {
  
  assert_that(rlang::is_logical(sort_by_type, n = 1))
  
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

# segment_video() ---------------------------------------------------------

#' Segment Video
#'
#' Use FFmpeg to quickly break a single video file into multiple smaller video
#' files (with the same encoding) based on pairs of start and stop timestamps.
#' Segment video files will be named by taking the name of \code{infile} and
#' appending a suffix of an underscore (_) and an integer indicating which
#' segment (based on the order provided in \code{ts_start} and \code{ts_stop}).
#'
#' @param infile A string containing the path to a video file.
#' @param ts_start A vector containing one or more timestamps indicating the
#'   start of each segment to create. Can be either a numeric vector indicating
#'   seconds or a character vector with time duration syntax. Must have the same
#'   length as \code{ts_stop}.
#' @param ts_stop A vector containing one or more timestamps indicating the stop
#'   of each segment to create. Can be either a numeric vector indicating
#'   seconds or a character vector with time duration syntax. Must have the same
#'   length as \code{ts_start}.
#' @param outdir A string containing the path to an existing directory in which
#'   to create the segmented video files.
#' @param padding The number of digits to zero-pad the segment filename to. For
#'   example, if \code{infile} was "x.mp4", then setting this to 3 would result
#'   in the first segment file being named "x_001.mp4" and setting this to 2
#'   would result in the first segment file being named "x_01.mp4".
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
segment_video <- function(infile, 
                          ts_start, 
                          ts_stop, 
                          outdir = NULL, 
                          padding = 3,
                          ...) {
  
  assert_that(is.character(infile))
  assert_that(is.numeric(ts_start) || is.character(ts_start))
  assert_that(is.numeric(ts_stop) || is.character(ts_stop))
  assert_that(length(ts_start) == length(ts_stop))
  assert_that(is.null(outdir) || is.character(outdir))
  
  outpaths <- paste0(
    tools::file_path_sans_ext(infile), 
    '_', 
    sprintf(paste0('%0', padding, 'd.'), seq_along(ts_start)),
    tools::file_ext(infile)
  )
  
  if (!is.null(outdir)) outpaths <- file.path(outdir, outpaths)
  
  command <- glue(
    '-y -i {infile} -vcodec copy -acodec copy ',
    paste0(
      '-ss ', 
      ts_start, 
      ' -to ', 
      ts_stop, 
      ' -sn ',
      outpaths,
      collapse = ' '
    )
  )
  
  ffmpeg(command)
}