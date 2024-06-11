
# ffmpeg() ----------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  assert_that(rlang::is_character(command, n = 1))
  out <- system(glue('{find_ffmpeg()} {command}'), intern = TRUE)
  out
}

# extract_frames() --------------------------------------------------------

#' @export
extract_frame <- function(infile, outfile, timestamp = NULL, frame = NULL) {
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile, n = 1))
  assert_that(rlang::is_null(timestamp) || 
                rlang::is_double(timestamp, n = 1, finite = TRUE) ||
                rlang::is_string(timestamp))
  assert_that(rlang::is_null(frame) || 
                rlang::is_integerish(frame, n = 1, finite = TRUE))
  assert_that(sum(rlang::is_null(timestamp), rlang::is_null(frame)) == 1,
              msg = "Please provide either timestamp or frame.")
  
  if (rlang::is_null(timestamp)) timestamp <- frame / get_framerate(infile)
  
  pre <- glue('-ss {timestamp}')
  post <- glue('-qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal')
  command <- glue('{pre} -i "{infile}" {post} "{outfile}"')
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


# separate_audio_video() --------------------------------------------------

#' @export
separate_audio_video <- function(infile, audiofile, videofile) {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(audiofile, n = 1))
  assert_that(rlang::is_character(videofile, n = 1))
  
  command <- glue('-i "{infile}" -map 0:a "{audiofile}" -map 0:v "{videofile}"')
  ffmpeg(command)
}


# audio_as_mp3() ----------------------------------------------------------

#' @export
audio_as_mp3 <- function(infile, outfile) {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile, n = 1))
  
  command <- glue('-i "{infile}" -q:a 0 -map a "{outfile}"')
  ffmpeg(command)
}

# crop_video() ------------------------------------------------------------

#' @export
crop_video <- function(infile, outfile, width, height, x, y, arg = "") {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile))
  assert_that(rlang::is_integerish(width, n = 1))
  assert_that(rlang::is_integerish(height, n = 1))
  assert_that(rlang::is_integerish(x, n = 1))
  assert_that(rlang::is_integerish(y, n = 1))
  assert_that(rlang::is_string(arg))
  
  command <- glue(
    '-i "{infile}" -map 0 -filter:v "crop={width}:{height}:{x}:{y}" {arg} "{outfile}"'
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
#' @param outfiles Either NULL or a character vector indicating the filename
#'   (with extension) for each segment to create. If NULL, will append a
#'   zero-padded integer to \code{infile}. If not NULL, must have the same
#'   length as \code{ts_start}.
#' @param run A logical indicating whether to run the command or just create it.
#' @param ... Not currently used
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @export
segment_video <- function(infile, 
                          ts_start, 
                          ts_stop, 
                          outfiles = NULL,
                          run = TRUE,
                          ...) {
  
  assert_that(is.character(infile))
  assert_that(is.numeric(ts_start) || is.character(ts_start))
  assert_that(is.numeric(ts_stop) || is.character(ts_stop))
  assert_that(length(ts_start) == length(ts_stop))
  assert_that(is.null(outnames) || length(outnames) == length(ts_start))
  
  # If no names are provided, add zero-padded integers to infile name
  if (is.null(outfiles)) {
    outfiles <- paste0(
      tools::file_path_sans_ext(infile),
      '_',
      pad_integers(seq_along(ts_start)),
      '.',
      tools::file_ext(infile)
    )
  }
  
  # Build command
  command <- glue(
    '-y -i "{infile}" -vcodec copy -acodec copy ',
    paste0(
      '-ss ', 
      ts_start, 
      ' -to ', 
      ts_stop, 
      ' -sn "',
      outfiles,
      '"',
      collapse = ' '
    )
  )
  
  # Run command
  if (run == TRUE) {
    ffmpeg(command)
  } else {
    command
  }
  
}


# concatenate_videos() ----------------------------------------------------

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
#' @export
concatenate_videos <- function(infiles, outfile) {
  
  assert_that(is.character(infiles), rlang::is_string(outfile))
  
  if (length(unique(tools::file_ext(infiles))) != 1) {
    warning("Not all infiles have the same extension.")
  }
  
  # Create a temporary text file to store the paths of the files to concatenate
  tempfn <- tempfile("file", fileext = ".txt")
  
  # Write the paths of the files to concatenate to the temporary text file
  fileConn <- file(tempfn)
  writeLines(paste(paste0("file '", infiles, "'"), collapse = "\n"), fileConn)
  
  # Build the FFmpeg command to perform the concatenation
  command <- glue::glue('-f concat -safe 0 -i "{tempfn}" -map 0 -c copy "{outfile}"')
  
  # Run the FFmpeg command to perform the concatenation
  ffmpeg(command)  
  
  # Close and remove the temporary text file
  close(fileConn)
}



# Get volume levels -------------------------------------------------------

get_volume <- function(infile) {
  assert_that(rlang::is_string(infile))
  
  command <- glue::glue('-i {infile} -af "volumedetect" -vn -sn -dn -f null NUL')
  
  output <- ffmpeg(command)
  
  #TODO: Clean up output
  
  mean_volumes <- regmatches(
    output, 
    regexpr("^\\[Parsed_volumedetect.*mean_volume.*", out, perl=TRUE)
  )
  
  max_volumes <- regmatches(
    output, 
    regexpr("^\\[Parsed_volumedetect.*max_volume.*", out, perl=TRUE)
  )
  
  output
}
