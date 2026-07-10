
# ffmpeg() ----------------------------------------------------------------

#' Run a raw FFmpeg command
#'
#' Send a raw argument string to the FFmpeg command-line program. This is the
#' Layer 0 escape hatch: the string is passed to FFmpeg verbatim (after the
#' executable path), so the caller is responsible for quoting and option order.
#'
#' @param command A string containing the arguments to pass to FFmpeg.
#' @return A character vector containing the text output by FFmpeg.
#' @export
ffmpeg <- function(command) {
  rlang::check_string(command)
  out <- system(glue('{find_ffmpeg()} {command}'), intern = TRUE)
  out
}

# extract_frames() --------------------------------------------------------

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

  if (rlang::is_null(timestamp)) timestamp <- frame / get_framerate(infile)

  # A single-frame grab: fast input seek plus the quality flags, one frame out.
  p <- ffm_files(infile, outfile)
  p <- ffm_seek(p, start = timestamp, reencode = FALSE)
  p <- ffm_output_options(
    p, "-qmin 1", "-q:v 1", "-qscale:v 2", "-frames:v 1", "-huffman optimal"
  )
  ffm_finish(p, run)
}

# extract_audio() ---------------------------------------------------------

#' Extract the audio stream from a media file
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the audio file to write.
#' @param acodec A string naming the audio codec for the output stream.
#'   (default = \code{"copy"}, i.e. remux without re-encoding)
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @export
extract_audio <- function(infile, outfile, acodec = "copy", run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  rlang::check_string(acodec)

  p <- ffm_files(infile, outfile)
  p <- ffm_codec(p, audio = acodec)
  p <- ffm_drop(p, "video")
  ffm_finish(p, run)
}


# separate_audio_video() --------------------------------------------------

#' Split a media file into separate audio and video files
#'
#' @param infile A string containing the path to a media file.
#' @param audiofile A string containing the path of the audio file to write.
#' @param videofile A string containing the path of the video file to write.
#' @return The character output from FFmpeg.
#' @export
separate_audio_video <- function(infile, audiofile, videofile) {
  
  check_file_exists(infile)
  rlang::check_string(audiofile)
  rlang::check_string(videofile)

  command <- glue('-i "{infile}" -map 0:a "{audiofile}" -map 0:v "{videofile}"')
  ffmpeg(command)
}


# audio_as_mp3() ----------------------------------------------------------

#' Extract a media file's audio as an MP3
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the MP3 file to write.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @export
audio_as_mp3 <- function(infile, outfile, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  p <- ffm_files(infile, outfile)
  p <- ffm_map(p, "a")
  p <- ffm_output_options(p, "-q:a 0")
  ffm_finish(p, run)
}

# crop_video() ------------------------------------------------------------

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
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @export
crop_video <- function(infile, outfile, width, height,
                       x = "(in_w-out_w)/2", y = "(in_h-out_h)/2",
                       run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  p <- ffm_files(infile, outfile)
  p <- ffm_crop(p, width = width, height = height, x = x, y = y)
  p <- ffm_map(p, "0")
  ffm_finish(p, run)
}


# format_for_web() --------------------------------------------------------

#' Re-encode a video for web playback
#'
#' Re-encode a video into a widely compatible, web-friendly form (H.264 video
#' with \code{yuv420p} and \code{+faststart}, AAC audio), padding odd
#' dimensions down to even values as required by the codec.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @export
format_for_web <- function(infile, outfile, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  p <- ffm_files(infile, outfile)
  p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")
  p <- ffm_codec(p, video = "libx264", audio = "aac")
  p <- ffm_pixel_format(p, "yuv420p")
  p <- ffm_output_options(p, "-movflags +faststart")
  ffm_finish(p, run)
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
  
  rlang::check_string(infile)
  if (!(is.numeric(ts_start) || is.character(ts_start))) {
    cli::cli_abort("{.arg ts_start} must be a numeric or character vector.")
  }
  if (!(is.numeric(ts_stop) || is.character(ts_stop))) {
    cli::cli_abort("{.arg ts_stop} must be a numeric or character vector.")
  }
  if (length(ts_start) != length(ts_stop)) {
    cli::cli_abort("{.arg ts_start} and {.arg ts_stop} must have the same length.")
  }
  if (!is.null(outfiles) && length(outfiles) != length(ts_start)) {
    cli::cli_abort("{.arg outfiles} must have the same length as {.arg ts_start}.")
  }
  
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
  
  if (!rlang::is_character(infiles)) {
    cli::cli_abort("{.arg infiles} must be a character vector of file paths.")
  }
  rlang::check_string(outfile)

  if (length(unique(tools::file_ext(infiles))) != 1) {
    cli::cli_warn("Not all {.arg infiles} have the same extension.")
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
