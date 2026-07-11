
# ffmpeg() ----------------------------------------------------------------

#' Run a raw FFmpeg command
#'
#' Send a raw argument string to the FFmpeg command-line program. This is the
#' Layer 0 escape hatch: the string is passed to FFmpeg verbatim (after the
#' executable path), so the caller is responsible for quoting and option order.
#'
#' @param command A string containing the arguments to pass to FFmpeg.
#' @return A character vector containing the text output by FFmpeg.
#' @family escape hatch functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' # Layer 0 escape hatch: the string is passed to FFmpeg verbatim
#' ffmpeg("-version")
#' @export
ffmpeg <- function(command) {
  rlang::check_string(command)
  # Redirect FFmpeg's stdin from an empty input (the `input = ""` temp file) so
  # it cannot drain the parent process's stdin. FFmpeg reads stdin for
  # interactive control while encoding; without this it would swallow whatever
  # is feeding R's stdin (e.g. the example stream during R CMD check). This is
  # the equivalent of FFmpeg's -nostdin flag, applied without touching the
  # verbatim `command` string.
  out <- system(glue('{find_ffmpeg()} {command}'), intern = TRUE, input = "")
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
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # run = FALSE returns the reproducible command instead of executing it
#' extract_frame(video, "frame.png", timestamp = 0.5, run = FALSE)
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
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' extract_audio(video, "audio.aac", run = FALSE)
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
#' By default the streams are copied, not re-encoded (\code{reencode =
#' FALSE}): separation is lossless and fast, but each output container must
#' support the source codec (e.g. write AAC audio from an MP4 to \code{.aac}
#' or \code{.m4a}, not \code{.mp3}). Set \code{reencode = TRUE} to let FFmpeg
#' re-encode each stream to whatever the output extension implies.
#'
#' @param infile A string containing the path to a media file.
#' @param audiofile A string containing the path of the audio file to write.
#' @param videofile A string containing the path of the video file to write.
#' @param reencode A logical: stream-copy the audio and video losslessly
#'   (\code{FALSE}, default) or re-encode them to match the output extensions
#'   (\code{TRUE}).
#' @param run A logical: run the commands through FFmpeg (\code{TRUE}, default)
#'   or return the compiled commands without running them (\code{FALSE}).
#' @return A named character vector of the two compiled commands
#'   (\code{audio}, \code{video}); invisible when \code{run = TRUE}.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#' @export
separate_audio_video <- function(infile, audiofile, videofile,
                                 reencode = FALSE, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(audiofile)
  rlang::check_string(videofile)
  rlang::check_bool(reencode)

  # One input -> two outputs is a fan-out: emit two single-output pipelines
  # (D-M03-2) rather than a dual-`-map` command the linear engine can't model.
  audio <- ffm_map(ffm_files(infile, audiofile), "0:a")
  video <- ffm_map(ffm_files(infile, videofile), "0:v")
  if (!reencode) {
    # D-M06-4: lossless stream copy by default.
    audio <- ffm_codec(audio, audio = "copy")
    video <- ffm_codec(video, video = "copy")
  }
  commands <- c(audio = ffm_compile(audio), video = ffm_compile(video))

  if (run) {
    ffm_run(audio)
    ffm_run(video)
    invisible(commands)
  } else {
    commands
  }
}


# audio_as_mp3() ----------------------------------------------------------

#' Extract a media file's audio as an MP3
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the MP3 file to write.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' audio_as_mp3(video, "audio.mp3", run = FALSE)
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
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' crop_video(video, "cropped.mp4", width = 160, height = 120, run = FALSE)
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
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' format_for_web(video, "web.mp4", run = FALSE)
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
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' head(get_codecs())
#' get_codecs(sort_by_type = FALSE)
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
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' head(get_encoders())
#' get_encoders(sort_by_type = FALSE)
#' @export
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
#' @param reencode A logical passed to \code{\link{ffm_seek}}: cut each segment
#'   frame-accurately by re-encoding (\code{TRUE}, default) or with a fast,
#'   lossless copy that snaps to keyframes (\code{FALSE}). See \code{ffm_seek}
#'   for the trade-off.
#' @param run A logical: run each segment's command (\code{TRUE}, default) or
#'   only compile them (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: cut segments in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default).
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: one row per segment with its \code{command} (and,
#'   when \code{run = TRUE}, \code{success}).
#' @seealso \code{\link{ffm_batch}}, \code{\link{ffm_seek}}
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Two segments; run = FALSE compiles one command per segment
#' segment_video(video, ts_start = c(0, 0.5), ts_stop = c(0.5, 1), run = FALSE)
#' @export
segment_video <- function(infile,
                          ts_start,
                          ts_stop,
                          outfiles = NULL,
                          reencode = TRUE,
                          run = TRUE,
                          parallel = FALSE) {

  check_file_exists(infile)
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
  rlang::check_bool(reencode)

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

  # Fan-out (one input -> many outputs) is a Layer 2 concern: build one
  # single-output seek pipeline per segment and run them through ffm_batch
  # (D-M03-2). The engine stays single-output (D003).
  jobs <- tibble::tibble(
    input = infile, output = outfiles, start = ts_start, end = ts_stop
  )
  ffm_batch(
    jobs,
    function(input, output, start, end, ...) {
      p <- ffm_seek(ffm_files(input, output), start = start, end = end,
                    reencode = reencode)
      if (!reencode) p <- ffm_copy(p)
      p
    },
    run = run,
    parallel = parallel
  )
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
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' concatenate_videos(c(video, video), "joined.mp4", run = FALSE)
#' @export
concatenate_videos <- function(infiles, outfile, run = TRUE) {

  if (!rlang::is_character(infiles)) {
    cli::cli_abort("{.arg infiles} must be a character vector of file paths.")
  }
  rlang::check_string(outfile)

  if (length(unique(tools::file_ext(infiles))) != 1) {
    cli::cli_warn("Not all {.arg infiles} have the same extension.")
  }

  # ffm_concat() writes the demuxer list file and sets copy + map 0.
  p <- ffm_concat(ffm_files(infiles, outfile))
  ffm_finish(p, run)
}



# compare_videos() --------------------------------------------------------

#' Build a side-by-side comparison video
#'
#' Stack two or more videos into a single comparison video — side-by-side
#' (\code{direction = "horizontal"}) or one above the other
#' (\code{direction = "vertical"}) — a common need when reviewing annotations or
#' before/after processing. Built on the blessed stacking verbs
#' (\code{\link{ffm_hstack}} / \code{\link{ffm_vstack}}).
#'
#' By default the two inputs are resized to share an edge (equal heights for a
#' horizontal stack, equal widths for a vertical one); resizing currently
#' supports exactly two inputs, so pass \code{resize = FALSE} to compare more.
#' Audio is dropped unless \code{audio} names an input to carry.
#'
#' @param infiles A character vector of two or more video file paths.
#' @param outfile A string giving the path to write the comparison video to.
#' @param direction Either \code{"horizontal"} (side-by-side, the default) or
#'   \code{"vertical"} (stacked top to bottom).
#' @param resize A logical indicating whether to resize the inputs to share an
#'   edge. Only supported for exactly two inputs. (default = \code{TRUE})
#' @param audio The 0-based index of the input whose audio to keep in the
#'   output, or \code{NULL} to drop audio entirely. (default = \code{NULL})
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' compare_videos(c(video, video), "compare.mp4", run = FALSE)
#' @export
compare_videos <- function(infiles, outfile,
                           direction = c("horizontal", "vertical"),
                           resize = TRUE, audio = NULL, run = TRUE) {

  if (!rlang::is_character(infiles) || length(infiles) < 2) {
    cli::cli_abort("{.arg infiles} must name two or more video files.")
  }
  rlang::check_string(outfile)
  direction <- rlang::arg_match(direction)
  rlang::check_bool(resize)
  rlang::check_number_whole(
    audio, min = 0, max = length(infiles) - 1, allow_null = TRUE
  )
  if (resize && length(infiles) != 2) {
    cli::cli_abort(c(
      "{.arg resize} currently supports exactly two inputs.",
      "i" = "Pass {.code resize = FALSE} to compare more than two videos."
    ))
  }

  p <- ffm_files(infiles, outfile)
  p <- switch(
    direction,
    horizontal = ffm_hstack(p, resize = resize),
    vertical = ffm_vstack(p, resize = resize)
  )
  if (!is.null(audio)) {
    p <- ffm_map(p, paste0(audio, ":a"))
  }
  ffm_finish(p, run)
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
