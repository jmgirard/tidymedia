
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

  ffm_finish(frame_pipeline(infile, outfile, timestamp), run)
}


# frame_pipeline() --------------------------------------------------------

# Shared single-frame grab for extract_frame() and extract_frames(): a fast
# input seek to the (already resolved) timestamp plus the still-image quality
# flags, one frame out. Both verbs build identical commands from this helper;
# frame->timestamp resolution stays in the verb layer (scalar vs. per-row).
frame_pipeline <- function(input, output, timestamp) {
  p <- ffm_files(input, output)
  p <- ffm_seek(p, start = timestamp, reencode = FALSE)
  ffm_output_options(
    p, "-qmin 1", "-q:v 1", "-qscale:v 2", "-frames:v 1", "-huffman optimal"
  )
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


# standardize_video() -----------------------------------------------------

#' Standardize a video to a reproducible format
#'
#' Re-encode a video to a consistent, reproducible format for analysis
#' pipelines: a single video codec, pixel format, and (optionally) resolution
#' and frame rate, with \code{+faststart} for smooth playback. Unlike
#' \code{\link{format_for_web}} (a fixed web-delivery recipe), every part of the
#' standard is a parameter, so a lab can pin its own house format once and apply
#' it across a dataset.
#'
#' @details
#' The default standard \code{standardize_video(infile, outfile)} re-encodes to
#' H.264 video (\code{vcodec = "libx264"}) with \code{pixel_format = "yuv420p"}
#' and \code{-movflags +faststart}, keeping the source resolution and frame
#' rate. Audio is stream-copied unchanged (\code{-c:a copy}); audio
#' standardization is out of scope. The same input therefore always compiles to
#' a byte-identical command.
#'
#' Resolution follows \code{width}/\code{height}: supplying both forces exact
#' output dimensions; supplying only one preserves the aspect ratio and rounds
#' the other to the nearest even number (FFmpeg's \code{-2}); supplying neither
#' keeps the source resolution but rounds odd dimensions down to the nearest
#' even value (a \code{yuv420p}/\code{libx264} requirement, and a no-op for
#' already-even input) so the output always encodes.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param width The output width in pixels (a positive number), or \code{NULL}
#'   (default) to leave the width unconstrained.
#' @param height The output height in pixels (a positive number), or \code{NULL}
#'   (default) to leave the height unconstrained.
#' @param fps The output frame rate (a positive number or FFmpeg framerate
#'   expression such as \code{"30000/1001"}), or \code{NULL} (default) to keep
#'   the input frame rate.
#' @param vcodec A string naming the output video codec (default
#'   \code{"libx264"}).
#' @param pixel_format A string naming the output pixel format (default
#'   \code{"yuv420p"}).
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # The documented default standard (H.264 / yuv420p / +faststart)
#' standardize_video(video, "std.mp4", run = FALSE)
#' # Pin resolution and frame rate too
#' standardize_video(video, "std.mp4", width = 1280, height = 720, fps = 30,
#'                   run = FALSE)
#' @export
standardize_video <- function(infile, outfile,
                              width = NULL, height = NULL, fps = NULL,
                              vcodec = "libx264", pixel_format = "yuv420p",
                              run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  ffm_finish(
    standardize_pipeline(infile, outfile, width, height, fps, vcodec,
                         pixel_format),
    run
  )
}


# standardize_pipeline() --------------------------------------------------

# Shared standardization pipeline for standardize_video() and
# standardize_videos(): build one single-output re-encode pipeline for a single
# input. Both verbs compile identical commands from this helper, so per-value
# validation (dimensions via check_dim, codec/pixfmt via check_token) and M12's
# guards (audio stream-copy, even-dimension safeguard, +faststart) live here
# once -- the batch sibling inherits them by construction (D002, D003, D007).
standardize_pipeline <- function(input, output, width, height, fps, vcodec,
                                 pixel_format) {
  p <- ffm_files(input, output)
  # Resolution: exact when both given; aspect-preserving with an even output
  # dimension (FFmpeg's -2) when only one. ffm_scale() validates each dimension
  # via check_dim(). When neither is given, still force even dimensions so
  # yuv420p/libx264 can encode odd-dimensioned sources -- floor-to-even is a
  # no-op for already-even input, mirroring format_for_web()'s guard.
  if (!is.null(width) || !is.null(height)) {
    p <- ffm_scale(
      p,
      width = if (is.null(width)) "-2" else width,
      height = if (is.null(height)) "-2" else height
    )
  } else {
    p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")
  }
  if (!is.null(fps)) {
    p <- ffm_fps(p, fps)
  }
  # Audio is stream-copied, not re-encoded: standardization is video-only, so
  # "leave audio untouched" means copy the bytes (matching extract_audio()).
  p <- ffm_codec(p, video = vcodec, audio = "copy")
  p <- ffm_pixel_format(p, pixel_format)
  ffm_output_options(p, "-movflags +faststart")
}


# normalize_audio() -------------------------------------------------------

#' Normalize a file's audio loudness (EBU R128)
#'
#' Normalize the perceived loudness of a file's audio toward an EBU R128 target
#' using FFmpeg's single-pass \code{loudnorm} filter, optionally downmixing the
#' channel count and resampling. The video stream is copied unchanged
#' (\code{-c:v copy}), so only the audio is touched -- the audio-side complement
#' to \code{\link{standardize_video}}, which leaves audio alone.
#'
#' @details
#' The default targets follow EBU Recommendation R 128 (2014) --
#' \code{target_loudness = -23} LUFS and \code{true_peak = -1} dBTP, loudness
#' measured per ITU-R BS.1770-4 -- with \code{loudness_range = 7}. This is
#' single-pass (dynamic) \code{loudnorm}: the same input and arguments always
#' compile to one reproducible command, with no separate measurement pass.
#' Because the audio is filtered it is re-encoded (the container's default audio
#' encoder). Leaving \code{channels} at \code{NULL} preserves the source channel
#' layout. Note that FFmpeg's \code{loudnorm} filter resamples its output (up to
#' 192 kHz, capped by the encoder), so the output sample rate is \emph{not} the
#' source rate unless you pin it: set \code{sample_rate} to control the output
#' rate.
#'
#' @param infile A string containing the path to a media file (with audio).
#' @param outfile A string containing the path of the file to write.
#' @param target_loudness The target integrated loudness, in LUFS (a number in
#'   \code{-70}..\code{-5}; default \code{-23}, the EBU R128 target).
#' @param true_peak The maximum true peak, in dBTP (a number in \code{-9}..\code{0};
#'   default \code{-1}, the EBU R128 ceiling).
#' @param loudness_range The target loudness range, in LU (a number in
#'   \code{1}..\code{50}; default \code{7}).
#' @param channels The output channel count, e.g. \code{1} to downmix to mono (a
#'   positive whole number), or \code{NULL} (default) to keep the source layout.
#' @param sample_rate The output sample rate in Hz, e.g. \code{48000} (a positive
#'   whole number), or \code{NULL} (default) to let \code{loudnorm} choose (it
#'   resamples, up to 192 kHz encoder-capped -- not the source rate). Set this to
#'   pin the output rate.
#' @param two_pass A logical: when \code{TRUE}, use accurate two-pass
#'   (measured/linear) normalization instead of the default single-pass
#'   (\code{FALSE}). A first \emph{analysis pass} measures the input's loudness,
#'   and a second \emph{correction pass} feeds those measurements back with
#'   \code{linear=true} so the output hits the EBU R128 target precisely.
#'   Two-pass therefore \strong{always runs the analysis pass through FFmpeg}
#'   (it needs the binary and readable input), even when \code{run = FALSE}: in
#'   that case the analysis still runs and the returned value is the exact
#'   correction command, left unexecuted. The single-pass default touches no
#'   binary under \code{run = FALSE}.
#' @param run A logical: run the (correction) command through FFmpeg
#'   (\code{TRUE}, default) or return the compiled command without running it
#'   (\code{FALSE}). Under \code{two_pass = TRUE} this gates only the correction
#'   pass; the analysis pass runs regardless (see \code{two_pass}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}). Under
#'   \code{two_pass = TRUE} this is the correction command built from the
#'   measured values.
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' normalize_audio(video, "normalized.mp4", run = FALSE)
#' # Normalize to a streaming target and downmix to mono
#' normalize_audio(video, "mono.mp4", target_loudness = -16, channels = 1,
#'                 run = FALSE)
#' @export
normalize_audio <- function(infile, outfile,
                            target_loudness = -23,
                            true_peak = -1,
                            loudness_range = 7,
                            channels = NULL,
                            sample_rate = NULL,
                            two_pass = FALSE,
                            run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  rlang::check_bool(two_pass)

  # Two-pass: measure the input first, then build a linear correction from the
  # measurements. Validate the shaping knobs up front so a bad channels/
  # sample_rate fails before the analysis pass runs, not after wasting it
  # (targets are validated when the analysis pipeline builds). Single-pass keeps
  # its pure, binary-free run = FALSE compile.
  measured <- NULL
  if (two_pass) {
    rlang::check_number_whole(channels, min = 1, allow_null = TRUE)
    rlang::check_number_whole(sample_rate, min = 1, allow_null = TRUE)
    measured <- run_loudnorm_analysis(infile, target_loudness, true_peak,
                                      loudness_range)
  }

  ffm_finish(
    normalize_audio_pipeline(infile, outfile, target_loudness, true_peak,
                             loudness_range, channels, sample_rate,
                             measured = measured),
    run
  )
}

# normalize_audio_pipeline() ----------------------------------------------

# Shared loudness-normalization pipeline for normalize_audio() and (M15)
# normalize_audios(): build one single-output pipeline for a single input. Both
# verbs compile identical commands from this helper, so per-value validation
# (loudness targets via ffm_loudnorm(), channels/sample_rate here) lives once --
# the batch sibling inherits it by construction (D002, D007; M13 lesson).
normalize_audio_pipeline <- function(input, output,
                                     target_loudness = -23,
                                     true_peak = -1,
                                     loudness_range = 7,
                                     channels = NULL,
                                     sample_rate = NULL,
                                     measured = NULL) {
  rlang::check_number_whole(channels, min = 1, allow_null = TRUE)
  rlang::check_number_whole(sample_rate, min = 1, allow_null = TRUE)

  p <- ffm_files(input, output)
  # Loudness: EBU R128 loudnorm; ffm_loudnorm() validates the target ranges. With
  # `measured` (the two-pass correction path), feed the analysis-pass values back
  # and switch to linear normalization so the target is hit precisely (M16).
  if (is.null(measured)) {
    p <- ffm_loudnorm(p, target_loudness = target_loudness,
                      true_peak = true_peak, loudness_range = loudness_range)
  } else {
    p <- ffm_loudnorm(p, target_loudness = target_loudness,
                      true_peak = true_peak, loudness_range = loudness_range,
                      measured_i = measured$i, measured_tp = measured$tp,
                      measured_lra = measured$lra, measured_thresh = measured$thresh,
                      offset = measured$offset, linear = TRUE)
  }
  # Touch audio only: stream-copy the video bytes unchanged (the inverse of
  # standardize_video()'s audio copy).
  p <- ffm_codec(p, video = "copy")
  if (!is.null(channels)) {
    p <- ffm_output_options(p, paste0("-ac ", channels))
  }
  if (!is.null(sample_rate)) {
    p <- ffm_output_options(p, paste0("-ar ", sample_rate))
  }
  p
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
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one segment at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
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

  # If no names are provided, derive per-segment names from the input file.
  if (is.null(outfiles)) {
    outfiles <- derive_segment_names(rep(infile, length(ts_start)))
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
      segment_pipeline(input, output, start, end, reencode)
    },
    run = run,
    parallel = parallel
  )
}


# derive_segment_names() --------------------------------------------------

# Derive one output path per segment from its input path, appending
# `_<n>.<ext>` to each input's basename. Numbering restarts per input file (in
# row order) and is zero-padded to that input's own segment count, so the
# single-input case (segment_video) and the multi-input jobs table
# (segment_videos with no `output` column) share one naming rule.
derive_segment_names <- function(input) {
  out <- character(length(input))
  for (f in unique(input)) {
    sel <- input == f
    padded <- pad_integers(seq_len(sum(sel)))
    out[sel] <- paste0(
      tools::file_path_sans_ext(f), "_", padded, ".", tools::file_ext(f)
    )
  }
  out
}


# derive_frame_names() ----------------------------------------------------

# Derive one image path per frame from its input path, appending `_<n>.<format>`
# to each input's basename. Same per-input-restart, zero-padded rule as
# derive_segment_names(), but the extension is the image `format` (a frame is an
# image, not a copy of the source container) rather than the input's extension.
derive_frame_names <- function(input, format = "png") {
  out <- character(length(input))
  for (f in unique(input)) {
    sel <- input == f
    padded <- pad_integers(seq_len(sum(sel)))
    out[sel] <- paste0(tools::file_path_sans_ext(f), "_", padded, ".", format)
  }
  out
}


# segment_pipeline() ------------------------------------------------------

# Shared cut logic for segment_video() and segment_videos(): build one
# single-output seek pipeline for a single segment, stream-copying on the fast
# (non-reencode) path. Fan-out verbs stay single-output per job (D003, D007);
# both verbs wrap this in a closure that captures the scalar `reencode`.
segment_pipeline <- function(input, output, start, end, reencode) {
  p <- ffm_seek(ffm_files(input, output), start = start, end = end,
                reencode = reencode)
  if (!reencode) p <- ffm_copy(p)
  p
}


# segment_videos() --------------------------------------------------------

#' Segment Many Videos From a Jobs Table
#'
#' Cut segments across many input files from a single jobs tibble — a
#' table-driven sibling of \code{\link{segment_video}} for when your segments
#' span more than one input. Each row is one segment; the four required columns
#' name its source, destination, and cut points. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per segment.
#'
#' @param jobs A data frame with one row per segment and (at least) the columns
#'   \code{input} (source path), \code{start} and \code{end} (cut points; a
#'   numeric column of seconds or a character column with time-duration syntax).
#'   Two optional columns are recognized: \code{output} (destination path) and
#'   \code{reencode} (a logical; see the \code{reencode} argument). If
#'   \code{output} is absent, one is derived per row by appending
#'   \code{_<n>.<ext>} to each input's basename, with the segment number
#'   restarting at 1 for each input file (the same rule as
#'   \code{\link{segment_video}}). Any other columns are ignored.
#' @param reencode A logical passed to \code{\link{ffm_seek}}: cut each segment
#'   frame-accurately by re-encoding (\code{TRUE}, default) or with a fast,
#'   lossless copy that snaps to keyframes (\code{FALSE}). See \code{ffm_seek}
#'   for the trade-off. Applies to every row, unless \code{jobs} carries a
#'   \code{reencode} column, which overrides this argument on a per-row basis.
#' @param run A logical: run each segment's command through FFmpeg
#'   (\code{TRUE}, default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: cut segments in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one segment at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso \code{\link{segment_video}} for the single-input, parallel-vector
#'   form; \code{\link{ffm_batch}} for the batch runner and the arguments
#'   forwarded through \code{...}; \code{\link{ffm_seek}} for the cut trade-off.
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   output = c("a.mp4", "b.mp4"),
#'   start  = c(0, 0.5),
#'   end    = c(0.5, 1)
#' )
#' # run = FALSE compiles one command per segment without calling FFmpeg
#' segment_videos(jobs, run = FALSE)
#' @export
segment_videos <- function(jobs, reencode = TRUE, run = TRUE,
                           parallel = FALSE, ...) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per segment.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  required <- c("input", "start", "end")
  missing <- setdiff(required, names(jobs))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} must have columns {.val {required}}.",
      "x" = "Missing column{?s}: {.val {missing}}."
    ))
  }
  # Validate cut-point column types up front (parity with segment_video()), so a
  # bad column fails clearly here rather than as an opaque FFmpeg error.
  if (!(is.numeric(jobs$start) || is.character(jobs$start))) {
    cli::cli_abort("The {.field start} column of {.arg jobs} must be numeric or character.")
  }
  if (!(is.numeric(jobs$end) || is.character(jobs$end))) {
    cli::cli_abort("The {.field end} column of {.arg jobs} must be numeric or character.")
  }
  if ("reencode" %in% names(jobs) &&
      (!is.logical(jobs$reencode) || anyNA(jobs$reencode))) {
    cli::cli_abort(
      "The {.field reencode} column of {.arg jobs} must be {.val {TRUE}} or {.val {FALSE}} (no {.val {NA}})."
    )
  }
  rlang::check_bool(reencode)

  # Auto-name outputs when the column is absent: derive per-input segment names
  # (numbering restarts per input file) and carry them on the returned tibble.
  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_segment_names(jobs$input)
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output seek pipeline
  # per row, sharing segment_pipeline() with segment_video(). A per-row
  # `reencode` column (arriving via `...` from pmap) overrides the scalar arg;
  # `...` also forwards ffm_batch options (verify/manifest/...) to the runner,
  # never to the pipeline builder.
  ffm_batch(
    jobs,
    function(input, output, start, end, ...) {
      dots <- list(...)
      re <- if ("reencode" %in% names(dots)) dots$reencode else reencode
      segment_pipeline(input, output, start, end, re)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# extract_frames() --------------------------------------------------------

#' Extract Still Frames From Many Videos From a Jobs Table
#'
#' Grab one still image per row across many input files from a single jobs
#' tibble — a table-driven sibling of \code{\link{extract_frame}} for when your
#' frames span more than one input. Each row is one frame; the required columns
#' name its source and the moment to capture. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per frame.
#'
#' @param jobs A data frame with one row per frame and (at least) an
#'   \code{input} column (source path) plus \strong{exactly one} of a
#'   \code{timestamp} column (seconds, or \pkg{FFmpeg} time-duration strings) or
#'   a \code{frame} column (whole frame numbers, converted per row to a
#'   timestamp via the input's frame rate, as \code{\link{extract_frame}} does).
#'   An optional \code{output} column names the destination image; when absent,
#'   one is derived per row by appending \code{_<n>.<format>} to each input's
#'   basename, with the frame number restarting at 1 for each input file. Any
#'   other columns are ignored.
#' @param format A string giving the image file extension used when \code{output}
#'   is derived (ignored when \code{jobs} carries an \code{output} column).
#'   (default = \code{"png"})
#' @param run A logical: run each frame's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: grab frames in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one frame at a time
#'   and warns.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso \code{\link{extract_frame}} for the single-frame form;
#'   \code{\link{ffm_batch}} for the batch runner and the arguments forwarded
#'   through \code{...}; \code{\link{segment_videos}} for the segment-cutting
#'   sibling.
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input     = c(video, video),
#'   output    = c("a.png", "b.png"),
#'   timestamp = c(0.25, 0.75)
#' )
#' # run = FALSE compiles one command per frame without calling FFmpeg
#' extract_frames(jobs, run = FALSE)
#' @export
extract_frames <- function(jobs, format = "png", run = TRUE,
                           parallel = FALSE, ...) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per frame.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }
  rlang::check_string(format)

  # Table-level exclusivity: exactly one of the selection columns, mirroring
  # extract_frame()'s scalar timestamp/frame exclusive-or.
  has_ts <- "timestamp" %in% names(jobs)
  has_fr <- "frame" %in% names(jobs)
  if (has_ts == has_fr) {
    cli::cli_abort(c(
      "{.arg jobs} must have exactly one of a {.field timestamp} or {.field frame} column.",
      "x" = if (has_ts) "Both columns are present." else "Neither column is present."
    ))
  }

  # Validate the selection column's type + reject NA up front, so a bad column
  # fails clearly here rather than as an opaque FFmpeg (or framerate) error.
  if (has_ts) {
    if (!(is.numeric(jobs$timestamp) || is.character(jobs$timestamp))) {
      cli::cli_abort("The {.field timestamp} column of {.arg jobs} must be numeric or character.")
    }
    if (anyNA(jobs$timestamp)) {
      cli::cli_abort("The {.field timestamp} column of {.arg jobs} must not contain {.val {NA}}.")
    }
    # A numeric timestamp must be finite (parity with extract_frame()'s
    # finite = TRUE check); anyNA() above already caught NA/NaN, so this is Inf.
    if (is.numeric(jobs$timestamp) && any(!is.finite(jobs$timestamp))) {
      cli::cli_abort("The {.field timestamp} column of {.arg jobs} must be finite.")
    }
  } else {
    if (!is.numeric(jobs$frame)) {
      cli::cli_abort("The {.field frame} column of {.arg jobs} must be numeric.")
    }
    if (anyNA(jobs$frame)) {
      cli::cli_abort("The {.field frame} column of {.arg jobs} must not contain {.val {NA}}.")
    }
    # Whole numbers only (parity with extract_frame()'s check_number_whole() and
    # this verb's documented "whole frame numbers" contract).
    if (any(jobs$frame %% 1 != 0)) {
      cli::cli_abort("The {.field frame} column of {.arg jobs} must contain whole numbers.")
    }
  }

  # A factor input column carries paths as levels; treat them as the strings
  # they are (parity with the character case).
  jobs$input <- as.character(jobs$input)

  # Auto-name outputs when the column is absent: per-input frame names with the
  # image extension, carried on the returned tibble.
  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_frame_names(jobs$input, format = format)
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-frame pipeline per
  # row, sharing frame_pipeline() with extract_frame(). frame->timestamp
  # resolution happens per row (via the input's frame rate); `...` forwards
  # ffm_batch options (verify/manifest/...) to the runner, never to the builder.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      timestamp <- if (!is.null(dots$timestamp)) {
        dots$timestamp
      } else {
        dots$frame / get_framerate(input)
      }
      frame_pipeline(input, output, timestamp)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_standardized_names() ---------------------------------------------

# Derive one output path per input for standardize_videos() when the `output`
# column is absent: `<base>_standardized.<input-ext>` (standardization keeps the
# source container, unlike a frame which becomes an image). Standardization is
# one-input -> one-output, so -- unlike the per-input-numbering siblings -- a
# duplicated input with no explicit `output` would collide; the caller
# (standardize_videos) rejects that up front, so this helper assumes unique
# inputs and stays a pure name map.
derive_standardized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_standardized.", tools::file_ext(input)
  )
}


# standardize_videos() ----------------------------------------------------

#' Standardize Many Videos From a Jobs Table
#'
#' Re-encode many input files to a reproducible format from a single jobs tibble
#' — a table-driven sibling of \code{\link{standardize_video}} for when you have
#' more than one video to standardize. Each row is one input; the only required
#' column names its source. This is a thin wrapper over \code{\link{ffm_batch}}:
#' one reproducible compiled command per input.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_standardized} to each input's basename, keeping the input's
#'   extension (e.g. \code{clip.mkv} becomes \code{clip_standardized.mkv}).
#'   Because standardization is one-input-to-one-output, a duplicated
#'   \code{input} with no \code{output} column would collide and is rejected.
#'   Each of the five standardization knobs — \code{width}, \code{height},
#'   \code{fps}, \code{vcodec}, \code{pixel_format} — may also appear as a
#'   column to override the corresponding argument on a per-row basis; rows (or
#'   knobs) that omit the column fall back to the argument's value. Any other
#'   columns are ignored.
#' @param width,height Optional target dimensions applied to every row, unless
#'   \code{jobs} carries a column of the same name (see \code{jobs}). When only
#'   one is given the other is derived to preserve aspect ratio; when neither is
#'   given the frame is floor-cropped to even dimensions so odd-sized sources
#'   encode. (default = \code{NULL})
#' @param fps Optional target frame rate applied to every row, unless
#'   \code{jobs} carries an \code{fps} column. (default = \code{NULL}, i.e.
#'   leave the frame rate unchanged)
#' @param vcodec A string naming the video codec applied to every row, unless
#'   \code{jobs} carries a \code{vcodec} column. (default = \code{"libx264"})
#' @param pixel_format A string naming the pixel format applied to every row,
#'   unless \code{jobs} carries a \code{pixel_format} column.
#'   (default = \code{"yuv420p"})
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: standardize in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one input at a
#'   time and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @seealso \code{\link{standardize_video}} for the single-input form;
#'   \code{\link{ffm_batch}} for the batch runner and the arguments forwarded
#'   through \code{...}; \code{\link{segment_videos}} and
#'   \code{\link{extract_frames}} for the other table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   output = c("a.mp4", "b.mp4"),
#'   width  = c(640, 320)
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' standardize_videos(jobs, run = FALSE)
#' @export
standardize_videos <- function(jobs, width = NULL, height = NULL, fps = NULL,
                               vcodec = "libx264", pixel_format = "yuv420p",
                               run = TRUE, parallel = FALSE, ...) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with extract_frames()).
  jobs$input <- as.character(jobs$input)

  # Validate present override columns up front so a bad column fails clearly
  # here rather than as an opaque FFmpeg error mid-batch (M11 parity lesson).
  # Value-level checks (positive dimensions, known codec/pixfmt) are inherited
  # per row from standardize_pipeline()'s check_dim/check_token guards.
  dim_cols <- c("width", "height", "fps")
  for (col in intersect(dim_cols, names(jobs))) {
    if (!(is.numeric(jobs[[col]]) || is.character(jobs[[col]]))) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric or character.")
    }
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  str_cols <- c("vcodec", "pixel_format")
  for (col in intersect(str_cols, names(jobs))) {
    if (!is.character(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be character (no {.val {NA}}).")
    }
  }

  # Auto-name outputs when the column is absent. One input -> one output, so a
  # duplicated input with no explicit output would map to the same file; reject
  # that rather than silently overwrite (the deliberate trade-off for readable
  # `_standardized` names over sibling-style per-input numbering).
  if (!"output" %in% names(jobs)) {
    dupes <- unique(jobs$input[duplicated(jobs$input)])
    if (length(dupes) > 0) {
      cli::cli_abort(c(
        "{.arg jobs} has duplicated {.field input} paths but no {.field output} column.",
        "x" = "Duplicated input{?s}: {.val {dupes}}.",
        "i" = "Add an {.field output} column to name each row's destination."
      ))
    }
    jobs$output <- derive_standardized_names(jobs$input)
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output re-encode
  # pipeline per row, sharing standardize_pipeline() with standardize_video().
  # A per-row knob column (arriving via `...` from pmap) overrides the scalar
  # arg of the same name; `...` also forwards ffm_batch options
  # (verify/manifest/...) to the runner, never to the pipeline builder.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      standardize_pipeline(
        input, output,
        width = pick("width", width),
        height = pick("height", height),
        fps = pick("fps", fps),
        vcodec = pick("vcodec", vcodec),
        pixel_format = pick("pixel_format", pixel_format)
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_normalized_names() -----------------------------------------------

# Derive one output path per input for normalize_audios() when the `output`
# column is absent: `<base>_normalized.<input-ext>` (loudness normalization
# keeps the source container). One input -> one output, so a duplicated input
# with no explicit `output` would collide; the caller (normalize_audios)
# rejects that up front, so this helper assumes unique inputs and stays a pure
# name map (parity with derive_standardized_names()).
derive_normalized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_normalized.", tools::file_ext(input)
  )
}


# normalize_audios() ------------------------------------------------------

#' Normalize Many Files' Audio Loudness From a Jobs Table
#'
#' Loudness-normalize the audio of many input files (EBU R128) from a single
#' jobs tibble — a table-driven sibling of \code{\link{normalize_audio}} for
#' when you have more than one file to normalize. Each row is one input; the
#' only required column names its source. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same single-pass \code{loudnorm} pipeline (and per-value validation) as
#' the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_normalized} to each input's basename, keeping the input's extension
#'   (e.g. \code{clip.mkv} becomes \code{clip_normalized.mkv}). Because
#'   normalization is one-input-to-one-output, a duplicated \code{input} with no
#'   \code{output} column would collide and is rejected. Each of the five
#'   loudness knobs — \code{target_loudness}, \code{true_peak},
#'   \code{loudness_range}, \code{channels}, \code{sample_rate} — may also appear
#'   as a column to override the corresponding argument on a per-row basis; rows
#'   (or knobs) that omit the column fall back to the argument's value. Any other
#'   columns are ignored.
#' @param target_loudness,true_peak,loudness_range The EBU R128 loudness targets
#'   applied to every row, unless \code{jobs} carries a column of the same name
#'   (see \code{jobs}). Defaults follow EBU Recommendation R 128 (2014):
#'   \code{target_loudness = -23} LUFS, \code{true_peak = -1} dBTP,
#'   \code{loudness_range = 7} LU.
#' @param channels The output channel count applied to every row, unless
#'   \code{jobs} carries a \code{channels} column, e.g. \code{1} to downmix to
#'   mono. \code{NULL} (default) keeps each source's channel layout.
#' @param sample_rate The output sample rate in Hz applied to every row, unless
#'   \code{jobs} carries a \code{sample_rate} column. \code{NULL} (default) lets
#'   \code{loudnorm} choose (it resamples, up to 192 kHz encoder-capped — not the
#'   source rate); set this to pin the output rate.
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: normalize in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one input at a time
#'   and warns. Set a plan first, e.g.
#'   \code{future::plan(future::multisession)}.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and, when \code{output} was derived, the resolved \code{output} column;
#'   when \code{run = TRUE}, a \code{success} column, plus any columns the
#'   forwarded arguments add, e.g. \code{verified}).
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' @seealso \code{\link{normalize_audio}} for the single-input form;
#'   \code{\link{ffm_batch}} for the batch runner and the arguments forwarded
#'   through \code{...}; \code{\link{standardize_videos}} for the video-side
#'   table-driven sibling.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input           = c(video, video),
#'   output          = c("a.mp4", "b.mp4"),
#'   target_loudness = c(-23, -16)
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' normalize_audios(jobs, run = FALSE)
#' @export
normalize_audios <- function(jobs, target_loudness = -23, true_peak = -1,
                             loudness_range = 7, channels = NULL,
                             sample_rate = NULL, two_pass = FALSE, run = TRUE,
                             parallel = FALSE, ...) {

  rlang::check_bool(two_pass)
  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ))
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with standardize_videos()).
  jobs$input <- as.character(jobs$input)

  # Validate present override columns up front so a bad column fails clearly
  # here rather than as an opaque FFmpeg error mid-batch (M11 parity lesson).
  # Value-level checks (loudness ranges, whole channels/sample_rate) are
  # inherited per row from normalize_audio_pipeline()'s ffm_loudnorm() and
  # check_number_whole() guards.
  knob_cols <- c("target_loudness", "true_peak", "loudness_range",
                 "channels", "sample_rate")
  for (col in intersect(knob_cols, names(jobs))) {
    if (!is.numeric(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric.")
    }
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }

  # Auto-name outputs when the column is absent. One input -> one output, so a
  # duplicated input with no explicit output would map to the same file; reject
  # that rather than silently overwrite (parity with standardize_videos()).
  if (!"output" %in% names(jobs)) {
    dupes <- unique(jobs$input[duplicated(jobs$input)])
    if (length(dupes) > 0) {
      cli::cli_abort(c(
        "{.arg jobs} has duplicated {.field input} paths but no {.field output} column.",
        "x" = "Duplicated input{?s}: {.val {dupes}}.",
        "i" = "Add an {.field output} column to name each row's destination."
      ))
    }
    jobs$output <- derive_normalized_names(jobs$input)
  }

  # Two-pass (measured/linear): the audio-side M16 analyze-then-build path fanned
  # across the jobs table (D013). Phase 1 measures every input (honoring
  # `parallel`) and appends the five measured columns; Phase 2 builds & runs one
  # linear correction per row from them. Fail-fast before Phase 2 if any row's
  # analysis did not yield a usable measurement (assemble_measured names the
  # row). Like the scalar verb, `run = FALSE` still runs Phase 1 (it needs the
  # binary and readable inputs) and gates only the Phase 2 correction commands.
  if (two_pass) {
    col_or <- function(nm, default) {
      if (nm %in% names(jobs)) jobs[[nm]] else rep(default, nrow(jobs))
    }
    outputs <- run_loudnorm_analysis_batch(
      jobs$input,
      col_or("target_loudness", target_loudness),
      col_or("true_peak", true_peak),
      col_or("loudness_range", loudness_range),
      parallel
    )
    measured <- assemble_measured(outputs)
    for (nm in names(measured)) jobs[[nm]] <- measured[[nm]]
    return(run_normalize_correction(
      jobs, target_loudness, true_peak, loudness_range, channels, sample_rate,
      run = run, parallel = parallel, ...
    ))
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output loudnorm
  # pipeline per row, sharing normalize_audio_pipeline() with normalize_audio().
  # A per-row knob column (arriving via `...` from pmap) overrides the scalar
  # arg of the same name; `...` also forwards ffm_batch options
  # (verify/manifest/...) to the runner, never to the pipeline builder (the
  # runner's params sit after `...` and bind by name — M09 lesson).
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      normalize_audio_pipeline(
        input, output,
        target_loudness = pick("target_loudness", target_loudness),
        true_peak = pick("true_peak", true_peak),
        loudness_range = pick("loudness_range", loudness_range),
        channels = pick("channels", channels),
        sample_rate = pick("sample_rate", sample_rate)
      )
    },
    run = run,
    parallel = parallel,
    ...
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


# picture_in_picture() ----------------------------------------------------

#' Inset one video over another (picture-in-picture)
#'
#' Composite a smaller \code{overlay} video onto a \code{main} video in one
#' corner (or the center) — the classic picture-in-picture layout for pairing a
#' speaker with a screen recording, or a stimulus with a webcam. Built on the
#' blessed \code{\link{ffm_overlay}} verb, which resizes the overlay to a
#' fraction of the main video's width and positions it.
#'
#' Audio is dropped unless \code{audio} names an input to carry (\code{0} = the
#' main video, \code{1} = the overlay).
#'
#' @param main A string giving the path to the background (full-size) video.
#' @param overlay A string giving the path to the inset video.
#' @param outfile A string giving the path to write the result to.
#' @param position Where to place the inset: one of \code{"topright"} (default),
#'   \code{"topleft"}, \code{"bottomright"}, \code{"bottomleft"}, or
#'   \code{"center"}.
#' @param scale The inset's width as a fraction of the main video's width, aspect
#'   preserved (\code{0 < scale <= 1}). (default = \code{0.25})
#' @param margin The gap in pixels between the inset and the video edges (ignored
#'   for \code{position = "center"}). (default = \code{16})
#' @param audio The 0-based index of the input whose audio to keep, or
#'   \code{NULL} to drop audio. (default = \code{NULL})
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' picture_in_picture(video, video, "pip.mp4", run = FALSE)
#' @export
picture_in_picture <- function(main, overlay, outfile,
                               position = c("topright", "topleft",
                                            "bottomright", "bottomleft",
                                            "center"),
                               scale = 0.25, margin = 16, audio = NULL,
                               run = TRUE) {

  check_file_exists(main)
  check_file_exists(overlay)
  rlang::check_string(outfile)
  position <- rlang::arg_match(position)
  rlang::check_number_decimal(scale)
  rlang::check_number_whole(margin, min = 0)
  rlang::check_number_whole(audio, min = 0, max = 1, allow_null = TRUE)
  m <- as.integer(margin)

  # Translate the corner/center choice into overlay x/y expressions, where
  # overlay_w/overlay_h are the (already scaled) inset's dimensions.
  pos <- switch(
    position,
    topleft     = list(x = as.character(m), y = as.character(m)),
    topright    = list(x = sprintf("main_w-overlay_w-%d", m),
                       y = as.character(m)),
    bottomleft  = list(x = as.character(m),
                       y = sprintf("main_h-overlay_h-%d", m)),
    bottomright = list(x = sprintf("main_w-overlay_w-%d", m),
                       y = sprintf("main_h-overlay_h-%d", m)),
    center      = list(x = "(main_w-overlay_w)/2",
                       y = "(main_h-overlay_h)/2")
  )

  p <- ffm_files(c(main, overlay), outfile)
  p <- ffm_overlay(p, x = pos$x, y = pos$y, scale = scale)
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
