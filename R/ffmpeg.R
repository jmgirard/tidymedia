
# ffmpeg() ----------------------------------------------------------------

#' Run a raw FFmpeg command
#'
#' Send a raw argument string to the FFmpeg command-line program. This is the
#' Layer 0 escape hatch: the string is passed to FFmpeg verbatim (after the
#' executable path), so the caller is responsible for quoting and option order.
#'
#' @param command A string containing the arguments to pass to FFmpeg.
#' @return A character vector containing the text output by FFmpeg.
#' @seealso [ffmpeg_codecs()] and [ffmpeg_encoders()] for structured capability
#'   queries, and the `ffm_*` pipeline builders (e.g. [ffm_run()]) for a safer
#'   command layer.
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

# extract_frame_batch() --------------------------------------------------------

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
#' @seealso [ffm_seek()], the builder it uses to grab the frame;
#'   [extract_frame_batch()] for the many-file (batch) form.
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

  if (rlang::is_null(timestamp)) timestamp <- frame / get_frame_rate(infile)

  ffm_finish(frame_pipeline(infile, outfile, timestamp), run)
}


# frame_pipeline() --------------------------------------------------------

# Shared single-frame grab for extract_frame() and extract_frame_batch(): a fast
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


# sample_frames() ---------------------------------------------------------

#' Sample frames from a video at a fixed rate
#'
#' Sample a video at a fixed rate (\code{fps}) or interval (\code{interval},
#' seconds between frames) into a numbered image sequence — the front door to
#' per-frame coding and computer-vision feature pipelines. Provide exactly one
#' of \code{fps} or \code{interval}.
#'
#' Unlike \code{\link{extract_frame}} (one frame) and
#' \code{\link{extract_frame_batch}} (a caller-enumerated set of frames), this
#' verb emits a \emph{single} FFmpeg command whose output is a printf-style
#' pattern that FFmpeg's \code{image2} muxer fills — the frame count is decided
#' at decode time, not enumerated by the caller. Frames are written to
#' \code{outdir} as \code{<prefix>_<n>.<format>}, where \code{<n>} is a
#' zero-padded integer starting at 1.
#'
#' @param infile A string containing the path to a video file.
#' @param outdir A string naming the directory to write the image sequence to.
#'   It is created (recursively) if it does not exist.
#' @param fps The sampling rate, in frames per second: either a positive number
#'   or an FFmpeg framerate expression string (for example \code{"30000/1001"}).
#'   Provide exactly one of \code{fps} or \code{interval}.
#' @param interval The number of seconds between sampled frames (a positive
#'   number); the reciprocal is used as the frame rate. Provide exactly one of
#'   \code{fps} or \code{interval}.
#' @param format A string giving the output image file extension (one of
#'   \code{"png"}, \code{"jpg"}, \code{"jpeg"}, \code{"bmp"}, \code{"tif"},
#'   \code{"tiff"}, \code{"webp"}). (default = \code{"png"})
#' @param prefix A string used as the basename stem of each image, or
#'   \code{NULL} to derive it from \code{infile}'s basename. (default =
#'   \code{NULL})
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_fps()], the builder it uses to set the sampling rate;
#'   [extract_frame()] for a single frame and [extract_frame_batch()] for a
#'   caller-enumerated set; [sample_frames_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # run = FALSE returns the reproducible command instead of executing it
#' sample_frames(video, tempdir(), fps = 2, run = FALSE)
#' @export
sample_frames <- function(infile, outdir, fps = NULL, interval = NULL,
                          format = "png", prefix = NULL, run = TRUE) {
  check_file_exists(infile)
  rlang::check_string(outdir)
  if (!is.null(prefix)) rlang::check_string(prefix)
  format <- check_image_format(format)
  fps <- resolve_sample_fps(fps, interval)
  outdir <- ensure_dir(outdir)

  pattern <- derive_frame_pattern(infile, outdir, prefix, format)
  ffm_finish(sample_frames_pipeline(infile, pattern, fps), run)
}


# sample_frames_pipeline() ------------------------------------------------

# Shared fixed-rate sampling pipeline for sample_frames() and
# sample_frames_batch(): a constant-rate fps filter into an image2 printf
# pattern, with a quality flag for the still encoder. Both verbs build identical
# commands from this helper; the fps filter's own value check (check_dim() via
# ffm_fps()) is inherited here, so the batch sibling gets per-row parity for
# free (M13). `output` is a %0Nd pattern, so the single command fans out to many
# image files (D003: still one input chain, one output target).
sample_frames_pipeline <- function(input, output, fps) {
  p <- ffm_files(input, output)
  p <- ffm_fps(p, fps = fps)
  ffm_output_options(p, "-qscale:v 2")
}


# resolve_sample_fps() ----------------------------------------------------

# Resolve the exclusive fps/interval pair to a single frame-rate value for
# ffm_fps(): `fps` passes through (a positive number, coerced to double so it
# clears check_dim()'s integer rejection, M20; or an FFmpeg rate-expression
# string), while `interval` (seconds/frame) becomes its reciprocal. Enforces the
# exactly-one contract, mirroring extract_frame()'s timestamp/frame XOR.
resolve_sample_fps <- function(fps, interval,
                               call = rlang::caller_env()) {
  if (is.null(fps) == is.null(interval)) {
    cli::cli_abort("Provide exactly one of {.arg fps} or {.arg interval}.",
                   call = call)
  }
  if (!is.null(fps)) {
    if (rlang::is_string(fps)) return(fps)
    if (!(rlang::is_bare_numeric(fps, n = 1) && is.finite(fps) && fps > 0)) {
      cli::cli_abort(
        "{.arg fps} must be a single positive number or a string.", call = call
      )
    }
    return(as.double(fps))
  }
  if (!(rlang::is_bare_numeric(interval, n = 1) && is.finite(interval) &&
        interval > 0)) {
    cli::cli_abort("{.arg interval} must be a single positive number.",
                   call = call)
  }
  1 / as.double(interval)
}


# derive_frame_pattern() --------------------------------------------------

# Build the image2 output pattern for one input: `<outdir>/<prefix>_%0Nd.<fmt>`,
# with `prefix` defaulting to the input's basename (sans extension). A fixed pad
# width keeps the numbering zero-padded and lexically sortable; FFmpeg widens it
# automatically past the cap. Basename prefixes distinguish most batch sequences
# sharing one `outdir`, but same-basename inputs still collide — the batch verb
# guards that at the pattern level before running.
derive_frame_pattern <- function(input, outdir, prefix, format, digits = 6L) {
  if (is.null(prefix)) prefix <- tools::file_path_sans_ext(basename(input))
  file.path(outdir, paste0(prefix, "_%0", digits, "d.", format))
}


# check_image_format() ----------------------------------------------------

# Validate `format` as a supported still-image extension, returning it
# lower-cased. Keeps a non-image container (e.g. "mp4") from silently producing
# a broken sequence, and a clean token from reaching the output pattern.
check_image_format <- function(format, arg = rlang::caller_arg(format),
                               call = rlang::caller_env()) {
  rlang::check_string(format, arg = arg, call = call)
  allowed <- c("png", "jpg", "jpeg", "bmp", "tif", "tiff", "webp")
  format <- tolower(format)
  if (!format %in% allowed) {
    cli::cli_abort(
      c("{.arg {arg}} must be a supported image format.",
        "x" = "{.val {format}} is not one of {.val {allowed}}."),
      call = call
    )
  }
  format
}


# ensure_dir() ------------------------------------------------------------

# Create `dir` (recursively) if it is absent, then confirm it exists — so an
# uncreatable path (e.g. under an existing file) aborts here with a clear
# message rather than as an opaque FFmpeg write error.
ensure_dir <- function(dir, arg = rlang::caller_arg(dir),
                       call = rlang::caller_env()) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dir)) {
      cli::cli_abort(
        c("Can't create {.arg {arg}}.", "x" = "Not a creatable directory: {.file {dir}}."),
        call = call
      )
    }
  }
  dir
}

# extract_audio() ---------------------------------------------------------

#' Extract the audio stream from a media file
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the audio file to write.
#' @param audio_codec A string naming the audio codec for the output stream.
#'   (default = \code{"copy"}, i.e. remux without re-encoding)
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_drop()] and [ffm_codec()], the builders it wraps;
#'   [convert_audio()] to re-encode the extracted audio.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' extract_audio(video, "audio.aac", run = FALSE)
#' @export
# Shared recipe behind extract_audio() and extract_audio_batch(): map the audio
# stream out (dropping video), applying `audio_codec` (default "copy" =
# stream-copy, lossless). Holding it here gives the batch sibling per-row parity
# for free (M13); command assembly stays in Layer 1 (IP1/D002).
extract_audio_pipeline <- function(input, output, audio_codec = "copy") {
  p <- ffm_files(input, output)
  p <- ffm_codec(p, audio = audio_codec)
  ffm_drop(p, "video")
}

extract_audio <- function(infile, outfile, audio_codec = "copy", run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)
  rlang::check_string(audio_codec)

  ffm_finish(extract_audio_pipeline(infile, outfile, audio_codec), run)
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
#' @seealso [ffm_map()] and [ffm_codec()], the builders it wraps;
#'   [extract_audio()] to pull out just the audio.
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


# convert_audio() ---------------------------------------------------------

#' Extract or convert a media file's audio track
#'
#' Maps the audio stream of \code{infile} into \code{outfile}. By default
#' (\code{format = NULL}) the output format follows the \code{outfile} file
#' extension at highest VBR quality (\code{-q:a 0}) — e.g. an \code{.mp3}
#' extension yields an MP3. Pass \code{format} to pin the output audio codec
#' explicitly, regardless of the extension.
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the audio file to write.
#' @param format An optional string naming the output audio codec (e.g.
#'   \code{"libmp3lame"}, \code{"aac"}, \code{"flac"}), passed to FFmpeg's
#'   \code{-c:a}. When \code{NULL} (default), the format is inferred from the
#'   \code{outfile} extension.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_codec()] and [ffm_map()], the builders it wraps;
#'   [extract_audio()] to copy audio without re-encoding.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' convert_audio(video, "audio.mp3", run = FALSE)
#' convert_audio(video, "audio.m4a", format = "aac", run = FALSE)
#' @export
# Shared recipe behind convert_audio() and convert_audio_batch(): map the audio
# stream out and either encode at highest VBR quality (`format = NULL`, the
# extension picks the codec) or pin `-c:a` to `format`. The per-value
# check_string(format) lives here so the batch sibling inherits it per row
# (M13); command assembly stays in Layer 1 (IP1/D002).
convert_audio_pipeline <- function(input, output, format = NULL) {
  p <- ffm_files(input, output)
  p <- ffm_map(p, "a")
  if (is.null(format)) {
    p <- ffm_output_options(p, "-q:a 0")
  } else {
    rlang::check_string(format)
    p <- ffm_codec(p, audio = format)
  }
  p
}

convert_audio <- function(infile, outfile, format = NULL, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  ffm_finish(convert_audio_pipeline(infile, outfile, format), run)
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
#' @seealso [ffm_crop()], the builder it wraps.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' crop_video(video, "cropped.mp4", width = 160, height = 120, run = FALSE)
#' @export
# Shared recipe behind crop_video() and crop_video_batch(): a crop filter to the
# requested rectangle mapping every stream through. ffm_crop() carries the
# per-value dimension guards, so the batch sibling inherits them per row (M13);
# command assembly stays in Layer 1 (IP1/D002).
crop_video_pipeline <- function(input, output, width, height,
                                x = "(in_w-out_w)/2", y = "(in_h-out_h)/2") {
  p <- ffm_files(input, output)
  p <- ffm_crop(p, width = width, height = height, x = x, y = y)
  ffm_map(p, "0")
}

crop_video <- function(infile, outfile, width, height,
                       x = "(in_w-out_w)/2", y = "(in_h-out_h)/2",
                       run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  ffm_finish(crop_video_pipeline(infile, outfile, width, height, x, y), run)
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
#' @seealso [ffm_codec()] and [ffm_pixel_format()], among the builders it wraps;
#'   [standardize_video()] for a configurable re-encode.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' format_for_web(video, "web.mp4", run = FALSE)
#' @export
# Shared recipe behind format_for_web() and format_for_web_batch(): the fixed
# web-delivery re-encode (H.264 + yuv420p + AAC + faststart), padding odd
# dimensions down to even as the codec requires. No per-row knobs — every input
# gets the same recipe. Command assembly stays in Layer 1 (IP1/D002).
format_for_web_pipeline <- function(input, output) {
  p <- ffm_files(input, output)
  p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")
  p <- ffm_codec(p, video = "libx264", audio = "aac")
  p <- ffm_pixel_format(p, "yuv420p")
  ffm_output_options(p, "-movflags +faststart")
}

format_for_web <- function(infile, outfile, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  ffm_finish(format_for_web_pipeline(infile, outfile), run)
}


# strip_metadata() --------------------------------------------------------

# Shared recipe behind strip_metadata() and strip_metadata_batch(): a lossless
# stream copy that discards all container/global metadata and chapters, and
# muxes bit-exactly so FFmpeg does not re-stamp its own creation_time / encoder
# tag onto the output. Holding it here gives the batch sibling per-row parity
# for free (M13). Metadata scrubbing is pure command assembly, so compile stays
# binary-free (IP1/D002).
strip_metadata_pipeline <- function(input, output) {
  p <- ffm_files(input, output)
  # -c:v copy -c:a copy -map 0: carry every stream through untouched.
  p <- ffm_copy(p)
  # -map_metadata -1 drops global tags (creation_time, location/GPS, make/model,
  # title, comment); -map_chapters -1 drops chapters; -fflags +bitexact stops the
  # muxer writing a fresh creation_time and an encoder=Lavf... tag. Per-stream
  # tags (handler_name, language) and codec-embedded identifiers survive a copy.
  ffm_output_options(
    p, "-map_metadata -1", "-map_chapters -1", "-fflags +bitexact"
  )
}

#' Strip identifying metadata from a media file
#'
#' Remove a media file's container and global metadata tags (creation time,
#' GPS/location, device make and model, title, comment, and the like) together
#' with any chapters, writing a de-identified copy — the front door for
#' IRB/de-identification of research recordings. The audio and video streams are
#' **stream-copied**, not re-encoded, so the operation is lossless and fast and
#' the picture and sound are bit-for-bit unchanged (including any rotation
#' display matrix, which is stream side data, not a metadata tag).
#'
#' @details
#' The output is muxed bit-exactly (\code{-fflags +bitexact}) so FFmpeg does not
#' re-stamp the container with a fresh \code{creation_time} or an
#' \code{encoder} tag naming its own version — either of which would defeat
#' de-identification and reproducibility.
#'
#' Because the streams are copied rather than re-encoded, identifiers embedded
#' **inside** the encoded bitstream, and per-stream metadata such as
#' \code{handler_name} or \code{language}, are not removed. Removing those would
#' require re-encoding (out of scope; use the \code{\link{ffmpeg}} escape hatch)
#' or per-stream metadata mapping that must probe the file first.
#'
#' @param infile A string containing the path to a media file.
#' @param outfile A string containing the path of the de-identified file to
#'   write. Use the same container extension as \code{infile} so the copied
#'   streams remux cleanly.
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [anonymize_video()] to remove faces or regions from the picture (the
#'   visual de-identification sibling); [probe_container()] and
#'   [mediainfo_query()] to inspect a file's metadata before and after;
#'   [ffm_copy()] and [ffm_output_options()], the builders it wraps;
#'   [strip_metadata_batch()] for the many-file form.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' strip_metadata(video, "clean.mp4", run = FALSE)
#' @export
strip_metadata <- function(infile, outfile, run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  p <- strip_metadata_pipeline(infile, outfile)
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
#' H.264 video (\code{video_codec = "libx264"}) with \code{pixel_format = "yuv420p"}
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
#' @param video_codec A string naming the output video codec (default
#'   \code{"libx264"}).
#' @param pixel_format A string naming the output pixel format (default
#'   \code{"yuv420p"}).
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_scale()], [ffm_codec()], and [ffm_pixel_format()], among the
#'   builders it wraps; [standardize_video_batch()] for the many-file form.
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
                              video_codec = "libx264", pixel_format = "yuv420p",
                              run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  ffm_finish(
    standardize_pipeline(infile, outfile, width, height, fps, video_codec,
                         pixel_format),
    run
  )
}


# standardize_pipeline() --------------------------------------------------

# Shared standardization pipeline for standardize_video() and
# standardize_video_batch(): build one single-output re-encode pipeline for a single
# input. Both verbs compile identical commands from this helper, so per-value
# validation (dimensions via check_dim, codec/pixfmt via check_token) and M12's
# guards (audio stream-copy, even-dimension safeguard, +faststart) live here
# once -- the batch sibling inherits them by construction (D002, D003, D007).
standardize_pipeline <- function(input, output, width, height, fps, video_codec,
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
  p <- ffm_codec(p, video = video_codec, audio = "copy")
  p <- ffm_pixel_format(p, pixel_format)
  ffm_output_options(p, "-movflags +faststart")
}


# anonymize_video() -------------------------------------------------------

#' Cover fixed regions of a video with opaque boxes
#'
#' Anonymize a video by covering one or more fixed rectangular regions with
#' opaque filled boxes -- for example, to redact a face, a name badge, or a
#' screen that stays in one place for the whole clip. The regions are fixed
#' (there is no face or object tracking), so this suits footage where the areas
#' to cover do not move.
#'
#' @details
#' \code{regions} is a data frame with one row per box and the columns
#' \code{x}, \code{y}, \code{width}, and \code{height} (each a pixel number or an
#' FFmpeg expression such as \code{"in_w/2"}); \code{x}/\code{y} give the
#' top-left corner and \code{width}/\code{height} the size. An optional
#' \code{color} column overrides the \code{color} argument for that row. Every
#' box is a solid fill (FFmpeg's \code{drawbox} with \code{t=fill}); hollow
#' outlines are intentionally not offered.
#'
#' Because a filter is applied, the video is re-encoded (\code{video_codec} /
#' \code{pixel_format}, defaulting to H.264 / \code{yuv420p}); odd source
#' dimensions are floored to even so the output always encodes (a
#' \code{yuv420p}/\code{libx264} requirement, and a no-op for already-even
#' input). Audio is stream-copied unchanged (\code{-c:a copy}). The same input
#' and regions therefore always compile to a byte-identical command.
#'
#' @param infile A string containing the path to a video file.
#' @param outfile A string containing the path of the video file to write.
#' @param regions A data frame with one row per box and columns \code{x},
#'   \code{y}, \code{width}, \code{height} (and optionally \code{color}); see
#'   Details.
#' @param color A string naming the default fill color in FFmpeg color syntax,
#'   used for any row without its own \code{color} (default \code{"black"}).
#' @param video_codec A string naming the output video codec (default
#'   \code{"libx264"}).
#' @param pixel_format A string naming the output pixel format (default
#'   \code{"yuv420p"}).
#' @param run A logical: run the command through FFmpeg (\code{TRUE}, default)
#'   or return the compiled command without running it (\code{FALSE}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}).
#' @seealso [ffm_drawbox()], the builder filter it wraps; [anonymize_video_batch()]
#'   for the many-file (batch) form.
#' @references https://ffmpeg.org/ffmpeg-filters.html#drawbox
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Cover two fixed regions with black boxes
#' regions <- data.frame(
#'   x = c(10, 200), y = c(10, 150),
#'   width = c(120, 80), height = c(90, 60)
#' )
#' anonymize_video(video, "anon.mp4", regions, run = FALSE)
#' @export
anonymize_video <- function(infile, outfile, regions,
                            color = "black",
                            video_codec = "libx264", pixel_format = "yuv420p",
                            run = TRUE) {

  check_file_exists(infile)
  rlang::check_string(outfile)

  ffm_finish(
    anonymize_pipeline(infile, outfile, regions, color, video_codec, pixel_format),
    run
  )
}


# anonymize_pipeline() ----------------------------------------------------

# Shared anonymization pipeline for anonymize_video() and (M21) the batch
# sibling: build one single-output box-fill pipeline for a single input. Both
# verbs compile identical commands from this helper, so region-table validation
# and the encode guards (even-dimension safeguard, audio stream-copy) live here
# once -- the batch sibling inherits them by construction (D002, D003, D007;
# M13 extract-first lesson).
anonymize_pipeline <- function(input, output, regions, color, video_codec,
                               pixel_format, call = rlang::caller_env()) {
  check_regions(regions, call = call)
  rlang::check_string(color, call = call)
  check_token(video_codec, call = call)
  check_token(pixel_format, call = call)

  # Integer coordinates are natural pixel values, but ffm_drawbox()'s check_dim()
  # accepts only doubles or expression strings; coerce numeric columns so an
  # integer/integerish table is not rejected.
  for (col in c("x", "y", "width", "height")) {
    if (is.numeric(regions[[col]])) regions[[col]] <- as.double(regions[[col]])
  }

  p <- ffm_files(input, output)
  # Force even output dimensions so yuv420p/libx264 can encode odd-dimensioned
  # sources (M12 guard); a no-op for already-even input. drawbox coordinates use
  # a top-left origin, so the <=1px floor never shifts a region's x/y.
  p <- ffm_crop(p, width = "floor(in_w/2)*2", height = "floor(in_h/2)*2")

  colors <- if ("color" %in% names(regions)) {
    regions$color
  } else {
    rep(color, nrow(regions))
  }
  # One filled drawbox per region; ffm_drawbox() validates each x/y/w/h.
  for (i in seq_len(nrow(regions))) {
    p <- ffm_drawbox(
      p,
      x = regions$x[i], y = regions$y[i],
      width = regions$width[i], height = regions$height[i],
      color = colors[i], thickness = "fill"
    )
  }
  # Re-encode video (a filter is applied), stream-copy audio untouched -- the
  # same encode profile as standardize_video().
  p <- ffm_codec(p, video = video_codec, audio = "copy")
  ffm_pixel_format(p, pixel_format)
}


# check_regions() ---------------------------------------------------------

# Validate the `regions` data frame for anonymize_video()/its batch sibling:
# structure and column type/NA only. Per-value dimension checks (positive size,
# valid expression) are inherited per row from ffm_drawbox()'s check_dim().
check_regions <- function(regions, call = rlang::caller_env()) {
  if (!is.data.frame(regions)) {
    cli::cli_abort("{.arg regions} must be a data frame with one row per box.",
                   call = call)
  }
  if (nrow(regions) == 0) {
    cli::cli_abort("{.arg regions} must have at least one row.", call = call)
  }
  required <- c("x", "y", "width", "height")
  missing <- setdiff(required, names(regions))
  if (length(missing)) {
    cli::cli_abort(
      c(
        "{.arg regions} is missing {length(missing)} required column{?s}: {.field {missing}}.",
        "i" = "Each row needs {.field x}, {.field y}, {.field width}, and {.field height}."
      ),
      call = call
    )
  }
  for (col in required) {
    v <- regions[[col]]
    if (!(is.numeric(v) || is.character(v))) {
      cli::cli_abort(
        "The {.field {col}} column of {.arg regions} must be numeric or character.",
        call = call
      )
    }
    if (anyNA(v)) {
      cli::cli_abort(
        "The {.field {col}} column of {.arg regions} must not contain {.val {NA}}.",
        call = call
      )
    }
  }
  if ("color" %in% names(regions)) {
    v <- regions$color
    if (!is.character(v) || anyNA(v)) {
      cli::cli_abort(
        "The {.field color} column of {.arg regions} must be character (no {.val {NA}}).",
        call = call
      )
    }
  }
  invisible(regions)
}


# derive_anonymized_names() -----------------------------------------------

# Derive one output path per input for anonymize_video_batch() when the `output`
# column is absent: `<base>_anonymized.<input-ext>` (box-fill re-encodes but
# keeps the source container). One input -> one output, so a duplicated input
# with no explicit `output` would collide; the caller (anonymize_video_batch)
# rejects that up front, so this helper assumes unique inputs and stays a pure
# name map (parity with derive_standardized_names()).
derive_anonymized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_anonymized.", tools::file_ext(input)
  )
}


# anonymize_video_batch() ------------------------------------------------------

#' Anonymize Many Videos From a Jobs Table
#'
#' Cover fixed rectangular regions of many input videos with opaque filled boxes
#' from a single jobs tibble — the **batch** (table-driven) sibling of
#' [anonymize_video()] for when you have more than one video to
#' redact. Each row is one input with its own regions; the required columns name
#' the source (\code{input}) and the boxes to cover (\code{regions}). This is a
#' thin wrapper over \code{\link{ffm_batch}}: one reproducible compiled command
#' per input, sharing the same box-fill pipeline (and per-region validation) as
#' the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) and a \code{regions} list-column. Each
#'   \code{regions} cell is itself a data frame of boxes for that input — the
#'   same \code{x}/\code{y}/\code{width}/\code{height} (and optional per-box
#'   \code{color}) shape \code{\link{anonymize_video}} takes. An optional
#'   \code{output} column names the destination; when absent, one is derived per
#'   row by appending \code{_anonymized} to each input's basename, keeping the
#'   input's extension (e.g. \code{clip.mkv} becomes \code{clip_anonymized.mkv}).
#'   Because anonymization is one-input-to-one-output, a duplicated \code{input}
#'   with no \code{output} column would collide and is rejected. Each of the
#'   three encode knobs — \code{color}, \code{video_codec}, \code{pixel_format} — may
#'   also appear as a column to override the corresponding argument on a per-row
#'   basis; rows (or knobs) that omit the column fall back to the argument's
#'   value. Any other columns are ignored.
#' @param color A string naming the default fill color (FFmpeg color syntax)
#'   applied to every row, unless \code{jobs} carries a \code{color} column or a
#'   box supplies its own \code{color}. (default = \code{"black"})
#' @param video_codec A string naming the output video codec applied to every row,
#'   unless \code{jobs} carries a \code{video_codec} column. (default =
#'   \code{"libx264"})
#' @param pixel_format A string naming the output pixel format applied to every
#'   row, unless \code{jobs} carries a \code{pixel_format} column.
#'   (default = \code{"yuv420p"})
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: anonymize in
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
#' @seealso [anonymize_video()] for the single-input form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [standardize_video_batch()] and [segment_video_batch()] for the other
#'   table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input   = c(video, video),
#'   output  = c("a.mp4", "b.mp4"),
#'   regions = list(
#'     data.frame(x = 10, y = 10, width = 120, height = 90),
#'     data.frame(x = 200, y = 150, width = 80, height = 60)
#'   )
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' anonymize_video_batch(jobs, run = FALSE)
#' @export
anonymize_video_batch <- function(jobs, color = "black", video_codec = "libx264",
                             pixel_format = "yuv420p", run = TRUE,
                             parallel = FALSE, ...) {

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
  if (!"regions" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have a {.field regions} column.",
      "x" = "Missing column: {.val regions}.",
      "i" = "Make it a list-column: one boxes data frame per input row."
    ))
  }
  # The regions column is a list-column (one boxes data frame per row); a flat
  # column can't hold per-row tables, so reject it here with a clear message
  # rather than as an opaque per-row abort. Each cell's structure is validated
  # per row by check_regions() inside anonymize_pipeline() (inherited, reported
  # by row index via purrr; M13 extract-first lesson).
  if (!is.list(jobs$regions) || is.data.frame(jobs$regions)) {
    cli::cli_abort(c(
      "The {.field regions} column of {.arg jobs} must be a list-column.",
      "i" = "Each element is a boxes data frame, one per input row."
    ))
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with standardize_video_batch()).
  jobs$input <- as.character(jobs$input)

  # Validate present override columns up front so a bad column fails clearly
  # here rather than as an opaque FFmpeg error mid-batch (M11 parity lesson).
  # Value-level checks (valid color/codec/pixfmt tokens) are inherited per row
  # from anonymize_pipeline()'s guards.
  str_cols <- c("color", "video_codec", "pixel_format")
  for (col in intersect(str_cols, names(jobs))) {
    if (!is.character(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be character (no {.val {NA}}).")
    }
  }

  # Auto-name outputs when the column is absent. One input -> one output, so a
  # duplicated input with no explicit output would map to the same file; reject
  # that rather than silently overwrite (parity with standardize_video_batch()).
  if (!"output" %in% names(jobs)) {
    dupes <- unique(jobs$input[duplicated(jobs$input)])
    if (length(dupes) > 0) {
      cli::cli_abort(c(
        "{.arg jobs} has duplicated {.field input} paths but no {.field output} column.",
        "x" = "Duplicated input{?s}: {.val {dupes}}.",
        "i" = "Add an {.field output} column to name each row's destination."
      ))
    }
    jobs$output <- derive_anonymized_names(jobs$input)
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output box-fill
  # pipeline per row, sharing anonymize_pipeline() with anonymize_video(). A
  # per-row knob column (arriving via `...` from pmap) overrides the scalar arg
  # of the same name; `...` also forwards ffm_batch options (verify/manifest/...)
  # to the runner, never to the pipeline builder. The `regions` list-column
  # arrives unwrapped per row (pmap passes each cell's data frame by name).
  ffm_batch(
    jobs,
    function(input, output, regions, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      anonymize_pipeline(
        input, output, regions,
        color = pick("color", color),
        video_codec = pick("video_codec", video_codec),
        pixel_format = pick("pixel_format", pixel_format)
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
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
#'   binary under \code{run = FALSE}. If the input is \strong{silent}, the
#'   analysis pass measures its loudness as \code{-inf}; normalizing silence to
#'   a target is undefined, so two-pass aborts with a clear error (the
#'   single-pass default leaves silence untouched).
#' @param run A logical: run the (correction) command through FFmpeg
#'   (\code{TRUE}, default) or return the compiled command without running it
#'   (\code{FALSE}). Under \code{two_pass = TRUE} this gates only the correction
#'   pass; the analysis pass runs regardless (see \code{two_pass}).
#' @return The compiled FFmpeg command (invisibly when \code{run = TRUE}). Under
#'   \code{two_pass = TRUE} this is the correction command built from the
#'   measured values.
#' @seealso [ffm_loudnorm()], the builder it wraps; [normalize_audio_batch()]
#'   for the many-file form; [standardize_video()], its video-side complement.
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
# normalize_audio_batch(): build one single-output pipeline for a single input. Both
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


# ffmpeg_codecs() ------------------------------------------------------------

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
#' @seealso [ffmpeg_encoders()] for the encoder list, [ffm_codec()] to set a
#'   codec in a pipeline, and [ffmpeg()] for the Layer 0 escape hatch.
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' head(ffmpeg_codecs())
#' ffmpeg_codecs(sort_by_type = FALSE)
#' @export
ffmpeg_codecs <- function(sort_by_type = TRUE) {
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

# ffmpeg_encoders() ------------------------------------------------------------

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
#' @seealso [ffmpeg_codecs()] for the codec list, [ffm_codec()] to set a codec
#'   in a pipeline, and [ffmpeg()] for the Layer 0 escape hatch.
#' @family capability functions
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' head(ffmpeg_encoders())
#' ffmpeg_encoders(sort_by_type = FALSE)
#' @export
ffmpeg_encoders <- function(sort_by_type = TRUE) {

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
#' segment (based on the order provided in \code{start} and \code{end}).
#'
#' @param infile A string containing the path to a video file.
#' @param start A vector containing one or more timestamps indicating the
#'   start of each segment to create. Can be either a numeric vector indicating
#'   seconds or a character vector with time duration syntax. Must have the same
#'   length as \code{end}.
#' @param end A vector containing one or more timestamps indicating the stop
#'   of each segment to create. Can be either a numeric vector indicating
#'   seconds or a character vector with time duration syntax. Must have the same
#'   length as \code{start}.
#' @param outfiles Either NULL or a character vector indicating the filename
#'   (with extension) for each segment to create. If NULL, will append a
#'   zero-padded integer to \code{infile}. If not NULL, must have the same
#'   length as \code{start}.
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
#' @seealso [ffm_seek()], the builder it uses to cut; [ffm_batch()], the runner;
#'   [segment_video_batch()] for the many-file form.
#' @references https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Two segments; run = FALSE compiles one command per segment
#' segment_video(video, start = c(0, 0.5), end = c(0.5, 1), run = FALSE)
#' @export
segment_video <- function(infile,
                          start,
                          end,
                          outfiles = NULL,
                          reencode = TRUE,
                          run = TRUE,
                          parallel = FALSE) {

  check_file_exists(infile)
  if (!(is.numeric(start) || is.character(start))) {
    cli::cli_abort("{.arg start} must be a numeric or character vector.")
  }
  if (!(is.numeric(end) || is.character(end))) {
    cli::cli_abort("{.arg end} must be a numeric or character vector.")
  }
  if (length(start) != length(end)) {
    cli::cli_abort("{.arg start} and {.arg end} must have the same length.")
  }
  if (!is.null(outfiles) && length(outfiles) != length(start)) {
    cli::cli_abort("{.arg outfiles} must have the same length as {.arg start}.")
  }
  rlang::check_bool(reencode)

  # If no names are provided, derive per-segment names from the input file.
  if (is.null(outfiles)) {
    outfiles <- derive_segment_names(rep(infile, length(start)))
  }

  # Fan-out (one input -> many outputs) is a Layer 2 concern: build one
  # single-output seek pipeline per segment and run them through ffm_batch
  # (D-M03-2). The engine stays single-output (D003).
  jobs <- tibble::tibble(
    input = infile, output = outfiles, start = start, end = end
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
# (segment_video_batch with no `output` column) share one naming rule.
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

# Shared cut logic for segment_video() and segment_video_batch(): build one
# single-output seek pipeline for a single segment, stream-copying on the fast
# (non-reencode) path. Fan-out verbs stay single-output per job (D003, D007);
# both verbs wrap this in a closure that captures the scalar `reencode`.
segment_pipeline <- function(input, output, start, end, reencode) {
  p <- ffm_seek(ffm_files(input, output), start = start, end = end,
                reencode = reencode)
  if (!reencode) p <- ffm_copy(p)
  p
}


# segment_video_batch() --------------------------------------------------------

#' Segment Many Videos From a Jobs Table
#'
#' Cut segments across many input files from a single jobs tibble — the
#' **batch** (table-driven) sibling of [segment_video()] for when your segments
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
#' @seealso [segment_video()] for the single-input, parallel-vector form;
#'   [ffm_batch()] for the batch runner and the arguments forwarded through
#'   \code{...}; [ffm_seek()] for the cut trade-off.
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
#' segment_video_batch(jobs, run = FALSE)
#' @export
segment_video_batch <- function(jobs, reencode = TRUE, run = TRUE,
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


# extract_frame_batch() --------------------------------------------------------

#' Extract Still Frames From Many Videos From a Jobs Table
#'
#' Grab one still image per row across many input files from a single jobs
#' tibble — the **batch** (table-driven) sibling of [extract_frame()] for when
#' your
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
#' @seealso [extract_frame()] for the single-frame form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [segment_video_batch()] for the segment-cutting sibling.
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
#' extract_frame_batch(jobs, run = FALSE)
#' @export
extract_frame_batch <- function(jobs, format = "png", run = TRUE,
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
        dots$frame / get_frame_rate(input)
      }
      frame_pipeline(input, output, timestamp)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_frames_dir() -----------------------------------------------------

# Derive one output directory per input for sample_frames_batch() when neither
# an `outdir` column nor a scalar `outdir` is given: `<input-base>_frames` beside
# each input. Per-input directories keep each recording's sequence separate, so
# the batch never collides even when many inputs sample at once.
derive_frames_dir <- function(input) {
  file.path(
    dirname(input),
    paste0(tools::file_path_sans_ext(basename(input)), "_frames")
  )
}


# sample_frames_batch() ---------------------------------------------------

#' Sample frames from many videos at a fixed rate from a jobs table
#'
#' Sample many videos into numbered image sequences from a single jobs tibble —
#' the **batch** (table-driven) sibling of [sample_frames()]. Each row is one
#' input video sampled at a fixed rate into its own image sequence. This is a
#' thin wrapper over \code{\link{ffm_batch}}: one reproducible compiled command
#' per input.
#'
#' Supply the sampling rate once as the scalar \code{fps} or \code{interval}
#' argument (applied to every row), or per row as an \code{fps} or
#' \code{interval} column that overrides the scalar of the same name. Exactly one
#' of the two — fps \emph{or} interval — may be supplied across arguments and
#' columns.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). Optional columns: \code{outdir} (the
#'   output directory for that row's sequence; when absent, one is derived as
#'   \code{<input-base>_frames} beside each input), and \code{fps} /
#'   \code{interval} (per-row rate overrides). Any other columns are ignored.
#' @param fps,interval The sampling rate applied to every row, as in
#'   [sample_frames()]; a per-row column of the same name overrides it. Supply
#'   exactly one of the two (as an argument or a column). (default = \code{NULL})
#' @param outdir An optional single output directory for all rows (overridden by
#'   an \code{outdir} column); when both are absent, per-input directories are
#'   derived. (default = \code{NULL})
#' @param format A string giving the output image file extension, as in
#'   [sample_frames()]. (default = \code{"png"})
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: sample in
#'   parallel with \pkg{furrr} (\code{TRUE}) or sequentially (\code{FALSE},
#'   default). Parallelism follows the active \code{\link[future:plan]{future}}
#'   plan; \code{TRUE} under the default sequential plan runs one at a time and
#'   warns.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}}, such as
#'   \code{verify}, \code{manifest}, \code{checksums}, and \code{progress}.
#' @return The [tibble][tibble::tibble-package] returned by
#'   \code{\link{ffm_batch}}: \code{jobs} with an added \code{command} column
#'   (and the resolved \code{outdir} column when it was derived; when
#'   \code{run = TRUE}, a \code{success} column, plus any columns the forwarded
#'   arguments add, e.g. \code{verified}).
#' @seealso [sample_frames()] for the single-video form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [extract_frame_batch()] for the enumerated-frame sibling.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   outdir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b"))
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' sample_frames_batch(jobs, fps = 2, run = FALSE)
#' @export
sample_frames_batch <- function(jobs, fps = NULL, interval = NULL,
                                outdir = NULL, format = "png", run = TRUE,
                                parallel = FALSE, ...) {

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
  format <- check_image_format(format)

  # Table-level rate exclusivity: exactly one of an fps source or an interval
  # source (argument or column), mirroring sample_frames()' scalar XOR. The
  # per-row value checks are inherited from resolve_sample_fps() in the closure.
  fps_src <- !is.null(fps) || "fps" %in% names(jobs)
  interval_src <- !is.null(interval) || "interval" %in% names(jobs)
  if (fps_src == interval_src) {
    cli::cli_abort(c(
      "Provide exactly one of {.arg fps} or {.arg interval} (argument or column).",
      "x" = if (fps_src) "Both are present." else "Neither is present."
    ))
  }

  # Validate present override columns up front so a bad column fails clearly here
  # rather than as an opaque FFmpeg error mid-batch (M11 parity lesson). An `fps`
  # column may be character (an FFmpeg rate expression) but `interval` may not —
  # resolve_sample_fps() rejects a character interval, so type it numeric-only
  # here (parity with extract_frame_batch()'s per-column typing).
  if ("fps" %in% names(jobs) &&
      !(is.numeric(jobs$fps) || is.character(jobs$fps))) {
    cli::cli_abort("The {.field fps} column of {.arg jobs} must be numeric or character.")
  }
  if ("interval" %in% names(jobs) && !is.numeric(jobs$interval)) {
    cli::cli_abort("The {.field interval} column of {.arg jobs} must be numeric.")
  }
  for (col in intersect(c("fps", "interval"), names(jobs))) {
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }

  # A factor input column carries paths as levels; treat them as strings
  # (parity with extract_frame_batch()).
  jobs$input <- as.character(jobs$input)

  # Resolve the per-row output directory: an explicit `outdir` column wins; else
  # the scalar `outdir` (one directory for every row); else one derived per
  # input. Carried on the returned tibble.
  if ("outdir" %in% names(jobs)) {
    if (!is.character(jobs$outdir) || anyNA(jobs$outdir)) {
      cli::cli_abort("The {.field outdir} column of {.arg jobs} must be character (no {.val {NA}}).")
    }
  } else if (!is.null(outdir)) {
    rlang::check_string(outdir)
    jobs$outdir <- outdir
  } else {
    jobs$outdir <- derive_frames_dir(jobs$input)
  }

  # Reject colliding output patterns before running: each row's pattern is
  # `<outdir>/<input-base>_%0Nd.<fmt>`, so two rows sharing a directory whose
  # inputs also share a basename (e.g. a duplicated input, or `cam1/rec.mp4` +
  # `cam2/rec.mp4` under one `outdir`) would silently overwrite each other's
  # frames. Fail clearly here rather than lose data mid-batch (the sibling
  # dup-input guard, adapted to the pattern level).
  patterns <- derive_frame_pattern(jobs$input, jobs$outdir, NULL, format)
  collisions <- unique(patterns[duplicated(patterns)])
  if (length(collisions) > 0) {
    cli::cli_abort(c(
      "Two or more jobs would write to the same image sequence.",
      "x" = "Colliding output pattern{?s}: {.file {collisions}}.",
      "i" = "Give colliding inputs distinct {.field outdir}s or rename them."
    ))
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one image2-pattern sampling
  # pipeline per input, sharing sample_frames_pipeline() with sample_frames().
  # A per-row rate column (arriving via `...` from pmap) overrides the scalar
  # arg of the same name; `...` also forwards ffm_batch options
  # (verify/manifest/...) to the runner, never to the pipeline builder.
  ffm_batch(
    jobs,
    function(input, outdir, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      rate <- resolve_sample_fps(pick("fps", fps), pick("interval", interval))
      dir_i <- ensure_dir(outdir)
      pattern <- derive_frame_pattern(input, dir_i, NULL, format)
      sample_frames_pipeline(input, pattern, rate)
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_standardized_names() ---------------------------------------------

# Derive one output path per input for standardize_video_batch() when the `output`
# column is absent: `<base>_standardized.<input-ext>` (standardization keeps the
# source container, unlike a frame which becomes an image). Standardization is
# one-input -> one-output, so -- unlike the per-input-numbering siblings -- a
# duplicated input with no explicit `output` would collide; the caller
# (standardize_video_batch) rejects that up front, so this helper assumes unique
# inputs and stays a pure name map.
derive_standardized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_standardized.", tools::file_ext(input)
  )
}


# standardize_video_batch() ----------------------------------------------------

#' Standardize Many Videos From a Jobs Table
#'
#' Re-encode many input files to a reproducible format from a single jobs tibble
#' — the **batch** (table-driven) sibling of [standardize_video()] for when you
#' have
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
#'   \code{fps}, \code{video_codec}, \code{pixel_format} — may also appear as a
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
#' @param video_codec A string naming the video codec applied to every row, unless
#'   \code{jobs} carries a \code{video_codec} column. (default = \code{"libx264"})
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
#' @seealso [standardize_video()] for the single-input form; [ffm_batch()] for
#'   the batch runner and the arguments forwarded through \code{...};
#'   [segment_video_batch()] and [extract_frame_batch()] for the other
#'   table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   output = c("a.mp4", "b.mp4"),
#'   width  = c(640, 320)
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' standardize_video_batch(jobs, run = FALSE)
#' @export
standardize_video_batch <- function(jobs, width = NULL, height = NULL, fps = NULL,
                               video_codec = "libx264", pixel_format = "yuv420p",
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
  # (parity with extract_frame_batch()).
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
  str_cols <- c("video_codec", "pixel_format")
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
        video_codec = pick("video_codec", video_codec),
        pixel_format = pick("pixel_format", pixel_format)
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_stripped_names() -------------------------------------------------

# Derive one output path per input for strip_metadata_batch() when the `output`
# column is absent: `<base>_stripped.<input-ext>` (a metadata scrub keeps the
# source container). The base keeps the input's directory, so inputs in
# different folders never collide; strip_metadata_batch() rejects any duplicated
# *resolved* output up front (M26), so this helper stays a pure name map.
derive_stripped_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_stripped.", tools::file_ext(input)
  )
}


# strip_metadata_batch() --------------------------------------------------------

#' Strip Metadata From Many Files From a Jobs Table
#'
#' De-identify many input files from a single jobs tibble — the **batch**
#' (table-driven) sibling of [strip_metadata()] for when you have more than one
#' file to scrub. Each row is one input; the only required column names its
#' source. This is a thin wrapper over \code{\link{ffm_batch}}: one reproducible
#' stream-copy strip command per input, sharing the same pipeline (and its
#' bit-exact, metadata-dropping behavior) as the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_stripped} to each input's basename, keeping the input's extension
#'   (e.g. \code{clip.mkv} becomes \code{clip_stripped.mkv}). Any two rows that
#'   resolve to the **same** output path — a duplicated \code{input} with no
#'   \code{output} column, or a repeated explicit \code{output} — are rejected
#'   so one file cannot silently overwrite another. Any other columns are
#'   ignored (the scrub has no per-row knobs).
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical passed to \code{\link{ffm_batch}}: scrub in
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
#' @seealso [strip_metadata()] for the single-input form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [standardize_video_batch()] and [anonymize_video_batch()] for the other
#'   table-driven siblings.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = video, output = "clean.mp4")
#' # run = FALSE compiles one command per input without calling FFmpeg
#' strip_metadata_batch(jobs, run = FALSE)
#' @export
strip_metadata_batch <- function(jobs, run = TRUE, parallel = FALSE, ...) {

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
  # (parity with the other *_batch verbs).
  jobs$input <- as.character(jobs$input)
  if (anyNA(jobs$input)) {
    cli::cli_abort("The {.field input} column of {.arg jobs} must not contain {.val {NA}}.")
  }

  # Resolve outputs (derive when absent), then reject any collision on the
  # *resolved* path — not just duplicated inputs — so an explicit `output`
  # column repeated across rows can't silently overwrite either (M26).
  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_stripped_names(jobs$input)
  } else {
    jobs$output <- as.character(jobs$output)
    if (anyNA(jobs$output)) {
      cli::cli_abort("The {.field output} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }
  dupes <- unique(jobs$output[duplicated(jobs$output)])
  if (length(dupes) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} has rows that resolve to the same output path.",
      "x" = "Colliding output{?s}: {.val {dupes}}.",
      "i" = "Give each row a distinct {.field output}, or de-duplicate the inputs."
    ))
  }

  # Thin Layer-2 fan-out over ffm_batch (D007): one single-output strip pipeline
  # per row, sharing strip_metadata_pipeline() with strip_metadata(). `...`
  # forwards ffm_batch options (verify/manifest/...) to the runner; the scrub
  # itself has no per-row knobs, so extra job columns are ignored.
  ffm_batch(
    jobs,
    function(input, output, ...) strip_metadata_pipeline(input, output),
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_normalized_names() -----------------------------------------------

# Derive one output path per input for normalize_audio_batch() when the `output`
# column is absent: `<base>_normalized.<input-ext>` (loudness normalization
# keeps the source container). One input -> one output, so a duplicated input
# with no explicit `output` would collide; the caller (normalize_audio_batch)
# rejects that up front, so this helper assumes unique inputs and stays a pure
# name map (parity with derive_standardized_names()).
derive_normalized_names <- function(input) {
  paste0(
    tools::file_path_sans_ext(input), "_normalized.", tools::file_ext(input)
  )
}


# normalize_audio_batch() ------------------------------------------------------

#' Normalize Many Files' Audio Loudness From a Jobs Table
#'
#' Loudness-normalize the audio of many input files (EBU R128) from a single
#' jobs tibble — the **batch** (table-driven) sibling of [normalize_audio()] for
#' when you have more than one file to normalize. Each row is one input; the
#' only required column names its source. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same \code{loudnorm} pipeline (and per-value validation) as the scalar
#' verb. Set \code{two_pass = TRUE} for accurate measured/linear normalization
#' across the whole table (see \code{two_pass}).
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
#' @param two_pass A logical selecting the batch normalization mode for
#'   \emph{every} row (\code{two_pass} is a whole-table switch, not a per-row
#'   column). \code{FALSE} (default) keeps the single-pass \code{loudnorm}
#'   pipeline. \code{TRUE} runs the accurate two-pass (measured/linear) path as a
#'   two-phase fan-out: an \emph{analysis pass} first measures every input's
#'   loudness (honoring \code{parallel} and each row's targets), and a
#'   \emph{correction pass} then feeds those measurements back with
#'   \code{linear=true} so each output hits its EBU R128 target precisely — the
#'   table-wide sibling of \code{\link{normalize_audio}}'s \code{two_pass}. The
#'   five measured values are surfaced on the result as columns \code{measured_I},
#'   \code{measured_TP}, \code{measured_LRA}, \code{measured_thresh}, and
#'   \code{offset}. Because it must measure each input, two-pass
#'   \strong{always runs the analysis pass through FFmpeg} (it needs the binary
#'   and readable inputs), even when \code{run = FALSE}. If any row's analysis
#'   fails or yields no parseable measurement, the call aborts — naming the
#'   offending row(s) — before any correction command is built. \strong{Silent}
#'   rows are the exception: a silent input (analysis loudness \code{-inf})
#'   cannot be normalized to a target, but one silent row does not abort the
#'   batch — the non-silent rows are normalized, the silent rows are marked in a
#'   logical \code{silent} column (with \code{success = FALSE} and no output
#'   written), and a warning names them. The single-pass default touches no
#'   binary under \code{run = FALSE}.
#' @param run A logical: run each input's command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}). Under
#'   \code{two_pass = TRUE} this gates only the correction pass; the analysis
#'   pass runs regardless (see \code{two_pass}).
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
#'   forwarded arguments add, e.g. \code{verified}). Under \code{two_pass = TRUE}
#'   the result also carries the five measured columns (\code{measured_I} etc.)
#'   and a logical \code{silent} column, and the \code{command} column holds the
#'   linear correction commands (\code{NA} for silent rows, which carry \code{NA}
#'   measurements and are not normalized). The two-pass result's schema is
#'   independent of how many rows are silent: the opt-in \code{verified} column
#'   (under \code{verify}) and provenance manifest (under \code{manifest}, read
#'   with \code{\link{ffm_manifest}}) are present whenever requested, even when
#'   \emph{every} row is silent -- silent rows simply carry \code{NA} for those
#'   outputs.
#' @references
#' EBU Recommendation R 128 (2014), \emph{Loudness normalisation and permitted
#' maximum level of audio signals}; ITU-R BS.1770-4.
#' @seealso [normalize_audio()] for the single-input form; [ffm_batch()] for the
#'   batch runner and the arguments forwarded through \code{...};
#'   [standardize_video_batch()] for the video-side table-driven sibling.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input           = c(video, video),
#'   output          = c("a.mp4", "b.mp4"),
#'   target_loudness = c(-23, -16)
#' )
#' # run = FALSE compiles one command per input without calling FFmpeg
#' normalize_audio_batch(jobs, run = FALSE)
#' # Accurate two-pass (measured/linear) normalization across the whole table
#' # (runs FFmpeg to measure each input, so needs the binary):
#' \dontrun{
#' normalize_audio_batch(jobs, two_pass = TRUE)
#' }
#' @export
normalize_audio_batch <- function(jobs, target_loudness = -23, true_peak = -1,
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
  # (parity with standardize_video_batch()).
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
  # that rather than silently overwrite (parity with standardize_video_batch()).
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
    # Validate the shaping knobs up front (parity with the scalar two-pass verb)
    # so a bad channels/sample_rate fails before Phase 1 wastes an analysis pass
    # per row; per-value target checks stay per-row in the Phase 2 pipeline.
    rlang::check_number_whole(channels, min = 1, allow_null = TRUE)
    rlang::check_number_whole(sample_rate, min = 1, allow_null = TRUE)
    for (col in intersect(c("channels", "sample_rate"), names(jobs))) {
      if (any(jobs[[col]] %% 1 != 0) || any(jobs[[col]] < 1)) {
        cli::cli_abort(
          "The {.field {col}} column of {.arg jobs} must be whole numbers \\
           ({.val {1}} or greater) for two-pass normalization."
        )
      }
    }
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
    # Continue-and-mark on silence (M18): a silent input (input_i = -inf) cannot
    # be normalized to a loudness target, but one silent row must not abort the
    # whole batch. assemble_measured() sets silent rows aside (measured cols NA)
    # and flags them; genuine failures still abort. Correct only the non-silent
    # rows, warn about the silent ones, and reassemble in original row order.
    measured <- assemble_measured(outputs)
    silent <- measured$silent
    for (nm in names(measured$measured)) jobs[[nm]] <- measured$measured[[nm]]
    if (any(silent)) {
      # Drive pluralization off the scalar {length(rows)} and list the row
      # indices without a `{?s}` marker: a `{?s}` governed by a `{.val
      # {vector}}` across cli_warn() message elements throws
      # `length(object) == 1` (M18 review).
      rows <- which(silent)
      cli::cli_warn(c(
        "Found {length(rows)} silent input{?s} that cannot be normalized to a \\
         loudness target.",
        "!" = "Affected rows (1-indexed): {.val {rows}}.",
        "i" = "Silent rows are marked in the {.field silent} column \\
               ({.field success} = {.val {FALSE}}, no output written)."
      ))
    }
    ok_res <- if (any(!silent)) {
      run_normalize_correction(
        jobs[!silent, , drop = FALSE], target_loudness, true_peak,
        loudness_range, channels, sample_rate, run = run, parallel = parallel,
        ...
      )
    } else {
      NULL
    }
    # Thread the opt-in intent (verify/manifest/checksums, forwarded via `...`)
    # so an all-silent batch synthesizes the same schema a mixed one produces
    # (D011); a mixed batch reads those from ok_res and ignores these.
    dots <- list(...)
    return(bind_two_pass_result(
      jobs, silent, ok_res, run,
      verify = !is.null(dots$verify),
      manifest = isTRUE(dots$manifest),
      checksums = isTRUE(dots$checksums)
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


# Batch jobs-table guards (shared by the M28 single-in/out batch verbs) --------

# Validate the common jobs-table contract and return `jobs` with `input` (and,
# when present, `output`) coerced to character. `require_output = TRUE` demands
# an explicit `output` column (audio verbs, whose extension is the instruction
# and cannot be auto-named); `verb` names the operation in that error. A factor
# path column carries paths as levels, so coerce to character (parity with the
# other *_batch verbs). Value-level per-row checks stay in the shared pipelines.
check_batch_jobs <- function(jobs, require_output = FALSE, verb = NULL,
                             call = rlang::caller_env()) {
  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per input.", call = call)
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.", call = call)
  }
  if (!"input" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field input} column.",
      "x" = "Missing column: {.val input}."
    ), call = call)
  }
  jobs$input <- as.character(jobs$input)
  if (anyNA(jobs$input)) {
    cli::cli_abort("The {.field input} column of {.arg jobs} must not contain {.val {NA}}.", call = call)
  }

  if (require_output && !"output" %in% names(jobs)) {
    cli::cli_abort(c(
      "{.arg jobs} must have an {.field output} column.",
      "x" = "Missing column: {.val output}.",
      "i" = "{verb} can't derive an output container; name each destination."
    ), call = call)
  }
  if ("output" %in% names(jobs)) {
    jobs$output <- as.character(jobs$output)
    if (anyNA(jobs$output)) {
      cli::cli_abort("The {.field output} column of {.arg jobs} must not contain {.val {NA}}.", call = call)
    }
  }
  jobs
}

# Reject any two rows that resolve to the same output path — not just duplicated
# inputs — so an explicit `output` repeated across rows can't silently overwrite
# either (M26). Assumes `jobs$output` is resolved (present or derived).
reject_duplicate_outputs <- function(jobs, call = rlang::caller_env()) {
  dupes <- unique(jobs$output[duplicated(jobs$output)])
  if (length(dupes) > 0) {
    cli::cli_abort(c(
      "{.arg jobs} has rows that resolve to the same output path.",
      "x" = "Colliding output{?s}: {.val {dupes}}.",
      "i" = "Give each row a distinct {.field output}, or de-duplicate the inputs."
    ), call = call)
  }
  jobs
}

# Guard an optional string override column: present -> character, no NA.
check_batch_string_col <- function(jobs, col, call = rlang::caller_env()) {
  if (col %in% names(jobs)) {
    if (!is.character(jobs[[col]]) || anyNA(jobs[[col]])) {
      cli::cli_abort(
        "The {.field {col}} column of {.arg jobs} must be character (no {.val {NA}}).",
        call = call
      )
    }
  }
  invisible(jobs)
}


# extract_audio_batch() ---------------------------------------------------

#' Extract Audio From Many Files From a Jobs Table
#'
#' Pull the audio track out of many input files from a single jobs tibble — the
#' **batch** (table-driven) sibling of [extract_audio()] for when you have more
#' than one file. Each row is one input; \code{input} and \code{output} columns
#' are required. This is a thin wrapper over \code{\link{ffm_batch}}: one
#' reproducible compiled command per input, sharing the same map/drop-video
#' pipeline as the scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) and an \code{output} column (destination
#'   path). An \code{output} column is **required** — unlike the video batch
#'   verbs, an audio destination cannot be auto-named because its extension is
#'   the instruction (it picks the container, and with \code{audio_codec =
#'   "copy"} must match the source codec). An optional \code{audio_codec} column
#'   overrides the \code{audio_codec} argument per row; rows omitting it fall
#'   back to the argument. Any other columns are ignored.
#' @param audio_codec The audio codec applied to every row unless \code{jobs}
#'   carries an \code{audio_codec} column. \code{"copy"} (default) stream-copies
#'   the audio losslessly; name an encoder (e.g. \code{"aac"}) to transcode.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [extract_audio()], the scalar verb it wraps; [ffm_batch()], the batch
#'   runner; [convert_audio_batch()] to transcode audio in batch.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.aac", "b.aac"))
#' extract_audio_batch(jobs, run = FALSE)
#' @export
extract_audio_batch <- function(jobs, audio_codec = "copy", run = TRUE,
                                parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, require_output = TRUE, verb = "Audio extraction")
  jobs <- reject_duplicate_outputs(jobs)
  check_batch_string_col(jobs, "audio_codec")

  # Thin Layer-2 fan-out over ffm_batch (D007): one map/drop-video pipeline per
  # row, sharing extract_audio_pipeline() with extract_audio(). A per-row
  # `audio_codec` column (via `...` from pmap) overrides the scalar arg; `...`
  # also forwards ffm_batch options to the runner.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      extract_audio_pipeline(input, output, audio_codec = pick("audio_codec", audio_codec))
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# convert_audio_batch() ---------------------------------------------------

#' Convert the Audio of Many Files From a Jobs Table
#'
#' Extract or transcode the audio track of many input files from a single jobs
#' tibble — the **batch** (table-driven) sibling of [convert_audio()] for when
#' you have more than one file. Each row is one input; \code{input} and
#' \code{output} columns are required. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same audio-map pipeline (and per-value \code{format} validation) as the
#' scalar verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path) and an \code{output} column (destination
#'   path). An \code{output} column is **required** — an audio destination
#'   cannot be auto-named because its extension picks the output format. An
#'   optional \code{format} column overrides the \code{format} argument per row;
#'   rows omitting it fall back to the argument. Any other columns are ignored.
#' @param format The output audio codec applied to every row unless \code{jobs}
#'   carries a \code{format} column. \code{NULL} (default) infers the format
#'   from each \code{output} extension at highest VBR quality; name a codec
#'   (e.g. \code{"aac"}, \code{"flac"}) to pin \code{-c:a}.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [convert_audio()], the scalar verb it wraps; [ffm_batch()], the batch
#'   runner; [extract_audio_batch()] to stream-copy audio in batch.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.mp3", "b.mp3"))
#' convert_audio_batch(jobs, run = FALSE)
#' @export
convert_audio_batch <- function(jobs, format = NULL, run = TRUE,
                                parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, require_output = TRUE, verb = "Audio conversion")
  jobs <- reject_duplicate_outputs(jobs)
  check_batch_string_col(jobs, "format")

  # Thin Layer-2 fan-out over ffm_batch (D007): one audio-map pipeline per row,
  # sharing convert_audio_pipeline() with convert_audio(). A per-row `format`
  # column overrides the scalar arg; the per-value check_string(format) is
  # inherited from the shared pipeline.
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      convert_audio_pipeline(input, output, format = pick("format", format))
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# derive_cropped_names() / derive_web_names() -----------------------------

# Derive one output path per input for the video batch verbs when the `output`
# column is absent. crop keeps the source container (`<base>_cropped.<ext>`);
# the web re-encode always writes H.264/mp4 (`<base>_web.mp4`). The base keeps
# the input's directory, so inputs in different folders never collide; the batch
# verbs reject any duplicated *resolved* output up front (M26), so these stay
# pure name maps.
derive_cropped_names <- function(input) {
  paste0(tools::file_path_sans_ext(input), "_cropped.", tools::file_ext(input))
}

derive_web_names <- function(input) {
  paste0(tools::file_path_sans_ext(input), "_web.mp4")
}


# crop_video_batch() ------------------------------------------------------

#' Crop Many Videos From a Jobs Table
#'
#' Crop many input videos to a rectangular region from a single jobs tibble —
#' the **batch** (table-driven) sibling of [crop_video()] for when you have more
#' than one file. Each row is one input. This is a thin wrapper over
#' \code{\link{ffm_batch}}: one reproducible compiled command per input, sharing
#' the same crop pipeline (and its per-value dimension guards) as the scalar
#' verb.
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_cropped} to each input's basename, keeping the input's extension
#'   (e.g. \code{clip.mp4} becomes \code{clip_cropped.mp4}). Each crop dimension
#'   — \code{width}, \code{height}, \code{x}, \code{y} — may also appear as a
#'   column to override the corresponding argument per row; rows (or dimensions)
#'   omitting the column fall back to the argument. Any two rows that resolve to
#'   the same output path are rejected. Any other columns are ignored.
#' @param width,height The output crop size in pixels, applied to every row
#'   unless \code{jobs} carries a column of the same name. Required: pass each as
#'   an argument or supply the column (there is no default crop size).
#' @param x,y The offset in pixels of the crop's left/top edge, applied to every
#'   row unless \code{jobs} carries a column of the same name. Default: centered.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [crop_video()], the scalar verb it wraps; [ffm_batch()], the batch
#'   runner; [standardize_video_batch()] to re-encode in batch.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), width = c(160, 80),
#'                        height = c(120, 60))
#' crop_video_batch(jobs, run = FALSE)
#' @export
crop_video_batch <- function(jobs, width = NULL, height = NULL,
                             x = "(in_w-out_w)/2", y = "(in_h-out_h)/2",
                             run = TRUE, parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, require_output = FALSE)

  # width/height have no default: each must be resolvable as an argument (applied
  # to every row) or a per-row column, else fail here rather than as an opaque
  # FFmpeg error mid-batch.
  for (dim in c("width", "height")) {
    if (is.null(get(dim)) && !dim %in% names(jobs)) {
      cli::cli_abort(c(
        "{.arg {dim}} is required.",
        "i" = "Pass {.arg {dim}} (applied to every row) or add a {.field {dim}} column."
      ))
    }
  }
  # Validate present override columns up front; per-value checks (positive
  # dimensions / valid expressions) are inherited per row from ffm_crop().
  for (col in intersect(c("width", "height", "x", "y"), names(jobs))) {
    if (!(is.numeric(jobs[[col]]) || is.character(jobs[[col]]))) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must be numeric or character.")
    }
    if (anyNA(jobs[[col]])) {
      cli::cli_abort("The {.field {col}} column of {.arg jobs} must not contain {.val {NA}}.")
    }
  }

  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_cropped_names(jobs$input)
  }
  jobs <- reject_duplicate_outputs(jobs)

  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      crop_video_pipeline(
        input, output,
        width = pick("width", width),
        height = pick("height", height),
        x = pick("x", x),
        y = pick("y", y)
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}


# format_for_web_batch() --------------------------------------------------

#' Re-encode Many Videos for the Web From a Jobs Table
#'
#' Re-encode many input videos into a widely compatible, web-friendly form from
#' a single jobs tibble — the **batch** (table-driven) sibling of
#' [format_for_web()] for when you have more than one file. Each row is one
#' input. This is a thin wrapper over \code{\link{ffm_batch}}: one reproducible
#' compiled command per input, sharing the same fixed H.264/AAC/\code{+faststart}
#' pipeline as the scalar verb (no per-row knobs).
#'
#' @param jobs A data frame with one row per input and (at least) an
#'   \code{input} column (source path). An optional \code{output} column names
#'   the destination; when absent, one is derived per row by appending
#'   \code{_web} to each input's basename with an \code{.mp4} extension (the web
#'   re-encode always writes H.264/mp4), e.g. \code{clip.mkv} becomes
#'   \code{clip_web.mp4}. Any two rows that resolve to the same output path are
#'   rejected. Any other columns are ignored.
#' @param run A logical: run each command through FFmpeg (\code{TRUE}, default)
#'   or only compile them for inspection (\code{FALSE}).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). See
#'   \code{\link{ffm_batch}} for the \pkg{future} plan requirement.
#' @param ... Additional arguments forwarded to \code{\link{ffm_batch}} (e.g.
#'   \code{verify}, \code{manifest}, \code{progress}).
#' @return The \code{jobs} tibble with an added \code{command} column and, when
#'   \code{run = TRUE}, a \code{success} column (plus \code{verified} /
#'   provenance manifest when requested via \code{...}). See
#'   \code{\link{ffm_batch}}.
#' @seealso [format_for_web()], the scalar verb it wraps; [ffm_batch()], the
#'   batch runner; [standardize_video_batch()] for a configurable re-encode.
#' @family task verb functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = c(video, video), output = c("a.mp4", "b.mp4"))
#' format_for_web_batch(jobs, run = FALSE)
#' @export
format_for_web_batch <- function(jobs, run = TRUE, parallel = FALSE, ...) {

  jobs <- check_batch_jobs(jobs, require_output = FALSE)

  if (!"output" %in% names(jobs)) {
    jobs$output <- derive_web_names(jobs$input)
  }
  jobs <- reject_duplicate_outputs(jobs)

  # Thin Layer-2 fan-out over ffm_batch (D007): one web re-encode pipeline per
  # row, sharing format_for_web_pipeline() with format_for_web(). No per-row
  # knobs, so extra job columns are ignored.
  ffm_batch(
    jobs,
    function(input, output, ...) format_for_web_pipeline(input, output),
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
#' @seealso [ffm_concat()], the builder it wraps.
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
#' @seealso [ffm_hstack()] and [ffm_vstack()], the builders it wraps;
#'   [picture_in_picture()] for insetting instead of stacking.
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
#' @seealso [ffm_overlay()], the builder it wraps; [compare_videos()] for
#'   side-by-side stacking.
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
