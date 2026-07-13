# new_ffm() --------------------------------------------------------------------

# S3 constructor for the ffm class.
#
# The object stores *unformatted values*, never pre-rendered flags or spaces:
# `ffm_compile()` is the single place that turns these into a command string.
# This keeps option positioning, quoting, and the simple-vs-complex filter
# decision in one place (M02 / DECISIONS D002).
#
# Fields:
#   input        character() of input file paths (>1 for stacking)
#   output       length-1 output file path
#   overwrite    length-1 logical: TRUE -> -y, FALSE -> -n (global option)
#   drop         character() subset of video/audio/subtitles/data to drop
#   codec_video  length-0/1 codec name (e.g. "libx264", "copy")
#   codec_audio  length-0/1 codec name
#   pixel_format length-0/1 pixel format name
#   filter_video character() of video filter tokens, in application order
#   filter_audio character() of audio filter tokens, in application order
#   map          character() of explicit -map targets
#   complex      length-1 logical: TRUE once a multi-input verb (e.g. hstack)
#                has been applied, forcing the -filter_complex + labels path
#   output_opts  character() of raw output-option tokens (e.g. "-q:v 1"),
#                positioned by ffm_compile() among the other output options
#   seek_start   length-0/1 start position for a seek-based cut (-ss)
#   seek_end     length-0/1 end position for a seek-based cut (-to)
#   seek_reencode length-0/1 logical: TRUE -> accurate (output seek, re-encode);
#                FALSE -> fast copy-safe (input seek + -avoid_negative_ts)
#   concat       length-1 logical: TRUE once ffm_concat() has been applied,
#                switching the input section to the concat demuxer
#   concat_list  length-0/1 path to the concat-demuxer list file (written by
#                ffm_concat() at verb time, referenced by ffm_compile())
new_ffm <- function(input = character(),
                    output = character(),
                    overwrite = logical(),
                    drop = character(),
                    codec_video = character(),
                    codec_audio = character(),
                    pixel_format = character(),
                    filter_video = character(),
                    filter_audio = character(),
                    map = character(),
                    complex = FALSE,
                    output_opts = character(),
                    seek_start = character(),
                    seek_end = character(),
                    seek_reencode = logical(),
                    concat = FALSE,
                    concat_list = character()) {

  stopifnot(is.character(input))
  stopifnot(is.character(output))
  stopifnot(is.logical(overwrite))
  stopifnot(is.character(drop))
  stopifnot(is.character(codec_video))
  stopifnot(is.character(codec_audio))
  stopifnot(is.character(pixel_format))
  stopifnot(is.character(filter_video))
  stopifnot(is.character(filter_audio))
  stopifnot(is.character(map))
  stopifnot(is.logical(complex), length(complex) == 1)
  stopifnot(is.character(output_opts))
  stopifnot(is.character(seek_start), length(seek_start) <= 1)
  stopifnot(is.character(seek_end), length(seek_end) <= 1)
  stopifnot(is.logical(seek_reencode), length(seek_reencode) <= 1)
  stopifnot(is.logical(concat), length(concat) == 1)
  stopifnot(is.character(concat_list), length(concat_list) <= 1)

  structure(
    list(
      input = input,
      output = output,
      overwrite = overwrite,
      drop = drop,
      codec_video = codec_video,
      codec_audio = codec_audio,
      pixel_format = pixel_format,
      filter_video = filter_video,
      filter_audio = filter_audio,
      map = map,
      complex = complex,
      output_opts = output_opts,
      seek_start = seek_start,
      seek_end = seek_end,
      seek_reencode = seek_reencode,
      concat = concat,
      concat_list = concat_list
    ),
    class = "tidymedia_ffm"
  )
}


# check_ffm() -------------------------------------------------------------

# Validate that `object` is a tidymedia ffm pipeline. Internal helper used by
# every ffm_* verb; aborts with a classed cli condition otherwise.
check_ffm <- function(object,
                      arg = rlang::caller_arg(object),
                      call = rlang::caller_env()) {
  if (!inherits(object, "tidymedia_ffm")) {
    cli::cli_abort(
      "{.arg {arg}} must be an ffm pipeline created by {.fn ffm_files}.",
      call = call
    )
  }
  invisible(object)
}


# check_multi_input_ordering() --------------------------------------------

# Shared guard for the blessed multi-input video verbs (hstack, vstack,
# overlay). Each consumes the raw input pads to make a single stacked/composited
# frame, so it must come *before* any single-input video filter; otherwise
# ffm_compile() would feed several pads to a one-input filter and emit an invalid
# graph. `verb` names the operation for the message (e.g. "Stacking").
check_multi_input_ordering <- function(object, verb,
                                       call = rlang::caller_env()) {
  if (length(object$filter_video) > 0) {
    cli::cli_abort(
      c(
        "{verb} must come before other video filters.",
        "i" = "Apply the multi-input verb first, then filter the combined result."
      ),
      call = call
    )
  }
  invisible(object)
}


# print.tidymedia_ffm() ---------------------------------------------------

#' Print an FFmpeg pipeline
#'
#' Print a tidymedia \code{ffm} pipeline by showing the FFmpeg command it
#' currently compiles to (via \code{\link{ffm_compile}}).
#'
#' @param x A tidymedia \code{ffm} pipeline object created by
#'   \code{\link{ffm_files}}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @seealso [ffm_compile()], which produces the printed command.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' ffm(video, "output.mp4") |>
#'   ffm_trim(start = 1, end = 5)
#' @method print tidymedia_ffm
#' @export
print.tidymedia_ffm <- function(x, ...) {
  cat('tidymedia ffmpeg pipeline:\n\n', ffm_compile(x), '\n')
  invisible(x)
}
