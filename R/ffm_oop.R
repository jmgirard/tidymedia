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
                    complex = FALSE) {

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
      complex = complex
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


# print.tidymedia_ffm() ---------------------------------------------------

#' @method print tidymedia_ffm
#' @export
print.tidymedia_ffm <- function(x, ...) {
  cat('tidymedia ffmpeg pipeline:\n\n', ffm_compile(x), '\n')
}
