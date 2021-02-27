# new_ffm() --------------------------------------------------------------------

# S3 Constructor for ffm class
new_ffm <- function(trim_start = character(),
                    trim_end = character(),
                    drop_streams = character(),
                    input = character(), 
                    overwrite = character(),
                    codec_video = character(),
                    codec_audio = character(),
                    pixel_format = character(),
                    filter_video = character(),
                    filter_audio = character(),
                    output = character()) {
  
  stopifnot(is.character(trim_start))
  stopifnot(is.character(trim_end))
  stopifnot(is.character(drop_streams))
  stopifnot(is.character(input))
  stopifnot(is.character(overwrite))
  stopifnot(is.character(codec_video))
  stopifnot(is.character(codec_audio))
  stopifnot(is.character(pixel_format))
  stopifnot(is.character(filter_video))
  stopifnot(is.character(filter_audio))
  stopifnot(is.character(output))
  
  structure(
    list(
      trim_start = trim_start,
      trim_end = trim_end,
      drop_streams = drop_streams,
      input = input,
      overwrite = overwrite,
      codec_video = codec_video,
      codec_audio = codec_audio,
      pixel_format = pixel_format,
      filter_video = filter_video,
      filter_audio = filter_audio,
      output = output
    ),
    class = "tidymedia_ffm"
  )
}


# print.tidymedia_ffm() ---------------------------------------------------

#' @method print tidymedia_ffm
#' @export
print.tidymedia_ffm <- function(x, ...) {
  cat('tidymedia ffmpeg pipeline:\n\n', ffm_compile(x), '\n')
}
