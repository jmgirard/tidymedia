
# ffmpeg() ----------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  assert_that(rlang::is_character(command, n = 1))
  out <- system(paste0('"', find_ffmpeg(), '" ', command), intern = TRUE)
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
format_for_web <- function(infile, outfile, preview = FALSE) {
  
  assert_that(rlang::is_character(infile, n = 1))
  assert_that(file.exists(infile))
  assert_that(rlang::is_character(outfile, n = 1))
  
  command <- glue(
    '-i "{infile}" -pix_fmt yuv420p -c:v libx264 -movflags +faststart ',
    '-filter:v crop="floor(in_w/2)*2:floor(in_h/2)*2" -c:a aac "{outfile}"'
  )
  
  if(preview == TRUE) {
    cat(command)
  } else {
    ffmpeg(command)
  }
  
}


# get_codecs() ------------------------------------------------------------

#' @export
get_codecs <- function(encoding = FALSE) {
  output <- ffmpeg("-codecs")
  output2 <- output[-(1:which(output == " -------"))]
  types <- regmatches(
    output2, 
    regexpr("(?<=\\s)\\S+(?=\\s)", output2, perl = TRUE)
  )
  
  decoding <- substr(types, 1, 1) == "D"
  encoding <- substr(types, 2, 2) == "E"
  video <- substr(types, 3, 3) == "V"
  audio <- substr(types, 3, 3) == "A"
  subtitle <- substr(types, 3, 3) == "S"
  intraframe <- substr(types, 4, 4) == "I"
  lossy <- substr(types, 5, 5) == "L"
  lossless <- substr(types, 6, 6) == "S"
  
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
      codec = abbrev,
      description,
      decoding,
      encoding,
      video,
      audio,
      subtitle,
      intraframe,
      lossy,
      lossless
    )
  
  # Sort by codec 
  out <- out[order(out$codec), ]
  
  out
}