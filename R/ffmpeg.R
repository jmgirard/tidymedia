
# find_ffmpeg() -----------------------------------------------------------

#' Find the location of ffmpeg
#'
#' Returns the location of the ffmpeg CLI program as a string. On Linux, this
#' will be "ffmpeg" if installed properly. On Windows, this will be the full
#' path to the ffmpeg.exe file.
#' 
#' @return A string indicating the location of the ffmpeg CLI program.
#' @export
find_ffmpeg <- function() {
  
  ffmpeg <- Sys.which("ffmpeg")
  
  if (ffmpeg == "") {
    ffmpeg <- 
      readRDS(
        system.file(
          "extdata/ffmpeg_location.rds", 
          package = "tidymedia"
        )
      )
  
    assert_that(
      rlang::is_null(ffmpeg) == FALSE, 
      msg = paste0(
        "ffmpeg CLI not found. On Linux, check that it is installed. ",
        "On Windows/Mac, check that it is installed and use set_ffmpeg()."
      )
    )
  }
  
  ffmpeg
}

# set_ffmpeg() ------------------------------------------------------------

#' Set the ffmpeg location
#'
#' Save the location of the ffmpeg CLI to options, which will persist across
#' sessions. This is currently necessary on Windows (and Mac?) but not on Linux
#' platforms.
#'
#' @param path *Required.* A string indicating the location of the ffmpeg CLI
#'   program.
#' @export
set_ffmpeg <- function(path) {
  
  assert_that(rlang::is_character(path, n = 1))
  assert_that(
    Sys.which(path) != "", 
    msg = "Could not find path, try again."
  )
  
  saveRDS(
    path,
    system.file(
      "extdata/ffmpeg_location.rds", 
      package = "tidymedia"
    )
  )
  
}

# tbd ---------------------------------------------------------------------
#' @export
do_ffmpeg <- function(input, output, pre, post, ffmpeg = find_ffmpeg()) {
  
  assert_that(rlang::is_character(input, n = 1))
  assert_that(rlang::is_character(output, n = 1))
  assert_that(rlang::is_character(pre, n = 1))
  assert_that(rlang::is_character(post, n = 1))
  assert_that(rlang::is_character(ffmpeg, n = 1))
  assert_that(Sys.which(ffmpeg) != "", msg = "Failed to load ffmpeg")
  assert_that(file.exists(input), msg = "Failed to find or access file")
  
  str_output <- 
    system(
      glue('"{ffmpeg}" {pre} -i "{input}" {post} "{output}"'),
      intern = TRUE
    )
  
}

# extract_frames() --------------------------------------------------------

extract_frames <- function(input, output, timestamp, ffmpeg = find_ffmpeg()) {
  pre <- glue('-ss {timestamp}')
  post <- glue('-qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal')
  do_ffmpeg(input, output, pre, post, ffmpeg)
}
