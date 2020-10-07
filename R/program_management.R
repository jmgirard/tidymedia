
# find_program() ----------------------------------------------------------

find_program <- function(program = c("ffmpeg", "mediainfo")) {
  
  # Validate arguments
  program <- match.arg(program)
  
  # Look for program in path
  location <- Sys.which(program)
  
  if (location == "") {
    # Look for program location in user_config_dir
    config <- file.path(
      rappdirs::user_config_dir("tidymedia", "R"),
      glue("{program}_location.rds")
    )
    if (file.exists(config)) {
      location <- readRDS(config)
    } else {
      location <- NULL
      warning(glue("Failed to find {program}. Check that it is installed ",
                   "and, if necessary, use the set_{program}() function."))
    }
  }
  
  location
}

# find_mediainfo() --------------------------------------------------------

#' Find the location of MediaInfo
#'
#' Returns the location of the MediaInfo CLI program as a string. On Linux, this
#' will be "mediainfo" if installed properly. On Windows, this will be the full
#' path to the mediainfo.exe file.
#' 
#' @return A string indicating the location of the MediaInfo CLI program.
#' @export
find_mediainfo <- function() {
  find_program("mediainfo")
}


# find_ffmpeg() -----------------------------------------------------------

#' Find the location of FFmpeg
#'
#' Returns the location of the FFmpeg CLI program as a string. On Linux, this
#' will be "ffmpeg" if installed properly. On Windows, this will be the full
#' path to the ffmpeg.exe file.
#' 
#' @return A string indicating the location of the FFmpeg program.
#' @export
find_ffmpeg <- function() {
  find_program("ffmpeg")
}


# set_location() ----------------------------------------------------------

set_location <- function(program = c("ffmpeg", "mediainfo"), location) {
  
  # Validate arguments 
  program <- match.arg(program)
  assert_that(rlang::is_character(location, n = 1))
  assert_that(
    Sys.which(location) != "", 
    msg = "Failed to find location."
  )
  
  # Find where to save user configuration data
  config_dir <- rappdirs::user_config_dir("tidymedia", "R")
  config_file <- file.path(config_dir, glue("{program}_location.rds"))
  
  # Create configuration directory if needed
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)
  
  # Save location to user configuration file
  saveRDS(location, config_file)
}

# set_mediainfo() ---------------------------------------------------------

#' Set the MediaInfo location
#'
#' Save the location of the MediaInfo CLI to options, which will persist across
#' sessions. This is currently necessary on Windows (and Mac?) but not on Linux
#' platforms.
#'
#' @param location *Required.* A string indicating the location of the MediaInfo
#'   CLI program.
#' @export
set_mediainfo <- function(location) {
  set_location("mediainfo", location)
}


# set_ffmpeg() ------------------------------------------------------------

#' Set the FFmpeg location
#'
#' Save the location of the FFmpeg CLI to options, which will persist across
#' sessions. This is currently necessary on Windows (and Mac?) but not on Linux
#' platforms.
#'
#' @param location *Required.* A string indicating the location of the FFmpeg
#'   CLI program.
#' @export
set_ffmpeg <- function(location) {
  set_location("ffmpeg", location)
}
