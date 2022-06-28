
# find_program() ----------------------------------------------------------

#' Find the location of a dependency program
#'
#' Returns the location of the requested program as a string.
#'
#' @param program A string indicating which program to find
#' @return Either a string indicating whether the requested program was found or
#'   `NULL` if the program could not be found.
find_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo")) {
  
  # Validate arguments
  program <- match.arg(program)
  
  # First, look for program in path
  location <- Sys.which(program)
  
  if (location == "") {
    # If program not found, look for a user config file
    config <- file.path(
      rappdirs::user_config_dir("tidymedia", "R"),
      glue("{program}_location.txt")
    )
    # If a user config file exists, read it in
    if (file.exists(config)) {
      location <- readLines(config)
      # Verify that the location in the user config file is valid
      if (Sys.which(location) == "") {
        warning(glue("{program} was set as being at {location} but ",
                     "this file does not seem to exist anymore."))
        location <- NULL
      }
    } else {
      # If config file not found, return NULL value and warning
      location <- NULL
      warning(glue("Failed to find {program}. Check that it is installed ",
                   "and, if necessary, use the set_{program}() function."))
    }
  }
  
  location
}

# find_mediainfo() --------------------------------------------------------

#' @rdname find_program
#' @export
find_mediainfo <- function() {
  find_program("mediainfo")
}

# find_ffmpeg() -----------------------------------------------------------

#' @rdname find_program
#' @export
find_ffmpeg <- function() {
  find_program("ffmpeg")
}

# find_ffprobe() -----------------------------------------------------------

#' @rdname find_program
#' @export
find_ffprobe <- function() {
  find_program("ffprobe")
}

# find_ffplay() -----------------------------------------------------------

#' @rdname find_program
#' @export
find_ffplay <- function() {
  find_program("ffplay")
}

# set_program() ------------------------------------------------------------

#' Set the location of a dependency program
#'
#' @param program A string indicating which program to set the location for.
#' @param location A string containing the location of the program.
#' @return A logical indicating whether the program location was set properly.
#'
#' @export
set_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"), 
                         location) {
  
  # Validate arguments 
  program <- match.arg(program)
  assert_that(rlang::is_character(location, n = 1))
  assert_that(
    # Check that the location is valid
    Sys.which(location) != "", 
    msg = "Failed to find location."
  )
  
  # Find where to save user configuration data
  config_dir <- rappdirs::user_config_dir("tidymedia", "R")
  config_file <- file.path(config_dir, glue("{program}_location.txt"))
  
  # Create configuration directory if needed
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)
  
  # Save location to user configuration file
  writeLines(location, config_file)
}

# set_mediainfo() ---------------------------------------------------------

#' @rdname set_program
#' @export
set_mediainfo <- function(location) {
  set_program("mediainfo", location)
}

# set_ffmpeg() ------------------------------------------------------------

#' @rdname set_program
#' @export
set_ffmpeg <- function(location) {
  set_program("ffmpeg", location)
}

#' @rdname set_program
#' @export
set_ffprobe <- function(location) {
  set_program("ffprobe", location)
}

#' @rdname set_program
#' @export
set_ffplay <- function(location) {
  set_program("ffplay", location)
}


# install_on_win() --------------------------------------------------------

#' Install FFmpeg on Windows
#'
#' Downloads an FFmpeg zip installer, extracts it, and updates the package's
#' user config files to point to the component executable files.
#'
#' @param download_url A string indicating the location of the FFmpeg
#'   installation zip file. If `NULL`, will default to the latest static
#'   essentials release from gyan.dev.
#' @param install_dir A string indicating a directory to install FFmpeg to. If
#'   `NULL`, will default to installing to the user data directory.
#' @return A logical indicating whether the installation was successful.
install_on_win <- function(download_url = NULL,
                           install_dir = NULL) {
  
  if (is.null(download_url)) {
    download_url <- "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z"
  }
  if (is.null(install_dir)) {
    install_dir <- file.path(rappdirs::user_data_dir("tidymedia", "R"), "ffmpeg")
  }
  if (!dir.exists(install_dir)) {
    status <- dir.create(install_dir, recursive = TRUE)
    if (status == FALSE) return(FALSE)
  }
  # Download the installer to a temporary file
  tf <- tempfile()
  status <- 
    utils::download.file(
      url = download_url, 
      destfile = tf,
      mode = "wb"
    )
  if (status != 0) {
    warning("File download failed")
    return(FALSE)
  }
  # Extract the archive from the temporary file to the install directory
  archive::archive_extract(tf, dir = install_dir, strip_components = 1)
  # Delete the temporary file
  unlink(tf)
  # Update the user config files with the locations of the installed files
  set_ffmpeg(file.path(install_dir, "bin", "ffmpeg.exe"))
  set_ffprobe(file.path(install_dir, "bin", "ffprobe.exe"))
  set_ffplay(file.path(install_dir, "bin", "ffplay.exe"))
  
  TRUE
}
