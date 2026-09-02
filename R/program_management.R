
# Config directory ---------------------------------------------------------

# Where a remembered program location lives. `tools::R_user_dir()` is the one
# user config location CRAN policy sanctions (M097); it honors
# `R_USER_CONFIG_DIR`, which is how the suite keeps its writes out of the
# user's real configuration.
tm_config_dir <- function() {
  tools::R_user_dir("tidymedia", "config")
}

# Where a location set before M097 was written. Delegated to the library that
# wrote it rather than reconstructed by hand: the legacy layout differs per
# platform, and a single-platform suite cannot catch a second copy of those
# rules drifting. The only place the package computes it (M097 AC5).
tm_legacy_config_dir <- function() {
  rappdirs::user_config_dir("tidymedia", "R")
}

tm_config_file <- function(program, dir = tm_config_dir()) {
  file.path(dir, glue("{program}_location.txt"))
}

# Data directory -----------------------------------------------------------

# Where `install_on_win()` puts a downloaded FFmpeg build when the caller names
# no directory. `tools::R_user_dir()` is the one user data location CRAN policy
# sanctions (M098); it honors `R_USER_DATA_DIR`, which is how the suite keeps
# its writes out of the user's real data directory. The `ffmpeg` subdirectory
# is the extraction root: `archive_extract(strip_components = 1)` unpacks the
# build's contents into it, so the binaries land under `bin/` beneath it.
tm_install_dir <- function() {
  file.path(tools::R_user_dir("tidymedia", "data"), "ffmpeg")
}

# find_program() ----------------------------------------------------------

#' Find the location of a dependency program
#'
#' Returns the location of the requested program as a string.
#'
#' The program is looked up on the `PATH` first. When it is not there, the
#' location remembered by [set_program()] is read from
#' `tools::R_user_dir("tidymedia", "config")`; a location remembered by a
#' version of tidymedia before 0.2.0 was written to
#' `rappdirs::user_config_dir("tidymedia", "R")`, and that file is read only
#' when no file exists in the current directory.
#'
#' @param program A string indicating which program to find
#' @return Either a string indicating whether the requested program was found or
#'   `NULL` if the program could not be found.
#' @seealso [set_program()] to point tidymedia at a binary in a non-standard
#'   location, and [install_on_win()] to download FFmpeg on Windows.
#' @family program management functions
#' @examples
#' # Returns the path to the binary, or NULL with a warning if it is not found
#' find_ffmpeg()
#' find_mediainfo()
find_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo")) {
  
  # Validate arguments
  program <- rlang::arg_match(program)

  # First, look for program in path
  location <- Sys.which(program)
  
  if (location == "") {
    # If program not found, look for a user config file: the R_user_dir()
    # location first, then the pre-M097 one, so a location set before
    # the move is still found. The read falls back; nothing is migrated.
    config <- tm_config_file(program)
    if (!file.exists(config)) {
      config <- tm_config_file(program, tm_legacy_config_dir())
    }
    # If a user config file exists, read it in
    if (file.exists(config)) {
      location <- readLines(config)
      # Verify that the location in the user config file is valid
      if (Sys.which(location) == "") {
        cli::cli_warn(c(
          "{program} was configured at {.file {location}} but that file no \\
           longer seems to exist.",
          "i" = "Use {.fn set_{program}} to point tidymedia at it again."
        ))
        location <- NULL
      }
    } else {
      # If config file not found, return NULL value and warning
      location <- NULL
      cli::cli_warn(c(
        "Failed to find {program}.",
        "i" = "Check that it is installed and, if necessary, use \\
               {.fn set_{program}}."
      ))
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

# run_program() -----------------------------------------------------------

# Run a resolved program with an argument vector and return its stdout as a
# character vector. Every token in `args` is passed through `shQuote()` so the
# CLI receives arguments verbatim without shell interpolation: file paths and
# MediaInfo `--Inform` strings containing spaces, quotes, `;`, `%`, or `$` are
# safe. This is the internal counterpart to the Layer 0 escape hatches
# `mediainfo()` / `ffprobe()`, which stay raw-string by design (D002); the
# structured readers build clean token vectors and route them through here.
#
# Callers pass tokens unquoted (one vector element per CLI argument); do not
# pre-quote. `location` is the output of a `find_*()` call; a missing binary
# aborts rather than shelling out to nothing.
#
# `input` and `stderr` pass through to system2(): ffm_run() sets `input = ""`
# so FFmpeg cannot drain the parent's stdin (see ffmpeg()) and `stderr = ""`
# so encode progress/errors stream to the console; the metadata readers keep
# the quiet defaults (stderr discarded).
run_program <- function(location, args, program = "the program",
                        input = NULL, stderr = FALSE,
                        call = rlang::caller_env()) {
  if (is.null(location) || is.na(location) || !nzchar(location)) {
    cli::cli_abort("Could not locate {program}.", call = call)
  }
  # shQuote()'s default is sh-style quoting on every OS; on Windows the child
  # process parses its command line with cmd-style (double-quote) rules, so
  # the type must follow the platform or spaced paths break there.
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  # `suppress = TRUE` reproduces the suppressWarnings() this replaces: every
  # caller here has always seen warnings discarded, and the timeout is the only
  # new thing that reaches them (M69/D047). The limit is 0 -- no limit, byte-for
  # -byte today's behavior -- unless the caller set `tidymedia.timeout`.
  limit <- resolve_timeout(call = call)
  guard_timeout(
    program, limit,
    system2(location, args = shQuote(args, type = quote_type), stdout = TRUE,
            stderr = stderr, input = input, timeout = limit),
    suppress = TRUE,
    call = call
  )
}

# set_program() ------------------------------------------------------------

#' Set the location of a dependency program
#'
#' The location is remembered across sessions in a file named
#' `<program>_location.txt` under `tools::R_user_dir("tidymedia", "config")`,
#' which [find_program()] reads whenever the program is not on the `PATH`.
#' Once this file exists, a location remembered by a version of tidymedia
#' before 0.2.0 is no longer read.
#'
#' @param program A string indicating which program to set the location for.
#' @param location A string containing the location of the program.
#' @return A logical indicating whether the program location was set properly.
#'
#' @seealso [find_program()] to locate a configured binary, and
#'   [install_on_win()] to download FFmpeg on Windows.
#' @family program management functions
#' @examples
#' \dontrun{
#' # Point tidymedia at a binary in a non-standard location
#' set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")
#' }
#' @export
set_program <- function(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"),
                         location) {
  
  # Validate arguments
  program <- rlang::arg_match(program)
  rlang::check_string(location)
  if (Sys.which(location) == "") {
    cli::cli_abort("Can't find an executable at {.file {location}}.")
  }
  
  # Find where to save user configuration data (tools::R_user_dir(), M097)
  config_dir <- tm_config_dir()
  config_file <- tm_config_file(program, config_dir)
  
  # Create configuration directory if needed
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)
  
  # Save location to user configuration file
  writeLines(location, config_file)

  # Pointing at a different binary invalidates everything remembered about the
  # old one, so the session memo goes with it (M67/D044). Unconditional across
  # all four programs: cheap, and it cannot be forgotten when a second
  # capability memo is added later.
  forget_ffmpeg_capabilities()
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
#'   `NULL`, will default to the `ffmpeg` subdirectory of
#'   `tools::R_user_dir("tidymedia", "data")`, the user data directory CRAN
#'   policy sanctions.
#' @return A logical indicating whether the installation was successful.
#' @seealso [set_program()] to register an existing binary, and [find_ffmpeg()]
#'   to check what is currently configured.
#' @family program management functions
#' @examples
#' \dontrun{
#' # Download and install a static FFmpeg build (Windows)
#' install_on_win()
#' }
#' @export
install_on_win <- function(download_url = NULL,
                           install_dir = NULL) {
  
  if (is.null(download_url)) {
    download_url <- "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z"
  }
  if (is.null(install_dir)) {
    install_dir <- tm_install_dir()
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
    cli::cli_warn("File download failed.")
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
