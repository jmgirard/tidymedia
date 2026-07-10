
# mediainfo() -------------------------------------------------------------

#' Run MediaInfo CLI
#'
#' Run the command through the MediaInfo command line interface (CLI) and return
#' its output as a string.
#' 
#' @param command A string containing a mediainfo command.
#' @return A string containing the command line output from mediainfo.
#' 
#' @family mediainfo functions
#' @family cli functions
#' @export
mediainfo <- function(command) {
  # Validate arguments
  rlang::check_string(command)
  # Look up mediainfo location and run it with command
  system(glue('"{find_mediainfo()}" {command}'), intern = TRUE)
}

# mediainfo_parameter() ---------------------------------------------------

#' Query a single parameter from a single MediaInfo section
#'
#' Query a single parameter in a single section from MediaInfo and return it as
#' an atomic object (not a tibble).
#'
#' @param file A string containing the path to a media file.
#' @param section A string containing the name of the mediainfo
#'   section from which to query \code{parameter}.
#' @param parameter A string containing the name of the mediainfo
#'   parameter to query from \code{section}.
#' @return A string or double containing the requested parameter's value or
#'   \code{NA} in the cases that the value was empty or the section-parameter
#'   combination could not be found.
#'   
#' @family mediainfo functions
#' @export
mediainfo_parameter <- function(file, section, parameter) {
  
  rlang::check_string(file)
  rlang::check_string(section)
  rlang::check_string(parameter)

  command <- glue('--Inform={section};%{parameter}% "{file}"')
  output <- mediainfo(command)
  # If section is not found, it outputs a long factor
  if (length(output) != 1) output <- NA_character_
  output <- utils::type.convert(output)
  output
}

# mediainfo_query() -------------------------------------------------------

#' Query multiple parameters from a single MediaInfo section
#'
#' Create a new row tibble that contains multiple parameters from a single
#' MediaInfo section. To query parameters from multiple sections at the same
#' time, use either \code{mediainfo_summary()} or \code{mediainfo_template()}.
#'
#' @param file A string indicating the path to a media file.
#' @param section A string indicating the MediaInfo section from which to query
#'   the \code{parameters}. Note that querying from multiple sections at once
#'   requires using \code{mediainfo_template()}.
#' @param parameters A vector of one or more strings indicating the MediaInfo
#'   parameters to query from \code{section}.
#' @param names A vector of one or more strings indicating the names of the
#'   variables in the returned tibble; must be the same length as
#'   \code{parameters} (default = the names of the strings in
#'   \code{parameters}).
#' @return A row tibble containing each parameter as a separate variable.
#' 
#' @family mediainfo functions
#' @export
mediainfo_query <- function(file, section, parameters, names = parameters) {
  # Validate arguments
  check_file_exists(file)
  rlang::check_string(section)
  if (!rlang::is_character(parameters) || length(parameters) == 0) {
    cli::cli_abort(
      "{.arg parameters} must be a character vector with at least one element."
    )
  }
  if (!rlang::is_character(names)) {
    cli::cli_abort("{.arg names} must be a character vector.")
  }
  if (length(parameters) != length(names)) {
    cli::cli_abort("{.arg parameters} and {.arg names} must have the same length.")
  }
  # Create mediainfo command
  command <- glue(
    '"--Inform={section};{paste0(names, collapse = ", ")}\\n',
    '{paste(paste0("%", parameters, "%"), collapse = ", ")}" "{file}"'
  )
  # Run mediainfo command
  output <- mediainfo(command)
  # Format the mediainfo output
  output <- utils::read.csv(text = output)
  output <- tibble::as_tibble(output)
  output <- tibble::add_column(output, File = file, .before = 1)
  output
}

# mediainfo_template() ----------------------------------------------------

#' Describe a media file by applying a MediaInfo template
#'
#' Create a new row tibble that contains information about a media file. This
#' information is gathered by applying a MediaInfo template, which can include
#' multiple parameters from multiple sections. This package include several
#' built-in templates that can be applied or a custom template file can be
#' created and used.
#'
#' @param file A string containing the file path to a media file.
#' @param template A string containing the template to be applied. Two templates
#'   are built into the package: \code{"brief"} and \code{"extended"}.
#'   Alternatively, \code{"custom"} can be used to apply a new template file
#'   specified in \code{templatefile}.
#' @param templatefile Either a string containing the file path to a MediaInfo
#'   template (.txt) file formatted to output comma-separated values (requires
#'   \code{template} to be set to \code{"custom"}) or \code{NULL} (default =
#'   \code{NULL}).
#' @return A row [tibble][tibble::tibble-package] containing variables
#'   describing \code{file}. The specific variables included and their
#'   ordering/naming is determined by the template.
#'   
#' @family mediainfo functions
#' @export
mediainfo_template <- function(file, 
                               template = c("brief", "extended", "custom"), 
                               templatefile = NULL) {
  # Validate arguments
  template <- rlang::arg_match(template)
  check_file_exists(file)
  if (!is.null(templatefile)) check_file_exists(templatefile)
  if ((template == "custom") != !is.null(templatefile)) {
    cli::cli_abort(c(
      "A {.arg templatefile} is required exactly when {.arg template} is {.val custom}.",
      "i" = 'Pass template = "custom" together with a templatefile, or a \\
             built-in template on its own.'
    ))
  }
  # If using a built-in template, build its file path
  if (template != "custom") {
    templatefile <- system.file(
      glue("extdata/mediainfo_template_{template}.txt"), 
      package = "tidymedia"
    )  
  }
  # Create the mediainfo command
  command <- glue('"--Inform=file://{templatefile}" "{file}"')
  # Run the MediaInfo command and capture output as string
  output <- mediainfo(command)
  # Turn the output string into a tibble
  output <- utils::read.csv(text = output)
  output <- tibble::as_tibble(output)
  # Return the formatted tibble
  output
}

#' @inherit mediainfo_template
#' @export
mediainfo_summary <- mediainfo_template

# get_duration() ----------------------------------------------------------

#' Get the duration of a media file
#'
#' Use MediaInfo to quickly look up the duration of different sections of a
#' media file in various units.
#'
#' @param file A string containing the file path of a media file.
#' @param section A string indicating the MediaInfo section from which to query
#'   the duration value. Can be either \code{"General"}, \code{"Video"}, or
#'   \code{"Audio"} (default = \code{"General"}).
#' @param unit A string indicating whether the duration should be returned in
#'   milliseconds (\code{"ms"}), seconds (\code{"sec"}), minutes (\code{"min"}),
#'   or hours (\code{"hour"}) (default = \code{"ms"}).
#' @return A double indicating the duration of the specified section of the file
#'   (in the specified units).
#'   
#' @family mediainfo functions
#' @family convenience functions
#' @export
get_duration <- function(file, 
                         section = c("General", "Video", "Audio"), 
                         unit = c("ms", "sec", "min", "hour")) {
  
  section <- rlang::arg_match(section)
  unit <- rlang::arg_match(unit)
  duration <- mediainfo_parameter(
    file = file, 
    section = section, 
    parameter = "Duration"
  )
  if (unit == "ms") {
    output <- duration
  } else if (unit == "sec") {
    output <- duration / 1000
  } else if (unit == "min") {
    output <- duration / 1000 / 60
  } else if (unit == "hour") {
    output <- duration / 1000 / 60 / 60
  }
  output
}

# get_framerate() ---------------------------------------------------------

#' Get the video frame rate of a media file
#'
#' Use MediaInfo to quickly look up the video frame rate of a media file in
#' frames per second (fps).
#'
#' @param file A string containing the file path of a media file.
#' @return A double indicating the video frame rate of \code{file} (in fps).
#' 
#' @family mediainfo functions
#' @family convenience functions
#' @export
get_framerate <- function(file) {
  mediainfo_parameter(file = file, section = "Video", parameter = "FrameRate")
}

# get_width() -------------------------------------------------------------

#' Get the video width of a media file
#'
#' Use MediaInfo to quickly look up the video width of a media file in
#' pixels (px).
#'
#' @param file A string containing the file path of a media file.
#' @return A double indicating the video width of \code{file} (in px).
#' 
#' @family mediainfo functions
#' @family convenience functions
#' @export
get_width <- function(file) {
  mediainfo_parameter(file = file, section = "Video", parameter = "Width")
}

# get_height() -------------------------------------------------------------

#' Get the video height of a media file
#'
#' Use MediaInfo to quickly look up the video height of a media file in
#' pixels (px).
#'
#' @param file A string containing the file path of a media file.
#' @return A double indicating the video height of \code{file} (in px).
#' 
#' @family mediainfo functions
#' @family convenience functions
#' @export
get_height <- function(file) {
  mediainfo_parameter(file = file, section = "Video", parameter = "Height")
}

# get_samplingrate() ------------------------------------------------------

#' Get the audio sampling rate of a media file
#'
#' Use MediaInfo to quickly look up the audio sampling rate of a media file in
#' hertz (Hz).
#'
#' @param file A string containing the file path of a media file.
#' @return A double indicating the audio sampling rate of \code{file} (in Hz).
#' 
#' @family mediainfo functions
#' @family convenience functions
#' @export
get_samplingrate <- function(file) {
  mediainfo_parameter(file = file, section = "Audio", parameter = "SamplingRate")
}
