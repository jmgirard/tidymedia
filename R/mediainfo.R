
# mediainfo() -------------------------------------------------------------

#' Run MediaInfo CLI
#'
#' Run the command through the MediaInfo command line interface (CLI) and return
#' its output as a string. This is the Layer 0 escape hatch: `command` is passed
#' to MediaInfo verbatim, so you are responsible for quoting it. For structured,
#' tibble-returning output use [mediainfo_template()], [mediainfo_query()], or
#' [mediainfo_parameter()], which quote their arguments safely.
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
#' Query a single parameter in a single section from MediaInfo. `file` may be a
#' vector of several files, in which case a vector of values (one per file) is
#' returned.
#'
#' @param file A character vector of one or more media-file paths.
#' @param section A string containing the name of the mediainfo section from
#'   which to query \code{parameter}.
#' @param parameter A string containing the name of the mediainfo parameter to
#'   query from \code{section}.
#' @param typed A logical. When \code{TRUE} (default) the value is converted to
#'   its natural type (e.g. a number); when \code{FALSE} it is returned as a
#'   string.
#' @return A vector the same length as \code{file} holding each requested value,
#'   or \code{NA} where the value was empty, the section-parameter combination
#'   was not found, or the file could not be read (a warning is issued for
#'   unreadable files rather than aborting).
#'
#' @family mediainfo functions
#' @export
mediainfo_parameter <- function(file, section, parameter, typed = TRUE) {
  if (!rlang::is_character(file) || length(file) == 0) {
    cli::cli_abort(
      "{.arg file} must be a character vector of one or more file paths."
    )
  }
  rlang::check_string(section)
  rlang::check_string(parameter)
  rlang::check_bool(typed)

  inform <- paste0("--Inform=", section, ";%", parameter, "%")
  loc <- NULL
  failed <- character(0)
  out <- character(length(file))
  for (i in seq_along(file)) {
    f <- file[[i]]
    if (!file.exists(f)) {
      failed <- c(failed, f)
      out[[i]] <- NA_character_
      next
    }
    if (is.null(loc)) loc <- find_mediainfo()
    res <- run_program(loc, c(inform, f), program = "mediainfo")
    # A missing section/parameter prints nothing or a multi-line dump.
    out[[i]] <- if (length(res) == 1) res else NA_character_
  }
  warn_unreadable(failed)
  if (typed) coerce_column(out) else out
}

# mediainfo_query() -------------------------------------------------------

#' Query multiple parameters from a single MediaInfo section
#'
#' Create a tibble containing multiple parameters from a single MediaInfo
#' section. To query parameters from multiple sections at once, use
#' \code{mediainfo_summary()} or \code{mediainfo_template()}. `file` may be a
#' vector of several files; results are stacked with a leading \code{file}
#' column.
#'
#' @param file A character vector of one or more media-file paths.
#' @param section A string indicating the MediaInfo section from which to query
#'   the \code{parameters}.
#' @param parameters A character vector of one or more MediaInfo parameters to
#'   query from \code{section}.
#' @param names A character vector naming the returned columns; must be the same
#'   length as \code{parameters} (default = \code{parameters}). Supplied names
#'   are used verbatim.
#' @param typed A logical. When \code{TRUE} (default) numeric columns are typed
#'   and empty values become \code{NA}; when \code{FALSE} columns stay strings.
#' @return A tibble with one row per input file, leading with a \code{file}
#'   column and one column per requested parameter.
#'
#' @family mediainfo functions
#' @export
mediainfo_query <- function(file, section, parameters, names = parameters,
                            typed = TRUE) {
  if (!rlang::is_character(file) || length(file) == 0) {
    cli::cli_abort(
      "{.arg file} must be a character vector of one or more file paths."
    )
  }
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
    cli::cli_abort(
      "{.arg parameters} and {.arg names} must have the same length."
    )
  }
  rlang::check_bool(typed)

  # `\\n` is MediaInfo's Inform newline escape (a literal backslash-n): the
  # first line becomes the CSV header, the second the values.
  inform <- paste0(
    "--Inform=", section, ";", paste(names, collapse = ", "), "\\n",
    paste(paste0("%", parameters, "%"), collapse = ", ")
  )
  out <- mediainfo_read(file, inform)
  if (typed) type_columns(out) else out
}

# mediainfo_template() ----------------------------------------------------

#' Describe media files by applying a MediaInfo template
#'
#' Create a tibble describing one or more media files by applying a MediaInfo
#' template, which can pull multiple parameters from multiple sections. Two
#' templates ship with the package (\code{"brief"} and \code{"extended"}); a
#' custom template file can also be supplied. `file` may be a vector of several
#' files; results are stacked with a leading \code{file} column.
#'
#' @param file A character vector of one or more media-file paths.
#' @param template A string naming the template to apply: a built-in
#'   (\code{"brief"} or \code{"extended"}) or \code{"custom"} to apply the file
#'   given in \code{templatefile}.
#' @param templatefile Either the path to a MediaInfo template (.txt) file
#'   formatted to output comma-separated values (required when \code{template}
#'   is \code{"custom"}) or \code{NULL} (default).
#' @param typed A logical. When \code{TRUE} (default) numeric columns are typed
#'   and empty values become \code{NA}; when \code{FALSE} columns stay strings.
#' @return A tibble with one row per input file. The columns (and their
#'   names/order) are determined by the template; custom-template column names
#'   are used verbatim.
#'
#' @family mediainfo functions
#' @export
mediainfo_template <- function(file,
                               template = c("brief", "extended", "custom"),
                               templatefile = NULL,
                               typed = TRUE) {
  template <- rlang::arg_match(template)
  if (!rlang::is_character(file) || length(file) == 0) {
    cli::cli_abort(
      "{.arg file} must be a character vector of one or more file paths."
    )
  }
  if (!is.null(templatefile)) check_file_exists(templatefile)
  if ((template == "custom") != !is.null(templatefile)) {
    cli::cli_abort(c(
      "A {.arg templatefile} is required exactly when {.arg template} is {.val custom}.",
      "i" = 'Pass template = "custom" together with a templatefile, or a \\
             built-in template on its own.'
    ))
  }
  rlang::check_bool(typed)
  # If using a built-in template, build its file path
  if (template != "custom") {
    templatefile <- system.file(
      glue("extdata/mediainfo_template_{template}.txt"),
      package = "tidymedia"
    )
  }
  inform <- paste0("--Inform=file://", templatefile)
  out <- mediainfo_read(file, inform)
  if (typed) type_columns(out) else out
}

#' @inherit mediainfo_template
#' @export
mediainfo_summary <- mediainfo_template

# mediainfo_read() --------------------------------------------------------

# Shared reader for the CSV-emitting MediaInfo verbs (query/template). Runs the
# given `--Inform=` argument against each file, parses the two-line CSV output
# as character (user-supplied column names kept verbatim, only surrounding
# whitespace trimmed), and stacks the rows with a leading `file` column.
# Unreadable files yield an all-NA row and a warning rather than aborting.
mediainfo_read <- function(file, inform) {
  loc <- NULL
  failed <- character(0)
  rows <- vector("list", length(file))
  for (i in seq_along(file)) {
    f <- file[[i]]
    if (!file.exists(f)) {
      failed <- c(failed, f)
      rows[[i]] <- tibble::tibble(file = f)
      next
    }
    if (is.null(loc)) loc <- find_mediainfo()
    res <- run_program(loc, c(inform, f), program = "mediainfo")
    df <- utils::read.csv(
      text = res, check.names = FALSE, strip.white = TRUE,
      colClasses = "character"
    )
    names(df) <- trimws(names(df))
    rows[[i]] <- tibble::add_column(tibble::as_tibble(df), file = f, .before = 1)
  }
  warn_unreadable(failed)
  dplyr::bind_rows(rows)
}

# warn_unreadable() -------------------------------------------------------

# Emit the shared "could not read these files" warning used by the resilient
# MediaInfo readers. No-op when nothing failed.
warn_unreadable <- function(failed) {
  if (length(failed)) {
    cli::cli_warn(c(
      "Could not read {length(failed)} file{?s}; returning {.val {NA}} row{?s}.",
      "x" = "{.file {failed}}"
    ))
  }
  invisible(failed)
}

# get_duration() ----------------------------------------------------------

#' Get the duration of a media file
#'
#' Use MediaInfo to quickly look up the duration of different sections of a
#' media file in various units.
#'
#' @param file A character vector of one or more media-file paths.
#' @param section A string indicating the MediaInfo section from which to query
#'   the duration value. Can be either \code{"General"}, \code{"Video"}, or
#'   \code{"Audio"} (default = \code{"General"}).
#' @param unit A string indicating whether the duration should be returned in
#'   milliseconds (\code{"ms"}), seconds (\code{"sec"}), minutes (\code{"min"}),
#'   or hours (\code{"hour"}) (default = \code{"ms"}).
#' @return A double vector (one per file) giving the duration of the specified
#'   section in the specified units.
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
  divisor <- switch(unit, ms = 1, sec = 1000, min = 1000 * 60,
                    hour = 1000 * 60 * 60)
  duration / divisor
}

# get_framerate() ---------------------------------------------------------

#' Get the video frame rate of a media file
#'
#' Use MediaInfo to quickly look up the video frame rate of a media file in
#' frames per second (fps).
#'
#' @param file A character vector of one or more media-file paths.
#' @return A double vector (one per file) giving the video frame rate in fps.
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
#' @param file A character vector of one or more media-file paths.
#' @return A double vector (one per file) giving the video width in px.
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
#' @param file A character vector of one or more media-file paths.
#' @return A double vector (one per file) giving the video height in px.
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
#' @param file A character vector of one or more media-file paths.
#' @return A double vector (one per file) giving the audio sampling rate in Hz.
#'
#' @family mediainfo functions
#' @family convenience functions
#' @export
get_samplingrate <- function(file) {
  mediainfo_parameter(file = file, section = "Audio", parameter = "SamplingRate")
}
