
# mediainfo() -------------------------------------------------------------

#' Run a MediaInfo command
#'
#' `mediainfo()` runs the MediaInfo program with the arguments in `command` and
#' returns what it prints. MediaInfo reads information about media files.
#'
#' `mediainfo()` is a direct command. The package passes `command` to MediaInfo
#' exactly as you wrote it, so you must add any quotes that it needs. To get a
#' tibble or a value instead, use [mediainfo_template()], [mediainfo_query()] or
#' [mediainfo_parameter()]. These functions quote their arguments for you.
#'
#' @param command A string with the arguments to give MediaInfo.
#' @return A character vector with the text that MediaInfo prints, one element
#'   for each line.
#'
#' @seealso [mediainfo_template()], [mediainfo_query()] and
#'   [mediainfo_parameter()] for a tibble or a value. [get_duration()] and the
#'   other `get_*()` functions for common single values.
#' @family direct command functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' mediainfo("--Version")
#' @export
mediainfo <- function(command) {
  # Validate arguments
  rlang::check_string(command)
  # Look up mediainfo location and run it with command
  limit <- resolve_timeout()
  guard_timeout(
    "MediaInfo", limit,
    system(glue('"{find_mediainfo()}" {command}'), intern = TRUE,
           timeout = limit)
  )
}

# mediainfo_parameter() ---------------------------------------------------

#' Query a single parameter from a single MediaInfo section
#'
#' `mediainfo_parameter()` uses the MediaInfo program to read one value, such as
#' the video width, from media files. MediaInfo groups its values in sections,
#' such as `"General"`, `"Video"` and `"Audio"`. You name the section and the
#' parameter to read.
#'
#' Give several files in `file` to get one value for each file. The function
#' returns a vector, not a tibble. The `probe_*()` functions read similar
#' information with FFprobe and return tibbles.
#'
#' @param file A character vector of one or more media file paths.
#' @param section A string. The name of the MediaInfo section to read
#'   `parameter` from.
#' @param parameter A string. The name of the MediaInfo parameter to read from
#'   `section`.
#' @param typed A logical. If `TRUE` (the default), the function converts the
#'   values to their natural type, for example to numbers. If `FALSE`, it
#'   returns strings.
#' @return A vector with one value for each element of `file`. A value is `NA`
#'   when it is empty, when `section` has no such parameter, or when the file
#'   could not be read.
#'
#'   The function does not stop at a file that it could not read. It reads the
#'   other files, and then gives one warning that names the files it could not
#'   read. A file that reaches the time limit counts as not read; see
#'   [with_timeout()].
#'
#' @seealso [mediainfo_query()] to read several parameters at once.
#'   [mediainfo_template()] to apply a whole template. [probe_all()] to read
#'   information with FFprobe. [get_duration()] and the other `get_*()`
#'   functions for common single values.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' mediainfo_parameter(video, section = "Video", parameter = "Width")
#' @export
mediainfo_parameter <- function(file, section, parameter, typed = TRUE) {
  check_path_vector(file)
  rlang::check_string(section)
  rlang::check_string(parameter)
  rlang::check_bool(typed)
  # D074, at the front door rather than left to the loop below: that loop `next`s
  # past an unreadable file without reaching run_program() at all, so a call
  # naming only missing files used to accept an invalid limit in silence -- and
  # run_program() refuses a missing MediaInfo binary before it resolves the
  # limit, which made the refusal the PATH's on a machine without one (D036).
  resolve_timeout()

  inform <- paste0("--Inform=", section, ";%", parameter, "%")
  loc <- NULL
  failed <- character(0)
  timed_out <- character(0)
  out <- character(length(file))
  for (i in seq_along(file)) {
    f <- file[[i]]
    if (!file.exists(f)) {
      failed <- c(failed, f)
      out[[i]] <- NA_character_
      next
    }
    if (is.null(loc)) loc <- find_mediainfo()
    # A timeout on one file is that file's failure, not the call's: this loop
    # documents an NA per unreadable file, so an escaping abort would discard
    # the values already collected for the files before it (D047).
    res <- absorb_timeout(run_program(loc, c(inform, f), program = "MediaInfo"))
    if (is_absorbed_timeout(res)) {
      failed <- c(failed, f)
      timed_out <- c(timed_out, f)
      out[[i]] <- NA_character_
      next
    }
    # A missing section/parameter prints nothing or a multi-line dump.
    out[[i]] <- if (length(res) == 1) res else NA_character_
  }
  warn_unreadable(failed, timed_out)
  if (typed) coerce_column(out) else out
}

# mediainfo_query() -------------------------------------------------------

#' Query multiple parameters from a single MediaInfo section
#'
#' `mediainfo_query()` uses the MediaInfo program to read several parameters
#' from one section, and returns a tibble. To read parameters from more than
#' one section in one call, use [mediainfo_template()].
#'
#' Give several files in `file` to get one row for each file. The first column,
#' `file`, names the input file. The `probe_*()` functions read similar
#' information with FFprobe.
#'
#' @param file A character vector of one or more media file paths.
#' @param section A string. The name of the MediaInfo section to read
#'   `parameters` from.
#' @param parameters A character vector of one or more MediaInfo parameters to
#'   read from `section`.
#' @param names A character vector of column names, one for each element of
#'   `parameters`. The default is `parameters`. The function keeps the names as
#'   you give them, but removes spaces at their start and end.
#' @param typed A logical. If `TRUE` (the default), numeric columns become
#'   numbers and empty values become `NA`. If `FALSE`, all columns stay strings.
#' @return A tibble with one row for each input file. The first column is
#'   `file`, and then there is one column for each parameter.
#'
#'   A file that the function could not read gets a row of `NA` values. The
#'   function reads the other files, and then gives one warning that names the
#'   files it could not read. A file that reaches the time limit counts as not
#'   read; see [with_timeout()].
#'
#' @seealso [mediainfo_parameter()] to read a single value.
#'   [mediainfo_template()] to apply a whole template. [probe_all()] to read
#'   information with FFprobe. [get_duration()] and the other `get_*()`
#'   functions for common single values.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' mediainfo_query(video, section = "Video", parameters = c("Width", "Height"))
#' @export
mediainfo_query <- function(file, section, parameters, names = parameters,
                            typed = TRUE) {
  check_path_vector(file)
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
  # D074: the verb the caller typed names a bad limit, not the reader below.
  resolve_timeout()
  out <- mediainfo_read(file, inform)
  if (typed) type_columns(out) else out
}

# mediainfo_template() ----------------------------------------------------

#' Describe media files by applying a MediaInfo template
#'
#' `mediainfo_template()` uses the MediaInfo program to describe media files,
#' and returns a tibble. It applies a MediaInfo template, which can read many
#' parameters from many sections.
#'
#' The package comes with two templates, `"brief"` and `"extended"`. You can
#' also give your own template file. Give several files in `file` to get one
#' row for each file. The first column, `file`, names the input file. The
#' `probe_*()` functions read similar information with FFprobe.
#'
#' @param file A character vector of one or more media file paths.
#' @param template A string. Use `"brief"` or `"extended"` for a template that
#'   comes with the package. Use `"custom"` to apply the file in `templatefile`.
#' @param templatefile The path to your own MediaInfo template, a `.txt` file
#'   that makes MediaInfo print comma-separated values. Give it when `template`
#'   is `"custom"`, and only then. The default is `NULL`.
#' @param typed A logical. If `TRUE` (the default), numeric columns become
#'   numbers and empty values become `NA`. If `FALSE`, all columns stay strings.
#' @return A tibble with one row for each input file. The template sets the
#'   columns, their names and their order. The function keeps the column names
#'   of a custom template, but removes spaces at their start and end.
#'
#'   A file that the function could not read gets a row of `NA` values. The
#'   function reads the other files, and then gives one warning that names the
#'   files it could not read. A file that reaches the time limit counts as not
#'   read; see [with_timeout()].
#'
#' @seealso [mediainfo_query()] to read one section. [mediainfo_parameter()] to
#'   read a single value. [probe_all()] to read information with FFprobe.
#'   [get_duration()] and the other `get_*()` functions for common single
#'   values.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' mediainfo_template(video, template = "brief")
#' @export
mediainfo_template <- function(file,
                               template = c("brief", "extended", "custom"),
                               templatefile = NULL,
                               typed = TRUE) {
  template <- rlang::arg_match(template)
  check_path_vector(file)
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
  # D074: the verb the caller typed names a bad limit, not the reader below.
  resolve_timeout()
  out <- mediainfo_read(file, inform)
  if (typed) type_columns(out) else out
}

# mediainfo_read() --------------------------------------------------------

# Shared reader for the CSV-emitting MediaInfo verbs (query/template). Runs the
# given `--Inform=` argument against each file, parses the two-line CSV output
# as character (user-supplied column names kept verbatim, only surrounding
# whitespace trimmed), and stacks the rows with a leading `file` column.
# Unreadable files yield an all-NA row and a warning rather than aborting.
mediainfo_read <- function(file, inform) {
  loc <- NULL
  failed <- character(0)
  timed_out <- character(0)
  rows <- vector("list", length(file))
  for (i in seq_along(file)) {
    f <- file[[i]]
    if (!file.exists(f)) {
      failed <- c(failed, f)
      rows[[i]] <- tibble::tibble(file = f)
      next
    }
    if (is.null(loc)) loc <- find_mediainfo()
    # Same absorption as mediainfo_parameter(), for the same reason: this
    # reader also promises an NA row per unreadable file (D047).
    res <- absorb_timeout(run_program(loc, c(inform, f), program = "MediaInfo"))
    if (is_absorbed_timeout(res)) {
      failed <- c(failed, f)
      timed_out <- c(timed_out, f)
      rows[[i]] <- tibble::tibble(file = f)
      next
    }
    # Valid output is a header line plus a values line; anything shorter means
    # MediaInfo could not read the file (empty or header-only). Treat it like a
    # missing file: warn + NA row rather than letting read.csv abort (D-M04-7).
    if (length(res) < 2) {
      failed <- c(failed, f)
      rows[[i]] <- tibble::tibble(file = f)
      next
    }
    df <- utils::read.csv(
      text = res, check.names = FALSE, strip.white = TRUE,
      colClasses = "character"
    )
    names(df) <- trimws(names(df))
    rows[[i]] <- tibble::add_column(tibble::as_tibble(df), file = f, .before = 1)
  }
  warn_unreadable(failed, timed_out)
  dplyr::bind_rows(rows)
}

# warn_unreadable() -------------------------------------------------------

# Emit the shared "could not read these files" warning used by the resilient
# MediaInfo readers. No-op when nothing failed.
warn_unreadable <- function(failed, timed_out = character(0)) {
  if (length(failed)) {
    cli::cli_warn(c(
      "Could not read {length(failed)} file{?s}; returning {.val {NA}} row{?s}.",
      "x" = "{.file {failed}}",
      if (length(timed_out)) c(
        "i" = "{length(timed_out)} of {cli::qty(length(failed))}{?these/those} \\
               timed out rather than being unreadable; raise or remove \\
               {.code options(tidymedia.timeout = )}."
      )
    ))
  }
  invisible(failed)
}

# get_duration() ----------------------------------------------------------

#' Get the duration of a media file
#'
#' `get_duration()` uses the MediaInfo program to look up the duration of a
#' media file. You choose the section of the file and the unit.
#'
#' The function returns one number for each file. The `probe_*()` functions,
#' [mediainfo_query()] and [mediainfo_template()] return tibbles instead.
#'
#' @param file A character vector of one or more media file paths.
#' @param section A string indicating the MediaInfo section from which to query
#'   the duration value. Can be either \code{"General"}, \code{"Video"}, or
#'   \code{"Audio"} (default = \code{"General"}).
#' @param unit A string indicating whether the duration should be returned in
#'   milliseconds (\code{"ms"}), seconds (\code{"sec"}), minutes (\code{"min"}),
#'   or hours (\code{"hour"}) (default = \code{"ms"}).
#' @return A double vector (one per file) giving the duration of the specified
#'   section in the specified units.
#'
#' @seealso [mediainfo_parameter()] for arbitrary MediaInfo fields, and
#'   [probe_all()] to read information with FFprobe.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' get_duration(video, unit = "sec")
#' @export
get_duration <- function(file,
                         section = c("General", "Video", "Audio"),
                         unit = c("ms", "sec", "min", "hour")) {

  section <- rlang::arg_match(section)
  unit <- rlang::arg_match(unit)
  # D074, both lines: `file` is checked HERE and not left to
  # mediainfo_parameter() below, because a check the caller's own call fails has
  # to report before the limit refusal does -- delegating it put the refusal
  # first whenever the limit was invalid (M94 review G1). Then the refusal, so
  # the verb the caller typed names a bad limit, not the reader.
  check_path_vector(file)
  resolve_timeout()
  duration <- mediainfo_parameter(
    file = file,
    section = section,
    parameter = "Duration"
  )
  divisor <- switch(unit, ms = 1, sec = 1000, min = 1000 * 60,
                    hour = 1000 * 60 * 60)
  duration / divisor
}

# get_frame_rate() ---------------------------------------------------------

#' Get the video frame rate of a media file
#'
#' `get_frame_rate()` uses the MediaInfo program to look up the video frame
#' rate of a media file, in frames per second (fps). The glossary in
#' `vignette("tidymedia")` explains media terms such as frame rate.
#'
#' The function returns one number for each file. The `probe_*()` functions,
#' [mediainfo_query()] and [mediainfo_template()] return tibbles instead.
#'
#' @param file A character vector of one or more media file paths.
#' @return A double vector (one per file) giving the video frame rate in fps.
#'
#' @seealso [mediainfo_parameter()] for arbitrary MediaInfo fields, and
#'   [probe_all()] to read information with FFprobe.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' get_frame_rate(video)
#' @export
get_frame_rate <- function(file) {
  # D074, both lines: `file` is checked HERE and not left to
  # mediainfo_parameter() below, because a check the caller's own call fails has
  # to report before the limit refusal does -- delegating it put the refusal
  # first whenever the limit was invalid (M94 review G1). Then the refusal, so
  # the verb the caller typed names a bad limit, not the reader.
  check_path_vector(file)
  resolve_timeout()
  mediainfo_parameter(file = file, section = "Video", parameter = "FrameRate")
}

# get_width() -------------------------------------------------------------

#' Get the video width of a media file
#'
#' `get_width()` uses the MediaInfo program to look up the video width of a
#' media file, in pixels (px).
#'
#' The function returns one number for each file. The `probe_*()` functions,
#' [mediainfo_query()] and [mediainfo_template()] return tibbles instead.
#'
#' @param file A character vector of one or more media file paths.
#' @return A double vector (one per file) giving the video width in px.
#'
#' @seealso [mediainfo_parameter()] for arbitrary MediaInfo fields, and
#'   [probe_all()] to read information with FFprobe.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' get_width(video)
#' @export
get_width <- function(file) {
  # D074, both lines: `file` is checked HERE and not left to
  # mediainfo_parameter() below, because a check the caller's own call fails has
  # to report before the limit refusal does -- delegating it put the refusal
  # first whenever the limit was invalid (M94 review G1). Then the refusal, so
  # the verb the caller typed names a bad limit, not the reader.
  check_path_vector(file)
  resolve_timeout()
  mediainfo_parameter(file = file, section = "Video", parameter = "Width")
}

# get_height() -------------------------------------------------------------

#' Get the video height of a media file
#'
#' `get_height()` uses the MediaInfo program to look up the video height of a
#' media file, in pixels (px).
#'
#' The function returns one number for each file. The `probe_*()` functions,
#' [mediainfo_query()] and [mediainfo_template()] return tibbles instead.
#'
#' @param file A character vector of one or more media file paths.
#' @return A double vector (one per file) giving the video height in px.
#'
#' @seealso [mediainfo_parameter()] for arbitrary MediaInfo fields, and
#'   [probe_all()] to read information with FFprobe.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' get_height(video)
#' @export
get_height <- function(file) {
  # D074, both lines: `file` is checked HERE and not left to
  # mediainfo_parameter() below, because a check the caller's own call fails has
  # to report before the limit refusal does -- delegating it put the refusal
  # first whenever the limit was invalid (M94 review G1). Then the refusal, so
  # the verb the caller typed names a bad limit, not the reader.
  check_path_vector(file)
  resolve_timeout()
  mediainfo_parameter(file = file, section = "Video", parameter = "Height")
}

# get_sample_rate() ------------------------------------------------------

#' Get the audio sample rate of a media file
#'
#' `get_sample_rate()` uses the MediaInfo program to look up the audio sample
#' rate of a media file, in hertz (Hz). The glossary in `vignette("tidymedia")`
#' explains media terms such as sample rate.
#'
#' The function returns one number for each file. The `probe_*()` functions,
#' [mediainfo_query()] and [mediainfo_template()] return tibbles instead.
#'
#' @param file A character vector of one or more media file paths.
#' @return A double vector (one per file) giving the audio sample rate in Hz.
#'
#' @seealso [mediainfo_parameter()] for arbitrary MediaInfo fields, and
#'   [probe_all()] to read information with FFprobe.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("mediainfo"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' get_sample_rate(video)
#' @export
get_sample_rate <- function(file) {
  # D074, both lines: `file` is checked HERE and not left to
  # mediainfo_parameter() below, because a check the caller's own call fails has
  # to report before the limit refusal does -- delegating it put the refusal
  # first whenever the limit was invalid (M94 review G1). Then the refusal, so
  # the verb the caller typed names a bad limit, not the reader.
  check_path_vector(file)
  resolve_timeout()
  mediainfo_parameter(file = file, section = "Audio", parameter = "SamplingRate")
}
