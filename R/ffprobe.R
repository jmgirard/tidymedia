
# ffprobe() ---------------------------------------------------------------

#' Send a command to the FFprobe program
#'
#' Probe a media file for information. This is the Layer 0 escape hatch: the
#' `command` string is passed to FFprobe verbatim, so you are responsible for
#' quoting it. For structured, tibble-returning output use [probe_all()] and the
#' `probe_*()` shortcuts, which quote their arguments safely.
#'
#' @param command A string containing the command to send to FFprobe.
#' @return A string containing the text output by FFprobe.
#' @family escape hatch functions
#' @examplesIf nzchar(Sys.which("ffprobe"))
#' ffprobe("-version")
#' @export
ffprobe <- function(command) {
  rlang::check_string(command)
  out <- system(glue('"{find_ffprobe()}" {command}'), intern = TRUE)
  out
}


# probe_all() -------------------------------------------------------------

#' Look up information about media files using FFprobe
#'
#' Probe one or more media files and return their container- and stream-level
#' metadata as tibbles. `infile` may be a vector of several files: the results
#' are stacked and keyed by a leading `file` column, so the output is ready for
#' `dplyr` joins and filters over a whole batch.
#'
#' @param infile A character vector of one or more media-file locations (file
#'   paths or web links) to probe.
#' @param typed A logical. When `TRUE` (default) numeric columns are converted
#'   to integers/doubles and FFprobe's `"N/A"` becomes `NA`; fractions, ratios,
#'   hex identifiers, and text stay as strings. When `FALSE` every value is
#'   returned as an unconverted string.
#' @return A list of two tibbles: `container` (one row per input file) and
#'   `streams` (one row per stream, or a single `NA` row for a file with no
#'   readable streams). Both lead with a `file` column identifying the input.
#'   Files that cannot be probed yield an all-`NA` row and a warning rather than
#'   aborting the call.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("ffprobe"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' info <- probe_all(video)
#' info$container
#' info$streams
#' @export
probe_all <- function(infile, typed = TRUE) {
  if (!rlang::is_character(infile) || length(infile) == 0) {
    cli::cli_abort(
      "{.arg infile} must be a character vector of one or more file locations."
    )
  }
  rlang::check_bool(typed)

  containers <- vector("list", length(infile))
  streams_l <- vector("list", length(infile))
  failed <- character(0)

  for (i in seq_along(infile)) {
    f <- infile[[i]]
    res <- probe_one(f)
    if (is.null(res)) {
      failed <- c(failed, f)
      containers[[i]] <- tibble::tibble(file = f)
      streams_l[[i]] <- tibble::tibble(file = f)
      next
    }
    containers[[i]] <- tibble::add_column(res$container, file = f, .before = 1)
    if (nrow(res$streams) == 0) {
      streams_l[[i]] <- tibble::tibble(file = f)
    } else {
      streams_l[[i]] <- tibble::add_column(res$streams, file = f, .before = 1)
    }
  }

  if (length(failed)) {
    cli::cli_warn(c(
      "Could not probe {length(failed)} file{?s}; returning {.val {NA}} row{?s}.",
      "x" = "{.file {failed}}"
    ))
  }

  container <- dplyr::bind_rows(containers)
  streams <- dplyr::bind_rows(streams_l)
  if (typed) {
    container <- type_columns(container)
    streams <- type_columns(streams)
  }
  list(container = container, streams = streams)
}

# probe_one() -------------------------------------------------------------

# Probe a single file. Returns list(container, streams) of raw-character tibbles
# (no `file` column, no type conversion; probe_all() adds those), or NULL if the
# file cannot be probed (missing path, unreachable URL, unreadable media).
probe_one <- function(file) {
  loc <- find_ffprobe()
  base <- c("-i", file, "-v", "quiet")
  fmt <- run_program(
    loc, c(base, "-show_format", "-of", "default=nw=1"), program = "ffprobe"
  )
  if (length(fmt) == 0) return(NULL)
  container <- format_probe(fmt)

  n <- suppressWarnings(as.integer(container[["nb_streams"]]))
  if (length(n) != 1 || is.na(n) || n < 1) {
    return(list(container = container, streams = tibble::tibble()))
  }
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    s <- run_program(
      loc,
      c(base, "-show_streams", "-select_streams", as.character(i - 1),
        "-of", "default=nw=1"),
      program = "ffprobe"
    )
    rows[[i]] <- if (length(s)) format_probe(s) else tibble::tibble()
  }
  list(container = container, streams = dplyr::bind_rows(rows))
}

# probe_container() -------------------------------------------------------

#' Shortcut functions for probing specific information
#'
#' Return just the `container` tibble via `probe_container()`, just the
#' `streams` tibble via `probe_streams()`, or just the video/audio stream rows
#' via `probe_video()` / `probe_audio()`. Each takes **either** the output of
#' [probe_all()] (via `probe`) **or** one or more file locations (via `infile`);
#' passing `infile` reprobes, so reuse a `probe` object when working with large
#' files.
#'
#' @param probe A list object created by [probe_all()]. Must be `NULL` if
#'   `infile` is supplied.
#' @param infile A character vector of one or more media-file locations. Must be
#'   `NULL` if `probe` is supplied.
#' @param typed A logical passed to [probe_all()] when `infile` is used (default
#'   `TRUE`); ignored when `probe` is supplied.
#' @return A tibble containing only the requested information.
#' @family metadata functions
#' @examplesIf nzchar(Sys.which("ffprobe"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' # Probe directly from a file location ...
#' probe_container(infile = video)
#' # ... or reuse a probe object to avoid reprobing large files
#' info <- probe_all(video)
#' probe_video(info)
#' probe_audio(info)
#' @export
probe_container <- function(probe = NULL, infile = NULL, typed = TRUE) {
  resolve_probe(probe, infile, typed)$container
}

# probe_streams() ---------------------------------------------------------

#' @rdname probe_container
#' @export
probe_streams <- function(probe = NULL, infile = NULL, typed = TRUE) {
  resolve_probe(probe, infile, typed)$streams
}

# probe_video() -----------------------------------------------------------

#' @rdname probe_container
#' @export
probe_video <- function(probe = NULL, infile = NULL, typed = TRUE) {
  filter_streams(resolve_probe(probe, infile, typed)$streams, "video")
}

# probe_audio() -----------------------------------------------------------

#' @rdname probe_container
#' @export
probe_audio <- function(probe = NULL, infile = NULL, typed = TRUE) {
  filter_streams(resolve_probe(probe, infile, typed)$streams, "audio")
}

# filter_streams() --------------------------------------------------------

# Select stream rows of a given codec type. When every input file failed to
# probe the streams tibble carries only a `file` column (no `codec_type`), so
# guard against the missing column and return an empty result rather than
# aborting (keeps the D-M04-7 resilience contract on wholly-unreadable input).
filter_streams <- function(streams, type) {
  if (!"codec_type" %in% names(streams)) return(streams[0, , drop = FALSE])
  dplyr::filter(streams, .data$codec_type %in% type)
}

# resolve_probe() ---------------------------------------------------------

# Shared front-end for the probe_*() shortcuts: require exactly one of `probe`
# or `infile`, and probe the file(s) when `infile` is given.
resolve_probe <- function(probe, infile, typed, call = rlang::caller_env()) {
  if (is.null(probe) + is.null(infile) != 1) {
    cli::cli_abort(
      "Provide exactly one of {.arg probe} or {.arg infile}.", call = call
    )
  }
  if (!is.null(infile)) probe <- probe_all(infile, typed = typed)
  probe
}

# format_probe() ----------------------------------------------------------

# Turn FFprobe's `key=value` lines into a one-row tibble. Splits on the *first*
# `=` only, so values that themselves contain `=` (e.g. tag values) survive.
format_probe <- function(x) {
  x <- x[nzchar(x)]
  key <- sub("=.*$", "", x)
  value <- sub("^[^=]*=", "", x)
  names(value) <- key
  tibble::as_tibble(as.list(value))
}


# convert_fractions() -----------------------------------------------------

#' Convert string fractions to doubles
#'
#' This is useful for columns such as frame rates, which FFprobe often lists as
#' fractions such as `"30000/1001"` (this converts to 29.97003).
#'
#' @param x A character vector containing fractions (`"a/b"`) or plain numbers
#'   to evaluate. Surrounding whitespace is ignored; `NA` passes through.
#' @return A numeric vector with each fraction evaluated to a double.
#' @noRd
convert_fractions <- function(x) {
  if (!rlang::is_character(x)) {
    cli::cli_abort("{.arg x} must be a character vector.")
  }
  vapply(x, function(s) {
    if (is.na(s)) return(NA_real_)
    parts <- strsplit(trimws(s), "/", fixed = TRUE)[[1]]
    nums <- suppressWarnings(as.numeric(parts))
    if (length(parts) == 2 && !anyNA(nums)) return(nums[[1]] / nums[[2]])
    if (length(parts) == 1 && !is.na(nums)) return(nums)
    cli::cli_abort(
      "{.arg x} contains a value that is not a number or fraction: {.val {s}}."
    )
  }, numeric(1), USE.NAMES = FALSE)
}
