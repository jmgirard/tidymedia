
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
#' @seealso [probe_all()] and the `probe_*()` shortcuts for structured,
#'   tibble-returning output.
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
#' This is tidymedia's **FFprobe** metadata reader, returning **tibbles** (one
#' row per file or per stream) — distinct from the **MediaInfo** readers
#' (`mediainfo_*()`, which return tibbles or values) and the scalar `get_*()`
#' helpers (which return a single value per file).
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
#' @seealso [mediainfo_template()] and [mediainfo_query()] for the MediaInfo
#'   backend, and [get_duration()] and friends for single scalar values.
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

# count_audio_streams() ---------------------------------------------------

# Count one input's audio streams, or return NA when the count cannot be had.
# This is the ONLY place the audio verbs' track-drop diagnostic assembles an
# FFprobe token vector -- it lives here beside probe_one(), the package's other
# FFprobe token builder, rather than in a Layer-2 verb body (D024/RR02 Q4). One
# narrow invocation rather than probe_all(), which runs FFprobe once per stream
# plus once for the container and warns on an unreadable file: this needs a
# single number and must stay silent.
#
# NA is "no answer", never "no audio". D024 licenses this probe only while its
# outcome changes nothing but whether a warning is signalled, so every failure
# path has to reach the caller as NA rather than as a condition:
#   - find_ffprobe() WARNS when the binary is missing and can also ERROR on a
#     corrupt user config, so it gets both a suppressWarnings() and a tryCatch()
#     of its own. suppressWarnings() rather than a bare Sys.which() keeps
#     find_program()'s user-config fallback, so a machine where ffprobe was
#     registered with set_ffprobe() is still found. Both are load-bearing:
#     nothing else here catches either channel.
#   - run_program() ABORTS on a NULL/empty location. Its own tryCatch() below is
#     what makes that silent; the explicit length/NA/nzchar short-circuit is
#     belt-and-braces, kept to avoid building a call that can only abort.
#     Measured at M44: deleting it leaves every test green, so do not read it as
#     the guarantee.
#   - a non-zero ffprobe exit arrives as a `status` attribute (system2 with
#     stdout = TRUE), which is not an R condition and would otherwise read as a
#     count of however many lines came back before the failure.
count_audio_streams <- function(file) {
  # find_ffprobe() can ERROR as well as warn, and the tryCatch() below does not
  # reach it: find_program() readLines() a user config written by set_ffprobe()
  # and then tests `if (Sys.which(location) == "")`, so an empty config gives
  # `if (logical(0))` and a two-line one gives a length-2 condition -- both
  # aborting the verb on a machine where it used to just run (M44 review F2).
  # length(loc) != 1L rather than is.null() first: `character(0)` would make
  # is.na(loc) return logical(0) and `if` throw on that too.
  loc <- tryCatch(suppressWarnings(find_ffprobe()), error = function(e) NULL)
  if (length(loc) != 1L || is.na(loc) || !nzchar(loc)) return(NA_integer_)
  out <- tryCatch(
    run_program(
      loc,
      c("-i", file, "-v", "error", "-select_streams", "a",
        "-show_entries", "stream=index", "-of", "csv=p=0"),
      program = "ffprobe"
    ),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(out) || !is.null(attr(out, "status"))) return(NA_integer_)
  sum(nzchar(trimws(out)))
}

# probe_one() -------------------------------------------------------------

# Probe a single file. Returns list(container, streams) of raw-character tibbles
# (no `file` column, no type conversion; probe_all() adds those), or NULL if the
# file cannot be probed (missing path, unreachable URL, unreadable media).
#
# ONE FFprobe process per file, not `nb_streams + 1`. The container and every
# stream come back from a single call, because the compact writer delimits its
# sections by line and its fields by `|` -- where `default=nw=1` runs sections
# together with no delimiter at all, which is why the old code had to ask for
# one stream at a time and pay a process for each.
#
# The three writer options are pinned rather than left to their defaults
# because parse_compact_probe() depends on all three: `print_section=1` emits
# the leading `stream|`/`format|` field it dispatches on, `nokey=0` keeps the
# `key=value` form, and `escape=c` is the escaping it decodes.
probe_one <- function(file) {
  out <- run_program(
    find_ffprobe(),
    c("-i", file, "-v", "quiet", "-show_format", "-show_streams",
      "-of", "compact=print_section=1:nokey=0:escape=c"),
    program = "ffprobe"
  )
  parse_compact_probe(out)
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
#' These **FFprobe**-backed shortcuts return **tibbles**; the **MediaInfo**
#' readers (`mediainfo_*()`) and the scalar `get_*()` helpers are the
#' alternatives.
#'
#' @param probe A list object created by [probe_all()]. Must be `NULL` if
#'   `infile` is supplied.
#' @param infile A character vector of one or more media-file locations. Must be
#'   `NULL` if `probe` is supplied.
#' @param typed A logical passed to [probe_all()] when `infile` is used (default
#'   `TRUE`); ignored when `probe` is supplied.
#' @return A tibble containing only the requested information.
#' @seealso [probe_all()] for the full probe; [mediainfo_query()] for the
#'   MediaInfo backend; [get_width()] and friends for single scalar values.
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

# parse_compact_probe() ---------------------------------------------------

# Turn the output of ONE `ffprobe -show_format -show_streams -of compact` call
# into the same list(container, streams) of raw-character tibbles the old
# per-stream `-of default=nw=1` loop built, or NULL when the file could not be
# probed.
#
# The compact writer is what lets a single call carry every stream: it puts one
# section per LINE and escapes anything that could forge a line or field break,
# where `default=nw=1` concatenates sections with no delimiter and escapes
# nothing at all. That second half is not a hypothetical -- a tag value carrying
# a newline splits into further `key=value`-looking lines under the old writer,
# and the old parser duly turned the remainder into bogus columns.
#
# Three things this has to undo to keep the returned columns byte-identical to
# what the package has always returned:
#   - each line leads with a KEYLESS section field (`stream|...`, `format|...`),
#     and in a combined call the format line arrives LAST, so rows are dispatched
#     by that field rather than by position;
#   - fields are separated by `|`, which a value may also contain as `\|`;
#   - nested sections render as `tag:` / `disposition:` where `default=nw=1`
#     renders them `TAG:` / `DISPOSITION:`.
parse_compact_probe <- function(x) {
  x <- x[nzchar(x)]
  if (length(x) == 0) return(NULL)

  rows <- lapply(x, compact_row)
  section <- vapply(rows, function(r) r$section, character(1))

  fmt <- rows[section == "format"]
  # No format section means FFprobe told us nothing about the container, which
  # is what an unreadable file looks like; the old code reached the same NULL by
  # way of an empty `-show_format` call.
  if (length(fmt) == 0) return(NULL)

  st <- rows[section == "stream"]
  streams <- if (length(st)) {
    dplyr::bind_rows(lapply(st, function(r) r$data))
  } else {
    # A container FFprobe can read but that carries no streams. The old code
    # reached this through an `nb_streams < 1` early return.
    tibble::tibble()
  }
  list(container = fmt[[1]]$data, streams = streams)
}

# compact_row() -----------------------------------------------------------

# One compact line -> list(section, one-row tibble). The `key=value` split is on
# the *first* `=` only, exactly as the old parser did, so a value containing `=`
# survives; the writer does not escape `=`, so splitting before unescaping is
# safe.
compact_row <- function(line) {
  fields <- compact_fields(line)
  kv <- fields[-1]
  kv <- kv[nzchar(kv)]
  key <- compact_section_case(compact_unescape(sub("=.*$", "", kv)))
  value <- compact_unescape(sub("^[^=]*=", "", kv))
  names(value) <- key
  list(section = fields[[1]], data = tibble::as_tibble(as.list(value)))
}

# compact_fields() --------------------------------------------------------

# Split one compact line on its UNESCAPED `|` separators. A `|` is a separator
# only when an even number of backslashes precede it, since the writer spells a
# literal backslash `\\` and a literal `|` `\|`.
#
# Walking the characters is what makes that decidable. The obvious shortcut --
# substitute a placeholder byte for the escaped forms, split on what is left --
# has no safe placeholder here: the writer passes control characters through
# RAW (BEL, TAB and vertical tab were each measured arriving unescaped), so no
# byte is free to stand in for a separator without risking a collision with a
# value that genuinely contains it.
compact_fields <- function(line) {
  ch <- strsplit(line, "", fixed = TRUE)[[1]]
  n <- length(ch)
  if (n == 0) return("")
  # Position of the last non-backslash at or before each index; the count of
  # backslashes immediately preceding index i is then (i - 1) - last[i - 1].
  last <- cummax(ifelse(ch == "\\", 0L, seq_len(n)))
  run <- c(0L, seq_len(n - 1L) - last[-n])
  cuts <- which(ch == "|" & run %% 2L == 0L)
  starts <- c(1L, cuts + 1L)
  ends <- c(cuts - 1L, n)
  vapply(seq_along(starts), function(k) {
    if (ends[[k]] < starts[[k]]) "" else
      paste0(ch[starts[[k]]:ends[[k]]], collapse = "")
  }, character(1))
}

# compact_unescape() ------------------------------------------------------

# Decode the writer's C-style escapes. Measured on ffmpeg 8.1.2, the compact
# writer emits exactly six: `\\`, `\|`, `\n`, `\r`, `\b` and `\f`. The control
# characters BEL, TAB and vertical tab arrive raw and so need no decoding.
#
# The table carries `t`, `v` and `a` anyway, and every other `\X` decodes to
# `X`. That is not guesswork: since a literal backslash always arrives as `\\`,
# a lone `\` in the decoded input is impossible, so no mapping here can collide
# with real content -- a source value of backslash-then-t arrives as `\\t`, is
# consumed as the pair `\\`, and comes back as backslash-then-t either way.
#
# The pairs are decoded in ONE pass. A sequence of gsub() calls cannot do this:
# resolving `\\` first turns the literal backslash-n of `\\n` into a real `\n`,
# which the next gsub then reads as a newline escape.
compact_unescape <- function(x) {
  map <- c(n = "\n", r = "\r", f = "\f", b = "\b", t = "\t", v = "\v", a = "\a")
  vapply(x, function(s) {
    if (!grepl("\\", s, fixed = TRUE)) return(s)
    hits <- gregexpr("\\\\.", s, perl = TRUE)
    pairs <- regmatches(s, hits)[[1]]
    if (length(pairs) == 0) return(s)
    tail <- substr(pairs, 2L, 2L)
    regmatches(s, hits) <- list(unname(ifelse(tail %in% names(map),
                                              map[tail], tail)))
    s
  }, character(1), USE.NAMES = FALSE)
}

# compact_section_case() --------------------------------------------------

# Restore the nested-section prefix casing `default=nw=1` used, so the column
# names `probe_all()` returns are unchanged: `tag:title` -> `TAG:title`,
# `disposition:default` -> `DISPOSITION:default`. Uppercasing whatever precedes
# the first `:` covers any nested section rather than the two seen today.
compact_section_case <- function(key) {
  at <- regexpr(":", key, fixed = TRUE)
  ifelse(at > 0L,
         paste0(toupper(substr(key, 1L, at - 1L)), substring(key, at)),
         key)
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
