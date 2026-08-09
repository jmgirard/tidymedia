
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
  limit <- resolve_timeout()
  out <- guard_timeout(
    "FFprobe", limit,
    system(glue('"{find_ffprobe()}" {command}'), intern = TRUE,
           timeout = limit)
  )
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
#' @param parallel A logical: probe the files in parallel with \pkg{furrr}
#'   (`TRUE`) or one at a time (`FALSE`, the default). The parallel path
#'   honors the active `future::plan()` and warns when that plan is
#'   sequential, since it would then give no speedup. Output is identical
#'   either way, rows included and in the same order. Requires the optional
#'   \pkg{furrr} package, which is checked for only when `parallel` is `TRUE`.
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
probe_all <- function(infile, typed = TRUE, parallel = FALSE) {
  if (!rlang::is_character(infile) || length(infile) == 0) {
    cli::cli_abort(
      "{.arg infile} must be a character vector of one or more file locations."
    )
  }
  rlang::check_bool(typed)
  rlang::check_bool(parallel)

  if (parallel) {
    rlang::check_installed("furrr", reason = "for parallel probing.")
    # probe_all() is a terminal entry point -- unlike loudnorm's Phase 1
    # fan-out, which leaves this warning to the ffm_batch() call that follows
    # it, nothing downstream warns for this one. Without the guard,
    # `parallel = TRUE` under the default sequential plan is a silent no-op,
    # which is the case D012 added the guard for (M53 T1).
    warn_if_sequential_plan()
  }

  # Only probe_one() shells out, so only probe_one() is worth fanning out; the
  # assembly below stays in this process. That is deliberate rather than
  # incidental -- the failure list and the single end-of-call warning are
  # parent-process state, and accumulating them inside workers would either
  # lose them or emit one warning per worker (AC3). Mapping over `infile` in
  # order also keeps the output rows in the INPUT vector's order, which is
  # what the preallocated `[[i]]` assignment used to guarantee.
  probes <- if (parallel) {
    furrr::future_map(infile, probe_one)
  } else {
    purrr::map(infile, probe_one)
  }

  containers <- vector("list", length(infile))
  streams_l <- vector("list", length(infile))
  failed <- character(0)

  for (i in seq_along(infile)) {
    f <- infile[[i]]
    res <- probes[[i]]
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
# narrow invocation rather than probe_all(), which reads every field of every
# stream and warns on an unreadable file: this needs a single number and must
# stay silent.
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
#' @param parallel A logical passed to [probe_all()] when `infile` is used:
#'   probe the files in parallel with \pkg{furrr} (`TRUE`) or one at a time
#'   (`FALSE`, the default). Ignored when `probe` is supplied, since a probe
#'   object has nothing left to probe.
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
probe_container <- function(probe = NULL, infile = NULL, typed = TRUE,
                            parallel = FALSE) {
  resolve_probe(probe, infile, typed, parallel)$container
}

# probe_streams() ---------------------------------------------------------

#' @rdname probe_container
#' @export
probe_streams <- function(probe = NULL, infile = NULL, typed = TRUE,
                          parallel = FALSE) {
  resolve_probe(probe, infile, typed, parallel)$streams
}

# probe_video() -----------------------------------------------------------

#' @rdname probe_container
#' @export
probe_video <- function(probe = NULL, infile = NULL, typed = TRUE,
                        parallel = FALSE) {
  filter_streams(resolve_probe(probe, infile, typed, parallel)$streams, "video")
}

# probe_audio() -----------------------------------------------------------

#' @rdname probe_container
#' @export
probe_audio <- function(probe = NULL, infile = NULL, typed = TRUE,
                        parallel = FALSE) {
  filter_streams(resolve_probe(probe, infile, typed, parallel)$streams, "audio")
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
resolve_probe <- function(probe, infile, typed, parallel = FALSE,
                          call = rlang::caller_env()) {
  if (is.null(probe) + is.null(infile) != 1) {
    cli::cli_abort(
      "Provide exactly one of {.arg probe} or {.arg infile}.", call = call
    )
  }
  # `parallel` is consumed exactly where `typed` is -- on the infile branch.
  # A probe object has nothing left to fan out, so both are ignored there.
  if (!is.null(infile)) {
    probe <- probe_all(infile, typed = typed, parallel = parallel)
  }
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
#   - nested sections carry a key prefix the old writer spelled differently, or
#     did not spell at all -- see compact_key_name().
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
#
# Every string operation from here down is byte-based (`useBytes = TRUE`). The
# separators and escapes are all ASCII, so byte matching is exactly equivalent
# on well-formed input, and on ill-formed input it is the difference between
# parsing the line and losing it: R's character-based string functions treat a
# line that is invalid in the session's `LC_CTYPE` as an error or an `NA`, which
# would drop a whole stream row for one unreadable metadata byte.
compact_row <- function(line) {
  fields <- compact_fields(line)
  kv <- fields[-1]
  kv <- kv[nzchar(kv)]
  key <- compact_key_name(compact_unescape(sub("=.*$", "", kv,
                                               useBytes = TRUE)))
  value <- compact_unescape(sub("^[^=]*=", "", kv, useBytes = TRUE))
  names(value) <- key
  list(section = fields[[1]], data = tibble::as_tibble(as.list(value)))
}

# compact_fields() --------------------------------------------------------

# Split one compact line on its UNESCAPED `|` separators. A `|` is a separator
# only when an even number of backslashes precede it, since the writer spells a
# literal backslash `\\` and a literal `|` `\|`.
#
# Split on every `|` first, then glue back the ones that were escaped. The
# obvious alternative -- substitute a placeholder for the escaped forms and
# split on what is left -- has no safe placeholder here: the writer passes
# control characters through RAW (BEL, TAB and vertical tab were each measured
# arriving unescaped), so no byte is free to stand in for a separator without
# risking a collision with a value that genuinely contains it.
compact_fields <- function(line) {
  raw <- charToRaw(line)
  if (length(raw) == 0) return("")
  pieces <- strsplit(line, "|", fixed = TRUE, useBytes = TRUE)[[1]]
  # strsplit() drops trailing empty fields, so `a|` comes back as one piece
  # where the line has two. Count the separators to put them back.
  n_sep <- sum(raw == as.raw(0x7C))
  if (length(pieces) < n_sep + 1L) {
    pieces <- c(pieces, rep("", n_sep + 1L - length(pieces)))
  }
  out <- character(0)
  cur <- pieces[[1]]
  for (i in seq_along(pieces)[-1]) {
    if (ends_in_odd_backslashes(cur)) {
      # The `|` that split these two was escaped: it belongs to the value.
      cur <- paste0(cur, "|", pieces[[i]])
    } else {
      out <- c(out, cur)
      cur <- pieces[[i]]
    }
  }
  c(out, cur)
}

# Does `s` end in an ODD number of backslashes -- i.e. would a `|` following it
# have been escaped? Counted over bytes, so a value carrying a byte invalid in
# the session locale is still measurable.
ends_in_odd_backslashes <- function(s) {
  raw <- charToRaw(s)
  n <- length(raw)
  if (n == 0 || raw[[n]] != as.raw(0x5C)) return(FALSE)
  other <- which(raw != as.raw(0x5C))
  run <- if (length(other) == 0) n else n - max(other)
  run %% 2L == 1L
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
    if (!grepl("\\", s, fixed = TRUE, useBytes = TRUE)) return(s)
    hits <- gregexpr("\\\\.", s, perl = TRUE, useBytes = TRUE)
    pairs <- regmatches(s, hits)[[1]]
    if (length(pairs) == 0) return(s)
    tail <- substr(pairs, 2L, 2L)
    regmatches(s, hits) <- list(unname(ifelse(tail %in% names(map),
                                              map[tail], tail)))
    s
  }, character(1), USE.NAMES = FALSE)
}

# compact_key_name() ------------------------------------------------------

# Give a nested-section key the name `default=nw=1` gave it, so the columns
# `probe_all()` returns are unchanged. The two writers disagree three ways, and
# only two of them are about casing:
#
#   tag:title                          -> TAG:title
#   disposition:default                -> DISPOSITION:default
#   side_datum/display_matrix:rotation -> rotation
#
# Side data is the one that bites. The old writer printed it with NO prefix at
# all, so `rotation` -- present on essentially every phone video, and read by
# anything that corrects orientation -- is a bare column name that users
# already depend on. Uppercasing the prefix instead of dropping it renamed that
# column out of existence, which is how M52's first review round found this.
# The prefix also varies by side-data type, so uppercasing scatters a mixed
# batch across per-type columns where it had one shared `rotation`.
#
# Those three are the whole nested-section set `-show_format -show_streams`
# emits, measured on ffmpeg 8.1.2. An unrecognized prefix is left ALONE rather
# than guessed at: a wrong rename is silent, where a compact-shaped name in the
# output is at least visible.
#
# The side-data pattern is deliberately wider than the one spelling measured
# here. FFprobe builds the prefix from the section's own name and an optional
# type slug, and both have moved across versions: an older writer emits the key
# bare (nothing to strip, and the `^` anchor means nothing is), 8.1.2 emits
# `side_datum/display_matrix:`, and the stem has been spelled `side_data` too.
# Matching the stem plus anything up to the first `:` covers all of them, and
# cannot over-reach: FFprobe's own field names carry no `:`, so only a nested
# section can match at all.
#
# Byte-based, like everything else in this parser: a metadata key carrying a
# byte invalid in the session locale must not take its row down with it.
compact_key_name <- function(key) {
  key <- sub("^tag:", "TAG:", key, useBytes = TRUE)
  key <- sub("^disposition:", "DISPOSITION:", key, useBytes = TRUE)
  sub("^side_dat(a|um)[^:]*:", "", key, useBytes = TRUE)
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
