# verify_media() ----------------------------------------------------------

#' Verify a Media File Against Expected Properties
#'
#' Probe a media file and check its structural metadata against a set of
#' expectations, returning a tidy pass/fail tibble with one row per checked
#' property. This turns "the command is reproducible" into "the *result* is what
#' I asked for": use it after an encode to confirm the output really has the
#' duration, dimensions, codecs, and so on that the pipeline was meant to
#' produce.
#'
#' Checks are structural (drawn from FFprobe metadata), not perceptual: this
#' does not measure visual or audio quality. The named arguments cover the most
#' common properties; pass any other FFprobe field by name through `...` (for
#' example `pix_fmt = "yuv420p"` or `bit_rate = 141800`). Extra names are
#' resolved against the probe columns in the order container, then video stream,
#' then audio stream, and the first match wins.
#'
#' Numeric checks pass when `abs(actual - expected) <= tolerance`; with the
#' default `tolerance` of `0.1` this means integer properties (width, height,
#' sample rate) must match exactly while `duration` is allowed a little slack
#' (e.g. for keyframe-snapped cuts). String checks (the codecs) must match
#' exactly. A property whose stream or column is absent yields an `NA` actual
#' value and a failing check.
#'
#' @param file A string naming a single media file to verify.
#' @param duration Expected container duration in seconds (numeric).
#' @param width,height Expected video-frame dimensions in pixels (numeric).
#' @param video_codec,audio_codec Expected codec names for the first video and
#'   audio stream (strings, e.g. `"h264"`, `"aac"`).
#' @param sample_rate Expected audio sample rate in Hz (numeric).
#' @param ... Further expectations given as `name = value`, each checked against
#'   the FFprobe field of that name (container first, then video, then audio).
#' @param tolerance The absolute tolerance for numeric checks (default `0.1`).
#' @return A [tibble][tibble::tibble-package] with one row per checked property
#'   and columns `file`, `check`, `expected`, `actual`, and `pass` (logical).
#' @family verification functions
#' @seealso [ffm_run()] and [ffm_batch()], which accept a `verify =` spec.
#' @examplesIf nzchar(Sys.which("ffprobe"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' verify_media(video, width = 320, height = 240, video_codec = "h264")
#' @export
verify_media <- function(file,
                         duration = NULL,
                         width = NULL,
                         height = NULL,
                         video_codec = NULL,
                         audio_codec = NULL,
                         sample_rate = NULL,
                         ...,
                         tolerance = 0.1) {

  check_file_exists(file)
  rlang::check_number_decimal(tolerance, min = 0)

  dots <- rlang::list2(...)
  if (length(dots) && !rlang::is_named(dots)) {
    cli::cli_abort("Every expectation passed via {.arg ...} must be named.")
  }

  # Blessed named checks, in a stable order, dropping the ones left NULL.
  named <- list(
    duration    = duration,
    width       = width,
    height      = height,
    video_codec = video_codec,
    audio_codec = audio_codec,
    sample_rate = sample_rate
  )
  named <- named[!vapply(named, is.null, logical(1))]

  expected <- c(named, dots)
  if (!length(expected)) {
    cli::cli_abort(c(
      "Provide at least one property to check.",
      "i" = 'e.g. {.code verify_media(file, width = 1920, video_codec = "h264")}.'
    ))
  }

  # Probe once; the shortcuts would reprobe per call.
  probe <- probe_all(file)
  container <- probe$container
  video <- filter_streams(probe$streams, "video")
  audio <- filter_streams(probe$streams, "audio")

  # Fixed probe location for each blessed check: (tibble, column).
  sources <- list(
    duration    = list(container, "duration"),
    width       = list(video, "width"),
    height      = list(video, "height"),
    video_codec = list(video, "codec_name"),
    audio_codec = list(audio, "codec_name"),
    sample_rate = list(audio, "sample_rate")
  )

  actual <- stats::setNames(vector("list", length(expected)), names(expected))
  for (nm in names(expected)) {
    if (nm %in% names(sources)) {
      actual[[nm]] <- pull_probe(sources[[nm]][[1L]], sources[[nm]][[2L]])
    } else {
      # `...` extra: search container -> video -> audio by column name.
      actual[[nm]] <- pull_probe_any(list(container, video, audio), nm)
    }
  }

  res <- compare_expectations(expected, actual, tolerance = tolerance)
  tibble::add_column(res, file = file, .before = 1)
}

# compare_expectations() --------------------------------------------------

# Pure, binary-free comparison core: given aligned named lists of `expected`
# and `actual` property values, return the tidy report tibble (one row per
# check: `check`, `expected`, `actual`, `pass`). Numeric expectations pass when
# the actual is within `tolerance` (absolute); everything else compares as an
# exact string. A missing actual (length-0 or NA) always fails. Kept separate
# from verify_media() so the comparison logic is testable without FFprobe.
compare_expectations <- function(expected, actual, tolerance = 0.1) {
  checks <- names(expected)
  n <- length(checks)
  pass <- logical(n)
  exp_chr <- character(n)
  act_chr <- character(n)

  for (i in seq_len(n)) {
    e <- expected[[i]]
    a <- actual[[i]]
    missing <- length(a) == 0 || (length(a) == 1 && is.na(a))
    exp_chr[[i]] <- format_check_value(e)
    act_chr[[i]] <- if (missing) NA_character_ else format_check_value(a)

    if (is.numeric(e)) {
      a_num <- suppressWarnings(as.numeric(a))
      pass[[i]] <- !missing && length(a_num) == 1 && !is.na(a_num) &&
        abs(a_num - e) <= tolerance
    } else {
      pass[[i]] <- !missing &&
        identical(as.character(a), as.character(e))
    }
  }

  tibble::tibble(check = checks, expected = exp_chr, actual = act_chr,
                 pass = pass)
}

# format_check_value() ----------------------------------------------------

# Render a scalar expectation/actual value as a single display string for the
# report tibble. A length-0 or NA value becomes NA_character_.
format_check_value <- function(x) {
  if (length(x) == 0 || (length(x) == 1 && is.na(x))) return(NA_character_)
  paste0(as.character(x), collapse = ", ")
}

# pull_probe() ------------------------------------------------------------

# Read a single scalar from the first row of a probe tibble, or NA when the
# tibble has no rows or lacks that column (a missing stream/property).
pull_probe <- function(tbl, col) {
  if (nrow(tbl) == 0 || !col %in% names(tbl)) return(NA)
  tbl[[col]][[1L]]
}

# pull_probe_any() --------------------------------------------------------

# Resolve an extra (`...`) check by column name across several probe tibbles in
# priority order (container, then video, then audio); first match wins, NA if
# none carries the column.
pull_probe_any <- function(tbls, col) {
  for (tbl in tbls) {
    if (nrow(tbl) > 0 && col %in% names(tbl)) return(tbl[[col]][[1L]])
  }
  NA
}
