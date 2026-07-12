# ffm_manifest() ----------------------------------------------------------

#' Read a Batch Provenance Manifest
#'
#' Retrieve the reproducibility manifest recorded by [ffm_batch()] when it was
#' run with \code{manifest = TRUE}. The manifest is a tibble with one row per
#' job capturing the compiled command, the FFmpeg and FFprobe versions used, a
#' run timestamp, the input and output paths, and the output file size — plus
#' input/output md5 checksums when the batch was run with
#' \code{checksums = TRUE}. Together with the reproducible command this is what
#' turns a batch run into an auditable record.
#'
#' @param x A tibble returned by [ffm_batch()] with \code{manifest = TRUE}.
#' @param path An optional file path. When supplied, the manifest is also
#'   written there as CSV (via \code{utils::write.csv}, no row names) and
#'   returned invisibly.
#' @return The manifest [tibble][tibble::tibble-package]; invisibly when
#'   \code{path} is written.
#' @family verification functions
#' @seealso [ffm_batch()], which records the manifest.
#' @examplesIf nzchar(Sys.which("ffmpeg"))
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(input = video, output = tempfile(fileext = ".mp3"))
#' res <- ffm_batch(jobs, manifest = TRUE, .f = function(input, output, ...) {
#'   ffm_files(input, output) |> ffm_drop("video")
#' })
#' ffm_manifest(res)
#' @export
ffm_manifest <- function(x, path = NULL) {
  man <- attr(x, "manifest")
  if (is.null(man)) {
    cli::cli_abort(c(
      "No provenance manifest is attached to {.arg x}.",
      "i" = "Run the batch with {.code ffm_batch(..., manifest = TRUE)} first."
    ))
  }
  if (!is.null(path)) {
    rlang::check_string(path)
    utils::write.csv(man, path, row.names = FALSE)
    return(invisible(man))
  }
  man
}

# manifest_schema() -------------------------------------------------------

# The canonical, zero-row manifest template: the single source of truth for the
# provenance manifest's column names, order, and types. build_manifest() fills
# it for the rows that ran; the two-pass batch's all-silent path pads it (no row
# ever runs) so its result carries the same schema as a mixed batch (D011). The
# `checksums` flag toggles the optional md5 columns, matching build_manifest().
manifest_schema <- function(checksums = FALSE) {
  schema <- tibble::tibble(
    command = character(0),
    input = character(0),
    output = character(0),
    output_size = numeric(0),
    ffmpeg_version = character(0),
    ffprobe_version = character(0),
    timestamp = character(0)
  )
  if (checksums) {
    schema$input_md5 <- character(0)
    schema$output_md5 <- character(0)
  }
  schema
}

# build_manifest() --------------------------------------------------------

# Assemble the per-job manifest tibble from the batch's pipeline objects,
# compiled commands, and captured tool versions. Kept free of any binary calls
# (versions are passed in) so the assembly and checksum logic is CI-testable;
# ffm_batch() supplies `versions` from tool_versions() at run time. Multiple
# inputs (e.g. a stacked job) are joined with ";" in one cell. The column set is
# taken from manifest_schema() so the populated and empty-template paths cannot
# drift apart.
build_manifest <- function(pipelines, commands, versions, checksums) {
  inputs <- vapply(
    pipelines, function(p) paste(p$input, collapse = ";"), character(1)
  )
  outputs <- vapply(pipelines, function(p) p$output, character(1))
  sizes <- as.numeric(file.size(outputs))

  cols <- list(
    command = commands,
    input = inputs,
    output = outputs,
    output_size = sizes,
    ffmpeg_version = versions$ffmpeg,
    ffprobe_version = versions$ffprobe,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  if (checksums) {
    cols$input_md5 <- vapply(
      pipelines,
      function(p) paste(unname(tools::md5sum(p$input)), collapse = ";"),
      character(1)
    )
    cols$output_md5 <- unname(tools::md5sum(outputs))
  }

  # Order and gate columns by the canonical schema (scalar version columns are
  # recycled to one row per job by tibble()).
  tibble::tibble(!!!cols[names(manifest_schema(checksums))])
}

# tool_versions() ---------------------------------------------------------

# Capture the FFmpeg and FFprobe version strings for the manifest. Returns a
# named list with NA for a binary that cannot be located or queried. The binary
# call lives here; parsing is factored into parse_version_line() so it can be
# tested without a binary.
tool_versions <- function() {
  list(
    ffmpeg = capture_version(find_ffmpeg(), "ffmpeg"),
    ffprobe = capture_version(find_ffprobe(), "ffprobe")
  )
}

# capture_version() -------------------------------------------------------

# Run `<tool> -version` and parse the version token; NA if the binary is absent
# or the call fails.
capture_version <- function(loc, name) {
  if (is.null(loc) || is.na(loc) || !nzchar(loc)) return(NA_character_)
  out <- tryCatch(
    run_program(loc, "-version", program = name),
    error = function(e) NULL
  )
  parse_version_line(out)
}

# parse_version_line() ----------------------------------------------------

# Pull the version token out of a `<tool> -version` first line, e.g.
# "ffmpeg version 8.1.2 ..." -> "8.1.2". NA when there is nothing to parse.
parse_version_line <- function(lines) {
  if (is.null(lines) || !length(lines)) return(NA_character_)
  m <- regmatches(lines[[1]], regexpr("version \\S+", lines[[1]]))
  if (length(m)) sub("^version ", "", m) else NA_character_
}
