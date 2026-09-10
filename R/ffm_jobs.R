# ffm_jobs() --------------------------------------------------------------

#' Build a Jobs Table From a Directory
#'
#' List the media files in a directory and return them as the jobs table
#' [ffm_batch()] takes: a tibble with one row per file and an \code{input}
#' column of full paths. This is the batch entry point's companion — the batch
#' story starts here rather than with a hand-rolled \code{list.files()} call.
#'
#' The returned tibble carries \code{input} and nothing else, deliberately:
#' [ffm_batch()] passes every column of the jobs table to \code{.f} by name, so
#' an extra column would become an argument every \code{.f} has to accept. Add
#' the columns your pipeline needs with the usual data-frame tools — some
#' \code{*_batch()} verbs want an \code{output} column, others a
#' task-specific one such as \code{start} and \code{end} — as the examples
#' below derive an \code{output} from \code{input}.
#'
#' @param directory A single string naming an existing directory.
#' @param type The media category to list: \code{"video"}, \code{"audio"}, or
#'   \code{"image"}. Required — it has no default, since any default would be
#'   one of the three (D079).
#' @param extension An optional character vector of file extensions narrowing
#'   the search within \code{type}, with or without a leading dot
#'   (\code{"mp4"} and \code{".mp4"} both work). Each must be one of the
#'   extensions \code{type} covers; the refusal lists them. \code{NULL} (the
#'   default) lists every extension of that type.
#' @param recursive A logical: descend into subdirectories (\code{TRUE}) or
#'   list only the top level (\code{FALSE}, default).
#' @return A [tibble][tibble::tibble-package] with one row per matching file
#'   and a single character column, \code{input}, holding each file's full
#'   path. Rows are in the order \code{\link[base]{list.files}} returns them.
#'   Every row is a path that exists and is not a directory: a subdirectory
#'   whose own name ends in a listed extension is never a row, nor — on macOS
#'   and Linux — is a symbolic link whose target is gone. Windows reports such
#'   a link as existing, so there it can still be a row. The call aborts rather
#'   than returning zero rows when nothing matches.
#' @family builder functions
#' @seealso [ffm_batch()], which consumes the returned table.
#' @examples
#' folder <- system.file("extdata", package = "tidymedia")
#' jobs <- ffm_jobs(folder, type = "video")
#' jobs
#'
#' # Derive an output column, then hand the whole table to ffm_batch().
#' jobs$output <- file.path(tempdir(), paste0(
#'   tools::file_path_sans_ext(basename(jobs$input)), ".mp3"
#' ))
#' ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
#'   ffm_files(input, output) |> ffm_drop("video")
#' })
#' @export
ffm_jobs <- function(directory, type, extension = NULL, recursive = FALSE) {
  rlang::check_required(type)
  tm_ffm_jobs(
    directory = directory, type = type, extension = extension,
    recursive = recursive, call = rlang::current_env()
  )
}

# tm_ffm_jobs() -----------------------------------------------------------

# The body, blamed on whichever front door called it. D087: `call` is threaded
# through an internal implementation and never published as an exported formal.
# `type` is checked for presence in the wrapper, at the front door whose frame
# the refusal names.
tm_ffm_jobs <- function(directory, type, extension, recursive, call) {
  rlang::check_string(directory, arg = "directory", call = call)
  rlang::check_bool(recursive, arg = "recursive", call = call)
  # check_string() before arg_match(): arg_match() reduces a multi-element
  # `arg` to its first element without complaint whenever `arg` is setequal()
  # to `values` — any permutation of the full set, not only the identical one —
  # so a multi-value `type` would otherwise be accepted silently.
  rlang::check_string(type, arg = "type", call = call)
  type <- rlang::arg_match(type, media_types(), error_call = call)

  if (!dir.exists(directory)) {
    cli::cli_abort(
      "{.arg directory} does not name an existing directory: {.file {directory}}.",
      call = call
    )
  }

  wanted <- media_extensions(type)
  if (!is.null(extension)) {
    if (!rlang::is_character(extension) || length(extension) == 0 ||
        anyNA(extension) || !all(nzchar(extension))) {
      cli::cli_abort(
        "{.arg extension} must be a character vector of one or more file extensions.",
        call = call
      )
    }
    given <- tolower(sub("^\\.", "", extension))
    unknown <- setdiff(given, wanted)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "{.arg extension} must name {type} extensions.",
        "x" = "{.val {unknown}} {?is not one of them/are not among them}.",
        "i" = "Accepted for this type: {.val {wanted}}."
      ), call = call)
    }
    wanted <- unique(given)
  }

  pattern <- paste0("\\.(", paste(wanted, collapse = "|"), ")$")
  files <- list.files(
    directory, pattern = pattern, full.names = TRUE,
    recursive = recursive, ignore.case = TRUE
  )
  # list.files() yields names, not readable files: a subdirectory whose own
  # name ends in a listed extension (when `recursive = FALSE`), and a symbolic
  # link whose target is gone. Keep only what is both there and not a
  # directory, rather than subtracting the non-file shapes one at a time --
  # file.exists() follows links, so a dangling one is already FALSE, and it is
  # also what keeps normalizePath() from passing an unresolvable path through
  # unchanged. Dropping directories is additionally what makes
  # `recursive = TRUE` a superset of `recursive = FALSE` rather than a
  # different set (list.files(recursive = TRUE) omits directories already).
  files <- files[file.exists(files) & !dir.exists(files)]
  if (length(files) == 0) {
    scope <- if (recursive) " or its subdirectories" else ""
    cli::cli_abort(c(
      "No {type} files were found in {.file {directory}}{scope}.",
      "i" = "Looked for these extensions: {.val {wanted}}."
    ), call = call)
  }

  tibble::tibble(
    input = normalizePath(files, winslash = "/", mustWork = FALSE)
  )
}

# media_types() / media_extensions() --------------------------------------

# The closed vocabulary `type` ranges over, and the extensions each category
# covers. Closed by choice (GP1/D001): a fixed set is what makes the "type
# outside the accepted set" and "extension outside the type" refusals name what
# they accept. A caller with a container outside these lists uses list.files()
# directly, as they did before this export existed.
media_types <- function() c("video", "audio", "image")

media_extensions <- function(type) {
  switch(
    type,
    video = c("mp4", "mov", "mkv", "avi", "m4v", "webm", "mpg", "mpeg",
              "wmv", "flv", "mts", "m2ts"),
    audio = c("wav", "mp3", "m4a", "aac", "flac", "ogg", "oga", "opus",
              "wma", "aiff", "aif"),
    image = c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "gif", "webp")
  )
}
