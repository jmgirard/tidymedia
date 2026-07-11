# ffm_batch() ------------------------------------------------------------------

#' Run an FFmpeg Pipeline Over Many Files
#'
#' Apply a pipeline-building function to every row of a jobs table and compile
#' (and optionally run) the resulting FFmpeg command for each. This is
#' tidymedia's batch-processing entry point: one reproducible compiled command
#' per job, collected back into a tibble.
#'
#' Each column of \code{jobs} is passed by name to \code{.f} (as
#' [purrr::pmap()] does), so a job table with columns \code{input}, \code{output}
#' and \code{start} calls \code{.f(input = ..., output = ..., start = ...)}.
#' \code{.f} must return an ffm pipeline (see \code{\link{ffm_files}}). Give
#' \code{.f} a \code{...} argument if \code{jobs} carries columns it does not
#' use.
#'
#' @param jobs A data frame with one row per job. Its column names are the
#'   arguments passed to \code{.f}.
#' @param .f A function that takes a job's columns (by name) and returns an ffm
#'   pipeline object.
#' @param ... Additional arguments passed on to every call of \code{.f}.
#' @param run A logical: run each compiled command through FFmpeg (\code{TRUE},
#'   default) or only compile them for inspection (\code{FALSE}, a dry run).
#' @param parallel A logical: map over jobs in parallel with \pkg{furrr}
#'   (\code{TRUE}) or sequentially (\code{FALSE}, default). Parallelism follows
#'   the \code{\link[future:plan]{future}} plan the caller has set.
#' @return \code{jobs} as a [tibble][tibble::tibble-package] with an added
#'   \code{command} column (the compiled FFmpeg command for each job) and, when
#'   \code{run = TRUE}, a logical \code{success} column.
#' @seealso \code{\link{segment_video}}, which is built on \code{ffm_batch()}.
#' @family builder functions
#' @examples
#' video <- system.file("extdata", "sample.mp4", package = "tidymedia")
#' jobs <- tibble::tibble(
#'   input  = c(video, video),
#'   output = c("a.mp3", "b.mp3")
#' )
#' # run = FALSE compiles one reproducible command per job without calling FFmpeg
#' ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
#'   ffm_files(input, output) |>
#'     ffm_drop("video") |>
#'     ffm_codec(audio = "libmp3lame")
#' })
#' @export
ffm_batch <- function(jobs, .f, ..., run = TRUE, parallel = FALSE) {

  if (!is.data.frame(jobs)) {
    cli::cli_abort("{.arg jobs} must be a data frame with one row per job.")
  }
  if (nrow(jobs) == 0) {
    cli::cli_abort("{.arg jobs} must have at least one row.")
  }
  if (!is.function(.f)) {
    cli::cli_abort("{.arg .f} must be a function returning an ffm pipeline.")
  }
  rlang::check_bool(run)
  rlang::check_bool(parallel)
  if (parallel) {
    rlang::check_installed("furrr", reason = "for parallel batch processing.")
  }

  # Build one pipeline per row (columns passed to .f by name, pmap-style).
  pipelines <- if (parallel) {
    furrr::future_pmap(jobs, .f, ...)
  } else {
    purrr::pmap(jobs, .f, ...)
  }

  not_ffm <- !vapply(pipelines, inherits, logical(1), what = "tidymedia_ffm")
  if (any(not_ffm)) {
    cli::cli_abort(c(
      "{.arg .f} must return an ffm pipeline for every job.",
      "x" = "Job{?s} {.val {which(not_ffm)}} returned something else."
    ))
  }

  out <- tibble::as_tibble(jobs)
  commands <- vapply(pipelines, ffm_compile, character(1))
  out$command <- commands

  if (run) {
    # Execute each pipeline's argument vector (shell-free, hostile-path-safe;
    # M06) rather than the display string. ffm_run()'s result carries a
    # "status" attribute on a non-zero exit; a hard failure throws.
    run_one <- function(pipeline) {
      res <- tryCatch(ffm_run(pipeline), error = function(e) e)
      !inherits(res, "error") && is.null(attr(res, "status"))
    }
    out$success <- if (parallel) {
      unlist(furrr::future_map(pipelines, run_one))
    } else {
      vapply(pipelines, run_one, logical(1))
    }
  }

  out
}
