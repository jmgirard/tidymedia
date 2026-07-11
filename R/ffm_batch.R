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
#' @param verify An optional output check applied to each job (only when
#'   \code{run = TRUE}). Either a named list of expected properties (the same
#'   spec for every job, e.g. \code{list(width = 1920)}) or a function of the
#'   job columns (called \code{\link[purrr:pmap]{pmap}}-style, like \code{.f})
#'   that returns such a list per job. Each job's output is passed to
#'   \code{\link{verify_media}}; unlike \code{\link{ffm_run}}, a failed check is
#'   \emph{recorded}, not aborted. Adds a logical \code{verified} column (all
#'   checks passed), \code{NA} for jobs that did not run successfully.
#' @param progress A logical: display a \pkg{cli} progress bar as the jobs run
#'   (\code{TRUE}) or run quietly (\code{FALSE}, default). Only applies when
#'   \code{run = TRUE}; safe (a no-op animation) in non-interactive sessions.
#' @param manifest A logical: when \code{TRUE} (and \code{run = TRUE}), record a
#'   provenance manifest (per-job command, FFmpeg/FFprobe versions, timestamp,
#'   output size) and attach it to the result, readable with
#'   \code{\link{ffm_manifest}}. (default = \code{FALSE})
#' @param checksums A logical: when \code{TRUE}, the manifest also captures md5
#'   checksums of each job's input(s) and output. Ignored unless
#'   \code{manifest = TRUE}. (default = \code{FALSE})
#' @return \code{jobs} as a [tibble][tibble::tibble-package] with an added
#'   \code{command} column (the compiled FFmpeg command for each job) and, when
#'   \code{run = TRUE}, a logical \code{success} column (plus a \code{verified}
#'   column when \code{verify} is supplied). When \code{manifest = TRUE} a
#'   provenance manifest is attached as an attribute; read it with
#'   \code{\link{ffm_manifest}}.
#' @seealso \code{\link{segment_video}}, which is built on \code{ffm_batch()};
#'   \code{\link{verify_media}} for the verification spec and
#'   \code{\link{ffm_manifest}} for the provenance manifest.
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
ffm_batch <- function(jobs, .f, ..., run = TRUE, parallel = FALSE,
                      verify = NULL, progress = FALSE, manifest = FALSE,
                      checksums = FALSE) {

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
  rlang::check_bool(progress)
  rlang::check_bool(manifest)
  rlang::check_bool(checksums)
  if (!is.null(verify) && !is.function(verify) &&
      !(rlang::is_list(verify) && rlang::is_named(verify))) {
    cli::cli_abort(
      "{.arg verify} must be a named list or a function of the job columns."
    )
  }
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
      # furrr drives its own progress reporting over the parallel workers.
      unlist(furrr::future_map(pipelines, run_one, .progress = progress))
    } else if (progress) {
      run_with_progress(pipelines, run_one)
    } else {
      vapply(pipelines, run_one, logical(1))
    }

    # Verification records outcomes (never aborts): resolve one spec per job
    # (a shared named list, or .f-style from the job columns) and collapse each
    # job's checks to a single logical, NA for jobs that did not run cleanly.
    if (!is.null(verify)) {
      specs <- resolve_batch_verify(verify, jobs, ...)
      out$verified <- vapply(seq_along(pipelines), function(i) {
        if (!isTRUE(out$success[[i]])) return(NA)
        report <- do.call(
          verify_media, c(list(file = pipelines[[i]]$output), specs[[i]])
        )
        all(report$pass)
      }, logical(1))
    }

    # Provenance manifest (opt-in): capture tool versions once and assemble a
    # per-job record, attached to the result for ffm_manifest() to read.
    if (manifest) {
      attr(out, "manifest") <- build_manifest(
        pipelines, out$command, tool_versions(), checksums
      )
    }
  }

  out
}

# run_with_progress() -----------------------------------------------------

# Run the pipelines sequentially behind a cli progress bar, returning the
# per-job success logical (same contract as the plain vapply path). The bar is
# owned by this function's frame, so it is cleaned up on return; cli renders a
# no-op in non-interactive sessions, so this never errors under `R CMD check`.
run_with_progress <- function(pipelines, run_one) {
  n <- length(pipelines)
  successes <- logical(n)
  cli::cli_progress_bar("Running FFmpeg jobs", total = n)
  for (i in seq_len(n)) {
    successes[[i]] <- run_one(pipelines[[i]])
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  successes
}

# resolve_batch_verify() --------------------------------------------------

# Turn a batch `verify` argument into one spec (named list) per job: a bare
# named list is reused for every job; a function is applied to the job columns
# pmap-style (mirroring .f), forwarding `...`. Each resolved spec must itself be
# a named list.
resolve_batch_verify <- function(verify, jobs, ...) {
  specs <- if (is.function(verify)) {
    purrr::pmap(jobs, verify, ...)
  } else {
    rep(list(verify), nrow(jobs))
  }
  ok <- vapply(
    specs,
    function(s) rlang::is_list(s) && (length(s) == 0 || rlang::is_named(s)),
    logical(1)
  )
  if (!all(ok)) {
    cli::cli_abort("Each {.arg verify} spec must be a named list.")
  }
  specs
}
