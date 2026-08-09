#' @details
#' tidymedia is organized in three layers. Layer 0 passes raw arguments to the
#' command-line tools: [ffmpeg()], [ffprobe()], and [mediainfo()]. Layer 1 is the
#' pipeline builder, which assembles an FFmpeg command step by step and then
#' compiles or runs it: [ffm()] and the `ffm_*()` functions. Layer 2 is the task
#' verbs, thin wrappers over the builder for common preprocessing jobs such as
#' [extract_audio()] and [segment_video()]. Metadata is read by two independent
#' back ends: FFprobe, through [probe_all()] and the other `probe_*()` readers,
#' which return container and stream tibbles; and MediaInfo, through
#' [mediainfo_query()] and the `get_*()` helpers such as [get_duration()], which
#' return a single value.
#'
#' @section Bounding a run that hangs:
#' Every tidymedia call that touches FFmpeg, FFprobe or MediaInfo waits for
#' that program to finish, and a program that hangs blocks the R session with
#' it. Set a wall-clock limit, in whole seconds, to bound the wait:
#'
#' \preformatted{options(tidymedia.timeout = 600)}
#'
#' A call that reaches the limit aborts, naming the program and the limit;
#' where the run had already written part of its output, that partial file is
#' removed just as it is after any other failed run. The default is `0`, which
#' means no limit — every call waits as long as the program takes, which is
#' what you want for a legitimate multi-hour encode. Fractional values are
#' refused rather than rounded, because the underlying limit is whole seconds
#' and a value below one second would otherwise be read as no limit at all.
#'
#' The limit applies per spawned program, not per batch: a 100-row batch with a
#' 600-second limit bounds each row at 600 seconds. It is also read in the
#' process that sets it, so under `parallel = TRUE` the worker processes do not
#' see it.
#'
#' See `vignette("tidymedia")` for the guided tour, `vignette("batch")` for
#' running a verb over many files, `vignette("metadata")` for the readers, and
#' `vignette("workflow")` for an end-to-end research preprocessing pipeline.
#' The full function list is on the package's reference index.
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom glue glue
## usethis namespace: end
NULL
