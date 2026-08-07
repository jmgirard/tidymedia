#' @details
#' tidymedia is organized in three layers. Layer 0 passes raw arguments to the
#' command-line tools: [ffmpeg()], [ffprobe()], and [mediainfo()]. Layer 1 is the
#' pipeline builder, which assembles an FFmpeg command step by step and then
#' compiles or runs it: [ffm()] and the `ffm_*()` functions. Layer 2 is the task
#' verbs, thin wrappers over the builder for common preprocessing jobs such as
#' [extract_audio()] and [segment_video()]. Container and stream metadata are read
#' as tibbles by [probe_all()] and the `get_*()` helpers.
#'
#' See `vignette("tidymedia")` for the guided tour, `vignette("batch")` for
#' running a verb over many files, and `vignette("metadata")` for the readers.
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
