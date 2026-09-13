#' @description
#' tidymedia prepares audio and video files for research. It trims, crops,
#' converts and standardizes files, and reads file details back as tibbles. It
#' runs the programs FFmpeg and MediaInfo. Start with `vignette("tidymedia")`.
#'
#' @details
#' Task functions, such as [extract_audio()], do one common job in one call.
#' Pipeline functions, such as [ffm_files()], build an FFmpeg command one step
#' at a time. Direct commands, [ffmpeg()], [ffprobe()] and [mediainfo()], pass
#' your own arguments to the programs. [probe_all()] and [get_duration()] read
#' file details.
#'
#' See `vignette("tidymedia")` for the guided tour and a glossary of media
#' terms. The other vignettes are `"batch"`, `"metadata"`, `"verification"` and
#' `"workflow"`.
#'
#' @section Session options:
#' A new option value takes effect at the next call. The workers of a
#' `parallel = TRUE` run use your session's values.
#'
#' * `options(tidymedia.timeout = 600)` sets a limit, in whole seconds, on each
#'   program the package starts. The default, `0`, means no limit. See
#'   [with_timeout()].
#' * `options(tidymedia.check_tracks = FALSE)` turns off the dropped-track
#'   warning of [extract_audio()], [convert_audio()], [normalize_audio()] and
#'   their `_batch` forms. The check runs one FFprobe call for each different
#'   input, when `run = TRUE` and the call or job row names no `audio_stream`.
#'   Turning it off skips those calls. A value other than `TRUE` or `FALSE`
#'   gives an error that names the option, when the check would run.
#' * `options(tidymedia.hardware_encoders = "h264_nvenc")` names the hardware
#'   video encoders of this computer, so the package does not ask FFmpeg.
#'   `character(0)` means none.
#'
#' @section Errors when FFmpeg fails:
#' * `tidymedia_ffmpeg_exit`: FFmpeg exited non-zero in [ffm_run()], or in the
#'   `loudnorm` analysis pass of `normalize_audio(two_pass = TRUE)`. The task
#'   functions that run their command with [ffm_run()], such as
#'   [extract_audio()] and [separate_audio_video()], give it too. The
#'   `tm_status` field holds the exit status.
#' * `tidymedia_loudnorm_no_measurement`: the two-pass analysis gave no
#'   measurement. In the `_batch` form, `tm_rows` holds the rows, and
#'   `tm_row_status` holds each exit status or `NA`.
#' * `tidymedia_multitrack_separation`: [separate_audio_video()] could not
#'   write several audio tracks into one audio file. The `_batch` form gives a
#'   warning.
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom glue glue
## usethis namespace: end
NULL
