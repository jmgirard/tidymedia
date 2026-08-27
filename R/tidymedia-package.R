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
#' A reached limit is never silent: every call that can start one of those
#' programs either aborts or warns.
#'
#' It **aborts**, naming the program and the limit, from the task verbs,
#' [ffm_run()], and the Layer 0 escape hatches [ffmpeg()], [ffprobe()] and
#' [mediainfo()]. [verify_media()] aborts as well, because a probe that never
#' answered is not an answer of "no".
#'
#' It **warns** everywhere a single hung file must not discard the rest of the
#' work. [probe_all()] and the `probe_*()` accessors, [mediainfo_parameter()],
#' [mediainfo_query()], [mediainfo_template()] and the `get_*()` helpers give
#' an `NA` row and one warning at the end of the call, saying how many of the
#' files it names timed out rather than being unreadable. [ffm_batch()] and the
#' `_batch` verbs mark the row `success = FALSE`, as they do for any failed
#' job, and warn once at the end of the run saying how many jobs the limit
#' killed — at `parallel = TRUE` the workers never see the limit, so nothing
#' there reaches it to warn about (see below). The dropped-track check behind
#' [extract_audio()], [convert_audio()], [separate_audio_video()] and their
#' `_batch` siblings warns that it could not check, and the provenance manifest
#' warns that it could not read a version; both then carry on as they would for
#' any other unreadable input.
#'
#' To handle either outcome programmatically, the abort carries the condition
#' class `tidymedia_timeout`; the dropped-track and version-probe warnings
#' carry `tidymedia_probe_timeout`, and the batch warning
#' `tidymedia_batch_timeout`. So the documented recipe for silencing the
#' dropped-track check, `suppressWarnings(classes = "tidymedia_dropped_audio")`,
#' silences only the check itself — add `"tidymedia_probe_timeout"` to also
#' silence the notice that the limit stopped it from running.
#'
#' Those two lists are not written from memory. A test derives the calls that
#' can start one of these programs from the package's own call graph and drives
#' a timeout through each of them, so a call that started absorbing one
#' silently would fail the package's own checks.
#'
#' For the task verbs and [ffm_run()], which know where their output goes, a
#' partial file the killed run had written is removed just as it is after any
#' other failed run. The raw [ffmpeg()] escape hatch is handed an argument
#' string it does not parse, so it cannot tell which of those arguments is the
#' output and leaves whatever the killed run wrote in place — check the output
#' of a timed-out [ffmpeg()] call yourself. The default is `0`, which
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
#' The limit bounds the wait; it does not promise the program dies at the
#' second. R asks the program to stop when the limit is reached, insists 20
#' seconds later, and kills it 20 seconds after that, so on Unix a program that
#' does not answer the first two can outlive its limit by up to 40 seconds — an
#' FFmpeg blocked on an unresponsive input has been measured doing exactly that.
#' R does not guarantee termination at all: a program can be written to survive
#' the attempt.
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
