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
#' See `vignette("tidymedia")` for the guided tour, `vignette("batch")` for
#' running a verb over many files, `vignette("metadata")` for the readers, and
#' `vignette("workflow")` for an end-to-end research preprocessing pipeline.
#' The full function list is on the package's reference index.
#'
#' @section Bounding a run that hangs:
#' Every tidymedia call that touches FFmpeg, FFprobe or MediaInfo waits for
#' that program to finish, and a program that hangs blocks the R session with
#' it. Set a wall-clock limit, in whole seconds, to bound the wait:
#'
#' \preformatted{options(tidymedia.timeout = 600)}
#'
#' To bound one call rather than the whole session, wrap it in
#' [with_timeout()]:
#'
#' \preformatted{with_timeout(extract_audio("in.mp4", "out.wav"), 300)}
#'
#' That call runs under its own limit; the session's setting, or the absence of
#' one, is back when it returns.
#'
#' To bound the rest of a function rather than an expression you wrap, say it as
#' a statement with [local_timeout()]:
#'
#' \preformatted{convert_all <- function(files) {
#'   local_timeout(300)
#'   for (f in files) extract_audio(f, sub("[.][^.]*$", ".wav", f))
#' }}
#'
#' Every program that function starts is waited for at most `seconds` — plus
#' the lag described below — and the caller's setting is back once it returns.
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
#' job, and warn once at the end of the run saying how many jobs timed out and
#' did not run to completion — at `parallel = TRUE` no differently from
#' sequentially. The dropped-track check behind [extract_audio()],
#' [convert_audio()], [normalize_audio()] and their `_batch` siblings warns
#' that it could not check, as does the multi-track diagnostic
#' [separate_audio_video()] reaches after a run has already failed; and the
#' provenance manifest warns that it could not read a version. All then carry
#' on as they would for any other unreadable input.
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
#' A refused value is refused by the function you called. Set the option to
#' `0.5` and `extract_audio()` says so as `extract_audio()`, not as the builder
#' underneath it. It says so on a `run = FALSE` call as well as a run, so a dry
#' run does not hand you a command compiled under a limit it could never have
#' used. An argument your call got wrong is reported first wherever the verb
#' itself can see it is wrong: the limit is checked after the verb's own guards
#' and after the command has been assembled, so a bad `regions`, `pixel_format`
#' or `video_codec` reports as itself whether or not a limit is set. Asking for
#' `hardware = "nvenc"` no longer changes that: your FFmpeg build is asked what
#' encoders it has after every check the verb itself makes, so a bad
#' `audio_codec`, `pixel_format` or `audio_stream` reports as itself there too —
#' whether or not a limit is set, and whether or not that build has nvenc.
#' `fallback` is checked where that question is asked and so moved down with it:
#' a call wrong about both `fallback` and `pixel_format` now hears about the
#' pixel format. Where the check runs somewhere the verb reaches only later, it
#' loses to both the limit and the encoder question: [segment_video()]'s
#' `outfiles`, a `_batch` job table's `output` column, and
#' [anonymize_video_batch()]'s `pixel_format` and `color` are validated inside
#' the per-row fan-out, so a set limit is reported instead of them, and so is a
#' missing nvenc encoder under `hardware = "nvenc"` on a build without one.
#' Two calls read no limit and so refuse nothing: [has_nvenc()]
#' answering from a `tidymedia.nvenc_encoders` you set, which asks FFmpeg
#' nothing, and a `probe_*()` shortcut handed a `probe` object instead of an
#' `infile`, which reprobes nothing.
#'
#' The limit applies per spawned program, not per batch: a 100-row batch with a
#' 600-second limit waits at most 600 seconds — plus the lag described below —
#' on each row. tidymedia's own
#' `parallel = TRUE` paths are bounded by the same limit as their sequential
#' ones: the limit you set is carried into each worker for the duration of the
#' call, and whatever that worker had set for itself is put back afterwards. A
#' limit the underlying `timeout=` could not use — a fraction of a second, a
#' negative number, a string — is refused by [ffm_batch()] before it dispatches
#' any job, on either of its paths and whether or not it is going to run
#' anything.
#'
#' The limit bounds the wait; it does not promise the program dies at the
#' second. R asks the program to stop when the limit is reached, insists 20
#' seconds later, and kills it 20 seconds after that, so a program that does not
#' answer the first two is waited for up to 40 seconds past its limit. That is
#' measured, not estimated: an FFmpeg blocked on an unresponsive input under a
#' 2-second limit returned at 42.0 seconds on Linux, and a shell child that
#' ignores both signals did the same on macOS. Budget for it — five one-second
#' limits over five hung files is three and a half minutes, not five seconds.
#'
#' Whether the program then dies is a separate question, and the answer is not
#' always yes. R does not guarantee termination: a program can be written to
#' survive the attempt, and one measured here did. Which FFmpeg build you have
#' matters too — the same blocked input that took 42.0 seconds against
#' FFmpeg 6.1.1 took 2.0 seconds against 9.0.1, because the newer build answers
#' the first signal.
#'
#' A run that FFmpeg itself refused is a different outcome from a run the
#' limit killed, and carries a different class. The abort from [ffm_run()], the
#' abort from the `loudnorm` analysis pass behind
#' `normalize_audio(two_pass = TRUE)` when FFmpeg exits non-zero, and the
#' multi-track diagnostic [separate_audio_video()] adds to a failed audio
#' output are classed `tidymedia_ffmpeg_exit` and carry the exit status in their
#' `tm_status` field. The last two name what failed as well as how, each
#' carrying a second class ahead of that one:
#' `tidymedia_loudnorm_no_measurement` and `tidymedia_multitrack_separation`.
#'
#' Those two names are the ones that hold across a verb's scalar and `_batch`
#' forms, where `tidymedia_ffmpeg_exit` cannot. The batch two-pass analysis
#' phase reports every offending row at once and fires for rows that exited zero
#' as well, so it raises `tidymedia_loudnorm_no_measurement` alone, carrying
#' `tm_rows` and `tm_row_status` and no exit status. The scalar abort for an
#' analysis pass that exited zero and printed nothing parseable raises that
#' class alone as well, carrying no fields at all. The batch separation warning
#' is `tidymedia_multitrack_separation` alone, with no exit status either.
#'
#' @section Session options:
#' Three options change how the package behaves for the rest of the session.
#' Each is read where it is needed rather than at load time, so setting one
#' takes effect on the next call. The first two refuse a value they cannot use,
#' naming the option, at the first call that reads it.
#'
#' \preformatted{options(tidymedia.timeout = 600)}
#'
#' A wall-clock limit, in whole seconds, on how long each spawned program is
#' waited for. `0` (the default) means no limit. Described in full under
#' *Bounding a run that hangs* above, with [with_timeout()] and
#' [local_timeout()] for one call and one function.
#'
#' \preformatted{options(tidymedia.check_tracks = FALSE)}
#'
#' Switches off the dropped-track check — the warning [extract_audio()],
#' [convert_audio()], [normalize_audio()] and their `_batch` siblings signal
#' when an input carries audio tracks the output will not. The default is
#' `TRUE`. The check costs one FFprobe call per distinct input, run before the
#' work starts and, on the `_batch` verbs, serially before the fan-out; that
#' cost is what switching the check off buys back. It is worth declining on a
#' large jobs table whose inputs you already know the tracks of. A row that
#' names an `audio_stream` is not probed at all, so a table whose rows all name
#' one pays nothing either way.
#'
#' \preformatted{options(tidymedia.nvenc_encoders = c("h264_nvenc", "hevc_nvenc"))}
#'
#' Names the NVIDIA hardware encoders this machine has, instead of asking
#' FFmpeg. Set it to `character(0)` to declare there are none. Unset (the
#' default), the package asks once per session and remembers the answer.
#'
#' All three are carried into `parallel = TRUE` workers, which run under the
#' settings the calling session had and hand their own back afterwards, and all
#' three can be set for one call with `withr::with_options()` or for the rest of
#' a function with `withr::local_options()`.
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom glue glue
## usethis namespace: end
NULL
