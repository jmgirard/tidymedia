# tidymedia: Tools for working with media files within R and the tidyverse

The goal of tidymedia is to provide tools for easily working with media
(e.g., image, audio, and video) files within R and the tidyverse.

## Details

tidymedia is organized in three layers. Layer 0 passes raw arguments to
the command-line tools:
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md),
and
[`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md).
Layer 1 is the pipeline builder, which assembles an FFmpeg command step
by step and then compiles or runs it:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md) and the
`ffm_*()` functions. Layer 2 is the task verbs, thin wrappers over the
builder for common preprocessing jobs such as
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
Metadata is read by two independent back ends: FFprobe, through
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
and the other `probe_*()` readers, which return container and stream
tibbles; and MediaInfo, through
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
and the `get_*()` helpers such as
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
which return a single value.

## Bounding a run that hangs

Every tidymedia call that touches FFmpeg, FFprobe or MediaInfo waits for
that program to finish, and a program that hangs blocks the R session
with it. Set a wall-clock limit, in whole seconds, to bound the wait:

    options(tidymedia.timeout = 600)

To bound one call rather than the whole session, wrap it in
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md):

    with_timeout(extract_audio("in.mp4", "out.wav"), 300)

That call runs under its own limit; the session's setting, or the
absence of one, is back when it returns.

To bound the rest of a function rather than an expression you wrap, say
it as a statement with
[`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md):

    convert_all <- function(files) {
      local_timeout(300)
      for (f in files) extract_audio(f, sub("[.][^.]*$", ".wav", f))
    }

Every program that function starts is bounded at `seconds`, and the
caller's setting is back once it returns.

A reached limit is never silent: every call that can start one of those
programs either aborts or warns.

It **aborts**, naming the program and the limit, from the task verbs,
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
and the Layer 0 escape hatches
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
and
[`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md).
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
aborts as well, because a probe that never answered is not an answer of
"no".

It **warns** everywhere a single hung file must not discard the rest of
the work.
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
and the `probe_*()` accessors,
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
and the `get_*()` helpers give an `NA` row and one warning at the end of
the call, saying how many of the files it names timed out rather than
being unreadable.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and the `_batch` verbs mark the row `success = FALSE`, as they do for
any failed job, and warn once at the end of the run saying how many jobs
the limit killed — at `parallel = TRUE` no differently from
sequentially. The dropped-track check behind
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
and their `_batch` siblings warns that it could not check, and the
provenance manifest warns that it could not read a version; both then
carry on as they would for any other unreadable input.

To handle either outcome programmatically, the abort carries the
condition class `tidymedia_timeout`; the dropped-track and version-probe
warnings carry `tidymedia_probe_timeout`, and the batch warning
`tidymedia_batch_timeout`. So the documented recipe for silencing the
dropped-track check,
`suppressWarnings(classes = "tidymedia_dropped_audio")`, silences only
the check itself — add `"tidymedia_probe_timeout"` to also silence the
notice that the limit stopped it from running.

Those two lists are not written from memory. A test derives the calls
that can start one of these programs from the package's own call graph
and drives a timeout through each of them, so a call that started
absorbing one silently would fail the package's own checks.

For the task verbs and
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
which know where their output goes, a partial file the killed run had
written is removed just as it is after any other failed run. The raw
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
escape hatch is handed an argument string it does not parse, so it
cannot tell which of those arguments is the output and leaves whatever
the killed run wrote in place — check the output of a timed-out
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
call yourself. The default is `0`, which means no limit — every call
waits as long as the program takes, which is what you want for a
legitimate multi-hour encode. Fractional values are refused rather than
rounded, because the underlying limit is whole seconds and a value below
one second would otherwise be read as no limit at all.

The limit applies per spawned program, not per batch: a 100-row batch
with a 600-second limit bounds each row at 600 seconds. tidymedia's own
`parallel = TRUE` paths are bounded by the same limit as their
sequential ones: the limit you set is carried into each worker for the
duration of the call, and whatever that worker had set for itself is put
back afterwards. A limit the underlying `timeout=` could not use — a
fraction of a second, a negative number, a string — is refused by
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
before it dispatches any job, on either of its paths and whether or not
it is going to run anything.

The limit bounds the wait; it does not promise the program dies at the
second. R asks the program to stop when the limit is reached, insists 20
seconds later, and kills it 20 seconds after that, so on Unix a program
that does not answer the first two can outlive its limit by up to 40
seconds — an FFmpeg blocked on an unresponsive input has been measured
doing exactly that. R does not guarantee termination at all: a program
can be written to survive the attempt.

See
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
for the guided tour,
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
for running a verb over many files,
[`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
for the readers, and
[`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
for an end-to-end research preprocessing pipeline. The full function
list is on the package's reference index.

## See also

Useful links:

- <https://github.com/jmgirard/tidymedia>

- <https://jmgirard.github.io/tidymedia/>

- Report bugs at <https://github.com/jmgirard/tidymedia/issues>

## Author

**Maintainer**: Jeffrey Girard <me@jmgirard.com>
([ORCID](https://orcid.org/0000-0002-7359-3746))

Authors:

- Jeffrey Girard <me@jmgirard.com>
  ([ORCID](https://orcid.org/0000-0002-7359-3746))
