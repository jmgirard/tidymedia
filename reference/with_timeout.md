# Set a time limit for one call

`with_timeout()` runs `expr` with a time limit of its own. The limit
applies to each FFmpeg, FFprobe or MediaInfo program that `expr` starts.
When `with_timeout()` returns, or stops with an error, the limit that
was in force before the call is back.

The session limit, `options(tidymedia.timeout = )`, applies to every
call in the session. `with_timeout()` applies to one call. For example,
you can give one test conversion five minutes in a session with a
one-hour limit.

## Usage

``` r
with_timeout(expr, seconds)
```

## Arguments

- expr:

  An expression. It is run once, where you wrote it, and its value is
  returned.

- seconds:

  A whole number of seconds. `0` means no limit, so
  `with_timeout(expr, 0)` removes a session limit for one call. A
  fraction, a negative number, a string or `NULL` gives an error before
  `expr` runs.

## Value

The value of `expr`.

## Details

The limit applies to each program, not to the whole call. In a 100-row
batch inside `with_timeout(expr, 600)`, each program that a row starts
gets 600 seconds, plus the delay in "How long the wait can be". The
workers of a `parallel = TRUE` run use the same limit.

The limit is a whole number of seconds. The package does not round a
fraction, because R would read a limit below one second as no limit.

A limit set with `options(tidymedia.timeout = )` follows the same rule,
with one difference. `options(tidymedia.timeout = NULL)` removes the
option, so it means no limit. A function that can start a program gives
an error for a wrong value, even when `run = FALSE`.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
gives that error before it starts any job. A function that starts no
program gives no such error. For example,
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
starts none when you set `tidymedia.hardware_encoders`. A `probe_*()`
function that you give a `probe` object also starts none.

Most functions check their own arguments before the limit. So a wrong
argument gives its own error, even when the limit is also wrong. A few
arguments of the `_batch` functions are checked inside each job, after
the limit. An example is the `pixel_format` of
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md).
When the limit is also wrong, the error is about the limit.

## How long the wait can be

The limit sets how long R waits for a program, and the wait can be
longer. When the limit is reached, R asks the program to stop. R asks
again 20 seconds later, and kills the program 20 seconds after that. So
R can wait up to 40 seconds past the limit. For example, five hung files
under a 1-second limit can take about three and a half minutes.

R does not guarantee that the program stops. A program can survive the
attempts to stop it. How fast a program stops also depends on its
version.

## What happens when the limit is reached

A reached limit is never silent. The call gives an error or a warning.

These functions give an error with the class `tidymedia_timeout`, which
names the program and the limit:

- the task functions whose names do not end in `_batch`, except
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)

- [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
  and
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md)

- [`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md),
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  and
  [`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  when it asks FFmpeg

- [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md),
  because a check with no answer is not a "no"

These functions give a warning instead, so that one hung file does not
lose the rest of the work:

- [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
  the other `probe_*()` functions,
  [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
  [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
  [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
  and the `get_*()` functions give `NA` for that file. One warning at
  the end says how many files timed out.

- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and the `_batch` task functions set `success = FALSE` for that job.
  One warning at the end says how many jobs timed out. It has the class
  `tidymedia_batch_timeout`. Two steps of these calls give an error
  instead. One is the analysis pass of
  `normalize_audio_batch(two_pass = TRUE)`. The other is the check that
  FFmpeg has the hardware encoder that `hardware` names, such as
  `"nvenc"`. That check asks FFmpeg only when
  `tidymedia.hardware_encoders` is not set and the session has no stored
  answer. The glossary in
  [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  explains hardware encoders.

- The dropped-track check of
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and their `_batch` forms warns that it could not check. The track
  count that
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  reads after a failed run warns the same way. A batch manifest, see
  [`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md),
  and
  [`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
  warn when they cannot read a program version. These warnings have the
  class `tidymedia_probe_timeout`, and the call goes on as it would for
  an unreadable input.

`suppressWarnings(classes = "tidymedia_dropped_audio")` hides the
dropped-track warning, but not the warning that the check timed out. To
hide both, add `"tidymedia_probe_timeout"` to `classes`.

The task functions and
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
delete a part-written output file after a timeout, as they do after any
failed run.
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
cannot tell which of your arguments is the output, so it leaves that
file. Check the output of a timed-out
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
call yourself.

## See also

[`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
to set a limit for the rest of a function.
[tidymedia-package](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
describes the session options.

## Examples

``` r
# Inside the call, the limit is the one you gave.
with_timeout(getOption("tidymedia.timeout"), 30)
#> [1] 30

# Outside it, the session's own setting is untouched.
getOption("tidymedia.timeout", default = "unset")
#> [1] "unset"

if (FALSE) { # \dontrun{
# Bound one conversion at five minutes, whatever the session is set to.
with_timeout(extract_audio("in.mp4", "out.wav"), 300)
} # }
```
