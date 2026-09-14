# tidymedia: Media File Preprocessing and Metadata for the 'tidyverse'

tidymedia prepares audio and video files for research. It trims, crops,
converts and standardizes files, and reads file details back as tibbles.
It runs the programs FFmpeg and MediaInfo. Start with
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md).

## Details

Task functions, such as
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
do one common job in one call. Pipeline functions, such as
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md),
build an FFmpeg command one step at a time. Direct commands,
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
and
[`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md),
pass your own arguments to the programs.
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
and
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md)
read file details.

See
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
for the guided tour and a glossary of media terms. The other vignettes
are `"batch"`, `"metadata"`, `"verification"` and `"workflow"`.

## Session options

A new option value takes effect at the next call. The workers of a
`parallel = TRUE` run use your session's values.

- `options(tidymedia.timeout = 600)` sets a limit, in whole seconds, on
  each program the package starts. The default, `0`, means no limit. See
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).

- `options(tidymedia.check_tracks = FALSE)` turns off the dropped-track
  warning of
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and their `_batch` forms. The default is `TRUE`. The check runs one
  FFprobe call for each different input, when `run = TRUE` and the call
  or job row names no `audio_stream`. Turning it off skips those calls.
  A value other than `TRUE` or `FALSE` gives an error that names the
  option, when the check would run.

- `options(tidymedia.hardware_encoders = "h264_nvenc")` names the
  hardware video encoders of this computer, so the package does not ask
  FFmpeg. `character(0)` means none.

## Errors when FFmpeg fails

- `tidymedia_ffmpeg_exit`: FFmpeg exited non-zero in
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  or in the `loudnorm` analysis pass of
  `normalize_audio(two_pass = TRUE)`. The task functions that run their
  command with
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  such as
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  give it too. The `tm_status` field holds the exit status.

- `tidymedia_loudnorm_no_measurement`: the two-pass analysis gave no
  measurement. In the `_batch` form, `tm_rows` holds the rows, and
  `tm_row_status` holds each exit status or `NA`.

- `tidymedia_multitrack_separation`:
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  could not write several audio tracks into one audio file. The `_batch`
  form gives a warning.

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
