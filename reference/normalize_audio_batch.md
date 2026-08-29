# Normalize Many Files' Audio Loudness From a Jobs Table

Loudness-normalize the audio of many input files (EBU R128) from a
single jobs tibble — the **batch** (table-driven) sibling of
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
for when you have more than one file to normalize. Each row is one
input; the only required column names its source. This is a thin wrapper
over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input, sharing the same `loudnorm`
pipeline (and per-value validation) as the scalar verb. Set
`two_pass = TRUE` for accurate measured/linear normalization across the
whole table (see `two_pass`).

## Usage

``` r
normalize_audio_batch(
  jobs,
  target_loudness = -23,
  true_peak = -1,
  loudness_range = 7,
  channels = NULL,
  sample_rate = NULL,
  audio_codec = NULL,
  two_pass = FALSE,
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path). An optional `output` column names the destination; when
  absent, one is derived per row by appending `_normalized` to each
  input's basename, keeping the input's extension (e.g. `clip.mkv`
  becomes `clip_normalized.mkv`) — note that the derived name keeps a
  *video* extension while the file itself holds audio only, so name an
  `output` column explicitly when that matters. Because normalization is
  one-input-to-one-output, a duplicated `input` with no `output` column
  would collide and is rejected. Each of the five loudness knobs —
  `target_loudness`, `true_peak`, `loudness_range`, `channels`,
  `sample_rate` — may also appear as a column to override the
  corresponding argument on a per-row basis; rows (or knobs) that omit
  the column fall back to the argument's value. An optional
  `audio_codec` column (character) names each row's output audio
  encoder, with `NA` meaning "leave the encoder unset"; rows omitting it
  fall back to the `audio_codec` argument. An optional numeric
  `audio_stream` column (`NA` to normalize that row's first audio track)
  likewise overrides the `audio_stream` argument per row. Any other
  columns are ignored.

- target_loudness, true_peak, loudness_range:

  The EBU R128 loudness targets applied to every row, unless `jobs`
  carries a column of the same name (see `jobs`). Defaults follow EBU
  Recommendation R 128 (2014): `target_loudness = -23` LUFS,
  `true_peak = -1` dBTP, `loudness_range = 7` LU.

- channels:

  The output channel count applied to every row, unless `jobs` carries a
  `channels` column, e.g. `1` to downmix to mono. `NULL` (default) keeps
  each source's channel layout.

- sample_rate:

  The output sample rate in Hz applied to every row, unless `jobs`
  carries a `sample_rate` column. `NULL` (default) lets `loudnorm`
  choose (it resamples, up to 192 kHz encoder-capped — not the source
  rate); set this to pin the output rate.

- audio_codec:

  The output audio encoder applied to every row, unless `jobs` carries
  an `audio_codec` column, e.g. `"aac"`. `NULL` (default) emits no
  `-codec:a`, leaving the output container's default encoder in place.
  `"copy"` is an error: loudness normalization filters the audio, so it
  must be re-encoded. See
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).

- two_pass:

  A logical selecting the batch normalization mode for *every* row
  (`two_pass` is a whole-table switch, not a per-row column). `FALSE`
  (default) keeps the single-pass `loudnorm` pipeline. `TRUE` runs the
  accurate two-pass (measured/linear) path as a two-phase fan-out: an
  *analysis pass* first measures every input's loudness (honoring
  `parallel` and each row's targets), and a *correction pass* then feeds
  those measurements back with `linear=true` so each output hits its EBU
  R128 target precisely — the table-wide sibling of
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)'s
  `two_pass`. The five measured values are surfaced on the result as
  columns `measured_I`, `measured_TP`, `measured_LRA`,
  `measured_thresh`, and `offset`. Because it must measure each input,
  two-pass **always runs the analysis pass through FFmpeg** (it needs
  the binary and readable inputs), even when `run = FALSE`. If any row's
  analysis fails or yields no parseable measurement, the call aborts —
  naming the offending row(s) — before any correction command is built.
  That abort is classed `tidymedia_loudnorm_analysis` and carries the
  same row numbers on `tm_rows`, alongside `tm_row_status`: each row's
  FFmpeg exit status, or `NA` where the row exited zero but printed
  nothing parseable. **Silent** rows are the exception: a silent input
  (analysis loudness `-inf`) cannot be normalized to a target, but one
  silent row does not abort the batch — the non-silent rows are
  normalized, the silent rows are marked in a logical `silent` column
  (with `success = FALSE` and no output written), and a warning names
  them. The single-pass default touches no binary under `run = FALSE`.

- audio_stream:

  The 0-based index of the audio track to normalize, counted *among that
  row's input's audio streams* – `0` is the first audio track, `1` the
  second, whatever their positions among the file's streams. `NULL`
  (default) normalizes the **first** audio track. The argument applies
  to every row lacking an `audio_stream` column; an `NA` cell in that
  column means the same as `NULL` for that row, rather than falling back
  to the argument. The first-track family reads `NULL` this way –
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  plus their `_batch` siblings. The every-track family keeps them all
  instead:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  plus theirs. This verb reads `NULL` the first-track way because the
  two-pass analysis produces one measurement per audio track while the
  correction takes a single set, so normalizing several tracks at once
  would apply one track's measurements to all of them. Under
  `two_pass = TRUE` the analysis pass measures this same track. Only the
  named track reaches the output, and no video does – whatever the
  container, so an output name that keeps a video extension yields a
  video file carrying audio alone. An input with no audio at all is an
  FFmpeg error. Naming a track the input does not have is an FFmpeg
  error, not an R one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`). Under `two_pass = TRUE`
  this gates only the correction pass; the analysis pass runs regardless
  (see `two_pass`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  normalize in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one input at a time and
  warns. Set a plan first, e.g. `future::plan(future::multisession)`.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  such as `verify`, `manifest`, `checksums`, and `progress`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
`jobs` with an added `command` column (and, when `output` was derived,
the resolved `output` column; when `run = TRUE`, a `success` column,
plus any columns the forwarded arguments add, e.g. `verified`). Under
`two_pass = TRUE` the result also carries the five measured columns
(`measured_I` etc.) and a logical `silent` column, and the `command`
column holds the linear correction commands (`NA` for silent rows, which
carry `NA` measurements and are not normalized). The two-pass result's
schema is independent of how many rows are silent: the opt-in `verified`
column (under `verify`) and provenance manifest (under `manifest`, read
with
[`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md))
are present whenever requested, even when *every* row is silent – silent
rows simply carry `NA` for those outputs.

## Details

When a row names no `audio_stream` and its input turns out to carry
tracks the output will not, the verb warns **once** for the whole batch,
naming every affected row. Naming a track silences it – the
`audio_stream` argument, or an `audio_stream` cell on every row – as
does `suppressWarnings(classes = "tidymedia_dropped_audio")`. The check
is **best-effort** and costs **one FFprobe call per distinct input** it
has to probe, so a repeated input is probed once and a row that names a
track is not probed at all: it is emitted when FFprobe is available and
the input can be probed, and skipped silently otherwise. Those probes
run **serially at the front door**, before the fan-out starts, so
`parallel` does not reach them; a sweep long enough to look like a hang
reports its progress. The check never runs under `run = FALSE`, never
changes any compiled command, and is skipped entirely when every row
names a track. Under `two_pass = TRUE` it lands *before* Phase 1, so it
arrives while adding `audio_stream` can still save the analysis pass.

Switch the check off – and skip the whole sweep – with
`options(tidymedia.check_tracks = FALSE)` for the session, or
`withr::local_options(tidymedia.check_tracks = FALSE)` for the rest of
one function.

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.

## See also

[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
for the single-input form;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
for the video-side table-driven sibling.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

Other audio selection functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input           = c(video, video),
  output          = c("a.mp4", "b.mp4"),
  target_loudness = c(-23, -16)
)
# run = FALSE compiles one command per input without calling FFmpeg
normalize_audio_batch(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                           output target_loudness command
#>   <chr>                                           <chr>            <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extd… a.mp4              -23 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extd… b.mp4              -16 "-y -i…
# Accurate two-pass (measured/linear) normalization across the whole table
# (runs FFmpeg to measure each input, so needs the binary):
if (FALSE) { # \dontrun{
normalize_audio_batch(jobs, two_pass = TRUE)
} # }
```
