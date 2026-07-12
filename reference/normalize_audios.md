# Normalize Many Files' Audio Loudness From a Jobs Table

Loudness-normalize the audio of many input files (EBU R128) from a
single jobs tibble — a table-driven sibling of
[`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
for when you have more than one file to normalize. Each row is one
input; the only required column names its source. This is a thin wrapper
over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input, sharing the same
single-pass `loudnorm` pipeline (and per-value validation) as the scalar
verb.

## Usage

``` r
normalize_audios(
  jobs,
  target_loudness = -23,
  true_peak = -1,
  loudness_range = 7,
  channels = NULL,
  sample_rate = NULL,
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
  becomes `clip_normalized.mkv`). Because normalization is
  one-input-to-one-output, a duplicated `input` with no `output` column
  would collide and is rejected. Each of the five loudness knobs —
  `target_loudness`, `true_peak`, `loudness_range`, `channels`,
  `sample_rate` — may also appear as a column to override the
  corresponding argument on a per-row basis; rows (or knobs) that omit
  the column fall back to the argument's value. Any other columns are
  ignored.

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

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

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
plus any columns the forwarded arguments add, e.g. `verified`).

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.

## See also

[`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
for the single-input form;
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`standardize_videos`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)
for the video-side table-driven sibling.

Other task verb functions:
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input           = c(video, video),
  output          = c("a.mp4", "b.mp4"),
  target_loudness = c(-23, -16)
)
# run = FALSE compiles one command per input without calling FFmpeg
normalize_audios(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                           output target_loudness command
#>   <chr>                                           <chr>            <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extd… a.mp4              -23 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extd… b.mp4              -16 "-y -i…
```
