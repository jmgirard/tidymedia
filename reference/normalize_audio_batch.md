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
  two_pass = FALSE,
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
  **Silent** rows are the exception: a silent input (analysis loudness
  `-inf`) cannot be normalized to a target, but one silent row does not
  abort the batch — the non-silent rows are normalized, the silent rows
  are marked in a logical `silent` column (with `success = FALSE` and no
  output written), and a warning names them. The single-pass default
  touches no binary under `run = FALSE`.

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
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
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
