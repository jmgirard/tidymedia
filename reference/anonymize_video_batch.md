# Anonymize Many Videos From a Jobs Table

Cover fixed rectangular regions of many input videos with opaque filled
boxes from a single jobs tibble — the **batch** (table-driven) sibling
of
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
for when you have more than one video to redact. Each row is one input
with its own regions; the required columns name the source (`input`) and
the boxes to cover (`regions`). This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input, sharing the same box-fill
pipeline (and per-region validation) as the scalar verb.

## Usage

``` r
anonymize_video_batch(
  jobs,
  color = "black",
  video_codec = "libx264",
  pixel_format = "yuv420p",
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path) and a `regions` list-column. Each `regions` cell is
  itself a data frame of boxes for that input — the same
  `x`/`y`/`width`/`height` (and optional per-box `color`) shape
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  takes. An optional `output` column names the destination; when absent,
  one is derived per row by appending `_anonymized` to each input's
  basename, keeping the input's extension (e.g. `clip.mkv` becomes
  `clip_anonymized.mkv`). Because anonymization is
  one-input-to-one-output, a duplicated `input` with no `output` column
  would collide and is rejected. Each of the three encode knobs —
  `color`, `video_codec`, `pixel_format` — may also appear as a column
  to override the corresponding argument on a per-row basis; rows (or
  knobs) that omit the column fall back to the argument's value. Any
  other columns are ignored.

- color:

  A string naming the default fill color (FFmpeg color syntax) applied
  to every row, unless `jobs` carries a `color` column or a box supplies
  its own `color`. (default = `"black"`)

- video_codec:

  A string naming the output video codec applied to every row, unless
  `jobs` carries a `video_codec` column. (default = `"libx264"`)

- pixel_format:

  A string naming the output pixel format applied to every row, unless
  `jobs` carries a `pixel_format` column. (default = `"yuv420p"`)

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  anonymize in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
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

## See also

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
for the single-input form;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
and
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the other table-driven siblings.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
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
  input   = c(video, video),
  output  = c("a.mp4", "b.mp4"),
  regions = list(
    data.frame(x = 10, y = 10, width = 120, height = 90),
    data.frame(x = 200, y = 150, width = 80, height = 60)
  )
)
# run = FALSE compiles one command per input without calling FFmpeg
anonymize_video_batch(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                                   output regions command
#>   <chr>                                                   <chr>  <list>  <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/samp… a.mp4  <df>    "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/samp… b.mp4  <df>    "-y -i…
```
