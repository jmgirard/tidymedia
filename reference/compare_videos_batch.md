# Build Many Comparison Videos From a Jobs Table

Stack videos side by side for many outputs from a single jobs tibble —
the **batch** (table-driven) sibling of
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
for when you have more than one comparison to produce. Each row carries
an `inputs` list-column (each cell two or more video paths) plus an
`output` column (D015). This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible stacking command per row, sharing the pipeline with the
scalar verb.

## Usage

``` r
compare_videos_batch(
  jobs,
  direction = c("horizontal", "vertical"),
  resize = TRUE,
  audio = NULL,
  video_codec = NULL,
  hardware = c("none", "nvenc"),
  fallback = FALSE,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per output and (at least) an `inputs`
  list-column — each cell a character vector of **two or more** video
  paths — and an `output` column (destination path). Optional
  `direction`, `resize`, `audio`, and `video_codec` columns override the
  like-named arguments per row (a row omitting one falls back to the
  argument). In an `audio` column, `NA` means "drop audio" (the column's
  way of writing the scalar's `NULL`); in a `video_codec` column it
  means "leave the codec unset". Any two rows resolving to the same
  output path are rejected; other columns are ignored.

- direction, resize, audio:

  Defaults applied to every row lacking the corresponding column. See
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  for their meaning. `audio` is validated per row against that row's
  input count.

- video_codec:

  A string naming the output video codec, applied to every row lacking a
  `video_codec` column, or `NULL` (default) to leave it unset so each
  output keeps its container's default encoder.

- hardware, fallback:

  The encoder backend and its fallback behavior, applied to the whole
  batch (a property of the machine, not of a row, so neither is read as
  a `jobs` column). See
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md).

- run:

  A logical: run each command through FFmpeg (`TRUE`, default) or only
  compile them for inspection (`FALSE`).

- parallel:

  A logical: map over jobs in parallel with furrr (`TRUE`) or
  sequentially (`FALSE`, default). See
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  for the future plan requirement.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (e.g. `verify`, `manifest`, `progress`).

## Value

The `jobs` tibble with an added `command` column and, when `run = TRUE`,
a `success` column (plus `verified` / provenance manifest when requested
via `...`). See
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

## See also

[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
the scalar verb it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the `hardware = "nvenc"` toggle;
[`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md)
and
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
the other fan-in batch siblings.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
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
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
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

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(inputs = list(c(video, video)), output = "compare.mp4")
compare_videos_batch(jobs, run = FALSE)
#> # A tibble: 1 × 3
#>   inputs    output      command                                                 
#>   <list>    <chr>       <chr>                                                   
#> 1 <chr [2]> compare.mp4 "-y -i \"/home/runner/work/_temp/Library/tidymedia/extd…
```
