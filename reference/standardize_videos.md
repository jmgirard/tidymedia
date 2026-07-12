# Standardize Many Videos From a Jobs Table

Re-encode many input files to a reproducible format from a single jobs
tibble — a table-driven sibling of
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
for when you have more than one video to standardize. Each row is one
input; the only required column names its source. This is a thin wrapper
over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input.

## Usage

``` r
standardize_videos(
  jobs,
  width = NULL,
  height = NULL,
  fps = NULL,
  vcodec = "libx264",
  pixel_format = "yuv420p",
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path). An optional `output` column names the destination; when
  absent, one is derived per row by appending `_standardized` to each
  input's basename, keeping the input's extension (e.g. `clip.mkv`
  becomes `clip_standardized.mkv`). Because standardization is
  one-input-to-one-output, a duplicated `input` with no `output` column
  would collide and is rejected. Each of the five standardization knobs
  — `width`, `height`, `fps`, `vcodec`, `pixel_format` — may also appear
  as a column to override the corresponding argument on a per-row basis;
  rows (or knobs) that omit the column fall back to the argument's
  value. Any other columns are ignored.

- width, height:

  Optional target dimensions applied to every row, unless `jobs` carries
  a column of the same name (see `jobs`). When only one is given the
  other is derived to preserve aspect ratio; when neither is given the
  frame is floor-cropped to even dimensions so odd-sized sources encode.
  (default = `NULL`)

- fps:

  Optional target frame rate applied to every row, unless `jobs` carries
  an `fps` column. (default = `NULL`, i.e. leave the frame rate
  unchanged)

- vcodec:

  A string naming the video codec applied to every row, unless `jobs`
  carries a `vcodec` column. (default = `"libx264"`)

- pixel_format:

  A string naming the pixel format applied to every row, unless `jobs`
  carries a `pixel_format` column. (default = `"yuv420p"`)

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  standardize in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
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

[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
for the single-input form;
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`segment_videos`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md)
and
[`extract_frames`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md)
for the other table-driven siblings.

Other task verb functions:
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("a.mp4", "b.mp4"),
  width  = c(640, 320)
)
# run = FALSE compiles one command per input without calling FFmpeg
standardize_videos(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                                     output width command
#>   <chr>                                                     <chr>  <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample… a.mp4    640 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample… b.mp4    320 "-y -i…
```
