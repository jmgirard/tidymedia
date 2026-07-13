# Sample frames from many videos at a fixed rate from a jobs table

Sample many videos into numbered image sequences from a single jobs
tibble — the **batch** (table-driven) sibling of
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md).
Each row is one input video sampled at a fixed rate into its own image
sequence. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input.

## Usage

``` r
sample_frames_batch(
  jobs,
  fps = NULL,
  interval = NULL,
  outdir = NULL,
  format = "png",
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path). Optional columns: `outdir` (the output directory for
  that row's sequence; when absent, one is derived as
  `<input-base>_frames` beside each input), and `fps` / `interval`
  (per-row rate overrides). Any other columns are ignored.

- fps, interval:

  The sampling rate applied to every row, as in
  [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md);
  a per-row column of the same name overrides it. Supply exactly one of
  the two (as an argument or a column). (default = `NULL`)

- outdir:

  An optional single output directory for all rows (overridden by an
  `outdir` column); when both are absent, per-input directories are
  derived. (default = `NULL`)

- format:

  A string giving the output image file extension, as in
  [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md).
  (default = `"png"`)

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  sample in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one at a time and warns.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  such as `verify`, `manifest`, `checksums`, and `progress`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
`jobs` with an added `command` column (and the resolved `outdir` column
when it was derived; when `run = TRUE`, a `success` column, plus any
columns the forwarded arguments add, e.g. `verified`).

## Details

Supply the sampling rate once as the scalar `fps` or `interval` argument
(applied to every row), or per row as an `fps` or `interval` column that
overrides the scalar of the same name. Exactly one of the two — fps *or*
interval — may be supplied across arguments and columns.

## See also

[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
for the single-video form;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
for the enumerated-frame sibling.

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
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input  = c(video, video),
  outdir = c(file.path(tempdir(), "a"), file.path(tempdir(), "b"))
)
# run = FALSE compiles one command per input without calling FFmpeg
sample_frames_batch(jobs, fps = 2, run = FALSE)
#> # A tibble: 2 × 3
#>   input                                                        outdir    command
#>   <chr>                                                        <chr>     <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 /tmp/Rtm… "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 /tmp/Rtm… "-y -i…
```
