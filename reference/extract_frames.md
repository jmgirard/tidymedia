# Extract Still Frames From Many Videos From a Jobs Table

Grab one still image per row across many input files from a single jobs
tibble — a table-driven sibling of
[`extract_frame`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
for when your frames span more than one input. Each row is one frame;
the required columns name its source and the moment to capture. This is
a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per frame.

## Usage

``` r
extract_frames(jobs, format = "png", run = TRUE, parallel = FALSE, ...)
```

## Arguments

- jobs:

  A data frame with one row per frame and (at least) an `input` column
  (source path) plus **exactly one** of a `timestamp` column (seconds,
  or FFmpeg time-duration strings) or a `frame` column (whole frame
  numbers, converted per row to a timestamp via the input's frame rate,
  as
  [`extract_frame`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
  does). An optional `output` column names the destination image; when
  absent, one is derived per row by appending `_<n>.<format>` to each
  input's basename, with the frame number restarting at 1 for each input
  file. Any other columns are ignored.

- format:

  A string giving the image file extension used when `output` is derived
  (ignored when `jobs` carries an `output` column). (default = `"png"`)

- run:

  A logical: run each frame's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  grab frames in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one frame at a time and
  warns.

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

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`extract_frame`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
for the single-frame form;
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`segment_videos`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md)
for the segment-cutting sibling.

Other task verb functions:
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input     = c(video, video),
  output    = c("a.png", "b.png"),
  timestamp = c(0.25, 0.75)
)
# run = FALSE compiles one command per frame without calling FFmpeg
extract_frames(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                                 output timestamp command
#>   <chr>                                                 <chr>      <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sa… a.png       0.25 "-y -s…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sa… b.png       0.75 "-y -s…
```
