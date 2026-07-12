# Segment Many Videos From a Jobs Table

Cut segments across many input files from a single jobs tibble — a
table-driven sibling of
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
for when your segments span more than one input. Each row is one
segment; the four required columns name its source, destination, and cut
points. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per segment.

## Usage

``` r
segment_videos(jobs, reencode = TRUE, run = TRUE, parallel = FALSE, ...)
```

## Arguments

- jobs:

  A data frame with one row per segment and (at least) the columns
  `input` (source path), `output` (destination path), `start` and `end`
  (cut points; a numeric column of seconds or a character column with
  time-duration syntax). Any other columns are ignored.

- reencode:

  A logical passed to
  [`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md):
  cut each segment frame-accurately by re-encoding (`TRUE`, default) or
  with a fast, lossless copy that snaps to keyframes (`FALSE`). See
  `ffm_seek` for the trade-off. Applies to every row.

- run:

  A logical: run each segment's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  cut segments in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one segment at a time
  and warns. Set a plan first, e.g.
  `future::plan(future::multisession)`.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  such as `verify`, `manifest`, `checksums`, and `progress`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
`jobs` with an added `command` column (and, when `run = TRUE`, a
`success` column, plus any columns the forwarded arguments add, e.g.
`verified`).

## References

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
for the single-input, parallel-vector form;
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
for the cut trade-off.

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
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("a.mp4", "b.mp4"),
  start  = c(0, 0.5),
  end    = c(0.5, 1)
)
# run = FALSE compiles one command per segment without calling FFmpeg
segment_videos(jobs, run = FALSE)
#> # A tibble: 2 × 5
#>   input                                               output start   end command
#>   <chr>                                               <chr>  <dbl> <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/… a.mp4    0     0.5 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/… b.mp4    0.5   1   "-y -i…
```
