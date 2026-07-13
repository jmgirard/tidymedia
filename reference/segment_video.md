# Segment Video

Use FFmpeg to quickly break a single video file into multiple smaller
video files (with the same encoding) based on pairs of start and stop
timestamps. Segment video files will be named by taking the name of
`infile` and appending a suffix of an underscore (\_) and an integer
indicating which segment (based on the order provided in `start` and
`end`).

## Usage

``` r
segment_video(
  infile,
  start,
  end,
  outfiles = NULL,
  reencode = TRUE,
  run = TRUE,
  parallel = FALSE
)
```

## Arguments

- infile:

  A string containing the path to a video file.

- start:

  A vector containing one or more timestamps indicating the start of
  each segment to create. Can be either a numeric vector indicating
  seconds or a character vector with time duration syntax. Must have the
  same length as `end`.

- end:

  A vector containing one or more timestamps indicating the stop of each
  segment to create. Can be either a numeric vector indicating seconds
  or a character vector with time duration syntax. Must have the same
  length as `start`.

- outfiles:

  Either NULL or a character vector indicating the filename (with
  extension) for each segment to create. If NULL, will append a
  zero-padded integer to `infile`. If not NULL, must have the same
  length as `start`.

- reencode:

  A logical passed to
  [`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md):
  cut each segment frame-accurately by re-encoding (`TRUE`, default) or
  with a fast, lossless copy that snaps to keyframes (`FALSE`). See
  `ffm_seek` for the trade-off.

- run:

  A logical: run each segment's command (`TRUE`, default) or only
  compile them (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  cut segments in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one segment at a time
  and warns. Set a plan first, e.g.
  `future::plan(future::multisession)`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one row per segment with its `command` (and, when `run = TRUE`,
`success`).

## References

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)

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
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Two segments; run = FALSE compiles one command per segment
segment_video(video, start = c(0, 0.5), end = c(0.5, 1), run = FALSE)
#> # A tibble: 2 × 5
#>   input                                               output start   end command
#>   <chr>                                               <chr>  <dbl> <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/… /home…   0     0.5 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/… /home…   0.5   1   "-y -i…
```
