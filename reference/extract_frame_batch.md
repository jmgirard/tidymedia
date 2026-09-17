# Extract Still Frames From Many Videos From a Jobs Table

Save one still image for each row, across many input files, using one
jobs table. This is the **batch** form of
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
for when your frames come from more than one input. Each row is one
frame. The required columns name its source and the moment to capture.
The function is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It builds one reproducible command for each frame. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as frame rate.

## Usage

``` r
extract_frame_batch(jobs, format = "png", run = TRUE, parallel = FALSE, ...)
```

## Arguments

- jobs:

  A data frame with one row per frame. It needs at least an `input`
  column (source path). It also needs **exactly one** of a `timestamp`
  column and a `frame` column. A `timestamp` holds seconds, or FFmpeg
  time-duration strings. A `frame` holds whole frame numbers. The
  function converts each one to a timestamp with the input's frame rate,
  as
  [`extract_frame`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
  does. An optional `output` column names the destination image. When it
  is absent, the function derives one per row by appending
  `_<n>.<format>` to each input's basename. The frame number restarts at
  1 for each input file. The function refuses two rows whose destination
  is the same path, before any row runs. That covers a repeated
  `output`, and two derived names that match. For example, `clip.mp4`
  and `clip.mkv` both give `clip_1.png`. The function ignores any other
  columns.

- format:

  A string giving the image file extension used when the function
  derives `output`. The function ignores it when `jobs` has an `output`
  column. (default = `"png"`)

- run:

  A logical: run each frame's command through FFmpeg (`TRUE`, default)
  or only build the commands for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  save frames in parallel with furrr (`TRUE`) or one after another
  (`FALSE`, default). Parallel work follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan.
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
`jobs` with an added `command` column. When `output` was derived, it
also has the resolved `output` column. When `run = TRUE`, it has a
`success` column, plus any columns the forwarded arguments add, such as
`verified`.

## References

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
for the single-frame form.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments passed on through `...`.
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the batch function that cuts segments.

Other task functions:
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
jobs <- tibble::tibble(
  input     = c(video, video),
  output    = c("a.png", "b.png"),
  timestamp = c(0.25, 0.75)
)
# run = FALSE compiles one command per frame without calling FFmpeg
extract_frame_batch(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                                 output timestamp command
#>   <chr>                                                 <chr>      <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sa… a.png       0.25 "-y -s…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sa… b.png       0.75 "-y -s…
```
