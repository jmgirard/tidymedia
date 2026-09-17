# Sample frames from many videos at a fixed rate from a jobs table

Sample many videos into numbered image sequences, using one jobs table.
This is the **batch** form of
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md).
Each row is one input video, sampled at a fixed rate into its own image
sequence. The function is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It builds one reproducible command for each input. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as frame rate.

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

  A data frame with one row per input. It needs at least an `input`
  column (source path). An optional `outdir` column gives the output
  directory for that row's sequence. When it is absent, the function
  derives one as `<input-base>_frames` beside each input. Optional `fps`
  and `interval` columns override the rate per row. The function ignores
  any other columns. The function refuses two rows whose image sequences
  would share a file-name pattern, before any row runs. Two rows share a
  pattern when they have the same output directory path and the same
  input file name without its extension. The directory path can come
  from the column, from the `outdir` argument, or from the derived name.

- fps, interval:

  The sampling rate applied to every row, as in
  [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md).
  A per-row column of the same name overrides it. Supply exactly one of
  the two (as an argument or a column). (default = `NULL`)

- outdir:

  An optional single output directory for all rows. An `outdir` column
  overrides it. When both are absent, the function derives one directory
  per input. (default = `NULL`)

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
  sample in parallel with furrr (`TRUE`) or one after another (`FALSE`,
  default). Parallel work follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan.
  `TRUE` under the default sequential plan runs one at a time and warns.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  such as `verify`, `manifest`, `checksums`, and `progress`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
`jobs` with an added `command` column. When `outdir` was derived, it
also has the resolved `outdir` column. When `run = TRUE`, it has a
`success` column, plus any columns the forwarded arguments add, such as
`verified`.

## Details

Supply the sampling rate once as the single `fps` or `interval`
argument, which applies to every row. Or supply it per row as an `fps`
or `interval` column, which overrides the argument of the same name.
Supply exactly one of the two, fps *or* interval, across arguments and
columns.

## See also

[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
for the single-video form.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments passed on through `...`.
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
for the batch function that takes a list of frames.

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
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
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
