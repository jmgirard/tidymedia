# Separate Audio and Video for Many Files From a Jobs Table

Split the audio and video streams of many input files from a single jobs
tibble — the **batch** (table-driven) sibling of
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
for when you have more than one file. Each row is one input that fans
out into **two** outputs; `input`, `audiofile`, and `videofile` columns
are all required. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
every input row is reshaped into two single-output jobs (one per
stream), so a jobs table of `N` rows returns `2N` rows — one
reproducible compiled command per stream — sharing the same per-stream
map/stream-copy pipeline as the scalar verb.

## Usage

``` r
separate_audio_video_batch(
  jobs,
  reencode = FALSE,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path) plus `audiofile` and `videofile` columns naming the two
  destinations. All three are **required** — like
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  this verb derives no output paths, because a copied stream's container
  extension is the instruction (it must match the source codec). An
  optional `reencode` column (logical) overrides the `reencode` argument
  per row; rows omitting it fall back to the argument. Any other columns
  are ignored.

- reencode:

  A logical applied to every row unless `jobs` carries a `reencode`
  column: stream-copy each output losslessly (`FALSE`, default) or
  re-encode it to match the output extension (`TRUE`). See
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  for the trade-off.

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

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with **two rows per input** (one per stream): the reshaped `input`, a
single `output` path, a `stream` marker (`"audio"` or `"video"`), and an
added `command` column — plus, when `run = TRUE`, a `success` column
(and `verified` / provenance manifest when requested via `...`). The
columns match the other `_batch` verbs' output plus the `stream` marker.
See
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

## See also

[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
the scalar verb it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the other fan-out batch verb.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
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
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input     = c(video, video),
  audiofile = c("a1.aac", "a2.aac"),
  videofile = c("v1.mp4", "v2.mp4")
)
# run = FALSE compiles two commands per input without calling FFmpeg
separate_audio_video_batch(jobs, run = FALSE)
#> # A tibble: 4 × 4
#>   input                                                    output stream command
#>   <chr>                                                    <chr>  <chr>  <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… a1.aac audio  "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… v1.mp4 video  "-y -i…
#> 3 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… a2.aac audio  "-y -i…
#> 4 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… v2.mp4 video  "-y -i…
```
