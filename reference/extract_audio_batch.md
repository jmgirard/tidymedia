# Extract Audio From Many Files From a Jobs Table

Pull the audio track out of many input files from a single jobs tibble —
the **batch** (table-driven) sibling of
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
for when you have more than one file. Each row is one input; `input` and
`output` columns are required. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input, sharing the same
map/drop-video pipeline as the scalar verb.

## Usage

``` r
extract_audio_batch(
  jobs,
  audio_codec = "copy",
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path) and an `output` column (destination path). An `output`
  column is **required** — unlike the video batch verbs, an audio
  destination cannot be auto-named because its extension is the
  instruction (it picks the container, and with `audio_codec = "copy"`
  must match the source codec). An optional `audio_codec` column
  overrides the `audio_codec` argument per row; rows omitting it fall
  back to the argument, and `NA` in a cell leaves that row's codec unset
  (the column form of `audio_codec = NULL`). An optional `audio_stream`
  column likewise overrides the `audio_stream` argument per row, where
  `NA` keeps that row on the first audio track. Any other columns are
  ignored.

- audio_codec:

  The audio codec applied to every row unless `jobs` carries an
  `audio_codec` column, in which case `NA` in a cell leaves that row's
  codec unset. `"copy"` (default) stream-copies the audio losslessly;
  name an encoder (e.g. `"aac"`) to transcode; or pass `NULL` to emit no
  `-codec:a` and let the output container's default encoder decide.

- audio_stream:

  The 0-based index of the audio track to take, applied to every row
  unless `jobs` carries an `audio_stream` column, in which case `NA` in
  a cell keeps that row on the first audio track. The index counts
  *among each input's audio streams*. `NULL` (default) takes the first
  audio track.

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

## Details

When a row names no `audio_stream` and its input turns out to carry
tracks the output will not, the verb warns **once** for the whole batch,
naming every affected row. That check is **best-effort**: it runs
FFprobe, so it is emitted when FFprobe is available and the input can be
probed, and is skipped silently otherwise. It never runs under
`run = FALSE`, never changes any compiled command, and is skipped
entirely when every row names a track. Suppress it by class with
`suppressWarnings(classes = "tidymedia_dropped_audio")`.

## See also

[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
the scalar verb it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
to transcode audio in batch.

Other task verb functions:
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
jobs <- tibble::tibble(input = c(video, video), output = c("a.aac", "b.aac"))
extract_audio_batch(jobs, run = FALSE)
#> # A tibble: 2 × 3
#>   input                                                        output command   
#>   <chr>                                                        <chr>  <chr>     
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 a.aac  "-y -i \"…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 b.aac  "-y -i \"…
```
