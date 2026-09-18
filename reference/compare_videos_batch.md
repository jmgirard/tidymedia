# Build Many Comparison Videos From a Jobs Table

Stack videos side by side for many outputs from a single jobs tibble.
This is the **batch** (table-driven) form of
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
for when you have more than one comparison to produce. Each row carries
an `inputs` list-column (each cell two or more video paths) plus an
`output` column. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible stacking command per row, sharing the pipeline with
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md).
The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, encoder and stream copy.

## Usage

``` r
compare_videos_batch(
  jobs,
  direction = c("horizontal", "vertical"),
  resize = TRUE,
  audio_input = NULL,
  video_codec = NULL,
  audio_codec = "copy",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per output and (at least) an `inputs`
  list-column and an `output` column (destination path). Each `inputs`
  cell is a character vector of **two or more** video paths. Optional
  `direction`, `resize`, `audio_input`, `video_codec`, and `audio_codec`
  columns override the like-named arguments per row (a row omitting one
  falls back to the argument). In an `audio_input` column, `NA` means
  "drop audio", the column's way of writing the scalar's `NULL`. In a
  `video_codec` or `audio_codec` column, it means "leave the codec
  unset". Two rows given the same `output` path are refused before any
  row runs; other columns are ignored.

- direction, resize:

  Defaults applied to every row lacking the corresponding column.
  `direction` is `"horizontal"` (the default) or `"vertical"`; a
  `direction` column is held to the same two values, per row. See
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  for their fuller meaning.

- audio_input:

  The input file whose audio to keep, as a number that counts from `0`.
  `0` is the first file you pass and `1` is the second. This counts the
  function's inputs, not the audio tracks of one input. So it is a
  different index from `audio_stream` on the functions that take one
  input. `NULL` (default) selects no audio at all, so the output is
  silent. This differs from `audio_stream = NULL`, which still selects
  audio. An input number the call does not have gives an R error, before
  FFmpeg runs. Without an `audio_input` column, the argument applies to
  every row. An `NA` cell in that column means `NULL` for that row, so
  that output has no audio. Each row's value is validated against that
  row's input count. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md).
  (default = `NULL`)

- video_codec:

  A string naming the output video codec, applied to every row lacking a
  `video_codec` column. `NULL` (default) leaves it unset, so each output
  keeps its container's default encoder.

- audio_codec:

  A string naming the codec for the carried audio track, applied to
  every row lacking an `audio_codec` column. `"copy"` (default)
  stream-copies it. Name an encoder to re-encode it, or `NULL` to leave
  the codec unset. A row carrying no audio emits no `-codec:a`, and
  naming an encoder on such a row is an error.

- hardware, fallback:

  The encoder backend and its fallback behavior, applied to the whole
  batch. They are a property of the machine, not of a row, so neither is
  read as a `jobs` column. See
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md).
  Resolving a hardware backend asks this FFmpeg build which encoders it
  has. So the first such call that re-encodes the video runs FFmpeg
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. This function checks that the encoder is available
  before any row runs. So an unavailable encoder aborts naming this
  function, not the internal step that runs the rows. A call can also
  contradict itself by naming an `audio_codec` with no audio carried
  into the output. Such a call is refused for the contradiction first,
  whether or not this machine has the encoder. A per-row value error
  likewise reports ahead of the encoder check. Examples are an
  `audio_input` index past that row's input count, and a `direction`
  outside the two accepted values. A value error and a contradiction
  resolve the same way whether the value arrived as an argument or in a
  `jobs` column. The contradiction reports first.

- run:

  A logical: run each command through FFmpeg (`TRUE`, default) or only
  compile them for inspection (`FALSE`).

- parallel:

  A logical: process the jobs in parallel with furrr (`TRUE`) or one at
  a time (`FALSE`, default). See
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  for the future plan requirement.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (e.g. `verify`, `manifest`, `progress`).

## Value

The `jobs` tibble with an added `command` column. When `run = TRUE`, it
also has a `success` column, plus `verified` or a provenance manifest,
each when requested through `...`. See
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

## See also

[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
the one-output function it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` argument.
[`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md)
and
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
the other batch functions that take several inputs per row.

Other task functions:
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

Other audio selection functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

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
