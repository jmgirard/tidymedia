# Anonymize Many Videos From a Jobs Table

Cover fixed rectangular regions of many input videos with opaque filled
boxes from a single jobs tibble. This is the **batch** (table-driven)
form of
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
for when you have more than one video to redact. Each row is one input
with its own regions. The required columns name the source (`input`) and
the boxes to cover (`regions`). This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It compiles one reproducible command per input. It shares the same
box-fill pipeline (and per-region validation) as
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md).
The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, pixel format and stream copy.

## Usage

``` r
anonymize_video_batch(
  jobs,
  color = "black",
  video_codec = "libx264",
  audio_codec = "copy",
  pixel_format = "yuv420p",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path) and a `regions` list-column. Each `regions` cell is
  itself a data frame of boxes for that input. It has the same shape
  that
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  takes: `x`, `y`, `width`, `height` and an optional per-box `color`. An
  optional `output` column names the destination. When it is absent, the
  function derives one per row. It appends `_anonymized` to each input's
  basename and keeps the input's extension (e.g. `clip.mkv` becomes
  `clip_anonymized.mkv`). Two rows naming the same output path are
  refused before any row runs. That is a path repeated in the `output`
  column, or a repeated `input` when there is no `output` column. Four
  encoding arguments may also appear as a column: `color`,
  `video_codec`, `audio_codec` and `pixel_format`. Such a column
  overrides the corresponding argument on a per-row basis. Rows (or
  arguments) that omit the column fall back to the argument's value. In
  either codec column, `NA` leaves that row's codec unset. This is the
  column form of `video_codec = NULL` / `audio_codec = NULL`. In a
  `color` or `pixel_format` column `NA` is an error, because those have
  no unset state. An `audio_stream` column overrides the `audio_stream`
  argument per row, where `NA` keeps that row on every audio track. Any
  other columns are ignored.

- color:

  A string naming the default fill color (FFmpeg color syntax) applied
  to every row. A `color` column in `jobs`, or a box that supplies its
  own `color`, overrides it. (default = `"black"`)

- video_codec:

  A string naming the output video codec applied to every row, unless
  `jobs` carries a `video_codec` column. In that column, `NA` in a cell
  leaves that row's codec unset. The default is `"libx264"`. `NULL`
  emits no `-codec:v` and lets the output container's default encoder
  decide. For a `.webm` output, pass `audio_codec = NULL` too, because
  the default `"copy"` would otherwise carry a codec WebM cannot hold.

- audio_codec:

  A string naming the output audio codec applied to every row, unless
  `jobs` carries an `audio_codec` column. In that column, `NA` in a cell
  leaves that row's codec unset. `"copy"` (default) stream-copies the
  audio through untouched. Name an encoder (e.g. `"aac"`) when the
  source audio cannot be copied into the output container.

- pixel_format:

  A string naming the output pixel format applied to every row, unless
  `jobs` carries a `pixel_format` column. (default = `"yuv420p"`)

- hardware:

  The encoder backend applied to every row. `"none"` (default) uses the
  software `video_codec`. `"nvenc"` gives NVIDIA GPU encoding (H.264,
  HEVC and AV1). `"videotoolbox"` gives Apple GPU encoding (H.264 and
  HEVC). Batch-wide (a machine property), not a per-row column; a
  `hardware` column in `jobs` is ignored. See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md).
  Resolving a hardware backend asks this FFmpeg build which encoders it
  has. So the first such call that re-encodes the video runs FFmpeg
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. This function checks that the encoder is available
  before any row runs. So an unavailable encoder aborts naming this
  function, not the internal step that runs the rows. A call can also be
  wrong about a per-row value, for example a `regions` table that is
  missing a required column. The function refuses that call for the
  value first, whether or not this machine has the encoder.

- fallback:

  A logical applied to every row. When a `hardware` other than `"none"`
  is requested but its encoder is unavailable, `TRUE` re-encodes with
  the software `video_codec` and a message. `FALSE` (default) aborts
  instead. It is batch-wide, not a per-row column. A `video_codec` in a
  family that the backend has no encoder for is a wrong argument, not an
  absent encoder. So it aborts whatever `fallback` says.

- audio_stream:

  The audio track to carry into each output, as a number that counts
  from `0` among the *audio tracks* of each row's input. `0` is the
  first audio track and `1` is the second. Other streams in the file,
  such as video, do not count. `NULL` (default) carries **every** audio
  track. Without an `audio_stream` column, the argument applies to every
  row. An `NA` cell in that column means `NULL` for that row. It does
  not fall back to the argument. The every-track family reads `NULL` as
  every audio track:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and their `_batch` forms. The first-track family reads it as the first
  audio track only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  and their `_batch` forms. The function does not carry subtitle or data
  streams in either case. A track the input does not have gives an
  FFmpeg error, not an R one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio_input`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  anonymize in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
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
`jobs` with an added `command` column. When `output` was derived, it
also has the resolved `output` column. When `run = TRUE`, it has a
`success` column, plus any columns the forwarded arguments add, such as
`verified`.

## See also

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
for the single-input form.
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` argument.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`.
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
and
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the other table-driven functions.

Other task functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
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
[`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
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
jobs <- tibble::tibble(
  input   = c(video, video),
  output  = c("a.mp4", "b.mp4"),
  regions = list(
    data.frame(x = 10, y = 10, width = 120, height = 90),
    data.frame(x = 200, y = 150, width = 80, height = 60)
  )
)
# run = FALSE compiles one command per input without calling FFmpeg
anonymize_video_batch(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                                   output regions command
#>   <chr>                                                   <chr>  <list>  <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/samp… a.mp4  <df>    "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/samp… b.mp4  <df>    "-y -i…
```
