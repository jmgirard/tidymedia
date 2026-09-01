# Anonymize Many Videos From a Jobs Table

Cover fixed rectangular regions of many input videos with opaque filled
boxes from a single jobs tibble — the **batch** (table-driven) sibling
of
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
for when you have more than one video to redact. Each row is one input
with its own regions; the required columns name the source (`input`) and
the boxes to cover (`regions`). This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input, sharing the same box-fill
pipeline (and per-region validation) as the scalar verb.

## Usage

``` r
anonymize_video_batch(
  jobs,
  color = "black",
  video_codec = "libx264",
  audio_codec = "copy",
  pixel_format = "yuv420p",
  hardware = c("none", "nvenc"),
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
  itself a data frame of boxes for that input — the same
  `x`/`y`/`width`/`height` (and optional per-box `color`) shape
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  takes. An optional `output` column names the destination; when absent,
  one is derived per row by appending `_anonymized` to each input's
  basename, keeping the input's extension (e.g. `clip.mkv` becomes
  `clip_anonymized.mkv`). Because anonymization is
  one-input-to-one-output, a duplicated `input` with no `output` column
  would collide and is rejected. Each of the four encode knobs —
  `color`, `video_codec`, `audio_codec`, `pixel_format` — may also
  appear as a column to override the corresponding argument on a per-row
  basis; rows (or knobs) that omit the column fall back to the
  argument's value. In either codec column, `NA` leaves that row's codec
  unset (the column form of `video_codec = NULL` /
  `audio_codec = NULL`); in a `color` or `pixel_format` column it is an
  error, because those have no unset state. An `audio_stream` column
  overrides the `audio_stream` argument per row, where `NA` keeps that
  row on every audio track. Any other columns are ignored.

- color:

  A string naming the default fill color (FFmpeg color syntax) applied
  to every row, unless `jobs` carries a `color` column or a box supplies
  its own `color`. (default = `"black"`)

- video_codec:

  A string naming the output video codec applied to every row, unless
  `jobs` carries a `video_codec` column, in which case `NA` in a cell
  leaves that row's codec unset. Default `"libx264"`; `NULL` emits no
  `-codec:v` and lets the output container's default encoder decide (for
  a `.webm` output, pass `audio_codec = NULL` too — the default `"copy"`
  would otherwise carry a codec WebM cannot hold).

- audio_codec:

  A string naming the output audio codec applied to every row, unless
  `jobs` carries an `audio_codec` column, in which case `NA` in a cell
  leaves that row's codec unset. `"copy"` (default) stream-copies the
  audio through untouched; name an encoder (e.g. `"aac"`) when the
  source audio cannot be copied into the output container.

- pixel_format:

  A string naming the output pixel format applied to every row, unless
  `jobs` carries a `pixel_format` column. (default = `"yuv420p"`)

- hardware:

  The encoder backend applied to every row: `"none"` (default, the
  software `video_codec`) or `"nvenc"` for NVIDIA GPU encoding.
  Batch-wide (a machine property), not a per-row column; a `hardware`
  column in `jobs` is ignored. See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md).
  Resolving `"nvenc"` asks this FFmpeg build which encoders it has, so
  the first `"nvenc"` call that re-encodes the video runs the binary
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. Availability is checked at this verb's own front door,
  before any row runs, so an unavailable encoder aborts naming this
  function rather than the internal fan-out it would otherwise be
  reported against. A call that is also wrong about a per-row value — a
  `regions` table missing a required column, say — is refused for the
  value first, whether or not this machine has the encoder.

- fallback:

  A logical applied to every row: when `hardware = "nvenc"` but nvenc is
  unavailable, re-encode with the software `video_codec` and a message
  (`TRUE`) instead of aborting (`FALSE`, default). Batch-wide, not a
  per-row column.

- audio_stream:

  The 0-based index of the audio track to carry into each output,
  counted *among that row's input's audio streams* – `0` is the first
  audio track, `1` the second, whatever their positions among the file's
  streams. `NULL` (default) carries **every** audio track. The argument
  applies to every row lacking an `audio_stream` column; an `NA` cell in
  that column means the same as `NULL` for that row, rather than falling
  back to the argument. The every-track family reads `NULL` this way –
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  plus their `_batch` siblings. The first-track family takes one track
  only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  plus theirs. Subtitle and data streams are not carried either way.
  Naming a track the input does not have is an FFmpeg error, not an R
  one. See
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
`jobs` with an added `command` column (and, when `output` was derived,
the resolved `output` column; when `run = TRUE`, a `success` column,
plus any columns the forwarded arguments add, e.g. `verified`).

## See also

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
for the single-input form;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware = "nvenc"` toggle;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
and
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the other table-driven siblings.

Other task verb functions:
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
