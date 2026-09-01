# Crop Many Videos From a Jobs Table

Crop many input videos to a rectangular region from a single jobs tibble
— the **batch** (table-driven) sibling of
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
for when you have more than one file. Each row is one input. This is a
thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input, sharing the same crop
pipeline as the scalar verb. Each row's geometry values are checked at
this verb's own front door, so a bad cell is refused – naming this
function – before any command runs.

## Usage

``` r
crop_video_batch(
  jobs,
  width = NULL,
  height = NULL,
  x = "(in_w-out_w)/2",
  y = "(in_h-out_h)/2",
  video_codec = NULL,
  audio_codec = "copy",
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
  (source path). An optional `output` column names the destination; when
  absent, one is derived per row by appending `_cropped` to each input's
  basename, keeping the input's extension (e.g. `clip.mp4` becomes
  `clip_cropped.mp4`). Each crop dimension — `width`, `height`, `x`, `y`
  — may also appear as a column to override the corresponding argument
  per row; rows (or dimensions) omitting the column fall back to the
  argument. A `video_codec` column overrides that argument per row, with
  `NA` meaning "leave the codec unset" (the column's way of writing the
  argument's `NULL`); an `audio_codec` column works the same way. An
  `audio_stream` column overrides that argument per row, with `NA`
  meaning "keep every audio track" (the column's way of writing that
  argument's `NULL`). Any two rows that resolve to the same output path
  are rejected. Any other columns are ignored.

- width, height:

  The output crop size in pixels, applied to every row unless `jobs`
  carries a column of the same name. Required: pass each as an argument
  or supply the column (there is no default crop size).

- x, y:

  The offset in pixels of the crop's left/top edge, applied to every row
  unless `jobs` carries a column of the same name. Default: centered.

- video_codec:

  A string naming the output video codec, applied to every row lacking a
  `video_codec` column, or `NULL` (default) to leave it unset so each
  output keeps its container's default encoder.

- audio_codec:

  A string naming the output audio codec, applied to every row lacking
  an `audio_codec` column. `"copy"` (default) stream-copies the audio;
  name an encoder to transcode it, or `NULL` to leave the codec unset so
  each output keeps its container's default encoder.

- hardware, fallback:

  The encoder backend and its fallback behavior, applied to the whole
  batch (a property of the machine, not of a row, so neither is read as
  a `jobs` column). See
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md).
  Resolving `"nvenc"` asks this FFmpeg build which encoders it has, so
  the first `"nvenc"` call that re-encodes the video runs the binary
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. Availability is checked at this verb's own front door,
  before any row runs, so an unavailable encoder aborts naming this
  function rather than the internal fan-out it would otherwise be
  reported against. A call that is also wrong about a per-row value — a
  `width` or `height` that is neither a positive number nor an FFmpeg
  expression — is refused for the value first, whether or not this
  machine has the encoder.

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

## See also

[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
the scalar verb it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware = "nvenc"` toggle;
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
to re-encode in batch.

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
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
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
jobs <- tibble::tibble(input = c(video, video), output = c("a.mp4", "b.mp4"),
                       width = c(160, 80), height = c(120, 60))
crop_video_batch(jobs, run = FALSE)
#> # A tibble: 2 × 5
#>   input                                              output width height command
#>   <chr>                                              <chr>  <dbl>  <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata… a.mp4    160    120 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata… b.mp4     80     60 "-y -i…
```
