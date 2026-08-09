# Inset One Video Over Another For Many Outputs From a Jobs Table

Composite an inset (overlay) video onto a main video for many outputs
from a single jobs tibble — the **batch** (table-driven) sibling of
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
for when you have more than one to produce. Its two inputs have distinct
roles, so `jobs` carries fixed `main` and `overlay` columns (not a
list-column; D015) plus an `output` column. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible overlay command per row, sharing the pipeline with the
scalar verb.

## Usage

``` r
picture_in_picture_batch(
  jobs,
  position = c("topright", "topleft", "bottomright", "bottomleft", "center"),
  scale = 0.25,
  margin = 16,
  audio = NULL,
  video_codec = NULL,
  audio_codec = "copy",
  hardware = c("none", "nvenc"),
  fallback = FALSE,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per output and (at least) `main` (background
  path), `overlay` (inset path), and `output` (destination path)
  columns. Optional `position`, `scale`, `margin`, `audio`,
  `video_codec`, and `audio_codec` columns override the like-named
  arguments per row (a row omitting one falls back to the argument). In
  an `audio` column, `NA` means "drop audio" (the column's way of
  writing the scalar's `NULL`); in a `video_codec` or `audio_codec`
  column it means "leave the codec unset". Any two rows resolving to the
  same output path are rejected; other columns are ignored.

- position, scale, margin:

  Defaults applied to every row lacking the corresponding column.
  `position` is one of `"topright"` (the default), `"topleft"`,
  `"bottomright"`, `"bottomleft"` or `"center"`; a `position` column is
  held to those same five values, per row. See
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  for their fuller meaning.

- audio:

  The 0-based index of the *input* whose audio to keep – `0` is the
  first file passed in, `1` the second. This counts the verb's inputs,
  not one input's audio streams, so it is a different index from
  `audio_stream` on the single-input verbs. `NULL` (default) maps no
  audio at all, so the output is silent – unlike `audio_stream = NULL`,
  which always maps something. Naming an input the call does not have is
  an R error, raised before FFmpeg runs. Applied to every row lacking an
  `audio` column; an `NA` cell in that column means the same as `NULL`
  for that row, dropping that output's audio. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md).
  (default = `NULL`)

- video_codec:

  A string naming the output video codec, applied to every row lacking a
  `video_codec` column, or `NULL` (default) to leave it unset so each
  output keeps its container's default encoder.

- audio_codec:

  A string naming the codec for the carried audio track, applied to
  every row lacking an `audio_codec` column. `"copy"` (default)
  stream-copies it; name an encoder to transcode it, or `NULL` to leave
  the codec unset. A row carrying no audio emits no `-codec:a`; naming
  an encoder on such a row is an error.

- hardware, fallback:

  The encoder backend and its fallback behavior, applied to the whole
  batch (a property of the machine, not of a row, so neither is read as
  a `jobs` column). See
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  Resolving `"nvenc"` asks this FFmpeg build which encoders it has, so
  the first `"nvenc"` call that re-encodes the video runs the binary
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. Availability is checked at this verb's own front door,
  before any row runs, so an unavailable encoder aborts naming this
  function rather than the internal fan-out it would otherwise be
  reported against. A call that also contradicts itself — naming an
  `audio_codec` with no audio carried into the output — is refused for
  the contradiction first, whether or not this machine has the encoder.
  A per-row value error — a negative `margin`, an `audio` index outside
  the two inputs, a `position` outside the five accepted values —
  likewise reports ahead of the encoder check. A value error and a
  contradiction resolve the same way whether the value arrived as an
  argument or in a `jobs` column; the contradiction reports first.

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

[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
the scalar verb it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the `hardware = "nvenc"` toggle;
[`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md)
and
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
the other fan-in batch siblings.

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
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(main = video, overlay = video, output = "pip.mp4")
picture_in_picture_batch(jobs, run = FALSE)
#> # A tibble: 1 × 4
#>   main                                                    overlay output command
#>   <chr>                                                   <chr>   <chr>  <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/samp… /home/… pip.m… "-y -i…
```
