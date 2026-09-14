# Re-encode Many Videos for the Web From a Jobs Table

Re-encode many videos into a widely compatible, web-friendly form, using
one jobs table. This is the **batch** form of
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
for when you have more than one file. Each row is one input. The
function is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It builds one reproducible command for each input. Each command uses the
same fixed H.264, AAC and `+faststart` steps as
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
with no per-row settings. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec and re-encode.

## Usage

``` r
format_for_web_batch(
  jobs,
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

  A data frame with one row per input. It needs at least an `input`
  column, the source path. An optional `output` column names the
  destination. Without it, each row's output name adds `_web` to the
  input's base name, with an `.mp4` extension. The web re-encode always
  writes H.264 in mp4. For example, `clip.mkv` becomes `clip_web.mp4`.
  Two rows with the same destination path are refused before any row
  runs. That happens with a repeated `output`, or with two derived names
  that match. For example, `clip.mov` and `clip.mkv` both give
  `clip_web.mp4`. An optional numeric `audio_stream` column overrides
  the `audio_stream` argument for each row. `NA` keeps every audio track
  in that row. Any other columns are ignored, `video_codec` and
  `audio_codec` included. The sibling batch functions read those two
  columns as per-row overrides, but this one does not. The web recipe
  fixes which codecs the output uses: H.264 video and AAC audio. For
  per-row codecs, use a function that has them, such as
  [`standardize_video_batch`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
  or
  [`crop_video_batch`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md).

- hardware:

  The encoder backend for every row. `"none"` (default) uses software
  libx264. `"nvenc"` uses NVIDIA GPU H.264 encoding, and
  `"videotoolbox"` uses Apple GPU H.264 encoding. It applies to the
  whole batch and is not read as a column. See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md).
  Resolving a hardware backend asks this FFmpeg build which encoders it
  has. So the first such call that re-encodes the video runs FFmpeg
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. This function checks that the encoder is available
  before any row runs. So an unavailable encoder aborts naming this
  function, not the internal step that runs the rows.

- fallback:

  A logical. When a `hardware` other than `"none"` is requested but its
  encoder is unavailable, `TRUE` re-encodes with software libx264 and a
  message. `FALSE` (default) aborts instead. A `video_codec` in a family
  that the backend has no encoder for is a wrong argument, not an absent
  encoder. So it aborts whatever `fallback` says.

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

[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
the single-input form it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
for a configurable re-encode.

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
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
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
jobs <- tibble::tibble(input = c(video, video), output = c("a.mp4", "b.mp4"))
format_for_web_batch(jobs, run = FALSE)
#> # A tibble: 2 × 3
#>   input                                                        output command   
#>   <chr>                                                        <chr>  <chr>     
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 a.mp4  "-y -i \"…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 b.mp4  "-y -i \"…
```
