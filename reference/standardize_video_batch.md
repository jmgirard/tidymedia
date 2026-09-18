# Standardize Many Videos From a Jobs Table

Re-encode many files to a reproducible format, using one jobs table.
This is the **batch** form of
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
for when you have more than one video to standardize. Each row is one
input, and the only required column names its source. The function is a
thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It builds one reproducible command for each input. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, pixel format and frame rate.

## Usage

``` r
standardize_video_batch(
  jobs,
  width = NULL,
  height = NULL,
  fps = NULL,
  video_codec = "libx264",
  audio_codec = "copy",
  pixel_format = "yuv420p",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  quality = NULL,
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
  destination. Without it, each row's output name adds `_standardized`
  to the input's base name and keeps its extension. For example,
  `clip.mkv` becomes `clip_standardized.mkv`. Two rows naming the same
  output path are refused before any row runs. That is a path repeated
  in the `output` column, or a repeated `input` when there is no
  `output` column. A column can override any of the six format arguments
  for each row: `width`, `height`, `fps`, `video_codec`, `audio_codec`
  and `pixel_format`. An argument with no column applies its value to
  every row. In either codec column, `NA` leaves that row's codec unset.
  That is the column form of `video_codec = NULL` or
  `audio_codec = NULL`. In a `width`, `height`, `fps` or `pixel_format`
  column, `NA` is an error. `pixel_format` has no unset state to
  express. `width`, `height` and `fps` do accept `NULL` as arguments,
  but their columns have no `NA` form for it. An `audio_stream` column
  overrides the `audio_stream` argument for each row, and `NA` keeps
  that row on every audio track. A numeric `quality` column overrides
  the `quality` argument per row (see `quality`). Any other columns are
  ignored.

- width, height:

  Optional target dimensions for every row, unless `jobs` has a column
  of the same name (see `jobs`). When only one is given, the other is
  derived to keep the aspect ratio. When neither is given, the frame is
  floor-cropped to even dimensions, so odd-sized sources encode.
  (default = `NULL`)

- fps:

  Optional target frame rate applied to every row, unless `jobs` carries
  an `fps` column. (default = `NULL`, i.e. leave the frame rate
  unchanged)

- video_codec:

  A string naming the video codec for every row, unless `jobs` has a
  `video_codec` column. In that column, `NA` leaves that row's codec
  unset. The default is `"libx264"`. `NULL` emits no `-codec:v` and lets
  the output container's default encoder decide. For a `.webm` output,
  pass `audio_codec = NULL` too, because the default `"copy"` would
  otherwise carry a codec WebM cannot hold.

- audio_codec:

  A string naming the audio codec for every row, unless `jobs` has an
  `audio_codec` column. In that column, `NA` leaves that row's codec
  unset. `"copy"` (default) stream-copies the audio through untouched.
  Name an encoder, such as `"aac"`, when the source audio cannot be
  copied into the output container.

- pixel_format:

  A string naming the pixel format applied to every row, unless `jobs`
  carries a `pixel_format` column. (default = `"yuv420p"`)

- hardware:

  The encoder backend for every row. `"none"` is the default. `"nvenc"`
  uses NVIDIA GPU encoding (H.264, HEVC and AV1), and `"videotoolbox"`
  uses Apple GPU encoding (H.264 and HEVC). It applies to the whole
  batch and is not read as a column. See
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
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
  encoder is unavailable, `TRUE` re-encodes with the software
  `video_codec` and a message. `FALSE` (default) aborts instead. A
  `video_codec` in a family that the backend has no encoder for is a
  wrong argument, not an absent encoder. So it aborts whatever
  `fallback` says.

- quality:

  A number, or `NULL` (default), applied to each row unless `jobs`
  carries a numeric `quality` column. In that column, `NA` leaves that
  row's encoder default in place, whatever the argument says. The value
  is the encoder's own rate-control value, passed through unchanged.
  Each cell is checked against the encoder its own row resolves to. A
  wrong cell is refused before any row runs, and the error names this
  function and the row. See
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  for the encoders, their flags and ranges, and the values it refuses.

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
  standardize in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
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

[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
for the single-input form;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
and
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
for the other batch task functions.

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
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
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
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("a.mp4", "b.mp4"),
  width  = c(640, 320)
)
# run = FALSE compiles one command per input without calling FFmpeg
standardize_video_batch(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                                     output width command
#>   <chr>                                                     <chr>  <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample… a.mp4    640 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample… b.mp4    320 "-y -i…
```
