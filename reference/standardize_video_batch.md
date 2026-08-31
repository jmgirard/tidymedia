# Standardize Many Videos From a Jobs Table

Re-encode many input files to a reproducible format from a single jobs
tibble — the **batch** (table-driven) sibling of
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
for when you have more than one video to standardize. Each row is one
input; the only required column names its source. This is a thin wrapper
over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per input.

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
  absent, one is derived per row by appending `_standardized` to each
  input's basename, keeping the input's extension (e.g. `clip.mkv`
  becomes `clip_standardized.mkv`). Because standardization is
  one-input-to-one-output, a duplicated `input` with no `output` column
  would collide and is rejected. Each of the six standardization knobs —
  `width`, `height`, `fps`, `video_codec`, `audio_codec`, `pixel_format`
  — may also appear as a column to override the corresponding argument
  on a per-row basis; rows (or knobs) that omit the column fall back to
  the argument's value. In either codec column, `NA` leaves that row's
  codec unset (the column form of `video_codec = NULL` /
  `audio_codec = NULL`); in a `width`, `height`, `fps` or `pixel_format`
  column it is an error. `pixel_format` has no unset state to express;
  `width`, `height` and `fps` do accept `NULL` as arguments, but their
  columns have no `NA` spelling for it. An `audio_stream` column
  overrides the `audio_stream` argument per row, where `NA` keeps that
  row on every audio track. Any other columns are ignored.

- width, height:

  Optional target dimensions applied to every row, unless `jobs` carries
  a column of the same name (see `jobs`). When only one is given the
  other is derived to preserve aspect ratio; when neither is given the
  frame is floor-cropped to even dimensions so odd-sized sources encode.
  (default = `NULL`)

- fps:

  Optional target frame rate applied to every row, unless `jobs` carries
  an `fps` column. (default = `NULL`, i.e. leave the frame rate
  unchanged)

- video_codec:

  A string naming the video codec applied to every row, unless `jobs`
  carries a `video_codec` column, in which case `NA` in a cell leaves
  that row's codec unset. Default `"libx264"`; `NULL` emits no
  `-codec:v` and lets the output container's default encoder decide (for
  a `.webm` output, pass `audio_codec = NULL` too — the default `"copy"`
  would otherwise carry a codec WebM cannot hold).

- audio_codec:

  A string naming the audio codec applied to every row, unless `jobs`
  carries an `audio_codec` column, in which case `NA` in a cell leaves
  that row's codec unset. `"copy"` (default) stream-copies the audio
  through untouched; name an encoder (e.g. `"aac"`) when the source
  audio cannot be copied into the output container.

- pixel_format:

  A string naming the pixel format applied to every row, unless `jobs`
  carries a `pixel_format` column. (default = `"yuv420p"`)

- hardware:

  The encoder backend applied to every row: `"none"` (default) or
  `"nvenc"` for NVIDIA GPU encoding. Batch-wide (not a per-row column).
  See
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`has_nvenc`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md).
  Resolving `"nvenc"` asks this FFmpeg build which encoders it has, so
  the first `"nvenc"` call that re-encodes the video runs the binary
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. Availability is checked at this verb's own front door,
  before any row runs, so an unavailable encoder aborts naming this
  function rather than the internal fan-out it would otherwise be
  reported against.

- fallback:

  A logical: when `hardware = "nvenc"` but nvenc is unavailable,
  re-encode with the software `video_codec` and a message (`TRUE`)
  instead of aborting (`FALSE`, default).

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
  for how this differs from `audio`, the input index on
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
  [`future`](https://rdrr.io/pkg/future/man/plan.html) plan; `TRUE`
  under the default sequential plan runs one input at a time and warns.
  Set a plan first, e.g. `future::plan(future::multisession)`.

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

[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
for the single-input form;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`;
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
and
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
for the other table-driven siblings.

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
