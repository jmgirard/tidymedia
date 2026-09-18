# Segment Many Videos From a Jobs Table

Cut segments across many input files from a single jobs tibble. This is
the **batch** (table-driven) form of
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
for when your segments span more than one input. Each row is one
segment; the four required columns name its source, destination, and cut
points. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one reproducible compiled command per segment. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, keyframe and stream copy.

## Usage

``` r
segment_video_batch(
  jobs,
  reencode = TRUE,
  video_codec = NULL,
  audio_codec = "copy",
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

  A data frame with one row per segment and (at least) the columns
  `input` (source path), `start` and `end` (cut points). Each cut-point
  column is a numeric column of seconds or a character column with
  time-duration syntax. Two optional columns are recognized: `output`
  (destination path) and `reencode` (a logical; see the `reencode`
  argument). If `output` is absent, the function derives one per row by
  appending `_<n>.<ext>` to each input's basename. The segment number
  restarts at 1 for each input file (the same rule as
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)).
  Two rows given the same `output` path are refused before any row runs.
  A `video_codec` or `audio_codec` column overrides that argument per
  row, with `NA` meaning "leave the codec unset" (the column's way of
  writing the argument's `NULL`). An `audio_stream` column likewise
  overrides that argument per row, with `NA` meaning "keep every audio
  track" (the column's way of writing that argument's `NULL`). Any other
  columns are ignored.

- reencode:

  A logical passed to
  [`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md):
  cut each segment frame-accurately by re-encoding (`TRUE`, default) or
  with a fast, lossless copy that snaps to keyframes (`FALSE`). See
  `ffm_seek` for the trade-off. Applies to every row, unless `jobs`
  carries a `reencode` column, which overrides this argument on a
  per-row basis.

- video_codec:

  A string naming the output video codec, applied to every row lacking a
  `video_codec` column. `NULL` (default) leaves it unset, so each
  segment keeps its container's default encoder. A row can resolve to a
  codec while cutting by stream copy (`reencode = FALSE`, as an argument
  or a column). That row is an error, because no encoder runs on that
  path.

- audio_codec:

  A string naming the output audio codec, applied to every row lacking
  an `audio_codec` column. `"copy"` (default) stream-copies the audio;
  name an encoder to re-encode it, or `NULL` to leave the codec unset. A
  row can resolve to anything but `"copy"` while cutting by stream copy
  (`reencode = FALSE`, as an argument or a column). That row is an
  error. So split a jobs table that mixes stream-copy rows with a
  re-encoding `audio_codec` into separate calls.

- hardware, fallback:

  The encoder backend and its fallback behavior, applied to the whole
  batch. They are a property of the machine, not of a row, so neither is
  read as a `jobs` column. See
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
  Because `hardware` is batch-wide, a non-`"none"` value conflicts with
  a stream-copy row on its own, even one naming no codec. So split a
  jobs table that mixes `reencode = FALSE` rows with GPU encoding into
  separate calls. Resolving a hardware backend asks this FFmpeg build
  which encoders it has. So the first such call that re-encodes the
  video runs FFmpeg while the command is built, even under
  `run = FALSE`. The answer is remembered for the rest of the R session.
  See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. This function checks that the encoder is available
  before any row runs. So an unavailable encoder aborts naming this
  function, not the internal step that runs the rows. A call can also
  contradict itself by asking for GPU encoding on a cut that
  stream-copies. Such a call is refused for the contradiction first,
  whether or not this machine has the encoder. The stream-copy conflict
  named under `reencode` is caught first, so such a call aborts without
  probing.

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

  A logical: run each segment's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  cut segments in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one segment at a time
  and warns. Set a plan first, e.g.
  `future::plan(future::multisession)`.

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

## References

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
for the single-input, parallel-vector form.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`.
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` argument.
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
for the cut trade-off.

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
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("a.mp4", "b.mp4"),
  start  = c(0, 0.5),
  end    = c(0.5, 1)
)
# run = FALSE compiles one command per segment without calling FFmpeg
segment_video_batch(jobs, run = FALSE)
#> # A tibble: 2 × 5
#>   input                                               output start   end command
#>   <chr>                                               <chr>  <dbl> <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/… a.mp4    0     0.5 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/… b.mp4    0.5   1   "-y -i…
```
