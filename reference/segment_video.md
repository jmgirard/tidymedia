# Segment Video

Use FFmpeg to quickly break a single video file into multiple smaller
video files (with the same encoding) based on pairs of start and stop
timestamps. Segment video files will be named by taking the name of
`infile` and appending a suffix of an underscore (\_) and an integer
indicating which segment (based on the order provided in `start` and
`end`).

## Usage

``` r
segment_video(
  infile,
  start,
  end,
  outfiles = NULL,
  reencode = TRUE,
  video_codec = NULL,
  audio_codec = "copy",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE
)
```

## Arguments

- infile:

  A string containing the path to a video file.

- start:

  A vector containing one or more timestamps indicating the start of
  each segment to create. Can be either a numeric vector indicating
  seconds or a character vector with time duration syntax. Must have the
  same length as `end`.

- end:

  A vector containing one or more timestamps indicating the stop of each
  segment to create. Can be either a numeric vector indicating seconds
  or a character vector with time duration syntax. Must have the same
  length as `start`.

- outfiles:

  Either NULL or a character vector indicating the filename (with
  extension) for each segment to create. If NULL, will append a
  zero-padded integer to `infile`. If not NULL, must have the same
  length as `start`, and each element must be a single string – so a
  list of strings is accepted as well as a character vector, and a
  missing value or a number in any position is refused by this function
  rather than by the per-segment fan-out below it. Two segments given
  the same path are refused before any segment is cut.

- reencode:

  A logical passed to
  [`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md):
  cut each segment frame-accurately by re-encoding (`TRUE`, default) or
  with a fast, lossless copy that snaps to keyframes (`FALSE`). See
  `ffm_seek` for the trade-off.

- video_codec:

  A string naming the output video codec, or `NULL` (default) to leave
  it unset. Then the output container's default encoder is used, and the
  compiled command is the same as one that never named a codec. A stream
  copy runs no encoder, so naming a codec (or a `hardware` backend)
  alongside `reencode = FALSE` is an error.

- audio_codec:

  A string naming the output audio codec. `"copy"` (default)
  stream-copies the audio through untouched. Name an encoder, such as
  `"aac"`, to transcode it. `NULL` leaves the codec unset, so the output
  container's default encoder is used. A stream copy
  (`reencode = FALSE`) always copies the audio, so any other value is an
  error there. Stream-copying fails if the output container cannot hold
  the source audio codec (e.g. FLAC in `.mp4`) — name an encoder
  instead.

- hardware:

  The encoder backend. `"none"` (default) uses the software
  `video_codec`. `"nvenc"` uses NVIDIA GPU encoding (H.264, HEVC and
  AV1), and `"videotoolbox"` uses Apple GPU encoding (H.264 and HEVC). A
  backend uses its own encoder for the family of `video_codec`. For
  example, `"libx264"` becomes `"h264_nvenc"` or `"h264_videotoolbox"`.
  With the default `video_codec = NULL`, the H.264 family is assumed. So
  a non-H.264 container, such as `.webm`, needs an explicit HEVC- or
  AV1-family `video_codec` (AV1 only under `"nvenc"`). See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  for availability and its caveats. Resolving a hardware backend asks
  this FFmpeg build which encoders it has. So the first such call that
  re-encodes the video runs FFmpeg while the command is built, even
  under `run = FALSE`. The answer is remembered for the rest of the R
  session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. This function checks that the encoder is available
  before any row runs. So an unavailable encoder aborts naming this
  function, not the internal step that runs the rows. A call can also
  contradict itself by asking for GPU encoding on a cut that
  stream-copies. Such a call is refused for the contradiction first,
  whether or not this machine has the encoder. The stream-copy conflict
  named under `reencode` is caught first, so such a call aborts without
  probing.

- fallback:

  A logical. When a `hardware` other than `"none"` is requested but its
  encoder is unavailable, `TRUE` encodes in software with a message.
  `FALSE` (default) aborts instead. With `video_codec = NULL`, the
  fallback leaves the codec unset rather than picking one, so the codec
  never changes silently. A `video_codec` in a family that the backend
  has no encoder for is a wrong argument, not an absent encoder. So it
  aborts whatever `fallback` says.

- audio_stream:

  The audio track to carry into the output, as a number that counts from
  `0` among the *audio tracks* of the input. `0` is the first audio
  track and `1` is the second. Other streams in the file, such as video,
  do not count. `NULL` (default) carries **every** audio track. The
  every-track family reads `NULL` this way:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  `segment_video` and
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

  A logical: run each segment's command (`TRUE`, default) or only
  compile them (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  cut segments in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan;
  `TRUE` under the default sequential plan runs one segment at a time
  and warns. Set a plan first, e.g.
  `future::plan(future::multisession)`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
one row per segment with its `command` (and, when `run = TRUE`,
`success`).

## References

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
the builder it uses to cut;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the runner;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` toggle;
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the many-file form.

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
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Two segments; run = FALSE compiles one command per segment
segment_video(video, start = c(0, 0.5), end = c(0.5, 1), run = FALSE)
#> # A tibble: 2 × 5
#>   input                                               output start   end command
#>   <chr>                                               <chr>  <dbl> <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/… /home…   0     0.5 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/… /home…   0.5   1   "-y -i…
```
