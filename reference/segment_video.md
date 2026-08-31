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
  hardware = c("none", "nvenc"),
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
  rather than by the per-segment fan-out below it.

- reencode:

  A logical passed to
  [`ffm_seek`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md):
  cut each segment frame-accurately by re-encoding (`TRUE`, default) or
  with a fast, lossless copy that snaps to keyframes (`FALSE`). See
  `ffm_seek` for the trade-off.

- video_codec:

  A string naming the output video codec, or `NULL` (default) to leave
  it unset, so the output container's default encoder is used and the
  compiled command is unchanged from one that never named a codec. A
  stream copy runs no encoder, so naming a codec (or a `hardware`
  backend) alongside `reencode = FALSE` is an error.

- audio_codec:

  A string naming the output audio codec. `"copy"` (default)
  stream-copies the audio through untouched; name an encoder (e.g.
  `"aac"`) to transcode it, or pass `NULL` to leave the codec unset so
  the output container's default encoder is used. A stream copy
  (`reencode = FALSE`) always copies the audio, so any other value is an
  error there. Stream-copying fails if the output container cannot hold
  the source audio codec (e.g. FLAC in `.mp4`) — name an encoder
  instead.

- hardware:

  The encoder backend: `"none"` (default, the software `video_codec`) or
  `"nvenc"` for NVIDIA GPU encoding. When `"nvenc"`, the nvenc encoder
  for `video_codec`'s family is used (e.g. `"libx264"` becomes
  `"h264_nvenc"`); with the default `video_codec = NULL` the H.264
  family is assumed, so a non-H.264 container (e.g. `.webm`) needs an
  explicit HEVC- or AV1-family `video_codec`. See
  [`has_nvenc`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
  for availability and its caveats. Resolving `"nvenc"` asks this FFmpeg
  build which encoders it has, so the first `"nvenc"` call that
  re-encodes the video runs the binary while the command is built, even
  under `run = FALSE`. The answer is remembered for the rest of the R
  session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. Availability is checked at this verb's own front door,
  before any row runs, so an unavailable encoder aborts naming this
  function rather than the internal fan-out it would otherwise be
  reported against. A call that also contradicts itself — asking for GPU
  encoding on a cut that stream-copies — is refused for the
  contradiction first, whether or not this machine has the encoder. The
  stream-copy conflict named under `reencode` is caught first, so such a
  call aborts without probing.

- fallback:

  A logical: when `hardware = "nvenc"` but nvenc is unavailable, encode
  in software with a message (`TRUE`) instead of aborting (`FALSE`,
  default). With `video_codec = NULL` the fallback leaves the codec
  unset rather than picking one, so the codec never changes silently.

- audio_stream:

  The 0-based index of the audio track to carry into the output, counted
  *among the input's audio streams* – `0` is the first audio track, `1`
  the second, whatever their positions among the file's streams. `NULL`
  (default) carries **every** audio track. The every-track family reads
  `NULL` this way –
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  `segment_video` and
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

  A logical: run each segment's command (`TRUE`, default) or only
  compile them (`FALSE`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  cut segments in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://rdrr.io/pkg/future/man/plan.html) plan; `TRUE`
  under the default sequential plan runs one segment at a time and
  warns. Set a plan first, e.g. `future::plan(future::multisession)`.

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
[`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the `hardware = "nvenc"` toggle;
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the many-file form.

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
