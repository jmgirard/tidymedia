# Crop a video to a rectangular region

Crop a video to a rectangular region

## Usage

``` r
crop_video(
  infile,
  outfile,
  width,
  height,
  x = "(in_w-out_w)/2",
  y = "(in_h-out_h)/2",
  video_codec = NULL,
  audio_codec = "copy",
  hardware = c("none", "nvenc"),
  fallback = FALSE,
  audio_stream = NULL,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a video file.

- outfile:

  A string containing the path of the video file to write.

- width:

  The width of the output video, in pixels.

- height:

  The height of the output video, in pixels.

- x:

  The horizontal offset, in pixels, of the left edge of the crop.
  (default = centered)

- y:

  The vertical offset, in pixels, of the top edge of the crop. (default
  = centered)

- video_codec:

  A string naming the output video codec, or `NULL` (default) to leave
  it unset, so the output container's default encoder is used and the
  compiled command is unchanged from one that never named a codec.

- audio_codec:

  A string naming the output audio codec. `"copy"` (default)
  stream-copies the audio through untouched; name an encoder (e.g.
  `"aac"`) to transcode it, or pass `NULL` to leave the codec unset so
  the output container's default encoder is used. Stream-copying fails
  if the output container cannot hold the source audio codec (e.g. FLAC
  in `.mp4`) — name an encoder in that case.

- hardware:

  The encoder backend: `"none"` (default, the software `video_codec`) or
  `"nvenc"` for NVIDIA GPU encoding. When `"nvenc"`, the nvenc encoder
  for `video_codec`'s family is used (e.g. `"libx264"` becomes
  `"h264_nvenc"`); with the default `video_codec = NULL` the H.264
  family is assumed, so a non-H.264 container (e.g. `.webm`) needs an
  explicit HEVC- or AV1-family `video_codec`. See
  [`has_nvenc`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
  for availability and its caveats.

- fallback:

  A logical: when `hardware = "nvenc"` but nvenc is unavailable, encode
  in software with a message (`TRUE`) instead of aborting (`FALSE`,
  default). With `video_codec = NULL` the fallback leaves the codec
  unset rather than picking one, so the codec never changes silently.

- audio_stream:

  The 0-based index of the audio track to carry into the output, counted
  *among the input's audio streams* – `0` is the first audio track, `1`
  the second, whatever their positions among the file's streams. `NULL`
  (default) carries **every** audio track, which is also what
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  do, and differs from
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  whose `NULL` takes the first track only. Naming a track the input does
  not have is an FFmpeg error, not an R one. Subtitle and data streams
  are not carried either way. (default = `NULL`)

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## See also

[`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md),
the builder it wraps;
[`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the `hardware = "nvenc"` toggle;
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
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

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
crop_video(video, "cropped.mp4", width = 160, height = 120, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:a copy -map \"0:v?\" -map \"0:a?\" \"cropped.mp4\""
```
