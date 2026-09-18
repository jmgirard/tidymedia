# Re-encode a video for web playback

Re-encode a video into a widely compatible, web-friendly form: H.264
video with `yuv420p` and `+faststart`, and AAC audio. Odd dimensions are
padded down to even values, as the codec requires. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, pixel format and re-encode.

## Usage

``` r
format_for_web(
  infile,
  outfile,
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  quality = NULL,
  audio_stream = NULL,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a video file.

- outfile:

  A string containing the path of the video file to write.

- hardware:

  The encoder backend. `"none"` (default) uses software libx264.
  `"nvenc"` uses NVIDIA GPU H.264 encoding (`"h264_nvenc"`), and
  `"videotoolbox"` uses Apple GPU H.264 encoding
  (`"h264_videotoolbox"`). The backend you name is the one used. An
  unavailable one aborts unless `fallback = TRUE`. See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md).
  Resolving a hardware backend asks this FFmpeg build which encoders it
  has. So the first such call that re-encodes the video runs FFmpeg
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it.

- fallback:

  A logical. When a `hardware` other than `"none"` is requested but its
  encoder is unavailable, `TRUE` re-encodes with software libx264 and a
  message. `FALSE` (default) aborts instead. A `video_codec` in a family
  that the backend has no encoder for is a wrong argument, not an absent
  encoder. So it aborts whatever `fallback` says.

- quality:

  A number, or `NULL` (default) to leave the encoder's own default in
  place. It is the encoder's own rate-control value, passed through
  unchanged. `libx264` reads it as `-crf` (0 to 51). `h264_nvenc` reads
  it as `-cq` (0 to 51), and `h264_videotoolbox` as `-q:v` (1 to 100).
  Each scale is its own: the same number means something different on
  each encoder. A value outside the encoder's range is refused. The
  encoder is `libx264`, `h264_nvenc` or `h264_videotoolbox`, as
  `hardware` chooses. When `fallback = TRUE` falls back to software, the
  value is dropped and the message says so, because it belonged to the
  hardware encoder's scale.

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
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and `format_for_web`, and their `_batch` forms. The first-track family
  reads it as the first audio track only:
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

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## See also

[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
and
[`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md),
among the pipeline functions it wraps;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` toggle;
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
for a configurable re-encode;
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md)
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
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
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
format_for_web(video, "web.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 -codec:a aac -pix_fmt yuv420p -movflags +faststart -map \"0:v?\" -map \"0:a?\" \"web.mp4\""
```
