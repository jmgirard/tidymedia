# Inset one video over another (picture-in-picture)

Composite a smaller `overlay` video onto a `main` video in one corner
(or the center). This is the classic picture-in-picture layout for
pairing a speaker with a screen recording, or a stimulus with a webcam.
Built on the
[`ffm_overlay`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
pipeline function, which resizes the overlay to a fraction of the main
video's width and positions it. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, encoder and stream copy.

## Usage

``` r
picture_in_picture(
  main,
  overlay,
  outfile,
  position = c("topright", "topleft", "bottomright", "bottomleft", "center"),
  scale = 0.25,
  margin = 16,
  audio_input = NULL,
  video_codec = NULL,
  audio_codec = "copy",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  quality = NULL,
  run = TRUE
)
```

## Arguments

- main:

  A string giving the path to the background (full-size) video.

- overlay:

  A string giving the path to the inset video.

- outfile:

  A string giving the path to write the result to.

- position:

  Where to place the inset: one of `"topright"` (default), `"topleft"`,
  `"bottomright"`, `"bottomleft"`, or `"center"`.

- scale:

  The inset's width as a fraction of the main video's width, aspect
  preserved (`0 < scale <= 1`). (default = `0.25`)

- margin:

  The gap in pixels between the inset and the video edges (ignored for
  `position = "center"`). (default = `16`)

- audio_input:

  The input file whose audio to keep, as a number that counts from `0`.
  `0` is the first file you pass and `1` is the second. This counts the
  function's inputs, not the audio tracks of one input. So it is a
  different index from `audio_stream` on the functions that take one
  input. `NULL` (default) selects no audio at all, so the output is
  silent. This differs from `audio_stream = NULL`, which still selects
  audio. An input number the call does not have gives an R error, before
  FFmpeg runs. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md).
  (default = `NULL`)

- video_codec:

  A string naming the output video codec, or `NULL` (default) to leave
  it unset. Then the output container's default encoder is used, and the
  compiled command is the same as one that never named a codec.

- audio_codec:

  A string naming the codec for the carried audio track. `"copy"`
  (default) stream-copies it through untouched. Name an encoder, such as
  `"aac"`, to transcode it. `NULL` leaves the codec unset, so the output
  container's default encoder is used. When `audio_input` is `NULL`, no
  audio reaches the output, so nothing is emitted. Naming an encoder in
  that case is an error.

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
  to discard it.

- fallback:

  A logical. When a `hardware` other than `"none"` is requested but its
  encoder is unavailable, `TRUE` encodes in software with a message.
  `FALSE` (default) aborts instead. With `video_codec = NULL`, the
  fallback leaves the codec unset rather than picking one, so the codec
  never changes silently. A `video_codec` in a family that the backend
  has no encoder for is a wrong argument, not an absent encoder. So it
  aborts whatever `fallback` says.

- quality:

  A number, or `NULL` (default) to leave the encoder's own default in
  place. It is the encoder's own rate-control value, passed through
  unchanged. `libx264` and `libx265` read it as `-crf` (0 to 51). The
  nvenc encoders read it as `-cq` (0 to 51), and the videotoolbox
  encoders as `-q:v` (1 to 100). Each scale is its own: the same number
  means something different on each encoder. A value outside the
  encoder's range is refused. An encoder outside those seven, such as
  `libvpx-vp9`, is refused with `quality` set. So is a `video_codec` of
  `"copy"`, or of `NULL` under `hardware = "none"`. When
  `fallback = TRUE` falls back to software, the value is dropped and the
  message says so, because it belonged to the hardware encoder's scale.

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

Audio is dropped unless `audio_input` names an input to carry (`0` = the
main video, `1` = the overlay). A carried track is stream-copied unless
`audio_codec` names an encoder.

## See also

[`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
the pipeline function it wraps;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` argument;
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
for side-by-side stacking.

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
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
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
picture_in_picture(video, video, "pip.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[1:v][0:v]scale2ref=w='main_w*0.25':h='main_w*0.25*ih/iw'[pip][bg];[bg][pip]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]\" -map \"[vout]\" \"pip.mp4\""
```
