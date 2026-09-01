# Inset one video over another (picture-in-picture)

Composite a smaller `overlay` video onto a `main` video in one corner
(or the center) — the classic picture-in-picture layout for pairing a
speaker with a screen recording, or a stimulus with a webcam. Built on
the blessed
[`ffm_overlay`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
verb, which resizes the overlay to a fraction of the main video's width
and positions it.

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
  hardware = c("none", "nvenc"),
  fallback = FALSE,
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

  The 0-based index of the *input* whose audio to keep – `0` is the
  first file passed in, `1` the second. This counts the verb's inputs,
  not one input's audio streams, so it is a different index from
  `audio_stream` on the single-input verbs. `NULL` (default) maps no
  audio at all, so the output is silent – unlike `audio_stream = NULL`,
  which always maps something. Naming an input the call does not have is
  an R error, raised before FFmpeg runs. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md).
  (default = `NULL`)

- video_codec:

  A string naming the output video codec, or `NULL` (default) to leave
  it unset, so the output container's default encoder is used and the
  compiled command is unchanged from one that never named a codec.

- audio_codec:

  A string naming the codec for the carried audio track. `"copy"`
  (default) stream-copies it through untouched; name an encoder (e.g.
  `"aac"`) to transcode it, or pass `NULL` to leave the codec unset so
  the output container's default encoder is used. Nothing is emitted
  when `audio_input` is `NULL`, since no audio reaches the output;
  naming an encoder in that case is an error.

- hardware:

  The encoder backend: `"none"` (default, the software `video_codec`) or
  `"nvenc"` for NVIDIA GPU encoding. When `"nvenc"`, the nvenc encoder
  for `video_codec`'s family is used (e.g. `"libx264"` becomes
  `"h264_nvenc"`); with the default `video_codec = NULL` the H.264
  family is assumed, so a non-H.264 container (e.g. `.webm`) needs an
  explicit HEVC- or AV1-family `video_codec`. See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  for availability and its caveats. Resolving `"nvenc"` asks this FFmpeg
  build which encoders it has, so the first `"nvenc"` call that
  re-encodes the video runs the binary while the command is built, even
  under `run = FALSE`. The answer is remembered for the rest of the R
  session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it.

- fallback:

  A logical: when `hardware = "nvenc"` but nvenc is unavailable, encode
  in software with a message (`TRUE`) instead of aborting (`FALSE`,
  default). With `video_codec = NULL` the fallback leaves the codec
  unset rather than picking one, so the codec never changes silently.

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
the builder it wraps;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware = "nvenc"` toggle;
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
for side-by-side stacking.

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
