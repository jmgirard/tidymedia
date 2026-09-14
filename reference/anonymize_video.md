# Cover fixed regions of a video with opaque boxes

Anonymize a video by covering one or more fixed rectangular regions with
opaque filled boxes – for example, to redact a face, a name badge, or a
screen that stays in one place for the whole clip. The regions are fixed
(there is no face or object tracking), so this suits footage where the
areas to cover do not move.

## Usage

``` r
anonymize_video(
  infile,
  outfile,
  regions,
  color = "black",
  video_codec = "libx264",
  audio_codec = "copy",
  pixel_format = "yuv420p",
  hardware = c("none", "nvenc", "videotoolbox"),
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

- regions:

  A data frame with one row per box and columns `x`, `y`, `width`,
  `height` (and optionally `color`); see Details.

- color:

  A string naming the default fill color in FFmpeg color syntax, used
  for any row without its own `color` (default `"black"`).

- video_codec:

  A string naming the output video codec (default `"libx264"`). `NULL`
  emits no `-codec:v` and lets the output container's default encoder
  decide. `NULL` is how you opt out of the H.264 default for a container
  that does not hold it. For a `.webm` output, pass `video_codec = NULL`
  *and* `audio_codec = NULL`, because the default `audio_codec = "copy"`
  would otherwise carry a codec WebM cannot hold.

- audio_codec:

  A string naming the output audio codec. The default `"copy"`
  stream-copies the source audio unchanged. Name a real encoder, such as
  `"aac"`, when the source audio codec cannot be copied into the output
  container. `NULL` emits no `-codec:a` and lets the container's default
  encoder decide.

- pixel_format:

  A string naming the output pixel format (default `"yuv420p"`).

- hardware:

  The encoder backend. `"none"` (default) uses the software
  `video_codec`. `"nvenc"` uses NVIDIA GPU encoding (H.264, HEVC and
  AV1), and `"videotoolbox"` uses Apple GPU encoding (H.264 and HEVC). A
  backend uses its own encoder for the family of `video_codec`. For
  example, `"libx264"` becomes `"h264_nvenc"` or `"h264_videotoolbox"`.
  See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  for availability and its caveats. This applies to video only.
  `audio_codec` is never hardware-accelerated. Resolving a hardware
  backend asks this FFmpeg build which encoders it has. So the first
  such call that re-encodes the video runs FFmpeg while the command is
  built, even under `run = FALSE`. The answer is remembered for the rest
  of the R session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it.

- fallback:

  A logical. When a `hardware` other than `"none"` is requested but its
  encoder is unavailable, `TRUE` re-encodes with the software
  `video_codec` and a message. `FALSE` (default) aborts instead. This
  keeps output reproducible by never changing the codec silently. A
  `video_codec` in a family that the backend has no encoder for is a
  wrong argument, not an absent encoder. So it aborts whatever
  `fallback` says.

- audio_stream:

  The audio track to carry into the output, as a number that counts from
  `0` among the *audio tracks* of the input. `0` is the first audio
  track and `1` is the second. Other streams in the file, such as video,
  do not count. `NULL` (default) carries **every** audio track. The
  every-track family reads `NULL` this way:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  `anonymize_video`,
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

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

`regions` is a data frame with one row per box and the columns `x`, `y`,
`width`, and `height` (each a pixel number or an FFmpeg expression such
as `"in_w/2"`); `x`/`y` give the top-left corner and `width`/`height`
the size. An optional `color` column overrides the `color` argument for
that row. Every box is a solid fill (FFmpeg's `drawbox` with `t=fill`);
hollow outlines are intentionally not offered.

Because a filter is applied, the video is re-encoded (`video_codec` /
`pixel_format`, defaulting to H.264 / `yuv420p`); odd source dimensions
are floored to even so the output always encodes (a `yuv420p`/`libx264`
requirement, and a no-op for already-even input). Audio is stream-copied
unchanged (`-c:a copy`) unless `audio_codec` names an encoder. The same
input and regions therefore always compile to a byte-identical command.

## References

https://ffmpeg.org/ffmpeg-filters.html#drawbox

## See also

[`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
the builder filter it wraps;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` toggle;
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
for the many-file (batch) form.

Other task functions:
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
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

Other audio selection functions:
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
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Cover two fixed regions with black boxes
regions <- data.frame(
  x = c(10, 200), y = c(10, 150),
  width = c(120, 80), height = c(90, 60)
)
anonymize_video(video, "anon.mp4", regions, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2,drawbox=x=10:y=10:w=120:h=90:c=black:t=fill,drawbox=x=200:y=150:w=80:h=60:c=black:t=fill\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -map \"0:v?\" -map \"0:a?\" \"anon.mp4\""
# Carry only the second audio track instead of all of them
anonymize_video(video, "anon.mp4", regions, audio_stream = 1, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2,drawbox=x=10:y=10:w=120:h=90:c=black:t=fill,drawbox=x=200:y=150:w=80:h=60:c=black:t=fill\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -map \"0:v?\" -map \"0:a:1\" \"anon.mp4\""
```
