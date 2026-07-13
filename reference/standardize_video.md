# Standardize a video to a reproducible format

Re-encode a video to a consistent, reproducible format for analysis
pipelines: a single video codec, pixel format, and (optionally)
resolution and frame rate, with `+faststart` for smooth playback. Unlike
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
(a fixed web-delivery recipe), every part of the standard is a
parameter, so a lab can pin its own house format once and apply it
across a dataset.

## Usage

``` r
standardize_video(
  infile,
  outfile,
  width = NULL,
  height = NULL,
  fps = NULL,
  video_codec = "libx264",
  pixel_format = "yuv420p",
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a video file.

- outfile:

  A string containing the path of the video file to write.

- width:

  The output width in pixels (a positive number), or `NULL` (default) to
  leave the width unconstrained.

- height:

  The output height in pixels (a positive number), or `NULL` (default)
  to leave the height unconstrained.

- fps:

  The output frame rate (a positive number or FFmpeg framerate
  expression such as `"30000/1001"`), or `NULL` (default) to keep the
  input frame rate.

- video_codec:

  A string naming the output video codec (default `"libx264"`).

- pixel_format:

  A string naming the output pixel format (default `"yuv420p"`).

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

The default standard `standardize_video(infile, outfile)` re-encodes to
H.264 video (`video_codec = "libx264"`) with `pixel_format = "yuv420p"`
and `-movflags +faststart`, keeping the source resolution and frame
rate. Audio is stream-copied unchanged (`-c:a copy`); audio
standardization is out of scope. The same input therefore always
compiles to a byte-identical command.

Resolution follows `width`/`height`: supplying both forces exact output
dimensions; supplying only one preserves the aspect ratio and rounds the
other to the nearest even number (FFmpeg's `-2`); supplying neither
keeps the source resolution but rounds odd dimensions down to the
nearest even value (a `yuv420p`/`libx264` requirement, and a no-op for
already-even input) so the output always encodes.

## See also

[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
and
[`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md),
among the builders it wraps;
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
for the many-file form.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
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
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# The documented default standard (H.264 / yuv420p / +faststart)
standardize_video(video, "std.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart \"std.mp4\""
# Pin resolution and frame rate too
standardize_video(video, "std.mp4", width = 1280, height = 720, fps = 30,
                  run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"scale=w=1280:h=720,fps=30\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart \"std.mp4\""
```
