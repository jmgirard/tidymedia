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
  pixel_format = "yuv420p",
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

  A string naming the output video codec (default `"libx264"`).

- pixel_format:

  A string naming the output pixel format (default `"yuv420p"`).

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
unchanged (`-c:a copy`). The same input and regions therefore always
compile to a byte-identical command.

## References

https://ffmpeg.org/ffmpeg-filters.html#drawbox

## See also

[`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
the builder filter it wraps;
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
for the many-file (batch) form.

Other task verb functions:
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
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
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2,drawbox=x=10:y=10:w=120:h=90:c=black:t=fill,drawbox=x=200:y=150:w=80:h=60:c=black:t=fill\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p \"anon.mp4\""
```
