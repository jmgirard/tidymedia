# Draw a Colored Box on the Videos in an FFmpeg Pipeline

Add a video filter that draws a colored rectangle on the input video.

## Usage

``` r
ffm_drawbox(
  object,
  x = 0,
  y = 0,
  width = "in_w",
  height = "in_h",
  color = "black",
  thickness = "fill"
)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- x:

  The horizontal position of the left edge of the box, in pixels of the
  input video. Give a nonnegative real number or a string that contains
  an FFmpeg expression. The default is `0`.

- y:

  The vertical position of the top edge of the box, in pixels of the
  input video. Give a nonnegative real number or a string that contains
  an FFmpeg expression. The default is `0`.

- width:

  The width of the box, in pixels. Give a positive real number or a
  string that contains an FFmpeg expression. The default is `"in_w"`.

- height:

  The height of the box, in pixels. Give a positive real number or a
  string that contains an FFmpeg expression. The default is `"in_h"`.

- color:

  A string with the color of the box, in FFmpeg color syntax. The
  reference link below explains that syntax. With the special value
  `"invert"`, the box has the color of the video with inverted luma. The
  default is `"black"`.

- thickness:

  The thickness of the box edge, in pixels. The value `"fill"` draws a
  filled box. The default is `"fill"`.

## Value

`object` with an added instruction to apply the `drawbox` filter.

## References

https://ffmpeg.org/ffmpeg-filters.html#drawbox

https://ffmpeg.org/ffmpeg-utils.html#color-syntax

## See also

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
the task function that uses `ffm_drawbox()` to fill regions.

Other pipeline functions:
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
[`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md),
[`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md),
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md),
[`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md),
[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md),
[`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md),
[`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
[`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md),
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Draw a filled red box covering the top-left quarter of the frame
ffm_files(video, "output.mp4") |>
  ffm_drawbox(width = "in_w/2", height = "in_h/2", color = "red") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"drawbox=x=0:y=0:w=in_w/2:h=in_h/2:c=red:t=fill\" \"output.mp4\""
```
