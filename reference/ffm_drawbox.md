# Draw a Colored Box on the Videos in an FFmpeg Pipeline

Add a video filter to draw a colored rectangle on the input video.

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

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- x:

  The horizontal position, in the input video, of the left edge of the
  box (in pixels). Either a nonnegative real number or a string that
  contains an FFMPEG expression. (default = 0)

- y:

  The vertical position, in the input video, of the top edge of the box
  (in pixels). Either a nonnegative real number or a string that
  contains an FFMPEG expression. (default = 0)

- width:

  The width of the box (in pixels). Either a positive real number or a
  string that contains an FFmpeg expression. (default = `"in_w"`)

- height:

  The height of the box (in pixels). Either a positive real number or a
  string that contains an FFmpeg expression. (default = `"in_h"`)

- color:

  A string containing the color of the box in FFmpeg color syntax, see
  reference link below for more details. If the special value `"invert"`
  is used, the box color is the same as the video with inverted luma.
  (default = `"black"`)

- thickness:

  A thickness of the box edge (in pixels). A value of `"fill"` will
  create a filled box. (default = `"fill"`)

## Value

`object` but with the added instruction to apply the drawbox filter.

## References

https://ffmpeg.org/ffmpeg-filters.html#drawbox

https://ffmpeg.org/ffmpeg-utils.html#color-syntax

## See also

Other builder functions:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md),
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
ffm(video, "output.mp4") |>
  ffm_drawbox(width = "in_w/2", height = "in_h/2", color = "red") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"drawbox=x=0:y=0:w=in_w/2:h=in_h/2:c=red:t=fill\" \"output.mp4\""
```
