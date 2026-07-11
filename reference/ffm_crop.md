# Crop Frames in an FFmpeg Pipeline

Decrease the size of the video's frames by cropping it.

## Usage

``` r
ffm_crop(object, width, height, x = "(in_w-out_w)/2", y = "(in_h-out_h)/2")
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- width:

  The width of the output video (in pixels). Either a positive real
  number or a string that contains an FFMPEG expression.

- height:

  The height of the output video (in pixels). Either a positive real
  number or a string that contains an FFMPEG expression.

- x:

  The horizontal position, in the input video, of the left edge of the
  output video (in pixels). Either a positive real number or a string
  that contains an FFMPEG expression. (default = `"(in_w-out_w)/2"`)

- y:

  The vertical position, in the input video, of the top edge of the
  output video (in pixels). Either a positive real number or a string
  that contains an FFMPEG expression. (default = `"(in_h-out_h)/2"`)

## Value

`object` but with the added instruction to crop the image(s).

## References

https://ffmpeg.org/ffmpeg-filters.html#toc-crop

## See also

Other builder functions:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md),
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
[`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
[`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md),
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md),
[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
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
# Crop to a centered 160x120 region
ffm(video, "output.mp4") |>
  ffm_crop(width = 160, height = 120) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" \"output.mp4\""
```
