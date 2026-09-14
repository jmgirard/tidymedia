# Set the Pixel Format in an FFmpeg Pipeline

Set the pixel format of the output with FFmpeg's `-pix_fmt` option. For
example, use `"yuv420p"` for broad player compatibility. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as pixel format.

## Usage

``` r
ffm_pixel_format(object, format)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- format:

  A string that names the pixel format of the output file.

## Value

`object` with an added instruction to set the pixel format.

## See also

[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
and
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
the task functions that use `ffm_pixel_format()` to set the pixel
format.

Other pipeline functions:
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
[`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md),
[`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
[`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md),
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md),
[`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md),
[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md),
[`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md),
[`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
ffm_files(video, "output.mp4") |>
  ffm_pixel_format("yuv420p") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -pix_fmt yuv420p \"output.mp4\""
```
