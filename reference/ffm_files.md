# Specify Files in an FFmpeg Pipeline

Start an FFmpeg pipeline by specifying input and output files.

## Usage

``` r
ffm_files(input, output, overwrite = TRUE)
```

## Arguments

- input:

  A character vector of paths to the input media files of the pipeline.
  Give more than one path for stacking.

- output:

  A string with the path of the output media file of the pipeline.

- overwrite:

  A logical. If `TRUE` (the default), an output media file that already
  exists is overwritten.

## Value

An FFmpeg pipeline object.

## See also

[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
to build the command and
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
to run it. The task functions, such as
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
and
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
are built on the pipeline functions.

Other pipeline functions:
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
[`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md),
[`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
[`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md),
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
ffm_files(video, "output.mp4") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" \"output.mp4\""
```
