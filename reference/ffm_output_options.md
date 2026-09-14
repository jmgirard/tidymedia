# Add Raw Output Options to an FFmpeg Pipeline

Add one or more raw FFmpeg output options to the pipeline, after any
added before. Output options are the flags after the input and before
the output file. Use this function for an option that has no pipeline
function of its own.
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
still decides where the options go and how the rest of the command is
quoted. So this is not the same as writing the command string yourself.

## Usage

``` r
ffm_output_options(object, ...)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- ...:

  One or more strings. Each string is a group of options separated by
  white space, for example `"-q:v 1"` or `"-frames:v 1"`. They are added
  in the order given. When the command runs, each word between white
  space becomes one FFmpeg argument. So an option value must not contain
  spaces.

## Value

`object` with the added output options.

## See also

[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
the direct command that takes any FFmpeg arguments, and
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
which places these options.

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
# Extract a single frame by adding a raw output option
ffm_files(video, "frame.png") |>
  ffm_output_options("-frames:v 1") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -frames:v 1 \"frame.png\""
```
