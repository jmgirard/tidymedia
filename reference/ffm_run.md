# Run the FFmpeg Pipeline

Compile the instructions in the pipeline and run them all through
FFmpeg.

## Usage

``` r
ffm_run(object, verify = NULL)
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- verify:

  An optional named list of expected output properties, passed to
  [`verify_media`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  (e.g. `list(width = 1920, video_codec = "h264")`). After a successful
  run the output is probed and, if any check fails, `ffm_run()` aborts
  with the failed checks (mirroring how it aborts on a non-zero FFmpeg
  exit). `NULL` (default) skips verification.

## Value

A character vector of FFmpeg's standard output (with a `status`
attribute on a non-zero exit), invisibly; called for its side effect of
writing the output file. The pipeline is executed as an argument vector
(never through a shell), so paths containing spaces or special
characters are safe.

## See also

[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
to get the command without running it,
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the many-file runner, and
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
for the `verify =` spec.

Other builder functions:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md),
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
[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md),
[`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md),
[`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
[`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md),
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
out <- tempfile(fileext = ".mp4")
ffm(video, out) |>
  ffm_scale(width = 160, height = 120) |>
  ffm_codec(video = "libx264") |>
  ffm_run(verify = list(width = 160, height = 120))
```
