# Vertically Stack Multiple Videos in an FFmpeg Pipeline

Add a complex video filter that stacks several videos vertically (one
above the other). It can also resize the videos to the same width.

## Usage

``` r
ffm_vstack(object, shortest = FALSE, resize = FALSE)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- shortest:

  A logical that says whether to trim the duration of all videos to that
  of the shortest video. The default is `FALSE`.

- resize:

  A logical that says whether to resize the input videos to the same
  width. Resizing takes longer, and for now it works only with two
  inputs. It fits both inputs to the same aspect ratio, so it assumes
  the inputs share one.

## Value

`object` with an added instruction to stack the videos vertically.

## Details

This is the vertical form of
[`ffm_hstack`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md).
Both are pipeline functions for several inputs. They force the
`-filter_complex` path and manage their own stream labels internally.
The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as stream.

## See also

[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
for horizontal stacking, and
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
the task function built on both.

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
[`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md),
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Stack two inputs one above the other (pass more than one input to ffm_files())
ffm_files(c(video, video), "output.mp4") |>
  ffm_vstack() |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]vstack=inputs=2:shortest=0[vout]\" -map \"[vout]\" \"output.mp4\""
```
