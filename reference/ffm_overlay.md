# Overlay One Video on Another in an FFmpeg Pipeline

Draw the second input (the overlay) on top of the first input (the main
video) at position `x` and `y`. Like
[`ffm_hstack`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
this is a pipeline function for several inputs. It forces the
`-filter_complex` path and manages its own stream labels internally. It
needs exactly two inputs. The first is the background, and the second is
drawn over it. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as stream.

## Usage

``` r
ffm_overlay(object, x = 0, y = 0, shortest = FALSE, scale = NULL)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
  with exactly two input files.

- x:

  The horizontal position of the overlay's left edge, as a number of
  pixels or an FFmpeg expression. The default is `0`.

- y:

  The vertical position of the overlay's top edge, as a number of pixels
  or an FFmpeg expression. The default is `0`.

- shortest:

  A logical that says whether to end the output when the shorter input
  ends. The default is `FALSE`.

- scale:

  An optional fraction (`0 < scale <= 1`). Before the overlay is drawn,
  it is resized to `scale` times the main video's width, and its aspect
  ratio is kept. `NULL` (the default) draws the overlay at its own size.
  When `scale` is set, `overlay_w` and `overlay_h` in `x` and `y` refer
  to the resized overlay.

## Value

`object` with an added instruction to draw the second input on the
first.

## Details

`x` and `y` accept plain numbers or FFmpeg overlay expressions. A plain
number counts pixels from the top-left of the main video. In an
expression, `main_w` and `main_h` are the main video's dimensions.
`overlay_w` and `overlay_h` are the overlay's dimensions. For example,
`x = "main_w-overlay_w-16"` puts the overlay 16 pixels from the right
edge.

When `scale` is set, the overlay is first resized to a fraction of the
main video's width, and its aspect ratio is kept. The task function
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
uses this resize. Otherwise, to resize the overlay yourself, filter it
in a separate pipeline first.

## See also

[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
the task function built on this function.

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
# Draw the second input over the first, 16px in from the top-right corner
ffm_files(c(video, video), "output.mp4") |>
  ffm_overlay(x = "main_w-overlay_w-16", y = 16) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]\" -map \"[vout]\" \"output.mp4\""
```
