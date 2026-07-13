# Overlay One Video on Another in an FFmpeg Pipeline

Composite the second input (the overlay) on top of the first (the main
video) at position `x`/`y`. This is a blessed multi-input verb (like
[`ffm_hstack`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)):
it forces the `-filter_complex` path and manages its own stream labels
internally. Exactly two inputs are required — the first is the
background, the second is drawn over it.

## Usage

``` r
ffm_overlay(object, x = 0, y = 0, shortest = FALSE, scale = NULL)
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
  with exactly two input files.

- x:

  The horizontal position of the overlay's left edge, as a number of
  pixels or an FFmpeg expression. (default = `0`)

- y:

  The vertical position of the overlay's top edge, as a number of pixels
  or an FFmpeg expression. (default = `0`)

- shortest:

  A logical indicating whether to end the output when the shorter input
  ends (default = `FALSE`).

- scale:

  An optional fraction (`0 < scale <= 1`) to resize the overlay to
  `scale` times the main video's width before compositing (aspect
  preserved); `NULL` (default) overlays at native size. When set,
  `overlay_w`/`overlay_h` in `x`/`y` refer to the resized overlay.

## Value

`object` with the added instruction to overlay the second input on the
first.

## Details

`x` and `y` accept plain numbers (pixels from the top-left of the main
video) or FFmpeg overlay expressions, where `main_w`/`main_h` are the
main video's dimensions and `overlay_w`/`overlay_h` are the overlay's.
For example, `x = "main_w-overlay_w-16"` pins the overlay 16 pixels from
the right edge. When `scale` is set, the overlay is first resized to a
fraction of the main video's width (aspect preserved), which is what the
Layer-2
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
verb uses. Otherwise, to resize the overlay yourself, filter it in a
separate pipeline first.

## See also

[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
the task verb built on this verb.

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
ffm(c(video, video), "output.mp4") |>
  ffm_overlay(x = "main_w-overlay_w-16", y = 16) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]\" -map \"[vout]\" \"output.mp4\""
```
