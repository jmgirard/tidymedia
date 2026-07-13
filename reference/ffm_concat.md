# Concatenate Multiple Inputs in an FFmpeg Pipeline

Join the pipeline's input files one after another using FFmpeg's [concat
demuxer](https://ffmpeg.org/ffmpeg-formats.html#concat-1). This is a
blessed multi-input verb (like
[`ffm_hstack`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)):
it stream-copies, so it is fast and lossless but requires that every
input share the same parameters (codec, resolution, frame rate, ...). To
concatenate inputs with differing parameters you must re-encode via the
concat filter (not yet wrapped; use the Layer 0 escape hatch).

## Usage

``` r
ffm_concat(object)
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
  with more than one input file.

## Value

`object` with the added instruction to concatenate the inputs.

## Details

The demuxer needs a list file naming the inputs; `ffm_concat()` writes
one to a temporary path immediately and stores it in the pipeline, so
the compiled command can reference it. It also copies codecs and maps
all streams (as
[`ffm_copy`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
would).

## See also

[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
the task verb built on this verb.

Other builder functions:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md),
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
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
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Join two inputs end-to-end (they must share codec/resolution/frame rate)
ffm(c(video, video), "output.mp4") |>
  ffm_concat() |>
  ffm_compile()
#> [1] "-y -f concat -safe 0 -i \"/tmp/RtmpamQvz3/ffm-concat1fc02c55c7e4.txt\" -codec:v copy -codec:a copy -map 0 \"output.mp4\""
```
