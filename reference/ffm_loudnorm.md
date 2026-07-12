# Normalize Loudness in an FFmpeg Pipeline

Append FFmpeg's `loudnorm` (EBU R128) audio filter, normalizing the
input's perceived loudness toward a target integrated loudness,
true-peak ceiling, and loudness range. This is the first builder
function to write the pipeline's audio filter chain, so it compiles to
`-af` (or joins an existing audio filter chain in application order).

## Usage

``` r
ffm_loudnorm(object, target_loudness = -23, true_peak = -1, loudness_range = 7)
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- target_loudness:

  The target integrated loudness, in LUFS (a number in `-70`..`-5`;
  default `-23`, the EBU R128 target).

- true_peak:

  The maximum true peak, in dBTP (a number in `-9`..`0`; default `-1`,
  the EBU R128 ceiling).

- loudness_range:

  The target loudness range, in LU (a number in `1`..`50`; default `7`).

## Value

`object` but with the added instruction to normalize loudness.

## Details

This is single-pass (dynamic) `loudnorm`: one reproducible command, no
measurement pass. The defaults follow EBU Recommendation R 128 (2014) —
`target_loudness = -23` LUFS and `true_peak = -1` dBTP, loudness
measured per ITU-R BS.1770-4 — with `loudness_range = 7` (FFmpeg's own
`loudnorm` default, EBU R128 not prescribing a single value).

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.
<https://ffmpeg.org/ffmpeg-filters.html#loudnorm>

## See also

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
ffm(video, "output.mp4") |>
  ffm_loudnorm() |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7\" \"output.mp4\""
```
