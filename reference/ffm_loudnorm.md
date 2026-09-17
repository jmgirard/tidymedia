# Normalize Loudness in an FFmpeg Pipeline

Add FFmpeg's `loudnorm` (EBU R128) audio filter. It normalizes the
input's perceived loudness toward a target integrated loudness,
true-peak ceiling and loudness range. The filter compiles to `-af`, or
joins an existing audio filter chain in the order the filters were
added. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as LUFS and true peak.

## Usage

``` r
ffm_loudnorm(
  object,
  target_loudness = -23,
  true_peak = -1,
  loudness_range = 7,
  measured_i = NULL,
  measured_tp = NULL,
  measured_lra = NULL,
  measured_thresh = NULL,
  offset = NULL,
  linear = FALSE,
  print_format = NULL
)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- target_loudness:

  The target integrated loudness, in LUFS (a number in `-70`..`-5`). The
  default, `-23`, is the EBU R128 target.

- true_peak:

  The maximum true peak, in dBTP (a number in `-9`..`0`). The default,
  `-1`, is the EBU R128 ceiling.

- loudness_range:

  The target loudness range, in LU (a number in `1`..`50`). The default
  is `7`.

- measured_i, measured_tp, measured_lra, measured_thresh:

  Measured input values from an earlier `loudnorm` analysis pass:
  integrated loudness, true peak, loudness range and threshold. Give
  them together for an accurate two-pass (linear) correction. Give these
  values and `offset` as one set, or give none of them. `NULL`, the
  default, gives single-pass dynamic normalization. These map to
  FFmpeg's `measured_I`, `measured_TP`, `measured_LRA` and
  `measured_thresh` options.

- offset:

  The `target_offset` (offset gain) that the analysis pass reports. It
  is part of the measured set (see `measured_i`). The default is `NULL`.

- linear:

  A logical. `TRUE` requests linear normalization (`linear=true`), which
  needs the measured values to hit the target precisely. `FALSE` (the
  default) leaves out the option, so the single-pass dynamic behavior
  does not change.

- print_format:

  The format of the measurement report for an analysis pass: `"json"`,
  `"summary"` or `"none"`. `NULL` (the default) leaves out the option.
  Use `"json"` for an analysis pass that a program can parse.

## Value

`object` with an added instruction to normalize loudness.

## Details

This is single-pass (dynamic) `loudnorm`. The pipeline stays one
reproducible command, with no measurement pass. The defaults follow EBU
Recommendation R 128 (2014): `target_loudness = -23` LUFS and
`true_peak = -1` dBTP. Loudness is measured per ITU-R BS.1770-4. The
default `loudness_range = 7` is FFmpeg's own `loudnorm` default. EBU
R128 does not prescribe a single value.

Two filters are added, not one. `loudnorm` is followed by
`asetnsamples`, which regroups the filtered audio into frames of 4096
samples and does not pad the last one. Dynamic `loudnorm` resamples to
192 kHz and gives frames of 192000 samples. Some encoders accept
whatever frame they are given, FLAC and Vorbis among them. Even those
encoders refuse to open at all on frames of 192000 samples.

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.
<https://ffmpeg.org/ffmpeg-filters.html#loudnorm>

## See also

[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
the task function built on this filter.

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
  ffm_loudnorm() |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7,asetnsamples=n=4096:p=0\" \"output.mp4\""
```
