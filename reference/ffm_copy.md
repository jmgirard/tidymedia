# Copy the codecs and map all streams

Copy the audio, the video, or both, with stream copy and no re-encoding.
It can also map all streams from the input. This is the fast, lossless
path when you only need to put the streams in a new container or cut on
keyframes. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, container, keyframe and stream copy.

## Usage

``` r
ffm_copy(object, audio = TRUE, video = TRUE, streams = TRUE)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- audio:

  A logical. `TRUE` (the default) copies the audio codec. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for the two things that the name `audio` means in the pipeline
  functions, and for the input index `audio_input`.

- video:

  A logical. `TRUE` (the default) copies the video codec.

- streams:

  A logical. `TRUE` (the default) maps all streams from the input. It
  **sets** the mapping to the all-streams specifier `"0"`, and does not
  add to it. So two `ffm_copy()` calls compile one `-map "0"`, not two.
  If the pipeline already has a *different* mapping, `ffm_copy()` gives
  an error and does not discard that mapping silently. To keep the
  mapping you set, pass `streams = FALSE`. Or call `ffm_copy()` first,
  and then narrow the mapping with `ffm_map(replace = TRUE)`.

## Value

`object` with an added instruction to copy codecs, map all streams, or
both.

## See also

[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
and
[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md),
which `ffm_copy()` calls, and
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
which uses it for fast copy cuts.

Other pipeline functions:
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md),
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
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
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
ffm_files(video, "output.mp4") |>
  ffm_copy() |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -map \"0\" \"output.mp4\""
```
