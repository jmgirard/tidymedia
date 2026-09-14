# Set the Stream Mapping in an FFmpeg Pipeline

Choose which input streams go into the output, with FFmpeg's `-map`
option. The default, `"0"`, maps every stream from the first input. The
glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as stream.

## Usage

``` r
ffm_map(object, mapping = "0", replace = FALSE)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- mapping:

  A character vector of one or more stream specifiers. Each one adds one
  `-map`.

- replace:

  A logical. `TRUE` discards any mapping already set on `object`.
  `FALSE` (the default) adds to it.

## Value

`object` with an added instruction to map streams.

## Details

`mapping` can be a character vector. Each element adds one `-map`, in
the order given. For example, `ffm_map(object, c("0:v", "0:a:1"))` keeps
the video and the *second* audio track of the input.

A second `ffm_map()` call **adds** to the maps already set. It does not
replace them. Pass `replace = TRUE` to discard them instead. That is how
you narrow the all-streams map that
[`ffm_copy`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
sets. Adding to that map puts the stream in the output twice, and does
not select it.

`ffm_map()` is the only pipeline function that adds to earlier calls.
Every other `ffm_*` function that sets a value,
[`ffm_copy`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
included, replaces it. `ffm_map()` is different because its arguments
are *partial* choices that combine. For example, you keep the video,
then name one audio track.

When the pipeline uses a function with several inputs, such as
[`ffm_hstack`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
your mapping is added *beside* the automatic `-map "[vout]"` of the
filtered stream. For example, `ffm_map(object, "0:a")` keeps the audio
of the first input next to the stacked video.

## See also

[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
which maps all streams, and
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
a task function built on `ffm_map()`.

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
  ffm_map(mapping = "0") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -map \"0\" \"output.mp4\""

# Keep the video and the second audio track only
ffm_files(video, "output.mkv") |>
  ffm_map(mapping = c("0:v", "0:a:1")) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -map \"0:v\" -map \"0:a:1\" \"output.mkv\""
```
