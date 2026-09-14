# Cut a Continuous Section from an FFmpeg Pipeline by Seeking

Keep one continuous section of the input with FFmpeg's fast `-ss` and
`-to` seek options. It does not use the `trim` *filter* of
[`ffm_trim`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md).
Unlike the filter, seeking can use stream copy, so it is the tool for
fast, lossless cuts. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as keyframe, re-encode and stream copy.

## Usage

``` r
ffm_seek(object, start = NULL, end = NULL, reencode = TRUE)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- start:

  The start of the kept section, in seconds or in FFmpeg time duration
  syntax. `NULL` keeps the section from the beginning.

- end:

  The end of the kept section, in seconds or in FFmpeg time duration
  syntax. `NULL` keeps the section to the end.

- reencode:

  A logical. `TRUE` (the default) re-encodes for a frame-accurate cut.
  `FALSE` makes a fast seek that is safe to copy, and its cut points
  move to keyframes.

## Value

`object` with an added instruction to cut the input by seeking.

## Details

The `reencode` argument trades accuracy against speed:

- `reencode = TRUE` (the default) is **frame-accurate**. The section is
  re-encoded, so it starts and ends on the exact frames you ask for.
  This is the safe default.

- `reencode = FALSE` is a **fast, lossless copy**. But the cut points
  move to the nearest keyframes. So the output duration can differ from
  the request by up to the gap between two keyframes. Use it with
  [`ffm_copy`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  for the fastest path.

## References

https://ffmpeg.org/ffmpeg.html#Main-options

## See also

[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)
for the filter that cuts,
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
for the fast copy path, and
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
the task function built on it.

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
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Fast, lossless copy cut (snaps to keyframes)
ffm_files(video, "output.mp4") |>
  ffm_seek(start = 1, end = 5, reencode = FALSE) |>
  ffm_copy() |>
  ffm_compile()
#> [1] "-y -ss 1 -to 5 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -avoid_negative_ts make_zero -map \"0\" \"output.mp4\""
```
