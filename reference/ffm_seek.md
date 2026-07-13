# Cut a Continuous Section from an FFmpeg Pipeline by Seeking

Keep one continuous section of the input using FFmpeg's fast `-ss`/
`-to` seek options, rather than the `trim` *filter* (see
[`ffm_trim`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)).
Unlike the filter, seeking can stream-copy, so it is the tool for fast,
lossless cutting.

## Usage

``` r
ffm_seek(object, start = NULL, end = NULL, reencode = TRUE)
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- start:

  The start of the kept section, in seconds or FFmpeg time duration
  syntax. `NULL` keeps from the beginning.

- end:

  The end of the kept section, in seconds or FFmpeg time duration
  syntax. `NULL` keeps to the end.

- reencode:

  A logical: re-encode for a frame-accurate cut (`TRUE`, default) or
  fast copy-safe seek that snaps to keyframes (`FALSE`).

## Value

`object` with the added instruction to seek-cut the input.

## Details

The `reencode` argument trades accuracy against speed:

- `reencode = TRUE` (default) is **frame-accurate**: the section is
  re-encoded so it begins and ends on the exact requested frames. This
  is the safe default.

- `reencode = FALSE` is a **fast, lossless copy**, but the cut points
  snap to the nearest keyframes, so the output duration can differ from
  the request by up to one group-of-pictures. Pair it with
  [`ffm_copy`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  for the fastest path.

## References

https://ffmpeg.org/ffmpeg.html#Main-options

## See also

[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)
for the filter-based alternative,
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
for the fast copy path, and
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
the task verb built on it.

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
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Fast, lossless copy cut (snaps to keyframes)
ffm(video, "output.mp4") |>
  ffm_seek(start = 1, end = 5, reencode = FALSE) |>
  ffm_copy() |>
  ffm_compile()
#> [1] "-y -ss 1 -to 5 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -avoid_negative_ts make_zero -map 0 \"output.mp4\""
```
