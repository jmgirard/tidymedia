# Trim the Duration of the FFmpeg Pipeline

Trim the input so that the output contains one continuous subpart of the
input. Note that, if `start=NULL`, then the kept section will start at
the beginning of the input. If both `end=NULL` and `duration=NULL`, the
kept section will end at the end of the input.

## Usage

``` r
ffm_trim(
  object,
  start = NULL,
  end = NULL,
  duration = NULL,
  units = c("tds", "pts", "frame"),
  setpts = TRUE
)
```

## Arguments

- object:

  An ffmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- start:

  The time of the start of the kept section (i.e., this will be the
  first frame in the output) given in `units`.

- end:

  The time of the first frame that will be dropped (i.e., the frame
  immediately preceding this will be the last frame in the output),
  given in `units`.

- duration:

  The maximum duration of the output given in time duration syntax.

- units:

  A string indicating whether the `start` and/or `end` are given time
  duration syntax ("tds"), timebase units ("pts"), or frame number
  ("frame"). default = `"tds"`

- setpts:

  A logical indicating whether the output timestamps should be modified
  to start at zero. If TRUE, will add a setpts filter after trim.

## Value

`object` but will added instructions to trim the duration.

## References

https://ffmpeg.org/ffmpeg-filters.html#trim

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
the faster seek-based cut that can stream-copy (this is the frame-exact
*filter*).

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
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
ffm(video, "output.mp4") |>
  ffm_trim(start = 1, end = 5) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"trim=start=1:end=5,setpts=PTS-STARTPTS\" \"output.mp4\""
```
