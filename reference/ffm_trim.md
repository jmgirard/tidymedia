# Trim the Duration of the FFmpeg Pipeline

Trim the input so that the output keeps one continuous part of the
input. If `start` is `NULL`, the kept section starts at the beginning of
the input. If both `end` and `duration` are `NULL`, the kept section
ends at the end of the input. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as stream copy.

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

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- start:

  The time of the start of the kept section, given in `units`. The frame
  at this time is the first frame of the output.

- end:

  The time of the first frame that is dropped, given in `units`. The
  frame just before it is the last frame of the output.

- duration:

  The maximum duration of the output, given in time duration syntax.

- units:

  A string that says how `start` and `end` are given: time duration
  syntax (`"tds"`), timebase units (`"pts"`) or frame numbers
  (`"frame"`). The default is `"tds"`.

- setpts:

  A logical that says whether the output timestamps change to start at
  zero. If `TRUE`, a `setpts` filter is added after the trim.

## Value

`object` with added instructions to trim the duration.

## References

https://ffmpeg.org/ffmpeg-filters.html#trim

https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax

## See also

[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
the faster cut by seeking, which can use stream copy. `ffm_trim()` is
the filter that cuts on exact frames.

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
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
ffm_files(video, "output.mp4") |>
  ffm_trim(start = 1, end = 5) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"trim=start=1:end=5,setpts=PTS-STARTPTS\" \"output.mp4\""
```
