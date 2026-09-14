# Run the FFmpeg Pipeline

Compile the instructions in the pipeline and run them all through
FFmpeg.

## Usage

``` r
ffm_run(object, verify = NULL)
```

## Arguments

- object:

  An FFmpeg pipeline (`ffm`) object created by
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md).

- verify:

  An optional named list of the properties you expect the output to
  have, for example `list(width = 1920, video_codec = "h264")`. It is
  passed to
  [`verify_media`](https://jmgirard.github.io/tidymedia/reference/verify_media.md).
  After a successful run, the output is probed. If a check fails,
  `ffm_run()` gives an error with the failed checks. It also gives an
  error when FFmpeg exits non-zero. `NULL` (the default) skips the
  checks.

## Value

FFmpeg's standard output as a character vector, returned invisibly. On a
non-zero exit it has a `status` attribute. You call `ffm_run()` to write
the output file, not for its return value. The pipeline runs as a vector
of arguments and never through a shell. So paths with spaces or special
characters are safe.

## When FFmpeg exits non-zero

If FFmpeg refuses a run, `ffm_run()` gives an error of class
`tidymedia_ffmpeg_exit`. A caller can catch a failed run without reading
the error text:


    tryCatch(
      ffm_run(pipeline),
      tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
    )

The `tm_status` field is one integer, the exit status exactly as
[`system2()`](https://rdrr.io/r/base/system2.html) reported it. If a
signal stopped FFmpeg, the field holds the shell's number, 128 plus the
signal number, unchanged. That number stands for the signal, not for a
status FFmpeg chose to return.

Two other paths give this class and carry this field, so one handler
covers all three:

- the `loudnorm` analysis pass of `normalize_audio(two_pass = TRUE)`,
  when FFmpeg exits non-zero.

- the error about several audio tracks that
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  adds to a failed audio output.

Each of those two paths also gives a second, narrower class before this
one. In the same order, they are `tidymedia_loudnorm_no_measurement` and
`tidymedia_multitrack_separation`. Catch that class when you want only
that failure.

Two related paths do **not** give this class, each for its own reason:

- `normalize_audio(two_pass = TRUE)` also gives an error when the
  analysis pass exits zero and prints no measurement block that can be
  read. FFmpeg did not exit non-zero there. So that error has only the
  class `tidymedia_loudnorm_no_measurement`, and no `tm_status`.

- `normalize_audio_batch(two_pass = TRUE)` reports in one error every
  row that failed in its analysis phase. The failed rows can include
  rows that exited zero and rows that FFmpeg refused. So a non-zero exit
  is one of its causes, not the fact it reports, and no single status
  can stand for the mix. It also has only the class
  `tidymedia_loudnorm_no_measurement`. It carries `tm_rows`, the failed
  rows counted from 1. It also carries `tm_row_status`, their exit
  statuses in the same order, with `NA` where a row exited zero.

So `tidymedia_loudnorm_no_measurement` is the one class that covers the
analysis pass in both forms.

## See also

[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
to get the command without running it,
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
to run many files, and
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
for the `verify` list.

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
[`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
out <- tempfile(fileext = ".mp4")
ffm_files(video, out) |>
  ffm_scale(width = 160, height = 120) |>
  ffm_codec(video = "libx264") |>
  ffm_run(verify = list(width = 160, height = 120))
```
