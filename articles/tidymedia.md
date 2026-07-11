# Get started with tidymedia

``` r

library(tidymedia)
```

tidymedia helps you build **reproducible media-preprocessing pipelines**
on top of [FFmpeg](https://ffmpeg.org/) and
[MediaInfo](https://mediaarea.net/en/MediaInfo). It is deliberately
*not* “all of FFmpeg in R”: the focus is batch trimming, cropping,
format standardization, and metadata extraction for research and
data-science work.

## The three layers

tidymedia is organized into three layers, from lowest to highest level:

- **Layer 0 — the escape hatch.**
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md),
  and
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md)
  pass a raw argument string straight to the corresponding command-line
  tool. Use these when you need something tidymedia does not wrap.
- **Layer 1 — the pipeline builder.** The `ffm_*` functions assemble an
  FFmpeg command step by step. All of the quoting, option ordering, and
  copy-vs-re-encode logic lives here.
- **Layer 2 — the task verbs.** Functions like
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  are thin wrappers over the builder for common jobs. See
  [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  for processing many files at once.

This vignette focuses on Layer 1, the builder, which is the heart of the
package.

## Building a pipeline

Every pipeline starts with
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md) (an
alias of
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)),
which names the input and output files. Throughout this vignette we use
a tiny sample clip that ships with the package:

``` r

video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

You then add steps with `|>`. Each `ffm_*` verb records an instruction;
nothing runs until you ask it to. Calling
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
turns the pipeline into the exact FFmpeg command it represents — a
reproducible string you can inspect, log, or run:

``` r

ffm(video, "output.mp4") |>
  ffm_trim(start = 1, end = 5) |>
  ffm_crop(width = 160, height = 120) |>
  ffm_codec(video = "libx264") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"trim=start=1:end=5,setpts=PTS-STARTPTS,crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 \"output.mp4\""
```

Printing a pipeline shows the same compiled command, so you can eyeball
a pipeline at any point:

``` r

ffm(video, "output.mp4") |>
  ffm_scale(width = 320, height = 240) |>
  ffm_pixel_format("yuv420p")
#> tidymedia ffmpeg pipeline:
#> 
#>  -y -i "/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4" -vf "scale=w=320:h=240" -pix_fmt yuv420p "output.mp4"
```

When you are ready, swap
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
for
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
to execute the command through FFmpeg and write the output file.

## Copy versus re-encode

Cutting a clip can be **frame-accurate** (re-encoded, slower) or **fast
and lossless** (a stream copy that snaps to keyframes).
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
exposes both via its `reencode` argument, and pairs naturally with
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
for the fast path:

``` r

# Fast, lossless cut
ffm(video, "output.mp4") |>
  ffm_seek(start = 1, end = 5, reencode = FALSE) |>
  ffm_copy() |>
  ffm_compile()
#> [1] "-y -ss 1 -to 5 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -avoid_negative_ts make_zero -map 0 \"output.mp4\""
```

Note that
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
(which uses FFmpeg’s `-ss`/`-to` options) is distinct from
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)
(the `trim` *filter*): only seeking can stream-copy.

## Combining multiple inputs

A few “blessed” verbs take more than one input. Pass a vector of files
to [`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md),
then stack them side by side with
[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
or join them end-to-end with
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md):

``` r

ffm(c(video, video), "side_by_side.mp4") |>
  ffm_hstack() |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]hstack=inputs=2:shortest=0[vout]\" -map \"[vout]\" \"side_by_side.mp4\""
```

The builder stays deliberately linear — one input chain, sequential
filters, one output — so full FFmpeg filtergraphs are out of scope. When
you need one, reach for the Layer 0 escape hatch:

``` r

# Layer 0: raw arguments passed straight to FFmpeg
ffmpeg("-version")[1]
#> [1] "ffmpeg version 6.1.1-3ubuntu5 Copyright (c) 2000-2023 the FFmpeg developers"
```

## Where to next

- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  — reading container and stream metadata as tibbles.
- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  — running a pipeline over many files at once.
