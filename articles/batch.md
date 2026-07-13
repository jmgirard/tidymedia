# Batch processing

``` r

library(tidymedia)
```

Batch processing over many files is what tidymedia is built for. The
examples below use `run = FALSE`, which **compiles** each FFmpeg command
without executing it — so you can inspect exactly what would run. Drop
that argument (or set `run = TRUE`) to actually process the files.

``` r

video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

## The batch runner

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
is the general-purpose engine: it takes a **jobs tibble** (one row per
output) and a function that turns a row into a pipeline. Each column is
passed to the function by name,
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)-style,
and the runner returns your jobs tibble with a `command` column added
(and a `success` column when `run = TRUE`).

``` r

jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("clip1.mp3", "clip2.mp3")
)

ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
  ffm_files(input, output) |>
    ffm_drop("video") |>
    ffm_codec(audio = "libmp3lame")
})
#> # A tibble: 2 × 3
#>   input                                                        output    command
#>   <chr>                                                        <chr>     <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 clip1.mp3 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 clip2.mp3 "-y -i…
```

Because you build the pipeline yourself, any combination of builder
verbs is available per job. Give `.f` a `...` argument so it tolerates
extra job columns it does not use.

## Per-verb batch siblings

You do not have to write `.f` by hand for the common jobs. Every task
verb ships a `*_batch()` companion —
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
and the rest — that takes a jobs tibble directly and applies the verb to
each row:

``` r

jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("session01_cropped.mp4", "session02_cropped.mp4")
)

crop_video_batch(jobs, width = 160, height = 120, run = FALSE)
#> # A tibble: 2 × 3
#>   input                                                        output    command
#>   <chr>                                                        <chr>     <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 session0… "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 session0… "-y -i…
```

When the jobs tibble has no `output` column, the transform verbs
auto-name each output from its input (e.g. `_cropped`); either way they
reject two rows that would resolve to the same output path.
[`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
chains several of these across a study folder.

## Fan-out verbs

Some tasks turn one input into *many* outputs. These fan-out verbs are
Layer 2 wrappers built on
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
cuts a file into pieces given start/stop timestamps, returning one row
per segment:

``` r

segment_video(
  video,
  start = c(0, 0.5),
  end  = c(0.5, 1),
  run = FALSE
)
#> # A tibble: 2 × 5
#>   input                                               output start   end command
#>   <chr>                                               <chr>  <dbl> <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/… /home…   0     0.5 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/… /home…   0.5   1   "-y -i…
```

[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
splits a file into its audio and video streams, returning the two
compiled commands:

``` r

separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#>                                                                                                         audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map 0:a \"audio.aac\"" 
#>                                                                                                         video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map 0:v \"video.mp4\""
```

## Running in parallel

Both
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and the fan-out verbs accept `parallel = TRUE`, which maps over jobs
with [furrr](https://furrr.futureverse.org/). Parallelism follows
whatever [future](https://future.futureverse.org/) plan you have set, so
you opt in explicitly:

``` r

library(future)
plan(multisession)

ffm_batch(jobs, parallel = TRUE, .f = function(input, output, ...) {
  ffm_files(input, output) |> ffm_drop("video") |> ffm_codec(audio = "libmp3lame")
})
```

Every job carries its own compiled command in the returned tibble, so a
batch run is fully reproducible: save that column and you have an exact
record of the FFmpeg commands that produced your outputs.

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  — an end-to-end research pipeline that applies these batch tools
  across a study folder.
- [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  — the task verbs and the builder they are made of.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  — reading each file’s metadata as a tibble.
