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
