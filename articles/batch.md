# Batch processing

``` r

library(tidymedia)
```

Batch processing over many files is what tidymedia is built for. The
examples below use `run = FALSE`, which **compiles** each FFmpeg command
without executing it — so you can inspect exactly what would run. Drop
that argument (or set `run = TRUE`) to actually process the files.

``` r

folder <- system.file("extdata", package = "tidymedia")
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

[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
builds that tibble from a folder: one row per file of the media type you
ask for, with the file’s full path in an `input` column. When the folder
holds no file of that type,
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
stops with an error rather than returning an empty table. Add any other
columns your function needs, here an `output` for each input:

``` r

jobs <- ffm_jobs(folder, type = "video")
jobs$output <- paste0(tools::file_path_sans_ext(basename(jobs$input)), ".mp3")

ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
  ffm_files(input, output) |>
    ffm_drop("video") |>
    ffm_codec(audio = "libmp3lame")
})
#> # A tibble: 1 × 3
#>   input                                                        output    command
#>   <chr>                                                        <chr>     <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 sample.m… "-y -i…
```

Because you build the pipeline yourself, any combination of builder
verbs is available per job. Give `.f` a `...` argument so it tolerates
extra job columns it does not use: without one, a column `.f` has no
argument for stops the batch with R’s “unused argument” error.

## Per-verb batch siblings

You do not have to write `.f` by hand for the common jobs. Every task
verb ships a `*_batch()` companion —
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
and the rest — that takes a jobs tibble directly and applies the verb to
each row.
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
can take the table straight from
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md);
others, such as
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
refuse it until you add an `output` column.
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
again stops with an error rather than returning an empty table when the
folder holds no file of the type:

``` r

jobs <- ffm_jobs(folder, type = "video")
jobs$output <- paste0(tools::file_path_sans_ext(basename(jobs$input)), "_cropped.mp4")

crop_video_batch(jobs, width = 160, height = 120, run = FALSE)
#> # A tibble: 1 × 3
#>   input                                                        output    command
#>   <chr>                                                        <chr>     <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 sample_c… "-y -i…
```

Without an `output` column,
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
names each output after its input, adding `_cropped`, in the input’s own
folder. Here that folder is inside the installed package, so the example
adds an `output` column that puts the files in the working directory
instead. Either way
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
rejects two rows that would resolve to the same output path.
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
#>                                                                                                             audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map \"0:a\" \"audio.aac\"" 
#>                                                                                                             video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map \"0:v\" \"video.mp4\""
```

## Running in parallel

`parallel = TRUE` is accepted by
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and by every `*_batch` verb; by
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
which sends its own segments through the same machinery; and by the five
metadata readers
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
[`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
[`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
and
[`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md).
On the four reader shortcuts it applies when you pass `infile` and is
ignored when you hand them an existing `probe` object, which has nothing
left to fan out. Nothing else takes the argument —
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
compiles its two commands directly rather than through
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
so only
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
accepts it.

The argument maps over jobs with
[furrr](https://furrr.futureverse.org/), and parallelism follows
whatever [future](https://future.futureverse.org/) plan you have set, so
you opt in explicitly. With no plan set, `parallel = TRUE` still runs
one job at a time, and warns to tell you so:

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
