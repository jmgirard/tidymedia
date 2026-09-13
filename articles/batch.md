# Batch processing

``` r

library(tidymedia)
```

tidymedia is made for running the same job on many files. The examples
on this page use `run = FALSE`. Each function then returns its FFmpeg
commands without running them, so you can read them first. Leave out
`run = FALSE` to process the files.

``` r

folder <- system.file("extdata", package = "tidymedia")
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

## The batch runner

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
runs a job for each row of a jobs table. You also give it a function,
`.f`, that turns one row into a pipeline. Each column of the table goes
to `.f` as an argument of the same name, as in
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html).

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
returns your jobs table with a `command` column added. When
`run = TRUE`, it also adds a `success` column.

[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
makes a jobs table from a folder. It has one row for each file of the
media type you ask for, with the file’s full path in an `input` column.
If the folder has no such files,
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
stops with an error.

Add any other columns that `.f` needs. Here each input gets an `output`:

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

You write the pipeline, so each job can use any pipeline functions. Give
`.f` a `...` argument, so that it accepts table columns it does not use.
Without `...`, such a column stops the batch with R’s “unused argument”
error.

## Batch task functions

For common jobs, you do not need to write `.f`. Each task function has a
`*_batch()` version that takes a jobs table and runs the task on each
row. Examples are
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
and
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md).

Some batch functions, such as
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
can take the table from
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
as it is. Others, such as
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
need an `output` column first:

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
adds `_cropped` to each input name and writes to the input’s folder.
Here that folder is inside the installed package. So the example adds an
`output` column that writes to the working folder instead.

[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
stops with an error if two rows would write the same output file.
[`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
uses several batch functions on a study folder.

## One input, many outputs

Some tasks make many outputs from one input.

[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
cuts a file into pieces at the start and end times you give. It returns
one row for each piece:

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
writes the audio and the video of a file to two files. It returns the
two commands:

``` r

separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#>                                                                                                             audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map \"0:a\" \"audio.aac\"" 
#>                                                                                                             video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map \"0:v\" \"video.mp4\""
```

## Running in parallel

These functions take `parallel = TRUE`:

- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  and each `*_batch()` task function.
- [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  which runs its pieces through
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
- The five metadata readers
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
  [`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  [`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  [`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  and
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md).

[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
does not take it, but
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
does. On
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
[`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
[`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
and
[`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
the argument has an effect only when you pass `infile`.

With `parallel = TRUE`, the jobs run through
[furrr](https://furrr.futureverse.org/). They run in parallel only if
you set a [future](https://future.futureverse.org/) plan. With no plan,
the jobs run one at a time, and R gives a warning that says so:

``` r

library(future)
plan(multisession)

ffm_batch(jobs, parallel = TRUE, .f = function(input, output, ...) {
  ffm_files(input, output) |> ffm_drop("video") |> ffm_codec(audio = "libmp3lame")
})
```

The result has the command for each job. Save that column, and you have
a full record of the FFmpeg commands that made your files.

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  shows a full research example that uses batch functions on a study
  folder.
- [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  explains the task functions and the pipeline functions.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  shows how to read each file’s metadata into a tibble.
