# Run an FFmpeg Pipeline Over Many Files

Apply a pipeline-building function to every row of a jobs table and
compile (and optionally run) the resulting FFmpeg command for each. This
is the package's main batch function. It gives one reproducible compiled
command per job, collected back into a tibble.

## Usage

``` r
ffm_batch(
  jobs,
  .f,
  ...,
  run = TRUE,
  parallel = FALSE,
  verify = NULL,
  progress = FALSE,
  manifest = FALSE,
  checksums = FALSE
)
```

## Arguments

- jobs:

  A data frame with one row per job. Its column names are the arguments
  passed to `.f`.

- .f:

  A function that takes a job's columns (by name) and returns an ffm
  pipeline object.

- ...:

  Additional arguments passed on to every call of `.f`.

- run:

  A logical: run each compiled command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`, a dry run).

- parallel:

  A logical: map over jobs in parallel with furrr (`TRUE`) or
  sequentially (`FALSE`, default). Parallel runs follow the
  [`future`](https://future.futureverse.org/reference/plan.html) plan
  that you set. With `TRUE` and the default sequential plan, jobs still
  run one at a time, and you get a warning. Set a plan first, for
  example `future::plan(future::multisession)`.

- verify:

  An optional output check applied to each job (only when `run = TRUE`).
  Give a named list of expected properties, or a function. A list, for
  example `list(width = 1920)`, applies the same checks to every job. A
  function takes the job columns like `.f` (called
  [`pmap`](https://purrr.tidyverse.org/reference/pmap.html)-style) and
  returns such a list for each job. Each job's output is passed to
  [`verify_media`](https://jmgirard.github.io/tidymedia/reference/verify_media.md).
  Unlike
  [`ffm_run`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  a failed check is *recorded*, and does not stop the call. Adds a
  logical `verified` column (all checks passed), `NA` for jobs that did
  not run successfully.

- progress:

  A logical: display a cli progress bar as the jobs run (`TRUE`) or run
  quietly (`FALSE`, default). Only applies when `run = TRUE`; safe (a
  no-op animation) in non-interactive sessions.

- manifest:

  A logical. When `TRUE` (and `run = TRUE`), the batch records a
  provenance manifest and attaches it to the result. The manifest has
  each job's command, the FFmpeg and FFprobe versions, a timestamp and
  the output size. Read it with
  [`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md).
  (default = `FALSE`)

- checksums:

  A logical: when `TRUE`, the manifest also captures md5 checksums of
  each job's input(s) and output. Ignored unless `manifest = TRUE`.
  (default = `FALSE`)

## Value

`jobs` as a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with an added `command` column, which holds the compiled FFmpeg command
for each job. When `run = TRUE`, it also has a logical `success` column.
When `verify` is supplied, it also has a `verified` column. When
`manifest = TRUE`, a provenance manifest is attached as an attribute;
read it with
[`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md).

## Details

Each column of `jobs` is passed by name to `.f` (as
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
does), so a job table with columns `input`, `output` and `start` calls
`.f(input = ..., output = ..., start = ...)`. `.f` must return a
pipeline (see
[`ffm_files`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)).
Give `.f` a `...` argument if `jobs` carries columns it does not use.

Two jobs whose pipelines write to the same `output` path are refused
before any job runs, under `run = FALSE` as well as `run = TRUE`. Paths
are compared exactly as written. An output that writes no file may
repeat. Such outputs are `-` (standard output), a `pipe:` URL, and an
output whose last `-f` option is `-f null`, as
`ffm_output_options("-f null")` gives.

[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
explains how to limit how long R waits for each program in a job, and
what happens when a program reaches the limit.

## See also

[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
which is built on `ffm_batch()`;
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
for the verification spec and
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
for the provenance manifest.

Other pipeline functions:
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
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input  = c(video, video),
  output = c("a.mp3", "b.mp3")
)
# run = FALSE compiles one command per job without calling FFmpeg
ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
  ffm_files(input, output) |>
    ffm_drop("video") |>
    ffm_codec(audio = "libmp3lame")
})
#> # A tibble: 2 × 3
#>   input                                                        output command   
#>   <chr>                                                        <chr>  <chr>     
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 a.mp3  "-y -i \"…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 b.mp3  "-y -i \"…
```
