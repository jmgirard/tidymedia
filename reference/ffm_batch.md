# Run an FFmpeg Pipeline Over Many Files

Apply a pipeline-building function to every row of a jobs table and
compile (and optionally run) the resulting FFmpeg command for each. This
is tidymedia's batch-processing entry point: one reproducible compiled
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
  sequentially (`FALSE`, default). Parallelism follows the
  [`future`](https://future.futureverse.org/reference/plan.html) plan
  the caller has set; with `TRUE` but the default sequential plan, jobs
  still run one at a time and a warning is issued. Set a plan first,
  e.g. `future::plan(future::multisession)`.

- verify:

  An optional output check applied to each job (only when `run = TRUE`).
  Either a named list of expected properties (the same spec for every
  job, e.g. `list(width = 1920)`) or a function of the job columns
  (called
  [`pmap`](https://purrr.tidyverse.org/reference/pmap.html)-style, like
  `.f`) that returns such a list per job. Each job's output is passed to
  [`verify_media`](https://jmgirard.github.io/tidymedia/reference/verify_media.md);
  unlike
  [`ffm_run`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  a failed check is *recorded*, not aborted. Adds a logical `verified`
  column (all checks passed), `NA` for jobs that did not run
  successfully.

- progress:

  A logical: display a cli progress bar as the jobs run (`TRUE`) or run
  quietly (`FALSE`, default). Only applies when `run = TRUE`; safe (a
  no-op animation) in non-interactive sessions.

- manifest:

  A logical: when `TRUE` (and `run = TRUE`), record a provenance
  manifest (per-job command, FFmpeg/FFprobe versions, timestamp, output
  size) and attach it to the result, readable with
  [`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md).
  (default = `FALSE`)

- checksums:

  A logical: when `TRUE`, the manifest also captures md5 checksums of
  each job's input(s) and output. Ignored unless `manifest = TRUE`.
  (default = `FALSE`)

## Value

`jobs` as a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with an added `command` column (the compiled FFmpeg command for each
job) and, when `run = TRUE`, a logical `success` column (plus a
`verified` column when `verify` is supplied). When `manifest = TRUE` a
provenance manifest is attached as an attribute; read it with
[`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md).

## Details

Each column of `jobs` is passed by name to `.f` (as
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
does), so a job table with columns `input`, `output` and `start` calls
`.f(input = ..., output = ..., start = ...)`. `.f` must return an ffm
pipeline (see
[`ffm_files`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)).
Give `.f` a `...` argument if `jobs` carries columns it does not use.

## See also

[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
which is built on `ffm_batch()`;
[`verify_media`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
for the verification spec and
[`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
for the provenance manifest.

Other builder functions:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md),
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
# run = FALSE compiles one reproducible command per job without calling FFmpeg
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
