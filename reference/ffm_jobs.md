# Build a Jobs Table From a Directory

List the media files in a directory and return them as the jobs table
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
takes: a tibble with one row per file and an `input` column of full
paths. This is the batch entry point's companion — the batch story
starts here rather than with a hand-rolled
[`list.files()`](https://rdrr.io/r/base/list.files.html) call.

## Usage

``` r
ffm_jobs(directory, type, extension = NULL, recursive = FALSE)
```

## Arguments

- directory:

  A single string naming an existing directory.

- type:

  The media category to list: `"video"`, `"audio"`, or `"image"`.
  Required — it has no default, since any default would be one of the
  three (D079).

- extension:

  An optional character vector of file extensions narrowing the search
  within `type`, with or without a leading dot (`"mp4"` and `".mp4"`
  both work). Each must be one of the extensions `type` covers; the
  refusal lists them. `NULL` (the default) lists every extension of that
  type.

- recursive:

  A logical: descend into subdirectories (`TRUE`) or list only the top
  level (`FALSE`, default).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per matching file whose name does not start with a dot, and
a single character column, `input`, holding each file's full path. Rows
are in the order [`list.files`](https://rdrr.io/r/base/list.files.html)
returns them. Every row is a path that exists and is not a directory: a
subdirectory whose own name ends in a listed extension is never a row,
nor — on macOS and Linux — is a symbolic link whose target is gone.
Windows reports such a link as existing, so there it can still be a row.
The call aborts rather than returning zero rows when nothing matches.
With `recursive = TRUE` the search follows a symbolic link to a
directory, so a row can name a file outside `directory`.

## Details

The returned tibble carries `input` and nothing else, deliberately:
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
passes every column of the jobs table to `.f` by name, so a column `.f`
has no argument for stops the batch with R's "unused argument" error
unless `.f` takes `...`.
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
and
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
return a column they do not read unchanged — other than one named like a
column
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
adds (see its Value section): a `command` column, for one, is replaced
by the compiled command — and they read a column named like one of their
per-row arguments (each help page lists which) in place of that
argument, row by row: a `width` column in
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
an `audio_codec` column in
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md).
Add the columns your pipeline needs with the usual data-frame tools —
some `*_batch()` verbs want an `output` column, others a task-specific
one such as `start` and `end` — as the examples below derive an `output`
from `input`.

## See also

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
which consumes the returned table.

Other builder functions:
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
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
[`print.tidymedia_ffm()`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)

## Examples

``` r
folder <- system.file("extdata", package = "tidymedia")
jobs <- ffm_jobs(folder, type = "video")
jobs
#> # A tibble: 1 × 1
#>   input                                                       
#>   <chr>                                                       
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4

# Derive an output column, then hand the whole table to ffm_batch().
jobs$output <- file.path(tempdir(), paste0(
  tools::file_path_sans_ext(basename(jobs$input)), ".mp3"
))
ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
  ffm_files(input, output) |> ffm_drop("video")
})
#> # A tibble: 1 × 3
#>   input                                                        output    command
#>   <chr>                                                        <chr>     <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4 /tmp/Rtm… "-y -i…
```
