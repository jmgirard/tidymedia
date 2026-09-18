# Build a Jobs Table From a Directory

List the media files in a directory and return them as a jobs table for
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
The table is a tibble with one row per file and an `input` column of
full paths. Start a batch here, instead of with your own
[`list.files()`](https://rdrr.io/r/base/list.files.html) call.

## Usage

``` r
ffm_jobs(directory, type, extension = NULL, recursive = FALSE)
```

## Arguments

- directory:

  A single string naming an existing directory.

- type:

  The media category to list, one of `"video"`, `"audio"` or `"image"`.
  You must give it, because it has no default.

- extension:

  An optional character vector of file extensions narrowing the search
  within `type`, with or without a leading dot (`"mp4"` and `".mp4"`
  both work). Each must be one of the extensions `type` covers, and the
  error message lists them. `NULL` (the default) lists every extension
  of that type.

- recursive:

  A logical: descend into subdirectories (`TRUE`) or list only the top
  level (`FALSE`, default).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per matching file and one character column, `input`, with
each file's full path. Files whose names start with a dot are left out.
Rows are in the order
[`list.files`](https://rdrr.io/r/base/list.files.html) returns them.

Every row is a path that exists and is not a directory. A subdirectory
whose own name ends in a listed extension is never a row. On macOS and
Linux, a symbolic link whose target is gone is never a row either.
Windows reports such a link as existing, so there it can still be a row.
The call gives an error, instead of zero rows, when nothing matches.
With `recursive = TRUE` the search follows a symbolic link to a
directory, so a row can name a file outside `directory`.

The scanned extensions include `.mka` as audio and `.ts` as video.
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
recommends `.mka` or `.m4a` for multi-track audio, and this function
reads both as audio, so it lists a folder of that output. Not every
container that can hold several audio streams is audio here. `.ts`, like
`.mp4` and `.mkv`, is video, so multi-track audio written to one of
those is a row under `type = "video"`. The name `.ts` also belongs to
TypeScript source files, and this function reads names rather than file
contents. A folder of TypeScript sources therefore comes back as video
rows when you ask for `type = "video"`.

## Details

The table has only the `input` column.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
passes every column of the jobs table to `.f` by name. So if `.f` has no
argument for a column, and no `...` argument, the batch stops with R's
"unused argument" error.

Add the columns your pipeline needs with the usual data-frame tools.
Some `*_batch()` task functions need an `output` column. Others need
columns for their task, such as `start` and `end`. The examples below
make an `output` column from `input`.

[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
and
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
handle the other columns of the table as follows:

- They read a column named like one of their per-row arguments in place
  of that argument, row by row. Each help page lists these arguments.
  Examples are a `width` column in
  [`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
  and an `audio_codec` column in
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md).

- They replace a column named like one that
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  adds. For example, the compiled command replaces a `command` column.
  The Value section of
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  lists the added columns.

- They return every other column unchanged.

## See also

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
which consumes the returned table.

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
# Two inputs sharing a name (a.mp4 and a.mkv) would derive one output here;
# ffm_batch() refuses jobs that share an output before any of them runs.
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
