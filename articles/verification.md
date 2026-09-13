# Checking results and bounding runs

``` r

library(tidymedia)
```

This page answers three questions about processed files. Does an output
have the properties you asked for? Can you later say how a file was
made? And how do you stop a program that hangs?

The examples use a copy of the sample clip that comes with the package:

``` r

file.copy(
  system.file("extdata", "sample.mp4", package = "tidymedia"),
  "session01.mp4"
)
#> [1] TRUE
```

## Check the output against what you asked for

A task function returns the FFmpeg command it ran. That command says
what you asked for.
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
reads the output file and says what you got:

``` r

extract_audio("session01.mp4", "session01.m4a")

verify_media("session01.m4a", audio_codec = "aac", duration = 1)
#> # A tibble: 2 × 5
#>   file          check       expected actual pass 
#>   <chr>         <chr>       <chr>    <chr>  <lgl>
#> 1 session01.m4a duration    1        1      TRUE 
#> 2 session01.m4a audio_codec aac      aac    TRUE
```

The result is a tibble with one row for each check. Its columns are
`file`, `check`, `expected`, `actual` and `pass`. When a check fails,
`actual` shows what the file holds instead:

``` r

verify_media("session01.mp4", duration = 5, width = 1920, audio_codec = "aac")
#> # A tibble: 3 × 5
#>   file          check       expected actual pass 
#>   <chr>         <chr>       <chr>    <chr>  <lgl>
#> 1 session01.mp4 duration    5        1      FALSE
#> 2 session01.mp4 width       1920     320    FALSE
#> 3 session01.mp4 audio_codec aac      aac    TRUE
```

You can check any field that FFprobe reports, by its name:

``` r

verify_media("session01.mp4", pix_fmt = "yuv420p", nb_streams = 2)
#> # A tibble: 2 × 5
#>   file          check      expected actual  pass 
#>   <chr>         <chr>      <chr>    <chr>   <lgl>
#> 1 session01.mp4 pix_fmt    yuv420p  yuv420p TRUE 
#> 2 session01.mp4 nb_streams 2        2       TRUE
```

### What the checks cover

The checks read the file’s metadata, such as its duration, size and
[codec](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
names. They do not judge whether the picture or the sound is good.

A number passes when it is within `tolerance` of the expected value. The
default `tolerance` is `0.1`. So a width or a [sample
rate](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
must match exactly, but a duration can differ a little. A codec name
must match exactly.

A property that the file does not have fails. For example, the audio
file above has no video
[stream](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary),
so a width check fails with an `actual` of `NA`:

``` r

verify_media("session01.m4a", width = 320, audio_codec = "aac")
#> # A tibble: 2 × 5
#>   file          check       expected actual pass 
#>   <chr>         <chr>       <chr>    <chr>  <lgl>
#> 1 session01.m4a width       320      NA     FALSE
#> 2 session01.m4a audio_codec aac      aac    TRUE
```

Another field name is looked for in the
[container](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
information first. Then it is looked for in the video stream, then in
the audio stream. The first match is used.

### Checking every job in a batch

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and the `*_batch()` task functions take a `verify` argument. It adds a
`verified` column to the result. The column is `TRUE` or `FALSE` for
each job that ran and could be checked. It is `NA` for a job that failed
or could not be checked. A named list applies the same checks to every
job:

``` r

jobs <- tibble::tibble(
  input  = "session01.mp4",
  output = "session01.mp3"
)

ffm_batch(
  jobs,
  verify = list(audio_codec = "mp3"),
  .f = function(input, output, ...) {
    ffm_files(input, output) |>
      ffm_drop("video") |>
      ffm_codec(audio = "libmp3lame")
  }
)
#> # A tibble: 1 × 5
#>   input         output        command                           success verified
#>   <chr>         <chr>         <chr>                             <lgl>   <lgl>   
#> 1 session01.mp4 session01.mp3 "-y -i \"session01.mp4\" -codec:… TRUE    TRUE
```

A failed check does not stop the batch. The other jobs still run, and
the `verified` column shows which outputs to look at. To use different
checks for each job, pass a function of the job columns in place of the
list.

## Record how the files were made

The command for each job records what you asked FFmpeg to do. It does
not record which FFmpeg version ran, and two versions can make different
files from one command.

`ffm_batch(manifest = TRUE)` records those facts. The manifest is stored
with the batch result, and
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
reads it:

``` r

res <- ffm_batch(
  jobs,
  manifest = TRUE,
  .f = function(input, output, ...) {
    ffm_files(input, output) |>
      ffm_drop("video") |>
      ffm_codec(audio = "libmp3lame")
  }
)

res
#> # A tibble: 1 × 4
#>   input         output        command                                    success
#>   <chr>         <chr>         <chr>                                      <lgl>  
#> 1 session01.mp4 session01.mp3 "-y -i \"session01.mp4\" -codec:a libmp3l… TRUE

ffm_manifest(res)
#> # A tibble: 1 × 7
#>   command      input output output_size ffmpeg_version ffprobe_version timestamp
#>   <chr>        <chr> <chr>        <dbl> <chr>          <chr>           <chr>    
#> 1 "-y -i \"se… sess… sessi…        8898 6.1.1-3ubuntu5 6.1.1-3ubuntu5  2026-09-…
```

The manifest has one row for each job. The `command`, `input` and
`output` columns repeat the job. The other columns record the run:

- `ffmpeg_version` is the FFmpeg version that did the work, and
  `ffprobe_version` is the FFprobe version that was available. tidymedia
  reads both from the programs. A version it cannot read is `NA`.
- `timestamp` is the time of the run, with its offset from UTC.
- `output_size` is the size of the output file in bytes, so an empty or
  cut-off file shows in the record.

A job with several inputs lists them in one `input` cell, separated by
`;`.

With `checksums = TRUE`, each row also has `input_md5` and `output_md5`.
A checksum lets you show later that a file on disk is the file in the
record:

``` r

res <- ffm_batch(
  jobs,
  manifest = TRUE,
  checksums = TRUE,
  .f = function(input, output, ...) {
    ffm_files(input, output) |>
      ffm_drop("video") |>
      ffm_codec(audio = "libmp3lame")
  }
)

ffm_manifest(res)[, c("input_md5", "output_md5")]
#> # A tibble: 1 × 2
#>   input_md5                        output_md5                      
#>   <chr>                            <chr>                           
#> 1 170526b94587d1a6e52a559eb1239e28 353f557796208278301d66c9de98df11
```

Checksums are off by default, because they read every input and output
file in full. That takes time on a large study.

The `*_batch()` task functions take `manifest` and `checksums` too. So
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
and the others record the same facts.

If you give
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
a `path`, it also writes the manifest to a CSV file. You can then keep
that file next to the processed data:

``` r

ffm_manifest(res, path = "session01_manifest.csv")

file.exists("session01_manifest.csv")
#> [1] TRUE
```

A batch run with `run = FALSE` runs nothing, so it has no manifest.
Reading the manifest of such a batch is an error, not an empty tibble:

``` r

compiled <- ffm_batch(jobs, run = FALSE, manifest = TRUE, .f = function(input, output, ...) {
  ffm_files(input, output) |> ffm_drop("video")
})

ffm_manifest(compiled)
#> Error in `ffm_manifest()`:
#> ! No provenance manifest is attached to `x`.
#> ℹ Run the batch with `ffm_batch(..., manifest = TRUE)` first.
```

## Bound a run that hangs

Each tidymedia call waits for the program it started. If that program
hangs, the R session waits with it. For example, a network drive can
stall, or FFmpeg can get stuck on a damaged file. A time limit stops the
call instead.

The limit is an option, in whole seconds:

``` r

options(tidymedia.timeout = 600)
```

The default is `0`, which means no limit.

To limit one call, wrap it in
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).
When the call ends, the option goes back to its earlier value:

``` r

with_timeout(
  extract_audio("session01.mp4", "bounded.m4a"),
  seconds = 60
)

getOption("tidymedia.timeout")
#> NULL
```

To limit the rest of a function, call
[`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
inside it. Each program started after that line has the limit. When the
function returns, the option goes back to its earlier value:

``` r

convert_all <- function(files) {
  local_timeout(60)
  vapply(files, function(f) extract_audio(f, sub("[.]mp4$", ".m4a", f)),
         character(1))
}

convert_all("session01.mp4")
#>                                                                session01.mp4 
#> "-y -i \"session01.mp4\" -codec:a copy -vn -map \"0:a:0\" \"session01.m4a\""

getOption("tidymedia.timeout")
#> NULL
```

### Limits of the time limit

The limit sets how long R waits, not how long the program runs. A
program that does not stop when asked can make R wait up to 40 seconds
longer than the limit. So choose a limit with room to spare.

The limit applies to each program that tidymedia starts. In a batch of
100 jobs, each job gets the full limit.

The limit must be a whole number of seconds. A value such as `0.5` is an
error:

``` r

with_timeout(extract_audio("session01.mp4", "x.m4a"), seconds = 0.5)
#> Error in `with_timeout()`:
#> ! `seconds` must be a whole number, not the number 0.5.
```

### What happens when the limit is reached

A reached limit always gives an error or a warning. The task functions,
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
the direct commands and
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
stop with an error.

Functions that work on many files give a warning, so the other files are
not lost. The metadata readers return a row of `NA` values.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and the `*_batch()` task functions mark the job with `success = FALSE`.

To handle these in code, use the condition classes:

- `tidymedia_timeout` for the error.
- `tidymedia_probe_timeout` for a warning from a version check or a
  track check.
- `tidymedia_batch_timeout` for the warning from a batch.

The warning from a metadata reader has no condition class of its own.

[`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
lists which function gives which condition.

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  shows a full research example.
- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  shows how to run a task function over many files.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  shows how to read metadata into tibbles.
- [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  lists the package options in one place.
