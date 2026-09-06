# Checking results and bounding runs

``` r

library(tidymedia)
```

A processed file raises two questions that the pipeline itself cannot
answer. Did the encode actually produce what you asked for? And a year
later, can you say how a given file was made? This vignette covers the
three facilities that answer them — checking an output against
expectations, recording a run’s provenance, and bounding a run that
hangs so it stops the call instead of your session.

We work on a copy of the sample clip that ships with the package:

``` r

file.copy(
  system.file("extdata", "sample.mp4", package = "tidymedia"),
  "session01.mp4"
)
#> [1] TRUE
```

## Check the output against what you asked for

Every verb returns the FFmpeg command it compiled, and that command is a
record of what you *asked* for.
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
answers the other half: it probes a file and compares its actual
properties against your expectations, one row per check.

``` r

extract_audio("session01.mp4", "session01.m4a")

verify_media("session01.m4a", audio_codec = "aac", duration = 1)
#> # A tibble: 2 × 5
#>   file          check       expected actual pass 
#>   <chr>         <chr>       <chr>    <chr>  <lgl>
#> 1 session01.m4a duration    1        1      TRUE 
#> 2 session01.m4a audio_codec aac      aac    TRUE
```

The report is a tibble with `file`, `check`, `expected`, `actual` and
`pass`, so a failure tells you which property was wrong and what the
file holds instead:

``` r

verify_media("session01.mp4", duration = 5, width = 1920, audio_codec = "aac")
#> # A tibble: 3 × 5
#>   file          check       expected actual pass 
#>   <chr>         <chr>       <chr>    <chr>  <lgl>
#> 1 session01.mp4 duration    5        1      FALSE
#> 2 session01.mp4 width       1920     320    FALSE
#> 3 session01.mp4 audio_codec aac      aac    TRUE
```

Three things are worth knowing before you rely on it.

**The checks are structural, not perceptual.** They read FFprobe’s
metadata — duration, dimensions, codec names, sample rate — and say
nothing about whether the picture or the sound looks and sounds right.
An encode that came out at the right size in the right codec and looks
terrible passes every check above.

**Numeric checks carry a tolerance; string checks do not.** A numeric
expectation passes when `abs(actual - expected) <= tolerance`, and
`tolerance` defaults to `0.1`. That is exact for integers like width and
sample rate, and a little slack for `duration`, which moves when a cut
snaps to a keyframe. Codec names must match exactly.

**A property that is not there fails.** If the file has no stream of
that kind, or FFprobe reports no such field, the actual value is `NA`
and the check fails rather than being skipped. The audio file extracted
above has no picture, so asking it for a width fails with nothing to
report:

``` r

verify_media("session01.m4a", width = 320, audio_codec = "aac")
#> # A tibble: 2 × 5
#>   file          check       expected actual pass 
#>   <chr>         <chr>       <chr>    <chr>  <lgl>
#> 1 session01.m4a width       320      NA     FALSE
#> 2 session01.m4a audio_codec aac      aac    TRUE
```

Beyond the named arguments, any FFprobe field can be checked by name.
Extra names are resolved against the container first, then the video
stream, then the audio stream, and the first match wins:

``` r

verify_media("session01.mp4", pix_fmt = "yuv420p", nb_streams = 2)
#> # A tibble: 2 × 5
#>   file          check      expected actual  pass 
#>   <chr>         <chr>      <chr>    <chr>   <lgl>
#> 1 session01.mp4 pix_fmt    yuv420p  yuv420p TRUE 
#> 2 session01.mp4 nb_streams 2        2       TRUE
```

### Checking every job in a batch

[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and the `*_batch()` verbs take the same expectations through a
`verify =` argument and add a `verified` column to the result, one
logical per job. Pass a named list to hold every job to the same
expectations:

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

A failed check here marks the row and does not stop the batch — the
remaining jobs still run, and `verified` tells you afterwards which
outputs to look at. For expectations that differ per job, pass a
function of the job columns instead of a list, and it is called once per
row.

## Record how the files were made

A compiled command is a complete recipe, and
[`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
builds its reproducibility story on exactly that: capture the command
each verb returns — or the `command` column the batch runner adds — and
you can re-run any step. The command is what you *asked* for, though,
and it is silent about the run itself. Two FFmpeg versions accept the
same command and need not produce the same file, which is why the
manifest below records the version that ran.

`ffm_batch(manifest = TRUE)` records the rest. The result itself looks
like any other batch result — the manifest rides along on it as an
attribute, and
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
reads it out:

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

One row per job. `command`, `input` and `output` restate the job; the
other columns are what the command cannot carry:

- `ffmpeg_version` and `ffprobe_version` — the versions tidymedia
  resolved when the batch ran, read out of the binaries themselves
  rather than assumed. `ffmpeg_version` is the version that did the
  work; `ffprobe_version` is the one that was on hand to probe, which a
  job like this one never needed. A version that could not be read is
  `NA`.
- `timestamp` — when the run happened, with its UTC offset.
- `output_size` — the size in bytes of the file that came out, so a
  truncated or empty result is visible in the record itself.

Multiple inputs — a stacked or concatenated job — are joined by `;` in
the one `input` cell.

Ask for `checksums = TRUE` and each row also carries `input_md5` and
`output_md5`, which is what lets you say later that a file on disk is
the file this row describes:

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

ffm_manifest(res)
#> # A tibble: 1 × 9
#>   command      input output output_size ffmpeg_version ffprobe_version timestamp
#>   <chr>        <chr> <chr>        <dbl> <chr>          <chr>           <chr>    
#> 1 "-y -i \"se… sess… sessi…        8898 6.1.1-3ubuntu5 6.1.1-3ubuntu5  2026-09-…
#> # ℹ 2 more variables: input_md5 <chr>, output_md5 <chr>

ffm_manifest(res)[, c("input_md5", "output_md5")]
#> # A tibble: 1 × 2
#>   input_md5                        output_md5                      
#>   <chr>                            <chr>                           
#> 1 170526b94587d1a6e52a559eb1239e28 353f557796208278301d66c9de98df11
```

The manifest is nine columns wide now, so at a typical console width the
two new ones print as a footer line rather than in the table; the second
call above pulls them out to show what they hold.

Checksums are off by default because they read every input and output in
full, which is real time on a large study.

`manifest =` and `checksums =` reach the `*_batch()` verbs too, so a
pipeline built from
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
and its siblings records provenance the same way without dropping down
to
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

Give
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
a `path` and it writes the manifest as CSV as well as returning it,
invisibly — one file to commit next to the processed data:

``` r

ffm_manifest(res, path = "session01_manifest.csv")

file.exists("session01_manifest.csv")
#> [1] TRUE
```

A manifest is a record of a run, so there is nothing to record when
nothing ran. A `run = FALSE` batch attaches none even when you ask for
one, and reading it back is an error rather than an empty tibble — a
silent empty record being the failure a provenance record exists to
prevent:

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

Every tidymedia call waits for the program it started to finish. A
program that hangs — a network path that stalls, a malformed file FFmpeg
will not give up on — takes the R session down with it, and in a batch
of a thousand files that is the whole run. Set a wall-clock limit and
the hang stops the call instead.

The limit is a session option, in whole seconds:

``` r

options(tidymedia.timeout = 600)
```

Unset, it is `0`, which means no limit — so nothing you already run
changes behavior until you ask for a bound.

To bound one call rather than the session, wrap it in
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).
The session’s setting, or the absence of one, is back when the call
returns, by any route:

``` r

with_timeout(
  extract_audio("session01.mp4", "bounded.m4a"),
  seconds = 60
)

getOption("tidymedia.timeout")
#> NULL
```

To bound the rest of a function rather than an expression you wrap, say
it as a statement with
[`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md).
Every program started after that line is bounded, and the caller’s
setting comes back when the function returns:

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

### What the limit actually bounds

The limit bounds **how long R waits**, not how long the program runs,
and the two are not the same number. R escalates: it interrupts the
program at the limit, asks it to terminate 20 seconds later, and kills
it 20 seconds after that. A program that ignores the first two signals
is therefore waited for up to **40 seconds longer than the limit you
set** — measured on 2026-08-28 at 42.0 s under a 2 s limit, on a Linux
runner, with the same reading on a macOS host. Set a limit meaning “give
up somewhere around here”, not “return at exactly this second”, and
leave that headroom in anything downstream that depends on the call
returning.

A fractional limit is refused rather than rounded, because R truncates
it toward zero and `0` is the no-limit sentinel, so a rounded `0.5`
would leave the call unbounded. Every seam refuses it:
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md),
as below, and equally
[`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
and `options(tidymedia.timeout = 0.5)` when a call reads the option:

``` r

with_timeout(extract_audio("session01.mp4", "x.m4a"), seconds = 0.5)
#> Error in `with_timeout()`:
#> ! `seconds` must be a whole number, not the number 0.5.
```

### A reached limit is never silent

Every call that can start one of these programs either aborts or warns
when the limit is reached; none of them quietly returns as though
nothing happened.

It **aborts**, naming the program and the limit, from the task verbs,
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
the escape hatches
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
and
[`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md),
and from
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
— a probe that never answered is not an answer of “no”, and absorbing it
there would blame a good output for the probe’s failure.

It **warns** where one hung file must not discard the rest of the work.
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
and the `probe_*()` accessors, the MediaInfo readers and the `get_*()`
helpers give an `NA` row and one warning at the end of the call.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
and the `*_batch()` verbs mark the row `success = FALSE`, as for any
failed job, and warn once at the end saying how many jobs timed out. The
provenance manifest above warns that it could not read a version, and
records `NA` for it.

To act on either outcome in code, the abort carries the condition class
`tidymedia_timeout`; the version-probe and dropped-track warnings carry
`tidymedia_probe_timeout`, and the batch warning
`tidymedia_batch_timeout`.

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  — an end-to-end preprocessing pipeline.
- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  — the jobs-tibble batch runner in depth.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  — reading each file’s metadata as a tibble.
- [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  — the package’s option seams in one place.
