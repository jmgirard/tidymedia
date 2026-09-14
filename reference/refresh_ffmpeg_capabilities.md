# Forget what tidymedia remembers about your FFmpeg build

Discard the package's record of which encoders your FFmpeg build has.
The next query then asks FFmpeg again.

## Usage

``` r
refresh_ffmpeg_capabilities()
```

## Value

`NULL`, invisibly. Called for its side effect.

## Details

The first call in an R session that uses `hardware = "nvenc"` or
`hardware = "videotoolbox"` asks FFmpeg which encoders it has. The
package remembers that answer for the rest of the session. Later calls
reuse it and do not start FFmpeg again each time, so a large batch stays
fast.

So the package does not see a change to your FFmpeg build until you
discard the record. Examples of a change are a new FFmpeg install, a new
graphics card (GPU) driver, or a different FFmpeg program. There are
three ways to discard the record:

- Call `refresh_ffmpeg_capabilities()` yourself, at any time.

- Call
  [`set_program`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  (or
  [`set_ffmpeg`](https://jmgirard.github.io/tidymedia/reference/set_program.md)).
  It discards the record for you, because the record describes the old
  program.

- Call
  [`unset_program`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
  and have it remove something. When it forgets a saved location, the
  package can find a different program. A call that removed nothing
  keeps the record, because the program in use did not change. A call
  that removed one saved file and then failed on another discards the
  record. The file it removed may have named the program that the record
  came from.

The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as encoder and hardware encoder.

## Parallel workers

Each R process keeps its own record, and a worker does not get the
record of your session. So in a batch on `W` workers, each worker asks
FFmpeg once. Your session can also ask once, before the jobs start.
Discarding the record in your session does not reach the workers.

The `tidymedia.hardware_encoders` option works in a different way. The
package copies your value into each worker for the duration of the call,
and then puts back the worker's own value. So a batch under your setting
does not ask FFmpeg for an encoder list at all. Every worker gives the
same answer as your session.

## Functions that never use the record

[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
and
[`ffmpeg_codecs`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
ask FFmpeg on every call. So they always show the build as it is now,
whether or not you called this function.

## See also

[`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
uses the remembered answer.
[`hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
gives the encoder name without asking FFmpeg.
[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
always gives a fresh encoder list.
[`set_program`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
points the package at a different FFmpeg program.

Other capability functions:
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md),
[`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md),
[`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)

## Examples

``` r
# After installing FFmpeg, or a GPU driver or OS update mid-session:
refresh_ffmpeg_capabilities()
```
