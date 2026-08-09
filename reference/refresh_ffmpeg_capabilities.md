# Forget what tidymedia remembers about your FFmpeg build

Discard the session-scoped record of which encoders your FFmpeg build
provides, so the next capability query asks FFmpeg again.

## Usage

``` r
refresh_ffmpeg_capabilities()
```

## Value

`NULL`, invisibly. Called for its side effect.

## Details

The first `hardware = "nvenc"` call in an R session asks FFmpeg which
encoders it has; later calls reuse that answer rather than starting a
new FFmpeg process per call, which is what makes a large batch
practical. The answer is remembered for the rest of the session, so a
build that changes underneath you – a fresh FFmpeg install, a new GPU
driver, a different binary – is not seen until the record is discarded.
There are two ways to discard it:

- call `refresh_ffmpeg_capabilities()` yourself, at any time;

- call
  [`set_program`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  (or
  [`set_ffmpeg`](https://jmgirard.github.io/tidymedia/reference/set_program.md)),
  which discards it for you, since pointing tidymedia at a different
  binary invalidates everything remembered about the old one.

The record is per R process. Under `parallel = TRUE` each worker keeps
its own, so a batch running on `W` workers asks FFmpeg `W` times rather
than once, and discarding it in the parent does not reach them.

[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
and
[`ffmpeg_codecs`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
are never remembered: they query FFmpeg on every call, so they always
report the build as it is now, whether or not this function has been
called.

## See also

[`has_nvenc`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
and
[`nvenc_encoder`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the queries that use the remembered answer,
[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
for an always-fresh encoder list, and
[`set_program`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to point tidymedia at a different binary.

Other capability functions:
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md),
[`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md),
[`nvenc_encoder()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)

## Examples

``` r
# After installing FFmpeg or an NVIDIA driver mid-session:
refresh_ffmpeg_capabilities()
```
