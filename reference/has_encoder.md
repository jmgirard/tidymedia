# Ask whether your FFmpeg build has an encoder

FFmpeg builds differ in the encoders they were compiled with. A build
without `libx264` refuses every call that names it, and some Linux
distributions ship exactly that build, because x264 is not free
software. This function answers whether one encoder is in the build this
package resolves. You can then name another encoder, or skip the work,
before you run anything.

## Usage

``` r
has_encoder(name)
```

## Arguments

- name:

  A string naming one encoder, spelled as
  [`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  spells it, such as `"libx264"`, `"aac"` or `"libmp3lame"`.

## Value

A single `TRUE` or `FALSE`.

## Details

The answer comes from the list
[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
returns. The list is asked of FFmpeg once per session and then
remembered, so a repeated call does not start FFmpeg again.
[`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
makes the next call ask again.

The `tidymedia.hardware_encoders` option does not change this answer.
That option names the hardware encoders of the machine, and this
question is about the whole build. Use
[`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the GPU backends.

Where the package finds no FFmpeg at all, the answer is `FALSE`: nothing
can be encoded, so no encoder is available. A location you saved with
[`set_ffmpeg`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
that no longer works still gives its warning beside the `FALSE`. So a
machine you can repair is never reported silently as a build without the
encoder.

## See also

[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
for the whole list,
[`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the GPU backends, and
[`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
to make the next call ask FFmpeg again.

Other capability functions:
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md),
[`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md),
[`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md),
[`refresh_ffmpeg_capabilities()`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)

## Examples

``` r
has_encoder("libx264")
#> [1] TRUE
has_encoder("no_such_encoder")
#> [1] FALSE
```
