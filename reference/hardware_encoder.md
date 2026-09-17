# Hardware video encoders

These functions help with optional hardware video encoding.
`hardware_encoder()` gives the hardware encoder name for a codec family.
`has_hardware_encoder()` reports whether that encoder is available in
the local FFmpeg build. The package supports two backends: NVIDIA nvenc
(H.264, HEVC and AV1) and Apple videotoolbox (H.264 and HEVC). So
`hardware_encoder("h264", "nvenc")` is `"h264_nvenc"`, and
`hardware_encoder("h264", "videotoolbox")` is `"h264_videotoolbox"`. The
glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, container and hardware encoder.

## Usage

``` r
hardware_encoder(codec = c("h264", "hevc", "av1", "prores"), hardware)

has_hardware_encoder(codec = c("h264", "hevc", "av1", "prores"), hardware)
```

## Arguments

- codec:

  The video codec family: one of `"h264"`, `"hevc"`, `"av1"`, or
  `"prores"`. These are the families the package recognizes, not the
  families a given backend covers. If the chosen `hardware` backend has
  no encoder for a family, the function refuses the call. The error
  names both the backend and the family (e.g. `"av1"` under
  `"videotoolbox"`). Both backends refuse `"prores"` today.

- hardware:

  The backend: `"nvenc"` or `"videotoolbox"`. Required, with no default.
  This set is narrower than the `hardware` argument of the task
  functions. There, `"none"` means "use no backend". That has no meaning
  here, so the function refuses it.

## Value

`hardware_encoder()` returns a single encoder-name string (e.g.
`"h264_nvenc"`). `has_hardware_encoder()` returns a length-one logical.
Neither returns for a `codec` that the chosen `hardware` backend has no
encoder for. That pair is a wrong argument, not a machine without
something. So both give the error that `codec` describes above.
`has_hardware_encoder()` returns `FALSE` only for a pair that the chosen
backend has an encoder for and this FFmpeg build does not list.

## Details

`has_hardware_encoder()` is a *cheap* check. It asks whether FFmpeg
lists the encoder (via
[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)).
That list reflects how FFmpeg was built. It does not reflect whether
working hardware and a driver are present at run time. An encode can
still fail at run time on a machine with no capable GPU. To override
detection in a known environment (or in tests), set
`options(tidymedia.hardware_encoders = )` to a character vector of
encoder names to treat as available.

The `hardware` argument of the task functions uses the same encoder
names and the same check. These task functions have that argument:
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
and
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
(and their `_batch` forms). Some of these functions have a `video_codec`
that defaults to `NULL` (no codec named), and they assume the H.264
family. So a container that does not take H.264 (e.g. `.webm`) needs an
explicit HEVC- or AV1-family `video_codec`. AV1 works only under
`"nvenc"`. Hardware *decoding* (`-hwaccel`) and GPU filter pipelines are
out of scope. Use the
[`ffmpeg`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
direct command for those.

## See also

[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
for the full encoder list. These task functions have the `hardware`
argument:
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
and
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).

Other capability functions:
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md),
[`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md),
[`refresh_ffmpeg_capabilities()`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)

## Examples

``` r
hardware_encoder("h264", "nvenc")
#> [1] "h264_nvenc"
has_hardware_encoder("h264", "nvenc")
#> [1] TRUE
```
