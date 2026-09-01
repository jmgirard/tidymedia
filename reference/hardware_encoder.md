# Hardware video encoders

Helpers for opt-in hardware video encoding. `hardware_encoder()` maps a
codec family to its hardware encoder name; `has_hardware_encoder()`
reports whether that encoder is available in the local FFmpeg build.
NVIDIA nvenc is the one backend, so `hardware_encoder("h264")` is
`"h264_nvenc"`.

## Usage

``` r
hardware_encoder(codec = c("h264", "hevc", "av1"))

has_hardware_encoder(codec = c("h264", "hevc", "av1"))
```

## Arguments

- codec:

  The video codec family: one of `"h264"`, `"hevc"`, or `"av1"`.

## Value

`hardware_encoder()` a single encoder-name string (e.g. `"h264_nvenc"`);
`has_hardware_encoder()` a length-one logical.

## Details

`has_hardware_encoder()` is a *cheap* check: it asks whether FFmpeg
lists the encoder (via
[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)),
which reflects how FFmpeg was built, not whether a working NVIDIA GPU
and driver are present at run time. An encode can still fail at run time
on a machine with no capable GPU. To override detection in a known
environment (or in tests), set `options(tidymedia.hardware_encoders = )`
to a character vector of encoder names to treat as available.

These back the `hardware = "nvenc"` toggle on
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
and
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
(and their `_batch` siblings). On the verbs whose `video_codec` defaults
to `NULL` (no codec named), the H.264 family is assumed under
`hardware = "nvenc"`, so a non-H.264 container (e.g. `.webm`) needs an
explicit HEVC- or AV1-family `video_codec`. Hardware *decoding*
(`-hwaccel`) and GPU filter pipelines are out of scope; use the
[`ffmpeg`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
escape hatch for those.

## See also

[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
for the full encoder list,
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
and
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
for the `hardware = "nvenc"` toggle that uses these.

Other capability functions:
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md),
[`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md),
[`refresh_ffmpeg_capabilities()`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)

## Examples

``` r
hardware_encoder("h264")
#> [1] "h264_nvenc"
has_hardware_encoder("h264")
#> [1] TRUE
```
