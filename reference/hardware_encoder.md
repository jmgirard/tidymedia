# Hardware video encoders

Helpers for opt-in hardware video encoding. `hardware_encoder()` maps a
codec family to its hardware encoder name; `has_hardware_encoder()`
reports whether that encoder is available in the local FFmpeg build. Two
backends are supported: NVIDIA nvenc (H.264, HEVC and AV1) and Apple
videotoolbox (H.264 and HEVC), so `hardware_encoder("h264", "nvenc")` is
`"h264_nvenc"` and `hardware_encoder("h264", "videotoolbox")` is
`"h264_videotoolbox"`.

## Usage

``` r
hardware_encoder(
  codec = c("h264", "hevc", "av1", "prores"),
  hardware,
  call = rlang::current_env()
)

has_hardware_encoder(codec = c("h264", "hevc", "av1", "prores"), hardware)
```

## Arguments

- codec:

  The video codec family: one of `"h264"`, `"hevc"`, `"av1"`, or
  `"prores"`. These are the families the package recognizes, not the
  families a given backend covers: a family the chosen `hardware`
  backend has no encoder for is refused naming both the backend and the
  family (e.g. `"av1"` under `"videotoolbox"`). `"prores"` is refused by
  both backends today.

- hardware:

  The backend: `"nvenc"` or `"videotoolbox"`. Required, with no default.
  Narrower than the verbs' `hardware` argument: `"none"` is the verbs'
  off position, meaning "use no backend", which has no meaning here, so
  it is refused.

- call:

  The environment a refusal is reported from, so a verb that consults
  these internally is blamed rather than the helper. Rarely set
  directly.

## Value

`hardware_encoder()` a single encoder-name string (e.g. `"h264_nvenc"`);
`has_hardware_encoder()` a length-one logical. Neither returns for a
`codec` the chosen `hardware` backend has no encoder for: that pair is a
wrong argument rather than a machine without something, so both raise
the error `codec` describes above. `has_hardware_encoder()` returns
`FALSE` only for a pair the table holds and this FFmpeg build does not
list.

## Details

`has_hardware_encoder()` is a *cheap* check: it asks whether FFmpeg
lists the encoder (via
[`ffmpeg_encoders`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)),
which reflects how FFmpeg was built, not whether working hardware and a
driver are present at run time. An encode can still fail at run time on
a machine with no capable GPU. To override detection in a known
environment (or in tests), set `options(tidymedia.hardware_encoders = )`
to a character vector of encoder names to treat as available.

These back the `hardware` toggle on
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
to `NULL` (no codec named), the H.264 family is assumed, so a non-H.264
container (e.g. `.webm`) needs an explicit HEVC- or AV1-family
`video_codec` (AV1 only under `"nvenc"`). Hardware *decoding*
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
for the `hardware` toggle that uses these.

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
