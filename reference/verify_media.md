# Verify a Media File Against Expected Properties

Probe a media file and check its structural metadata against a set of
expectations, returning a tidy pass/fail tibble with one row per checked
property. This turns "the command is reproducible" into "the *result* is
what I asked for": use it after an encode to confirm the output really
has the duration, dimensions, codecs, and so on that the pipeline was
meant to produce.

## Usage

``` r
verify_media(
  file,
  duration = NULL,
  width = NULL,
  height = NULL,
  video_codec = NULL,
  audio_codec = NULL,
  sample_rate = NULL,
  ...,
  tolerance = 0.1
)
```

## Arguments

- file:

  A string naming a single media file to verify.

- duration:

  Expected container duration in seconds (numeric).

- width, height:

  Expected video-frame dimensions in pixels (numeric).

- video_codec, audio_codec:

  Expected codec names for the first video and audio stream (strings,
  e.g. `"h264"`, `"aac"`).

- sample_rate:

  Expected audio sample rate in Hz (numeric).

- ...:

  Further expectations given as `name = value`, each checked against the
  FFprobe field of that name (container first, then video, then audio).

- tolerance:

  The absolute tolerance for numeric checks (default `0.1`).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per checked property and columns `file`, `check`,
`expected`, `actual`, and `pass` (logical).

## Details

Checks are structural (drawn from FFprobe metadata), not perceptual:
this does not measure visual or audio quality. The named arguments cover
the most common properties; pass any other FFprobe field by name through
`...` (for example `pix_fmt = "yuv420p"` or `bit_rate = 141800`). Extra
names are resolved against the probe columns in the order container,
then video stream, then audio stream, and the first match wins.

Numeric checks pass when `abs(actual - expected) <= tolerance`; with the
default `tolerance` of `0.1` this means integer properties (width,
height, sample rate) must match exactly while `duration` is allowed a
little slack (e.g. for keyframe-snapped cuts). String checks (the
codecs) must match exactly. A property whose stream or column is absent
yields an `NA` actual value and a failing check.

## See also

[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
and
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
which accept a `verify =` spec.

Other verification functions:
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
verify_media(video, width = 320, height = 240, video_codec = "h264")
#> # A tibble: 3 × 5
#>   file                                               check expected actual pass 
#>   <chr>                                              <chr> <chr>    <chr>  <lgl>
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata… width 320      320    TRUE 
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata… heig… 240      240    TRUE 
#> 3 /home/runner/work/_temp/Library/tidymedia/extdata… vide… h264     h264   TRUE 
```
