# Get the duration of a media file

Use MediaInfo to quickly look up the duration of different sections of a
media file in various units.

## Usage

``` r
get_duration(
  file,
  section = c("General", "Video", "Audio"),
  unit = c("ms", "sec", "min", "hour")
)
```

## Arguments

- file:

  A character vector of one or more media-file paths.

- section:

  A string indicating the MediaInfo section from which to query the
  duration value. Can be either `"General"`, `"Video"`, or `"Audio"`
  (default = `"General"`).

- unit:

  A string indicating whether the duration should be returned in
  milliseconds (`"ms"`), seconds (`"sec"`), minutes (`"min"`), or hours
  (`"hour"`) (default = `"ms"`).

## Value

A double vector (one per file) giving the duration of the specified
section in the specified units.

## Details

This **MediaInfo**-backed helper returns a **single value per file** (a
numeric scalar), unlike the tibble-returning `probe_*()`,
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
and
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
readers.

## See also

[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
for arbitrary MediaInfo fields, and
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
for the FFprobe backend.

Other metadata functions:
[`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
get_duration(video, unit = "sec")
#> [1] 1
```
