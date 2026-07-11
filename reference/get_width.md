# Get the video width of a media file

Use MediaInfo to quickly look up the video width of a media file in
pixels (px).

## Usage

``` r
get_width(file)
```

## Arguments

- file:

  A character vector of one or more media-file paths.

## Value

A double vector (one per file) giving the video width in px.

## See also

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_framerate()`](https://jmgirard.github.io/tidymedia/reference/get_framerate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_samplingrate()`](https://jmgirard.github.io/tidymedia/reference/get_samplingrate.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
get_width(video)
#> [1] 320
```
