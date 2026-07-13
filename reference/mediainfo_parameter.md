# Query a single parameter from a single MediaInfo section

Query a single parameter in a single section from MediaInfo. `file` may
be a vector of several files, in which case a vector of values (one per
file) is returned.

## Usage

``` r
mediainfo_parameter(file, section, parameter, typed = TRUE)
```

## Arguments

- file:

  A character vector of one or more media-file paths.

- section:

  A string containing the name of the mediainfo section from which to
  query `parameter`.

- parameter:

  A string containing the name of the mediainfo parameter to query from
  `section`.

- typed:

  A logical. When `TRUE` (default) the value is converted to its natural
  type (e.g. a number); when `FALSE` it is returned as a string.

## Value

A vector the same length as `file` holding each requested value, or `NA`
where the value was empty, the section-parameter combination was not
found, or the file could not be read (a warning is issued for unreadable
files rather than aborting).

## See also

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
mediainfo_parameter(video, section = "Video", parameter = "Width")
#> [1] 320
```
