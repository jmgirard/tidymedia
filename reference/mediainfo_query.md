# Query multiple parameters from a single MediaInfo section

Create a tibble containing multiple parameters from a single MediaInfo
section. To query parameters from multiple sections at once, use
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md)
or
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md).
`file` may be a vector of several files; results are stacked with a
leading `file` column.

## Usage

``` r
mediainfo_query(file, section, parameters, names = parameters, typed = TRUE)
```

## Arguments

- file:

  A character vector of one or more media-file paths.

- section:

  A string indicating the MediaInfo section from which to query the
  `parameters`.

- parameters:

  A character vector of one or more MediaInfo parameters to query from
  `section`.

- names:

  A character vector naming the returned columns; must be the same
  length as `parameters` (default = `parameters`). Supplied names are
  used verbatim.

- typed:

  A logical. When `TRUE` (default) numeric columns are typed and empty
  values become `NA`; when `FALSE` columns stay strings.

## Value

A tibble with one row per input file, leading with a `file` column and
one column per requested parameter.

## See also

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_framerate()`](https://jmgirard.github.io/tidymedia/reference/get_framerate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_samplingrate()`](https://jmgirard.github.io/tidymedia/reference/get_samplingrate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
mediainfo_query(video, section = "Video", parameters = c("Width", "Height"))
#> # A tibble: 1 × 3
#>   file                                                         Width Height
#>   <chr>                                                        <int>  <int>
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4   320    240
```
