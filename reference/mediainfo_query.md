# Query multiple parameters from a single MediaInfo section

`mediainfo_query()` uses the MediaInfo program to read several
parameters from one section, and returns a tibble. To read parameters
from more than one section in one call, use
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md).

## Usage

``` r
mediainfo_query(file, section, parameters, names = parameters, typed = TRUE)
```

## Arguments

- file:

  A character vector of one or more media file paths.

- section:

  A string. The name of the MediaInfo section to read `parameters` from.

- parameters:

  A character vector of one or more MediaInfo parameters to read from
  `section`.

- names:

  A character vector of column names, one for each element of
  `parameters`. The default is `parameters`. The function keeps the
  names as you give them, but removes spaces at their start and end.

- typed:

  A logical. If `TRUE` (the default), numeric columns become numbers and
  empty values become `NA`. If `FALSE`, all columns stay strings.

## Value

A tibble with one row for each input file. The first column is `file`,
and then there is one column for each parameter.

A file that the function could not read gets a row of `NA` values. The
function reads the other files, and then gives one warning that names
the files it could not read. A file that reaches the time limit counts
as not read; see
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).

## Details

Give several files in `file` to get one row for each file. The first
column, `file`, names the input file. The `probe_*()` functions read
similar information with FFprobe.

## See also

[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
to read a single value.
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
to apply a whole template.
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
to read information with FFprobe.
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md)
and the other `get_*()` functions for common single values.

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
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
