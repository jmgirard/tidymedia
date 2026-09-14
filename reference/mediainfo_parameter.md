# Query a single parameter from a single MediaInfo section

`mediainfo_parameter()` uses the MediaInfo program to read one value,
such as the video width, from media files. MediaInfo groups its values
in sections, such as `"General"`, `"Video"` and `"Audio"`. You name the
section and the parameter to read.

## Usage

``` r
mediainfo_parameter(file, section, parameter, typed = TRUE)
```

## Arguments

- file:

  A character vector of one or more media file paths.

- section:

  A string. The name of the MediaInfo section to read `parameter` from.

- parameter:

  A string. The name of the MediaInfo parameter to read from `section`.

- typed:

  A logical. If `TRUE` (the default), the function converts the values
  to their natural type, for example to numbers. If `FALSE`, it returns
  strings.

## Value

A vector with one value for each element of `file`. A value is `NA` when
MediaInfo prints more than one line, for example for a `section` it does
not know. A parameter that `section` does not have gives an empty value.
That value is `NA` when `typed = TRUE` and `""` when `typed = FALSE`. A
value is also `NA` for a file that does not exist or that reaches the
time limit.

The function does not stop at those files. It reads the other files, and
then gives one warning that names the files that do not exist or reached
the limit. See
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
for the time limit.

## Details

Give several files in `file` to get one value for each file. The
function returns a vector, not a tibble. The `probe_*()` functions read
similar information with FFprobe and return tibbles.

## See also

[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
to read several parameters at once.
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
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
mediainfo_parameter(video, section = "Video", parameter = "Width")
#> [1] 320
```
