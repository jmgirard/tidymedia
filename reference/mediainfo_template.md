# Describe media files by applying a MediaInfo template

`mediainfo_template()` uses the MediaInfo program to describe media
files, and returns a tibble. It applies a MediaInfo template, which can
read many parameters from many sections.

## Usage

``` r
mediainfo_template(
  file,
  template = c("brief", "extended", "custom"),
  templatefile = NULL,
  typed = TRUE
)
```

## Arguments

- file:

  A character vector of one or more media file paths.

- template:

  A string. Use `"brief"` or `"extended"` for a template that comes with
  the package. Use `"custom"` to apply the file in `templatefile`.

- templatefile:

  The path to your own MediaInfo template, a `.txt` file that makes
  MediaInfo print comma-separated values. Give it when `template` is
  `"custom"`, and only then. The default is `NULL`.

- typed:

  A logical. If `TRUE` (the default), numeric columns become numbers and
  empty values become `NA`. If `FALSE`, all columns stay strings.

## Value

A tibble with one row for each input file. The template sets the
columns, their names and their order. The function keeps the column
names of a custom template, but removes spaces at their start and end.

A file that the function could not read gets a row of `NA` values. The
function reads the other files, and then gives one warning that names
the files it could not read. A file that reaches the time limit counts
as not read; see
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).

## Details

The package comes with two templates, `"brief"` and `"extended"`. You
can also give your own template file. Give several files in `file` to
get one row for each file. The first column, `file`, names the input
file. The `probe_*()` functions read similar information with FFprobe.

## See also

[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
to read one section.
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
to read a single value.
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
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
mediainfo_template(video, template = "brief")
#> # A tibble: 1 × 12
#>   file           complete_name format file_size duration width height frame_rate
#>   <chr>          <chr>         <chr>      <int>    <int> <int>  <int>      <dbl>
#> 1 /home/runner/… /home/runner… MPEG-4     17725     1000   320    240         15
#> # ℹ 4 more variables: video_bit_rate <int>, channels <int>,
#> #   sampling_rate <int>, audio_bit_rate <int>
```
