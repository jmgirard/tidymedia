# Describe media files by applying a MediaInfo template

Create a tibble describing one or more media files by applying a
MediaInfo template, which can pull multiple parameters from multiple
sections. Two templates ship with the package (`"brief"` and
`"extended"`); a custom template file can also be supplied. `file` may
be a vector of several files; results are stacked with a leading `file`
column.

## Usage

``` r
mediainfo_summary(
  file,
  template = c("brief", "extended", "custom"),
  templatefile = NULL,
  typed = TRUE
)
```

## Arguments

- file:

  A character vector of one or more media-file paths.

- template:

  A string naming the template to apply: a built-in (`"brief"` or
  `"extended"`) or `"custom"` to apply the file given in `templatefile`.

- templatefile:

  Either the path to a MediaInfo template (.txt) file formatted to
  output comma-separated values (required when `template` is `"custom"`)
  or `NULL` (default).

- typed:

  A logical. When `TRUE` (default) numeric columns are typed and empty
  values become `NA`; when `FALSE` columns stay strings.

## Value

A tibble with one row per input file. The columns (and their
names/order) are determined by the template; custom-template column
names are used verbatim.

## See also

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_framerate()`](https://jmgirard.github.io/tidymedia/reference/get_framerate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_samplingrate()`](https://jmgirard.github.io/tidymedia/reference/get_samplingrate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
mediainfo_summary(video, template = "brief")
#> # A tibble: 1 × 12
#>   file           complete_name format file_size duration width height frame_rate
#>   <chr>          <chr>         <chr>      <int>    <int> <int>  <int>      <dbl>
#> 1 /home/runner/… /home/runner… MPEG-4     17725     1000   320    240         15
#> # ℹ 4 more variables: video_bit_rate <int>, channels <int>,
#> #   sampling_rate <int>, audio_bit_rate <int>
```
