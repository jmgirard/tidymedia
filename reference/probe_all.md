# Look up information about media files using FFprobe

`probe_all()` uses the FFprobe program to read information about media
files. It returns two tibbles. One describes each file as a whole, and
one describes each stream in the files.

## Usage

``` r
probe_all(infile, typed = TRUE, parallel = FALSE)
```

## Arguments

- infile:

  A character vector of one or more media files to probe, as file paths
  or web links.

- typed:

  A logical. If `TRUE` (the default), numeric columns become integers or
  doubles, and FFprobe's `"N/A"` becomes `NA`. Fractions, ratios, hex
  identifiers and text stay as strings. If `FALSE`, every value stays a
  string.

- parallel:

  A logical. If `TRUE`, the function probes the files in parallel with
  furrr. If `FALSE` (the default), it probes them one at a time. A
  parallel run uses the active
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).
  It warns when that plan is sequential, because the run is then no
  faster. The output is the same either way, with the same rows in the
  same order. `parallel = TRUE` needs the furrr package, and only then
  does the function check for it.

## Value

A list of two tibbles. `container` has one row for each input file.
`streams` has one row for each stream. Both tibbles start with a `file`
column that names the input file. A file with no readable streams gets
one row in `streams`, with `NA` in every other column.

The function does not stop at a file that it could not probe. That file
gets a row of `NA` values in both tibbles, and the function gives a
warning. A file that reaches the time limit counts as not probed; see
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).

## Details

Give several files in `infile` to read them all in one call. The
function stacks the rows, and the first column, `file`, names the input
file. So you can join and filter the results for a whole batch with
`dplyr`.

The MediaInfo functions, `mediainfo_*()`, return tibbles or values. The
`get_*()` functions return one value for each file.

The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as container and stream.

## See also

[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
and
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
to read information with MediaInfo.
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md)
and the other `get_*()` functions for single values.

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
info <- probe_all(video)
info$container
#> # A tibble: 1 × 15
#>   file   filename nb_streams nb_programs format_name format_long_name start_time
#>   <chr>  <chr>         <int>       <int> <chr>       <chr>                 <dbl>
#> 1 /home… /home/r…          2           0 mov,mp4,m4… QuickTime / MOV           0
#> # ℹ 8 more variables: duration <dbl>, size <int>, bit_rate <int>,
#> #   probe_score <int>, `TAG:major_brand` <chr>, `TAG:minor_version` <int>,
#> #   `TAG:compatible_brands` <chr>, `TAG:encoder` <chr>
info$streams
#> # A tibble: 2 × 71
#>   file      index codec_name codec_long_name profile codec_type codec_tag_string
#>   <chr>     <int> <chr>      <chr>           <chr>   <chr>      <chr>           
#> 1 /home/ru…     0 h264       H.264 / AVC / … High    video      avc1            
#> 2 /home/ru…     1 aac        AAC (Advanced … LC      audio      mp4a            
#> # ℹ 64 more variables: codec_tag <chr>, width <int>, height <int>,
#> #   coded_width <int>, coded_height <int>, closed_captions <int>,
#> #   film_grain <int>, has_b_frames <int>, sample_aspect_ratio <chr>,
#> #   display_aspect_ratio <chr>, pix_fmt <chr>, level <int>, color_range <chr>,
#> #   color_space <chr>, color_transfer <chr>, color_primaries <chr>,
#> #   chroma_location <chr>, field_order <chr>, refs <int>, is_avc <chr>,
#> #   nal_length_size <int>, id <chr>, r_frame_rate <chr>, …
```
