# Look up information about media files using FFprobe

Probe one or more media files and return their container- and
stream-level metadata as tibbles. `infile` may be a vector of several
files: the results are stacked and keyed by a leading `file` column, so
the output is ready for `dplyr` joins and filters over a whole batch.

## Usage

``` r
probe_all(infile, typed = TRUE)
```

## Arguments

- infile:

  A character vector of one or more media-file locations (file paths or
  web links) to probe.

- typed:

  A logical. When `TRUE` (default) numeric columns are converted to
  integers/doubles and FFprobe's `"N/A"` becomes `NA`; fractions,
  ratios, hex identifiers, and text stay as strings. When `FALSE` every
  value is returned as an unconverted string.

## Value

A list of two tibbles: `container` (one row per input file) and
`streams` (one row per stream, or a single `NA` row for a file with no
readable streams). Both lead with a `file` column identifying the input.
Files that cannot be probed yield an all-`NA` row and a warning rather
than aborting the call.

## See also

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_framerate()`](https://jmgirard.github.io/tidymedia/reference/get_framerate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_samplingrate()`](https://jmgirard.github.io/tidymedia/reference/get_samplingrate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md),
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
