# Shortcut functions for probing specific information

Return just the `container` tibble via `probe_container()`, just the
`streams` tibble via `probe_streams()`, or just the video/audio stream
rows via `probe_video()` / `probe_audio()`. Each takes **either** the
output of
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
(via `probe`) **or** one or more file locations (via `infile`); passing
`infile` reprobes, so reuse a `probe` object when working with large
files.

## Usage

``` r
probe_container(probe = NULL, infile = NULL, typed = TRUE)

probe_streams(probe = NULL, infile = NULL, typed = TRUE)

probe_video(probe = NULL, infile = NULL, typed = TRUE)

probe_audio(probe = NULL, infile = NULL, typed = TRUE)
```

## Arguments

- probe:

  A list object created by
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md).
  Must be `NULL` if `infile` is supplied.

- infile:

  A character vector of one or more media-file locations. Must be `NULL`
  if `probe` is supplied.

- typed:

  A logical passed to
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  when `infile` is used (default `TRUE`); ignored when `probe` is
  supplied.

## Value

A tibble containing only the requested information.

## See also

Other metadata functions:
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
[`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md),
[`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
[`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md),
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# Probe directly from a file location ...
probe_container(infile = video)
#> # A tibble: 1 × 15
#>   file   filename nb_streams nb_programs format_name format_long_name start_time
#>   <chr>  <chr>         <int>       <int> <chr>       <chr>                 <dbl>
#> 1 /home… /home/r…          2           0 mov,mp4,m4… QuickTime / MOV           0
#> # ℹ 8 more variables: duration <dbl>, size <int>, bit_rate <int>,
#> #   probe_score <int>, `TAG:major_brand` <chr>, `TAG:minor_version` <int>,
#> #   `TAG:compatible_brands` <chr>, `TAG:encoder` <chr>
# ... or reuse a probe object to avoid reprobing large files
info <- probe_all(video)
probe_video(info)
#> # A tibble: 1 × 71
#>   file      index codec_name codec_long_name profile codec_type codec_tag_string
#>   <chr>     <int> <chr>      <chr>           <chr>   <chr>      <chr>           
#> 1 /home/ru…     0 h264       H.264 / AVC / … High    video      avc1            
#> # ℹ 64 more variables: codec_tag <chr>, width <int>, height <int>,
#> #   coded_width <int>, coded_height <int>, closed_captions <int>,
#> #   film_grain <int>, has_b_frames <int>, sample_aspect_ratio <chr>,
#> #   display_aspect_ratio <chr>, pix_fmt <chr>, level <int>, color_range <chr>,
#> #   color_space <chr>, color_transfer <chr>, color_primaries <chr>,
#> #   chroma_location <chr>, field_order <chr>, refs <int>, is_avc <chr>,
#> #   nal_length_size <int>, id <chr>, r_frame_rate <chr>, …
probe_audio(info)
#> # A tibble: 1 × 71
#>   file      index codec_name codec_long_name profile codec_type codec_tag_string
#>   <chr>     <int> <chr>      <chr>           <chr>   <chr>      <chr>           
#> 1 /home/ru…     1 aac        AAC (Advanced … LC      audio      mp4a            
#> # ℹ 64 more variables: codec_tag <chr>, width <int>, height <int>,
#> #   coded_width <int>, coded_height <int>, closed_captions <int>,
#> #   film_grain <int>, has_b_frames <int>, sample_aspect_ratio <chr>,
#> #   display_aspect_ratio <chr>, pix_fmt <chr>, level <int>, color_range <chr>,
#> #   color_space <chr>, color_transfer <chr>, color_primaries <chr>,
#> #   chroma_location <chr>, field_order <chr>, refs <int>, is_avc <chr>,
#> #   nal_length_size <int>, id <chr>, r_frame_rate <chr>, …
```
