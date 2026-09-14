# Shortcut functions for probing specific information

These functions return one part of what
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
returns. `probe_container()` returns the `container` tibble, and
`probe_streams()` returns the `streams` tibble. `probe_video()` and
`probe_audio()` return only the video rows or the audio rows of
`streams`.

## Usage

``` r
probe_container(probe = NULL, infile = NULL, typed = TRUE, parallel = FALSE)

probe_streams(probe = NULL, infile = NULL, typed = TRUE, parallel = FALSE)

probe_video(probe = NULL, infile = NULL, typed = TRUE, parallel = FALSE)

probe_audio(probe = NULL, infile = NULL, typed = TRUE, parallel = FALSE)
```

## Arguments

- probe:

  A list made by
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md).
  Must be `NULL` if you give `infile`.

- infile:

  A character vector of one or more media files. Must be `NULL` if you
  give `probe`.

- typed:

  A logical that the function passes to
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  when you give `infile`. The default is `TRUE`. The function ignores it
  when you give `probe`.

- parallel:

  A logical that the function passes to
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  when you give `infile`. If `TRUE`, it probes the files in parallel
  with furrr. If `FALSE` (the default), it probes them one at a time.
  The function ignores it when you give `probe`, because nothing is left
  to probe.

## Value

A tibble with only the requested information. When you give `infile`, a
file that could not be probed gives a warning, as in
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md).

## Details

Give each function either the output of
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
in `probe`, or one or more files in `infile`. Give exactly one of the
two, or the function gives an error. With `infile`, the function probes
the files again. For large files, probe once with
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
and reuse the result.

These functions use FFprobe and return tibbles. The MediaInfo functions,
`mediainfo_*()`, and the `get_*()` functions are the other ways to read
information. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as container and stream.

## See also

[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
for the full probe.
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
to read information with MediaInfo.
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md)
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
