# Media metadata as tibbles

``` r

library(tidymedia)
```

tidymedia reads media metadata as **tibbles**, so a whole folder of
files becomes a data frame you can filter, join, and summarize with the
tidyverse. Two engines back these readers:

- **FFprobe** (`probe_*()`) returns container- and stream-level
  metadata.
- **MediaInfo** (`mediainfo_*()`, `get_*()`) returns a broader,
  differently organized set of fields.

Both require the corresponding command-line tool to be installed (see
the [README](https://github.com/jmgirard/tidymedia) for setup). We use
the sample clip that ships with the package:

``` r

video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

## Probing with FFprobe

[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
returns a list of two tibbles: `container` (one row per file) and
`streams` (one row per stream). Both lead with a `file` column, so
results stack cleanly when you pass several files.

``` r

info <- probe_all(video)
info$container
#> # A tibble: 1 × 15
#>   file   filename nb_streams nb_programs format_name format_long_name start_time
#>   <chr>  <chr>         <int>       <int> <chr>       <chr>                 <dbl>
#> 1 /home… /home/r…          2           0 mov,mp4,m4… QuickTime / MOV           0
#> # ℹ 8 more variables: duration <dbl>, size <int>, bit_rate <int>,
#> #   probe_score <int>, `TAG:major_brand` <chr>, `TAG:minor_version` <int>,
#> #   `TAG:compatible_brands` <chr>, `TAG:encoder` <chr>
```

``` r

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

The `probe_*()` shortcuts pull out just what you need. They accept
**either** a `probe` object (to avoid re-reading the file) or a file
location via `infile`:

``` r

# Reuse the probe object rather than reprobing
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
```

Numeric columns are typed by default (`typed = TRUE`); pass
`typed = FALSE` to keep everything as strings. Frame rates arrive as
fractions, which
[`convert_fractions()`](https://jmgirard.github.io/tidymedia/reference/convert_fractions.md)
evaluates:

``` r

convert_fractions(c("30000/1001", "25"))
#> [1] 29.97003 25.00000
```

## Querying with MediaInfo

MediaInfo organizes metadata into sections (`General`, `Video`, `Audio`,
…).
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
pulls several parameters from one section into a tibble:

``` r

mediainfo_query(
  video,
  section = "Video",
  parameters = c("Width", "Height", "FrameRate")
)
#> # A tibble: 1 × 4
#>   file                                                    Width Height FrameRate
#>   <chr>                                                   <int>  <int>     <dbl>
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/samp…   320    240        15
```

[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
applies a whole template at once; two ship with the package (`"brief"`
and `"extended"`):

``` r

mediainfo_template(video, template = "brief")
#> # A tibble: 1 × 12
#>   file           complete_name format file_size duration width height frame_rate
#>   <chr>          <chr>         <chr>      <int>    <int> <int>  <int>      <dbl>
#> 1 /home/runner/… /home/runner… MPEG-4     17725     1000   320    240         15
#> # ℹ 4 more variables: video_bit_rate <int>, channels <int>,
#> #   sampling_rate <int>, audio_bit_rate <int>
```

For single values, the `get_*()` helpers are the quickest path:

``` r

get_duration(video, unit = "sec")
#> [1] 1
get_width(video)
#> [1] 320
get_height(video)
#> [1] 240
```

## Batching over many files

Because every reader accepts a vector of files and keys its output by
`file`, describing a whole directory is a one-liner:

``` r

files <- list.files("my/videos", pattern = "\\.mp4$", full.names = TRUE)
probe_all(files)$container
```

Files that cannot be read yield an all-`NA` row plus a warning, rather
than aborting the whole call — so one bad file never sinks a batch.
