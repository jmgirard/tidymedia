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

## Which reader?

The reader families differ by **backend** (FFprobe vs. MediaInfo) and by
**return shape** (a tibble vs. a single value). Pick by what you need
back:

| Reader family | Backend | Returns | Reach for it when |
|----|----|----|----|
| [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md), [`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md), [`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md), [`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md), [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md) | FFprobe | tibbles (container + per-stream rows) | you want container/stream metadata as a data frame |
| [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md), [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md), [`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md) | MediaInfo | a tibble (one row per file) | you want MediaInfo’s broader field set as a data frame |
| [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md) | MediaInfo | a value (one per file) | you want a single MediaInfo parameter across files |
| [`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md), [`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md), [`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md), [`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md), [`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md) | MediaInfo | a scalar (numeric, one per file) | you want one common field without naming MediaInfo sections |

The `probe_*()` family needs FFprobe installed; the `mediainfo_*()` and
`get_*()` families need MediaInfo. Because
[`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
and
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md)
can report the same underlying fact (here, the frame width), the choice
usually comes down to the shape you want back and which tool you have
installed.

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
`typed = FALSE` to keep everything as strings. Frame rates that FFprobe
reports as fractions (e.g. `"30000/1001"`) are evaluated to doubles
automatically when the columns are typed.

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

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  — an end-to-end research preprocessing pipeline.
- [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  — the task verbs and the builder beneath them.
- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  — running a verb over many files at once.
