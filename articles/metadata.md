# Media metadata as tibbles

``` r

library(tidymedia)
```

tidymedia reads media metadata into tibbles. So the metadata of a whole
folder becomes a data frame that you can filter, join and summarize.

Two programs read the metadata:

- FFprobe, used by the `probe_*()` functions, reads facts about the
  [container](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
  and each
  [stream](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary).
- MediaInfo, used by the `mediainfo_*()` and `get_*()` functions, reads
  a larger set of fields, grouped in a different way.

You need the program installed to use its functions. The
[README](https://github.com/jmgirard/tidymedia) shows how to install
them. The examples use the sample clip that comes with the package:

``` r

video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

## Which reader?

The readers differ in the program they use and in what they return.
Choose by what you need back:

| Functions | Program | Returns | Use it when |
|----|----|----|----|
| [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md), [`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md), [`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md), [`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md), [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md) | FFprobe | tibbles, with rows for the file and for each stream | you want the file and stream facts as a data frame |
| [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md), [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md) | MediaInfo | a tibble with one row per file | you want MediaInfo’s larger set of fields as a data frame |
| [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md) | MediaInfo | one value per file | you want one MediaInfo field for several files |
| [`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md), [`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md), [`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md), [`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md), [`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md) | MediaInfo | one number per file | you want one common field without naming a MediaInfo section |

Some facts, such as the width of the picture, come from both
[`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
and
[`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md).
Then choose by the shape you want back and the program you have.

## Probing with FFprobe

[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
returns a list of two tibbles. `container` has one row for each file,
and `streams` has one row for each stream. Both start with a `file`
column, so the results for several files stack into one table.

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

The other `probe_*()` functions return one part of that result. You can
give them the result of
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
so FFprobe does not read the file again. Or you can give them a file
with `infile`:

``` r

# Use the probe result, so the file is not read again
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

By default, `typed = TRUE` gives number columns a number type. With
`typed = FALSE`, every column is a string. FFprobe reports a [frame
rate](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
as a fraction such as `"30000/1001"`. The fraction stays a string, even
with `typed = TRUE`.

## Querying with MediaInfo

MediaInfo groups its fields in sections, such as `General`, `Video` and
`Audio`.
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
reads several fields from one section into a tibble:

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
reads a whole set of fields at once. The package has two templates,
`"brief"` and `"extended"`:

``` r

mediainfo_template(video, template = "brief")
#> # A tibble: 1 × 12
#>   file           complete_name format file_size duration width height frame_rate
#>   <chr>          <chr>         <chr>      <int>    <int> <int>  <int>      <dbl>
#> 1 /home/runner/… /home/runner… MPEG-4     17725     1000   320    240         15
#> # ℹ 4 more variables: video_bit_rate <int>, channels <int>,
#> #   sampling_rate <int>, audio_bit_rate <int>
```

For one value, use the `get_*()` functions:

``` r

get_duration(video, unit = "sec")
#> [1] 1
get_width(video)
#> [1] 320
get_height(video)
#> [1] 240
```

## Batching over many files

Each reader takes a vector of files, so you do not need a loop to read a
whole folder. The `probe_*()`,
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
and
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
functions mark each row with its `file`. The `get_*()` functions return
one value per file, in the order given.

[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
lists the video files in a folder, in all the formats it knows. To list
only one format, add `extension = "mp4"`. If the folder has no such
files,
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
stops with an error:

``` r

files <- ffm_jobs("my/videos", type = "video")$input
probe_all(files)$container
```

A file that cannot be read gives a row of `NA` values and a warning. The
other files are still read.

For a large folder, add `parallel = TRUE`. The files are then read in
parallel with [furrr](https://furrr.futureverse.org/). Each `probe_*()`
function takes this argument. On the functions other than
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
it has an effect only when you pass `infile`.

``` r

probe_all(files, parallel = TRUE)$container
```

The files are read in parallel only if you set a
[future](https://future.futureverse.org/) plan. With no plan, they are
read one at a time, and R gives a warning that says so.
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
shows how to set a plan.

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  shows a full research example.
- [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  explains the task functions and the pipeline functions.
- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  shows how to run a task function over many files.
