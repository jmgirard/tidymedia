# tidymedia

**tidymedia** helps you work with video, audio and image files in R. It
runs [FFmpeg](https://ffmpeg.org/) and
[MediaInfo](https://mediaarea.net/en/MediaInfo) for you, so you can
prepare media for research in a way you can repeat. It trims, crops and
converts files, often many at once. It also reads media metadata into
tibbles.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r

# install.packages("devtools")
devtools::install_github("jmgirard/tidymedia")
```

### Dependencies

tidymedia uses two free command-line programs.
[FFmpeg](https://ffmpeg.org/) converts media files and comes with
FFprobe, which reads them.
[MediaInfo](https://mediaarea.net/en/MediaInfo) also reads media files.
Install the ones you need.

**Debian and Ubuntu.** In a terminal, run:  
`sudo apt-get install ffmpeg mediainfo`

**macOS.** Install [Homebrew](https://brew.sh/). Then, in a terminal,
run:  
`brew install ffmpeg media-info`

**Windows.** For FFmpeg, run
[`tidymedia::install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
in R. It asks you to confirm before it downloads anything. For
MediaInfo, download the CLI version from the [MediaInfo download
page](https://mediaarea.net/en/MediaInfo/Download/Windows) and unzip it
to a folder such as `C:/Program Files/MediaInfo`. Then tell tidymedia
where the program is:  
`tidymedia::set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")`

**Check the install.** In R, run:  
[`tidymedia::program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)

Each program that tidymedia found shows a location and a version. If a
location is `NA`, give tidymedia the path with
[`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
[`set_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
or
[`set_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/set_program.md).
On macOS, run `brew --prefix ffmpeg` in a terminal. FFmpeg and FFprobe
are in the `bin` folder of the path that it prints. The help pages
[`?set_program`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
and
[`?install_on_win`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
give the details.

## Examples

``` r

library(tidymedia)
```

The examples below use a short sample clip that comes with the package.
They copy it to the working folder, so the paths they print stay short:

``` r

invisible(file.copy(system.file("extdata", "sample.mp4", package = "tidymedia"), "."))
video <- "sample.mp4"
```

### Build reproducible FFmpeg commands

The pipeline functions, whose names start with `ffm_`, build an FFmpeg
command one step at a time. Nothing runs until you ask.
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
returns the command as a string, and
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
runs it. See
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
for a full tour.

``` r

ffm_files(video, "output.mp4") |>
  ffm_trim(start = 1, end = 5) |>
  ffm_crop(width = 160, height = 120) |>
  ffm_codec(video = "libx264") |>
  ffm_drop(streams = "audio") |>
  ffm_compile()
#> [1] "-y -i \"sample.mp4\" -vf \"trim=start=1:end=5,setpts=PTS-STARTPTS,crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 -an \"output.mp4\""
```

Common jobs have their own task functions. Add `run = FALSE` to see the
command without running it:

``` r

extract_audio(video, "audio.aac", run = FALSE)
#> [1] "-y -i \"sample.mp4\" -codec:a copy -vn -map \"0:a:0\" \"audio.aac\""
```

### Process a folder in batch

[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
turns a folder into a jobs table, with one row for each media file. Some
`*_batch()` functions, such as
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
take that table as it is. Others need a column added first, such as
`output`.

If the folder has no files of the type you ask for,
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
stops with an error. See
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
for more.

``` r

jobs <- ffm_jobs(".", type = "video")
# ffm_jobs() returns full paths; keep only the file name so this page stays
# short. The clip is in the working folder, so the commands still find it.
jobs$input <- basename(jobs$input)
crop_video_batch(jobs, width = 160, height = 120, run = FALSE)
#> # A tibble: 1 × 3
#>   input      output             command                                         
#>   <chr>      <chr>              <chr>                                           
#> 1 sample.mp4 sample_cropped.mp4 "-y -i \"sample.mp4\" -vf \"crop=w=160:h=120:x=…
```

### Read metadata as tibbles

[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
uses FFprobe to read facts about the
[container](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
and each
[stream](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary).
It returns them as tibbles. Each tibble starts with a `file` column, so
the results for many files stack into one table:

``` r

probe_all(video)$streams
#> # A tibble: 2 × 69
#>   file      index codec_name codec_long_name profile codec_type codec_tag_string
#>   <chr>     <int> <chr>      <chr>           <chr>   <chr>      <chr>           
#> 1 sample.m…     0 h264       H.264 / AVC / … High    video      avc1            
#> 2 sample.m…     1 aac        AAC (Advanced … LC      audio      mp4a            
#> # ℹ 62 more variables: codec_tag <chr>, mime_codec_string <chr>, width <int>,
#> #   height <int>, coded_width <int>, coded_height <int>, has_b_frames <int>,
#> #   sample_aspect_ratio <chr>, display_aspect_ratio <chr>, pix_fmt <chr>,
#> #   level <int>, color_range <chr>, color_space <chr>, color_transfer <chr>,
#> #   color_primaries <chr>, chroma_location <chr>, field_order <chr>,
#> #   is_avc <chr>, nal_length_size <int>, id <chr>, r_frame_rate <chr>,
#> #   avg_frame_rate <chr>, time_base <chr>, start_pts <int>, start_time <dbl>, …
```

MediaInfo works too, through
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
and the `get_*()` functions. See
[`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md).

``` r

get_duration(video, unit = "sec")
#> [1] 1
get_width(video)
#> [1] 320
```

### Query FFmpeg’s capabilities

[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
lists the
[codecs](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
that your FFmpeg build knows:

``` r

head(ffmpeg_codecs())
#> # A tibble: 6 × 8
#>   name       details           type  decoding encoding intraframe lossy lossless
#>   <chr>      <chr>             <fct> <lgl>    <lgl>    <lgl>      <lgl> <lgl>   
#> 1 012v       Uncompressed 4:2… Video TRUE     FALSE    TRUE       FALSE TRUE    
#> 2 4xm        4X Movie          Video TRUE     FALSE    FALSE      TRUE  FALSE   
#> 3 8bps       QuickTime 8BPS v… Video TRUE     FALSE    TRUE       FALSE TRUE    
#> 4 a64_multi  Multicolor chars… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#> 5 a64_multi5 Multicolor chars… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#> 6 aasc       Autodesk RLE      Video TRUE     FALSE    FALSE      FALSE TRUE
```

## Code of Conduct

Please note that the **tidymedia** project is released with a
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
