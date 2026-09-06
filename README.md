
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidymedia

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jmgirard/tidymedia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmgirard/tidymedia/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmgirard/tidymedia/graph/badge.svg)](https://app.codecov.io/gh/jmgirard/tidymedia)
<!-- badges: end -->

The goal of **tidymedia** is to provide tools for easily working with
media (e.g., image, audio, and video) files within R and the tidyverse.
It wraps [FFmpeg](https://ffmpeg.org/) and
[MediaInfo](https://mediaarea.net/en/MediaInfo) for **reproducible media
preprocessing** — batch trimming, cropping, format standardization, and
metadata extraction as tibbles.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/tidymedia")
```

### Dependencies

#### 1. MediaInfo

**tidymedia** uses [MediaInfo](https://mediaarea.net/en/MediaInfo) to
query information about media files. If you would like to use these
functions, you will need to install the command line interface (CLI)
version of this program. Links and instructions for doing so are
available [here](https://mediaarea.net/en/MediaInfo/Download). Below are
instructions for several popular platforms.

**Debian/Ubuntu**

1.  Enter this code into your
    terminal:<br />`sudo apt-get install mediainfo`
2.  Check that R can find it:<br /> `tidymedia::program_status()`<br />
    The `mediainfo` row should show a location and a version. If its
    location is `NA`, tidymedia cannot see the program: give it the path
    yourself with `tidymedia::set_mediainfo("/path/to/mediainfo")`.

**Windows**

1.  Download the appropriate CLI .zip file
    from:<br /><https://mediaarea.net/en/MediaInfo/Download/Windows>
2.  Extract (or copy) the contents of this .zip file to a folder on your
    computer such as:<br />`C:/Program Files/MediaInfo`
3.  Run the following code in R (changing the path to match Step
    2):<br />
    `tidymedia::set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")`<br />
    This asks you to confirm the path before it remembers it. In a
    script with no one to ask, add `confirm = FALSE`.
4.  Check that R can find it:<br /> `tidymedia::program_status()`<br />
    The `mediainfo` row should show a location and a version. If its
    location is `NA`, the path in Step 3 is not where `mediainfo.exe`
    actually landed: look in the folder from Step 2 and run
    `tidymedia::set_mediainfo()` again with the path you find there.

**Mac**

1.  Download the appropriate CLI .dmg file
    from:<br /><https://mediaarea.net/en/MediaInfo/Download/Mac_OS>
2.  Open the .dmg file and drag the program icon to the Applications
    folder
3.  The Applications folder is not on the `PATH`, so tell tidymedia
    where the program landed (changing the path to match Step 2):<br />
    `tidymedia::set_mediainfo("/Applications/mediainfo")`<br /> This
    asks you to confirm the path before it remembers it, and it is
    remembered across R sessions.
4.  Check that R can find it:<br /> `tidymedia::program_status()`<br />
    The `mediainfo` row should show a location and a version. If its
    location is `NA`, the path in Step 3 is not where the program
    actually landed: find it in the Applications folder and run
    `tidymedia::set_mediainfo()` again with that path.

#### 2. FFmpeg

**tidymedia** uses [FFmpeg](https://ffmpeg.org/) to encode media files.
If you would like to use these functions, you will need to install the
command line interface (CLI) version of this program. Links and
instructions for doing so are available
[here](https://ffmpeg.org/download.html). Below are instructions for
several popular platforms.

**Debian/Ubuntu**

1.  Enter this code into your
    terminal:<br />`sudo apt-get install ffmpeg`
2.  Check that R can find it:<br /> `tidymedia::program_status()`<br />
    The `ffmpeg` and `ffprobe` rows should show locations and versions.
    If they are `NA`, tidymedia cannot see the program: give it the path
    yourself with `tidymedia::set_ffmpeg("/path/to/ffmpeg")`.

**Windows**

1.  Install the `tidymedia` package in R.
2.  Run `tidymedia::install_on_win()` in R and confirm the prompt, which
    names the digest and the archive it will download, the directory it
    will unpack into, and the program locations it may overwrite. The
    archive is checked against the SHA-256 digest gyan.dev publishes
    beside it before anything is unpacked, and no program location is
    remembered unless the extraction produced that program. To install
    from a different build, pass its address and its digest:
    `install_on_win(download_url = ..., archive_checksum = ...)`.
3.  Check that R can find it:<br /> `tidymedia::program_status()`<br />
    The `ffmpeg` and `ffprobe` rows should show locations and versions.
    If they are `NA`, tidymedia cannot see the program: read the message
    `install_on_win()` printed, or point tidymedia at a build you
    already have with `tidymedia::set_ffmpeg("C:/path/to/ffmpeg.exe")`.

**macOS Homebrew Install**

1.  Open the macOS Terminal or Linux shell prompt.
2.  Install [Homebrew](https://brew.sh/) by entering this code into your
    terminal:<br />`/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
3.  Enter this code into your terminal:<br />`brew install ffmpeg`
4.  Check that R can find it:<br /> `tidymedia::program_status()`<br />
    The `ffmpeg` and `ffprobe` rows should show locations and versions.
    If they are `NA`, tidymedia cannot see the program: run
    `brew --prefix ffmpeg` in the terminal and give tidymedia the
    `bin/ffmpeg` under what it prints, with
    `tidymedia::set_ffmpeg("/path/to/ffmpeg")`.

**macOS Manual Install**

1.  Download the latest snapshot version
    from:<br /><https://evermeet.cx/ffmpeg/get><br /> That address gives
    you `ffmpeg` on its own. The metadata functions also need `ffprobe`,
    which is a separate download:<br />
    <https://evermeet.cx/ffmpeg/getrelease/ffprobe/7z>
2.  Extract the contents of each downloaded
    [.7z](https://www.7-zip.org/) file
3.  Drag the extracted contents to the Applications folder
4.  The Applications folder is not on the `PATH`, so nothing looking a
    program up by name will find it there. Tell tidymedia where each
    program landed (changing the paths to match Step 3):<br />
    `tidymedia::set_ffmpeg("/Applications/ffmpeg")`<br />
    `tidymedia::set_ffprobe("/Applications/ffprobe")`<br /> Each asks
    you to confirm the path before it remembers it, and it is remembered
    across R sessions. Set both: the two programs are found separately,
    so setting `ffmpeg` does not make `ffprobe` findable.
5.  Check that R can find them:<br />
    `tidymedia::program_status()`<br /> The `ffmpeg` and `ffprobe` rows
    should show locations and versions. If either is `NA`, the path you
    gave in Step 4 is not where that program actually landed: find it in
    the Applications folder and run the call again with that path.

## Examples

``` r
library(tidymedia)
```

The examples below use a tiny sample clip that ships with the package:

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

### Build reproducible FFmpeg commands

The `ffm_*` builder assembles a command step by step. Nothing runs until
you ask it to: `ffm_compile()` returns the exact FFmpeg command as a
string, and `ffm_run()` executes it. See `vignette("tidymedia")` for the
full tour.

``` r
ffm_files(video, "output.mp4") |>
  ffm_trim(start = 1, end = 5) |>
  ffm_crop(width = 160, height = 120) |>
  ffm_codec(video = "libx264") |>
  ffm_drop(streams = "audio") |>
  ffm_compile()
#> [1] "-y -i \"/private/var/folders/px/frfvbz4n0sx90__c62fwwzz40000gn/T/Rtmpn5c3d7/temp_libpath147006c35cc6d/tidymedia/extdata/sample.mp4\" -vf \"trim=start=1:end=5,setpts=PTS-STARTPTS,crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 -an \"output.mp4\""
```

Common tasks have their own verbs (Layer 2). Pass `run = FALSE` to see
the command without executing it:

``` r
extract_audio(video, "audio.aac", run = FALSE)
#> [1] "-y -i \"/private/var/folders/px/frfvbz4n0sx90__c62fwwzz40000gn/T/Rtmpn5c3d7/temp_libpath147006c35cc6d/tidymedia/extdata/sample.mp4\" -codec:a copy -vn -map \"0:a:0\" \"audio.aac\""
```

### Read metadata as tibbles

`probe_all()` returns container- and stream-level metadata from FFprobe
as tibbles, keyed by a leading `file` column so a whole batch stacks
cleanly:

``` r
probe_all(video)$streams
#> # A tibble: 2 × 69
#>   file      index codec_name codec_long_name profile codec_type codec_tag_string
#>   <chr>     <int> <chr>      <chr>           <chr>   <chr>      <chr>           
#> 1 /private…     0 h264       H.264 / AVC / … High    video      avc1            
#> 2 /private…     1 aac        AAC (Advanced … LC      audio      mp4a            
#> # ℹ 62 more variables: codec_tag <chr>, mime_codec_string <chr>, width <int>,
#> #   height <int>, coded_width <int>, coded_height <int>, has_b_frames <int>,
#> #   sample_aspect_ratio <chr>, display_aspect_ratio <chr>, pix_fmt <chr>,
#> #   level <int>, color_range <chr>, color_space <chr>, color_transfer <chr>,
#> #   color_primaries <chr>, chroma_location <chr>, field_order <chr>,
#> #   is_avc <chr>, nal_length_size <int>, id <chr>, r_frame_rate <chr>,
#> #   avg_frame_rate <chr>, time_base <chr>, start_pts <int>, start_time <dbl>, …
```

MediaInfo is available too, via `mediainfo_query()`,
`mediainfo_template()`, and the `get_*()` shortcuts (see
`vignette("metadata")`):

``` r
get_duration(video, unit = "sec")
#> [1] 1
get_width(video)
#> [1] 320
```

### Query FFmpeg’s capabilities

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
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
