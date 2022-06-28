
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidymedia

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of **tidymedia** is to provide tools for easily working with
media (e.g., image, audio, and video) files within R and the tidyverse.

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

**Windows**

1.  Download the appropriate CLI .zip file
    from:<br /><https://mediaarea.net/en/MediaInfo/Download/Windows>
2.  Extract (or copy) the contents of this .zip file to a folder on your
    computer such as:<br />`C:/Program Files/MediaInfo`
3.  Run the following code in R (changing the path to match Step
    2):<br />
    `tidymedia::set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")`

**Mac**

1.  Download the appropriate CLI .dmg file
    from:<br /><https://mediaarea.net/en/MediaInfo/Download/Mac_OS>
2.  Open the .dmg file and drag the program icon to the Applications
    folder

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

**Windows**

1.  Install the `tidymedia` package in R.
2.  Run `tidymedia::install_on_win()` in R.

**macOS Homebrew Install**

1.  Open the macOS Terminal or Linux shell prompt.
2.  Install [Homebrew](https://brew.sh/) by entering this code into your
    terminal:<br />`/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"`
3.  Enter this code into your terminal:<br />`brew install ffmpeg`

**macOS Manual Install**

1.  Download the latest snapshot version
    from:<br /><https://evermeet.cx/ffmpeg/get>
2.  Extract the contents of the downloaded [.7z](https://www.7-zip.org/)
    file
3.  Drag the extracted contents to the Applications folder

## Examples

### MediaInfo

``` r
# Interface with the mediainfo CLI
mediainfo("--Version")
#> [1] "MediaInfo Command line, " "MediaInfoLib - v20.09"
```

``` r
# Create a row tibble using a built-in template
mediainfo_template(file = "C:/example.m4v", template = "brief")
```

| Path           | Format | FileSize | Duration | Width | Height | FrameRate | VideoBitRate | Channels | SamplingRate | AudioBitRate |
|:---------------|:-------|---------:|---------:|------:|-------:|----------:|-------------:|---------:|-------------:|-------------:|
| C:/example.m4v | MPEG-4 |  5301159 |    60064 |   720 |    480 |     29.97 |       571972 |        2 |        32000 |       128117 |

``` r
# Create a row tibble from specific parameters
mediainfo_query(
  file = "C:/example.m4v", 
  section = "Video", 
  parameters = c("Width", "Height", "DisplayAspectRatio")
)
```

| File           | Width | Height | DisplayAspectRatio |
|:---------------|------:|-------:|-------------------:|
| C:/example.m4v |   720 |    480 |              1.333 |

``` r
# Lookup common parameters
get_duration(file = "C:/example.m4v", unit = "sec")
#> Warning in type.convert.default(output): 'as.is' should be specified by the
#> caller; using TRUE
#> [1] 60.064
get_height(file = "C:/example.m4v")
#> Warning in type.convert.default(output): 'as.is' should be specified by the
#> caller; using TRUE
#> [1] 480
```

### FFmpeg

``` r
# Interface with the mediainfo CLI
ffmpeg("-version")
#>  [1] "ffmpeg version 5.0.1-essentials_build-www.gyan.dev Copyright (c) 2000-2022 the FFmpeg developers"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#>  [2] "built with gcc 11.2.0 (Rev7, Built by MSYS2 project)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#>  [3] "configuration: --enable-gpl --enable-version3 --enable-static --disable-w32threads --disable-autodetect --enable-fontconfig --enable-iconv --enable-gnutls --enable-libxml2 --enable-gmp --enable-lzma --enable-zlib --enable-libsrt --enable-libssh --enable-libzmq --enable-avisynth --enable-sdl2 --enable-libwebp --enable-libx264 --enable-libx265 --enable-libxvid --enable-libaom --enable-libopenjpeg --enable-libvpx --enable-libass --enable-libfreetype --enable-libfribidi --enable-libvidstab --enable-libvmaf --enable-libzimg --enable-amf --enable-cuda-llvm --enable-cuvid --enable-ffnvcodec --enable-nvdec --enable-nvenc --enable-d3d11va --enable-dxva2 --enable-libmfx --enable-libgme --enable-libopenmpt --enable-libopencore-amrwb --enable-libmp3lame --enable-libtheora --enable-libvo-amrwbenc --enable-libgsm --enable-libopencore-amrnb --enable-libopus --enable-libspeex --enable-libvorbis --enable-librubberband"
#>  [4] "libavutil      57. 17.100 / 57. 17.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#>  [5] "libavcodec     59. 18.100 / 59. 18.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#>  [6] "libavformat    59. 16.100 / 59. 16.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#>  [7] "libavdevice    59.  4.100 / 59.  4.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#>  [8] "libavfilter     8. 24.100 /  8. 24.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#>  [9] "libswscale      6.  4.100 /  6.  4.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> [10] "libswresample   4.  3.100 /  4.  3.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> [11] "libpostproc    56.  3.100 / 56.  3.100"
```

``` r
# Query information about codecs from FFmpeg
get_codecs()
#> # A tibble: 490 × 8
#>    name       details          type  decoding encoding intraframe lossy lossless
#>    <chr>      <chr>            <fct> <lgl>    <lgl>    <lgl>      <lgl> <lgl>   
#>  1 012v       Uncompressed 4:… Video TRUE     FALSE    TRUE       FALSE TRUE    
#>  2 4xm        4X Movie         Video TRUE     FALSE    FALSE      TRUE  FALSE   
#>  3 8bps       QuickTime 8BPS … Video TRUE     FALSE    TRUE       FALSE TRUE    
#>  4 a64_multi  Multicolor char… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#>  5 a64_multi5 Multicolor char… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#>  6 aasc       Autodesk RLE     Video TRUE     FALSE    FALSE      FALSE TRUE    
#>  7 agm        Amuse Graphics … Video TRUE     FALSE    FALSE      TRUE  FALSE   
#>  8 aic        Apple Intermedi… Video TRUE     FALSE    TRUE       TRUE  FALSE   
#>  9 alias_pix  Alias/Wavefront… Video TRUE     TRUE     TRUE       FALSE TRUE    
#> 10 amv        AMV Video        Video TRUE     TRUE     TRUE       TRUE  FALSE   
#> # … with 480 more rows
```

``` r
# Query information about encoders from FFmpeg
get_encoders()
#> # A tibble: 196 × 8
#>    name    details type  frame_mt slice_mt experimental horiz_band direct_render
#>    <chr>   <chr>   <fct> <lgl>    <lgl>    <lgl>        <lgl>      <lgl>        
#>  1 a64mul… Multic… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#>  2 a64mul… Multic… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#>  3 alias_… Alias/… Video FALSE    FALSE    FALSE        FALSE      FALSE        
#>  4 amv     AMV Vi… Video FALSE    FALSE    FALSE        FALSE      FALSE        
#>  5 apng    APNG (… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#>  6 asv1    ASUS V1 Video FALSE    FALSE    FALSE        FALSE      FALSE        
#>  7 asv2    ASUS V2 Video FALSE    FALSE    FALSE        FALSE      FALSE        
#>  8 avrp    Avid 1… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#>  9 avui    Avid M… Video FALSE    FALSE    TRUE         FALSE      TRUE         
#> 10 ayuv    Uncomp… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#> # … with 186 more rows
```

``` r
# Create ffmpeg commands by pipeline
ffm(input = "C:/example.m4v", output = "C:/example2.m4v") |>
  ffm_trim(start = 1, end = 5) |> 
  ffm_crop(width = 640, height = 480) |>  
  ffm_codec(video = "libx264") |>
  ffm_drop(streams = "audio") |> 
  ffm_compile()
#> [1] "-an -i \"C:/example.m4v\" -y -codec:v libx264 -filter_complex:v \"trim=start=1:end=5,setpts=PTS-STARTPTS,crop=w=640:h=480:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" \"C:/example2.m4v\""
```

``` r
# Run ffmpeg commands by pipeline
ffm(input = "C:/example.m4v", output = "C:/example2.m4v") |> 
  ffm_trim(start = 1, end = 5) |> 
  ffm_crop(width = 640, height = 480) |> 
  ffm_codec(video = "libx264") |>
  ffm_drop(streams = "audio") |> 
  ffm_run()
```

## Code of Conduct

Please note that the **tidymedia** project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
