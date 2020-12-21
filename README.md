
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

#### 1\. MediaInfo

**tidymedia** uses [MediaInfo](https://mediaarea.net/en/MediaInfo) to
query information about media files. If you would like to use these
functions, you will need to install the command line interface (CLI)
version of this program. Links and instructions for doing so are
available [here](https://mediaarea.net/en/MediaInfo/Download). Below are
instructions for several popular platforms.

**Debian/Ubuntu**

1.  Enter this code into your terminal:<br />`sudo apt-get install
    mediainfo`

**Windows**

1.  Download the appropriate CLI .zip file
    from:<br /><https://mediaarea.net/en/MediaInfo/Download/Windows>
2.  Extract (or copy) the contents of this .zip file to a folder on your
    computer such as:<br />`C:/Program Files/MediaInfo`
3.  Run the following code in R (changing the path to match Step
    2):<br /> `tidymedia::set_mediainfo("C:/Program
    Files/MediaInfo/mediainfo.exe")`

**Mac**

1.  Download the appropriate CLI .dmg file
    from:<br /><https://mediaarea.net/en/MediaInfo/Download/Mac_OS>
2.  Open the .dmg file and drag the program icon to the Applications
    folder

#### 2\. FFmpeg

**tidymedia** uses [FFmpeg](https://ffmpeg.org/) to encode media files.
If you would like to use these functions, you will need to install the
command line interface (CLI) version of this program. Links and
instructions for doing so are available
[here](https://ffmpeg.org/download.html). Below are instructions for
several popular platforms.

**Debian/Ubuntu**

1.  Enter this code into your terminal:<br />`sudo apt-get install
    ffmpeg`

**Windows**

1.  Download the latest git version (full or essentials build)
    from:<br /> <https://www.gyan.dev/ffmpeg/builds/>
2.  Extract the contents of the downloaded [.7z](https://www.7-zip.org/)
    file to a folder on your computer such as:<br />`C:/Program
    Files/ffmpeg`
3.  Run the following code in R (changing the path to match Step
    2):<br />`tidymedia::set_ffmpeg("C:/Program
    Files/ffmpeg/bin/ffmpeg.exe")`

**macOS Homebrew Install**

1.  Open the macOS Terminal or Linux shell prompt.
2.  Install [Homebrew](https://brew.sh/) by entering this code into your
    terminal:<br />`/bin/bash -c "$(curl -fsSL
    https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"`
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
#> [1] "MediaInfo Command line, " "MediaInfoLib - v20.08"
```

``` r
# Create a row tibble using a built-in template
mediainfo_template(file = "D:/example.mp4", template = "brief")
```

| Path           | Format | FileSize | Duration | Width | Height | FrameRate | VideoBitRate | Channels | SamplingRate | AudioBitRate |
| :------------- | :----- | -------: | -------: | ----: | -----: | --------: | -----------: | -------: | -----------: | -----------: |
| D:/example.mp4 | MPEG-4 |  7570203 |   180084 |   480 |    360 |        30 |       199653 |        2 |        44100 |       128007 |

``` r
# Create a row tibble from specific parameters
mediainfo_query(
  file = "D:/example.mp4", 
  section = "Video", 
  parameters = c("Width", "Height", "DisplayAspectRatio")
)
```

| File           | Width | Height | DisplayAspectRatio |
| :------------- | ----: | -----: | -----------------: |
| D:/example.mp4 |   480 |    360 |              1.333 |

``` r
# Lookup common parameters
get_duration(file = "D:/example.mp4", unit = "sec")
#> [1] 180.084
get_height(file = "D:/example.mp4")
#> [1] 360
```

### FFmpeg

``` r
# Interface with the mediainfo CLI
ffmpeg("-version")
#>  [1] "ffmpeg version 4.3.1-2020-10-01-full_build-www.gyan.dev Copyright (c) 2000-2020 the FFmpeg developers"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#>  [2] "built with gcc 10.2.0 (Rev3, Built by MSYS2 project)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#>  [3] "configuration: --enable-gpl --enable-version3 --enable-static --disable-w32threads --disable-autodetect --enable-fontconfig --enable-iconv --enable-gnutls --enable-libxml2 --enable-gmp --enable-lzma --enable-libsnappy --enable-zlib --enable-libsrt --enable-libssh --enable-libzmq --enable-avisynth --enable-libbluray --enable-libcaca --enable-sdl2 --enable-libdav1d --enable-libzvbi --enable-librav1e --enable-libwebp --enable-libx264 --enable-libx265 --enable-libxvid --enable-libaom --enable-libopenjpeg --enable-libvpx --enable-libass --enable-frei0r --enable-libfreetype --enable-libfribidi --enable-libvidstab --enable-libvmaf --enable-libzimg --enable-amf --enable-cuda-llvm --enable-cuvid --enable-ffnvcodec --enable-nvdec --enable-nvenc --enable-d3d11va --enable-dxva2 --enable-libmfx --enable-libcdio --enable-libgme --enable-libmodplug --enable-libopenmpt --enable-libopencore-amrwb --enable-libmp3lame --enable-libshine --enable-libtheora --enable-libtwolame --enable-libvo-amrwbenc --enable-libilbc --enable-libgsm --enable-libopencore-amrnb --enable-libopus --enable-libspeex --enable-libvorbis --enable-ladspa --enable-libbs2b --enable-libflite --enable-libmysofa --enable-librubberband --enable-libsoxr --enable-chromaprint"
#>  [4] "libavutil      56. 51.100 / 56. 51.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#>  [5] "libavcodec     58. 91.100 / 58. 91.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#>  [6] "libavformat    58. 45.100 / 58. 45.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#>  [7] "libavdevice    58. 10.100 / 58. 10.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#>  [8] "libavfilter     7. 85.100 /  7. 85.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#>  [9] "libswscale      5.  7.100 /  5.  7.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> [10] "libswresample   3.  7.100 /  3.  7.100"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> [11] "libpostproc    55.  7.100 / 55.  7.100"
```

``` r
# Create ffmpeg commands by pipeline
tidymedia(input = "D:/example.mp4", output = "D:/example2.mp4") %>% 
  trim_duration(start_at = 1, stop_at = 5) %>% 
  crop_frames(width = 640, height = 480) %>% 
  set_codec("video", "libx264") %>%
  drop_streams("audio") %>% 
  compile_command()
#> [1] "-ss 1 -to 5 -an -i \"D:/example.mp4\" -y -codec:v libx264 -filter:v \"crop=w=640:h=480:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" \"D:/example2.mp4\""
```

``` r
# Run ffmpeg commands by pipeline
tidymedia(input = "D:/example.mp4", "D:/example2.mp4") %>% 
  trim_duration(start_at = 1, stop_at = 5) %>% 
  crop_frames(width = 640, height = 480) %>% 
  set_codec("video", "libx264") %>%
  drop_streams("audio") %>% 
  run_ffmpeg()
```

## Code of Conduct

Please note that the **tidymedia** project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
