
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

1.  Download the appropriate CLI .zip file from this
    [webpage](https://mediaarea.net/en/MediaInfo/Download/Windows)
2.  Extract (or copy) the contents of this .zip file to a folder on your
    computer such as:<br /> `C:/Program Files/MediaInfo`
3.  Run the following code in R (changing the path to match Step
    2):<br /> `tidymedia::set_mediainfo("C:/Program
    Files/MediaInfo/mediainfo.exe")`

**Mac**

Coming soon…

## Example

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
get_duration(file = "D:/example.mp4", unit = "sec")
#> [1] 180.084
```

``` r
get_height(file = "D:/example.mp4")
#> [1] 360
```

## Code of Conduct

Please note that the **tidymedia** project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
