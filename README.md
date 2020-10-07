
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
info_query(
  file = "D:/example.mp4", 
  section = "Video", 
  parameters = c("Width", "Height", "DisplayAspectRatio")
)
#> # A tibble: 1 x 4
#>   File           Width Height DisplayAspectRatio
#>   <chr>          <int>  <int>              <dbl>
#> 1 D:/example.mp4   480    360               1.33
```

``` r
info_summary(file = "D:/example.mp4", style = "brief")
#> # A tibble: 1 x 11
#>   File  Format FileSize Duration Width Height FrameRate VideoBitRate Channels
#>   <chr> <chr>     <int>    <int> <int>  <int>     <dbl>        <int>    <int>
#> 1 D:/e~ MPEG-4  7570203   180084   480    360        30       199653        2
#> # ... with 2 more variables: SamplingRate <int>, AudioBitRate <int>
```

``` r
info_summary(file = "D:/example.mp4", style = "full")
#> # A tibble: 1 x 42
#>   File  General_Format General_FileSiz~ General_FileSize General_Duratio~
#>   <chr> <chr>          <chr>                       <int> <chr>           
#> 1 D:/e~ MPEG-4         7.22 MiB                  7570203 3 min 0 s       
#> # ... with 37 more variables: General_Duration <int>, Video_Format <chr>,
#> #   Video_FormatVersion <lgl>, Video_FormatProfile <chr>, Video_CodecID <chr>,
#> #   Video_DurationString <chr>, Video_Duration <int>, Video_BitRateMode <lgl>,
#> #   Video_BitRateString <chr>, Video_BitRate <int>, Video_Width <int>,
#> #   Video_Height <int>, Video_DisplayAspectRatioString <chr>,
#> #   Video_DisplayAspectRatio <dbl>, Video_FrameRateMode <chr>,
#> #   Video_FrameRateString <chr>, Video_FrameRate <dbl>, Video_FrameCount <int>,
#> #   Video_Standard <lgl>, Video_ScanType <chr>, Video_StreamSizeString <chr>,
#> #   Video_StreamSize <int>, Audio_Format <chr>, Audio_FormatVersion <lgl>,
#> #   Audio_FormatProfile <lgl>, Audio_CodecID <chr>, Audio_DurationString <chr>,
#> #   Audio_Duration <int>, Audio_BitRateMode <chr>, Audio_BitRateString <chr>,
#> #   Audio_BitRate <int>, Audio_Channels <int>, Audio_ChannelPositions <chr>,
#> #   Audio_SamplingRateString <chr>, Audio_SamplingRate <int>,
#> #   Audio_StreamSizeString <chr>, Audio_SteamSize <int>
```

## Code of Conduct

Please note that the **tidymedia** project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
