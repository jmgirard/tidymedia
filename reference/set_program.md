# Set the location of a dependency program

Set the location of a dependency program

## Usage

``` r
set_program(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"), location)

set_mediainfo(location)

set_ffmpeg(location)

set_ffprobe(location)

set_ffplay(location)
```

## Arguments

- program:

  A string indicating which program to set the location for.

- location:

  A string containing the location of the program.

## Value

A logical indicating whether the program location was set properly.

## See also

[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
to locate a configured binary, and
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
to download FFmpeg on Windows.

Other program management functions:
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md),
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Point tidymedia at a binary in a non-standard location
set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")
} # }
```
