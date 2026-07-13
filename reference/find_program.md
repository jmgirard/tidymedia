# Find the location of a dependency program

Returns the location of the requested program as a string.

## Usage

``` r
find_program(program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"))

find_mediainfo()

find_ffmpeg()

find_ffprobe()

find_ffplay()
```

## Arguments

- program:

  A string indicating which program to find

## Value

Either a string indicating whether the requested program was found or
`NULL` if the program could not be found.

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to point tidymedia at a binary in a non-standard location, and
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
to download FFmpeg on Windows.

Other program management functions:
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)

## Examples

``` r
# Returns the path to the binary, or NULL with a warning if it is not found
find_ffmpeg()
#>            ffmpeg 
#> "/usr/bin/ffmpeg" 
find_mediainfo()
#>            mediainfo 
#> "/usr/bin/mediainfo" 
```
