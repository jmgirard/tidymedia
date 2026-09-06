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

## Details

The program is looked up on the `PATH` first. When it is not there, the
location remembered by
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
is read from `tools::R_user_dir("tidymedia", "config")`; a location
remembered by a version of tidymedia before 0.2.0 was written to
`rappdirs::user_config_dir("tidymedia", "R")`, and that file is read
only when no file exists in the current directory.

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to point tidymedia at a binary in a non-standard location, and
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
to download FFmpeg on Windows.

Other program management functions:
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)

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
