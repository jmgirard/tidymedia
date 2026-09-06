# Report which dependency programs tidymedia can find

Looks up all four programs tidymedia knows about and returns one row for
each: where it resolved to, and what version it reported. Nothing is
installed, written, or changed by the call.

## Usage

``` r
program_status()
```

## Value

A tibble with one row per program and three columns: `program`, the
program's name; `location`, the resolved path or `NA`; and `version`,
the version the binary reported or `NA`.

## Details

A program that cannot be found gets `NA` in both columns rather than a
warning, so the answer for four programs arrives as one table instead of
a pile of messages. The lookup is
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)'s:
the `PATH` first, then a location remembered by
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
and finally a location a version of tidymedia before 0.2.0 remembered
under `rappdirs::user_config_dir("tidymedia", "R")`.

The version is whatever the binary reports for its own version flag, so
it is the FFmpeg build number for `ffmpeg`, `ffprobe` and `ffplay`, and
the MediaInfo library version for `mediainfo`. A program that resolves
but cannot be asked – because the call fails, or because
`options(tidymedia.timeout = )` ended it – has a location and an `NA`
version.

## See also

[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
for one program at a time,
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to point tidymedia at a binary in a non-standard location, and
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
to forget one it remembered.

Other program management functions:
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md),
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)

## Examples

``` r
# One row per program; NA where the program was not found
program_status()
#> # A tibble: 4 × 3
#>   program   location           version       
#>   <chr>     <chr>              <chr>         
#> 1 ffmpeg    /usr/bin/ffmpeg    6.1.1-3ubuntu5
#> 2 ffprobe   /usr/bin/ffprobe   6.1.1-3ubuntu5
#> 3 ffplay    /usr/bin/ffplay    6.1.1-3ubuntu5
#> 4 mediainfo /usr/bin/mediainfo 24.01         
```
