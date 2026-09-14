# Report which dependency programs tidymedia can find

`program_status()` looks for the four programs that the package uses:
`ffmpeg`, `ffprobe`, `ffplay` and `mediainfo`. It returns a table with
one row for each program. The row shows where the program is and which
version it reports. The call does not install, write or change anything.

## Usage

``` r
program_status()
```

## Value

A tibble with one row for each program and three columns:

- `program`, the name of the program.

- `location`, the path to the program, or `NA`.

- `version`, the version that the program reported, or `NA`.

## Details

The call looks in the same places as
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md).
First it looks on the `PATH`, then at a location saved by
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md).
For locations saved by versions before 0.2.0, see the section "Locations
saved by earlier versions" in
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md).

A program that is not installed and has no saved location gets `NA` in
both columns. This case gives no warning, so four missing programs give
one table and not four warnings.

A saved location that cannot be used still gives a warning. Without it,
the `NA` would look like a program you never had. Each warning names the
saved location or the file that holds it, so you can fix it. There are
two cases:

- `tidymedia_location_gone`: no program is at the saved location now.

- `tidymedia_location_unreadable`: the file that stores the location
  does not hold exactly one location.

In both cases, the row has `NA` in both columns.
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
forgets the location, and
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
replaces it.

The version is what the program reports about itself. For `ffmpeg`,
`ffprobe` and `ffplay`, it is the FFmpeg build number. For `mediainfo`,
it is the MediaInfo library version.

Sometimes the call finds a program but cannot get its version. Then the
row has a location and an `NA` version. This happens when the program
call fails. It also happens when the time limit in
`options(tidymedia.timeout = )` stops it.

## See also

[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md)
and the other `find_*()` functions to look up one program.
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to save the location of a program that is not on the `PATH`.
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
to forget a saved location.

Other program management functions:
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md),
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
