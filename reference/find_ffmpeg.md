# Find the location of a dependency program

Returns the location of one of the programs tidymedia drives as a
string: `find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()` and
`find_mediainfo()`, one per program.

## Usage

``` r
find_ffmpeg()

find_mediainfo()

find_ffprobe()

find_ffplay()
```

## Value

The location of the program as a string, or `NULL` when it could not be
found.

## Details

The program is looked up on the `PATH` first. When it is not there, the
location remembered by
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
is read from `tools::R_user_dir("tidymedia", "config")`; a location
remembered by a version of tidymedia before 0.2.0 was written to
`rappdirs::user_config_dir("tidymedia", "R")`, and that file is read
only when no file for the program exists under
`tools::R_user_dir("tidymedia", "config")`.

A remembered location that no longer works warns and returns `NULL`
rather than failing, under a condition class you can catch:

- `tidymedia_location_gone` – the location was read, but there is no
  binary there any more. The condition carries the program in
  `tm_program` and the location in `tm_location`.

- `tidymedia_location_unreadable` – the file holding the location does
  not hold one location to try: it is empty, holds more than one line,
  or holds one empty line. A line holding only spaces is read as a
  location, and raises `tidymedia_location_gone` instead. The condition
  carries the program in `tm_program` and the file in `tm_file`.

Either is repaired with
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md),
which forgets the location, or
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
which replaces it.

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
