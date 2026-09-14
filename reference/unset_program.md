# Forget the location of a dependency program

`unset_program()` forgets the location that
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
saved for a program. After that,
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md)
and the other `find_*()` functions look for the program on the `PATH`
only.

## Usage

``` r
unset_program(program)
```

## Arguments

- program:

  A string naming the program to forget. One of `"ffmpeg"`, `"ffprobe"`,
  `"ffplay"` or `"mediainfo"`. There is no default, because the call
  deletes a file. A call that names no program gives an error.

## Value

Invisibly, `TRUE` when the call removed a saved location, and `FALSE`
when there was none to remove.

## Details

The call deletes the file that holds the location. It does not ask you
to confirm first. It does not remove the program, and it does not change
the `PATH`. A program on the `PATH` is still found afterwards.

If no location is saved for the program, the call gives a warning and
returns `FALSE`. It does not give an error, because the program is
already forgotten.

The call also clears a location saved by a version before 0.2.0. See the
section "Locations saved by earlier versions" in
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md).

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to save a location, and
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
to see where the package finds each program.

Other program management functions:
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md),
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Forget a location set_program() remembered, so that find_mediainfo() goes
# back to answering from the PATH
unset_program("mediainfo")
} # }
```
