# Forget the location of a dependency program

Removes the location
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
remembered for a program, so that
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
goes back to answering from the `PATH`. Both places a location can live
are cleared: the file under `tools::R_user_dir("tidymedia", "config")`
and, where one is still there, the file a version of tidymedia before
0.2.0 wrote under `rappdirs::user_config_dir("tidymedia", "R")`.

## Usage

``` r
unset_program(program)
```

## Arguments

- program:

  A string naming which program to forget the location for: one of
  `"ffmpeg"`, `"ffprobe"`, `"ffplay"` or `"mediainfo"`. There is no
  default: the call deletes a file, and D079's rule for this package
  keeps a member of the set out of the default position, so a call that
  names no program refuses rather than picking one.

## Value

Invisibly, `TRUE` where a remembered location was removed and `FALSE`
where there was none to remove.

## Details

Forgetting a location does not remove the program itself, and it does
not change what is on the `PATH`. A program tidymedia found on the
`PATH` is still found afterwards. A location remembered by a version
before 0.2.0 is cleared as well, so it is not left behind for
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
to answer with once the current file is gone.

There is nothing to confirm: deleting the remembered location is the
whole of what the call does. Calling it for a program with nothing
remembered warns and returns `FALSE` rather than failing – the state you
asked for is already the state you have.

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to remember a location, and
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
to see what tidymedia currently finds.

Other program management functions:
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md),
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Forget a location set_program() remembered, so that find_program() goes
# back to answering from the PATH
unset_program("mediainfo")
} # }
```
