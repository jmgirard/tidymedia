# Find the location of a dependency program

Each of these functions returns the location of one program as a string:
`find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()` and
`find_mediainfo()`.

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

The function looks on the `PATH` first. If the program is not there, the
function reads the location that
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
saved. That location is in a file under
`tools::R_user_dir("tidymedia", "config")`. If neither place has the
program, the function gives a warning and returns `NULL`.

## Problems with a saved location

A saved location that no longer works gives a warning, and the function
returns `NULL`. The warning has one of three classes that you can catch:

- `tidymedia_location_gone`: the function read the location, but no
  program is there now. The condition holds the program name in
  `tm_program` and the location in `tm_location`, as the file holds it.

- `tidymedia_location_not_executable`: a file is at the saved location,
  and it cannot be run. Give it permission to execute, or use the
  `set_*()` function to point at another copy. The condition holds the
  program name in `tm_program` and the location in `tm_location`, as the
  file holds it. This case does not arise on Windows, which has no
  execute bit. There a location is read the way R's own
  [`Sys.which()`](https://rdrr.io/r/base/Sys.which.html) reads it,
  except that a folder never counts and a leading `~` is expanded.

- `tidymedia_location_unreadable`: the file does not hold exactly one
  location. It is empty, has more than one line, or has one empty line.
  Or the package cannot read the file, because it has no read
  permission. Or a folder is where that file belongs, and no file for
  the program is in either place. The condition holds the program name
  in `tm_program` and the path in `tm_file`. For folders, that is the
  first folder the function looked in.

A line that holds only spaces counts as a location. So it gives
`tidymedia_location_gone`, not `tidymedia_location_unreadable`.

To fix a location that is gone or cannot be run, or a file that does not
hold one location, forget the location with
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md),
or replace it with
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md).
A file that cannot be run can also be fixed by giving it permission to
execute. A file the package cannot read can be fixed by giving it read
permission, or removed with
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md).

[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
removes an empty folder where the file belongs, in either folder, and
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
removes one in the current folder. A folder counts as empty only when it
holds nothing, not even a hidden file or an empty folder. A folder the
package cannot read never counts as empty. Both functions leave a folder
that is not empty, and they give an error. The warning says which case
you have: it advises
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
when every folder it found is empty, and otherwise it names each folder
to delete by hand.

## Locations saved by earlier versions

Versions of the package before 0.2.0 saved locations in a different
folder, `rappdirs::user_config_dir("tidymedia", "R")`. The functions
still read a file in that folder. They read it only when the current
folder has no file for the program. A folder in the place of the current
file does not count as a file, so the old file is still read.
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
looks in the same places, in the same order.

A `tidymedia_location_unreadable` warning names the file that the
function read. That file can be the one in the old folder.

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
writes to the current folder only. After it writes a file for a program,
the old file for that program is not read.

[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
removes the file in both folders. So the old location does not come back
after the current file is gone.

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to save the location of a program that is not on the `PATH`, and
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
