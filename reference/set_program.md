# Set the location of a dependency program

`set_program()` saves the location of a program, so the package can find
it in later sessions. `set_ffmpeg()`, `set_ffprobe()`, `set_ffplay()`
and `set_mediainfo()` do the same for one program each.

The location goes in a file named after the program, such as
`ffmpeg_location.txt`. The file is under
`tools::R_user_dir("tidymedia", "config")`.
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md)
and the other `find_*()` functions read it when the program is not on
the `PATH`.

## Usage

``` r
set_program(
  program = c("ffmpeg", "ffprobe", "ffplay", "mediainfo"),
  location,
  confirm = TRUE
)

set_mediainfo(location, confirm = TRUE)

set_ffmpeg(location, confirm = TRUE)

set_ffprobe(location, confirm = TRUE)

set_ffplay(location, confirm = TRUE)
```

## Arguments

- program:

  A string naming the program to set the location for.

- location:

  A string with the location of the program.

- confirm:

  Whether to ask before the call writes the location. `TRUE`, the
  default, asks. In a session where no one can answer, `TRUE` gives an
  error. `FALSE` writes without asking.

## Value

Invisibly, `TRUE` when the call wrote the location and `FALSE` when you
said no.

## Details

The file stays after the session ends, so the call asks you to confirm
first. It writes nothing until you agree. The question shows the
location the call writes, which is the location as you typed it. The one
change is a leading `~`, which is written as your home folder in full.
The question also shows the full path of the file. If you say no, the
call changes nothing.

In a session where no one can answer, the call gives an error. Pass
`confirm = FALSE` to write without the question, for example in a script
that runs on its own.

A folder can be where the location file goes. If the folder is empty,
the call removes it before it writes the file, and the question names
the folder. A folder counts as empty only when it holds nothing, not
even a hidden file or an empty folder, and a folder the package cannot
read never counts as empty. If the folder is not empty, the call gives a
`tidymedia_location_folder` error before it asks, and it changes
nothing. The error names the folder in its message and in `tm_files`.
Delete the folder by hand, then call again. If an empty folder is still
there after the call tries to remove it, the call gives a
`tidymedia_location_not_removed` error and writes nothing. The call
looks for such a folder only in the current folder. A folder in the
place of a location file saved before 0.2.0 stays, and
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
removes it when it is empty.

If the call cannot write the location file, it gives a
`tidymedia_location_not_written` error. This happens when the file is
there without write permission. It also happens when the file is not
there and its folder has no write permission, or when that folder is
missing and cannot be created. The error names the file in its message
and in `tm_file`. The call does not replace a file without write
permission, so that file keeps the location it held.

If the call created the folder for the location file, or folders above
it, and the write then fails, it tries to remove those folders, starting
with the deepest. It never removes a folder that holds anything, so a
folder it could not remove also keeps each folder above it. The
`tidymedia_location_not_written` error names in its message each of
those folders that stays. If the call removed an empty folder where the
file goes and the write then fails, that folder stays removed, and the
error message names it.

For locations saved by versions before 0.2.0, see the section "Locations
saved by earlier versions" in
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md).

## See also

[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md)
and the other `find_*()` functions to find a program, and
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
to download FFmpeg on Windows.

Other program management functions:
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md),
[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md),
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Point tidymedia at a binary in a non-standard location; asks first
set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")

# In an unattended script, where there is no one to ask
set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe", confirm = FALSE)
} # }
```
