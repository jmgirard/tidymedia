# Set the location of a dependency program

The location is remembered across sessions in a file named
`<program>_location.txt` under
`tools::R_user_dir("tidymedia", "config")`, which
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
reads whenever the program is not on the `PATH`. Once this file exists,
a location remembered by a version of tidymedia before 0.2.0 is no
longer read.

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

  A string indicating which program to set the location for.

- location:

  A string containing the location of the program.

- confirm:

  Whether to ask before writing the remembered location. `TRUE` (the
  default) asks and, in a non-interactive session, refuses. `FALSE`
  writes without asking.

## Value

Invisibly, `TRUE` where the location was written and `FALSE` where the
caller declined to write it.

## Details

Because the call writes a file that outlives the session, it asks for
confirmation first and writes nothing until it has it. The prompt names
the location as you typed it – which is what gets written – and the full
path of the file that would record it. Declining leaves the config
directory exactly as it was. In a session with no one to ask, the call
refuses rather than assume consent; pass `confirm = FALSE` to write
without being asked, which is what an unattended script wants.

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
# Point tidymedia at a binary in a non-standard location; asks first
set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe")

# In an unattended script, where there is no one to ask
set_mediainfo("C:/Program Files/MediaInfo/mediainfo.exe", confirm = FALSE)
} # }
```
