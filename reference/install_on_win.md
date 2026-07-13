# Install FFmpeg on Windows

Downloads an FFmpeg zip installer, extracts it, and updates the
package's user config files to point to the component executable files.

## Usage

``` r
install_on_win(download_url = NULL, install_dir = NULL)
```

## Arguments

- download_url:

  A string indicating the location of the FFmpeg installation zip file.
  If `NULL`, will default to the latest static essentials release from
  gyan.dev.

- install_dir:

  A string indicating a directory to install FFmpeg to. If `NULL`, will
  default to installing to the user data directory.

## Value

A logical indicating whether the installation was successful.

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to register an existing binary, and
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
to check what is currently configured.

Other program management functions:
[`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Download and install a static FFmpeg build (Windows)
install_on_win()
} # }
```
