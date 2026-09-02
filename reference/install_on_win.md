# Install FFmpeg on Windows

Downloads an FFmpeg archive, extracts it, and updates the package's user
config files to point to the component executable files. Because the
call downloads a third-party build and overwrites remembered program
locations, it asks for confirmation first and does nothing at all until
it has it.

## Usage

``` r
install_on_win(download_url = NULL, install_dir = NULL, confirm = TRUE)
```

## Arguments

- download_url:

  A string indicating the location of the FFmpeg installation archive.
  If `NULL`, will default to the latest static essentials release from
  gyan.dev, a `.7z` archive.

- install_dir:

  A string indicating a directory to install FFmpeg to. If `NULL`, will
  default to the `ffmpeg` subdirectory of
  `tools::R_user_dir("tidymedia", "data")`, the user data directory CRAN
  policy sanctions.

- confirm:

  A logical indicating whether to ask for confirmation before
  downloading and installing anything. Defaults to `TRUE`. The prompt
  names the archive to be downloaded, the directory it will be unpacked
  into, and the remembered program locations it will overwrite. Where
  there is no one to ask, the call aborts rather than assume consent,
  naming those same items; pass `confirm = FALSE` to install without
  being asked.

## Value

A logical indicating whether the installation was successful. `FALSE` is
also what a declined confirmation returns, alongside the existing
failures to create the install directory or download the archive.

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
