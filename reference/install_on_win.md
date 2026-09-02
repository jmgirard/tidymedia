# Install FFmpeg on Windows

Downloads an FFmpeg archive, extracts it, and updates the package's user
config files to point to the component executable files. Because the
call downloads a third-party build and overwrites remembered program
locations, it asks for confirmation first and does nothing at all until
it has it.

## Usage

``` r
install_on_win(
  download_url = NULL,
  install_dir = NULL,
  confirm = TRUE,
  archive_checksum = NULL
)
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
  into, and the remembered program locations it may overwrite. Where
  there is no one to ask, the call aborts rather than assume consent,
  naming those same items; pass `confirm = FALSE` to install without
  being asked.

- archive_checksum:

  A string giving the archive's expected SHA-256 digest as 64
  hexadecimal characters, in either case. Defaults to `NULL`. A digest
  supplied here is used on every source, and no digest is fetched. Where
  it is `NULL` and `download_url` is not the package's own default,
  nothing is verified and the call says so.

## Value

A logical indicating whether the installation was successful. `FALSE` is
returned by a declined confirmation and by a failure to create the
install directory. Five other outcomes abort with a condition of their
own rather than returning: a download that did not deliver
(`tidymedia_download_unavailable`), a published digest that could not be
fetched or read (`tidymedia_checksum_unavailable`), a digest that did
not match the downloaded archive (`tidymedia_checksum_mismatch`), an
archive that could not be unpacked (`tidymedia_archive_unreadable`), and
a required program the archive did not contain
(`tidymedia_program_not_extracted`).

## Details

The archive is checked against a SHA-256 digest before anything is
unpacked, and no program location is remembered unless the extraction
actually produced that program. For the package's own default source the
digest is fetched from `<download_url>.sha256`, which is what gyan.dev
publishes beside each build; for any other source, pass
`archive_checksum`. Because the digest travels from the same host over
the same connection as the archive, this catches a corrupted or
truncated download, not a compromised source.

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
