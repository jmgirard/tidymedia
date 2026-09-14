# Install FFmpeg on Windows

`install_on_win()` downloads a Windows build of FFmpeg and unpacks it.
Then it saves the locations of `ffmpeg`, `ffprobe` and `ffplay`, as
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
does. After that, the package can find these programs.

By default, the call downloads the latest "essentials" build from
gyan.dev. It unpacks the build into the `ffmpeg` folder under
`tools::R_user_dir("tidymedia", "data")`.

By default, the call asks you to confirm before it does anything. The
question names each file it will download and the folder it will unpack
into. It also names the saved program locations that the install can
replace. If you say no, the call returns `FALSE` and changes nothing.

This function works on Windows only. On any other system, it gives an
error before it asks, writes or downloads anything. The error names the
system it found. On macOS, you can install FFmpeg with
`brew install ffmpeg`. On Linux, you can use
`sudo apt-get install ffmpeg`. On any system,
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
tells the package where an installed FFmpeg is.

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

  A string with the address of the FFmpeg archive. If `NULL`, the call
  uses the latest "essentials" build from gyan.dev, a `.7z` archive.

- install_dir:

  A string with the folder to install FFmpeg into. If `NULL`, the call
  uses the `ffmpeg` folder under
  `tools::R_user_dir("tidymedia", "data")`. CRAN allows packages to keep
  user data in that place.

- confirm:

  `TRUE` or `FALSE`. Whether to ask before the call downloads or
  installs anything. Defaults to `TRUE`. In a session where no one can
  answer, `TRUE` gives an error that names the same items as the
  question. Pass `confirm = FALSE` to install without the question.

- archive_checksum:

  A string with the expected SHA-256 checksum of the archive, as 64
  hexadecimal characters in upper or lower case. Defaults to `NULL`. If
  you give a checksum, the call uses it for any source and downloads no
  checksum. If it is `NULL` and `download_url` is not the default
  source, the call checks nothing and says so.

## Value

`TRUE` when the install finished. `FALSE` when you said no to the
question, or when the call could not create the install folder. Other
failures give an error. The section "Errors" lists the error classes the
call gives. A wrong argument gives an error before any of these.

## Details

Before the call unpacks the archive, it checks the archive against a
SHA-256 checksum. A checksum is a fingerprint of the file's contents.
For the default source, the call downloads the checksum that gyan.dev
publishes next to each build. That file has the archive's address with
`.sha256` added. For any other source, give the checksum in
`archive_checksum`.

The published checksum comes from the same site as the archive, over the
same connection. So the check finds a damaged or incomplete download. It
does not find a source that someone has tampered with.

After the unpack, the call checks each program before it saves any
location. The path must be one that R finds as a program. It must be a
file, not a folder, and the file must not be empty. The call does not
run the program. So a build for the wrong type of processor can pass
this check.

The package needs `ffmpeg` and `ffprobe`. If either one fails the check,
the call saves no location and gives an error. The error names each
failed program and its full path. If `ffplay` is missing or fails the
check, the install finishes. A message says that the call did not save
`ffplay`.

## What a failed install leaves behind

When the call gives an error, it tries to leave the install folder as it
found it. It removes the files that a failed unpack wrote. It removes a
folder that the call created. It does not touch the files that were
already in the folder, with one exception.

The exception is a file of yours that the failed unpack wrote over. The
call removes that file too, because the file no longer holds what you
put there.

On Windows, the removal can fail. After a failed unpack, the unpack
library can still hold a file open. Windows does not delete a file that
is open.

The error names by full path the entries of the first case below that
applies:

- each unpacked file that the call could not remove

- each folder that the call created and could not remove

- each file of yours that the call removed

Two errors come after a successful unpack:
`tidymedia_program_not_extracted` and `tidymedia_program_unusable`.
These errors leave the unpacked files in the folder, and they say so.

The call learns which files the unpack made from the archive's own list
and from the folder. A program that the list names but that is not in
the folder counts as not unpacked. For example, antivirus software can
remove a program right after the unpack. The error then says that the
unpack reported writing that file.

If none of the unpacked files are in the folder,
`tidymedia_program_not_extracted` follows the usual rule. The call
removes a folder that it created, and the error says so.

## Errors

The call gives an error of its own class in these cases:

- `tidymedia_wrong_platform`: the session is not running on Windows.

- `tidymedia_confirmation_unavailable`: the call must ask you to
  confirm, but no one can answer in this session.

- `tidymedia_download_unavailable`: the archive did not download, or
  nothing readable arrived.

- `tidymedia_checksum_unavailable`: the call could not download or read
  the published checksum.

- `tidymedia_checksum_mismatch`: the downloaded archive does not match
  its checksum.

- `tidymedia_archive_unreadable`: the call could not unpack the archive.

- `tidymedia_program_not_extracted`: `ffmpeg` or `ffprobe` is not at the
  path where the install would put it.

- `tidymedia_program_unusable`: the archive made `ffmpeg` or `ffprobe`,
  but the file cannot be used.

## See also

[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
to save the location of a program you already have, and
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md)
to check where the package finds a program.

Other program management functions:
[`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_ffmpeg.md),
[`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md),
[`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
[`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Download and install a static FFmpeg build (Windows)
install_on_win()
} # }
```
