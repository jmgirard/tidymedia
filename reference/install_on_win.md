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
install directory. Six other outcomes abort with a condition of their
own rather than returning: a download that did not deliver
(`tidymedia_download_unavailable`), a published digest that could not be
fetched or read (`tidymedia_checksum_unavailable`), a digest that did
not match the downloaded archive (`tidymedia_checksum_mismatch`), an
archive that could not be unpacked (`tidymedia_archive_unreadable`), a
required program that is not at the path it would be installed to
(`tidymedia_program_not_extracted`), and a required program the archive
produced in a form that cannot be used (`tidymedia_program_unusable`).
Every one of these aims to leave the install directory as the call found
it, except the last two, which leave the files the archive did unpack –
and `tidymedia_program_not_extracted` is back inside the rule where none
of the archive's files are there. Removal is best-effort: on Windows a
partly-written file cannot be deleted while the extraction library still
holds it, and the error names what it could not remove. See Details.

## Details

The archive is checked against a SHA-256 digest before anything is
unpacked, and no program location is remembered unless the extraction
actually produced that program. For the package's own default source the
digest is fetched from `<download_url>.sha256`, which is what gyan.dev
publishes beside each build; for any other source, pass
`archive_checksum`. Because the digest travels from the same host over
the same connection as the archive, this catches a corrupted or
truncated download, not a compromised source.

Every program the extraction produced is checked before any location is
remembered: the path has to resolve the way an executable does, and what
is there has to be a file rather than a directory, and not be empty. The
program itself is never run, so a build that unpacks and then cannot
execute – the wrong architecture, say – passes this check. Where a
required program fails it, nothing at all is registered and the error
names each failed program and its full path; where an optional one fails
it, the install completes and says which program it skipped.

A refusal leaves the install directory as the call found it. Files a
failed extraction wrote are removed, a directory the call created is
removed again, and anything already in the directory is left alone –
with one deliberate exception: a file of yours the failed extraction
wrote over is removed with the rest of the debris, because what it holds
after a failed extraction is nothing you put there. The error names that
file by full path, so a refusal never reports a directory as untouched
when it took something of yours out of it.

Removal is best-effort, and on Windows it does not always succeed. Where
an extraction fails part-way, the library that was writing the file is
still holding it open, and Windows will not delete a file something
holds. Those entries are named in the error by full path, so a refusal
on Windows can leave files behind – the error tells you which. A
directory this call created and could not remove again is named the same
way.

Two refusals sit outside that rule, both of them below a successful
extraction: `tidymedia_program_not_extracted`, where a required program
is not at the path it would be installed to, and
`tidymedia_program_unusable`, where a file is at that path and cannot be
used. Each says so, and the unpacked files stay where they are. What the
extraction produced is read from the archive's own file list and from
the install directory together, so a path the archive listed and did not
leave behind – an unpacked program an antivirus quarantined, say – is
refused as a program that is not there rather than as one that cannot be
used, and the error says the extraction reported writing it. It is the
unpacked files that put these refusals outside the rule, so where none
of them are there the rule applies to `tidymedia_program_not_extracted`
like any other: a directory this call created is removed again, and the
error says so instead.

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
