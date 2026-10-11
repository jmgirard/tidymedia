## Submission

This patch fixes the two ERRORs the CRAN checks of 0.2.1 report on
`r-devel-linux-x86_64-fedora-clang` and `-gcc`, due by 2026-10-31.

* Examples failed with "Unknown encoder 'libx264'", which Fedora's FFmpeg
  lacks. Examples, vignettes and tests that re-encode now run only where the
  build has the encoder they name, as the new `has_encoder()` reports.
* Tests failed because `Sys.which()` there answers `""` for an absolute path.
  A saved program location is now read from the filesystem.

## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 1. This update fixes the check ERRORs CRAN asked to
  have corrected by 2026-10-31.

## Test environments

* local macOS (arm64), R 4.6.1, with and without FFmpeg and MediaInfo on the PATH
* win-builder, R-devel
* GitHub Actions: Fedora 44 with `ffmpeg-free` (no `libx264`), macOS, Windows,
  and Ubuntu (devel, release, oldrel-1, 4.1.0)

## Reverse dependencies

None.
