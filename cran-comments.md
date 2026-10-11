## Submission

This is a patch release. It fixes the two ERRORs that the CRAN checks of 0.2.1
report on `r-devel-linux-x86_64-fedora-clang` and
`r-devel-linux-x86_64-fedora-gcc`, before the correction deadline of
2026-10-31.

* The examples step failed with FFmpeg's "Unknown encoder 'libx264'". Fedora
  builds FFmpeg without `libx264`, because x264 is not free software. Every
  example and vignette chunk that re-encodes now names its encoder and runs
  only where the FFmpeg build has that encoder. The new exported function
  `has_encoder()` answers that question. Tests that name an encoder skip where
  the build lacks it.
* The tests step failed because `Sys.which()` on those machines answers `""`
  for an absolute path, so a saved program location read as missing. The
  package now reads a saved location from the filesystem, and uses
  `Sys.which()` only for a bare program name.

The release also adds an error class for a failed FFmpeg run whose encoder the
build lacks. NEWS.md lists every change.

## R CMD check results

`R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes

Measured on 2026-10-11 at tidymedia 0.2.2, R 4.6.1, macOS arm64, in 2m 15s.
That run had neither FFmpeg nor MediaInfo on the PATH, no remembered program
location, and `NOT_CRAN` unset, so it is the same condition a CRAN machine
checks in. It reports "checking examples with --run-donttest ... OK",
"checking for non-standard things in the check directory ... OK" and
"checking for detritus in the temp directory ... OK".

With both tools present, `devtools::check()` gives the same result, 0 notes.

win-builder (R-devel, 2026-10-09 r90655 ucrt) gives 1 NOTE, from the incoming
feasibility check: "Days since last update: 1". This update comes soon after
0.2.1 because it fixes the check ERRORs CRAN asked to have corrected by
2026-10-31. Every other check on win-builder is OK.

## Test environments

* local macOS (arm64), R 4.6.1, with and without the two tools on the PATH
* win-builder, R-devel
* GitHub Actions, Fedora 44 with Fedora's own `ffmpeg-free` (no `libx264`) and
  `NOT_CRAN` unset, the model of the two failing flavors
* GitHub Actions, macOS (release) and Windows (release)
* GitHub Actions, Ubuntu (devel, release, oldrel-1, and 4.1.0, the declared
  `Depends: R (>= 4.1.0)` floor)

## Notes

* tidymedia is an interface to the FFmpeg and MediaInfo command-line tools.
  Neither tool is bundled with the package. `SystemRequirements` names both,
  with their project URLs.
* Examples, tests and vignette chunks that invoke those binaries are guarded.
  Where the tools are absent, they are skipped. The execution tests also skip on
  CRAN's own check. The package therefore checks cleanly on a machine with
  neither tool installed.
* win-builder flagged "tibbles" in the `Description` as possibly misspelled at
  0.2.0. It is spelled as intended. A tibble is the data frame class of the 'tibble'
  package, and the plural is the ordinary way to name what the metadata readers
  return.

## Reverse dependencies

* None. `tools::package_dependencies("tidymedia", reverse = TRUE)` over CRAN
  returns no package (checked 2026-10-11).
