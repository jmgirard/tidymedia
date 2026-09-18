## R CMD check results

Local `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes
(tidymedia 0.2.0, 2026-09-18, R 4.6.1, macOS arm64, 8m 18s)

* This is a new release. CRAN's incoming checks are expected to raise the usual
  "New submission" NOTE.

## Test environments

* local macOS (arm64), R 4.6.1
* win-builder, R-devel (submitted 2026-09-18)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release,
  oldrel-1, and 4.1.0, the declared `Depends: R (>= 4.1.0)` floor)

## Notes

* tidymedia is an interface to the FFmpeg and MediaInfo command-line tools.
  Neither tool is bundled with the package. `SystemRequirements` names both,
  with their project URLs.
* Examples, tests and vignette chunks that invoke those binaries are guarded.
  Where the tools are absent, they are skipped. The execution tests also skip on
  CRAN's own check. The package therefore checks cleanly on a machine with
  neither tool installed.
* `install_on_win()` downloads a Windows FFmpeg build on request only. No
  example or vignette runs it. The tests that call it aim at a local `file://`
  URL that does not exist, so no test reaches the network.
* The package writes outside a temporary directory only on the user's own
  request. `set_program()` and its wrappers record a binary location under
  `tools::R_user_dir("tidymedia", "config")`, and `install_on_win()` unpacks
  under `tools::R_user_dir("tidymedia", "data")`. Both confirm with the user
  before they write, and both refuse rather than assume consent in a session
  with no one to ask. The tests redirect those directories to a temporary root.

## Reverse dependencies

* None. This is the package's first CRAN release.
