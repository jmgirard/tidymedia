## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local macOS, R release
* GitHub Actions: macOS, Windows, and Ubuntu (R devel, release, and oldrel-1)

## Notes

* tidymedia is an interface to the FFmpeg and MediaInfo command-line tools.
  Examples, tests, and vignette chunks that invoke those binaries are guarded so
  they are skipped when the tools are unavailable; the package therefore checks
  cleanly on machines without them.
