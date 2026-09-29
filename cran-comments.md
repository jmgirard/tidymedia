## Resubmission

This is a resubmission of a first release, answering the review of 0.2.0.

* `\value` added to `?tidyeval`, the page CRAN named. It documents the `.data`
  pronoun reexported from rlang, which is an object rather than a function, so
  the tag says the page documents no function, that `.data` is not called, what
  class it carries, and what subsetting it means. A sweep of every other .Rd
  file found one more page of the same kind, `?audio_stream`, which now carries
  the tag too. The only page still without one is `tidymedia-package.Rd`, the
  package overview, which carries `\docType{package}`.
* Three of the six `\dontrun` uses are gone. `?local_timeout` wrapped an example
  that only defines a function and calls nothing, so it now runs unwrapped.
  `?with_timeout` and `?normalize_audio_batch` are now `\donttest`, and both
  were rewritten to be safe to run: they use the sample clip that ships with the
  package, they write to `tempfile()`, and each is guarded on FFmpeg being
  present. `R CMD check --as-cran` runs them, and reports
  "checking examples with --run-donttest ... OK" both with FFmpeg installed and
  without it.
* Three `\dontrun` uses remain, on `?install_on_win`, `?set_program` and
  `?unset_program`. Running those would download and unpack an FFmpeg build,
  write a file into the user's configuration directory, and delete one.
* No package is installed by any function, example, test or vignette.
  `install_on_win()` downloads a static FFmpeg build, the external tool this
  package is an interface to, named in `SystemRequirements`. It runs only when a
  user calls it. No function of this package calls it, and no example, test or
  vignette runs it. It asks before it downloads or writes, and it aborts in a
  session where nothing can answer. Off Windows it aborts before it downloads,
  writes or asks anything.
* Nothing is written to the user's filespace without being asked for.
  `set_program()` and its wrappers record a binary location under
  `tools::R_user_dir("tidymedia", "config")`, and `install_on_win()` unpacks
  under `tools::R_user_dir("tidymedia", "data")`. `confirm = TRUE` is the
  default on both. The prompt names the exact path, declining creates nothing,
  and a session with no one to ask gets an error rather than assumed consent.
  Both vignettes that produce files set
  `knitr::opts_knit$set(root.dir = tempdir())` in their first chunk, ahead of
  any chunk that writes. The examples that name a short output path pass
  `run = FALSE`, which compiles the FFmpeg command and returns it as a string
  without writing; the examples that do run FFmpeg write to `tempfile()`. The
  tests redirect `R_USER_CONFIG_DIR` and `R_USER_DATA_DIR` to a temporary root.

## R CMD check results

`R CMD check --as-cran`: 0 errors | 0 warnings | 1 note

The note is "New submission". Measured on 2026-09-29 at tidymedia 0.2.1, R
4.6.1, macOS arm64, in 3m 02s. That run had neither FFmpeg nor MediaInfo on the
PATH, and no remembered program location, so it is the same condition a CRAN
machine checks in. It reports "checking for non-standard things in the check
directory ... OK" and "checking for detritus in the temp directory ... OK".

With both tools present the same check gives 0 notes. It also takes longer,
because the execution tests then run.

## Test environments

* local macOS (arm64), R 4.6.1, with and without the two tools on the PATH
* win-builder, R-devel
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
* win-builder flags "tibbles" in the `Description` as possibly misspelled. It is
  spelled as intended. A tibble is the data frame class of the 'tibble' package,
  and the plural is the ordinary way to name what the metadata readers return.

## Reverse dependencies

* None. This is the package's first CRAN release.
