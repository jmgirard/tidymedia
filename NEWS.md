# tidymedia 0.0.0.9001 (development version)

## Infrastructure

* Added a testthat (3rd edition) test suite covering the `ffm_*` pipeline
  builder and `ffm_compile()` output, plus binary-gated tests for the
  ffmpeg/ffprobe/mediainfo task functions.
* Added GitHub Actions workflows for `R CMD check` (macOS, Windows, Linux) and
  test coverage; the Linux jobs install ffmpeg and mediainfo so execution
  tests run in CI.
* All input validation and user-facing messages now use rlang's `check_*`
  helpers and cli (`cli::cli_abort()` / `cli::cli_warn()`); the assertthat
  dependency has been removed. Added `dplyr`, `tidyr`, `purrr`, and `cli` to
  Imports (the first three were already used but undeclared).
* Enumerated arguments (e.g. `units`, `unit`, `section`, `template`,
  `program`) are now matched exactly via `rlang::arg_match()` instead of the
  partial matching of `match.arg()`; pass the full value.

# tidymedia 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
