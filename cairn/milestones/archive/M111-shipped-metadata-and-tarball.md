# M111: The shipped metadata and the tarball say what CRAN's incoming checks expect

**Status:** done (2026-09-05, PR #115 https://github.com/jmgirard/tidymedia/pull/115)

**Goal:** Rewrite `DESCRIPTION`'s `Title` and `Description` to the form CRAN's incoming checks accept, and stop shipping four files and one URL that should not be in the tarball.

**Outcome:** `Title` is now `Media File Preprocessing and Metadata for the 'tidyverse'`, `Description` two sentences quoting `'FFmpeg'`, `'MediaInfo'` and `'tidyverse'`; `R CMD check --as-cran` under `_R_CHECK_CRAN_INCOMING_=TRUE` reports one NOTE (maintainer line plus dev-version suffix), none naming either field. `inst/extdata/{ffmpeg,mediainfo}_location.rds`, dead since M097 moved the remembered location to `tools::R_user_dir()`, are deleted; `.Rbuildignore` gained `^tests/testthat/_problems$`, `^tests/testthat/testthat-problems\.rds$` and `^tools$`. README's Homebrew URL moved to `install/HEAD/`, the lifecycle badge to `lifecycle.r-lib.org` (urlchecker's suggested target 404s). `_pkgdown.yml` lost the `has_hardware_encoder` duplicate and lists page names, so `set_mediainfo()`, `find_mediainfo()` and `probe_video()` reach the rendered index; `tools/pkgdown_duplicate_topics.R` resolves each `contents:` entry to its `.Rd` file and is AC5's instrument.

**Decisions:** none.

**Review:** Three-lens fan-out (user-facing tier); blame-history and prior-PR-comments lenses 0 findings, diff-bug lens 10 ranked. Fixed now: an `.Rbuildignore` guard naming a path testthat never writes, two factual slips in the NEWS bullet, the script's undeclared `yaml` requirement. Follow-up row: the script drops parenthesised `contents:` entries silently. Six rejected with reasons. At hygiene the M092 milestone-authoring lesson was pruned to hold LESSONS' byte budget.
