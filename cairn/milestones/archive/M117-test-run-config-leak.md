# M117: A test run leaves no location behind in the user's real config directories

**Status:** done (2026-09-08, PR #121 https://github.com/jmgirard/tidymedia/pull/121)

**Goal:** A test-suite run writes no remembered location into either real user config directory.

**Outcome:** The premise was falsified, not fixed: from a clean state neither `devtools::test()`
nor `R CMD check` creates `tools::R_user_dir("tidymedia", "config")` or
`rappdirs::user_config_dir("tidymedia", "R")`, and no site escapes the redirect in
`tests/testthat/helper-program-config.R` at HEAD. The two leftovers are accounted for:
`ffmpeg_location.txt` from M097's intermediate state, closed by `425c424` itself, and
`mediainfo_location.txt` from an ad-hoc session, never the suite. What ships is
`tools/config_leak_check.R`, which computes both directories in a subprocess with
`R_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` cleared, snapshots each file by md5, size and mtime
around a command, and exits non-zero on a difference. Two controls prove its reach: a plant in a
`test_that()` body, and one in `R/zzz-*.R` top-level code run at install. No package code changed.

**Decisions:** none promoted. Local: a hand-run `tools/` script over a CI leg, the leak having
arisen in a half-finished working state (falsified by one reaching a pushed branch).

**Review:** One pass, no defect and no amendment returns. Blame-history and prior-review found
nothing, the latter finding the diff satisfying past positive-control findings rather than
regressing them. The [O] lens returned ten, all in the harness: eight fixed and all four criteria
re-measured against the fixed instrument, one routed to a candidate row, one defused. The two that
mattered were a PASS reported for a command that never ran, and blindness to a same-bytes rewrite.
