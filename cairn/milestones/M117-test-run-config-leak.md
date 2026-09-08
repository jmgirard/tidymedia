# M117: A test run leaves no location behind in the user's real config directories

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the shipped test suite's filesystem behaviour is what CRAN's machines run
- **Branch/PR:** `m117-test-run-config-leak` — https://github.com/jmgirard/tidymedia/pull/121

## Goal

A run of the test suite writes no remembered location into either of the two real
user config directories.

## Scope

**In:** find and fix the site(s) that escape the config redirect; add a
before/after harness with a positive control in both forms it must cover.

**Out:** removing the two files already on the maintainer's machine → the
maintainer's own call, reported not done. The `set_program()` consent gate's
design → unchanged (D080). A config path that is a directory, or a remembered
location that exists but is not executable → the two existing candidate rows.
`tm_install_dir()` (`R/program_management.R:33`) → outside the promise; this
milestone is scoped to the two config directories.

## Acceptance criteria

- [x] AC1: On a machine where both `tools::R_user_dir("tidymedia", "config")` and
      `rappdirs::user_config_dir("tidymedia", "R")` are absent or empty before the
      run, `devtools::test()` leaves both absent or empty afterwards.
- [x] AC2: The same holds for `R CMD check` on the same machine.
- [x] AC3: The before/after comparison is shown able to fail in both forms it must
      cover: a planted write into each of the two directories from a test body makes
      AC1's comparison report a difference, and a planted write from outside any test
      body — during the check's build or install phase, which is where AC2's leaks
      would come from — makes AC2's comparison report a difference.
- [x] AC4: `devtools::check()` reports 0 errors and 0 warnings, and the `verify` slot
      of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2, T3
- AC3 → T3, T4
- AC4 → T5

## Tasks

- [x] T1: Reproduce from a clean state. With both directories emptied (back them up
      first), run `devtools::test()` and then `R CMD check`, recording which run
      creates which file and each run's skip count. The evidence in hand:
      `mediainfo_location.txt` in the legacy `rappdirs` directory holding
      `/private/tmp/claude-501/.../25b4c341-.../scratchpad/blockbin.sh` (2026-08-26)
      and `ffmpeg_location.txt` under `R_user_dir` (2026-09-01).
- [x] T2: Name what wrote each leftover file, and confirm no site escapes the
      redirect at HEAD. The redirect helper is
      `tests/testthat/helper-program-config.R:41-60`; `R_USER_CONFIG_DIR` redirects
      the `tools::` half while `rappdirs::user_config_dir` is mocked (`:41-49`). The
      `set_program()` calls at `tests/testthat/test-blame-frame-table.R:21-39` and
      `:115-119` do not escape it: every one refuses before reaching a write.
- [x] T3: Build the before/after harness over both directories.
- [x] T4: Add both plant forms, confirm each comparison reports the difference, and
      revert every plant.
- [x] T5: `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), over two rounds with a fresh-context [O] reader that authored none of the criteria. Round 1 returned 11 findings across four milestones, round 2 returned 12 across six. Findings against this milestone: AC3's single plant form stood in for a family free in form as well as location and reached only AC1's comparison (repaired — AC3 now names both forms and binds the second to AC2); AC1/AC2's "the run's skip count is recorded" second sentences were unfalsifiable recording acts (repaired — moved to T1). AC4 noted as a restatement of PROFILE's standing gate; kept.
- 2026-09-07: plan gate chose fixing the escaping test site over widening the redirect helper to cover every write, because the helper already redirects both directories and the defect is a site that bypasses it, not a gap in it; falsified by a second leak from a site that does use the helper.
- 2026-09-08: T1 measured from a clean state. Both real config directories were moved aside (backed up, restored afterwards byte-identical with their original mtimes), then `devtools::test()` ran (FAIL 0 | WARN 10 | SKIP 18 | PASS 13175) and `devtools::check(document = FALSE)` ran (0 errors, 0 warnings, 0 notes, 5m30s). Neither run created either directory. No site escapes the redirect at HEAD.
- 2026-09-08: T1 provenance of the two leftover files. `ffmpeg_location.txt` (2026-09-01, `/opt/homebrew/bin/ffmpeg`) matches what `tests/testthat/test-nvenc-memo.R:103` writes; `git show 425c424` moves that test from a `rappdirs` mock to `R_USER_CONFIG_DIR` in the same commit that moved the package's write target, so the leak existed only in M097's intermediate state and was fixed there. `mediainfo_location.txt` (2026-08-26) names a scratchpad path `blockbin.sh` that `git log --all -S blockbin` finds nowhere but M117's own plan commit — an ad-hoc session, never the suite.
- 2026-09-08: T3 built `tools/config_leak_check.R` — watches both directories as a `--vanilla` subprocess with `R_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` cleared computes them, snapshots file-by-md5 before and after a command, and exits non-zero on a difference (`--expect-difference` inverts that for the controls).
- 2026-09-08: T4 both plants ran. The first pair FAILED and that is what caught a defect in the harness: `tm_dir_diff()` read membership with `[[`, which errors on a named character vector rather than returning NULL, so the run crashed the moment a name was absent from one side — the instrument could report nothing but "no difference". Fixed to `%in%`; both plants then PASSED, each reporting `+ added: tidymedia_leak_probe.txt` in both directories (`devtools::test()` PASS 13176 for the test-body plant; `devtools::check()` 0/0/0 for the build plant). A silent-case control with the plant present but never executed correctly reports no difference. Every plant reverted; `git status` clean of them.
- 2026-09-08: T2 amended at the implementation gate, from "name the escaping site(s) by file:line and fix them" to naming what wrote each leftover file and confirming no site escapes at HEAD. The plan assumed a live escaping site; T1 measured none, and both leftovers are accounted for. Goal, Scope and the acceptance criteria are unchanged -- AC1 and AC2 ask what the run leaves behind, not that a fix was made.
- 2026-09-08: gate chose a hand-run `tools/` script over a CI leg, because the leak it guards against arose inside a half-finished working state rather than on a pushed branch, and a leg would cost about six minutes on every push; falsified by a leak that reaches a pushed branch. The CI question is not deferred to a backlog row -- it was settled, not postponed.
- 2026-09-08: gate chose to leave the two leftover files on the maintainer's machine as Scope already directed; both were restored byte-identical with their original mtimes after T1's clean-state measurement.
- 2026-09-08: T5 on the finished tree. `devtools::test()`: FAIL 0 | WARN 10 | SKIP 18 | PASS 13175. `devtools::check()`: Status OK, 0 errors, 0 warnings, 0 notes. Tasks all checked; status to review.
- 2026-09-08: step-7 approval: PR #121 approved for merge, after the eight fix-now findings landed and all four criteria were re-measured against the shipped harness.
- 2026-09-08: CI wait hit the harness ceiling with 2 of 8 legs green (macos-latest release 7m36s, pkgdown 2m16s) and 6 pending; the moved watcher was stopped rather than left armed, and the merge was not made. PR #121 is ready for review, not draft.

## Review

Evidence gathered 2026-09-08, PR #121. Every figure below is from a run of the
harness **as it ships** — the fix-now repairs from the gate landed first, and
all four measurements were then re-run, so nothing here describes superseded
code. Preconditions: both real config directories were moved aside into the
session scratchpad before each set of runs (`current` held
`ffmpeg_location.txt`, md5 `668376533c6a0169d1489b53cd897194`; `legacy` held
`mediainfo_location.txt`, md5 `bce5cbc9a2639c0e6db112ec4710015b`), so each run
started from the absent-or-empty state AC1 and AC2 name, and both were restored
afterwards with those md5s and their original mtimes re-verified.

- AC1 — PASS. `Rscript tools/config_leak_check.R -- Rscript -e 'devtools::test()'`
  exited 0. Watched `/Users/jmgirard/Library/Preferences/org.R-project.R/R/tidymedia`
  and `/Users/jmgirard/Library/Application Support/tidymedia`: each 0 files before
  and 0 files after, "no difference" on both, and neither directory was created.
  The suite itself: FAIL 0 | WARN 10 | SKIP 18 | PASS 13175.
- AC2 — PASS. `Rscript tools/config_leak_check.R -- Rscript -e 'devtools::check()'`
  exited 0 from the same clean state. Both watched directories: 0 files before,
  0 files after, "no difference" on both. `R CMD check` itself: Status OK,
  0 errors, 0 warnings, 0 notes, 5m 32.5s (tidymedia 0.1.0.9000).
- AC3 — PASS, both forms. Test-body form:
  `--plant=test-body --expect-difference -- Rscript -e 'devtools::test()'` exited 0,
  reporting `+ added: tidymedia_leak_probe.txt` in both directories (0 files before,
  1 after in each); the suite ran the planted test (PASS 13176 against AC1's 13175).
  Build form: `--plant=build --expect-difference -- Rscript -e 'devtools::check(document = FALSE)'`
  exited 0, the plant living in `R/zzz-config-leak-probe.R` — top-level package code
  executed at the check's install phase, never inside a test — and reporting the same
  `+ added:` line in both directories. Both plant files were removed on exit and both
  probe files cleaned up: `git status` clean of them and both directories absent again
  before the next run.
- AC4 — PASS. `devtools::check()` (AC2's run): Status OK, 0 errors, 0 warnings,
  0 notes. `verify` slot clean: that run's `document()` step left no diff, and
  `devtools::test()` was FAIL 0 | WARN 10 | SKIP 18 | PASS 13175.

### Consistency gate

`cairn_validate.py` exit 0, all 16 checks PASS and all 7 advisories OK — the
`release window` advisory did not fire. No `DESIGN.md` principle changed
(`Principles touched: —`), so `cairn_impact.py` was not run.

`r-package` profile `consistency-gate` slot: `devtools::check()`'s `document()`
step left no diff; the diff hand-edits no generated file (`NAMESPACE`, `man/`,
`data/` untouched); README.md and README.Rmd are in sync (both last written by
`0cf121d`, and the diff touches neither); `pkgdown::check_pkgdown()` reports
"No problems found"; no `NEWS.md` entry is owed, because `^tools$` is
`.Rbuildignore`d and the milestone changes no shipped package behaviour;
`^tools$` already covers the one new file; full `devtools::check()` clean.

### Independent review

Three fresh-context lenses, none having seen the implementation, each on a
distinct evidence base. Executable surface is touched, so the full fan-out ran.

**[S] blame-history — no findings.** It verified rather than refuted: `git show
425c424` confirms M097 moved `test-nvenc-memo.R` off the `rappdirs` mock in the
same commit that moved the package's write target, so the leak window closed
there; `git log --all -S blockbin` finds the string only in M117's own commits,
corroborating the ad-hoc-session account; the `set_program()` calls in
`test-blame-frame-table.R` all refuse above the write; no recorded decision
requires CI enforcement of config-directory hygiene, so the hand-run choice
reverses nothing.

**[S] prior-review record — two weak echoes, no regression.** The probe
`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so
the per-PR thread walk was skipped and the archived `## Review` sections were
the evidence base. It found the diff *satisfying* past findings rather than
regressing them (M079/M112's positive-control demands, M113/M114-115's
`XDG_CONFIG_HOME` lesson, M104's tilde-expansion trap). Its two candidates and
their dispositions: `list.files(recursive = TRUE)` descends through a directory
symlink (M103's primitive) — **rejected**, out-of-scope taxonomy: the harm M103
named was destructive cleanup off that enumeration, which this script does not
do (`tm_remove_probe()` targets a fixed filename); and `tm_watched_dirs()`
deriving the watched paths from the same library calls under discussion (M097's
F1 pattern) — **rejected**, the lens itself confirmed it does not reproduce, the
script establishing where the package would write rather than asserting those
functions correct.

**[O] diff-bug — ten findings**, ranked as the lens ranked them, all in
`tools/config_leak_check.R`. Eight were fixed on the branch at the maintainer's
direction at the gate; one was routed to a candidate row; one was defused by
another's fix. Every fix is below, with what shows it works.

1. **Fixed.** A command that never ran still reported PASS: `system2()`'s status
   was printed but never gated the verdict, so a typo'd command gave 127, no
   directory changed, and the script said PASS. Two guards now: `Sys.which()`
   refuses an unfindable command before anything is planted, and statuses 126
   and 127 fail the verdict whatever `--expect-difference` asked for. Shown to
   fire: `-- Rscritp -e 'devtools::test()'` exits 1 with "command not found on
   PATH: Rscritp".
2. **Fixed.** A leak that rewrote an existing file with identical content was
   invisible, state being name + md5 only — on a populated machine the likeliest
   leak of all, and precisely the one this repo has physical evidence of. State
   now records md5, size and mtime. Shown to fire: writing
   `/opt/homebrew/bin/ffmpeg` twice into a probe directory leaves the md5
   identical on both sides and now reports `~ changed: ffmpeg_location.txt`.
3. **Fixed.** The header claimed the plant is removed "whether the run succeeds,
   fails, or is interrupted", which `on.exit()` does not deliver for every
   signal. Both halves of the replacement text were measured rather than
   recalled: a probe script under `Rscript` printed its `on.exit` message when
   sent SIGINT and printed nothing when sent SIGTERM. The comment now names
   Ctrl-C as covered, SIGTERM/SIGKILL as not, and lists the five paths to delete
   by hand after a kill.
4. **Fixed.** Plant cleanup unlinked only the leaf, leaving any intermediate
   levels `dir.create(recursive = TRUE)` had made. It now walks back up, deleting
   each empty level and stopping at the first that existed before the run.
   Shown to work in AC3's control 1: `.../org.R-project.R/R/tidymedia` was
   removed and `.../org.R-project.R/R` was kept, its other contents intact.
5. **Routed to a candidate row.** The watched directories are computed under
   `--vanilla`, which ignores `~/.Renviron`, while the measured run reads it — so
   on a machine setting `R_USER_CONFIG_DIR` there the harness would watch the
   wrong pair. Not fixed here: the repair changes what "the watched directories"
   means, which is a design question rather than a defect in this instrument.
   The search-first sweep found no existing candidate row covering it, so it
   gets a new one — written in the post-merge hygiene commit, where pruning
   M114's terminal row under the three-row cap pays for the line ROADMAP needs
   to stay under 60.
6. **Defused by fix 1.** Options placed after `--` are swallowed and become the
   command name; the `Sys.which()` guard now makes that a loud refusal instead of
   a silent PASS.
7. **Fixed.** Plant paths are relative with no package-root check, so a run from
   the wrong directory would write into an unrelated project. It now refuses
   unless `DESCRIPTION` is present and names `tidymedia`. Shown to fire: run from
   `/tmp` it exits 1 with "no DESCRIPTION here".
8. **Fixed.** An `md5sum()` of `NA` on both sides read as unchanged, since
   `identical(NA, NA)` is `TRUE`. An unreadable file's digest is now the literal
   `unreadable`, and the mtime in the same state string moves on any rewrite.
9. **Fixed.** The file carried a shebang but was committed `100644`; now `100755`,
   matching two of its four siblings.
10. **Fixed.** The dead first assignment in `tm_write_plant()` is gone.

The lens also confirmed: `^tools$` covers the new file; T2's claim that no
`set_program()` call site escapes the redirect is accurate (every one passes a
non-string or a pinned-absent path and refuses above the write); clearing
`R_USER_CONFIG_DIR` in `tm_watched_dirs()` is load-bearing, not belt-and-braces;
the `%in%` fix is correct; and the two-plant split does reach the two origins
AC3 names, since `R/zzz-*.R` executes at install where no test-body plant runs.

**Return floor.** No finding demonstrated an acceptance criterion failing. AC1
and AC2 were measured from the absent-or-empty precondition the criteria name,
and both runs demonstrably executed (exit status 0 with the suite's own
`PASS 13175` and `R CMD check`'s `Status: OK` in the logs), so finding 1's
never-ran path was never on the evidence path. AC3's controls both reported the
difference. Status stayed `review` throughout; no defect return was taken.

**PR conversation.** No reviews and no unresolved threads on PR #121.
- conversation: codecov[bot] PR #121 — noted; coverage unchanged at 98.43%,
  requests nothing (author type `Bot`, so the merge chip was unaffected).
