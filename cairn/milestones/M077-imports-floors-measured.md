# M077: The nine other Imports floors, measured

- **Status:** review
- **Priority:** normal
- **Depends on:** M076
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m077-imports-floors-measured` / https://github.com/jmgirard/tidymedia/pull/81

## Goal

Every version floor DESCRIPTION's `Imports` field declares is exercised against
the version it names, so each floor states what was measured rather than what
was assumed.

## Scope

Surface tier: **user-facing** — a declared floor is what a user's installer may
resolve to, so a floor nothing has run is a promise the package has not kept.

**In:** `data-raw/imports-floors.R`, a developer-only script that builds one
library pinning every declared `Imports` floor at once and runs the test suite
under it; per-package bisection only if that run fails; DESCRIPTION floors left
or moved on the result; a D-entry carrying the per-floor disclosure; a NEWS
entry carrying the user-visible fact.

**Out:** a permanent CI job installing the floors on every push → a candidate
row added by this plan, promoted on the first floor regression that reaches a
user. Nine one-floor-at-a-time legs and a three-OS run → not done; the gap each
leaves is disclosed in the D-entry, not closed. The `Depends: R (>= )` field →
M076. Hardening `data-raw/withr-floor.R`'s four inherited rough edges → stays
on the `Imports`-floors candidate row.

## Acceptance criteria

- [x] AC1 Every non-base entry of DESCRIPTION's `Imports` field — the entries
      `read.dcf(DESCRIPTION, "Imports")` enumerates, less `tools` and `utils`,
      which carry no floor — declares a version that AC2's run loaded and
      passed on. A floor whose run failed has moved to one that passes.
- [x] AC2 The package's `testthat` suite — less `test-with-timeout.R`,
      `test-runtime-timeout.R` and `test-timeout-silence.R`, the three files
      whose fixtures block a spawned program on a named pipe and which wedge on
      the runner AC4 names, for a reason no dependency floor touches — runs to
      completion in a fresh `Rscript` session whose first `.libPaths()` entry is a library holding
      exactly the version each of those entries declares, with 0 test failures
      and a skip count equal to the same three-file-less suite's skip count on
      current dependencies; the session asserts `ffmpeg` and `mediainfo` are both on
      `PATH` before running, and asserts per pinned package both the version
      loaded and the directory it loaded FROM. A mismatch, a failing
      `test_that()` block, an absent binary, or a non-zero child exit stops the
      run.
- [x] AC3 Where a declared floor cannot be installed or built on the R and
      system toolchain AC2's run uses, the error is recorded and the floor
      moves to the first version that installs and passes AC2, found by walking
      that package's CRAN Archive listing forward from the declared floor and
      attempting each in turn.
- [x] AC4 A `cairn/DECISIONS.md` entry states, per floor this milestone leaves
      in place or moves, what was run against it — naming AC2's suite, its R
      version and its runner OS — and these three things that were not: the
      pinned set is the direct `Imports` only, so siblings and transitive
      dependencies were at their current CRAN versions, except the packages
      the run held back so the pinned floors could load at all — the entry
      names each one, the version it was held at, and the requirement that
      forced it — and except the three files AC2 excludes, so nothing the
      timeout surface does was exercised under the pinned floors; no floor was
      run alone against
      current siblings, so a joint pass does not attribute; and the run was on
      one operating system.
- [x] AC5 `NEWS.md` states as a user-visible fact that the declared dependency
      floors are now measured. `devtools::test()` and `devtools::check()` clean
      (0 errors, 0 warnings) on current dependencies.

## Coverage

- AC1 → T2, T4
- AC2 → T1, T2
- AC3 → T3
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1 Write `data-raw/imports-floors.R`: read the `Imports` entries and their
      floors from DESCRIPTION by `read.dcf`, install each into one library
      under a temporary root, and drive a fresh `Rscript` child with that
      library first. Carry over M074's load-bearing control from
      `data-raw/withr-floor.R:1-40` — assert per package the *directory* it
      loaded from, not only the version string, since the user library holds
      current releases and a failed pin would otherwise pass silently.
- [x] T2 Record the current-dependency baseline skip count first, then run the
      pinned suite. Record per-file pass / fail / skip counts in the milestone
      file. Note `archive` 1.1.1 needs `libarchive` and compiles against `cli`
      headers (`LinkingTo: cli, cpp11`), and `purrr` 1.0.0 also `LinkingTo:
      cli` — expect these two to be where an install fails, if any does.
- [x] T3 On a failure: bisect per package (re-run with one floor pinned and the
      rest current) to attribute it, then apply AC3's Archive walk to the
      package it attributes to.
- [x] T4 Apply the result to DESCRIPTION — floors left or moved.
- [x] T5 Draft the D-entry per AC4, including its falsifier.
- [x] T6 NEWS entry, `devtools::document()`, `devtools::test()`,
      `devtools::check()`.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: criteria audit (full mode, user-facing tier) returned 13 findings across both milestones' drafts; AC5 was the one criterion here that passed all six questions clean. Fixed for M077: AC1 was a tautology satisfied by any DESCRIPTION; AC2's "0 test failures" was satisfiable by a run where every execution test skipped, since the suite `skip_if`s the media binaries; AC3's "the oldest version that does install" named no procedure enumerating the archived versions; AC4 claimed something of *every* transitive dependency with nothing enumerating the closure.
- 2026-08-27: plan gate chose one joint pinned-library run with bisection on failure over nine one-floor legs plus the joint one, because the extra nine builds only buy attribution in the case where something fails; falsified by a joint run that fails and whose bisection is itself expensive, or by a floor that passes jointly and fails against current siblings.
- 2026-08-27: plan gate chose a one-off `data-raw/` script over a permanent hand-rolled min-deps CI job, because the r-lib action has no oldest-version input so the job must be hand-rolled, and a permanent job commits the repo to keeping ten floors green on every push; falsified by a floor regression reaching a user between audits.
- 2026-08-27: plan gate chose keeping the per-floor what-ran / what-did-not statement in the D-entry with NEWS carrying only the user-visible fact, over binding that sentence in NEWS, because that exact slot failed three review rounds in M074 before being descoped to this row; the audit also read a NEWS-bound version as promising a property of the write-up rather than of the package. Disclosed: AC4 still binds a record rather than the package, which is the user's call at this gate rather than an oversight. Falsified by the D-entry proving as hard to state accurately as the NEWS sentence was.

- 2026-08-27: gate 1 — the declared floors do not build on the host's R 4.6.1: R 4.5 hid `Rf_findVar`/`ATTRIB` behind `ENABLE_LEGACY_NONAPI_FUNS` and dropped `SET_FORMALS`/`SET_CLOENV`/`PRVALUE` outright, so `cli` 3.4.0, `rlang` 1.1.0, `tibble` 3.1.4, `purrr` 1.0.0 and `dplyr` 1.1.0 fail to compile and `archive` 1.1.1 finds no `libarchive`; AC3's walk forward reached today's current release in every case. User chose measuring under an older R in a container (colima + `rocker/r-ver:4.4.3`, the newest release that still declares those entry points and inside the package's own `R (>= 4.1.0)`) over moving six floors to 2026 releases or amending AC1/AC3 to disclose instead. Criteria unchanged.
- 2026-08-27: amendment (substantive, gate 2) — AC4's "current CRAN versions" clause now carves out the test harness. With the nine floors pinned and everything else current, R's own namespace version checks reject three floors for reasons no user installs: current `testthat` requires `withr (>= 3.0.2)` and `cli (>= 3.6.5)`, current `furrr` requires `purrr (>= 1.2.1)`. Moving those three would have superseded D053's measured `withr` 2.5.0 floor for a test-side reason. User chose holding the harness at the newest versions the floors permit, so floors move only where tidymedia's own runtime closure forces it — current `vctrs` and `dplyr` requiring `rlang (>= 1.1.7)`, current `dplyr` requiring `cli (>= 3.6.2)` and `tibble (>= 3.2.0)`. AC1, AC2, AC3 and AC5 unchanged; AC4 amended as above.
- 2026-08-27: criteria audit of the amended AC4, full mode (user-facing tier), run in-session rather than by a fresh-context [O] reader because agent delegation is disabled in this session — one finding, fixed at the gate: the draft carved out "`testthat`, `pkgload`, and the `Suggests` the suite loads", a universal claim over a set no named procedure enumerates; the carve-out now names the packages the run itself held back, which the run records. The satisfiability, reachability, proportionality and probe questions returned nothing. The instrument question returns what the plan gate already disclosed and the user already took — AC4 binds a record rather than the package — unchanged by this amendment.

- 2026-08-28: T1 — `data-raw/imports-floors.R` written. It reads the `Imports` floors by `read.dcf`, orders the installs by the LinkingTo/Imports edges among the pinned set itself (so `archive` and `purrr` compile against the PINNED `cli` headers rather than the user library's), drives `R CMD INSTALL` directly so AC3 records the compiler's own error rather than `install.packages()`'s "non-zero exit status" warning, and runs the suite in a fresh `Rscript` whose first `.libPaths()` entry is the pinned library. It carries M074's load-bearing control over: per pinned package it asserts the version AND the directory resolved, before anything loads and again for every pinned namespace loaded after the suite. Added beyond the plan: `--baseline`, `--only`, `--repair` and `--walk` modes; `TM_LIBROOT` to persist the pinned libraries across runs (whose reuse is guarded by re-reading the installed DESCRIPTION's `Version`, not by `file.exists()` — the trap M074's review left on the `Imports`-floors row); and `TM_RUN_TIMEOUT`, a wall-clock bound on each child, after two runs wedged for over half an hour on a single spawn.

- 2026-08-28: amendment (substantive, gate 3) — AC2 now names the suite it runs as the suite less `test-with-timeout.R`, `test-runtime-timeout.R` and `test-timeout-silence.R`, and AC4 records that exclusion among what was not measured. Those three build a named pipe with no writer and run `ffmpeg` against it, expecting the package's limit to kill it: on this runner a blocked `ffmpeg` survives `kill -TERM` and dies only on `kill -KILL`, and `system2(stdout = TRUE, input = , timeout = )` — the call `R/program_management.R:125` makes — does not escalate. One isolated run took 191.8 s against a 2 s limit; six consecutive full-suite runs never returned. The baseline wedges identically, so no floor is implicated. Six attempts to get a green pair another way failed first: a colima VM restart, container-local I/O instead of the bind mount, and `--init` (zombies went from hundreds to zero, the wedge unchanged). User chose excluding the three from BOTH runs, named and disclosed, over retrying indefinitely or descoping the milestone. The timeout behaviour itself goes to a ROADMAP candidate row.
- 2026-08-28: criteria audit of the amended AC2 and AC4, full mode (user-facing tier), run in-session for the reason recorded above — nothing returned. The excluded set is three named files, so the promise's domain stays enumerable; the exclusion narrows what AC2 promises rather than widening it; and AC4's added item is the disclosure that narrowing requires.

- 2026-08-28: T2, T4 — measured. Runner: `rocker/r-ver:4.4.3` (R 4.4.3, Ubuntu noble, aarch64) under colima on macOS 26.5, with `ffmpeg` 6.1.1 and `mediainfo` on `PATH`. The nine floors install into one library and the suite passes on them, the baseline and pinned runs identical file for file: **pass=6120 fail=0 skip=22 over 66 files**, both. The pinned child resolved each of the nine from `/libs/all` at exactly the declared version, asserted on the DIRECTORY and not only the version string; the baseline child resolved the current releases from the site library — archive 1.1.14, cli 3.6.6, dplyr 1.2.1, glue 1.8.1, purrr 1.2.2, rappdirs 0.3.4, rlang 1.3.0, tibble 3.3.1, withr 3.0.3. Held back so the floors could load: `testthat` 3.3.2 -> 3.1.10 (its current release needs `cli (>= 3.6.5)` and `withr (>= 3.0.2)`) and `furrr` 0.4.0 -> 0.3.1 (needs `purrr (>= 1.2.1)`). Both are test-side; moving a runtime floor for them would have raised what a user must install for something the user never runs, D053's measured `withr` 2.5.0 among them.
- 2026-08-28: T3 — attribution, and the milestone's one floor move. Pinned at the declared `rlang` 1.1.0, 1528 tests failed, every one of them reading `'check_string' is not an exported object from 'namespace:rlang'` (or `check_bool`, or `check_number_whole`). `R/` calls `rlang::check_string()` 46 times, `check_bool()` 36, `check_number_whole()` 38 and `check_number_decimal()` 12, and a NAMESPACE walk over every rlang release from 1.0.0 through 1.2.0 puts the first export of all four in **1.2.0** — so on any earlier release every verb in the package aborts at its own front door. No bisection run was needed: the failure names its own package and the walk confirms it. The environment forces the same direction independently, current `vctrs` requiring `rlang (>= 1.1.7)`. DESCRIPTION now declares `rlang (>= 1.2.0)`; the other eight floors stand, and a static sweep of every `pkg::fn` call in `R/` against each floor version's NAMESPACE finds all of them exported at the version declared.
- 2026-08-28: a harness defect found and fixed mid-measurement, recorded because it is the exact failure this harness exists to prevent. `system2(env = )` pastes its assignments into a `sh -c` line unquoted, so the `;` separating the `TM_PINS` entries ended the assignment and began a new command: neither `TM_LIB` nor `TM_PINS` reached the child, and the child — which inferred "baseline" from an empty `TM_LIB` — skipped every provenance assertion while still reporting a green suite. `R_LIBS` survived, prefixed to the final command, so the pin itself held and the numbers were right; nothing proved it. Fixed by quoting the values and by making the mode an explicit `TM_MODE` the child refuses to run without. The counts recorded above are from the re-run with the assertions live.

Per-file counts, identical in the baseline and the pinned run (66 files; `test-with-timeout.R`, `test-runtime-timeout.R` and `test-timeout-silence.R` excluded from both, per AC2):

```
  file                                         pass fail skip
  test-anonymize-video-batch.R                   60    0    0
  test-anonymize-video.R                         25    0    0
  test-audio-codec.R                            177    0    0
  test-audio-index-docs.R                        43    0    0
  test-audio-stream-crop-segment.R               86    0    0
  test-audio-stream-format-web.R                 30    0    0
  test-audio-stream-normalize.R                 195    0    0
  test-audio-stream-passthrough.R                85    0    0
  test-audio-stream.R                            64    0    0
  test-audio-track-drop.R                       106    0    0
  test-builder-blame-front-door.R               631    0    0
  test-check-batch-cell.R                        24    0    0
  test-codec-arg-front-door.R                  1143    0    0
  test-codec-null-na-semantics.R                140    0    0
  test-compare-videos-batch.R                    22    0    0
  test-concatenate-videos-batch.R                20    0    0
  test-contradiction-front-door.R               146    0    0
  test-convert-audio-batch.R                     36    0    0
  test-crop-video-batch.R                        22    0    0
  test-extract-audio-batch.R                     25    0    0
  test-extract-frame-batch.R                     29    0    0
  test-failed-run-cleanup.R                      54    0    2
  test-fan-in-batch-forwarding.R                 11    0    0
  test-ffm-batch.R                               35    0    0
  test-ffm-manifest.R                            30    0    0
  test-ffm.R                                    227    0    0
  test-ffmpeg.R                                 135    0    0
  test-ffprobe.R                                 35    0    0
  test-fixture-helpers.R                         11    0    0
  test-format-for-web-batch.R                    16    0    0
  test-front-door-ordering.R                    245    0    0
  test-input-path-front-door.R                  152    0    3
  test-local-timeout.R                          105    0    0
  test-loudnorm-two-pass.R                       16    0    0
  test-mediainfo.R                               46    0    0
  test-normalize-audio-batch.R                  104    0    0
  test-normalize-audio.R                         43    0    0
  test-normalize-audios-two-pass.R               66    0    0
  test-nvenc-docs.R                              13    0    0
  test-nvenc-front-door.R                       113    0    0
  test-nvenc-memo-grid.R                         51    0    0
  test-nvenc-memo.R                              34    0    0
  test-nvenc.R                                   83    0    3
  test-package-topic.R                            3    0    0
  test-parallel-option-carry.R                   31    0   11
  test-parallel-surface.R                         3    0    0
  test-picture-in-picture-batch.R                24    0    0
  test-probe-compact-parser.R                   100    0    0
  test-probe-parallel.R                          28    0    0
  test-probe-single-call.R                       10    0    0
  test-probe-typed-resilience.R                  43    0    0
  test-program-management.R                       3    0    0
  test-row-locator-grid.R                       257    0    0
  test-sample-frames-batch.R                     33    0    0
  test-segment-video-batch.R                     39    0    0
  test-separate-audio-video-batch.R              53    0    0
  test-separate-av-codec.R                       79    0    0
  test-separate-av-multitrack.R                  76    0    1
  test-shared-range-bindings.R                   51    0    0
  test-standardize-video-batch.R                 50    0    0
  test-strip-metadata-batch.R                    31    0    0
  test-strip-metadata.R                          25    0    0
  test-utils.R                                    4    0    0
  test-value-check-front-door.R                 287    0    0
  test-verify.R                                  34    0    0
  test-video-codec.R                            122    0    2
```

- 2026-08-28: T5, T6 — D055 appended to `cairn/DECISIONS.md`; NEWS entry under Requirements states the measurement and the `rlang` move; `devtools::document()` no diff, `devtools::test()` FAIL 0 / SKIP 5 / PASS 6690, `devtools::check()` 0 errors, 0 warnings, 0 notes, all on the host (macOS 26.5, R 4.6.1, current dependencies).
- 2026-08-28: candidate row added for the timeout finding — `with_timeout()` taking 191.8 s against a 2 s limit on Linux when the spawned program ignores `SIGTERM`, with the reproducer and what a fix would owe D047's promise.

## Decisions

- **D055** (promoted, `cairn/DECISIONS.md`) — the nine `Imports` floors say what was measured, and `rlang`'s was wrong. Eight stand; `rlang` moves 1.1.0 → 1.2.0. The entry names the runner, the two held-back harness packages and the four things the run did not measure.

## Review

### Acceptance criteria — fresh evidence (2026-08-28)

Every criterion was re-executed at review against the branch as it stands; the
harness was re-run end to end rather than read off the implementation's own
record. Runner: `rocker/r-ver:4.4.3` (R 4.4.3, Ubuntu noble, aarch64) under
colima on macOS 26.5, the repo bind-mounted, the pinned libraries persisted in
a `TM_LIBROOT` volume. Script exit 0.

- **AC1 — met.** `read.dcf("DESCRIPTION", "Imports")` enumerates 11 entries;
  less `tools` and `utils` that is nine carrying a floor: `archive` 1.1.1,
  `cli` 3.4.0, `dplyr` 1.1.0, `glue` 1.6.2, `purrr` 1.0.0, `rappdirs` 0.3.3,
  `rlang` 1.2.0, `tibble` 3.1.4, `withr` 2.5.0. The re-run's pinned child
  reported each of the nine resolving at exactly that version and from
  `/libs/all`, and the suite passed on them. `rlang` is the one floor that
  moved (1.1.0 → 1.2.0); the re-run's comparison block printed `no floor
  moved`, i.e. nothing else needs to.
- **AC2 — met.** Baseline `pass=6120 fail=0 skip=22 over 66 files`; pinned
  `pass=6120 fail=0 skip=22 over 66 files` — the skip counts are equal, not
  merely both small. Both children asserted `ffmpeg` and `mediainfo` on `PATH`
  before starting. The pinned child asserted per pinned package the version
  *and* the directory, up front and again over `loadedNamespaces()` after the
  suite; the baseline child printed the current releases resolving from
  `/usr/local/lib/R/site-library` (archive 1.1.14, cli 3.6.6, dplyr 1.2.1,
  glue 1.8.1, purrr 1.2.2, rappdirs 0.3.4, rlang 1.3.0, tibble 3.3.1, withr
  3.0.3), which is what makes the pinned run's `/libs/all` provenance
  load-bearing. Both runs printed `66 of 69 files run` with the same three
  named exclusions, and the child checks the file set it actually ran against
  the set it intended rather than trusting the `invert` filter.
- **AC3 — met, vacuously on this run and non-vacuously on the record.** On the
  re-run every declared floor installed and built on the runner AC2 names, so
  the Archive walk had nothing to repair. The criterion's procedure is
  exercised on the record it was written for: on the host's R 4.6.1 six floors
  do not compile and `archive` finds no `libarchive`, the errors are recorded
  in the work log and D055, and walking each forward reached the current
  release every time — which is why the measurement moved to an older R rather
  than moving six floors. The `--repair` and `--walk` modes implementing the
  walk are present in `data-raw/imports-floors.R`.
- **AC4 — met.** D055 (`cairn/DECISIONS.md:2426`) names AC2's suite, the R
  version (4.4.3) and the runner OS (Ubuntu noble, aarch64), and states the
  four things not measured: direct `Imports` only with siblings and transitive
  dependencies current; the two held-back packages with the version and the
  requirement that forced each (`testthat` 3.3.2 → 3.1.10, forced by its `cli
  (>= 3.6.5)` and `withr (>= 3.0.2)`; `furrr` 0.4.0 → 0.3.1, forced by its
  `purrr (>= 1.2.1)`) — the re-run printed both holdbacks identically, so the
  entry names what the run actually held; the three excluded files and the
  consequence that nothing the timeout surface does was exercised; no floor run
  alone, and one operating system.
- **AC5 — met.** `NEWS.md` states under Requirements that the declared floors
  are now measured and that `rlang` moved. Fresh on the host (macOS 26.5, R
  4.6.1, current dependencies): `devtools::test()` FAIL 0 / WARN 12 / SKIP 5 /
  PASS 6690; `devtools::check()` **0 errors, 0 warnings, 0 notes**.

### Consistency gate

Universal: `cairn_validate.py` passes (all checks; 70 advisory `work-log
format` warnings, all from the fenced per-file count table embedded in the work
log — advisory, not gate failures). No principle changed, so `cairn_impact.py`
does not apply.

Toolchain (`r-package` profile): `devtools::document()` produces no diff;
`NAMESPACE`, `man/` and `data/` unchanged by the diff; `README.Rmd` and
`README.md` untouched by this milestone and in sync; `pkgdown::check_pkgdown()`
— "No problems found"; `NEWS.md` carries the user-visible entry; no new
top-level files (`data-raw/` is already `.Rbuildignore`d, and `check()` reports
0 notes); `devtools::check()` clean.

Gate outcome: **pass**, no return.

### Independent fresh-context review

Executable surface touched (`data-raw/imports-floors.R`, 754 new lines) and a
user-facing tier, so the full three-lens fan-out ran, each lens on a distinct
evidence base and none having seen the implementation. 22 candidate findings.

**[O] diff-bug lens (Opus) — 18 findings.** Verified correct by that lens and
independently useful: the DESCRIPTION change is right (it fetched the NAMESPACE
of rlang 1.0.6 through 1.1.7 and 1.2.0 from CRAN — none of 1.0.6–1.1.7 export
any of the four, 1.2.0 exports all four); NEWS's "132 places" is exact
(46+36+38+12, all namespace-qualified); the exclusion filter, the binary
assertion, the pre- and post-suite provenance checks, the `TM_MODE` refusal,
the exact-equality skip comparison and the child-exit checks all fire as
claimed and none can silently no-op.

**[S] blame-history lens (Sonnet) — 0 defects.** `rlang (>= 1.1.0)` was set in
the M01 modernization commit as a routine bump and never measured; raising it
undoes no deliberate decision. The three excluded timeout files resurrect
nothing — D047 already discloses base R's escalation ladder; what M077 found is
narrower and new, and went to a candidate row rather than being dropped. The
new script repeats none of the rough edges `withr-floor.R`/`r-floor.R`'s
reviews named. It flagged, as a process note, that both criteria amendments had
their audits run in-session rather than by a fresh-context reader, disclosed at
the time.

**[S] prior-review lens (Sonnet) — 1 finding.** No inline PR review comments
exist on this repo (probe returned empty), so the archive was the only surface,
as in M076.

**Findings, disposition, and where each landed.**

| # | Lens | Finding | Disposition |
|---|---|---|---|
| F1 | O | `data-raw/imports-floors.R:208-210` claims the `-Wno-error=format-security` Makevars change "is disclosed in the D-entry"; D055 said nothing about it, so `archive` 1.1.1 and `rlang` were built with a hardening error demoted and no record said so | **fixed now** — D055 now states the one flag changed, why, and that a user compiling those versions on a hardened distro does hit the errors |
| F2 | O | The move loop can raise a pin above the declared floor, and the closing line printed "the declared floors load, and the suite passes on them" unconditionally — reporting a version nobody declared as evidence for the one that is written down | **fixed now** — the closing line is now conditional and says the pinned set is not the declared set, with the moves to apply |
| F3 | O | D055's "Pinned at the declared `rlang` 1.1.0" is not reproducible from the committed harness: `vctrs` is in the runtime closure and requires `rlang (>= 1.1.7)`, so the reconciliation step would move 1.1.0 to 1.1.7 before any install | **fixed now** — D055 now says the 1.1.0 pin was direct, ahead of that step, and that 1.1.7 is equally short of all four exports |
| F4 | S(prior) | `sub <- ...` at `:634` and `:650` shadows base `sub()` — the verbatim pattern M076's review named (F8) and the ROADMAP row instructs the next milestone to sweep | **fixed now** — renamed to `req` throughout both loops |
| F5 | O | `install_pin` passes `env=` values unquoted (`:180`) while `run_child` carefully `shQuote`s — a `TM_LIBROOT` containing a space silently drops `R_LIBS` from the install | **fixed now** — both values now `shQuote`d, with the reason named |
| F6 | O | D055's NAMESPACE walk is stated as exhaustive but skipped rlang 1.0.1, 1.0.3, 1.0.5 | **fixed now** — all three fetched from the Archive at review and confirmed to export none of the four; D055's list is complete and now says so |
| F7 | O | An install failure cascades: the loop continues, so `archive`/`purrr` compile against the user library's `cli` headers, and `--repair` walks only the failed package forward without reinstalling its LinkingTo dependents | follow-up — `Imports`-floors candidate row |
| F8 | O | The library-reuse guard keys on `Version` only, so a persisted `TM_LIBROOT` can keep binaries compiled against superseded headers after a floor is raised | follow-up — same row; the *half-written-install* half of this trap is closed, the staleness half is not |
| F9 | O | `fetch_tarball`'s cache short-circuit (`:124`) returns before the `untar`/`DESCRIPTION` validation directly beneath it, so a truncated download in a persisted `TM_SCRATCH` is reused forever | follow-up — same row; this is the enumerated 1000-byte-heuristic shape |
| F10 | O | The hold-back set is "every installed package outside the runtime closure with an unmet requirement", not "the test harness" as D055 item 2 describes; run against a developer's own library it can downgrade unrelated packages and abort the measurement | follow-up — same row |
| F11 | O | The move loop's `for (round in 1:5)` exits silently on non-convergence, skipping the holdback branch entirely | follow-up — same row |
| F12 | O | `archive_versions` turns a network failure into "no later versions exist" (`:404`), so `--repair` could jump a floor straight to today's release | follow-up — same row |
| F13 | O | `R CMD INSTALL --no-test-load` means "installs" does not mean "loads", so an unresolved symbol at dlopen is recorded as a successful install | follow-up — same row |
| F14 | O | `--only X --walk Y` bypasses the walk's name guard (`:420`) and errors with an unrelated message | follow-up — same row |
| F15 | O | The per-file table and TOTALS line print failures but not errors; the child still `stop()`s on either, so the control holds, but the transcribed table reads stronger than it is | follow-up — same row |
| F16 | O | `BASE_PKGS` uses `priority = c("base", "recommended")`, wider than AC1's carve-out of `tools` and `utils` | follow-up — same row |
| F17 | O | A `~` in `TM_LIBROOT` reaches `R CMD INSTALL -l` inside single quotes; the lens could not verify whether R path-expands internally | follow-up — same row, carrying the unverified note |
| F18 | O | Dead `probe <- file.path(LIBROOT, "walk")` at `:681` with a comment describing behaviour that does not exist | follow-up — same row |
| F19 | O | NEWS says "the package's test suite has been run" without naming the three-file exclusion | **rejected** — the plan gate deliberately split this: NEWS carries the user-visible fact, D055 carries the per-floor what-ran/what-did-not, and that split is a recorded decision, not an oversight. Surfaced here rather than silently dropped |
| F20 | S(blame) | Both criteria amendments had their audits run in-session rather than by a fresh-context reader | **rejected** — a disclosed constraint of the implementing session, recorded in the work log at the time, not a defect in the diff |
| F21 | O | The measurement container is an ad-hoc image (`tidymedia-floors:r443`) built from no committed Dockerfile, so D055's runner is not reproducible from the repo alone | follow-up — same row; raised by the reviewer at this gate |
| F22 | O | `install_order` is computed once and not recomputed after a `--repair` walk | follow-up — same row (the recompute half of F7) |

Sixteen findings deferred; all land on the existing `Imports`-floors candidate
row, which already carries the sweep-both-scripts instruction — this extends it
a fifth time, disposed explicitly per records-hygiene §7 rather than silently.

**Return floor.** No actioned finding demonstrates an acceptance criterion
failing. F1, F3 and F6 are accuracy gaps in the record AC4 binds and were fixed
at the gate rather than returned, since AC4's enumeration — the three things
not measured — is stated correctly and completely in D055 either way. No
finding is a load-bearing defect in what the package does for its users: every
one of F7–F18 and F21–F22 is in a developer-only script `.Rbuildignore` keeps
out of the built package. Status stays `review`; no return.

**Re-verification after the fix-now edits.** The harness was re-run end to end
after F1–F5's code and record edits were applied. The script parses; the run
exits 0 and reproduces the measurement exactly — baseline and pinned both
`pass=6120 fail=0 skip=22 over 66 files`, `no floor moved`, the same two
holdbacks — so the evidence recorded above stands against the code as merged,
not only against the code as measured.

