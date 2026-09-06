# M113: One call says which programs tidymedia found, and a remembered location can be forgotten

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — two new exports
- **Branch/PR:** `m113-program-status-and-unset` — https://github.com/jmgirard/tidymedia/pull/117

## Goal

Export `program_status()`, which reports the resolved path and reported version
of all four programs in one call, and `unset_program()`, which removes a
remembered location `set_program()` wrote.

## Scope

**In:** `program_status()` returning one row per program for `ffmpeg`,
`ffprobe`, `ffplay` and `mediainfo`; `unset_program(program)` removing the
`<program>_location.txt` file `set_program()` wrote (`R/program_management.R:20`)
and, where one exists, the pre-M097 `rappdirs` file `find_program()` still
reads (`:59-101`); both wired into `_pkgdown.yml`, `NEWS.md` and the program
management family.

**Out:** four `unset_*()` wrappers mirroring the `set_*()` exports — one
generic is the reversible default; the wrappers become a candidate row.
Migrating or rewriting the legacy file → nothing here changes what
`find_program()` reads, only what exists for it to read. A dispatching
`install_ffmpeg()` → declined at M108's gate, unchanged. Version probes for
`ffplay` and `mediainfo` beyond what each binary's own version flag answers.
Teaching either function in a vignette → M114.

## Acceptance criteria

- [x] AC1: `program_status()` returns a tibble with one row for each of
      `ffmpeg`, `ffprobe`, `ffplay` and `mediainfo`, carrying the resolved path
      or `NA` and the version string the binary reported or `NA`. Evidence: the
      printed tibble in two states — every program present, and none resolvable
      (an emptied `PATH` AND a redirected empty config dir, since `PATH` alone
      leaves `find_program()`'s config and legacy lookups intact).
- [x] AC2: `program_status()` returns that tibble rather than aborting or
      warning when a program is unresolvable, for each of the four taken alone.
      Evidence: four runs, one program hidden per run.
- [x] AC3: When a location is remembered for a program and `unset_program()`
      then returns `TRUE` for it, no file named `<program>_location.txt` exists
      under either the current config dir or the legacy `rappdirs` dir, and
      `find_program()` answers as it did when nothing was remembered: `NULL`
      with its `Failed to find` warning, under the emptied `PATH` the walk runs
      under. Evidence: a test walking each of the three remembered
      configurations (current dir only, legacy dir only, both) from never
      remembered through remembered to forgotten, run for each of the four
      programs, showing both file paths and the `find_program()` answer at each
      step.
- [x] AC4: `unset_program()` called for a program with no remembered location
      raises a classed condition carrying a `tm_program` field, per D062's
      naming rule and D086's field rule, rather than failing silently or
      unclassed. Evidence: the class vector and the message.
      (RB tripwire: irreversible-api)
- [x] AC5: Both exports appear in `_pkgdown.yml`, carry `@examplesIf`-guarded
      examples where they touch a binary, join `@family program management
      functions`, and have a `NEWS.md` entry.
- [x] AC6: `devtools::document()` produces no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings).

## Coverage

- AC1 → T2, T3
- AC2 → T3
- AC3 → T4, T5
- AC4 → T4, T5
- AC5 → T6
- AC6 → T6, T7

## Tasks

- [x] T1: Settle the two names against D014/D078 before writing either body:
      `program_status()` and `unset_program()` pair with the family's existing
      `find_program()`/`set_program()`, and `get_*` is reserved for per-file
      metadata scalars. (RB tripwire: irreversible-api)
- [x] T2: Build `program_status()` over the internal `tool_versions()` shape
      (`R/ffm_manifest.R:121`), widened from two programs to four; a program
      with no version flag answer contributes `NA`, not an abort.
- [x] T3: Tests for AC1 and AC2, including the both-hidden state — emptied
      `PATH` plus a redirected `R_USER_CONFIG_DIR` — and one run per program.
- [x] T4: `unset_program()`: remove the current-dir file and any legacy file,
      report a classed refusal when neither exists.
- [x] T5: Tests for AC3 and AC4 across the three states and both config
      locations. The M097 lesson holds: one `R_USER_CONFIG_DIR` collapses the
      two directories together on Windows, so redirect the legacy library by a
      recording mock rather than by the envvar.
- [x] T6: Roxygen, `_pkgdown.yml` rows, `NEWS.md`, `document()`, `test()`,
      `check()`.
- [x] T7: Discovered at review — give the timeout sweeps a gate for a domain
      member whose spawning is conditional on the machine, so the grid stops
      reading the runner's `PATH` through `program_status()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned three findings against this milestone's draft: the absent-program evidence used an emptied `PATH`, which `find_program()`'s config and legacy fallbacks defeat; the forget criterion compared against a pre-`set_program()` state that need not exist and named one config file where two can hold a location; and the criteria said "a new exported function" without naming it, so nothing bound them to one pair. All three fixed before writing; none needed a gate question.
- 2026-09-05: plan gate chose one generic `unset_program()` over four `unset_*()` wrappers mirroring the `set_*()` exports, because the wrappers are additive later and four exports is the larger trade against GP1. Falsified by a report that the asymmetry with `set_ffmpeg()` and its siblings is what a caller trips on.
- 2026-09-05: plan gate chose widening the internal `tool_versions()` (`R/ffm_manifest.R:121`) over a fresh probe, because it already spawns `-version` for two of the four programs and is the shape the manifest records. Falsified by a program whose version flag that helper's parsing cannot read.
- 2026-09-05: T2/T4 — `program_status()` and `unset_program()` written in `R/program_management.R`; `tool_versions()` widened with a `programs`/`locations` pair so the manifest's own two-program call is unchanged, and `parse_version_line()` gained MediaInfo's `--version` shape, which names no "version" token.
- 2026-09-05: T3/T5 — tests in `tests/testthat/test-program-status-and-unset.R`; the three fixtures the family shares moved from `test-program-management.R` to `helper-program-config.R` so a second test file can see them.
- 2026-09-05: T6 discovery, minor amendment — `program_status()` joins the computed timeout domain (it spawns), and the M094 blame sweep found it raising nothing for an invalid `options(tidymedia.timeout = )`: `capture_version()`'s `tryCatch()` turned that refusal into a silent NA. `tool_versions()` now resolves the limit once above the probes and threads `call`, so the refusal blames `program_status()`. Registered in `tm_timeout_recorded_domain()`, the call-spec table and the absorber partition; the two written-membership censuses measured at `ae5ff1c` get a named `tm_timeout_post_baseline()` exclusion, since a function that did not exist then has no pre-change reading to record.
- 2026-09-05: T6 second sweep finding — the M096 wrong-argument sweep asserts its table covers the whole timeout domain, and `program_status()` takes no arguments, so it can contribute no cell. `tm_timeout_argumentless()` computes that set and the test pins it, so a second argumentless export reddens there rather than vanishing from the sweep.
- 2026-09-05: T6 — roxygen, `_pkgdown.yml` rows, `NEWS.md`; `devtools::document()` no diff, `pkgdown::check_pkgdown()` clean, `devtools::test()` 12,843 pass / 0 fail / 18 skip, `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-09-05: implement question gate settled four choices — both names as planned, the three-column shape, a warning rather than an abort for nothing-to-forget, and no `confirm` argument. Recorded below; T1 closed by the naming half.
- 2026-09-05: review (wip) — PR #117 opened as a draft; AC1-AC5 verified with fresh evidence and ticked. AC6 pending `devtools::check()`; three review lenses spawned, the blame-history and prior-review lenses reported zero findings, the diff-bug lens still running.
- 2026-09-05: review — PR #117; AC1, AC2, AC4, AC5 and AC6 verified with fresh evidence and ticked. `cairn_validate` clean, toolchain consistency gate clean, `check()` 0/0/0. Three review lenses ran; the two Sonnet lenses returned zero findings, the [O] diff-bug lens returned ten, six reproduced against the branch head.
- 2026-09-05: amendment return: AC3 — "`find_program()` returns what it returned before the `set_program()` call — including `NULL` with its warning where that is what it returned". Falsified only outside the domain of the walk AC3's evidence clause names, by a legacy file predating the `set_program()` call; contradicts this milestone's own Scope, which asks for both files to be removed. Status to `in-progress` for the amendment alone; AC3 unticked.
- 2026-09-05: review — CI on PR #117 red on macos-latest and windows-latest, green on all four Linux legs and pkgdown. Both red legs fail `test-timeout-silence.R:351` and `test-timeout-refusal-blame.R:425` on `program_status`, which spawns nothing on a runner with no binaries installed, so a forced limit is never reached. Reproduced locally under an emptied `PATH` with both config dirs redirected. Carried into the amendment return's work.
- 2026-09-05: AC3 amended at the implement mini gate, executing the review return. The falsified clause "`find_program()` returns what it returned before the `set_program()` call" is replaced by "`find_program()` answers as it did when nothing was remembered: `NULL` with its `Failed to find` warning, under the emptied `PATH` the walk runs under"; the antecedent now names a successful `unset_program()` rather than `set_program()`'s write, and the evidence names the three remembered configurations x four programs. No second `amendment return: AC3` line is written: the review's line of that shape already records this single return, and a second would read as a second return. The same false promise is fixed in `?unset_program`, its example, and the `NEWS.md` bullet.
- 2026-09-05: re-audit: AC3 (full) — returned three findings against the mini gate's wording: the promise held over a `unset_program()` run that aborts with the file still on disk (F6's state, pinned at `test-program-status-and-unset.R:307`); the `program` axis was free while the evidence enumerated only states x locations; and the antecedent named `set_program()` as the writer, which cannot instantiate the legacy arm of its own walk, there being no exported writer for that directory. All three fixed before writing.
- 2026-09-05: re-audit: AC3 (full) — the once re-entry, on the fixed wording. Returned two findings: the `PATH`-has-the-program arm has no reachable cell in the named walk, which empties `PATH` so the other states can assert the `Failed to find` warning; and the antecedent ranges over three remembered configurations while the evidence named two, the both-present one being where the defect was found. Both confirmed against `test-program-status-and-unset.R:203-244` and `helper-program-config.R:41-43`, taken to the user as further churn, and applied on selection.
- 2026-09-05: T7 — the two timeout sweeps went red on the macOS and Windows CI legs because `program_status()` probes a version only for a program it resolved, so on a runner with no binaries it spawns nothing and a forced limit is never reached. `tm_force_timeout()` now intercepts resolution at `find_program()` as well as the two spawn wrappers, and a new grid case runs the whole domain under an emptied `PATH` with both config dirs empty. Discrimination measured: with the resolver mock removed the new case fails on `program_status` while the grid above it stays green on this machine.
- 2026-09-05: amendment work complete; status to `review`. `devtools::test()` 0 failures / 12,897 pass / 18 skip (54 more assertions than the pre-amendment run, which is the new grid case plus its text assertion); `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes, 15m31s; `devtools::document()` no diff. AC3 stays unticked for re-review to re-evidence against the amended wording; the nine review findings other than F4 stay open for the maintainer at the re-review gate.
- 2026-09-06: re-review (wip) — AC3 re-evidenced against the amended wording and ticked; AC1, AC2, AC4 and AC5 re-run fresh against branch head `8baf7ac` and recorded. `cairn_validate` exit 0, every check PASS, no advisory. AC6's `devtools::check()`, the three review lenses and CI on PR #117 still running.
- 2026-09-06: re-review triage — finding 1 (the `unset_program()` default, against D079) and findings 7, 11, 12, 14 fixed now on the branch at the maintainer's direction; findings 2, 3, 4, 5, 6, 8, 9, 10 and 13 go to candidate rows at hygiene; F8 stays rejected. All six criteria pass with fresh evidence, `cairn_validate` exit 0, toolchain gate clean, and CI at `8baf7ac` green on all eight check legs including the two that were red at `fa58566`.

## Decisions

### The two names, and what `unset_program()` does at its two edges (2026-09-05, implement question gate)

`program_status()` and `unset_program()` ship as planned. Both name a category
rather than one member of an open set, and neither takes the `get_*` prefix the
package reserves for per-file metadata scalars; the pair reads against the
family's existing `find_program()` / `set_program()`, which is what a reader
guesses from. `program_versions()` was declined because the tibble also carries
the resolved location, and `forget_program()` because it breaks the set/unset
pairing.

The tibble is three columns: `program`, `location`, `version`, one row per
program, `NA` in either value column where there is no answer. The path column
is `location` because that is what `set_program(location =)` and
`find_program()` already call it. A fourth `found` logical was declined: it
restates `is.na(location)`.

`unset_program()` on a program with nothing remembered warns and returns
`FALSE` invisibly rather than aborting. The end state the caller asked for
already holds, so a repeated call is a no-op and not a failure; this is how
`find_program()` reports a program it cannot find. The warning is classed
`tidymedia_no_remembered_location` — the event, not the severity — and carries
`tm_program`.

`unset_program()` takes no `confirm` argument. `set_program()` asks because
writing a file that outlives the session is a side effect of a call named
"set"; deleting that file is the whole of what `unset` names, so a prompt would
ask the caller to confirm the thing they typed.

- **Falsified by** a caller who calls `unset_program()` by mistake and cannot
  recover the path it forgot, or by a handler that needs the nothing-remembered
  case to stop a script.

## Review

### Acceptance criteria

- AC1 — **pass.** Two states run against the branch head (`Rscript` over `pkgload::load_all()`, 2026-09-05). All present: a 4x3 `tbl_df` with `ffmpeg`/`ffprobe`/`ffplay` at `/opt/homebrew/bin/*` version `9.0.1` and `mediainfo` at `/opt/homebrew/bin/mediainfo` version `26.05`. None resolvable — `PATH=""` plus `R_USER_CONFIG_DIR` pointed at an empty dir plus `tm_legacy_config_dir()` mocked to an empty dir, all three as the criterion requires: the same four rows with `NA` in both `location` and `version`. `devtools::test(filter = "program-status-and-unset")` covers both states, 204 assertions, 0 failures.
- AC2 — **pass.** Four runs, one program hidden per run: a temp `PATH` dir holding symlinks to the other three, an empty config dir and an empty mocked legacy dir. Each run returned the 4x3 tibble with the hidden program `NA`/`NA` and the other three resolved and versioned; a `withCallingHandlers` collar on `warning`, `message` and `error` caught nothing in any of the four.
- AC3 — **pass.** The three-state walk run twice, once per config location, on `mediainfo` under an emptied `PATH`. Current dir: never remembered — both `mediainfo_location.txt` paths absent, `find_program()` `NULL` with "Failed to find mediainfo."; remembered — current file present, legacy absent, `find_program()` returns the stub path; forgotten — `unset_program()` returned `TRUE`, both files absent, `find_program()` `NULL` with the same warning. Legacy dir: identical, with the presence flag on the legacy file instead. The suite's own walk (`test-program-status-and-unset.R`) additionally covers both-files-present and the leave-the-other-programs-alone case.
- AC4 — **pass.** For each of the four programs with neither config file present, `unset_program()` signalled a warning whose class vector is `tidymedia_no_remembered_location` / `rlang_warning` / `warning` / `condition`, carrying `tm_program` equal to the program name, message `No remembered location to forget for <program>.` with the hint `Use \`set_<program>()\` to remember one.` The suite also pins that the warning's `call` blames `unset_program()`.
- AC5 — **pass.** `_pkgdown.yml:126` and `:129` carry `program_status` and `unset_program`; `pkgdown::check_pkgdown()` reports "No problems found." Both carry `@family program management functions`. `program_status()` touches binaries and its example is `@examplesIf nzchar(Sys.which("ffmpeg"))`; `unset_program()` touches no binary and its example is `\dontrun{}`, deleting a real user's remembered location being the thing an unguarded example would do. `NEWS.md` has one entry each under "New features".
- AC6 — **pass.** `devtools::document()` re-run on the branch head left `NAMESPACE`, `man/` and `_pkgdown.yml` untouched (`git status` showed only the milestone file). `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes, 5m39s, its own `checking tests ... OK`. A separate `devtools::test()` run over the whole suite finished with no `F` or `E` marker on any progress line and one Skipped block.

### Re-review evidence (2026-09-06, branch head `8baf7ac`)

The amendment return set status back to `in-progress` for AC3's amendment
alone; the amendment landed at `cdfd779`/`8baf7ac` and every criterion is
re-evidenced here against that head.

- AC3 — **pass** (re-evidenced against the amended wording). The three
  remembered configurations x four programs = twelve walks, each through
  never remembered -> remembered -> forgotten, run against the branch head
  (`Rscript` over `pkgload::load_all()`, 2026-09-06) under `PATH=""` with
  `R_USER_CONFIG_DIR` redirected and `rappdirs::user_config_dir()` mocked to a
  separate legacy root. Current-dir configuration (written through
  `set_program(confirm = FALSE)`): never — both files absent, `find_program()`
  `NULL` with "Failed to find <program>"; remembered — current file present,
  legacy absent, `find_program()` returns the stub, no warning; forgotten —
  `unset_program()` returned `TRUE`, both files absent, `find_program()` `NULL`
  with the same "Failed to find" warning. Legacy-dir configuration: identical,
  with the presence flag on the legacy file. Both-present configuration:
  remembered state has both files present and `find_program()` answering with
  the current file's stub; after `unset_program()` returned `TRUE` both files
  are absent and `find_program()` is `NULL` with the "Failed to find" warning —
  the state F4 falsified the old wording in, and the state the amended clause
  now covers. All twelve walks identical in shape; no walk left a file behind
  and no walk returned a non-`NULL` answer in the forgotten state.
- AC1 — **pass**, re-run. All present: 4x3 `tbl_df`, `ffmpeg`/`ffprobe`/`ffplay`
  at `/opt/homebrew/bin/*` version `9.0.1`, `mediainfo` version `26.05`. None
  resolvable (`PATH=""` + empty redirected config dir + mocked empty legacy
  dir): the same four rows, `NA` in both `location` and `version`.
- AC2 — **pass**, re-run. Four runs, one program hidden per run behind a temp
  `PATH` dir holding symlinks to the other three, with both config dirs empty.
  Each returned the 4x3 tibble with the hidden program `NA`/`NA` and the other
  three resolved and versioned; a `withCallingHandlers` collar on `warning`,
  `message` and `error` caught nothing in any of the four.
- AC4 — **pass**, re-run. Each of the four programs, neither config file
  present: class vector `tidymedia_no_remembered_location` / `rlang_warning` /
  `warning` / `condition`, `tm_program` equal to the program name, message
  `No remembered location to forget for <program>.` with the
  `Use \`set_<program>()\` to remember one.` hint.
- AC5 — **pass**, re-run. `_pkgdown.yml:126` and `:129`;
  `pkgdown::check_pkgdown()` "No problems found."; both carry
  `@family program management functions`; `program_status()`'s example is
  `@examplesIf nzchar(Sys.which("ffmpeg"))` and `unset_program()`'s is
  `\dontrun{}`; `NEWS.md` carries one entry each under "New features".
- AC6 — **pass**, re-run at the branch head. `devtools::document()` produced no
  diff (`git status` clean afterwards). `devtools::check()`: `Status: OK`, 0
  errors / 0 warnings / 0 notes, 15m49s, its own `checking tests ... OK`.

### Independent review (re-review, 2026-09-06)

Surface tier user-facing, so the full three-lens fan-out ran again against the
amended head, each lens fresh-context with a distinct evidence base.

- **[S] prior-PR-comments** — zero findings. Archived `## Review` sections are
  the primary surface; the probe
  (`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`,
  so the per-PR walk was skipped. Checked and clear against M097's Windows
  config-collapse trap, M067/D044's memo discard, M110/M112's `call` threading
  and M094/M096's computed-domain rule.
- **[S] blame-history** — no new findings. It independently re-derived F1 as
  its top item against D079, restated F2/F3/F5/F6/F7/F9, and confirmed as
  sound: the `call`-threading shape against D087, the `tm_force_timeout()`
  resolution-seam fix as a real CI-cause fix rather than weakened coverage, the
  guarded `tm_timeout_argumentless()`/`tm_timeout_post_baseline()` narrowings,
  the byte-identical fixture move, and `parse_version_line()`'s new arm firing
  only after the old one finds nothing.
- **[O] diff-bug** — fourteen findings, ranked. Six reproduce the prior pass's
  F1, F2, F3, F5, F6 and F9 against the current head; six are new; F4 is
  resolved by the amendment and F8 stays rejected.

#### Findings and dispositions (re-review)

Numbering is the re-review lens's; the prior pass's label follows where one
exists.

1. **(F1) `unset_program()` called with no arguments deletes ffmpeg's
   remembered location** (`R/program_management.R:236`). Reproduced. The
   exported formal defaults to the four-member vocabulary and `arg_match()`
   resolves it to `"ffmpeg"`, so a call naming nothing performs the package's
   only irreversible filesystem deletion. D079's rule reaches it — the
   pre-0.2.0 window is open (`Version: 0.1.0.9000`) and neither carve-out
   applies — and `find_program()`/`set_program()` carry the same default while
   being read-only and consent-gated.
2. **(F2) `program_status()` dies with an unclassed base error on a malformed
   config file** (`:174`). Reproduced: an empty `ffplay_location.txt` gives
   `Error: argument is of length zero`, a two-line one
   `Error: the condition has length > 1`, both `simpleError`. The fault is
   `find_program()`'s and predates the branch, but `program_status()` reads all
   four config files on every call. `count_audio_streams()`
   (`R/ffprobe.R:213-219`, from M44 review F2) already defends against exactly
   this with a `tryCatch`.
3. **(F6) A partial removal aborts with the capability memo left stale**
   (`:270-291`). Reproduced: the `cli_abort` sits above
   `forget_ffmpeg_capabilities()`, so a removal that takes one file and not the
   other leaves `find_program()` answering with a different binary than the
   memo describes — the drift D044/M067 exists to prevent.
4. **(F5) `program_status()` reports `NA` for a stale remembered location and
   swallows the warning that explains it** (`:174`). Reproduced. `NA` covers
   both "never configured" and "configured at a path that is gone", the second
   being the state `unset_program()` exists to repair.
5. **(F3) The probe-timeout warning tells a `program_status()` caller about a
   manifest** (`R/ffm_manifest.R:154-166`). Reproduced. There is no manifest on
   this path, and the bullet lists display labels (`FFmpeg`) rather than the
   `program` column's values (`ffmpeg`).
6. **NEW — `program_status()` is permanently excluded from the byte-for-byte
   spawn-trace baseline with no substitute pin**
   (`tests/testthat/helper-timeout-sweep.R:147-177`). Read, not run. The
   exclusion is documented and `stopifnot`-guarded, and the live rules still
   sweep the member; what has no present-day reading is its spawn trace.
7. **(F9) NEWS overclaims silence** (`NEWS.md:6-11`, "It warns about
   nothing"). Reproduced through finding 5.
8. **NEW — `program_status()` resolves through `find_program()` directly,
   walking past the mock seam its sibling exists to respect** (`:174` against
   the comment at `R/ffm_manifest.R:183-186`). Read. This is why T7's fix had
   to add a `find_program` mock.
9. **(F7) `tool_versions()` reassigns its own `programs` argument inside the
   warning branch** (`R/ffm_manifest.R:154`). Read. Correct only because
   nothing below reads it.
10. **NEW — `tool_versions()` does not check that `locations` matches
    `programs` in length** (`R/ffm_manifest.R:143-149`). Read; `Map` recycles
    silently, so a short `locations` would relabel every row.
11. **NEW — `?program_status` describes a lookup order that omits the legacy
    directory** (`:167-169`), where `?find_program` and `?unset_program` both
    name the pre-0.2.0 `rappdirs` file.
12. **NEW — the config-redirect helper's comment claims a recording mock that
    records nothing** (`tests/testthat/helper-program-config.R:32-45`); T5's
    task text repeats the claim.
13. **(F10) `program_status()` joins the absorber partition with no
    explanatory paragraph** (`tests/testthat/test-timeout-silence.R:58-63`),
    where every other member carries one.
14. **NEW — `expect_invisible(unset_program("ffmpeg"))` asserts invisibility
    but not the return value** (`test-program-status-and-unset.R:243`), in the
    test named "returns TRUE invisibly".

**(F8) stays rejected** — dropping the memoized FFmpeg capabilities on
`unset_program("mediainfo")` is an intentional change the code comment records,
matching `set_program()`'s documented choice.

**Return floor.** No finding demonstrates an acceptance criterion failing
inside the domain of the procedure that criterion names: F2 falls outside AC2's
named four-runs-one-hidden procedure, F9 and finding 11 fall outside AC5, which
asks for a NEWS entry and a pkgdown row rather than for their wording, and AC1,
AC3, AC4 and AC6 are unaffected. So no floor return fires from the findings
themselves; finding 1 is the one put to the maintainer as a possible
load-bearing defect.

**Dispositions taken at the re-review gate (2026-09-06, maintainer's call).**

- **Finding 1 — fix now.** `unset_program(program)` loses its default; the
  argument is required, so `unset_program()` naming nothing refuses
  (`rlang_error`, "`program` must be a character vector, not absent") and
  deletes nothing. Verified by execution against the fixed head: two config
  files on disk before the no-argument call, the same two after it. D079's
  reasoning applies as written — a default can be added later compatibly, and
  cannot be removed after 0.2.0 without a deprecation cycle. The test that
  pinned the default now pins its absence, alongside the vocabulary
  `arg_match()` still accepts; `?unset_program`'s `@param`, the `NEWS.md`
  bullet, and `tm_programs()`'s comment say the argument is required and why.
- **Findings 7, 11, 12, 14 — fix now** (prose and test claims, no behavior
  change). 7: the NEWS line "It warns about nothing" is replaced by "A program
  it cannot find is reported rather than warned about". 11: `?program_status`
  now names the pre-0.2.0 `rappdirs` file as the third step of the lookup.
  12: the helper comment drops the "records what the helper asked it for"
  claim for what the mock actually does. 14: the invisibility assertion is
  wrapped so the `TRUE` the test's own name promises is asserted too. T5's task
  text repeats 12's claim and is left as written — it is a completed task
  record, and the correction belongs in the code it describes.
- **Findings 2, 3, 4, 5, 6, 8, 9, 10, 13 — follow-up**, filed as candidate rows
  at the post-merge hygiene pass. None fails an acceptance criterion, and each
  needs a design call rather than a patch: 2/4/5 change what
  `program_status()` reports about a broken configuration, 3 changes where the
  memo drop sits relative to the partial-failure abort, 6/8/13 change how the
  timeout sweeps cover a conditionally-spawning member, and 9/10 are internal
  `tool_versions()` hygiene with one caller.
- **Finding 8 of the prior pass (F8) — reject**, unchanged.

Post-fix verification at `7a7d655`: `devtools::test()` 0 failures / 12,900
pass / 18 skip; `devtools::document()` re-run, `man/program_status.Rd` and
`man/unset_program.Rd` regenerated in the same commit, working tree clean
afterwards; `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes,
14m41s; `cairn_validate.py` exit 0 with every check PASS and no advisory;
`pkgdown::check_pkgdown()` "No problems found."

### PR conversation (read once, 2026-09-06, before the merge gate)

`reviews` empty; `reviewThreads` filtered to `isResolved: false` returned none,
with `hasNextPage` false; one conversation comment.

- conversation: codecov[bot] PR #117 — noted. Patch coverage 97.18%, two lines
  in the change not covered. Author `type` is `Bot`, so it does not touch the
  chip; the `r-package` profile's test-doctrine makes coverage reporting
  diagnostic-only and never a merge gate, and `codecov/patch` and
  `codecov/project` are the only two non-passing statuses on the PR for that
  reason.

### CI (final head `8baf42c`)

All eight check legs pass: `macos-latest (release)` 11m17s,
`windows-latest (release)` 14m33s, `ubuntu-latest` release 21m0s / devel 22m58s
/ oldrel-1 21m51s / 4.1.0 21m21s, `pkgdown` 2m25s, `test-coverage` 20m42s.

### Consistency gate (re-review)

`cairn_validate.py` exit 0, all 16 PASS and all 7 advisories OK — the
`release window` advisory did not fire. No `DESIGN.md` principle changed, so
`cairn_impact.py` is skipped. Toolchain half, per the `r-package` profile's
`consistency-gate` slot: `document()` no diff; no hand-edited generated file;
`README.Rmd`/`README.md` untouched by the branch; `pkgdown::check_pkgdown()`
"No problems found."; `NEWS.md` carries an entry per export with no milestone
number; no new top-level file, so no `.Rbuildignore` entry is owed;
`devtools::check()` clean.

### CI (re-review, commit `8baf7ac`)

The two legs that were red at `fa58566` are green: `macos-latest (release)`
pass 9m55s, `windows-latest (release)` pass 15m43s. Also green:
`ubuntu-latest (release)` 17m52s and `pkgdown` 2m22s. So the T7 sweep fix holds
on both platforms the reproduced cause named.


### Consistency gate

`cairn_validate.py` exit 0, every check PASS, no advisory fired (release window
included). No `DESIGN.md` principle changed, so `cairn_impact.py` is skipped.
Toolchain half, per the `r-package` profile's `consistency-gate` slot:
`document()` no diff; no hand-edited generated file (the no-diff run is what
says so); `README.Rmd`/`README.md` untouched by the branch and last committed
together at `5f171d9`; `pkgdown::check_pkgdown()` "No problems found."; `NEWS.md`
carries an entry per export with no milestone number in it; no new top-level
file, so no `.Rbuildignore` entry is owed; `devtools::check()` clean.

### Independent review

Surface tier is user-facing, so the full three-lens fan-out ran, each lens
fresh-context with a distinct evidence base.

- **[S] blame-history** — zero findings. Traced the modified lines back to the
  commits that wrote them: `tool_versions()`'s zero-arg default preserves
  `ffm_batch()`'s call, `capture_version()`'s `absorb_timeout()`-inside-
  `tryCatch()` shape is untouched, `parse_version_line()`'s new arm fires only
  after the old one finds nothing, and the four fixtures moved to
  `helper-program-config.R` are byte-identical with no assertion dropped.
- **[S] prior-PR-comments** — zero findings. Archived `## Review` sections are
  the primary surface; the GitHub probe
  (`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`,
  so the per-PR walk was correctly skipped. Checked and clear against M097's
  Windows config-collapse trap, M067/D044's memo discard, M110/M112's `call`
  threading, and M094/M096's computed-domain rule.
- **[O] diff-bug** — ten findings, ranked. Each of the six that make a
  behavioral claim was reproduced against the branch head before triage; the
  reproductions are below with the dispositions.

#### Findings and dispositions

- **F1 — `unset_program()` with no arguments deletes ffmpeg's remembered
  location.** CONFIRMED by execution: with `ffmpeg_location.txt` and
  `mediainfo_location.txt` on disk, `unset_program()` returned `TRUE` and left
  only `mediainfo_location.txt`. The exported formal defaults to the four-member
  vocabulary and `arg_match()` resolves a full permutation to its first element.
  D079's rule reaches it — an argument added to an exported function inside
  D014's window takes no default where every candidate default is one member of
  the set it ranges over — and the milestone's own recorded reason for declining
  `confirm` ("a prompt would ask the caller to confirm the thing they typed")
  does not hold for a call that types nothing. `find_program()`/`set_program()`
  carry the same default but are read-only and write-with-consent. The default
  is pinned by a test, so it was chosen, not overlooked; it was not weighed
  against D079. **Disposition: open, for the maintainer at the re-review gate.**
- **F2 — `program_status()` aborts with an unclassed base error on a malformed
  config file.** CONFIRMED: an empty `ffplay_location.txt` gives
  `Error: argument is of length zero`; a two-line one gives
  `Error: the condition has length > 1`; both `simpleError`. The fault is
  `find_program()`'s `if (Sys.which(location) == "")` on a raw `readLines()` and
  predates this branch, but `program_status()` now reads all four config files
  on every call, so the diagnostic a user reaches for to find a broken setup is
  the one that dies on it. Outside AC2's named procedure (four runs, one program
  hidden per run, each hidden by an absent file). **Disposition: open.**
- **F3 — the timeout warning tells a `program_status()` caller about a
  manifest.** CONFIRMED with `capture_version()` stubbed to the absorbed-timeout
  sentinel for two programs: `tidymedia_probe_timeout`, body
  "The version probe timed out after 5 seconds. / FFplay and MediaInfo / The
  manifest records NA for those versions; ...". There is no manifest on this
  path, and the bullet lists display labels rather than the `program` column's
  values. The reviewer's "four copies of one label" was an artifact of its own
  stub — not reproduced; one label per timed-out program is what fires.
  **Disposition: open.**
- **F4 — AC3's promise is false where a legacy file exists.** CONFIRMED:
  legacy `ffmpeg_location.txt` holding `/bin/ls`, `find_program()` returns
  `/bin/ls`; `set_program("ffmpeg", "/bin/cat")`; `unset_program("ffmpeg")`;
  `find_program()` returns `NULL` with its warning, not `/bin/ls`. The code is
  what Scope asks for ("removing the file `set_program()` wrote **and**, where
  one exists, the pre-M097 `rappdirs` file"), the comment argues for it and the
  suite pins it. AC3's clause "`find_program()` returns what it returned before
  the `set_program()` call" contradicts the milestone's own Scope in the
  both-present state. **Disposition: amendment return — see below.**
- **F5 — `program_status()` suppresses the stale-location warning.**
  CONFIRMED: with `ffplay_location.txt` naming `/no/such/binary`,
  `find_program()` warns "ffplay was configured at ... but that file no longer
  seems to exist", `program_status()` reports `location = NA` and no warning,
  and the stale file stays on disk. `NA` therefore covers both "never
  configured" and "configured at a path that is gone" — the second being the
  state `unset_program()` exists to repair. **Disposition: open.**
- **F6 — a partial removal leaves the capability memo stale.** CONFIRMED with
  `tm_unlink()` stubbed to remove only the first file: `unset_program("ffmpeg")`
  aborted `tidymedia_location_not_removed`, `forget_ffmpeg_capabilities()` was
  never called, the current file was gone, the legacy file remained, and
  `find_program()` then answered `/bin/cat` — a different binary than the memo
  describes, which is the drift D044/M067 exists to prevent.
  **Disposition: open.**
- **F7 — `tool_versions()` shadows its own `programs` argument.** CONFIRMED by
  reading `R/ffm_manifest.R:154`: the argument is reassigned to the timed-out
  display labels inside the warning branch. Correct today only because nothing
  reads it afterwards. **Disposition: open.**
- **F8 — `unset_program("mediainfo")` drops the memoized FFmpeg capabilities.**
  The reviewer raised it for symmetry of the record and judged it harmless and
  consistent with `set_program()`. **Disposition: reject — an intentional
  change the code comment records, matching `set_program()`'s documented
  choice.**
- **F9 — NEWS overclaims silence.** CONFIRMED against F3: the NEWS bullet says
  "It warns about nothing", and the probe-timeout warning does fire from this
  call path. **Disposition: open.**
- **F10 — the absorber pin gains a member without a note.** Reading confirms
  `program_status` qualifies through `suppressWarnings()` over `find_program()`,
  which spawns nothing, and that every other member of that list carries a
  paragraph and this one does not. A comment gap, not a defect.
  **Disposition: open, lowest rank.**

### Amendment return (AC3)

F4 falsifies AC3 only outside the domain of the procedure AC3 names. AC3's
evidence clause names a walk over "the three states (never remembered,
remembered, forgotten) x two config locations"; that walk starts each location
from a clean slate and never reaches the state F4 needs, where a legacy file
predates a `set_program()` call that writes the current one. Inside the named
walk AC3 holds, and the evidence for it is recorded above. Outside it, AC3's
clause "`find_program()` returns what it returned before the `set_program()`
call" contradicts this milestone's own Scope, which asks `unset_program()` to
remove both files. So this is evidence about the promise, not about the work:
it routes to the gated criterion-amendment protocol
(`/milestone-implement` step 6) rather than to a defect return, and it does not
count on the defect-return track the thrash rule reads. First amendment return
on this milestone.

AC3's tick is withdrawn — the evidence stands as recorded, but the criterion
text it was measured against is what changes, so the amended clause is
re-evidenced at re-review. The other five criteria keep their ticks and their
evidence; re-review re-runs them against whatever the amendment lands.

The same false promise is repeated in `?unset_program`'s roxygen
(`R/program_management.R:200`) and in the `NEWS.md` bullet. Those are ordinary
fix-now work, not criterion text, and are carried with the amendment.

The nine other findings stay open and untriaged: the amendment is the only work
this return convenes, and they go to the maintainer at the re-review gate. F1
is the one worth flagging ahead of that — it authorizes an irreversible
deletion from a call that names no program.

### CI

PR #117, commit `fa58566`. Four Linux legs and `pkgdown` green; **macOS and
Windows red**, both with the same two failures:

- `test-timeout-silence.R:351` — "no swept function absorbs a forced timeout
  silently": `program_status absorbed the timeout silently: no condition at all`
- `test-timeout-refusal-blame.R:425` — "a REACHED limit still aborts or warns
  exactly as it did (AC6)": `got$aborted || got$warned` was `FALSE`

macOS `[ FAIL 2 | WARN 2 | SKIP 277 | PASS 11591 ]`, Windows
`[ FAIL 2 | WARN 0 | SKIP 312 | PASS 11273 ]`; macOS `R CMD check` exits
`Status: 1 ERROR`.

Cause, reproduced locally rather than inferred: the workflow's
"Install ffmpeg and mediainfo" step is Linux-only and was skipped on both
runners, so neither has any of the four binaries. `program_status()` spawns a
version probe only for a program that resolves, so on a machine where none
resolves it spawns nothing and a forced limit is never reached — while the two
sweeps assert that every member of the timeout domain either aborts or warns
under a forced limit. Under this machine's real `PATH`,
`tm_force_timeout("program_status", ...)` gives `warned = TRUE`; with `PATH=""`
and both config dirs redirected empty it gives `aborted = FALSE, warned =
FALSE`, which is the runners' reading.

So `program_status()` is the first domain member whose spawning is conditional
on the environment, and the sweeps have no gate for that. The fix belongs with
the work this return convenes.
