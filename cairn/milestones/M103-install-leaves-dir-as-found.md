# M103: A refused `install_on_win()` leaves the install directory as it found it

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m103-install-leaves-dir-as-found` / [#107](https://github.com/jmgirard/tidymedia/pull/107)

## Goal

Every refusal `install_on_win()` can take before it registers a program leaves the install directory holding what it held when the call started — the files a failed extraction wrote are removed or named, the files it found are kept, and a directory the call created is gone.

## Scope

The deliverable is **user-facing**: `install_on_win()` is exported and a caller reads the on-disk state a refusal leaves behind.

**In:** (i) `tm_unpack()` snapshots its destination before extracting and, where libarchive fails, removes the entries that extraction created or changed and reports any it could not delete (D046's rule, applied set-wise to a directory); (ii) `install_on_win()` removes the directories it created when a refusal below `dir.create()` leaves them empty; (iii) the `tidymedia_archive_unreadable` refusal names anything left behind; (iv) a `cairn/DECISIONS.md` entry extending D046 to the installer.

**Out:** the `tidymedia_program_not_extracted` path, whose extraction SUCCEEDED and whose shipped message deliberately tells the caller the unpacked files remain — the boundary is stated in AC4's entry, not deferred. Multi-digest `sha256sum` sidecar parsing → the M102 candidate row, whose promotion condition (a source publishing one) is unmet. Partial registration through `set_program()` → its own candidate row. Archive provenance/substitution → its own candidate row. Re-cutting `data-raw/corrupt-archive-fixtures.R`, which bakes the generator's absolute temp path into the entry name → the test derives entry paths from `archive::archive()` instead.

## Acceptance criteria

- [ ] AC1: `tm_unpack()` snapshots its destination recursively (path, size, mtime) before extracting and, where `archive::archive_extract()` fails, takes a second snapshot and removes what the comparison shows this extraction added: every file it shows created or changed, with `unlink(expand = FALSE)`, and every directory it shows **created**, with `unlink(recursive = TRUE, expand = FALSE)`. A directory that existed before the extraction is never removed, whatever its timestamp shows — removing it recursively would take entries the extraction never touched — and its created or changed children are removed individually instead. Every entry the comparison shows the extraction neither created nor changed is left untouched by the cleanup, its mtime unmoved, except a directory whose mtime moves because the cleanup removed a child from it. Removal is best-effort: a third snapshot taken after the removals names what survived, and every entry the removal targeted that it still shows is returned to the caller rather than silently dropped.
- [ ] AC2: AC1 is verified over the two failure routes the committed fixtures reach — `not-an-archive.7z` refused at open (writing nothing) and `corrupt-payload.7z` failing mid-read after it has created entries (a directory chain and a zero-length file) — by tests driving real `archive::archive_extract()` calls, no mock, each route against an empty destination, a destination holding a file at a path that fixture's own `archive::archive()` listing shows it writes (where that listing can be read and shows any), a destination holding a file at a path it does not write, and a destination holding a nested subdirectory on a path that listing shows the fixture writes into (same guard), that subdirectory and its own entry asserted still present after the failure; plus cells mocking the removal seam to fail, once on a created file and once on a created directory removed recursively, each asserting the entry that would not delete is reported.
- [ ] AC3: Every refusal `install_on_win()` can take above its `tm_unpack()` call leaves no directory the call created, at or above `install_dir`, and leaves an already-existing `install_dir`'s entries unchanged. The domain is enumerated by the M102 AC6 census (`tests/testthat/test-program-management.R:1216-1236`) narrowed by a positional filter to the `return()` and `cli_abort()` nodes preceding the `tm_unpack()` call, with the test asserting a bijection between those nodes and its registry of triggering cases so a node with no case fails it. The four front-door refusals that walk cannot see — `rlang::check_bool(confirm)`, `rlang::check_string()` on `download_url` and on `install_dir`, `check_sha256()` on `archive_checksum` — and `tm_confirm()`'s non-interactive refusal each get their own case in that registry; each refuses above the call's first `dir.create()`, so each must create no directory at all.
- [x] AC4: A `cairn/DECISIONS.md` entry extends D046 to the installer: it states the rule, states that removal is best-effort and that a failed removal is named in the refusal rather than swallowed, states that a pre-existing directory is never removed even where the comparison shows it changed and why, states that a pre-existing file the extraction overwrote or truncated is removed under D046's created-or-changed rule and why that is not the same case, names `tidymedia_program_not_extracted` as the path the rule deliberately does not cover and why, and states a falsifier.
- [x] AC5: `man/install_on_win.Rd` states both the general rule — a refusal leaves the install directory as it found it, naming anything it could not remove — and the single exception, `tidymedia_program_not_extracted`, and is byte-identical to a fresh `devtools::document()` run; `NEWS.md` records the change.
- [ ] AC6: `devtools::check()` reports 0 errors and 0 warnings, with every NOTE drawn from the known-acceptable set the review evidence lists and any other NOTE failing this criterion; `devtools::test()` reports 0 failures; and all six `R-CMD-check` matrix jobs are green, `windows-latest (release)` and `ubuntu-latest (4.1.0)` included.
- [ ] AC7: The `tidymedia_archive_unreadable` refusal names every entry `tm_unpack()` reported it could not remove; where the call created `install_dir` and the cleanup left it empty, the call removes that directory and the refusal names no directory it has just removed, saying instead that the install directory is as the call found it. Verified by tests over both fixtures, over a call that created the install directory, and with the removal seam mocked to fail leaving more than one entry — on a created file and, separately, on a created directory that keeps a child — every one asserted named.

## Coverage

- AC1 → T1, T3, T6
- AC2 → T1, T2, T3, T6
- AC3 → T4, T5
- AC4 → T7
- AC5 → T8
- AC6 → T9
- AC7 → T5, T6

## Tasks

- [x] T1: Add two internals beside `tm_unpack()` (`R/program_management.R:405`): `tm_dir_snapshot(dir)`, returning path/size/mtime for every entry under `dir` recursively including directories and dotfiles, and `tm_unlink()`, a thin wrapper over `unlink()` that exists so the suite can mock a removal failure.
- [x] T2: Tests first, all red before T3: AC2's matrix over both fixtures × the four starting states, with entry paths read from `archive::archive()` rather than hard-coded (the fixture stores an absolute generator path, so `strip_components = 1` writes a deep name), plus the mocked-`tm_unlink()` cell.
- [x] T3: `tm_unpack()` snapshots before extraction, re-snapshots on failure, removes the created directories first (one recursive call covers the deep chain a stripped absolute path makes) and then the added files still standing outside them, and returns the undeletable leftovers to its caller instead of a bare `NULL`, in a two-slot list — R drops attributes on `NULL`, so the attribute alternative the plan named does not exist — with `install_on_win()`'s failure test moved to that list's file slot.
- [x] T4: Tests first, all red before T5: extend the M102 census helper with the positional filter and the bijection assertion, and add the five uncoverable-by-walk cases AC3 names.
- [x] T5: `install_on_win()` records what `dir.create(recursive = TRUE)` created (`R/program_management.R:608-611`) and, on any refusal below it, removes those directories deepest-first, stopping at the first that is not empty.
- [x] T6: The `tidymedia_archive_unreadable` abort (`R/program_management.R:687-696`) names the leftovers T3 reports; tests for both fixtures and for the created-parent case.
- [x] T7: Write AC4's `cairn/DECISIONS.md` entry.
- [x] T8: `@details`/`@return` on `install_on_win()`, `devtools::document()`, `NEWS.md` bullet.
- [x] T9: `devtools::check()`, full `devtools::test()`, push and confirm all six R-CMD-check jobs.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: plan gate chose best-effort removal with the refusal naming what remains over unconditional removal, because libarchive may still hold the partial file open on Windows — the leak that cost M102 a red `windows-latest` — and the handle here belongs to `archive_extract()`'s writer, which cannot be measured off Windows; falsified by a measurement showing the writer handle closed on the failure path, which would make the unconditional promise honest.
- 2026-09-02: plan gate chose leaving `tidymedia_program_not_extracted` outside the rule over one unconditional rule, because that extraction succeeded and D046's rule is about what a FAILED run wrote, and the shipped abort already promises the files remain; falsified by a report of a caller surprised to find a complete unwanted extraction left in the install directory.
- 2026-09-02: plan gate chose keeping `dir.create()` above the digest fetch and removing the directory on refusal over moving `dir.create()` below the download, because the second trades fail-fast on an unwritable directory for a several-hundred-megabyte download before the same refusal; falsified by a refusal path where the created directory cannot be identified for removal.
- 2026-09-02: plan gate chose leaving the multi-digest sidecar (M102 deferral (b)) as a candidate row over folding it in, because its promotion condition — a source publishing a `sha256sum` manifest — is unmet and the fix would be guesswork about a format nothing in play emits; falsified by a source publishing one.
- 2026-09-02: criteria audit ran in FULL mode (declared surface tier user-facing), two rounds, fresh-context [O] reader both times. Round 1 returned findings on AC1 (probe variety, unbounded domain), AC2 (instrument-bound, incomplete exit list), AC2/AC3 (leaf-only directory claim), AC3 (vacuous satisfaction), AC5 and AC6 (unbounded universals); all repaired, and the Windows-handle risk it raised became the gate's first question. Round 2 on the repaired wording returned findings on all five re-audited criteria and three factual corrections: the M102 census walks both node types but has no positional filter (`test-program-management.R:1216-1236`), `rlang::check_bool(confirm)` (`R/program_management.R:549`) was a fourth uncoverable exit the draft omitted, and the CI matrix has six jobs not five (`.github/workflows/R-CMD-check.yaml:21-28`, including the `4.1.0` floor job). All repaired into the wording above; AC1's changed-entry contradiction resolved by D046's own created-or-changed split.
- 2026-09-02: implement gate chose removing only directories this extraction created over removing every directory the comparison marks changed, because a pre-existing directory's mtime moves the instant an entry lands in it — measured on this machine, 19:13:23.663 to 19:13:24.848 — so the recursive removal AC1 first specified would have deleted the caller's own untouched files inside it.
- 2026-09-02: implement gate chose a refusal that says what state the install directory is in — leftovers named, otherwise stated as found and the created-and-now-empty directory removed — over one that names leftovers only, because today's message points at a directory the call may have just deleted.
- 2026-09-02: implement gate chose keeping D046's created-or-changed rule for a pre-existing FILE the failed extraction overwrote or truncated, over exempting it, because the emptied file is the harm D046 exists to clear; AC4's entry now has to state the case rather than leave it implied.
- 2026-09-02: substantive amendment, criteria audit ran in FULL mode (declared surface tier user-facing), two rounds, a fresh-context [O] reader each time, neither having authored the wording it read. Round 1 on the narrowed AC1 returned four findings: two clauses disagreeing about an overwritten file inside a pre-existing directory, an untouched-entries promise no named procedure enumerates, a leftover-reporting promise `unlink()`'s single status cannot keep, and no AC2 cell able to fail the new rule. Round 2 on the repaired text returned findings on AC1, AC2 and AC7, plus three measured corrections: `corrupt-payload.7z` writes no content bytes (seven directory levels and a zero-length file), `archive::archive()` errors outright on `not-an-archive.7z` so that fixture has no readable listing, and a third snapshot of the whole destination would report the caller's own kept files as undeletable leftovers. All repaired into the wording now in the file.
- 2026-09-02: amendment executed — AC1 narrowed (created directories only; comparison-bound promises; targeted-entry leftover reporting), AC2 sharpened (corrected fixture facts, listing-readable guard, a nested subdirectory on a path the fixture writes into, two removal-seam mock cells), AC4 gained the pre-existing-file and pre-existing-directory clauses, AC7 added binding the refusal message the Goal's "or named" half promised and no criterion bound; Coverage gained `AC7 → T5, T6`. T3 reworded: R drops attributes on `NULL`, so the two-slot list is the only option the plan's alternative left standing.- 2026-09-02: T1-T3 -- `tm_dir_snapshot()`, `tm_snapshot_added()`, `tm_unlink()` and `tm_remove_added()` added beside `tm_unpack()`, which now snapshots before extracting, removes what the comparison shows the failed extraction added, and returns `list(files, leftovers)`. Tests in `tests/testthat/test-unpack-cleanup.R` were red on all four leftover assertions before the change and are 49 green after. The M102 test helper `tm_dir_snapshot()` was renamed `tm_roots_snapshot()`: it snapshots the two redirected roots, and the package now owns that name.
- 2026-09-02: T4-T5 -- the M102 census gained `tm_collect_exits_before()` (a positional narrowing to the statements above the `tm_unpack()` call) and `tm_exit_keys()` (class- or return-value-keyed, so a new exit takes a key no case claims). Seven exits precede the call; the registry holds seven cases and the bijection asserts both directions, with floors that the narrowed set still holds both `return(FALSE)` exits and that it drops both aborts below the call. `install_on_win()` records `tm_missing_ancestors(install_dir)` above `dir.create()` and gives them back on any refusal below. The directory assertions were red for five of the seven cases before the change.
- 2026-09-02: T5 amended one M098 assertion rather than deleting it: `test-program-management.R:220` pinned the default install directory by the chain the call left behind, which M103 now removes. The removal seam is mocked to record and no-op, so the chain is still observed and the recorded set additionally shows the removal aimed at exactly it.
- 2026-09-02: T6 -- the `tidymedia_archive_unreadable` refusal now takes one of three shapes: leftovers named by full path; the install directory named and said to hold what it held; or, where the call created and then removed that directory, no directory named at all. The M102 assertion that the message names `install_dir` is superseded by AC7 and now asserts the opposite on that path, with a comment saying why.
- 2026-09-02: T7 -- D082 written, extending D046 from one run's designated outputs to a whole destination directory.
- 2026-09-02: T8 -- `install_on_win()`'s `@details` and `@return` state the rule and its single exception; `devtools::document()` rewrote `man/install_on_win.Rd`; NEWS.md gained a Configuration bullet.
- 2026-09-02: T1-T8 landed in one checkpoint commit rather than eight. The tests-first ordering held throughout (each red run is named above), but the per-task checkpoint discipline did not.- 2026-09-02: minor amendment -- T3's removal order reversed from "files-first then directories" to directories-first. One recursive call clears the whole created chain, where file-first would delete the files and then still have to walk the chain; and the two orders are distinguishable at the mock seam, where directories-first is what lets a failing recursive call report the directory it kept.- 2026-09-02: T9 -- branch pushed and PR #107 opened, because the R-CMD-check workflow triggers on `pull_request` and on pushes to `master` only, so AC6's six jobs cannot run off a branch push alone.- 2026-09-02: T9 -- `devtools::check()` on the branch reports `Status: OK`, 0 errors / 0 warnings / 0 notes (21m 31s; `testthat.R` 15m/20m OK), so AC6's known-acceptable NOTE set is empty on this platform. `devtools::test()` separately reported 0 failures, 11731 passing, 18 skips. The six R-CMD-check jobs are still pending.- 2026-09-02: `windows-latest (release)` failed on the first CI round with 15 failures, every one M103's own: the cleanup could not delete `payload.txt` or the `var` chain holding it, and the refusal named them as leftovers instead. macOS and pkgdown were green. The best-effort design absorbed it -- nothing dishonest shipped -- but the removing half of the goal held on every platform except the only one `install_on_win()` runs on, which is the risk the plan's first gate named.
- 2026-09-02: gate chose attempting a fix and measuring it on the Windows job over accepting naming-instead-of-deleting, because no local machine can take that measurement and the falsifier D082 states is exactly what the job would produce. `tm_remove_added()` now sweeps twice, the second pass only where the first left something, with `gc()` and a 0.1 s pause between them: the handle belongs to `archive_extract()`'s writer and is not an R connection, so `gc()`'s finalizers are the only lever R has, and the pause covers a transient hold rather than a leaked one. The strict tests are left strict for this round -- they ARE the measurement.- 2026-09-02: the Windows measurement came back negative and is now on record: with `gc()` and a 0.1 s pause between two sweeps, `windows-latest (release)` failed identically -- 15 failures, the same leftovers, `payload.txt` and the `var` chain holding it. The handle libarchive writes the failed entry through is leaked for the process lifetime, not held transiently. The other five R-CMD-check jobs and pkgdown and test-coverage were all green on that round. This confirms rather than falsifies D082's best-effort choice; D082's own falsifier asks for the opposite result.
- 2026-09-02: gate chose aligning the tests with AC1's own best-effort clause over redesigning around a scratch directory or returning to planning. AC1 promises removal as best-effort with an undeletable entry returned to the caller, which is exactly what Windows does, so no criterion changed wording -- the tests were over-claiming. `tm_expect_left_as_found()` now asserts the whole promise: nothing the caller had is gone or changed, every named leftover really is on disk, and nothing survives unnamed or under something unnamed. The strict "removed, and nothing to report" claim is kept on the platforms that can keep it. The scratch-directory redesign is filed as a candidate row.
- 2026-09-02: NEWS.md's Configuration bullet rewritten to say the Windows behaviour plainly rather than hide it behind "best-effort", and to name the refusals that DO take their directory back there.- 2026-09-02: the scratch-directory redesign filed as a ROADMAP candidate row (search-first: no existing row covers it; the three neighbouring installer rows are the sidecar manifest, archive provenance, and partial registration). Adding it took `ROADMAP.md` to 24,536 b, over its 24,000 budget, so nine of the widest candidate rows were compressed to 23,969 b; no row was pruned and no claim dropped.- 2026-09-02: T9 closed on commit `6a0aedd`. `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes (20m 35s). All six R-CMD-check jobs green -- `windows-latest (release)` 14m34s, `macos-latest (release)` 10m34s, `ubuntu-latest` release 21m20s / devel 18m48s / oldrel-1 20m46s / 4.1.0 18m30s -- plus pkgdown and test-coverage (run 33708179316). Status set to review; the commit that records this is tracking-only, so the CI run it triggers reruns the same code.
- 2026-09-02: review pass 1 — defect return. AC1 fails its "rather than silently dropped" clause: an entry `file.info()` cannot stat (measured with a broken symlink) becomes `NA` in `tm_snapshot_added()` and is neither removed nor reported. AC2 fails its created-file mock cell: the cell pre-writes the entry, so the seam fails on a file the extraction truncated (22 b to 0 b), not one it created. AC3's default-install-dir and `check_string(install_dir)` assertions cannot go red. AC7 fails "names every entry": cli truncates the leftover vector at 20 (measured on 25 paths), and the mocked cell's directory assertion is satisfied by the file's own path as a literal prefix. Also fix-now: the `!registered` exit handler can delete `install_dir` under the `tidymedia_program_not_extracted` message, and `@return`/`@details`/NEWS state two things the code does not do. Full triage in the Review section; status in-progress.

## Decisions

## Review

Review pass 1, 2026-09-02, on `6d0028b` against `origin/master`. PR #107 open,
branch 7 ahead / 0 behind. Three criteria could not be verified, so this pass
returns the milestone; the evidence gathered before the return is below.

### Acceptance criteria

- **AC1 — not verified.** `tm_dir_snapshot()`/`tm_snapshot_added()`/
  `tm_remove_added()` do what the criterion describes for every entry
  `file.info()` can stat, and `test-unpack-cleanup.R` is 57 green. But an entry
  it cannot stat is neither removed nor reported: measured on this machine, a
  broken symlink under the destination gives `isdir = NA`, `tm_snapshot_added()`
  puts `NA_character_` into `files`, `NA %in% still` is `FALSE`, so the removal
  skips it and the survivor comparison drops it. AC1's "returned to the caller
  rather than silently dropped" is the clause this fails. ([O]3)
- **AC2 — not verified.** The matrix over both fixtures × the four starting
  states is present and green, and the guard for `not-an-archive.7z`'s
  unreadable listing is correct. The criterion also asks for two removal-seam
  mock cells, "once on a created file and once on a created directory removed
  recursively". The directory cell is there. The file cell pre-creates the
  entry's whole ancestor chain and pre-writes the entry, so the file the seam
  fails on is one the extraction TRUNCATED, not one it created — measured:
  size 22 before, 0 after, `added$dirs` empty, `added$files` that one
  pre-existing path. No cell mocks the seam to fail on a created file.
- **AC3 — not verified.** `tm_collect_exits_before()` narrows the M102 census
  positionally, seven exits precede the `tm_unpack()` call, the registry holds
  seven cases, and the bijection asserts both directions with both floors; the
  five uncoverable-by-walk cases are each present. Two of the directory
  assertions cannot go red, though. `tm_ac3_run()` calls `tm_redirect_data()`,
  which points `R_USER_DATA_DIR` at its own temp root, so the test's
  `default_root` — computed under a different root — names a path no run could
  create ([O]8); and the `check_string(install_dir)` case calls
  `install_on_win(install_dir = 1)`, never passing `dir`, so both its
  assertions are about paths the call has no name for ([O]9).
- **AC4 — verified.** D082 (`cairn/DECISIONS.md`) states the rule, states
  removal is best-effort with a failed removal named rather than swallowed,
  states that a pre-existing directory is never removed however its timestamp
  reads and why, states that a pre-existing file the extraction overwrote or
  truncated IS removed under D046's created-or-changed rule and why that is a
  different case, names `tidymedia_program_not_extracted` as the deliberately
  uncovered path with its reason, and states two falsifiers.
- **AC5 — verified.** `man/install_on_win.Rd` states the general rule and the
  single exception in `@details` (`:67-73`) and again in `@return` (`:45-49`);
  `devtools::document()` produced no diff, so the file is byte-identical to a
  fresh run; `NEWS.md`'s Configuration bullet records the change. (The
  `@return` sentence's accuracy is a separate finding, [O]4 — AC5 asks that the
  page state the rule, which it does.)
- **AC6 — not run to completion.** `devtools::test()` is 0 failures across the
  two touched files (57 + 364 assertions); `devtools::check()` was in its
  `testthat.R` phase and `macos-latest (release)` and `pkgdown` had passed with
  the other five jobs pending when the return was decided. Both watchers were
  stopped rather than left armed: the code changes now, so this evidence would
  be stale before it could tick anything.
- **AC7 — not verified.** The refusal's three shapes are implemented and the
  tests over both fixtures, the created-directory case and the mocked-failure
  case are green. Two gaps. The message interpolates the leftovers as
  `{.file {left}}`, and cli truncates a vector at 20 — measured on a 25-element
  path vector, entries 19 through 23 are replaced by an ellipsis — so a failure
  leaving more than 20 undeletable entries names 20 of them, against AC7's
  "names every entry" ([O]1). And the mocked cell's directory assertion,
  `grepl(file.path(d, topmost), msg)`, is satisfied by the file's own path,
  which contains it as a literal prefix, so the cell cannot fail on the
  directory being dropped from the message ([O]7).

### Consistency gate

`cairn_validate.py` 16/16 PASS, 7 advisories OK, exit 0. No principle change,
so `cairn_impact.py` does not apply. Toolchain slot: `devtools::document()` no
diff; `pkgdown::check_pkgdown()` no problems; `README.Rmd`/`README.md`
untouched by the branch; NEWS.md carries the entry; no new top-level files;
`devtools::check()` not completed (see AC6).

### Independent review

Three fresh-context lenses, distinct evidence bases. [O] diff-bug returned 12
findings; [S] blame-history returned zero regressions, confirming each of the
two prior-milestone assertions this branch changed (M098's directory-chain pin,
M102's "names install_dir") is genuinely superseded and logged; [S]
prior-review-record returned zero findings, with the GitHub inline-comment
probe empty and the archived `## Review` sections of M097/M098/M101/M102
showing no lesson reintroduced.

Findings and disposition — the four above that fail a criterion, plus:

- **[O]2, fix now.** `on.exit(if (!registered) tm_remove_created_dirs(...))` is
  armed above `dir.create()` and is still armed when the
  `tidymedia_program_not_extracted` abort fires. Where the successful
  extraction left `install_dir` empty, the handler deletes the directory while
  the message says the unpacked files are still in it. Measured reachable: an
  archive whose entries are all single-segment has every one stripped by
  `strip_components = 1`, extraction succeeds with `files = character(0)`, and
  the destination is left empty. This is the one path Scope, AC4 and D082 all
  put outside the rule, so the diff reaches a boundary it declared; D082's
  "every refusal above the registration leaves the install directory holding
  what it held" is inconsistent with its own final paragraph on the same point.
- **[O]4, fix now.** `@return`'s "Every one of these leaves the install
  directory as the call found it, except the last" carries no hedge, and the
  milestone's own Windows measurement is that `tidymedia_archive_unreadable`
  leaves `payload.txt` and its directory chain behind. `@details` hedges;
  `@return` does not.
- **[O]5, fix now.** `@details` and the NEWS bullet both say "files that were
  already there are kept". D082 and the shipped code remove a pre-existing file
  the extraction overwrote or truncated, and a test asserts that removal. A
  caller with their own binary where the archive writes would find it deleted
  by a failed install.
- **[O]10, fix now (cheap).** The four front-door AC3 cases use classless
  `expect_error()`, so removing the check each names would still leave the cell
  green. AC3 asks only that each have a case, so this is not why AC3 is
  unverified.
- **[O]6, rejected.** Size-and-mtime cannot see a same-size rewrite inside one
  filesystem timestamp tick. That is D046's comparison rule, which this
  milestone extends rather than introduces, and AC1 is written comparison-bound.
- **[O]11, rejected.** AC7's two message branches are exercised only where
  cleanup succeeds, never on Windows. That is the recorded consequence of the
  gate that chose best-effort removal, not a new defect.
- **[O]12, rejected.** AC7's "on a created file and, separately, on a created
  directory that keeps a child" distinguishes the two entries the one mock
  leaves, not two cells; the combined cell leaves and names both. What the cell
  cannot do is discriminate, which is [O]7.
- **[O] note, no action.** `gc()` and `Sys.sleep(0.1)` in `tm_remove_added()`
  are kept with the negative Windows measurement recorded beside them. Worth
  reconsidering in the next pass, not a defect.
- **[S] blame note, no action.** A Windows caller reading the Goal's prose
  alone could be surprised by leftovers. Disclosed in `@details` and NEWS; [O]4
  and [O]5 are the two places that disclosure is actually wrong.
- **[S] prior-review note, no action.** The "nested subdirectory" cell compares
  `tm_dir_snapshot()` before and after as a whole-state equality check rather
  than deriving an expected value from the logic under test.
