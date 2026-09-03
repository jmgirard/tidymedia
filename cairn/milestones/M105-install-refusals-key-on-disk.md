<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M105: `install_on_win()`'s refusals say what is on disk

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m105-install-refusals-key-on-disk` / https://github.com/jmgirard/tidymedia/pull/109

## Goal

A produced path with nothing on it is refused as a program the archive did not
produce, and the helper that judges the paths that do exist is tested directly.

## Scope

User-facing tier: `install_on_win()` is exported and the condition class one of
its refusals raises changes.

**In:** `install_on_win()` decides what an extraction produced from libarchive's
list AND the disk, at all four sites that ask (`R/program_management.R:1083`,
`:1093`, `:1103`, `:1112`), so a listed path holding nothing falls to
`tidymedia_program_not_extracted` and no refusal claims files are in a directory
that has none. `tm_usable_binary()` becomes elementwise, loses its dead
`!is.na(info$size)` clause, and gains direct tests — including the one case no
POSIX runner reaches. Docs, NEWS, and a DECISIONS entry.

**Out:** running each unpacked program to see whether it executes (candidate row,
promotes on a report of a registered binary that cannot run). Classing
`set_program()`'s own `Can't find an executable` abort (candidate row; a
different exported function's contract). A `~`-expansion change anywhere but the
tests — M104 fixed that at `tm_install_binary()` and this milestone only probes it.

## Acceptance criteria

- [x] AC1: A required program the extraction listed and never created aborts
      `tidymedia_program_not_extracted`, blamed on `install_on_win()`. Its full
      message, read back, names the path that was looked for and contains
      neither `cannot be used` nor `still in that directory`.
- [x] AC2: With a required program planted at `ffmpeg`, at `ffprobe`, and at
      both in one call, each planted form is disposed as stated: `absent` aborts
      `tidymedia_program_not_extracted` naming every absent program; `empty`,
      `dir` and `noexec` abort `tidymedia_program_unusable` naming every failed
      program and each full path. Every aborting call leaves the config root's
      file list unchanged, an existing remembered location's contents unchanged
      when read back, and the install directory's snapshot identical to the one
      taken when the extraction finished.
- [x] AC3: Where the extraction lists entries and the disk holds none of them —
      every program in `tm_install_registers` planted `absent` — the refusal
      describes the directory truthfully and never says unpacked files are still
      there: an install directory this call created is removed again and the
      message says so; one that already existed is reported as holding what it
      held. Both cases are evidenced.
- [x] AC4: A directory at a produced path is refused by `tm_usable_binary()` on
      its own account rather than by `Sys.which()`'s answer: with `Sys.which()`
      mocked to resolve that directory the function answers `FALSE`, and
      deleting the `!info$isdir` clause makes that assertion fail.
- [x] AC5: `tm_usable_binary()` answers `TRUE` for a non-empty executable file
      and `FALSE` for each of an absent path, an empty file, a directory, a
      non-executable file, and a tilde-relative path naming a non-empty
      executable — the last being the `file.info()`/`Sys.which()` disagreement
      that refused a good install at M104 — and it answers elementwise: over the
      vector of those six paths one call returns the same unnamed logical vector
      as the six one-path calls, a vector repeating one of them twice answers
      twice, a length-1 vector answers length 1, and a zero-length vector
      answers `logical(0)`.
- [x] AC6: `install_on_win()`'s `@return` and Details no longer describe
      `tidymedia_program_not_extracted` as "the archive did not contain a
      required program" but by the path check that now decides it;
      `man/install_on_win.Rd` regenerates with no further diff; `NEWS.md` records
      the class a listed-but-absent program now raises; and a DECISIONS entry
      annotating D083 records the re-partition, that D082's give-back boundary
      now keys on disk, the residual the conjunction cannot separate, and
      quarantine-after-extraction as the field-reachable cause.
- [x] AC7: `devtools::test()` and `devtools::check()` clean (0 errors,
      0 warnings; NOTEs justified), and `devtools::document()` produces no diff.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T2, T3
- AC3 → T1, T2, T3
- AC4 → T4
- AC5 → T4, T5
- AC6 → T6, T7
- AC7 → T7

## Tasks

- [x] T1: Compute the produced set once as libarchive's list intersected with
      what is on disk now, and read it at all four sites: `unpacked_here`
      (`R/program_management.R:1083`), the per-program partition (`:1093`), the
      empty-extraction guard (`:1103`), and the message-arm selector (`:1112`).
      Re-word the absent-program branch so it is true of the disk in each arm.
- [x] T2: Stop `tm_mock_install()`'s extract mock creating `bin/` when it will
      write nothing into it (`tests/testthat/test-program-management.R:409`) —
      without it the all-absent state leaves a non-empty directory and AC3's
      removal arm is unreachable. Add the all-absent plant.
- [x] T3: One test per planted form at each of `ffmpeg`, `ffprobe` and both;
      assert the full message on the absent form; cover AC3's two directory
      cases (created by the call, and pre-existing).
- [x] T4: Direct tests of `tm_usable_binary()` over AC5's six inputs and its
      four vector shapes, including the mocked-`Sys.which()` directory case;
      confirm that case red with the `!info$isdir` clause deleted.
- [x] T5: Vectorize `tm_usable_binary()` (`&&` → `&`), drop the dead
      `!is.na(info$size)` clause, collapse the call site's `vapply()`
      (`R/program_management.R:1139-1143`) to one call.
- [x] T6: Roxygen for the two below-extraction refusals, `devtools::document()`,
      `NEWS.md`.
- [x] T7: The DECISIONS entry; full `devtools::test()` and `devtools::check()`.

## Work log

- 2026-09-03: created by /milestone-plan. Promoted from the M104-review candidate row (F6, F7, F8), which this absorbs whole.
- 2026-09-03: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader, two passes. Pass 1 returned findings on all six drafted criteria plus two cross-cutting: nine fixed before the gate (a vacuous directory criterion that mocked a call the short-circuit never reaches; a byte-for-byte promise no snapshot can establish; two criteria bound to the test suite rather than the function; one plant location standing for a family; the missing tilde axis; two internal-refactor promises at a user-facing tier; an unbounded "elementwise"; an under-scoped message prohibition; and a D-entry no criterion required), one posed at the gate (the empty-extraction guard reading the list).
- 2026-09-03: criteria audit pass 2, over the gate's new AC3 and revised AC7, returned one blocking finding — `tm_mock_install()` creates `bin/` unconditionally, so AC3's removal arm was unreachable through it (now T2) — plus four fixed: two directory cases where one was presupposed, four `produced$files` readers where two were named, no probe reaching AC3's state, and an overclaim about what the conjunction decides. It also confirmed the conjunction contradicts neither D082's stated reason nor M102's reason for reading the list.
- 2026-09-03: plan gate chose routing the listed-but-absent form to `tidymedia_program_not_extracted` over keeping `tidymedia_program_unusable` with corrected wording, and over a third class of its own; the existing class already names the event and its message is true of the disk, where the other two either make one class cover two events or add a class to an exported function for a case no real archive has produced. Falsified by a caller needing to tell an absent path apart from an unlisted one.
- 2026-09-03: plan gate chose fixing the empty-extraction guard inside this milestone over disclosing it as a gap, so one meaning of "produced" holds across the whole below-extraction path. Falsified by the re-point changing the give-back for an extraction that lists and writes nothing in a way a caller wanted the old behaviour for.
- 2026-09-03: `cairn_validate` fired the >7-criteria split tripwire at eight; the per-input and per-vector-shape promises about `tm_usable_binary()` merged into one criterion (same function, same task) rather than re-opening the split question the gate had settled at four-of-four recommended answers.
- 2026-09-03: implement gate: chose naming the install directory and the files the extraction did leave there in the absent-program refusal (user deferred the choice), and adding a line telling the caller when a listed file is not on disk, naming antivirus quarantine as the usual cause. Verified `base::Sys.which()` can be mocked with `local_mocked_bindings()`, so AC4 needs no wrapper indirection.
- 2026-09-03: T1 code landed: `tm_files_on_disk()` added, all four below-extraction sites re-pointed at the list-intersected-with-disk set, absent-program refusal re-worded (full path in the `Looked for` line, new quarantine line, no `still in that directory`). Existing M103/M104 wording and class assertions updated with it. Task left unchecked pending the full-suite verify; `test-program-management.R` passes 478/478.
- 2026-09-03: T1 checked off: full `devtools::test()` clean (0 failures, 11,896 passes, 18 skips).
- 2026-09-03: T2 and T3 landed (checkboxes pending the shared full-suite verify): the extract mock creates `bin/` only where a form will write into it, plus a named all-absent plant; the refusal helper generalized to plant at either required program or both, and the four forms added at `ffmpeg` and at both; new tests for AC1's full message and AC3's two directory cases. Discrimination checked: with the mock's `bin/` guard reverted, the AC3 removal test goes red on all three of its directory assertions. `test-program-management.R` 576 passes, up from 478.
- 2026-09-03: T2 and T3 checked off: full `devtools::test()` clean (0 failures, 11,994 passes, 18 skips).
- 2026-09-03: T4 and T5 landed together (checkboxes pending the full-suite verify), because T4's vector-shape tests cannot pass before T5's change: written first, they errored on `'length = 6' in coercion to 'logical(1)'` at the scalar `&&`, and passed after the `&`. T4's other tests were green before T5. AC4's discrimination checked: with `!info$isdir` deleted, the mocked-`Sys.which()` directory expectation goes red on its own (1 failure, 596 passes). `test-program-management.R` 597 passes.
- 2026-09-03: T4 and T5 checked off: full `devtools::test()` clean (0 failures, 12,015 passes, 18 skips).
- 2026-09-03: T6: `install_on_win()`'s Details and `@return` now describe `tidymedia_program_not_extracted` by the path check rather than by what the archive contained; `devtools::document()` rewrote `man/install_on_win.Rd` and a second run produced no further diff. NEWS entry added under Configuration.
- 2026-09-03: T7: D084 appended, annotating D083 — the produced set is the reported list intersected with the disk, D082's give-back boundary now keys on the disk, the two absent cases the one class cannot separate (with the message that does), and quarantine-after-extraction as the reachable cause. `devtools::check()` running; task unchecked until it and the full suite are clean.
- 2026-09-03: T7 checked off: `devtools::check()` Status OK (0 errors, 0 warnings, 0 notes, 6m 42s) and `devtools::test()` clean (0 failures, 12,015 passes, 18 skips). Status to review.
- 2026-09-03: plan gate chose making `tm_usable_binary()` elementwise over keeping it scalar with a length guard; the underlying `file.info()` and `Sys.which()` are already vectorized, it makes the dead clause disappear rather than be deleted, and the caller's `vapply()` collapses to one call. Falsified by a call site needing the short-circuit `&&` gave it.
- 2026-09-03: review. All seven criteria evidenced fresh on the branch head; `check()` Status OK, `test()` 0 failures / 12,034 passes / 18 skips; `cairn_validate` all checks passed; `pkgdown::check_pkgdown()` clean. Three fresh-context lenses: both Sonnet lenses zero findings, the [O] lens thirteen. F1, F2, F3, F5, F6 and F12 fixed on the branch (each mutation-tested red before the gate); F4, F10 and F11 deferred to one candidate row; F7, F8, F9 and F13 rejected with reason. AC5's vector clause was unmet as committed — the sixth path named nothing rather than a non-empty executable — and was fixed at review before the box was ticked. No finding met the return floor.
- 2026-09-03: step-7 approval: PR #109 approved for merge.

## Decisions

## Review

Verified 2026-09-03 on `m105-install-refusals-key-on-disk` at PR #109.
`origin/master` had not moved since the branch was cut (0 behind, 9 ahead), so
no merge preceded the evidence below. Every figure here was measured this
session on the branch head as reviewed, not carried from the work log.

### Acceptance criteria

- **AC1 — evidenced.** `a listed path that was never created is not reported
  as unusable`: with `ffprobe` planted `absent`, the call aborts
  `tidymedia_program_not_extracted`, `blamed_verb()` is `install_on_win`, and
  the message read back through `cli::ansi_strip()` names the full path
  `<install_dir>/bin/ffprobe.exe` and matches neither `cannot be used` nor
  `still in that directory`. The test also asserts the arm is non-vacuous:
  `ffmpeg.exe` is on disk, so the message is the one for a directory that does
  hold the rest of the build.
- **AC2 — evidenced.** Twelve tests: four planted forms (`absent`, `empty`,
  `dir`, `noexec`) at each of `ffprobe`, `ffmpeg`, and both in one call, one
  test per cell. Each asserts the class the form is due (`absent` →
  `tidymedia_program_not_extracted`; the other three →
  `tidymedia_program_unusable`), `blamed_verb()` = `install_on_win`, and for
  every planted program both its name and its full path in the message. Each
  also asserts the three no-write promises: `tm_roots_snapshot(config$root)`
  identical before and after, a pre-existing remembered location for `ffplay`
  identical when read back with `readLines()`, and `tm_dir_snapshot(d)`
  identical to `rec$after_extract`. `noexec` skips on Windows (no such bit).
- **AC3 — evidenced, both cases.** `an extraction that listed everything and
  created nothing gives back the directory it made`: every program in
  `tm_install_registers` planted `absent`, install directory created by the
  call — `dir.exists()` FALSE for the directory and its parent afterwards, the
  message says `removed the install directory it created`, and it matches
  neither `the files the extraction did produce are in` nor `still in that
  directory`. `... leaves a directory it found alone`: same plant, install
  directory pre-existing with a file of the caller's — directory still there,
  `keep.txt` unchanged when read back, message says `holds what it held when
  this call started`.
- **AC4 — evidenced, with discrimination measured.** `tm_usable_binary()
  refuses a directory on its own account`: `base::Sys.which()` mocked via
  `local_mocked_bindings()` to resolve the directory (asserted in the test), and
  `tm_usable_binary(f$dir)` still answers `FALSE`; the control `f$good` still
  answers `TRUE`, so the mock has not made the check refuse everything.
  Discrimination re-measured this session: deleting the `!info$isdir` clause
  from `R/program_management.R` makes exactly one expectation fail — this one,
  at `test-program-management.R:2572` — and no other test in the file.
- **AC5 — evidenced after a review fix.** Six inputs: `TRUE` for a non-empty
  executable; `FALSE` for an absent path, an empty file, a directory, a
  non-executable file (POSIX-only), and a tilde-relative path naming a
  non-empty executable (`HOME` redirected and the redirection asserted; the
  same file expanded answers `TRUE`, so the `FALSE` is the tilde). Four vector
  shapes: the six-path vector equals the six one-path calls and is unnamed; a
  repeated path answers twice; length-1 answers length 1; `character(0)`
  answers `logical(0)`. **As committed the vector clause was not met** — the
  sixth path was `"~/nowhere.exe"`, a tilde path naming nothing rather than
  AC5's "tilde-relative path naming a non-empty executable", so the
  `file.info()`/`Sys.which()` disagreement went untested in the vector shape.
  Fixed on the branch at review (the [O] lens independently found the same
  thing, F7) and re-verified; the criterion is now evidenced as written.
- **AC6 — evidenced.** `install_on_win()`'s Details and `@return` describe
  `tidymedia_program_not_extracted` as "a required program that is not at the
  path it would be installed to", and Details states what decides it — the
  archive's file list and the install directory together, with quarantine named
  as the cause and the "reported writing" line disclosed. Neither text contains
  "the archive did not contain a required program". `devtools::document()` run
  twice this session produced no diff in `man/` or `NAMESPACE` (clean
  `git status` both times). `NEWS.md` records the class change under
  Configuration, in user-facing words with no milestone number. `D084` appended,
  annotating D083: the re-partition, D082's give-back boundary now keying on
  disk, the residual the one class cannot separate, and quarantine-after-
  extraction as the field-reachable cause, with a falsifier.
- **AC7 — evidenced.** `devtools::check()` **Status: OK** (0 errors, 0
  warnings, 0 notes). `devtools::test()` **0 failures, 12,034 passes, 18
  skips**. `devtools::document()` no diff.

### Consistency gate

- `cairn_validate.py` — exit 0, all checks passed (16 PASS, 7 OK).
- `cairn_impact.py` — skipped; `Principles touched:` is `—`.
- `devtools::document()` no diff; `man/` and `NAMESPACE` regenerate clean.
- `pkgdown::check_pkgdown()` — "No problems found". No new exports, so no
  `_pkgdown.yml` row was owed.
- `README.Rmd`/`README.md` untouched by the diff; no re-knit owed.
- `NEWS.md` carries the milestone's user-visible change, no milestone numbers.
- No new top-level files, so no `.Rbuildignore` entry owed; `check()` clean.

### Independent review

Three fresh-context lenses, distinct evidence bases. **[S] blame-history: zero
findings** — nothing undone, no fixed bug resurrected, no D-entry contradicted;
it confirmed the new `tm_files_on_disk()` does not reopen M102's
previous-run-leftovers bug, because it tests existence of paths already on the
extraction's own list and never enumerates the directory. **[S] prior-review
record: zero findings** — the archived `## Review` sections are the primary
surface here and the `gh api .../pulls/comments` probe returned `[]`, so the
thread walk was skipped; the lens reports this diff *implements* M104's
deferred F6-F8 rather than reopening anything. **[O] diff-bug: thirteen
findings**, verified by mutation testing on a scratch copy.

**Actioned — fixed on the branch (six):**

- **F1. The `vanished` intersect is completely undiscriminated.** Replacing
  `vanished <- intersect(absent_required, tm_extracted_programs(produced$files,
  tm_install_required))` with `vanished <- absent_required` left the whole
  suite passing. That intersect is the only thing separating D084's two absent
  cases, so a false "the extraction reported writing … Antivirus quarantine is
  the usual cause" line for a program the archive never listed would have
  shipped silently. *Fixed:* negative assertions on both flat-archive tests
  (no `reported writing`, no `Antivirus quarantine`). Mutation now fails 5.
- **F2. The refusal's headline contradicted its own body.** "The archive did
  not produce `ffprobe.exe`." was followed immediately by "The extraction
  reported writing `ffprobe.exe`, but it is not there." AC6 made the *docs*
  stop describing the class that way; the message the caller actually reads
  still said it. *Fixed:* where every missing program is one the extraction
  reported, the headline is now "The archive did not leave behind …"; where any
  was never listed it stays "did not produce". Both arms asserted positively
  and negatively. Mutation now fails 4.
- **F3. "None of the files the extraction reported are there" was said when
  the extraction reported no files at all.** M105 replaced M103's exact "The
  archive produced no files at all" with a sentence presupposing a report, and
  rewrote the two M103 tests to lock the weaker wording in. *Fixed:* the
  sentence is now selected on `length(produced$files)`, and the M103 tests
  assert the exact one again. Mutation now fails 4.
- **F5. `tm_files_on_disk()` had no direct test; its separator normalization
  was dead in the suite.** Replacing the `gsub`/`sub` normalization with
  `as.character(files)` passed everything, and that normalization exists for
  the one platform this function runs on. *Fixed:* a direct test over a
  backslash entry, a `./`-prefixed entry, a listed path the disk does not
  hold, and `character(0)`. Mutation now fails 1.
- **F6. A code comment overclaimed what the disk check proves.** "asked
  whether it is there, which is a question about this build alone" — but
  `file.exists()` cannot tell this extraction's file from an earlier install's
  at the same path, and `install_dir` is one stable path across installs.
  *Fixed:* the comment now says what the check does and does not establish.
- **F12. The `tm_files_on_disk()` comment claimed normalization parity with
  `tm_extracted_programs()` that does not hold** — that helper folds case, this
  one does not. No field impact (Windows is case-insensitive and is the only
  platform), but the comment was wrong. *Fixed:* the comment now states why the
  two differ.

**Actioned — deferred to one candidate row (three):**

- **F11. Directory entries in the archive's file list count as produced
  files.** If libarchive reports `bin/` as an entry, an extraction whose every
  *file* was quarantined still has a non-empty `on_disk`, so AC3's give-back
  arm becomes unreachable in the field and the message points the caller at an
  empty `bin/`. Unmeasured against a real ffmpeg archive; the mock returns file
  entries only. Not cleanly fixable by excluding directories, because the `dir`
  spoil form requires a directory at a program path to be *seen* so it reaches
  the unusable check.
- **F4. The first of the four re-pointed sites is unobservable.** Reverting
  `unpacked_here <- length(on_disk) > 0L` to `length(produced$files) > 0L`
  fails nothing, because the explicit `tm_remove_created_dirs()` call has
  already removed the directory before the abort, so the `on.exit` handler
  never matters. The other three sites each fail exactly one test when
  reverted. Behaviour is correct; the belt is untested.
- **F10. An empty-string entry in the file list counts as on disk**, because
  `file.exists("<dir>/")` is TRUE. Not reachable through the mock or any
  measured libarchive behaviour; defensive only.

**Rejected (four), with reason:**

- **F7. AC5's vector clause unmet as committed.** Correct, and already fixed
  at review before this triage — recorded under AC5 above rather than carried
  as an open finding.
- **F8. AC7's recorded evidence predates the current tree.** Correct of the
  work log, and the reason review re-runs everything: `check()` and `test()`
  were both re-run on the reviewed tree, after the fixes above. No open defect.
- **F9. `expect_match(msg, program)` is vacuous for the `ffmpeg` plant
  location**, since `ffmpeg` appears in the install path regardless. True, but
  the very next line asserts the full `tm_install_binary()` path for that same
  program, which is not vacuous and is what carries AC2. A redundant assertion,
  not a gap.
- **F13. Cosmetic** — one 83-char line and two double blank lines. Linter and
  formatter territory, and the file already carries >80-char lines; out of
  scope at review per the taxonomy.

No finding met the return floor: none demonstrated an acceptance criterion
failing that was not closed on the branch before the gate, and none showed a
load-bearing defect left standing in what `install_on_win()` does for its
callers.
