# M104: `install_on_win()` registers every program or none

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m104-install-registers-all-or-none` / https://github.com/jmgirard/tidymedia/pull/108

## Goal

An `install_on_win()` that cannot use a program the archive produced registers
nothing at all, and says which program it could not use.

## Scope

**Surface tier: user-facing** — the deliverable is an exported function's
refusal contract and its documented `@return`.

**In:** A registration check over every program the extraction produced, run
before the first `set_program()` call (`R/program_management.R:1098-1100`).
The check asks what `set_program()` asks — `Sys.which()` on the path
(`:204`) — and additionally that the file is not empty, because Windows has no
execute bit, so parity alone catches only an absent file there and leaves a
truncated `ffmpeg.exe` registered. A required program failing it aborts with a
new class, registering nothing; an optional one is informed about and the
install completes. Docs, `NEWS.md`, and a `DECISIONS.md` entry annotating
D082, whose "the one refusal there" sentence this milestone makes stale.

**Out:** Classing `set_program()`'s own abort and threading `call` into it for
direct callers → candidate row (M100's exported-`call` lesson applies there,
and it overlaps the standing unclassed-aborts row from M086's Out).
Running each unpacked binary to verify it → candidate row; it would be the
first probe in this seam to execute a downloaded program and needs its own
D024 call. Recovering a partly-registered state by rolling back written config
files → not needed once nothing is written before the check passes; the
residual window between the check and the loop is disclosed in the D-entry.

## Acceptance criteria

- [x] AC1: No `set_program()` call is made by `install_on_win()` until every
      path the extraction produced has passed the registration check.
      Evidence: a test under `tm_mock_install(real_set = TRUE)` that plants a
      failing `ffprobe`, starts with an `ffmpeg` location already remembered,
      and asserts `tm_roots_snapshot(config$root)`
      (`tests/testthat/test-program-management.R:328`) is identical before and
      after the call.
- [x] AC2: A program in `tm_install_required` failing the check aborts with
      class `tidymedia_program_unusable`, blamed on `install_on_win()`, its
      message naming every failed program and each one's full path; the
      refusal removes nothing from the install directory. Evidence: a test
      failing both `ffmpeg` and `ffprobe` in one call, asserting the class,
      the blamed call, both paths in the message,
      `tm_roots_snapshot(config$root)` unchanged, and `tm_dir_snapshot()` of
      the install directory identical to its post-extraction value.
- [x] AC3: A program outside `tm_install_required` failing the check leaves
      the install successful: one `cli_inform()` naming that program and its
      path and saying the produced file could not be used — wording distinct
      from the message for a program the archive never produced — no config
      file for it, and `install_on_win()` returns `TRUE` with `ffmpeg` and
      `ffprobe` registered. Evidence: a test planting a failing `ffplay`,
      asserting the new wording and the ABSENCE of the "did not produce"
      wording.
- [x] AC4: Four planted forms of a produced path that fails the check are each
      disposed as AC2 states at a required program, and one of them also at
      `ffplay` as AC3 states: a path the extraction listed and did not create,
      a path created as an empty file, a path created as a directory, and — on
      POSIX only, where the bit exists — a path created without the executable
      bit. None reaches `set_program()`'s own `Can't find an executable`
      abort. Evidence: five tests, the POSIX one `skip_on_os("windows")`.
- [x] AC5: `?install_on_win` names `tidymedia_program_unusable` in `@return`'s
      enumeration of aborting outcomes, and Details says what the check asks —
      that the path resolves and the file is not empty — and that it does not
      run the binary; `NEWS.md` carries a user-facing bullet. Evidence:
      `devtools::document()` produces no diff and `man/install_on_win.Rd`
      contains the class name.
- [x] AC6: `devtools::test()` and `devtools::check()` are clean (0 errors,
      0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T1, T4
- AC4 → T1, T2, T3, T4
- AC5 → T6
- AC6 → T7

## Tasks

- [x] T1: Extend `tm_mock_install()`
      (`tests/testthat/test-program-management.R:341`) with a `spoil =`
      argument naming, per program, which of AC4's four forms to plant — it
      currently chmods every stub `0755` under `real_set = TRUE` (`:414`),
      which is why no existing test can reach this defect. Write the AC1–AC4
      tests; confirm red.
- [x] T2: Add the registration check to `install_on_win()` above the loop
      (`R/program_management.R:1098`): partition `unpacked` by
      `Sys.which(tm_install_binary(install_dir, p)) != ""` and
      `file.size(...) > 0`.
- [x] T3: Abort for a failing required program with
      `tidymedia_program_unusable`, naming every failed program and path, and
      touching nothing in the install directory (D082's boundary — this sits
      below a successful extraction, so the unpacked files stay).
- [x] T4: Give a failing optional program its own `cli_inform()` wording,
      distinct from the archive-did-not-produce one, and emit one message
      where both kinds occur in a call.
- [x] T5: Append a `DECISIONS.md` entry annotating D082: a second refusal now
      sits below its successful-extraction boundary; the pre-loop check was
      chosen over rolling back written config files; the check's two limits —
      it does not run the binary, and there is a window between it and the
      loop — are disclosed; falsifier.
- [x] T6: Roxygen `@return` and Details, `devtools::document()`, `NEWS.md`
      bullet.
- [x] T7: `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-09-03: created by /milestone-plan. Promoted from the ROADMAP candidate row added 2026-09-02 (M102 review pass 3 [O]11), which is absorbed here.
- 2026-09-03: criteria audit ran in FULL mode (declared surface tier is user-facing), fresh-context [O] reader. Returned findings on five of six criteria; six were fixed before writing (wrong line cite `:203`→`:204`; AC1 stated a code shape rather than a behavior; AC1's evidence was also true of an empty config root; AC2 promised all unpacked files but probed one, and its plural "each refused program" was probed with one; AC4 said four forms are each disposed as a refusal and then exempted the fourth; AC4/AC5 asserted zero-byte behavior as fact for a Windows-only function on a macOS measurement). Two became gate questions. AC6 passed all six questions. Re-audited after the gate changed the predicate wording; no further findings.
- 2026-09-03: plan gate chose a static pre-loop check (`Sys.which()` parity plus a non-empty test) over `Sys.which()` parity alone, because Windows has no execute bit and parity alone would register a truncated binary there; falsified by a report of the non-empty test refusing a good build. It chose that check over executing each binary with `-version`, because spawning three downloaded programs crosses D024's probe boundary and makes a blocked spawn an install failure; falsified by a report of a build passing the static check and failing to run.
- 2026-09-03: plan gate chose a distinct `cli_inform()` wording for a produced-but-unusable optional program over reusing the archive-did-not-produce message, because that message is false in the new branch (the M38/M40 lesson); falsified by a caller confused by two messages where one would do.
- 2026-09-03: implement gate chose to add an is-a-file test to the check's two planned clauses, because the directory form's refusal otherwise rests on `Sys.which()` behaviour measured on macOS only (2026-09-03) and unmeasured on Windows, the one platform this function runs on. AC5's enumeration is a floor and stays as written; Details names all three.
- 2026-09-03: implement gate chose to give the new refusal no condition data fields, matching the seven refusals this installer already raises, none of which carries any.
- 2026-09-03: implement gate chose to leave the existing `tidymedia_program_not_extracted` check running first, so a call whose archive both omits one required program and produces an unusable one reports the omission; no existing message or test changes.
- 2026-09-03: T1 — `tm_mock_install()` gained `spoil =`, naming per program which of the four forms to plant, and its working stubs are now non-empty and executable on every path rather than only under `real_set = TRUE`; the eight AC1-AC4 tests were confirmed red against the unchecked installer (11 failures).
- 2026-09-03: T2-T4 — `tm_usable_binary()` added; `install_on_win()` partitions the produced set before the loop, aborts `tidymedia_program_unusable` for a failed required program, and emits one `cli_inform()` carrying a distinct sentence for each of the two optional-program states.
- 2026-09-03: T5 — D083 appended, annotating D082.
- 2026-09-03: T7 — `devtools::test()` 11,886 passing / 0 failures / 10 warnings / 18 skips; `devtools::check()` 0 errors, 0 warnings, 0 notes (7m 17s). `devtools::document()` produces no diff on a second run.
- 2026-09-03: T6 — `@return` enumerates six aborting outcomes, Details says what the check asks and that it does not run the binary, `NEWS.md` carries a Configuration bullet.
- 2026-09-03: plan gate chose `tidymedia_program_unusable` over `tidymedia_program_not_executable` (a name for a property Windows does not have, and a lie for the absent case) and over widening `tidymedia_program_not_extracted` (collapses two events D062 keeps apart); falsified by a handler needing to tell the four AC4 forms apart, which one class cannot do.
- 2026-09-03: review — all six criteria evidenced against the branch head and ticked; `cairn_validate` 16/16 and the r-package consistency gate clean; three-lens fan-out returned eight findings from the [O] lens, none from the other two, all logged in the Review section awaiting gate triage.
- 2026-09-03: review gate triaged the eight findings: F1-F5 fixed on the branch, F6-F8 to a candidate row. F1 was a real defect the branch introduced — `tm_install_binary()` now expands `~` where the path is built, with a regression test confirmed red without it.
- 2026-09-03: re-verified after the gate fixes: `devtools::test()` FAIL 0 / WARN 10 / SKIP 18 / PASS 11896; `devtools::check()` 0 errors, 0 warnings, 0 notes (5m 25s); `devtools::document()` no diff; `cairn_validate` 16/16.
- 2026-09-03: step-7 approval: PR #108 approved for merge.
- 2026-09-03: PR #108 marked ready; the CI watch hit the harness ceiling with seven of eight checks pending (pkgdown pass) and was stopped rather than left armed. Merge not attempted; the approval marker is written and stands for PR #108.

## Decisions

- 2026-09-03: D083 records the rule, what the check asks, the two limits it
  discloses, and why a pre-loop check was chosen over rolling back written
  config files. It annotates D082 rather than superseding it: the new refusal
  sits below the same successful-extraction boundary, for the same reason.

## Review

Verified 2026-09-03 on branch `m104-install-registers-all-or-none` (PR #108),
diffed against `origin/master` at `fddf31b`. Test evidence is a run of
`tests/testthat/test-program-management.R` under `devtools::load_all()`:
468 expectations, 0 failures, 0 skips, of which the nine M104 tests contribute
58 expectations.

**Check discrimination.** The nine tests were re-run with
`tm_usable_binary()` replaced by a function returning `TRUE` — the defect the
check exists to catch, planted. Eight of the nine went red (22 failures, two
of them errors where `set_program()`'s own abort escaped); the ninth is the
absent-optional-program test, which exercises pre-M104 behaviour and is
correctly silent. The green is therefore a green the tests could have failed.

- AC1 — PASS. `a produced program that cannot be used stops every
  registration` (4 expectations): a config root holding an `ffmpeg` location
  written before the call, a planted zero-byte `ffprobe`, and
  `tm_roots_snapshot(config$root)` identical before and after, with the kept
  file's contents read back so a re-registration at the same name could not
  pass. Its premise is asserted too — `ffmpeg`'s produced path is non-empty,
  so the unchanged root is the check refusing rather than nothing being
  registrable. Red under the planted defect (3 failures).
- AC2 — PASS. `the refusal names every failed program and leaves the install
  directory alone` (9 expectations): both `ffmpeg` and `ffprobe` spoiled in one
  call; `tidymedia_program_unusable` asserted by class, `blamed_verb()` is
  `install_on_win`, both program names and both full paths matched in the
  message, `Can't find an executable` absent, `tm_roots_snapshot()` unchanged,
  and `tm_dir_snapshot(d)` identical to the snapshot the mock records at the
  moment extraction finished. Red under the planted defect (errored).
- AC3 — PASS. `a produced ffplay that cannot be used leaves the install
  successful` (8 expectations): `install_on_win()` returns `TRUE`, the message
  names `ffplay` and its full path and carries `could not be used`, the
  `did not produce` wording is absent, config files exist for `ffmpeg` and
  `ffprobe` and not for `ffplay`. Red under the planted defect (4 failures).
- AC4 — PASS. Five tests, one per planted form plus the optional-program one.
  `absent` (8), `empty` (7), `dir` (7) and `noexec` (7, `skip_on_os("windows")`
  — it RAN here, macOS 25.6.0) each assert the AC2 disposition through the
  shared `tm_expect_required_refusal()` helper, including
  `expect_no_match(msg, "Can't find an executable")`. `a directory planted at
  ffplay's path is informed about, not refused` (6) disposes the `dir` form at
  `ffplay` the way AC3 states. Four of the five went red under the planted
  defect; the `dir` and `noexec` forms by 4 failures each, `absent` by 5,
  `empty` by an escaped abort.
- AC5 — PASS. `Rscript -e 'devtools::document()'` leaves `git status` clean
  apart from this milestone file (no diff in `man/` or `NAMESPACE`).
  `man/install_on_win.Rd` names `tidymedia_program_unusable` twice: in
  `@return`'s enumeration, now six aborting outcomes rather than five
  (`:48`), and in the Details paragraph on which refusals sit outside D082's
  rule (`:99`). Details states what the check asks and its limit at `:73-76`
  ("the path has to resolve the way an executable does, and what is there has
  to be a file rather than a directory, and not be empty... The program itself
  is never run"). `NEWS.md` carries a Configuration bullet.
- AC6 — PASS. `Rscript -e 'devtools::test()'`:
  `[ FAIL 0 | WARN 10 | SKIP 18 | PASS 11886 ]`. `Rscript -e
  'devtools::check()'`: `Status: OK`, 0 errors / 0 warnings / 0 notes, 5m 19s.
  Re-run after the gate's F1-F5 fixes: `[ FAIL 0 | WARN 10 | SKIP 18 | PASS
  11896 ]` and `0 errors | 0 warnings | 0 notes` (5m 25s); the
  `test-program-management.R` file went from 56 tests / 468 expectations to
  57 / 478, and `document()` still produces no diff.

**Consistency gate.** `cairn_validate.py` exit 0 — 16 PASS, 7 advisories OK,
no `release window`. No `DESIGN.md` principle changed, so `cairn_impact.py`
was not run. Toolchain checks from the `r-package` profile's
`consistency-gate` slot: `document()` no diff (above); `NAMESPACE`/`man/`
regenerate clean and are not hand-edited; `README.Rmd` untouched by the diff,
so `README.md` is in sync; `pkgdown::check_pkgdown()` — "No problems found";
`NEWS.md` carries the user-visible entry and names no milestone number; no new
top-level files, and `check()` reports 0 notes; full `check()` clean.

### Independent review (three lenses, fresh context)

Full three-lens fan-out: the declared tier is user-facing and the diff touches
`R/` and `tests/`.

- **[S] blame-history** — no findings. It traced `tm_mock_install()`'s stub
  block to M102, confirmed no other test in the file depends on the old
  zero-byte / conditionally-chmod'd stub shape, and confirmed the roxygen
  rewrite of M103's D082 paragraph is consistent with D083. It added one
  observation, which the [O] lens also raised as F4.
- **[S] prior-review** — no findings. Archived `## Review` sections for M97,
  M98, M101, M102, M103 were read; the GitHub probe
  (`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`,
  so the per-PR walk was correctly skipped. It checked M102's own deferred
  "partial registration through `set_program()`" row (this milestone is its
  fix), M102's LESSONS line on what "produced" means, the M38/M40 hint-truth
  lesson (applied, not violated), M101's F1 self-derived-expectation finding,
  M103's cleanup findings, and M97's `list.files()` finding.
- **[O] diff-bug** — eight findings, ranked below.

**Findings and dispositions.**

- **F1 — `R/program_management.R:695-700`: the check's clauses disagree about
  which path they mean, so a `~`-relative `install_dir` refuses a good
  install.** `file.info()` calls `path.expand()`; `Sys.which()` does not.
  Reproduced here 2026-09-03: with a real 13-byte executable at
  `~/tm_m104_probe/bin/ffmpeg.exe`, `Sys.which()` on the tilde path returns
  `""` and on the expanded path returns the file, so `tm_usable_binary()` is
  `FALSE` and `TRUE` for the same file. `install_on_win(install_dir =
  "~/ffmpeg")` therefore downloads, verifies and unpacks correctly and then
  aborts `tidymedia_program_unusable`, blaming the archive. This is D083's own
  stated falsifier. Pre-M104 the same path failed at `set_program()`'s
  `Can't find an executable at ~/…`, which at least named the path.
  DISPOSITION: FIXED at the gate. `tm_install_binary()` now `path.expand()`s the path it builds, so the check and the `set_program()` call below it ask about one file; expanding inside the check alone would have moved the same failure into the loop. Regression test `an install directory written with a tilde is not refused as unusable` (5 expectations, `skip_on_os("windows")`) asserts the whole install succeeds and that what was remembered carries no `~`; it asserts its own instrument (`path.expand("~")` resolves to the redirected `HOME`) rather than skipping on it. Confirmed red without the fix (the abort escapes as an error). D083 now records the expansion and why it belongs where the path is built.
- **F2 — `cairn/DECISIONS.md` D083: the motivating story cannot happen on the
  only platform the function runs on.** The opening says a zero-byte
  `ffprobe.exe` "wrote `ffmpeg`'s location first and then aborted out of
  `set_program()`'s own unclassed `Can't find an executable`", while the
  entry's own "What the check asks" paragraph says Windows has no execute bit,
  so such a file resolves under `Sys.which()` and would be remembered as
  working. The abort described is POSIX behaviour; on Windows both programs
  register silently. The story is right for the `absent` form and wrong for
  the zero-byte one it names. DISPOSITION: FIXED at the gate. D083's opening paragraph now separates the two forms: the listed-and-never-created path is the one that registered `ffmpeg` and then aborted out of `set_program()`, while a zero-byte `ffprobe.exe` on Windows resolves under `Sys.which()` and registered silently. The correction is marked in the entry. D083 has not reached the default branch, so this is a fix before the record lands rather than an edit to history.
- **F3 — `NEWS.md`: the bullet asserts old behaviour that is false for two of
  its three named forms.** "unpacked as a truncated file, as a directory, or
  not at all used to be registered anyway … so a caller could be left with
  `ffmpeg` remembered and `ffprobe` pointing nowhere" — for "not at all" the
  old loop aborted at `set_program()` rather than registering; for "as a
  directory" on Windows D083 says the behaviour is unmeasured; and "`ffprobe`
  pointing nowhere" is true only of the truncated case.
  DISPOSITION: FIXED at the gate. The `NEWS.md` bullet no longer claims three unpack forms were registered anyway. It now names the two states that were real — a truncated `ffprobe.exe` remembered as working, and a missing one registered only after `ffmpeg` had been written — and drops the directory form, whose old behaviour on Windows is unmeasured.
- **F4 — `R/program_management.R:1156-1176` and
  `tests/testthat/test-program-management.R`: the "one message where both
  kinds occur" branch is unreachable, and the test named for it exercises
  neither kind together.** `tm_install_registers` minus `tm_install_required`
  is exactly `{ffplay}` (`:306`, `:312`), so `absent_optional` and
  `unusable_optional` can never both be non-empty. The test titled "an absent
  program and an unusable one are reported in one message" plants no `spoil`
  and passes unmodified against `origin/master` — it is the one M104 test that
  stayed green under the planted-defect run above. T4's second half is
  unverified in effect. DISPOSITION: FIXED at the gate, as documentation. The test is renamed `an absent optional program is still reported in one message` and its comment now records that no call can reach both optional states at once, because the optional set is exactly `ffplay`; the combining branch is written for a fourth registered program that does not exist yet. The branch itself is left in place.
- **F5 — `tests/testthat/test-program-management.R:1642-1652`: the census's
  "reads too far" floor was not extended for the new exit.** The comment still
  says "the two exits BELOW the unpack" where there are now three, and the
  floor asserts only `tidymedia_archive_unreadable` and
  `tidymedia_program_not_extracted` are absent from the narrowed set.
  DISPOSITION: FIXED at the gate. The census floor now names three exits below the unpack and asserts `tidymedia_program_unusable` is present in the whole-body set and absent from the narrowed one, the same shape the other two carry.
- **F6 — `R/program_management.R:1140-1152`: the refusal message is false for
  AC4's `absent` form.** With a path the extraction listed and never created,
  the caller reads "The archive produced 'ffprobe', but it cannot be used" and
  "whatever the archive unpacked is still in `<dir>`" while there is nothing
  at that path to look at. Arguably that form is what
  `tidymedia_program_not_extracted` names (D062's two-events rule); the tests
  assert the name and path appear, never that the sentence is true of the
  state on disk. DISPOSITION: FOLLOW-UP. Which condition class a listed-but-never-created path should raise is a contract question that overlaps D062's rule on keeping two events apart, and answering it changes an exported function's refusal contract rather than a message. Filed as a candidate row.
- **F7 — `tests/testthat/test-program-management.R`: the `!info$isdir` clause
  the implement gate added for Windows has no test that isolates it.** On
  POSIX `Sys.which()` already returns `""` for a directory, so the `dir` test
  still refuses with that clause deleted; nothing green-or-red in the suite
  discriminates it. There is also no direct unit test of `tm_usable_binary()`.
  DISPOSITION: FOLLOW-UP, filed with F6 and F8. A test that isolates the `!info$isdir` clause needs a platform where `Sys.which()` answers for a directory, which is the Windows behaviour this project does not measure; a direct unit test of `tm_usable_binary()` is plannable beside it.
- **F8 — `R/program_management.R:697-699`: `tm_usable_binary()` reads as
  vectorized but is scalar-only, and one clause is dead.** A length-2 `path`
  throws at `&&` under R >= 4.3 and length 0 yields `NA`; and
  `!is.na(info$size)` can never be `FALSE` once `!is.na(info$isdir)` passed.
  Neither bites at the single `vapply()` call site.
  DISPOSITION: FOLLOW-UP, filed with F6 and F7. Neither the scalar-only contract nor the dead `!is.na(info$size)` clause bites at the single `vapply()` call site, so nothing is wrong today; the helper's shape is worth settling alongside the direct unit test F7 asks for.

What the [O] lens checked and found sound: the partition runs strictly before
the first `set_program()` write and the loop skips `unusable`; the abort is
blamed on `install_on_win()` by `cli_abort`'s default `call`; the cli
pluralization renders correctly for one and two items in every branch; the AC1
test defeats the name-only snapshot by reading the kept config file's bytes
back; broken symlinks refuse via the `NA` guard; `man/install_on_win.Rd`
matches the roxygen.

