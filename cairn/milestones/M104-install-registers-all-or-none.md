# M104: `install_on_win()` registers every program or none

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m104-install-registers-all-or-none`

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

- [ ] AC1: No `set_program()` call is made by `install_on_win()` until every
      path the extraction produced has passed the registration check.
      Evidence: a test under `tm_mock_install(real_set = TRUE)` that plants a
      failing `ffprobe`, starts with an `ffmpeg` location already remembered,
      and asserts `tm_roots_snapshot(config$root)`
      (`tests/testthat/test-program-management.R:328`) is identical before and
      after the call.
- [ ] AC2: A program in `tm_install_required` failing the check aborts with
      class `tidymedia_program_unusable`, blamed on `install_on_win()`, its
      message naming every failed program and each one's full path; the
      refusal removes nothing from the install directory. Evidence: a test
      failing both `ffmpeg` and `ffprobe` in one call, asserting the class,
      the blamed call, both paths in the message,
      `tm_roots_snapshot(config$root)` unchanged, and `tm_dir_snapshot()` of
      the install directory identical to its post-extraction value.
- [ ] AC3: A program outside `tm_install_required` failing the check leaves
      the install successful: one `cli_inform()` naming that program and its
      path and saying the produced file could not be used — wording distinct
      from the message for a program the archive never produced — no config
      file for it, and `install_on_win()` returns `TRUE` with `ffmpeg` and
      `ffprobe` registered. Evidence: a test planting a failing `ffplay`,
      asserting the new wording and the ABSENCE of the "did not produce"
      wording.
- [ ] AC4: Four planted forms of a produced path that fails the check are each
      disposed as AC2 states at a required program, and one of them also at
      `ffplay` as AC3 states: a path the extraction listed and did not create,
      a path created as an empty file, a path created as a directory, and — on
      POSIX only, where the bit exists — a path created without the executable
      bit. None reaches `set_program()`'s own `Can't find an executable`
      abort. Evidence: five tests, the POSIX one `skip_on_os("windows")`.
- [ ] AC5: `?install_on_win` names `tidymedia_program_unusable` in `@return`'s
      enumeration of aborting outcomes, and Details says what the check asks —
      that the path resolves and the file is not empty — and that it does not
      run the binary; `NEWS.md` carries a user-facing bullet. Evidence:
      `devtools::document()` produces no diff and `man/install_on_win.Rd`
      contains the class name.
- [ ] AC6: `devtools::test()` and `devtools::check()` are clean (0 errors,
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
- 2026-09-03: T6 — `@return` enumerates six aborting outcomes, Details says what the check asks and that it does not run the binary, `NEWS.md` carries a Configuration bullet.
- 2026-09-03: plan gate chose `tidymedia_program_unusable` over `tidymedia_program_not_executable` (a name for a property Windows does not have, and a lie for the absent case) and over widening `tidymedia_program_not_extracted` (collapses two events D062 keeps apart); falsified by a handler needing to tell the four AC4 forms apart, which one class cannot do.

## Decisions

- 2026-09-03: D083 records the rule, what the check asks, the two limits it
  discloses, and why a pre-loop check was chosen over rolling back written
  config files. It annotates D082 rather than superseding it: the new refusal
  sits below the same successful-extraction boundary, for the same reason.

## Review
