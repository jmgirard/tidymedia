<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M105: `install_on_win()`'s refusals say what is on disk

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m105-install-refusals-key-on-disk`

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

- [ ] AC1: A required program the extraction listed and never created aborts
      `tidymedia_program_not_extracted`, blamed on `install_on_win()`. Its full
      message, read back, names the path that was looked for and contains
      neither `cannot be used` nor `still in that directory`.
- [ ] AC2: With a required program planted at `ffmpeg`, at `ffprobe`, and at
      both in one call, each planted form is disposed as stated: `absent` aborts
      `tidymedia_program_not_extracted` naming every absent program; `empty`,
      `dir` and `noexec` abort `tidymedia_program_unusable` naming every failed
      program and each full path. Every aborting call leaves the config root's
      file list unchanged, an existing remembered location's contents unchanged
      when read back, and the install directory's snapshot identical to the one
      taken when the extraction finished.
- [ ] AC3: Where the extraction lists entries and the disk holds none of them —
      every program in `tm_install_registers` planted `absent` — the refusal
      describes the directory truthfully and never says unpacked files are still
      there: an install directory this call created is removed again and the
      message says so; one that already existed is reported as holding what it
      held. Both cases are evidenced.
- [ ] AC4: A directory at a produced path is refused by `tm_usable_binary()` on
      its own account rather than by `Sys.which()`'s answer: with `Sys.which()`
      mocked to resolve that directory the function answers `FALSE`, and
      deleting the `!info$isdir` clause makes that assertion fail.
- [ ] AC5: `tm_usable_binary()` answers `TRUE` for a non-empty executable file
      and `FALSE` for each of an absent path, an empty file, a directory, a
      non-executable file, and a tilde-relative path naming a non-empty
      executable — the last being the `file.info()`/`Sys.which()` disagreement
      that refused a good install at M104 — and it answers elementwise: over the
      vector of those six paths one call returns the same unnamed logical vector
      as the six one-path calls, a vector repeating one of them twice answers
      twice, a length-1 vector answers length 1, and a zero-length vector
      answers `logical(0)`.
- [ ] AC6: `install_on_win()`'s `@return` and Details no longer describe
      `tidymedia_program_not_extracted` as "the archive did not contain a
      required program" but by the path check that now decides it;
      `man/install_on_win.Rd` regenerates with no further diff; `NEWS.md` records
      the class a listed-but-absent program now raises; and a DECISIONS entry
      annotating D083 records the re-partition, that D082's give-back boundary
      now keys on disk, the residual the conjunction cannot separate, and
      quarantine-after-extraction as the field-reachable cause.
- [ ] AC7: `devtools::test()` and `devtools::check()` clean (0 errors,
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
- [ ] T4: Direct tests of `tm_usable_binary()` over AC5's six inputs and its
      four vector shapes, including the mocked-`Sys.which()` directory case;
      confirm that case red with the `!info$isdir` clause deleted.
- [ ] T5: Vectorize `tm_usable_binary()` (`&&` → `&`), drop the dead
      `!is.na(info$size)` clause, collapse the call site's `vapply()`
      (`R/program_management.R:1139-1143`) to one call.
- [ ] T6: Roxygen for the two below-extraction refusals, `devtools::document()`,
      `NEWS.md`.
- [ ] T7: The DECISIONS entry; full `devtools::test()` and `devtools::check()`.

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
- 2026-09-03: plan gate chose making `tm_usable_binary()` elementwise over keeping it scalar with a length guard; the underlying `file.info()` and `Sys.which()` are already vectorized, it makes the dead clause disappear rather than be deleted, and the caller's `vapply()` collapses to one call. Falsified by a call site needing the short-circuit `&&` gave it.

## Decisions

## Review
