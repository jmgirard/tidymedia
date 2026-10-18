<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M143: `unset_program()` forgets a location file beside a folder with contents, and `set_program()` reports an unwritable location

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — changes what two exported functions do and report   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m143-location-write-and-forget   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

`unset_program()` forgets a location file beside a folder with contents, and `set_program()` reports an unwritable location with a tidymedia error.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** `tm_set_program()` (`R/program_management.R:1271-1275`), whose
`dir.create()` and `writeLines()` let base R's "cannot open file ...
Permission denied" warning and "cannot open the connection" error reach the
caller (0.2.2 has the same code). `tm_unset_program()`
(`R/program_management.R:1401-1407`), which refuses before it removes
anything on any folder with contents, so the `unset_program()` advice of the
lookup warnings fails beside such a folder (D104, unreleased). The help pages
`?set_program`, `?unset_program` and `?find_ffmpeg`, the stale code comment
at `R/program_management.R:179-183`, `NEWS.md`, and a new D-entry, D105.

**Out:**
- Items (b) to (e) of the "Location folder edges after M141" candidate row
  (the removal race, `install_on_win()` stopping partway, a link to a
  folder, a FIFO at a location path) stay in that row.
- `set_program()` on a file without the execute bit → the "M138 review
  leftovers" candidate row, item (a).
- A pre-prompt writability check → not planned: it needs a new
  `file.access()` caller, which the census at
  `tests/testthat/test-input-path-front-door.R:416` refuses.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: On a system other than Windows, the current location file has
      mode 0444. Then `set_program("ffmpeg", <runnable stub>, confirm =
      FALSE)` signals no warning and stops with one error of the new class
      `tidymedia_location_not_written`. Its `tm_program` field is
      `"ffmpeg"`, and its `tm_file` field is the path of that file. The file
      still holds the line it held before the call. A test in
      `tests/testthat/test-program-location-repair.R` shows this. It skips on
      Windows, and it skips where `file.access(<file>, 2) == 0`.
- [ ] AC2: On a system other than Windows, the same call signals no warning
      and stops with one `tidymedia_location_not_written` error whose
      `tm_file` is the path of the location file, in two cases. In case (a)
      the folder for the current location file does not exist, and its
      parent has mode 0555. The folder still does not exist after the call.
      In case (b) that folder exists with mode 0555 and holds no location
      file. No location file exists after the call. A test runs each case.
      It skips on Windows, and it skips where `file.access(<that folder's
      parent or that folder>, 2) == 0`.
- [ ] AC3: One location path holds a folder with contents. The other holds
      a location file or an empty folder, and its removal succeeds. Then
      `unset_program("ffmpeg")` removes that file or empty folder and leaves
      the folder with contents and its entries in place. It returns `TRUE`
      invisibly. After the removal, it gives one warning of the new class
      `tidymedia_location_folder_kept`. The warning's `tm_program` is
      `"ffmpeg"`, and its `tm_files` is the path of the folder with
      contents. A test runs each of the `file`, `hidden` and `subfolder`
      shapes of `tm_folder_shapes` at each of the two paths, against a
      location file and against an empty folder at the other path. It
      asserts that the call drops the FFmpeg capability memo in the
      location-file cases and keeps it in the empty-folder cases. A second
      test, on a system other than Windows, puts a mode-0333 folder beside a
      location file. It asserts that the folder is still there and the
      warning names it. It skips on Windows, and it skips where
      `file.access(<folder>, 4) == 0`.
- [ ] AC4: At least one location path holds something, and each path that
      holds something holds a folder with contents. Then
      `unset_program("ffmpeg")` still stops with the
      `tidymedia_location_folder` error, and both paths stay as they were.
      A test runs each of the three shapes at each path, with nothing or a
      second folder with contents at the other path.
- [ ] AC5: `?set_program` names `tidymedia_location_not_written` and the
      cases that give it. `?unset_program` and `?find_ffmpeg` say three
      things about `unset_program()`. It removes the location files and
      empty folders. It leaves each folder with contents and gives the
      `tidymedia_location_folder_kept` warning. It gives the
      `tidymedia_location_folder` error where each path that holds something
      holds a folder with contents. `NEWS.md`'s development section has one
      new bullet for the unwritable-location error. The bullet that opens
      "`set_program()` and `unset_program()` now remove an empty folder"
      describes the new `unset_program()` behavior in place of "removes
      nothing".
- [ ] AC6: `devtools::check()` reports 0 errors and 0 warnings.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. -->

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T2
- AC5 → T3
- AC6 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate. -->

- [x] T1: Write the AC1 and AC2 tests first and see them fail. Then wrap
      the `dir.create()` and `writeLines()` calls in `tm_set_program()` so
      that a failure gives `tidymedia_location_not_written` (fields
      `tm_program`, `tm_file`, `call` threaded) and base R's warning does
      not reach the caller. Write the file in place, with no
      temp-then-rename step, so a mode-0444 file is refused and not
      replaced. Leave the capability memo alone on that path.
- [x] T2: Rewrite the M141 test at
      `tests/testthat/test-program-location-repair.R:785-818`, whose
      location-file and empty-folder cases now contradict AC3, into the
      AC3 and AC4 tests, and add the mode-0333 case. See them fail. Then
      change `tm_unset_program()`: remove every file and empty folder,
      keep the D089 memo rule and the `tidymedia_location_not_removed`
      error, and give the folder-kept warning after a removal that took.
      Keep the `tidymedia_location_folder` error for the AC4 case. Update
      the comment at `R/program_management.R:179-183`.
- [ ] T3: Update the roxygen for `?set_program`, `?unset_program` and
      `?find_ffmpeg` in `R/program_management.R`, run
      `devtools::document()`, and edit `NEWS.md` as AC5 says. Derive each
      new claim from an executed call.
- [ ] T4: Run the full `devtools::test()` with no other R session working,
      once more with `PATH=/usr/local/bin:/usr/bin:/bin` and both config
      folders empty (the M142 lesson), then `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: question set: which candidate to plan — "Location edge cases" (the "Unwritable location file" row and item (a) of the "Location folder edges after M141" row); split policy — split and plan every part.
- 2026-10-18: one milestone, not two: both changes touch `R/program_management.R`, the same test file and the same NEWS area, and together they fit one session; the split answer did not fire.
- 2026-10-18: collision check: absorbs the "Unwritable location file" row in full and item (a) of "Location folder edges after M141"; extends done M141; supersedes D104's refuse-before-removing rule for `unset_program()` as D105; inbox empty (0 issues, 0 PRs).
- 2026-10-18: criteria audit (full, fresh Opus reader) returned 10 findings, all fixed: helper location and shapes corrected (3 shapes, no unreadable shape), mode-0333 case added to AC3, AC3 bounded to a removal that succeeds, the warning given its own class, AC4 bounded to a path that holds something, AC1 reworded to the behavior with mode 0444, AC2 skips fixed and a second case added, AC5 names the real NEWS bullet and Rd text, T2 names the M141 test to rewrite.
- 2026-10-18: plan gate chose a new warning class `tidymedia_location_folder_kept` over reusing `tidymedia_location_folder` because a caller's handler for the error class would unwind on the warning; falsified by a caller who needs one class for both outcomes.
- 2026-10-18: plan gate chose catching the failed write over a writability check before the prompt, because the check needs a new `file.access()` caller and cannot foresee every failure; falsified by a caller who answers the prompt and then gets this error and needed to know first.
- 2026-10-18: plan gate chose refusing an unwritable file over removing it and writing a new one, because the caller set the permission; falsified by a caller who expects `set_program()` to replace a read-only location file.
- 2026-10-18: implement started on branch m143-location-write-and-forget, cut from master at f80dd15b.
- 2026-10-18: T1 done: AC1 and AC2 tests failed first (base `simpleError` plus one base warning), then `tm_set_program()` catches the `dir.create()`/`writeLines()` failure as `tidymedia_location_not_written` with the warnings muffled; the `find_program()` comment citing the row now names the class; full `devtools::test()` clean.
- 2026-10-18: T2 done: M141's refuse-before-removing test rewritten into the AC3 test (3 shapes x 2 paths x file or empty folder, memo asserted), a mode-0333 case, and the AC4 test (nothing or a second full folder); both new tests failed first; `tm_unset_program()` now refuses only when every present path is a full folder, removes the rest, and warns `tidymedia_location_folder_kept` after the removal; full `devtools::test()` clean.
- 2026-10-18: the folder-kept warning says `find_<program>()` warns about the folder while the program is not on the `PATH`, read from an executed `unset_program()` then `find_ffmpeg()` with `PATH` empty.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive. -->
