<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M147: A failed `unset_program()` removal or `set_program()` write accounts for the folders it touched

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate -->
- **Surface tier:** user-facing — the errors and help pages of the exported `set_program()` and `unset_program()`   <!-- owner: plan · create/amend-via-gate -->
- **Branch/PR:** m147-location-edges   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

A failed removal in `unset_program()` or a failed write in `set_program()` names or takes back each folder that the call kept, created or removed.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** the three items of the candidate row "Location edges after M143". (a) The
`tidymedia_location_not_removed` error of `unset_program()` names a folder with contents
that the call kept. (b) A failed write in `set_program()` takes back the directories that
it created on the way to the config folder (the D082 shape), and its error says when the
call removed an empty folder at the location path. (c) The comment above
`tm_save_location()` stops saying that the suite cannot mock base R functions. Help pages
and one NEWS bullet follow.

**Out:**
- The race between the emptiness check and the removal in `unset_program()` stays in the
  row "Location folder edges after M141", item (b).
- Putting the removed empty folder back after a failed write. The plan gate rejected it
  (work log).
- A new condition field for the kept folder. The plan gate rejected it (work log).
- A partial location file that a failed write leaves in a config folder that was already
  there. M143 chose no write-elsewhere-then-rename step, and this milestone keeps that.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: `unset_program()` can keep a folder with contents at one location path and fail to remove the item at the other path. Then its `tidymedia_location_not_removed` error has a line of its own that says the call kept that folder and names it. That line is apart from the `x` line, which names only what the call failed to remove. Tests cross two axes. The kept folder is at the current path or at the pre-0.2.0 path. The item that fails to go is a location file or an empty folder. In each of the four cases the removal fails through the `tm_unlink()` seam. Each test asserts the error class and `tm_files` equal to the path not removed. It also asserts the kept-folder line with the kept path in `conditionMessage()`. A further test has no kept folder and a removal that fails. It asserts that the message has no kept-folder line.
- [ ] AC2: The ancestors are the directories that are missing on the way to the config folder (`tm_config_dir()`). `set_program()` records them before it calls the `tm_save_location()` seam. If the write then fails, the call tries to remove each ancestor, deepest first. It stops at the first one that holds anything or does not go. Its `tidymedia_location_not_written` error names each ancestor that `dir.exists()` still finds after that removal. The tests replace the seam with a function that makes the config folder by `dir.create(recursive = TRUE)` and then fails. Case (i) has two missing levels. Both are gone afterward, the existing parent is still there, and the message names no directory. In case (ii) the stand-in also writes a file in the config folder before it fails. That folder and its parent stay, and the message names both. In case (iii) the `tm_unlink()` seam returns a status that is not 0 for the deepest level. In case (iv) it returns 0 and leaves the deepest level in place. In cases (iii) and (iv) both levels stay, and the message names both.
- [ ] AC3: `set_program()` can remove an empty folder at the location path and then fail to write. Then its `tidymedia_location_not_written` error says in its message that the call removed that folder. A test reaches this case through the `tm_save_location()` seam. Two more tests assert that the message does not say so. In the first, no folder is at the path. In the second, the folder is there at the first look and goes away while the prompt waits, so the call removes nothing.
- [ ] AC4: The comment above `tm_save_location()` in `R/program_management.R` names the two writes that the tests put in its place. One write warns without failing, and one fails after it creates directories. It no longer says that the suite cannot mock base R functions.
- [ ] AC5: `?unset_program` says that its `tidymedia_location_not_removed` error names a folder that the call kept. `?set_program` says that a failed write tries to remove the folders that the call created. It also says that its `tidymedia_location_not_written` error names each such folder that stays, and names an empty folder that the call removed. A read of `?set_program`, `?unset_program` and `?find_ffmpeg` finds no sentence that AC1, AC2 or AC3 makes false. Each sentence whose wording the branch adds or changes on those pages or in `NEWS.md` describes behavior that a test of AC1, AC2 or AC3 shows. A re-wrap does not count as a change. `NEWS.md` adds no bullet for this milestone. It edits in place the development version's bullet that names `tidymedia_location_folder_kept` for AC1, and the bullet that names `tidymedia_location_not_written` for AC2 and AC3, because the kept folder and that error are both new since 0.2.2 (D091). No `NEWS.md` edit names `tm_save_location()` or a code comment.
- [ ] AC6: On the branch head, `devtools::check()` reports 0 errors and 0 warnings, and `devtools::test()` reports 0 failures.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Write the AC1 tests in `tests/testthat/test-program-location-repair.R` (the
      `tm_lay_out()` helper builds the four layouts) and see them fail. Then add the
      kept-folder line to the `tidymedia_location_not_removed` abort in
      `tm_unset_program()` (`R/program_management.R:1511-1523`). Put `{cli::qty(n)}` before
      each `{?}` (M141 lesson).
- [x] T2: Write the AC2 tests, cases (i) to (iv), and see them fail. In `tm_set_program()`,
      record `tm_missing_ancestors(config_dir)` before the seam call
      (`R/program_management.R:1303-1316`). On a failure, call `tm_remove_created_dirs()`,
      then ask `dir.exists()` which ancestors stay, and name them in the
      `tidymedia_location_not_written` abort. Mock only package seams, never a base function,
      in a test that compares values (M108 lesson).
- [x] T3: Write the AC3 tests, the vanished-folder case included, and see them fail. Record
      that the removal at `R/program_management.R:1270-1288` ran and took, and add a line
      that names the removed folder to the same abort.
- [x] T4: Rewrite the comment above `tm_save_location()` (`R/program_management.R:1343-1345`)
      for AC4.
- [x] T5: Edit the roxygen of `?set_program` (`R/program_management.R:1149-1168`) and
      `?unset_program` (`R/program_management.R:1404-1415`). Read the folder paragraph of
      `?find_ffmpeg` (near `R/program_management.R:1052`) against AC1 to AC3. Run
      `devtools::document()`. Add one NEWS bullet, and avoid the phrases that
      `test-timeout-silence.R` fences out of `NEWS.md` (M143 lesson). Run the claim read of
      AC5 over the diff.
- [ ] T6: Run the full `devtools::test()` with no other R session working (M124 lesson),
      then `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-18: created by /milestone-plan from the candidate row "Location edges after M143".
- 2026-10-18: tier check: milestone — items (a) and (b) change the errors of exported functions, and only item (c) is a trivial edit.
- 2026-10-18: collision check: "Location folder edges after M141" is a separate row (its item (b) is the race in `unset_program()`), cross-referenced in Out; M141 and M143 shipped the neighboring behavior; D082, D104 and D105 read whole, none forbids this scope.
- 2026-10-18: inbox sweep: 0 open issues and 0 open PRs on jmgirard/tidymedia.
- 2026-10-18: lessons harvested: M108 (a base stub also stubs waldo), M124 (run the suite alone), M141 (`cli::qty()` before `{?}`), M143 (NEWS phrases fenced by `test-timeout-silence.R`).
- 2026-10-18: criteria audit (full) returned 8 findings, all fixed: AC1 crosses path and item form and defines the kept-folder line; AC2 names the ancestors that `tm_missing_ancestors()` records, reads what stays by `dir.exists()`, and varies the failure form and level; AC3 adds the vanished-folder case; AC4 names both stand-in writes; AC5 names what each page says and reads `?find_ffmpeg` for contradictions; AC6 had none.
- 2026-10-18: question set: scope — all three items; empty folder after a failed write — say so in the error; kept folder in the removal error — message line only.
- 2026-10-18: plan gate chose saying in the error that the empty folder was removed over putting it back, because the caller agreed to the removal and a recreate can collide with a partial file the failed write left; falsified by a caller who needs that folder back after a failed write.
- 2026-10-18: plan gate chose a message line for the kept folder over a new `tm_kept` field, because no caller is known to read it in code and a field is one more public promise; falsified by a caller who reads the kept folder from the error in code.
- 2026-10-18: plan chose taking back the directories a failed write created over only naming them, because D082 already takes back what a failed install created; falsified by a caller who needs those directories kept after a failed write.
- 2026-10-18: D109 written for the `set_program()` rule.
- 2026-10-18: implement started on branch m147-location-edges, cut from master at e7e790fc.
- 2026-10-18: T1 done: the removal abort in `tm_unset_program()` gets a last "Kept the folder" line; the new test (4 layouts and a no-kept case) failed on the missing line before the fix; full `devtools::test()` 0 failures. The line goes after the write-access hint so that hint's "it" still means the file not removed.
- 2026-10-18: T2 done: `tm_set_program()` reads `tm_missing_ancestors(config_dir)` before the seam, runs `tm_remove_created_dirs()` on a failure, and adds a last "Could not remove the folders it created" line from `dir.exists()`; the four-case test failed before the fix; full `devtools::test()` 0 failures.
- 2026-10-18: T3 done: a `removed` flag set where the empty-folder removal runs adds a "Removed the empty folder" line to the write error; only the "removed" case of the three-case test failed before the fix; full `devtools::test()` 0 failures.
- 2026-10-18: T4 done: the `tm_save_location()` comment names the warning-only and fail-after-create stand-ins and gives the M108 waldo reason for a package seam; comment only, the repair test file still passes.
- 2026-10-18: T5 found that AC5's new NEWS bullet goes against D091: the kept-folder warning and the `tidymedia_location_not_written` error are new since 0.2.2, so the two development-version bullets that name them are edited in place instead; amendment drafted.
- 2026-10-18: re-audit: AC5 (full) — 3 findings: the "arrived after the last release" reason is partly false for AC2, "adds" missed rewritten sentences, "It says nothing about AC4" was unclear; all three fixed in the draft.
- 2026-10-18: re-audit: AC5 (full) — 5 findings: the reason clause was half-wrong, a re-wrap would count as a change, "the error" named no class, "takes back" overstated AC2's cases (ii)-(iv), the AC4 clause was vague; second re-audit on AC5, so the fix goes to the user.
- 2026-10-18: substantive amendment: AC5 now has `NEWS.md` edit in place the two development-version bullets that name `tidymedia_location_folder_kept` and `tidymedia_location_not_written`, adds no bullet, and carries the second re-audit's five fixes; the user chose this over the planned new bullet at the stop.
- 2026-10-18: T5 done: `?set_program`, `?unset_program` and `?find_ffmpeg` roxygen edited and `devtools::document()` run; the two NEWS bullets edited in place, no bullet added; full `devtools::test()` 0 failures, and `test-timeout-silence.R` passes after the last NEWS edit.
- 2026-10-18: claim audit: 38 claims read, 4 corrected — R/program_management.R (the take-back comment said "removes" for "tries to remove"; `?find_ffmpeg` did not limit the kept-folder sentence to a kept folder; `?set_program`'s "stops at a folder that ... does not go" did not hold where the seam answers 0; the seam comment called waldo testthat's own code).

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive. -->
