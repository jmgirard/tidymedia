# M141: `set_program()` and `unset_program()` clear an empty folder at a location path, and an unreadable location file warns

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — it changes exported `set_program()`, `unset_program()`, the `find_*()` lookups and `program_status()`
- **Branch/PR:** m141-location-folder-and-unreadable

## Goal

A folder at a remembered-location path, or a location file that the lookup cannot read, gives the caller a tidymedia condition with advice that works.

## Scope

**In:** Terms used below. The current path is `tm_config_file(program)`. The pre-0.2.0 path is `tm_config_file(program, tm_legacy_config_dir())`. If `Sys.which(program)` is empty, `find_program()` reads the two paths in that order. A folder is empty when it holds no entries, hidden entries and subfolders included.

The milestone changes four things. The lookup falls back past a folder at the current path. The lookup warns about a location file that it cannot read. `set_program()` and `unset_program()` remove an empty folder at a location path and refuse a folder that holds anything. The folder warning names the repair that works for the folders present.

**Out:** `set_program()` over a current location file that it cannot write still stops with the `writeLines()` error ("Unwritable location file" candidate row). The stricter Windows reading of a location stays in the "Three gaps in reading or running a remembered location" row. Removal of a folder with contents stays out at the user's choice (D104).

## Acceptance criteria

- [x] AC1: Take an empty `PATH`, a folder at the current path, and a file at the pre-0.2.0 path that names a runnable program. Then `find_program()` returns that program's path and gives no warning, and `program_status()` reports that path with no warning. If neither path holds a file and a folder is at one or both paths, `find_program()` gives `tidymedia_location_unreadable`. Its `tm_file` is the first folder in read order.
- [x] AC2: Take a location file that `file.access(mode = 4)` reports the R process cannot read. The file is at the current path, or at the pre-0.2.0 path with no current file. Then `find_program()` and `program_status()` give `tidymedia_location_unreadable`, with `tm_program` and `tm_file` naming that file. `program_status()` still returns its four-row table. The warning tells the caller to give the file read permission or to use `unset_program()`.
- [x] AC3: If the current path is an empty folder, `set_program()` with `confirm = FALSE` removes the folder, writes the location file there, and returns `TRUE`. With `confirm = TRUE`, the prompt and the non-interactive refusal both name the empty folder. An approval removes the folder and writes the file, and a decline leaves the folder in place. If the removal does not take, `set_program()` stops with `tidymedia_location_not_removed` and writes nothing. If the folder holds anything, `set_program()` stops with `tidymedia_location_folder`, with fields `tm_program` and `tm_files`. The error names the folder and tells the caller to delete it by hand. That stop comes before any prompt, and the folder and its contents stay as they were.
- [x] AC4: In one call, `unset_program()` removes every location file and every empty folder at the two paths, and returns `TRUE`. If a path is a folder that holds anything, `unset_program()` stops with `tidymedia_location_folder` before it removes anything. Its `tm_files` lists every such folder, and every file and folder at the two paths stays in place. The call drops the FFmpeg capability memo only when it removes a location file. A call that removes only empty folders keeps the memo.
- [x] AC5: If every folder at the two paths is empty, the folder warning from `find_program()` tells the caller to use `unset_program()`. Otherwise the warning names each folder that holds anything and tells the caller to delete it by hand.
- [x] AC6: `?set_program`, `?unset_program`, `?find_ffmpeg` and `?program_status` say what each function does with a folder at a location path. `?find_ffmpeg` and `?program_status` also say what the lookup does with a location file that it cannot read. The development section of NEWS.md describes the fallback past a folder, the unreadable-file warning, and the empty and non-empty folder cases of `set_program()` and `unset_program()`. `devtools::check()` gives 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T3
- AC5 → T1
- AC6 → T4

## Tasks

- [x] T1: Change the lookup in `find_program()` (`R/program_management.R:136-171`). Read the first of the two paths that is a regular file. If neither path is a regular file, warn about the first folder. Base the advice on the emptiness of every folder at the two paths, with `list.files(all.files = TRUE, no.. = TRUE)`. Ask `file.access(config, 4)` before `readLines()`. Update the folder comment at `:147-155`. Write the tests first, in `test-program-location-repair.R`. Cover a folder at the current path only, at the pre-0.2.0 path only, at both, and beside a file at the other path, plus an empty and a non-empty folder in the mixed case. The unreadable-file tests use `Sys.chmod(path, "0000")`. They skip on Windows, and where `file.access()` still reports the file readable, as it does for root.
- [x] T2: Change `tm_set_program()` (`R/program_management.R:1109-1163`). Refuse a folder with contents at the current path before the consent prompt, with the new `tidymedia_location_folder` class. Name an empty folder in `tm_set_details()` and `tm_set_prompt()` (`:1952-1975`). After consent, remove the folder through `tm_unlink(recursive = TRUE)`. Check that it is gone before the write. Write the tests first. Test the four folder shapes: empty, holding a file, holding only a hidden file, and holding only an empty subfolder. Test approve and decline through a `tm_confirm` mock, and the refusal with no one to ask. Test a removal that does not take through a `tm_unlink` mock. Replace the test "neither unset_program() nor set_program() clears a folder at the current path" (`test-program-location-repair.R:403`).
- [x] T3: Change `tm_unset_program()` (`R/program_management.R:1245-1310`). If a present path is a folder with contents, refuse before any removal. Remove empty folders with the files. Key the memo discard on a removed location file. Write the tests first, in `test-program-status-and-unset.R`. Cover both orientations of a folder and a file, two empty folders, two non-empty folders, the four folder shapes, and the memo in each case.
- [x] T4: Update the roxygen for `?set_program`, `?unset_program`, `?find_ffmpeg` (`:976-981`) and `?program_status` (`:840-848`). Fold the M140 folder entry in NEWS.md into the new entries. Run `devtools::document()` and `devtools::check()`.

## Work log

- 2026-10-17: created by /milestone-plan.
- 2026-10-17: tier check: not run — work named at the question set.
- 2026-10-17: question set: which work to plan — the location file edges ("Folder at a remembered-location path in set and unset" and "Unreadable location file" rows, absorbed in full).
- 2026-10-17: question set: what set and unset do with a folder at a location path — remove an empty folder, refuse one with contents (D104).
- 2026-10-17: inbox sweep: no open issues or PRs on jmgirard/tidymedia, so the Resolves slot is empty.
- 2026-10-17: collision check: D103's falsifier fired with the user's folder choice, so D104 supersedes its advice clause. D088 and D089 read whole. M140 is done and shipped the lookup warning only.
- 2026-10-17: criteria audit (full mode, fresh Opus reader) returned 13 findings, all fixed. AC5 now advises `unset_program()` only when every folder is empty. "Empty" counts hidden entries and subfolders. AC6 adds `?program_status`. AC2 lost its platform and test-mechanics wording, moved to T1. The goal narrowed to the lookup for the unreadable file, with the unwritable set case as a candidate row. AC4 names `tm_files` and the memo rule. AC3 covers a removal that does not take and all three consent paths. AC1 covers `program_status()`, with test layouts in T1. T2 replaces the M140 test that asserts the old behavior.
- 2026-10-17: plan gate chose a `file.access(mode = 4)` check before `readLines()` over a `tryCatch()` around it, because the check gives advice for one known cause; falsified by a read failure on a file that `file.access()` reports readable.
- 2026-10-17: plan gate chose a new `tidymedia_location_folder` error class over reusing `tidymedia_location_not_removed`, because a folder with contents is a refusal and not a failed removal; falsified by a caller who handles both the same way and needs one class.
- 2026-10-17: plan gate chose that `unset_program()` refuses before any removal over removing what it can and then refusing, because a partial unset changes which location the lookup answers with; falsified by a caller who needs the file at the other path gone while the folder stays.
- 2026-10-17: ROADMAP.md is over its 24,000-byte budget, and this plan adds two rows. The two absorbed rows graduate at merge.
- 2026-10-17: implement started on branch m141-location-folder-and-unreadable.
- 2026-10-17: T1 done. `find_program()` reads the first regular file of `tm_config_paths()`, warns on folders with advice from `tm_folder_empty()`, and warns on an unreadable file. Both read checks go through `unreadable_paths()`, because `test-input-path-front-door.R`'s census keeps readability at one site. The M140 folder test now covers seven layouts. `devtools::test()` clean.
- 2026-10-17: T2 done. `tm_set_program()` refuses a folder with contents through the new `tm_abort_location_folder()`, names an empty folder in the consent details, checks the folder again after consent because the removal is recursive, and stops with `tidymedia_location_not_removed` if it stays. The M140 test at `test-program-location-repair.R:403` is replaced by four set tests. `devtools::test()` clean.
- 2026-10-17: T3 done. `tm_unset_program()` refuses through `tm_abort_location_folder()` before any removal, removes empty folders in the same `tm_unlink(recursive = TRUE)` call, and drops the memo only for a removed location file. The unset folder tests went to `test-program-location-repair.R`, not `test-program-status-and-unset.R` as T3 named, because they share `tm_folder_shapes` and the memo helpers defined there. `devtools::test()` clean. This commit also carries T4's roxygen edits, which the suite run did not depend on.
- 2026-10-17: claim audit: 64 claims read, 6 corrected — R/program_management.R, NEWS.md, man/find_ffmpeg.Rd, man/program_status.Rd, man/set_program.Rd, man/unset_program.Rd, tests/testthat/test-program-location-repair.R
- 2026-10-17: the six corrections: `readLines()` warns and then stops with "cannot open the connection"; a folder tidymedia cannot read never counts as empty, now in the docs, the error text, NEWS and a new test; `set_program()` clears an empty folder at the current path only; the empty-folder advice counts two folders; the lookup test gained the `both_full` layout.
- 2026-10-17: `devtools::check()` on 217abdfc gave 0 errors, 0 warnings, 0 notes. The spelling comparison lists "encodable" and "filesystem" from text released in 0.2.2, not from this branch.
- 2026-10-17: T4 done. `devtools::check()` on a78913b8, after the claim-audit fixes, gave 0 errors, 0 warnings, 0 notes, with the test suite inside it. Status set to review.

## Decisions

## Review

Evidence gathered 2026-10-17 on branch head 0dcaf9b0, master unmoved. `test-program-location-repair.R` run alone: 26 tests, 0 failed, 0 skipped.

- AC1: `test-program-location-repair.R:180` passes (32 expectations). For each of the four programs, with an empty and a non-empty folder at the current path and a stub at the pre-0.2.0 path, `find_*()` returns the stub with no warning, and `program_status()` reports it with no warning. `:114` passes (280 expectations). Over eight folder layouts with no file at either path, the warning is `tidymedia_location_unreadable`, and `tm_file` is the first folder in read order.
- AC2: `:209` passes (72 expectations, not skipped on this macOS run). A mode-0000 file at the current path, and one at the pre-0.2.0 path with no current file, gives `tidymedia_location_unreadable` with `tm_program` and `tm_file` from `find_*()`. `program_status()` returns four rows with that one warning. The message holds "read permission" and `unset_program("<program>")`.
- AC3: `:525` (12), `:541` (42), `:604` (10), `:632` (4) and `:651` (5) pass. An empty folder is replaced with `confirm = FALSE`. Three non-empty shapes give `tidymedia_location_folder` with `tm_program` and `tm_files` and the text "by hand", under `confirm = TRUE` with no one to ask, so before the prompt, and the contents stay. Approve writes the file, and decline keeps the folder. The prompt and the refusal both name the empty folder. A `tm_unlink` that removes nothing gives `tidymedia_location_not_removed`, and nothing is written.
- AC4: `:700` (60), `:723` (96) and `:759` (4) pass. Five layouts of empty folders and files are all removed with `TRUE`. Three non-empty shapes at each path, beside nothing, a file, an empty folder or a second non-empty folder, give `tidymedia_location_folder`, `tm_files` lists every non-empty folder, and the trees are unchanged. An empty-folder-only removal keeps the memo, and one with a file drops it.
- AC5: `:114` passes. When every folder is empty, the warning holds `unset_program("<program>")` and not "by hand". When any folder holds something, it names each such folder and "by hand", and not `unset_program`.
- AC6: The rendered `?set_program`, `?unset_program`, `?find_ffmpeg` and `?program_status` (`tools::Rd2txt`) each state the empty-folder removal and the refusal of a folder that is not empty. `?find_ffmpeg` and `?program_status` state the `tidymedia_location_unreadable` warning for a file without read permission. NEWS.md's development section has entries for the fallback past a folder (line 14), the unreadable-file warning (line 19), and the empty and non-empty cases of both functions (lines 5 and 24). `devtools::check()` on 7406b857, after the review fixes, gave 0 errors, 0 warnings, 0 notes in 8m 23s.
- Gate: `cairn_validate.py` all checks passed. `devtools::document()` writes nothing. `pkgdown::check_pkgdown()` finds no problems. README is not touched. No principle changed, so `cairn_impact` is skipped. NEWS.md has entries for the user-visible changes.
- spawned: diff-bug, blame-history, prior-review

Findings (20) and dispositions:

- diff-bug #1: the unreadable, malformed, not-executable and gone warnings advise `unset_program()`, which refuses when the other path holds a non-empty folder — follow-up ("Location folder edges after M141" row, item a). The refusal names the folder, so the caller still has a next step.
- diff-bug #2: the emptiness check and the recursive removal leave a window in which a new entry is deleted, and `unset_program()` has no second check — follow-up (row item b). It needs a removal that fails on a non-empty folder on every OS.
- diff-bug #3: if the folder vanishes during the prompt, the second check refuses with "a folder is there" — fix now, fixed 7406b857.
- diff-bug #4: no test reaches the second check after consent — fix now, fixed 7406b857.
- diff-bug #5: the folder error is singular when it lists two folders — fix now, fixed 7406b857.
- diff-bug #6: `?set_program` leaves out the `tidymedia_location_not_removed` stop for a folder that will not go — fix now, fixed 7406b857.
- diff-bug #7: NEWS says both functions remove an empty folder without saying `set_program()` clears the current path only, and one NEWS line runs past 100 characters — fix now, fixed 7406b857.
- diff-bug #8: the read-permission tests skip under root — reject (false as a defect): no test can make a file unreadable to root, and the GitHub runners are not root.
- diff-bug #9: the prompt test matches the path, which the "By writing:" line also holds — fix now, fixed 7406b857.
- diff-bug #10: a link to a folder is handled as the folder, and the prompt says "Remove the empty folder" when only the link goes — follow-up (row item d). Both outcomes are safe.
- diff-bug #11: a FIFO or socket at a location path counts as a file, and `readLines()` on a FIFO blocks — follow-up (row item e). This predates M141.
- diff-bug #12: the empty-folder advice says `unset_program()` then `set_*()`, though `set_*()` alone works — fix now, fixed 7406b857.
- blame-history #1: a non-empty folder at the current path no longer warns when a pre-0.2.0 file is read — reject (planned change): AC1 promises the fallback for any folder there, and `?find_ffmpeg` states it.
- blame-history #2: `unset_program()` cannot forget a valid current file while the other path holds a non-empty folder — follow-up (row item a), the falsifier the plan recorded for refusing before any removal.
- blame-history #3: `unset_program()` has no second emptiness check — follow-up (row item b), the same as diff-bug #2.
- blame-history #4: `install_on_win()` can stop after the first program on a non-empty folder at a later path, and its prompt does not name an empty-folder removal — follow-up (row item c). Before M141 it stopped there too, with R's error.
- prior-review #1: the same `install_on_win()` point, against M104's all-or-none registration and M110's prompt — follow-up (row item c).
- prior-review #2: M097's fallback was keyed on existence, and a folder now falls through — reject (planned change): AC1.
- prior-review #3: the read-permission tests skip under root — reject (false as a defect), as diff-bug #8.
- prior-review #4: the memo is kept on a folder-only removal, narrower than D089 — reject (planned change): AC4 and D104 record it.
