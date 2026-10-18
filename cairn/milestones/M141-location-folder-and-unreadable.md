# M141: `set_program()` and `unset_program()` clear an empty folder at a location path, and an unreadable location file warns

- **Status:** in-progress
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

- [ ] AC1: Take an empty `PATH`, a folder at the current path, and a file at the pre-0.2.0 path that names a runnable program. Then `find_program()` returns that program's path and gives no warning, and `program_status()` reports that path with no warning. If neither path holds a file and a folder is at one or both paths, `find_program()` gives `tidymedia_location_unreadable`. Its `tm_file` is the first folder in read order.
- [ ] AC2: Take a location file that `file.access(mode = 4)` reports the R process cannot read. The file is at the current path, or at the pre-0.2.0 path with no current file. Then `find_program()` and `program_status()` give `tidymedia_location_unreadable`, with `tm_program` and `tm_file` naming that file. `program_status()` still returns its four-row table. The warning tells the caller to give the file read permission or to use `unset_program()`.
- [ ] AC3: If the current path is an empty folder, `set_program()` with `confirm = FALSE` removes the folder, writes the location file there, and returns `TRUE`. With `confirm = TRUE`, the prompt and the non-interactive refusal both name the empty folder. An approval removes the folder and writes the file, and a decline leaves the folder in place. If the removal does not take, `set_program()` stops with `tidymedia_location_not_removed` and writes nothing. If the folder holds anything, `set_program()` stops with `tidymedia_location_folder`, with fields `tm_program` and `tm_files`. The error names the folder and tells the caller to delete it by hand. That stop comes before any prompt, and the folder and its contents stay as they were.
- [ ] AC4: In one call, `unset_program()` removes every location file and every empty folder at the two paths, and returns `TRUE`. If a path is a folder that holds anything, `unset_program()` stops with `tidymedia_location_folder` before it removes anything. Its `tm_files` lists every such folder, and every file and folder at the two paths stays in place. The call drops the FFmpeg capability memo only when it removes a location file. A call that removes only empty folders keeps the memo.
- [ ] AC5: If every folder at the two paths is empty, the folder warning from `find_program()` tells the caller to use `unset_program()`. Otherwise the warning names each folder that holds anything and tells the caller to delete it by hand.
- [ ] AC6: `?set_program`, `?unset_program`, `?find_ffmpeg` and `?program_status` say what each function does with a folder at a location path. `?find_ffmpeg` and `?program_status` also say what the lookup does with a location file that it cannot read. The development section of NEWS.md describes the fallback past a folder, the unreadable-file warning, and the empty and non-empty folder cases of `set_program()` and `unset_program()`. `devtools::check()` gives 0 errors and 0 warnings.

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
- [ ] T3: Change `tm_unset_program()` (`R/program_management.R:1245-1310`). If a present path is a folder with contents, refuse before any removal. Remove empty folders with the files. Key the memo discard on a removed location file. Write the tests first, in `test-program-status-and-unset.R`. Cover both orientations of a folder and a file, two empty folders, two non-empty folders, the four folder shapes, and the memo in each case.
- [ ] T4: Update the roxygen for `?set_program`, `?unset_program`, `?find_ffmpeg` (`:976-981`) and `?program_status` (`:840-848`). Fold the M140 folder entry in NEWS.md into the new entries. Run `devtools::document()` and `devtools::check()`.

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

## Decisions

## Review
