# M141: `set_program()` and `unset_program()` clear an empty folder at a location path, and an unreadable location file warns

**Status:** done (2026-10-18, PR #145 https://github.com/jmgirard/tidymedia/pull/145)

**Goal:** A folder at a remembered-location path, or a location file that the lookup cannot read, gives the caller a tidymedia condition with advice that works.

**Outcome:** `find_program()` (`R/program_management.R`) reads the first regular file of `tm_config_paths()`, so a
folder at the current path no longer hides a pre-0.2.0 file. With no file at either path, a folder gives
`tidymedia_location_unreadable`, and its advice names `unset_program()` only when every folder is empty
(`tm_folder_empty()`, which counts hidden entries and treats a folder it cannot read as not empty). A location file
without read permission gives the same class. Both read checks go through `unreadable_paths()`. `set_program()`
removes an empty folder at the current path after consent, checks the folder again after the prompt, and stops with
`tidymedia_location_not_removed` if it stays. `unset_program()` removes empty folders at either path with the files.
Both refuse a folder that is not empty with the new `tidymedia_location_folder` (`tm_program`, `tm_files`), and
`unset_program()` refuses before removing anything. The memo is dropped only when a location file is removed.
`?set_program`, `?unset_program`, `?find_ffmpeg`, `?program_status` and four NEWS entries. Tests in
`test-program-location-repair.R`.

**Decisions:** D104 supersedes D103's delete-by-hand advice and narrows D089's memo discard.

**Review:** One three-lens pass, no returns, no amendments. Criteria audit (full) took 13 findings; claim audit read
64 claims and corrected 6. Review: 20 findings, 7 fixed on the branch, 8 to the new "Location folder edges after
M141" row, 5 rejected. CI green on all 11 checks; the PR conversation held only a Codecov report. Graduated the
"Folder at a remembered-location path in set and unset" and "Unreadable location file" rows; no lesson retired.
