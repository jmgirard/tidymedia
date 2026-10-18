# M143: `unset_program()` forgets a location file beside a folder with contents, and `set_program()` reports an unwritable location

**Status:** done (2026-10-18, PR #147 https://github.com/jmgirard/tidymedia/pull/147)

**Goal:** `unset_program()` forgets a location file beside a folder with contents, and `set_program()` reports an unwritable location with a tidymedia error.

**Outcome:** `tm_set_program()` (`R/program_management.R`) writes through the new `tm_save_location()` seam inside a
handler. A failed `dir.create()` or `writeLines()` becomes the `tidymedia_location_not_written` error (`tm_program`,
`tm_file`, `parent` = R's own error), and the warnings are dropped only when that error follows. A warning-only write
still warns. A file without write permission is refused, not replaced, and the memo stays. `tm_unset_program()` now
refuses with `tidymedia_location_folder` only when every present path is a folder with contents. Otherwise it removes
the files and empty folders, keeps each full folder, and then warns with the new `tidymedia_location_folder_kept`
(`tm_program`, `tm_files`). The memo rules (D089, D104) are unchanged, and `R/cache.R`'s census comment names the
non-discarding cases. `?set_program`, `?unset_program` and `?find_ffmpeg` describe both. The M141 NEWS bullet was
edited in place, and one new bullet was added. Tests are in `test-program-location-repair.R`, with the M141
refuse-first test rewritten.

**Decisions:** D105 supersedes D104's refuse-before-removing rule for `unset_program()`.

**Review:** One three-lens pass, no returns, no amendments. The criteria audit (full) took 10 findings, and the claim
audit read 28 claims and corrected 3. The review found 20 findings: 7 fixed on the branch (the warning-only write was
silenced and the write's cause was discarded, among them), 7 to the new "Location edges after M143" row, and 6
rejected. CI passed on all 11 checks, and the PR conversation held only a Codecov report. Graduated the "Unwritable
location file" row, narrowed "Location folder edges after M141" by its item (a), added one lesson, and retired none.
