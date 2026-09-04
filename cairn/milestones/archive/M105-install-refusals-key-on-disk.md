# M105: `install_on_win()`'s refusals say what is on disk

**Status:** done (2026-09-03, PR #109 https://github.com/jmgirard/tidymedia/pull/109)

**Goal:** A produced path with nothing on it is refused as a program the archive did not produce, and the helper that judges the paths that do exist is tested directly.

**Outcome:** `tm_files_on_disk()` (`R/program_management.R`) intersects libarchive's reported file list with
what `file.exists()` finds under the install directory, and all four below-extraction sites read that set:
`unpacked_here`, the per-program partition, the empty-extraction guard, the message-arm selector. A required program
the archive listed but never wrote now aborts `tidymedia_program_not_extracted` rather than
`tidymedia_program_unusable`; its message names the full path, names antivirus quarantine as the usual cause of a
listed file not being on disk, and never claims files are still in a directory that has none — the headline splits
on whether every missing program was one the extraction reported, and the give-back arm removes an install
directory this call created. `tm_usable_binary()` became elementwise (`&&` → `&`), dropped its dead
`!is.na(info$size)` clause, and collapsed the call site's `vapply()` to one call.

**Decisions:** D084 (annotates D083).

**Review:** Three-lens fan-out, user-facing tier. Both Sonnet lenses zero findings; the [O] lens thirteen,
mutation-tested. F1, F2, F3, F5, F6, F12 fixed on the branch — F1 the `vanished` intersect being wholly
undiscriminated, so a false quarantine line could ship for a program the archive never listed; F2 a headline
contradicting its own body; F3 M103's exact no-files-at-all wording silently weakened. F4, F10, F11 to one
candidate row; F7 (AC5's vector clause unmet as committed, fixed at review), F8, F9, F13 rejected. Seven
criteria evidenced, eight CI checks green; `codecov/patch` red at 90.24% over the four reworded lines of the
removal-failure arm, accepted at the gate (`codecov/project` green, total coverage unchanged).
