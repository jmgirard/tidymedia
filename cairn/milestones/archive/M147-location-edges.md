# M147: A failed `unset_program()` removal or `set_program()` write accounts for the folders it touched

**Status:** done (2026-10-19, PR #151 https://github.com/jmgirard/tidymedia/pull/151)

**Goal:** A failed removal in `unset_program()` or a failed write in `set_program()` names or takes back each folder that the call kept, created or removed.

**Outcome:** `tm_unset_program()`'s `tidymedia_location_not_removed` abort gets a last "Kept the folder" line when a
folder with contents was kept beside the failed removal. `tm_set_program()` reads
`tm_missing_ancestors(config_dir, normalize = FALSE)` before the `tm_save_location()` seam; on a failed write it runs
`tm_remove_created_dirs()` and names each ancestor `dir.exists()` still finds ("Could not remove the folders it
created"). A `removed` flag set where the empty-folder removal runs adds a "Removed the empty folder" line.
`tm_missing_ancestors()` gained `normalize` (default `TRUE`, so `install_on_win()` is unchanged) so the names match
the raw config path on Windows. The `tm_save_location()` comment names its two stand-in writes. `?set_program`,
`?unset_program` and `?find_ffmpeg` describe it; the two development-version NEWS bullets that name
`tidymedia_location_folder_kept` and `tidymedia_location_not_written` were edited in place, no bullet added (D091).
Tests are in `test-program-location-repair.R`.

**Decisions:** D109 (a failed write takes back created folders, a removed empty folder stays removed). AC5 was
amended at implement, by the user's choice at a stop after two re-audits, to edit the NEWS bullets in place.

**Review:** One four-lens pass and a resume pass after a CI-wait stop; no returns. The criteria audit (full) took 8
findings, and the claim audit read 38 claims and corrected 4. The review found 22: 7 fixed on the branch (the Windows
path spelling in the created-folder names among them), 5 to the new row "Location write edges after M147", 10
rejected. CI passed on all 11 checks, and the PR held only a Codecov report. Graduated "Location edges after M143",
extended the M105 Windows-path lesson, and retired none.
