# M103: A refused `install_on_win()` leaves the install directory as it found it

**Status:** done (2026-09-03, PR #107 https://github.com/jmgirard/tidymedia/pull/107)

**Goal:** Every refusal `install_on_win()` can take before it registers a program leaves the
install directory holding what it held when the call started — the files a failed extraction
wrote removed or named, the files it found kept, and a directory the call created gone.

**Outcome:** `tm_dir_snapshot()`, `tm_snapshot_added()`, `tm_unlink()` and `tm_remove_added()`
beside `tm_unpack()`, which snapshots its destination before extracting, removes what the
comparison shows a failed extraction added, and returns `list(files, leftovers, removed_yours)`.
A directory counts as created by TYPE, not by path; only the topmost of a chain is targeted, and
files under one the removal could not take are left rather than deleted singly.
`tm_missing_ancestors()`/`tm_remove_created_dirs()` give back the directories `install_on_win()`
created and report what would not go. Both refusals name their real state: leftovers, a file of
the caller's written over and removed, a created directory that would not delete, or nothing.
New under `tests/testthat/`: `test-unpack-cleanup.R` (94 assertions), `helper-unpack-fixtures.R`.

**Decisions:** none milestone-local; D082 extends D046 from one run's designated outputs to a whole destination directory.

**Review:** three passes, three-lens fan-out each. Two defect returns, both AC1 classification
fall-throughs, plus one amendment return. Pass 3 returned nothing: three fix-now defects in what
the refusal SAYS, one of which could delete files outside the destination through a directory
symlink. Windows measured twice — libarchive leaks the failed entry's write handle for the
process lifetime, so removal is best-effort.
