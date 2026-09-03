# M103: A refused `install_on_win()` leaves the install directory as it found it

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** —

## Goal

Every refusal `install_on_win()` can take before it registers a program leaves the install directory holding what it held when the call started — the files a failed extraction wrote are removed or named, the files it found are kept, and a directory the call created is gone.

## Scope

The deliverable is **user-facing**: `install_on_win()` is exported and a caller reads the on-disk state a refusal leaves behind.

**In:** (i) `tm_unpack()` snapshots its destination before extracting and, where libarchive fails, removes the entries that extraction created or changed and reports any it could not delete (D046's rule, applied set-wise to a directory); (ii) `install_on_win()` removes the directories it created when a refusal below `dir.create()` leaves them empty; (iii) the `tidymedia_archive_unreadable` refusal names anything left behind; (iv) a `cairn/DECISIONS.md` entry extending D046 to the installer.

**Out:** the `tidymedia_program_not_extracted` path, whose extraction SUCCEEDED and whose shipped message deliberately tells the caller the unpacked files remain — the boundary is stated in AC4's entry, not deferred. Multi-digest `sha256sum` sidecar parsing → the M102 candidate row, whose promotion condition (a source publishing one) is unmet. Partial registration through `set_program()` → its own candidate row. Archive provenance/substitution → its own candidate row. Re-cutting `data-raw/corrupt-archive-fixtures.R`, which bakes the generator's absolute temp path into the entry name → the test derives entry paths from `archive::archive()` instead.

## Acceptance criteria

- [ ] AC1: `tm_unpack()` snapshots its destination recursively (path, size, mtime) before extracting and, where `archive::archive_extract()` fails, takes a second snapshot and removes every entry the comparison shows this extraction created or changed — files with `unlink(expand = FALSE)`, directories with `unlink(recursive = TRUE, expand = FALSE)` — leaving every untouched entry byte-identical with its mtime unmoved. Removal is best-effort: an entry that will not delete is returned to the caller rather than silently dropped.
- [ ] AC2: AC1 is verified over the two failure routes the committed fixtures reach — `not-an-archive.7z` refused at open (writing nothing) and `corrupt-payload.7z` failing mid-read after bytes are written — by tests driving real `archive::archive_extract()` calls, no mock, each route against an empty destination, a destination holding a file at a path that fixture's own `archive::archive()` listing shows it writes (where it writes any), a destination holding a file at a path it does not write, and a destination holding a nested subdirectory; plus one cell mocking the removal seam to fail, asserting the undeletable entry is reported.
- [ ] AC3: Every refusal `install_on_win()` can take above its `tm_unpack()` call leaves no directory the call created, at or above `install_dir`, and leaves an already-existing `install_dir`'s entries unchanged. The domain is enumerated by the M102 AC6 census (`tests/testthat/test-program-management.R:1216-1236`) narrowed by a positional filter to the `return()` and `cli_abort()` nodes preceding the `tm_unpack()` call, with the test asserting a bijection between those nodes and its registry of triggering cases so a node with no case fails it. The four front-door refusals that walk cannot see — `rlang::check_bool(confirm)`, `rlang::check_string()` on `download_url` and on `install_dir`, `check_sha256()` on `archive_checksum` — and `tm_confirm()`'s non-interactive refusal each get their own case in that registry; each refuses above the call's first `dir.create()`, so each must create no directory at all.
- [ ] AC4: A `cairn/DECISIONS.md` entry extends D046 to the installer: it states the rule, states that removal is best-effort and that a failed removal is named in the refusal rather than swallowed, names `tidymedia_program_not_extracted` as the path the rule deliberately does not cover and why, and states a falsifier.
- [ ] AC5: `man/install_on_win.Rd` states both the general rule — a refusal leaves the install directory as it found it, naming anything it could not remove — and the single exception, `tidymedia_program_not_extracted`, and is byte-identical to a fresh `devtools::document()` run; `NEWS.md` records the change.
- [ ] AC6: `devtools::check()` reports 0 errors and 0 warnings, with every NOTE drawn from the known-acceptable set the review evidence lists and any other NOTE failing this criterion; `devtools::test()` reports 0 failures; and all six `R-CMD-check` matrix jobs are green, `windows-latest (release)` and `ubuntu-latest (4.1.0)` included.

## Coverage

- AC1 → T1, T3, T6
- AC2 → T1, T2, T3, T6
- AC3 → T4, T5
- AC4 → T7
- AC5 → T8
- AC6 → T9

## Tasks

- [ ] T1: Add two internals beside `tm_unpack()` (`R/program_management.R:405`): `tm_dir_snapshot(dir)`, returning path/size/mtime for every entry under `dir` recursively including directories and dotfiles, and `tm_unlink()`, a thin wrapper over `unlink()` that exists so the suite can mock a removal failure.
- [ ] T2: Tests first, all red before T3: AC2's matrix over both fixtures × the four starting states, with entry paths read from `archive::archive()` rather than hard-coded (the fixture stores an absolute generator path, so `strip_components = 1` writes a deep name), plus the mocked-`tm_unlink()` cell.
- [ ] T3: `tm_unpack()` snapshots before extraction, re-snapshots on failure, removes the created-or-changed entries files-first then directories, and returns the undeletable leftovers to its caller instead of a bare `NULL` — keeping `NULL` as the "extraction failed" signal by returning the leftovers in an attribute or a two-slot list, whichever leaves `install_on_win()`'s existing `is.null(produced)` test intact.
- [ ] T4: Tests first, all red before T5: extend the M102 census helper with the positional filter and the bijection assertion, and add the five uncoverable-by-walk cases AC3 names.
- [ ] T5: `install_on_win()` records what `dir.create(recursive = TRUE)` created (`R/program_management.R:608-611`) and, on any refusal below it, removes those directories deepest-first, stopping at the first that is not empty.
- [ ] T6: The `tidymedia_archive_unreadable` abort (`R/program_management.R:687-696`) names the leftovers T3 reports; tests for both fixtures and for the created-parent case.
- [ ] T7: Write AC4's `cairn/DECISIONS.md` entry.
- [ ] T8: `@details`/`@return` on `install_on_win()`, `devtools::document()`, `NEWS.md` bullet.
- [ ] T9: `devtools::check()`, full `devtools::test()`, push and confirm all six R-CMD-check jobs.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: plan gate chose best-effort removal with the refusal naming what remains over unconditional removal, because libarchive may still hold the partial file open on Windows — the leak that cost M102 a red `windows-latest` — and the handle here belongs to `archive_extract()`'s writer, which cannot be measured off Windows; falsified by a measurement showing the writer handle closed on the failure path, which would make the unconditional promise honest.
- 2026-09-02: plan gate chose leaving `tidymedia_program_not_extracted` outside the rule over one unconditional rule, because that extraction succeeded and D046's rule is about what a FAILED run wrote, and the shipped abort already promises the files remain; falsified by a report of a caller surprised to find a complete unwanted extraction left in the install directory.
- 2026-09-02: plan gate chose keeping `dir.create()` above the digest fetch and removing the directory on refusal over moving `dir.create()` below the download, because the second trades fail-fast on an unwritable directory for a several-hundred-megabyte download before the same refusal; falsified by a refusal path where the created directory cannot be identified for removal.
- 2026-09-02: plan gate chose leaving the multi-digest sidecar (M102 deferral (b)) as a candidate row over folding it in, because its promotion condition — a source publishing a `sha256sum` manifest — is unmet and the fix would be guesswork about a format nothing in play emits; falsified by a source publishing one.
- 2026-09-02: criteria audit ran in FULL mode (declared surface tier user-facing), two rounds, fresh-context [O] reader both times. Round 1 returned findings on AC1 (probe variety, unbounded domain), AC2 (instrument-bound, incomplete exit list), AC2/AC3 (leaf-only directory claim), AC3 (vacuous satisfaction), AC5 and AC6 (unbounded universals); all repaired, and the Windows-handle risk it raised became the gate's first question. Round 2 on the repaired wording returned findings on all five re-audited criteria and three factual corrections: the M102 census walks both node types but has no positional filter (`test-program-management.R:1216-1236`), `rlang::check_bool(confirm)` (`R/program_management.R:549`) was a fourth uncoverable exit the draft omitted, and the CI matrix has six jobs not five (`.github/workflows/R-CMD-check.yaml:21-28`, including the `4.1.0` floor job). All repaired into the wording above; AC1's changed-entry contradiction resolved by D046's own created-or-changed split.

## Decisions

## Review
