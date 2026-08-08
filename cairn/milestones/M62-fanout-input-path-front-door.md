# M62: A missing input file is refused at the front door, in both forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M61
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m62-fanout-input-path-front-door`

## Goal

Make a call naming an input file that does not exist report against the verb the
user called, in both the table-driven and the scalar form.

## Scope

**In:** One shared, vector-capable missing-input checker, arity-templated so a
one-path call renders byte-identically to today's `check_file_exists()` message.
Every exported verb that fans out through `ffm_batch()` sweeps its resolved
input paths at the front door — the `input` column, the `inputs` list-column,
and `picture_in_picture_batch()`'s `main`/`overlay` pair. The two scalar fan-in
verbs `concatenate_videos()` and `compare_videos()`, which have no front-door
input guard at all today, gain one. A `cairn/DECISIONS.md` entry licensing a
filesystem read at a verb's front door under D024's third exclusion. The sweep's
precedence fixed and measured against every front-door abort it now precedes.

**Out:** The existing-but-unreadable input, which `check_file_exists()`'s
existence predicate does not catch and `ffm_files()`'s readability predicate
does → M63 (planned, depends on this). Output paths and `outdir` creation →
unchanged, no row. Any verb outside the derived fan-out and `ffm_files`-reaching
sets — the metadata side (`probe_all()`, `mediainfo()`) — → no row; those never
reach `ffm_files()`.

## Acceptance criteria

- [ ] AC1 — The missing-input abort is written at exactly one site. A test
      derives every function in `asNamespace("tidymedia")`, walks each parsed
      body for call nodes (never substring matches on deparsed text, so a name
      inside a `cli` string literal is not a hit), and asserts exactly one body
      raises it; `check_file_exists()` and `ffm_files()` both reach it.
- [ ] AC2 — For a one-path call the shared checker's rendering is byte-identical
      to the string `check_file_exists()` emits on merged master, asserted by a
      snapshot recorded against the pre-change ref; for a multi-path call it
      names every missing path, not the first.
- [ ] AC3 — Every exported verb whose parsed call graph transitively reaches
      `ffm_batch` refuses a missing input at its front door, with
      `conditionCall()` naming that verb. The domain is derived by that walk and
      never listed: the test fails when a verb the walk returns has no
      call-shape spec, so the walk fixes membership and the spec supplies only
      the shape of a legal call.
- [ ] AC4 — Every exported verb whose parsed call graph transitively reaches
      `ffm_files` but not `ffm_batch` refuses a missing input at its own front
      door, with `conditionCall()` naming that verb — the same walk-derived,
      spec-required construction. This is what makes `concatenate_videos()` and
      `compare_videos()` stop reporting `Error in ffm_files(infiles, outfile)`.
- [ ] AC5 — `data-raw/input-guard-baseline.R` generates its cells from declared
      axes rather than hand-written rows, crossing each verb and form with each
      front-door abort named in its declaration: the M58 contradiction sweep,
      `check_nvenc_available()`, `ffm_batch()`'s `run` guard, and the four value
      guards D039 moved (`direction`, `position`, `margin`, the per-row `audio`
      bound). A companion `input_guard_uncovered()` re-derives the same product
      and reports any declared combination with no cell. Measured over both
      refs: no call's refused-or-accepted status changes, no message regresses,
      no abort loses its `call`, and every cell's crossed error is shown live by
      a paired control.
- [ ] AC6 — The sweep runs after each fan-out verb's jobs-shape and column-type
      guards and before its M58 contradiction sweep, pinned by the AC5 cells
      that cross the two.
- [ ] AC7 — `input_guard_uncovered()`, the control validator, and the domain
      walk are each verified by mutation: deleting one verb's spec makes the
      reader report every combination that verb owed, re-pointing a control at a
      different error makes it fail, and deleting a call edge changes the
      derived verb set.
- [ ] AC8 — A `cairn/DECISIONS.md` entry licenses a filesystem read at a verb's
      front door, quoting D024's third exclusion verbatim, taking D035's *shape*
      and not its licence (D035's rule is conditioned on a probe whose result
      enters the compiled command; this one's does not), stating its conditions
      and what it does not license, and recording the M63 residual.
- [ ] AC9 — `NEWS.md` records the blame move in user-facing terms with no
      milestone number, and a named test fails without the behavior it asserts.
- [ ] AC10 — `devtools::document()` produces no diff, `devtools::test()` and
      `devtools::check()` are clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T7
- AC2 → T1, T7
- AC3 → T2, T3, T7
- AC4 → T4, T7
- AC5 → T5
- AC6 → T2, T3, T5
- AC7 → T6
- AC8 → T8
- AC9 → T8
- AC10 → T8

## Tasks

- [x] T1 — Add the shared vector-capable checker beside `check_file_exists()`
      (`R/utils.R:26-37`); make `check_file_exists()` delegate to it so the
      one-path rendering is unchanged. Snapshot the pre-change message first.
- [x] T2 — Wire the per-row sweep into the two shared jobs validators —
      `check_batch_jobs()` (`R/ffmpeg.R:4310`) and `check_fanin_jobs()`
      (`R/ffmpeg.R:4563`) — and into `picture_in_picture_batch()`'s inline
      `main`/`overlay` block (`R/ffmpeg.R:6176`).
- [x] T3 — Wire it into the fan-out verbs that validate their jobs table inline
      rather than through those two helpers: the sites at `R/ffmpeg.R:1822`,
      `:3155`, `:3319`, `:3478`, `:3705`, `:3903`, `:4102`, plus `segment_video()`
      and `normalize_audio(two_pass = TRUE)`, which reaches `ffm_batch` through
      `run_normalize_correction()`.
- [x] T4 — Add the front-door guard to `concatenate_videos()` and
      `compare_videos()`.
- [ ] T5 — Author `data-raw/input-guard-baseline.R` and
      `input_guard_uncovered()` on the D039 pattern
      (`data-raw/value-guard-baseline.R` is the model); run it against both refs.
- [ ] T6 — Mutation-verify the reader, the control validator, and the domain
      walk (`data-raw/value-guard-mutations.py` is the model).
- [ ] T7 — Write the walk-derived completeness tests for AC1–AC4, reading the
      namespace rather than the source tree (the M51/M59 lesson).
- [ ] T8 — D-entry, `NEWS.md`, roxygen for the two newly-guarded verbs; then
      `document()` / `test()` / `check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned 7 findings. Six had one right answer and were fixed before the gate: vector-capable single abort site (AC1/AC2), crossing axes narrowed to what the grid declares and widened to D039's four value guards (AC5), domain derived by parsed call-node walk rather than deparsed substring — fixing a measured false positive (`ffm_manifest`, matched inside a cli string) and a measured false negative (`normalize_audio(two_pass = TRUE)`) (AC3/AC4), aggregate-vs-first-path message made a measured property (AC2), D035 cited for shape not licence (AC8), third mutation over the domain walk (AC7). The seventh became gate question 2.
- 2026-08-08: plan gate chose the existence predicate over `ffm_files()`'s readability predicate because it reuses the wording thirteen scalar verbs already emit and keeps both forms uniform, at the cost of an existing-but-unreadable residual; falsified by a report of an unreadable-but-present input reporting differently by form before M63 ships.
- 2026-08-08: plan gate chose placing the sweep above the M58 contradiction sweep over placing it below, because a caller who mistyped a path should hear about the path and this matches where the scalar verbs' guard already sits; falsified by a report preferring the contradiction on a table that is both wrong about a path and self-contradictory. The alternative carried D036's machine-independent-first reasoning, which the M62 D-entry must therefore address rather than ignore.
- 2026-08-08: T2-T4 - the sweep wired into 15 `_batch` verbs plus the 2 scalar fan-in verbs, delegated to an [S] agent against an objective checker; diff reviewed site by site. 30/30 verbs now refuse at their own front door; suite unchanged at 4658 pass / 0 fail. Two spec bugs found and fixed first (anonymize's `regions` cell shape, extract_frame's `timestamp` column) - both were aborting for their own reason and reading as passes, the M54 vacuity trap.
- 2026-08-08: T7 (part) - walk-derived completeness tests added, including one pinning that `ffm_manifest` is excluded because its `ffm_batch(` occurrence is inside a cli string.
- 2026-08-08: plan chose a generated cross-product grid over hand-written per-verb tests because M61's three review returns were each a combination nobody typed; falsified by the grid's declaration itself dropping a crossing, which its reader cannot catch.
- 2026-08-08: implement gate chose the count-first plural rendering over one always-pluralized sentence, and `jobs$input` over the package's "the `input` column of `jobs`" phrasing, so both forms share one sentence shape; both prototyped against cli before the chip.
- 2026-08-08: T1 — `check_paths_exist()` added at `R/utils.R:26`; `check_file_exists()` delegates its existence half. One-path rendering pinned byte-for-byte against strings captured from master before the change. Suite 4658 pass / 0 fail; the 4 warnings and 5 skips are pre-existing (M44 dropped-track warnings, nvenc-absent skips).

## Decisions

## Review
