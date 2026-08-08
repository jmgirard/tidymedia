# M62: A missing input file is refused at the front door, in both forms

- **Status:** review
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

- [ ] AC1 — The front door's missing-input abort is written at exactly one
      site. A test walks every function body in `asNamespace("tidymedia")` and
      asserts that abort's wording appears in exactly one, which
      `check_file_exists()` and every front-door sweep reach. `ffm_files()`
      keeps its own separate readability abort, which M63 unifies with this
      site; the same test asserts `ffm_files()` and its `ffm` alias are the only
      places that second wording appears, so the residual is pinned by a test
      rather than assumed.
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
- [x] T5 — Author `data-raw/input-guard-baseline.R` and
      `input_guard_uncovered()` on the D039 pattern
      (`data-raw/value-guard-baseline.R` is the model); run it against both refs.
- [x] T6 — Mutation-verify the reader, the control validator, and the domain
      walk (`data-raw/value-guard-mutations.py` is the model).
- [x] T7 — Write the walk-derived completeness tests for AC1–AC4, reading the
      namespace rather than the source tree (the M51/M59 lesson).
- [x] T8 — D-entry, `NEWS.md`, roxygen for the two newly-guarded verbs; then
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
- 2026-08-08: amendment (substantive, user-approved) - AC1 rewritten. It promised `ffm_files()` reaches the shared abort site, which M62 deliberately does not do: `ffm_files()`'s predicate is readability and unifying the two is M63's scope. The replacement promises one site for the FRONT DOOR and pins the residual with a test asserting `ffm_files()`/`ffm` are the only other place an input refusal is worded.
- 2026-08-08: T7 - walk-derived tests complete; 138 pass. Verified by mutation, not by eye: deleting crop_video_batch's sweep -> 3 red; deleting compare_videos' sweep -> 3 red; duplicating the abort wording into another body -> 1 red; deleting one verb's call-shape spec -> 3 red; degrading the walk to a deparsed-substring match -> `ffm_manifest` re-enters the fan-out set, which its pinning test refuses.
- 2026-08-08: session end at a task boundary; T1-T4 and T7 done, branch pushed, tree clean, suite 4785 pass / 0 fail. Resume at T5 (`data-raw/input-guard-baseline.R`, modelled on `data-raw/value-guard-baseline.R`), then T6 and T8. `data-raw/input-guard-progress.R` is the working checker, not evidence, and is deleted at T8.
- 2026-08-08: T5 — `data-raw/input-guard-baseline.R` generates its cells from three declarations (the walk-derived verb domain, a per-verb crossing list, a per-verb call shape) crossed with the two forms. Measured at this commit by sourcing that file and running its nine readers over `origin/master` and the working tree: 404 cells; vacuous 0 and 0, refusals 0, message regressions 0, blame regressions 0, lost `call` 0, dead controls 0, uncovered 0, misordered 0, with 66 cells' blame moved to the verb the user called. Three declarations were corrected by the grid rather than by eye: six fan-out verbs validate their table inline and reject no NA in the input carrier, so `jobs_na` is declared only where that guard exists and those six pin AC6's upper half through `column_type` instead; `sample_frames_batch` needs a per-row `outdir` or every multi-row cell reports the frame-pattern collision guard; and the `nvenc` crossing must not name `video_codec` on a verb that has none, which raises "unused argument" in place of the crossed error.
- 2026-08-08: T5 (minor amendment) — the crossing declaration names two aborts beyond AC5's colon-list, `jobs_na` and `column_type`, because AC6's first half ("after each fan-out verb's jobs-shape and column-type guards") has no cell without them. AC5's four remain as written; this widens the declaration the reader re-derives from, never narrows it.
- 2026-08-08: T6 — `data-raw/input-guard-mutations.py`; all three AC7 mutations caught, tree restored by the harness after each. Deleting `crop_video_batch`'s call shape made `input_guard_uncovered()` report exactly the 10 combinations it owed (5 crossings x 2 forms); re-pointing the `audio_codec` contradiction's control at `ffm_batch()`'s `run` guard made `input_guard_dead_controls()` report 8 controls, each `reported run_guard`; deleting `strip_metadata_batch`'s `ffm_batch()` call edge moved it out of the walk's fan-out set and into its scalar set. `input_guard_domain()` was narrowed so the missing-shape case reaches the reader instead of a hard error that would have shadowed it.
- 2026-08-08: T8 — D040 appended (licenses the front-door filesystem read; quotes D024's third exclusion verbatim, takes D035's shape and not its licence, discloses the existence-vs-readability residual M63 closes, and answers D036's machine-independence argument rather than ignoring it). `NEWS.md` bug-fix entry, no milestone number. `@param infiles` on `concatenate_videos()` and `compare_videos()` records the new front door. `data-raw/input-guard-progress.R` deleted as planned — it was the working checker, never evidence.
- 2026-08-08: T8 — measured at this commit: `devtools::document()` no diff after regenerating the two `.Rd` files, `devtools::test()` 4785 pass / 0 fail (4 warnings and 5 skips pre-existing: M44 dropped-track warnings, nvenc-absent skips), `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes. `spelling::spell_check_package()` clean after two NEWS words were reworded rather than added to the wordlist.

## Decisions

- **M62-D1: the grid's ordering claim is stated over the after ref alone.**
  `input_guard_misordered()` asserts only where each crossed cell's error
  reports NOW — the crossing for the two guards above the sweep, `input` for
  the four below. It states no expected BEFORE value, because there is no
  single one: a verb that already guarded its input reported `input` before
  too, and the two scalar fan-in verbs reported `ffm_files`'s readability
  refusal for some crossings and the crossing itself for others, depending on
  where their pipeline happened to reach `ffm_files()`. Declaring a
  per-(verb, crossing) `want_before` would be fitting the expectation to the
  measurement. The before ref carries AC5's claims instead — fate, message,
  blame, lost `call` — and `input_guard_ordering()` shows the move as a table
  without asserting one shape for it.

## Review
