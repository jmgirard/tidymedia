# M125: Batch runs refuse two jobs writing one output

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — changes which tables the exported batch verbs, `segment_video()` and `ffm_batch()` accept
- **Branch/PR:** `m125-batch-output-collisions`

## Goal

A call that fans jobs out through `ffm_batch()`, or a direct `ffm_batch()` call, refuses before any job runs when two of its jobs would write the same output path.

## Scope

**In:** An output-collision refusal at the front door of each export reaching `ffm_batch()` that lacks one or checks only some forms — `standardize_video_batch()`, `normalize_audio_batch()` and `anonymize_video_batch()` (caller-supplied `output`), `segment_video_batch()`, `extract_frame_batch()` and `segment_video()` (`outfiles`) — placed above every external-program start in that verb, and `strip_metadata_batch()`'s inline copy routed through `reject_duplicate_outputs()` (`R/ffmpeg.R:5568`). A refusal in `ffm_batch()` itself for direct callers, exempting outputs that write no file. A per-export collision test over the call-graph domain. Help pages and `NEWS.md`.

**Out:**
- Treating `a.mp4`/`./a.mp4`, `~` forms, or case-only differences as one path → new candidate row (added with this plan).
- `ffm_jobs(recursive = TRUE)` returning one file as two rows (a link and its target) → the `ffm_jobs()` candidate row, item (a), trimmed to that half.
- A `tidymedia_*` condition class on the collision abort → the unclassed-aborts candidate row's naming pass.

## Acceptance criteria

- [ ] AC1: Each export that reaches `ffm_batch()` in the package call graph (`tm_call_graph()` and `tm_reaches()` in `tests/testthat/helper-input-paths.R`; 16 exports on 2026-09-11) aborts when two jobs it would build resolve to one destination, compared as exact strings — with the error's call being that export, before any external program starts, under `run = FALSE` and `run = TRUE`, and for `normalize_audio_batch()` under `two_pass = TRUE` as well. The message names the colliding destination, except the derived-output duplicate-input refusal D057 places below the input sweep, which names the duplicated input. The promise covers, per export: a repeat, through each column or argument whose value enters the output path the export hands `ffm_batch()`, that makes two resolved outputs equal; two rows deriving one destination, for each export whose name derivation can give two rows one name; the scalar destination argument where one exists; and for `separate_audio_video_batch()`, a collision within one row and across two rows. A test runs each of those cells; each cell for an export that can start a program before `ffm_batch()` sets the arguments that make it start one (a hardware backend with `fallback = FALSE` and the capability cache cleared, `audio_stream` unset with the track check on, `two_pass = TRUE`).
- [ ] AC2: Each export in AC1's domain accepts the same calls with distinct destinations without a collision refusal — including, where the export has an input column, a table repeating an input with distinct supplied destinations (for a multi-input verb, a whole repeated input row). A test runs a control beside each AC1 cell.
- [ ] AC3: `ffm_batch()` called directly aborts, naming the path, before any job runs, under `run = FALSE` and `run = TRUE`, when two pipelines `.f` returns share an `output` — except outputs that write no file: `-`, a `pipe:` URL, or a pipeline whose output options, split into whitespace tokens across all its option strings in order, have `null` after their last `-f`. A table whose pipelines all take exempt outputs compiles and runs as before, and a table mixing exempt rows with a real repeat still aborts. Tested for each case, with `-f null` given as one string, as two strings, and overridden by a later `-f`.
- [ ] AC4: The help page of each export in AC1's domain, and of `ffm_batch()`, states the refusal in terms of that page's own destination argument or columns (for `ffm_batch()`, the `output` of the pipelines `.f` returns, with the no-file exemptions).
- [ ] AC5: `NEWS.md`'s development section carries one entry stating that every export in AC1's domain and `ffm_batch()` refuse jobs sharing an output path, naming as newly refusing only the exports whose AC1 cells fail on `master`; the existing sentences describing which batch verbs check collisions (`NEWS.md:409-417` on 2026-09-11) are rewritten to match. Every current behaviour either text asserts is one AC1's or AC3's tests exercise.
- [ ] AC6: `devtools::test()` clean and `devtools::check()` 0 errors / 0 warnings.

## Coverage

- AC1 → T1, T2, T3, T5
- AC2 → T1
- AC3 → T4
- AC4 → T6
- AC5 → T6
- AC6 → T7

## Tasks

- [x] T1: Write the collision test first: `tests/testthat/test-batch-output-collision.R` plus a helper declaring one cell per form per export (AC1) and a control per cell (AC2). The domain comes from `tm_reaches(tm_call_graph(), e, "ffm_batch")` over the exports, with a completeness check failing on a domain export that has no cell. Stub the spawn primitives `helper-timeout-sweep.R` stubs and assert none is called; assert the destination in the message and the blamed export via `blamed_verb()` (`helper-blame.R`). Record in the work log which cells are red on `master`.
- [x] T2: Add `reject_duplicate_outputs()` on the resolved outputs of `standardize_video_batch()`, `normalize_audio_batch()` and `anonymize_video_batch()`, above the dropped-track probe (`R/ffmpeg.R:5274`) and the two-pass analysis (`:5318`), leaving `reject_duplicate_inputs()` where D057 puts it; route `strip_metadata_batch()`'s inline check (`:4964`) through the shared helper.
- [x] T3: Add the refusal to `segment_video_batch()`, `extract_frame_batch()` and `segment_video()`'s `outfiles`, moving `segment_video()`'s `outfiles` derivation (`R/ffmpeg.R:3875`) above `check_hardware_available()` (`:3872`); make any cell T1 shows red on `sample_frames_batch()`, `separate_audio_video_batch()` or the other sibling verbs pass.
- [x] T4: In `R/ffm_batch.R`, refuse repeated pipeline outputs after `.f` builds them and before any run, naming the path only, with the exemption rule from AC3; tests for refusal under both `run` values, the all-exempt table (execution cell `skip_if` FFmpeg is absent), and the mixed table.
- [x] T5: Re-run the ordering and blame suites (`test-input-path-front-door.R`, `test-nvenc-front-door.R`, `test-hardware-out-of-table-blame.R`, `test-builder-blame-front-door.R`); where a new refusal moves a pinned precedence, log it in Decisions.
- [ ] T6: Roxygen on the 16 exports and `ffm_batch()` (and a line beside `ffm_jobs()`'s output-derivation example), `devtools::document()`, the `NEWS.md` entry and the rewrite of its existing collision sentences (`NEWS.md:409-417`); at review, read each of the 17 pages against AC4.
- [ ] T7: `devtools::test()` then `devtools::check()`, each run with no other R session working (LESSONS 2026-09-11).

## Work log

- 2026-09-11: created by /milestone-plan.
- 2026-09-11: criteria audit (full mode, fresh [O] reader) returned 8 findings on the first draft; 5 fixed in wording (verb-named call and program-start bound, derived-input message, `ffm_batch()` names no row numbers, NEWS names no verb list, help pages read rather than grepped, per-export control, stubbed spawns) and 3 posed at the gate.
- 2026-09-11: plan gate chose a direct `ffm_batch()` refusal exempting `-`, `pipe:` URLs and `-f null` over checking the verbs only, because `ffm_jobs()`'s example leads callers to `ffm_batch()` directly; falsified by a legitimate direct batch refused for a no-file destination the rule does not exempt.
- 2026-09-11: plan gate chose exact-text path comparison over resolving `~`/relative forms or folding case, because the 8 existing checks compare text and the reported routes produce identical strings; falsified by a report of a batch overwriting through `./`, `~` or a case-only difference.
- 2026-09-11: plan gate chose sweeping every collision form over caller-supplied repeats only, because derived-name and two-file forms reach the same overwrite; falsified by the form cells catching no collision the caller-supplied cells miss.
- 2026-09-11: checkpoint — plan committed while the full-mode re-audit of the gate-changed criteria is still running; its findings land as a follow-up plan commit before implementation.
- 2026-09-11: re-audit (full mode, fresh [O] reader) of the gate-changed criteria returned 7 findings, each with one fix, applied: AC5 rewrites NEWS's existing collision sentences and names only newly refusing exports; AC1 cells set the arguments that start a program, derived-name cells only where derivation can collide, repeats defined by the resolved output path rather than the help page; AC3's `-f null` rule defined over whitespace tokens and the last `-f`, bound before any job runs; AC4 names `ffm_batch()`'s destination; T3 moves `segment_video()`'s `outfiles` derivation above the hardware check. The instrument finding on "a test runs each cell" needed no change. The checkpoint is closed.
- 2026-09-11: implement started on `m125-batch-output-collisions`. Question gate chose placing each new check last in its verb's front door, just above the first program start, over near-the-top parity with the ten existing checks, so no refusal that reports first today reports later.
- 2026-09-11: T1 — `test-batch-output-collision.R` + `helper-batch-output-collision.R`: 30 cells over the 16 exports, each under both `run` values, 121 tests. On the unchanged code 16 fail, the 8 cells planned to lack a check: the output column of `standardize_video_batch()`, `anonymize_video_batch()`, `segment_video_batch()`, `extract_frame_batch()` and `normalize_audio_batch()` (both `two_pass` values), `extract_frame_batch()`'s shared-stem derived name, and `segment_video()`'s `outfiles`. All 60 controls pass, including the per-cell record of which ones start a program before the hand-off. Tick waits for T2/T3 to turn it green.
- 2026-09-11: T2 — `check_distinct_outputs()` is now the one collision abort, `reject_duplicate_outputs()` a caller of it; `standardize_video_batch()`, `anonymize_video_batch()` and `normalize_audio_batch()` call it last before their first program start, and `strip_metadata_batch()`'s inline copy calls the helper. Collision test down to the 8 T3 failures; the four verbs' own test files pass. Tick waits for the full suite after T3.
- 2026-09-11: T3 — `segment_video()` derives `outfiles` above its nvenc probe and refuses a repeat there; `segment_video_batch()` and `extract_frame_batch()` call `reject_duplicate_outputs()` last before hand-off. No sibling-verb cell was red at T1, so nothing else changed. Full `devtools::test()`: 1728 tests, 0 failures, 5 skips — T1-T3 ticked on that run, which included T5's four ordering and blame suites.
- 2026-09-11: T4 — `ffm_batch()` refuses repeated pipeline outputs below its `verify`-spec check and above the first job, on both `run` values, leaving out `-`, `pipe:` URLs and a `null` after the last `-f` (`writes_no_file()`); `check_distinct_outputs()` moved to `R/ffm_batch.R` beside it. 9 tests added, 8 red before the code (the ninth pins the `verify` refusal first), including an FFmpeg run of two `-f null` jobs to `-`. Found in passing: `ffm_batch()`'s not-a-pipeline and `verify`-spec refusals crash inside cli when two or more jobs are at fault — candidate row added. Full `devtools::test()`: 1737 tests, 0 failures, 5 skips.
- 2026-09-11: T5 — `test-input-path-front-door.R`, `test-nvenc-front-door.R`, `test-hardware-out-of-table-blame.R` and `test-builder-blame-front-door.R` passed in both full runs (after T3, after T4): no pinned precedence moved, so no Decisions entry is owed. The unpinned change is the gate's choice: on the six verbs given a new check, a repeated output now reports ahead of any program they start before the hand-off.

## Decisions

## Review
