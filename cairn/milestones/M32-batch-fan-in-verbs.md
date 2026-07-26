# M32: Batch siblings for the fan-in verbs (`concatenate_videos`/`compare_videos`/`picture_in_picture` `_batch`)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3
- **Branch/PR:** m32-batch-fan-in-verbs · https://github.com/jmgirard/tidymedia/pull/34

## Goal

Add table-driven `_batch` siblings for the three fan-in (many-inputs → one-output) verbs, completing the batch-coverage family after M28 (single-io) and M29 (fan-out).

## Scope

**In:** `concatenate_videos_batch()`, `compare_videos_batch()`,
`picture_in_picture_batch()` as thin Layer-2 wrappers over `ffm_batch()`
(D007), each sharing an extracted pipeline helper with its scalar sibling.
Jobs-table input shape (D015): concat/compare take an `inputs` **list-column**
(each cell a character vector) + `output`; PiP takes fixed `main`/`overlay` +
`output`. Optional per-row override columns fall back to the scalar arg
(compare: `direction`/`resize`/`audio`; PiP: `position`/`scale`/`margin`/`audio`).
Reuse `reject_duplicate_outputs`; forward `run`/`parallel`/`verify`/`manifest`/
`progress` through `...`.

**Out:** any `ffm_batch()` engine change (pmap already passes list-columns; the
manifest already joins multi-input with `";"`) — none needed. No new
multi-output model (reaffirms D007/IP2). No `hardware=`/quality knobs → their
own candidates.

## Acceptance criteria

- [x] AC1: `concatenate_videos_batch(jobs)` compiles one concat command per row
      from an `inputs` list-column + `output`; returns jobs + `command`
      (+ `success` when run). Compile test, binary-free.
- [x] AC2: `compare_videos_batch(jobs)` compiles per-row hstack/vstack commands,
      honoring optional `direction`/`resize`/`audio` columns with scalar-arg
      fallback. Compile + override test.
- [x] AC3: `picture_in_picture_batch(jobs)` compiles per-row overlay commands
      from fixed `main`/`overlay`/`output`, honoring optional
      `position`/`scale`/`margin`/`audio` columns with fallback. Compile + override test.
- [x] AC4: each verb aborts with a clear cli error on a malformed jobs table —
      missing input/output column(s), NA path, or duplicate output paths.
- [x] AC5: each batch command is byte-identical to the scalar verb's command for
      the equivalent single job (shared pipeline helper). Parity test.
- [x] AC6: batch options forward through `...` to `ffm_batch` — `success`/
      `verified` columns populate and a multi-input manifest records inputs
      joined with `";"`. Binary-gated execution test (`skip_if` binaries absent).
- [x] AC7: D015 recorded (extends D007); roxygen for the 3 verbs, `_pkgdown.yml`,
      and the wordlist synced; `devtools::check()` clean (0 errors / 0 warnings).

## Coverage

- AC1 → T3
- AC2 → T4
- AC3 → T5
- AC4 → T2
- AC5 → T1, T3, T4, T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: extract pipeline helpers from the three scalar verbs (concat, compare,
      PiP), refactoring each scalar to call its helper with no behavior change;
      place helpers ABOVE the roxygen block (M28 lesson). Existing scalar tests stay green.
- [x] T2: add fan-in job-table validation — a shared `check_fanin_jobs()` for the
      `inputs` list-column + `output` shape (concat/compare) and inline
      `main`/`overlay` validation for PiP; reuse `reject_duplicate_outputs`.
      Malformed-jobs tests (AC4).
- [x] T3: `concatenate_videos_batch()` over `ffm_batch`; compile + parity tests.
- [x] T4: `compare_videos_batch()` with per-row `direction`/`resize`/`audio`
      overrides; compile + override + parity tests.
- [x] T5: `picture_in_picture_batch()` with per-row `position`/`scale`/`margin`/
      `audio` overrides; compile + override + parity tests.
- [x] T6: options-forwarding + multi-input manifest execution test (binary-gated).
- [x] T7: docs + decision — roxygen (`@family`, `@seealso` scalar sibling +
      `ffm_batch`), sync `_pkgdown.yml` (M23 lesson), `spelling::update_wordlist()`
      (M17 lesson), record D015, `devtools::check()` to 0/0.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: status → in-progress; branch m32-batch-fan-in-verbs cut from master.
- 2026-07-26: T1 done — extracted concatenate_pipeline / compare_videos_pipeline / picture_in_picture_pipeline (above roxygen); scalars refactored to call them; test-ffmpeg.R 119 pass / 0 fail.
- 2026-07-26: T2 done — check_fanin_jobs() validates the `inputs` list-column + `output` shape (min_inputs param for compare's ≥2); PiP keeps inline main/overlay validation. Verb-level malformed-jobs tests land with T3–T5.
- 2026-07-26: T3 done — concatenate_videos_batch() over ffm_batch; 19 tests (compile + list-path-scrubbed parity + AC4 guards + binary-gated exec) pass. Parity scrubs the concat demuxer's per-invocation temp list-file path.
- 2026-07-26: T4 done — compare_videos_batch() with per-row direction/resize/audio overrides (audio NA = drop, checked per row against that row's input count); 21 tests pass. Byte-identical scalar parity.
- 2026-07-26: T5 done — picture_in_picture_batch() with fixed main/overlay/output columns (inline validation) + per-row position/scale/margin/audio overrides; 21 tests pass. Byte-identical scalar parity.
- 2026-07-26: T6 done — AC6 forwarding test (binary-gated): verify/manifest/checksums reach ffm_batch via `...`; the multi-input manifest joins a row's two inputs (and their md5s) with ";". 11 tests pass.
- 2026-07-26: T7 done — pkgdown reference gains the 3 batch siblings (pkgdown::check_pkgdown() clean); wordlist +scalar's; D015 already recorded at plan. devtools::check() Status: OK (0/0/0). Vignette fan-in-shape prose left out of scope (see review note).
- 2026-07-26: all tasks done; status → review. Note: cairn/PROFILE.md is absent (repo predates profiles) — verify slot inferred as the r-package devtools::check(); flag for /cairn-init repair.
- 2026-07-26: review — 3-lens independent review + scorer. Fixed F1 (cli-pluralization crash, M18 regression, score 88), F2 (PiP margin column bypassed scalar guard, 80), F3 (PiP logical audio=NA rejected vs docs, 74). F4 (no check_file_exists, 15) rejected as intentional convention. Post-fix check 0/0/0; +5 regression tests.

## Decisions

## Review

**Reviewed 2026-07-26 · PR #34 · branch cut from master @ c3f753c (master unmoved).**

Acceptance-criteria evidence (fresh runs on the branch; `devtools::check()` → Status: OK, 0/0/0):

- AC1 ✓ — test-concatenate-videos-batch.R (19 pass): "compiles one concat command per row" asserts `-f concat -safe 0` + per-row output; returns inputs/output/command.
- AC2 ✓ — test-compare-videos-batch.R (21 pass): per-row hstack/vstack; direction/resize/audio override tests confirm column-wins-over-arg with fallback.
- AC3 ✓ — test-picture-in-picture-batch.R (21 pass): per-row overlay from fixed main/overlay/output; position/scale/margin/audio override tests pass.
- AC4 ✓ — malformed-jobs tests across all three files: missing inputs/output/main/overlay column, empty table, non-list inputs, NA path, and duplicate outputs each abort with a clear cli error.
- AC5 ✓ — parity tests: compare & PiP byte-identical to the scalar command; concatenate identical after scrubbing the concat demuxer's per-invocation temp list-file path. test-ffmpeg.R (119 pass) confirms the scalar refactor is behavior-preserving.
- AC6 ✓ — test-fan-in-batch-forwarding.R (11 pass): verify/manifest/checksums reach ffm_batch via `...`; success + verified columns populate; the multi-input manifest joins a row's two inputs (and md5s) with ";".
- AC7 ✓ — D015 recorded in DECISIONS.md (extends D007); roxygen for the 3 verbs, `_pkgdown.yml` (+3, pkgdown::check_pkgdown() clean), and inst/WORDLIST synced; devtools::check() Status: OK.

**Consistency gate:** cairn_validate exit 0 (all checks pass). No DESIGN principle changed (works under IP1/IP3) → cairn_impact skipped. Toolchain (r-package, inferred — PROFILE.md absent): devtools::check() 0/0/0; pkgdown clean.

**Independent review — three lenses (diff-bug/Opus, blame-history/Sonnet, prior-review/Sonnet) + scorer/Sonnet.** 4 findings; blame-history additionally confirmed the scalar refactor behavior-preserving with all guards retained.

Actioned (score ≥ 80):
- **F1 (88) — cli-pluralization crash (prior-review lens).** `check_fanin_jobs()` reintroduced the M18 bug: two `{?s}` governed by the numeric `which(!ok)` vector threw `length(object) == 1` on 2+ invalid rows, and the guard tests used only single rows (the M18 blind spot). **Fixed:** message now drives plurals off the scalar `{length(bad)}`; added 2-invalid-row tests to concatenate + compare batch.
- **F2 (80) — PiP `margin` column bypassed the scalar's guard (diff-bug lens).** A `margin` column skipped `check_number_whole(min = 0)`, so `-8` compiled a sign-flipped offset and `16.5` truncated. **Fixed:** margin re-checked per row in the closure (`arg = "margin"`); added negative/fractional regression tests.

Also fixed (score 74, below the 80 bar — logged, actioned anyway as a documented-contract alignment, one-line/zero-risk):
- **F3 (74) — PiP rejected a logical `audio = NA` column** the roxygen documents as "drop audio", and inconsistent with `compare_videos_batch`. **Fixed:** the up-front guard now accepts an all-NA column; added a drop-audio test. Left unfixed it would be a lying doc.

Logged below bar, not actioned:
- **F4 (15) — PiP omits `check_file_exists` (blame-history + diff-bug).** Rejected: intentional and consistent with the whole `_batch` family (none check input existence; the other two fan-in scalars never did); `run = TRUE` records `success = FALSE` on a missing file.

Post-fix: all 77 fan-in tests + 119 scalar tests pass; devtools::check() re-run → Status: OK (0/0/0).
