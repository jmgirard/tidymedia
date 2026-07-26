# M32: Batch siblings for the fan-in verbs (`concatenate_videos`/`compare_videos`/`picture_in_picture` `_batch`)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3
- **Branch/PR:** m32-batch-fan-in-verbs

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

- [ ] AC1: `concatenate_videos_batch(jobs)` compiles one concat command per row
      from an `inputs` list-column + `output`; returns jobs + `command`
      (+ `success` when run). Compile test, binary-free.
- [ ] AC2: `compare_videos_batch(jobs)` compiles per-row hstack/vstack commands,
      honoring optional `direction`/`resize`/`audio` columns with scalar-arg
      fallback. Compile + override test.
- [ ] AC3: `picture_in_picture_batch(jobs)` compiles per-row overlay commands
      from fixed `main`/`overlay`/`output`, honoring optional
      `position`/`scale`/`margin`/`audio` columns with fallback. Compile + override test.
- [ ] AC4: each verb aborts with a clear cli error on a malformed jobs table —
      missing input/output column(s), NA path, or duplicate output paths.
- [ ] AC5: each batch command is byte-identical to the scalar verb's command for
      the equivalent single job (shared pipeline helper). Parity test.
- [ ] AC6: batch options forward through `...` to `ffm_batch` — `success`/
      `verified` columns populate and a multi-input manifest records inputs
      joined with `";"`. Binary-gated execution test (`skip_if` binaries absent).
- [ ] AC7: D015 recorded (extends D007); roxygen for the 3 verbs, `_pkgdown.yml`,
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
- [ ] T3: `concatenate_videos_batch()` over `ffm_batch`; compile + parity tests.
- [ ] T4: `compare_videos_batch()` with per-row `direction`/`resize`/`audio`
      overrides; compile + override + parity tests.
- [ ] T5: `picture_in_picture_batch()` with per-row `position`/`scale`/`margin`/
      `audio` overrides; compile + override + parity tests.
- [ ] T6: options-forwarding + multi-input manifest execution test (binary-gated).
- [ ] T7: docs + decision — roxygen (`@family`, `@seealso` scalar sibling +
      `ffm_batch`), sync `_pkgdown.yml` (M23 lesson), `spelling::update_wordlist()`
      (M17 lesson), record D015, `devtools::check()` to 0/0.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: status → in-progress; branch m32-batch-fan-in-verbs cut from master.
- 2026-07-26: T1 done — extracted concatenate_pipeline / compare_videos_pipeline / picture_in_picture_pipeline (above roxygen); scalars refactored to call them; test-ffmpeg.R 119 pass / 0 fail.
- 2026-07-26: T2 done — check_fanin_jobs() validates the `inputs` list-column + `output` shape (min_inputs param for compare's ≥2); PiP keeps inline main/overlay validation. Verb-level malformed-jobs tests land with T3–T5.

## Decisions

## Review
