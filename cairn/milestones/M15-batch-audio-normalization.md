# M15: Batch audio normalization verb

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M14
- **Branch/PR:** m15-batch-audio-normalization

## Goal

Add `normalize_audios()`, the tibble-driven batch sibling of `normalize_audio()`,
built over `ffm_batch()` (D007) — mirroring the M12 → M13 (scalar → batch) split.

## Scope

**In:**
- `normalize_audios(jobs, …)`: a fan-out Layer-2 wrapper that builds one
  `normalize_audio()` pipeline per jobs-tibble row via `ffm_batch()`, returning
  the jobs tibble plus a `command` column (and `success` when run).
- Per-element validation parity with the scalar verb, inherited from the shared
  `normalize_audio_pipeline()` helper (M14 T3) plus front-door column/NA guards.
- Pass-through of `verify`/`manifest`/`checksums`/`progress`/`parallel` to
  `ffm_batch()`.

**Out:**
- Any change to `normalize_audio()`'s loudness semantics or defaults — those are
  fixed in M14; this milestone only fans them out.
- Two-pass measured loudnorm → its own candidate (depends on M14).

## Acceptance criteria

- [ ] AC1 — `normalize_audios(jobs, …)` fans out over the jobs tibble via
      `ffm_batch()`, emitting one reproducible command per row and returning the
      jobs tibble + `command` (+ `success` when run). Evidence: passing test.
- [ ] AC2 — Per-row validation rejects the same invalid values the scalar verb
      rejects (via the shared pipeline helper + column/NA guards), per the
      M11/M13 parity lesson. Evidence: passing parity test.
- [ ] AC3 — `verify`/`manifest`/`checksums`/`progress`/`parallel` forward to
      `ffm_batch()` and never leak into the per-row `.f` (M09 lesson — batch
      params sit after `...` and bind by name). Evidence: passing test.
- [ ] AC4 — An execution test (`skip_if` ffmpeg absent) runs a 2-row jobs tibble
      and verifies non-empty, audio-decodable outputs. Evidence: passing
      skip-guarded test.
- [ ] AC5 — `devtools::check()` clean (zero errors/warnings/notes).

## Coverage

- AC1 → T2
- AC2 → T1, T2
- AC3 → T2
- AC4 → T3
- AC5 → T4

## Tasks

- [ ] T1 — Confirm/extract the shared `normalize_audio_pipeline()` helper (from
      M14 T3) carrying per-value validation, so the batch front door needs only
      column type/NA guards, not re-implemented value checks (M13 lesson).
- [ ] T2 — Add `normalize_audios(jobs, run = TRUE, …)` batch sibling over
      `ffm_batch()`, forwarding batch params after `...` (M09 lesson);
      tests-first for compile + per-row validation parity.
- [ ] T3 — Add an execution test (`skip_if` binary absent) on a 2-row jobs
      tibble producing non-empty, audio-decodable outputs.
- [ ] T4 — Roxygen + `devtools::document()`; add to the `@family` lists and to
      DESIGN.md function families; `devtools::check()` clean.

## Work log

- 2026-07-12: created by /milestone-plan.

## Decisions

## Review
