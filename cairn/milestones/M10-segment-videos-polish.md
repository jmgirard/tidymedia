<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M10: segment_videos() parity polish

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Bring `segment_videos()` to parity with `segment_video()` by making the
`output` column optional (per-input auto-naming), honoring a per-row
`reencode` column, and validating `start`/`end` types up front.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** three additive, backward-compatible refinements to `segment_videos()`
(`R/ffmpeg.R:568`), each absorbing one M09 deferral candidate:
- Optional `output`: when the column is absent, derive one path per row by
  appending `_<zero-padded integer>.<ext>` to each input's basename, numbered
  within each input file (parity with `segment_video()`, `R/ffmpeg.R:479`).
- Per-row `reencode`: a logical `reencode` column in `jobs`, when present,
  selects the cut path per row, overriding the scalar `reencode` arg.
- Type validation: reject non-numeric/character `start`/`end` and a
  non-logical `reencode` column with `cli::cli_abort()` before any FFmpeg call.
- Roxygen + `NEWS.md` for the above.

**Out:**
- Any behavior change to `segment_video()` (it already has these features) or
  to the shared `segment_pipeline()` / engine contract — the verb stays a thin
  `ffm_batch` wrapper, single-output per job (D003, D007, IP1).
- Research-workflow Layer-2 verbs (standardization presets next) → future
  milestones; still candidate rows in the ROADMAP.
- CRAN readiness → its own later milestone (candidate row).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1 — Auto-naming: with a `jobs` table that has **no** `output` column,
      `segment_videos()` returns derived outputs — `<input-basename>_NN.<ext>`
      numbered within each input file (single-input multi-segment and
      multi-input tables both correct); with an `output` column present the
      outputs are used unchanged. Verified by `run = FALSE` tests asserting the
      returned `output`/`command` values.
- [ ] AC2 — Per-row `reencode`: a logical `reencode` column controls the cut
      path per row — a mixed `c(TRUE, FALSE)` table yields one re-encode command
      and one fast-copy command (`-codec:v copy … -avoid_negative_ts`) — while a
      table without the column follows the scalar `reencode` arg. Verified by
      `run = FALSE` tests.
- [ ] AC3 — Validation parity: a non-numeric/non-character `start` or `end`
      column, and a non-logical `reencode` column, each abort via
      `cli::cli_abort()` naming the offending column, before any FFmpeg call.
      Each branch fired by a test.
- [ ] AC4 — Docs: `segment_videos()` roxygen documents optional `output` (with
      the naming rule) and the per-row `reencode` column and its precedence
      over the scalar arg; a `NEWS.md` entry is added; `devtools::document()`
      regenerates `man/` with no diff churn beyond these changes.
- [ ] AC5 — `devtools::check()` clean: zero errors, zero warnings (any note
      pre-existing and explained in the Review section).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T1, T3
- AC2 → T1, T4
- AC3 → T1, T2
- AC4 → T5
- AC5 → T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate -->

- [ ] T1 — Test-first: add failing cases to
      `tests/testthat/test-segment-videos.R` (all `run = FALSE`, CI-safe) for:
      auto-naming with no `output` column (single input → `_01`, `_02`;
      multi-input → numbering restarts per input file); `output` present ⇒
      unchanged; a mixed `reencode` column (one copy, one re-encode); scalar
      `reencode` still applies when no column; non-numeric `start`/`end`
      rejected; non-logical `reencode` column rejected.
- [ ] T2 — Validation: in `segment_videos()` (`R/ffmpeg.R:571`) add `start`/`end`
      numeric-or-character checks (mirror `R/ffmpeg.R:464-469`) and, when a
      `reencode` column is present, an `is.logical()` check — all before the
      `ffm_batch()` call. Make `output` no longer a hard-required column
      (`R/ffmpeg.R:577`).
- [ ] T3 — Auto-naming: when `jobs` has no `output` column, derive it —
      per-input group index, zero-padded within each input, appended to the
      input basename + original extension — and inject it into `jobs` before
      `ffm_batch()` so the result carries the names. Reuse/extract the
      `segment_video()` naming logic (`R/ffmpeg.R:479-487`, `pad_integers`) into
      a shared internal helper if it stays clean.
- [ ] T4 — Per-row `reencode`: have the batch closure
      (`R/ffmpeg.R:593`) prefer a per-row `reencode` value (arriving via `...`
      from the column) over the captured scalar, passing the resolved value to
      `segment_pipeline()`.
- [ ] T5 — Docs: update `segment_videos()` roxygen (`jobs` param: optional
      `output` + naming rule; `reencode` column + precedence), run
      `devtools::document()`, and add a `NEWS.md` entry.
- [ ] T6 — `devtools::test()` green, then `devtools::check()` clean; record
      evidence at review.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (absorbs the three M09 `segment_videos()` deferral candidates).

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

- 2026-07-12 (plan): `reencode` column, when present, overrides the scalar
  `reencode` arg per row; scalar stays the default/fallback for tables without
  the column. Additive and backward-compatible — not an irreversible-API change.

## Review
<!-- owner: review · exclusive; evidence per criterion; consistency-gate
     results; independent-review findings and their triage -->
