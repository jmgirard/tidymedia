<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M10: segment_videos() parity polish

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m10-segment-videos-polish · https://github.com/jmgirard/tidymedia/pull/12   <!-- owner: implement (branch) / review (PR URL) · create -->

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

- [x] AC1 — Auto-naming: with a `jobs` table that has **no** `output` column,
      `segment_videos()` returns derived outputs — `<input-basename>_NN.<ext>`
      numbered within each input file (single-input multi-segment and
      multi-input tables both correct); with an `output` column present the
      outputs are used unchanged. Verified by `run = FALSE` tests asserting the
      returned `output`/`command` values.
- [x] AC2 — Per-row `reencode`: a logical `reencode` column controls the cut
      path per row — a mixed `c(TRUE, FALSE)` table yields one re-encode command
      and one fast-copy command (`-codec:v copy … -avoid_negative_ts`) — while a
      table without the column follows the scalar `reencode` arg. Verified by
      `run = FALSE` tests.
- [x] AC3 — Validation parity: a non-numeric/non-character `start` or `end`
      column, and a non-logical `reencode` column, each abort via
      `cli::cli_abort()` naming the offending column, before any FFmpeg call.
      Each branch fired by a test.
- [x] AC4 — Docs: `segment_videos()` roxygen documents optional `output` (with
      the naming rule) and the per-row `reencode` column and its precedence
      over the scalar arg; a `NEWS.md` entry is added; `devtools::document()`
      regenerates `man/` with no diff churn beyond these changes.
- [x] AC5 — `devtools::check()` clean: zero errors, zero warnings (any note
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

- [x] T1 — Test-first: add failing cases to
      `tests/testthat/test-segment-videos.R` (all `run = FALSE`, CI-safe) for:
      auto-naming with no `output` column (single input → `_01`, `_02`;
      multi-input → numbering restarts per input file); `output` present ⇒
      unchanged; a mixed `reencode` column (one copy, one re-encode); scalar
      `reencode` still applies when no column; non-numeric `start`/`end`
      rejected; non-logical `reencode` column rejected.
- [x] T2 — Validation: in `segment_videos()` (`R/ffmpeg.R:571`) add `start`/`end`
      numeric-or-character checks (mirror `R/ffmpeg.R:464-469`) and, when a
      `reencode` column is present, an `is.logical()` check — all before the
      `ffm_batch()` call. Make `output` no longer a hard-required column
      (`R/ffmpeg.R:577`).
- [x] T3 — Auto-naming: when `jobs` has no `output` column, derive it —
      per-input group index, zero-padded within each input, appended to the
      input basename + original extension — and inject it into `jobs` before
      `ffm_batch()` so the result carries the names. Reuse/extract the
      `segment_video()` naming logic (`R/ffmpeg.R:479-487`, `pad_integers`) into
      a shared internal helper if it stays clean.
- [x] T4 — Per-row `reencode`: have the batch closure
      (`R/ffmpeg.R:593`) prefer a per-row `reencode` value (arriving via `...`
      from the column) over the captured scalar, passing the resolved value to
      `segment_pipeline()`.
- [x] T5 — Docs: update `segment_videos()` roxygen (`jobs` param: optional
      `output` + naming rule; `reencode` column + precedence), run
      `devtools::document()`, and add a `NEWS.md` entry.
- [x] T6 — `devtools::test()` green, then `devtools::check()` clean; record
      evidence at review.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (absorbs the three M09 `segment_videos()` deferral candidates).
- 2026-07-12: T1–T5 done — tests-first (8 new cases), extracted `derive_segment_names()` (shared with `segment_video()`), optional `output` + per-row `reencode` column + start/end/reencode validation in `segment_videos()`, roxygen + NEWS; full suite green.
- 2026-07-12: T6 done — `devtools::check()` clean (0 errors/0 warnings/0 notes) after fixing a spelling NOTE; status → review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

- 2026-07-12 (plan): `reencode` column, when present, overrides the scalar
  `reencode` arg per row; scalar stays the default/fallback for tables without
  the column. Additive and backward-compatible — not an irreversible-API change.

## Review

_Reviewed 2026-07-12 (same session). PR #12._

**Acceptance-criteria evidence** (fresh run):
- AC1 — `test-segment-videos.R` cases "auto-names outputs when the column is
  absent" (single input → `_1`,`_2`), "restarts numbering per input file" (f1
  → `_1`,`_2`; f2 → `_1`,`_2`,`_3`), and "uses an explicit output column
  unchanged" all pass.
- AC2 — cases "honors a per-row reencode column" and "reencode column overrides
  the scalar arg" pass: mixed `c(TRUE, FALSE)` yields one re-encode command and
  one `-codec:v copy … -avoid_negative_ts` command; scalar applies when no
  column present (existing cases).
- AC3 — three rejection cases pass: non-numeric/character `start`, non-…`end`,
  and non-logical `reencode` column each abort naming the offending column,
  before FFmpeg. Whole file: 38 checks, 0 failed, 0 warnings.
- AC4 — roxygen documents optional `output` + per-input naming and the per-row
  `reencode` precedence; `NEWS.md` entry added; `devtools::document()` produces
  no diff (verified at review).
- AC5 — `devtools::check()` → **Status: OK** (0 errors / 0 warnings / 0 notes).

**Consistency gate:**
- `cairn_validate.py`: all checks PASS except the known benign ISO-date
  false-positive (LESSONS.md 2026-07-12) — 7 hits, all in `archive/M02–M08`
  `0/0/0` check shorthand, none in M10; not introduced here.
- Coverage completeness: AC1–AC5 each map to existing tasks T1–T6. PASS.
- No DESIGN principle (IP/GP) changed → impact report skipped.
- `document()` no diff; pkgdown `check_pkgdown()` "No problems found";
  README.Rmd untouched (in sync); NEWS.md entry present; no new top-level files.

**Independent review:** (findings + triage recorded below)
