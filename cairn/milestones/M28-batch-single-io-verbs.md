<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M28: Batch siblings for single-in/single-out verbs

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m28-batch-single-io-verbs · https://github.com/jmgirard/tidymedia/pull/30   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Give the four remaining single-input/single-output transform verbs
(`extract_audio`, `convert_audio`, `crop_video`, `format_for_web`) the same
table-driven `_batch` sibling every other single-in/single-out verb already has,
closing the batch-coverage gap that most directly serves D001 (batch is the
package's differentiator).

## Scope

**In:** four new exported batch verbs — `extract_audio_batch`,
`convert_audio_batch`, `crop_video_batch`, `format_for_web_batch` — each a thin
Layer-2 fan-out over `ffm_batch()` (D007), sharing the scalar verb's recipe via
an extracted `*_pipeline()` helper (M13 lesson). Standard jobs-table guards,
resolved-output duplicate-path guard (M26 lesson), per-row override columns for
the verbs that have knobs, schema parity with the existing `_batch` verbs (M19),
and full doc/housekeeping sync.

**Out:**
- `separate_audio_video_batch` (fan-out, 1 input → 2 outputs) → M29.
- Batch for the composite/fan-in verbs (`concatenate_videos`, `compare_videos`,
  `picture_in_picture`) → ROADMAP candidate (needs a jobs-tibble input-shape
  design call).
- Any change to the scalar verbs' observable behavior — the `*_pipeline()`
  extraction must be byte-for-byte compile-preserving.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] AC1 — Four new verbs are exported (`NAMESPACE`) and each is a thin
      `ffm_batch()` wrapper that glues no command strings of its own (IP1): the
      per-row work is delegated to a shared `*_pipeline()` helper. Evidence:
      `grep '^export' NAMESPACE` shows all four; each body contains an
      `ffm_batch(` call and no direct string assembly.
- [x] AC2 — Each scalar verb's recipe is extracted into a shared `*_pipeline()`
      helper that both the scalar verb and its batch sibling call, and the
      scalar verb's compiled output is unchanged for a fixed call (byte-for-byte).
      Evidence: a test asserting `<verb>(…, run = FALSE)` returns the exact
      command string it did before the refactor (golden string per verb).
- [x] AC3 — Every batch verb rejects, with a `cli` error: a non-data-frame
      `jobs`, a zero-row `jobs`, a missing `input` column, an `NA` in `input`,
      and any two rows whose **resolved** output paths collide (M26 — guard at
      the resolved-path level, not just duplicated inputs). Evidence: one test
      per guard, per verb (duplicate-path test uses a repeated explicit output).
- [x] AC4 — Per-row override columns are honored where the scalar verb has knobs
      and fall back to the argument default when the column is absent:
      `crop_video_batch` reads `width`/`height`/`x`/`y`; `convert_audio_batch`
      reads `format`; `extract_audio_batch` reads `audio_codec`;
      `format_for_web_batch` has no knobs (input/output only). Evidence: a test
      per knobbed verb showing a per-row column changes that row's command while
      a column-less row uses the default (parity with `normalize_audio_batch`).
- [x] AC5 — Return-schema parity: each batch verb's output tibble carries the
      same `ffm_batch()` columns (`command`, and the opt-in `verified` column /
      manifest attribute) as the existing `_batch` verbs, with identical
      names/types (M19). Evidence: a test comparing `names()`/types of a new
      batch verb's result against `strip_metadata_batch`'s on a matched call.
- [x] AC6 — Docs & housekeeping synced: each new verb has roxygen with
      `@family task verb functions`, `@seealso` to its scalar sibling and
      `ffm_batch`, and batch-disambiguation prose (M24); `man/` regenerated,
      `NAMESPACE` updated, `_pkgdown.yml` synced, spelling wordlist updated for
      any new terms (M23/M17). Evidence: `pkgdown::check_pkgdown()` clean; `man/`
      pages exist for all four.
- [x] AC7 — `devtools::check()` reports 0 errors / 0 warnings and raw
      `00check.log` shows `Status: OK` (M17 — confirm in the log, not just the
      devtools summary).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T2, T3
- AC6 → T4
- AC7 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1 — Extract a shared `*_pipeline()` helper from each of the four scalar
      verbs (`R/ffmpeg.R`: `extract_audio` 268, `convert_audio` 364,
      `crop_video` 401, `format_for_web` 435), following the
      `strip_metadata_pipeline()` model (R/ffmpeg.R:457). The scalar verb becomes
      a thin front door (arg checks → `*_pipeline()` → `ffm_finish`); preserve
      each verb's conditional branches (`convert_audio`'s `format = NULL` vs. set;
      `crop_video`'s `x`/`y` defaults). Confirm compile output unchanged (AC2).
- [x] T2 — Write the four `_batch` front doors as thin `ffm_batch()` wrappers
      modeled on `strip_metadata_batch` (R/ffmpeg.R:2073): jobs guards +
      resolved-output duplicate-path guard, per-row override columns forwarded
      pmap-style to the shared pipeline (AC4), and output derivation when the
      `output` column is absent — derive `<base>_cropped.<ext>` /
      `<base>_web.mp4` where the container is implied, and require an explicit
      `output` for the audio verbs whose extension is the instruction (decide per
      verb and document it).
- [x] T3 — Tests (`tests/testthat/`): per verb — compile-parity golden string
      (AC2), the five jobs guards incl. duplicate resolved-path (AC3), per-row
      override column vs. default fallback for knobbed verbs (AC4), and
      fast/normal schema parity against an existing `_batch` verb (AC5).
      Execution tests `skip_if` ffmpeg absent (D004); compilation tests stay pure.
- [x] T4 — Docs & housekeeping: roxygen for all four (`@family`, `@seealso`
      scalar + `ffm_batch`, batch-disambiguation prose per M24), `document()`,
      confirm `NAMESPACE`, sync `_pkgdown.yml`, `spelling::update_wordlist()`,
      `build_readme()` only if content changed (M24 path-churn caveat), then
      `devtools::check()` → `Status: OK` (AC6, AC7).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (batch-coverage gap analysis — Tier 1).
- 2026-07-12: T1 — extracted extract_audio/convert_audio/crop_video/format_for_web `_pipeline()` helpers; scalar verbs delegate; all 119 test-ffmpeg.R tests green (compile-preserving).
- 2026-07-12: T2 — added the four `_batch` front doors + shared jobs guards (`check_batch_jobs`/`reject_duplicate_outputs`/`check_batch_string_col`). Audio verbs require explicit `output`; crop derives `_cropped`, web derives `_web.mp4`. Smoke-tested: compile parity holds for all four; per-row override columns work.
- 2026-07-12: T3 — four test files (82 tests): compile-parity golden strings, 5 jobs guards each incl. M26 duplicate-output, per-row override columns, schema parity, binary-gated execution+verify. All green (0 fail/warn/skip locally).
- 2026-07-12: T4 — document() (man/ + NAMESPACE, 4 new exports); `_pkgdown.yml` rows; NEWS entry; wordlist +transcode. Fixed a self-colliding crop example. `devtools::check()` Status: OK (0/0/0); `pkgdown::check_pkgdown()` clean. Status → review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->

**Reviewed 2026-07-12 · PR #30 · branch cut from master (in sync, no merge needed).**

### Acceptance-criteria evidence (fresh)

- AC1 — `grep '^export'` NAMESPACE shows all four (`extract_audio_batch`,
  `convert_audio_batch`, `crop_video_batch`, `format_for_web_batch`); each body
  delegates to `ffm_batch(...)` and a shared `*_pipeline()` helper, no hand-glued
  strings (IP1). **PASS**
- AC2 — Pipeline helpers extracted; per-verb compile-parity golden-string tests
  (`res$command[[1]] == <scalar>(…, run = FALSE)`) pass for all four; the 119
  `test-ffmpeg.R` scalar tests stay green (extraction is byte-preserving). **PASS**
- AC3 — Guard tests pass per verb: non-data-frame, zero-row, missing `input`,
  `NA` input, and duplicated **resolved** output (M26) all abort with a `cli`
  error. **PASS**
- AC4 — Per-row override-column tests pass: `crop_video_batch` reads
  `width`/`height`/`x`/`y`; `convert_audio_batch` reads `format`;
  `extract_audio_batch` reads `audio_codec`; a column-less row uses the argument
  default (parity with `standardize_video_batch`). **PASS**
- AC5 — Schema-parity tests pass: `setdiff(names(res), names(jobs)) == "command"`
  under `run = FALSE`, matching another `_batch` verb's added-column schema
  (M19). **PASS**
- AC6 — `document()` produces no diff; four new `man/*.Rd` exist;
  `pkgdown::check_pkgdown()` clean; `_pkgdown.yml` rows added; NEWS entry present;
  wordlist `+transcode`. **PASS**
- AC7 — `devtools::check()` **Status: OK** — 0 errors / 0 warnings / 0 notes. **PASS**

Test totals (fresh, ffmpeg present): 201 pass / 0 fail / 0 warn / 0 skip across
the four new files (82) + `test-ffmpeg.R` (119).

### Consistency gate

- `cairn_validate.py` — all checks PASS (exit 0), incl. coverage completeness,
  mirror agreement, single in-progress, weight caps, principles slot.
- Toolchain (`r-package` consistency-gate): `document()` no-diff PASS; generated
  files (NAMESPACE/man) regenerate clean; `pkgdown::check_pkgdown()` PASS; NEWS
  entry present; `devtools::check()` Status: OK. No `DESIGN.md` principle changed
  (IP1 worked under, not modified) → `cairn_impact` skipped.

### Independent review

Three fresh-context reviewers (ref-based git, shared tree):
- **[O] diff-bug (Opus):** no correctness/contract defects. Verified compile
  preservation (incl. the default-centered crop x/y case, byte-identical across
  scalar/pipeline/batch), the `pick()` override mechanism, and empirically that
  the duplicate-output `cli` message renders safely with 2+ colliding paths (no
  M18 pluralization crash).
- **[S] blame-history (Sonnet):** no findings. Confirmed each `*_pipeline()`
  helper reproduces its scalar verb's builder sequence verbatim vs `master:R/ffmpeg.R`;
  `reject_duplicate_outputs()` is the corrected post-M26 pattern (supersedes the
  older input-level style), not a regression.
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence — all 24 merged PRs
  carry only Codecov bot comments; clean no-op.

No finding scored ≥80 (no actionable defects). Two sub-threshold observations,
both surfaced here:
1. `extract_audio_pipeline` keeps `check_string(audio_codec)` in the scalar
   front door (vs. `convert_audio_pipeline` holding `check_string(format)`
   inside the helper) — a slight asymmetry from the M13 ideal. **Rejected (no
   action):** no observable consequence — the batch path already guarantees
   character/non-NA per row via `check_batch_string_col`; moving it would be a
   gratuitous behavior-neutral edit.
2. `crop_video_batch` lacked an explicit `run = FALSE` return-schema test
   (AC5 was already met by `extract_audio_batch`'s comparison). **Fixed now:**
   added the schema-parity test to `test-crop-video-batch.R` (vs
   `strip_metadata_batch`); crop file 19 → 22 passing.

CI (PR #30): green on all platforms — macOS, Ubuntu devel/release/oldrel-1,
Windows, pkgdown, test-coverage.
