<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M29: Batch sibling for separate_audio_video (fan-out)

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M28   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m29-batch-separate-audio-video · https://github.com/jmgirard/tidymedia/pull/31   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Give `separate_audio_video` a table-driven `_batch` sibling: **N** input rows
reshape into **2N** single-output pipelines (one audio + one video per row),
following the `segment_video` scalar's reshape-to-rows fan-out (D007) — because
`ffm_batch()` is strictly one-row→one-command — and completing batch coverage of
the fan-out verbs.

## Scope

**In:** a new exported `separate_audio_video_batch` — a thin Layer-2 fan-out
over `ffm_batch()` (D007). Each input row carries one `input` plus two **required**
output columns, `audiofile` and `videofile` (matching the scalar's parameter
names; no derivation — parity with the scalar, which requires both). The verb
**reshapes** the N-row jobs table into a 2N-row internal table (melting
`audiofile`/`videofile` into one `output` column plus a `stream` marker ∈
{`audio`,`video`}), then fans out over `ffm_batch()` sharing a per-stream recipe
with the scalar. Optional per-row `reencode` column; a single duplicate-path
guard on the reshaped `output` column (M26) — which pools across both output
columns and catches within-row `audiofile == videofile` in one check.
Return-schema parity with `segment_video_batch` (single `output`/`command`
column) **plus** the `stream` marker column (M19).

**Out:**
- Batch for the composite/fan-in verbs (`concatenate_videos`, `compare_videos`,
  `picture_in_picture`) → ROADMAP candidate (jobs-tibble input-shape design call).
- Per-row output-path derivation — both output columns are required (a
  derivation design call, e.g. copy-safe audio containers, was rejected at the
  M29 plan gate for scalar parity).
- Any change to `separate_audio_video`'s scalar behavior — the shared-recipe
  extraction must be compile-preserving.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] AC1 — `separate_audio_video_batch` is exported and is a thin fan-out over
      `ffm_batch()`: it reshapes each of N input rows into **two** single-output
      pipelines (one `0:a` → audiofile, one `0:v` → videofile) and returns a
      **2N-row** result, sharing a per-stream recipe with the scalar verb and
      gluing no command strings of its own (IP1/D007). Evidence:
      `grep '^export' NAMESPACE`; a compile test showing N input rows → 2N result
      rows / 2N commands.
- [x] AC2 — Jobs guards with `cli` errors: non-data-frame `jobs`, zero-row
      `jobs`, missing `input`/`audiofile`/`videofile` column, `NA` in `input`,
      `audiofile`, or `videofile`. An optional per-row `reencode` column
      overrides the `reencode` argument, falling back when absent. Evidence: one
      test per guard; a test showing a `reencode` column flips one row's
      copy-vs-re-encode while a column-less row uses the default.
- [x] AC3 — Single duplicate-path guard on the reshaped `output` column rejects
      any collision: no two resolved output paths collide across the whole 2N-row
      table (any audiofile ↔ any videofile, any row), which subsumes within-row
      `audiofile == videofile` (both melt to identical `output` entries) (M26).
      Evidence: a test where a cross-column path collision is rejected and a test
      where within-row `audiofile == videofile` is rejected.
- [x] AC4 — Return-schema parity: the 2N-row result carries the same
      `ffm_batch()` columns as `segment_video_batch` (single `output`, `command`,
      opt-in `verified`/manifest) **plus** a `stream` marker column (`audio`/
      `video`) identifying each row's stream; row shape is one row per output
      stream (model on the `segment_video` scalar's reshape). Evidence: a test
      comparing `names()`/types against `segment_video_batch` on a matched call
      and asserting the added `stream` column and its values.
- [x] AC5 — Docs & housekeeping synced (roxygen `@family`/`@seealso` scalar +
      `ffm_batch` + batch-disambiguation prose per M24; `man/`, `NAMESPACE`,
      `_pkgdown.yml`, spelling wordlist) and `devtools::check()` shows
      `Status: OK` in `00check.log` (M23/M24/M17). Evidence: `pkgdown::check_pkgdown()`
      clean; `00check.log` `Status: OK`.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1 — Extract a **per-stream** shared recipe from `separate_audio_video`
      (R/ffmpeg.R:316), e.g. `separate_stream_pipeline(input, output, stream,
      reencode)`: map `0:a` + optional `-c:a copy` when `stream == "audio"`, map
      `0:v` + optional `-c:v copy` when `stream == "video"`. The scalar calls it
      twice (compile-preserved); place the helper **above** the scalar's roxygen
      block, not between block and function (M28 lesson).
- [x] T2 — Write `separate_audio_video_batch`, modeling the fan-out on the
      `segment_video` **scalar**'s reshape-to-rows (R/ffmpeg.R:1391), not
      `segment_video_batch` (which is 1-row→1-command): jobs guards (non-df,
      zero-row, required `input`/`audiofile`/`videofile`, NA checks, per-row
      `reencode` column type); **reshape** the N-row jobs table into a 2N-row
      table (melt `audiofile`/`videofile` → one `output` column + a `stream`
      marker); a single `anyDuplicated()` guard on the reshaped `output`; then
      `ffm_batch()` calling `separate_stream_pipeline()` per row, forwarding `...`
      (incl. per-row `reencode`) to the runner.
- [x] T3 — Tests (`tests/testthat/`): N input rows → 2N result rows / 2N commands
      (AC1), the jobs guards + `reencode` column override (AC2), duplicate
      rejection for a cross-column collision and for within-row
      `audiofile == videofile` (AC3), schema parity against `segment_video_batch`
      plus the added `stream` column (AC4). Execution tests `skip_if` ffmpeg
      absent (D004).
- [x] T4 — Docs & housekeeping: roxygen (`@family`, `@seealso`, batch
      disambiguation), `document()`, `NAMESPACE`, `_pkgdown.yml` sync, spelling
      wordlist, `devtools::check()` → `Status: OK` (AC5).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (batch-coverage gap analysis — Tier 2, fan-out).
- 2026-07-12: /milestone-plan refresh (M28 now done). Re-investigated vs current
  code; `ffm_batch` confirmed 1-row→1-command, so the fan-out models the
  `segment_video` scalar's reshape, not `segment_video_batch`. Gate decisions:
  2N-row return + `stream` marker; `audiofile`/`videofile` required columns (no
  derivation); single `anyDuplicated()` guard on the reshaped `output`. Amended
  Goal/Scope/AC1–AC4/T1–T3 (amend-via-gate); T1 recipe made per-stream.
- 2026-07-12: T1 — extracted `separate_stream_pipeline(input, output, stream,
  reencode)` above the scalar's roxygen block; scalar now calls it twice.
  Compile-preserving (test-ffmpeg.R: 119 pass, 0 fail).
- 2026-07-12: T2+T3 — wrote `separate_audio_video_batch`; reuses M28's
  `check_batch_jobs`/`reject_duplicate_outputs`. Reshape N→2N via
  `rbind(audiofile, videofile)` + `stream` marker; the single dup guard on the
  melted `output` pools cross-column and catches within-row equality. 41 tests
  (AC1–AC4 + binary-gated end-to-end); full suite green.
- 2026-07-12: T4 — `document()` (new `@family` entry propagated to sibling
  `.Rd`s), `_pkgdown.yml` synced, wordlist needs no new terms.
  `pkgdown::check_pkgdown()` clean; `devtools::check()` `Status: OK` (0/0/0).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->

**Reviewer:** /milestone-review, 2026-07-12. PR #31 (draft). Branch 4 ahead / 0
behind `master`; master unmoved since cut — no merge needed.

### Acceptance-criteria evidence (fresh)

- AC1 — PASS. `grep '^export(separate_audio_video_batch)' NAMESPACE` matches.
  Compile: 3 input rows → 6 result rows / 6 commands, streams `audio,video`
  interleaved; audio rows carry `-map 0:a "aN.aac"`, video rows `-map 0:v
  "vN.mp4"`. Thin-wrapper contract verified by a test asserting batch commands
  are byte-identical to `separate_audio_video()`'s (glues nothing, IP1/D007).
- AC2 — PASS. Tests fire each guard: non-df, zero-row, missing `input`, missing
  output column (`Missing column`), NA in `input`/`audiofile`/`videofile`,
  non-logical `reencode`, NA in `reencode`. Per-row `reencode` column overrides
  the arg on both of an input's rows; column-absent falls back to the arg
  (default-copy test).
- AC3 — PASS. One `reject_duplicate_outputs()` on the melted `output` rejects a
  cross-column collision (audiofile[1] == videofile[2]) and within-row
  `audiofile == videofile` (both → identical `output` rows). Two tests.
- AC4 — PASS. Return carries `input`/`output`/`command` matching
  `segment_video_batch` (`class()` equal, `output`/`command` character) plus a
  `stream` marker with values `{audio, video}`. Parity test green.
- AC5 — PASS. `pkgdown::check_pkgdown()` clean; `_pkgdown.yml` lists the verb;
  `devtools::check()` 0 errors / 0 warnings / 0 notes (fresh); wordlist needs no
  new terms (no maskable spelling NOTE — M17).

### Consistency gate

- `cairn_validate.py` exit 0 — all checks pass (incl. coverage complete, mirror
  agreement, weight caps, sizing OK).
- Coverage completeness: every AC maps to ≥1 existing task (AC1→T1,T2;
  AC2/3/4→T2,T3; AC5→T4).
- No `DESIGN.md` principle changed (works under IP1) → `cairn_impact` skipped.
- Full suite: 1104 pass, 0 fail, 1 skip.
- r-package consistency-gate: `devtools::check()` Status OK; `document()` clean.
