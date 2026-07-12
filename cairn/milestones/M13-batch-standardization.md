<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M13: Batch video standardization verb

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m13-batch-standardization · https://github.com/jmgirard/tidymedia/pull/15   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add `standardize_videos()`, a tibble-driven fan-out over `standardize_video()`
via `ffm_batch()` (D007), mirroring `segment_videos()`/`extract_frames()`.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- Extract a shared `standardize_pipeline(input, output, width, height, fps,
  vcodec, pixel_format)` helper from `standardize_video()`'s inline body
  (`R/ffmpeg.R:318-341`, everything before `ffm_finish`), and refactor
  `standardize_video()` to call it — the same scalar↔batch sharing pattern as
  `frame_pipeline()` / `segment_pipeline()`. This keeps M12's guards (`-c:a
  copy`, floor-to-even crop, `+faststart`) in one place and gives the batch verb
  per-value validation parity by construction (avoids the M11 divergence ding).
- `standardize_videos(jobs, width = NULL, height = NULL, fps = NULL,
  vcodec = "libx264", pixel_format = "yuv420p", run = TRUE, parallel = FALSE,
  ...)`: one required `input` column; optional `output` column; the five knobs
  (`width`, `height`, `fps`, `vcodec`, `pixel_format`) recognized as **per-row
  override columns** — a column, when present, overrides the scalar arg for that
  row (scalar arg is the default for rows/knobs that omit it).
- Auto-name outputs when the `output` column is absent: `<base>_standardized.<input-ext>`
  (e.g. `clip.mkv → clip_standardized.mkv`), via a new `derive_standardized_names()`
  helper. **Abort** with a clear error if two rows share an input path and no
  `output` column (the 1:1 scheme would collide) — this is the deliberate
  trade-off for readable one-off names over sibling-style `_<n>` numbering.
- `...` forwards `ffm_batch()` options (`verify`, `manifest`, `checksums`,
  `progress`) to the runner, never to the pipeline builder (M09 lesson).
- Roxygen docs + `@family task verb functions`, `@examples` with `run = FALSE`;
  regenerate `man/` and `NAMESPACE` via `devtools::document()`.

**Out:**
- Any change to the engine / `ffm` object — this is a thin Layer-2 wrapper (D002,
  D003, D007); all command assembly stays in the shared pipeline helper.
- New standardization knobs beyond M12's five (e.g. audio re-encode, container
  remux) → not in scope; would be a future candidate.
- Sibling-style `_<n>` per-input numbering for auto-naming → rejected here in
  favor of the `_standardized` suffix; revisit only if duplicate-input batch
  standardization becomes a real need (candidate, not this milestone).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] AC1: For a multi-row jobs table with explicit `output`, `standardize_videos(jobs,
      run = FALSE)` returns one row per job with a `command` column, and each row's
      command is byte-identical to the corresponding scalar `standardize_video(...,
      run = FALSE)` call with the same parameters (compile-parity, CI-safe).
- [x] AC2: Per-row override columns work: a jobs table carrying e.g. a per-row `fps`
      (or `width`/`vcodec`/…) column produces commands reflecting the per-row value,
      while rows without that column fall back to the scalar default. Verified by
      compiled-command inspection.
- [x] AC3: Auto-naming: with no `output` column, outputs are `<base>_standardized.<input-ext>`
      per row; and a jobs table with a duplicated `input` and no `output` column
      aborts with a clear `cli::cli_abort` error (tested via `expect_error`).
- [x] AC4: Front-door validation rejects, with clear messages: a non-data-frame `jobs`,
      an empty `jobs`, a `jobs` missing the `input` column, and an override column of
      the wrong type / containing `NA` (mirroring `segment_videos()` guards and the
      M11 per-element-parity lesson). Each tested via `expect_error`.
- [x] AC5: `...`-forwarding works: `standardize_videos(jobs, verify = ...)` records a
      `verified` column and `manifest = TRUE` yields a manifest readable by
      `ffm_manifest()` — both binary-gated (`skip_if` ffmpeg absent), mirroring the
      `segment_videos()` forwarding tests.
- [x] AC6: One binary-gated E2E test: `standardize_videos()` with `run = TRUE` over a
      real 2-row jobs table writes standardized outputs that exist and probe to the
      requested spec (parity with `standardize_video()`'s E2E tests).
- [x] AC7: `standardize_video()`'s existing tests still pass unchanged after the
      `standardize_pipeline()` refactor (no behavior change to the scalar verb).
- [x] AC8: `devtools::check()` clean — zero errors / warnings / notes; full
      `devtools::test()` suite green.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2, T4
- AC2 → T2, T4
- AC3 → T3, T4
- AC4 → T2, T4
- AC5 → T2, T4
- AC6 → T5
- AC7 → T1, T4
- AC8 → T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Extract `standardize_pipeline(input, output, width, height, fps, vcodec,
      pixel_format)` from `standardize_video()`'s body (`R/ffmpeg.R:318-341`); refactor
      `standardize_video()` to `ffm_finish(standardize_pipeline(...), run)`. Confirm the
      existing scalar tests (`tests/testthat/test-ffmpeg.R`) still pass (AC7).
- [x] T2: Write `standardize_videos()`: jobs validation (data.frame, non-empty, `input`
      present), per-row override-column recognition + light front-door type/NA guards for
      present override columns, and the `ffm_batch()` fan-out closure sharing
      `standardize_pipeline()` (per-row values from `...`/pmap override scalar args;
      `...` forwarded to `ffm_batch`). Mirror `segment_videos()` structure (`R/ffmpeg.R:701`).
- [x] T3: Add `derive_standardized_names(input)` (`<base>_standardized.<ext>`), and the
      duplicate-input-without-`output` collision abort; wire into `standardize_videos()`.
- [x] T4: Tests in `tests/testthat/test-standardize-videos.R` (new file), modeled on
      `test-segment-videos.R`: compile-parity vs scalar, per-row overrides, auto-naming +
      collision abort, the four front-door rejections, and verify/manifest forwarding
      (binary-gated). Use `devtools::test(filter=)` / `load_all()` for fresh exports
      (M11 lesson), not `test_file()` under `library()`.
- [x] T5: Add the binary-gated E2E write test (AC6): 2-row real jobs table, `run = TRUE`,
      assert outputs exist and probe to spec; `skip_if` ffmpeg absent.
- [x] T6: `devtools::document()` (roxygen → `man/` + `NAMESPACE`); `devtools::test()`;
      `devtools::check()` to zero errors/warnings/notes (AC8).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: review — fresh evidence gathered for AC1–AC8 (all pass); consistency
  gate found two doc gaps (NEWS entry + `_pkgdown.yml` reference) and fixed both on
  branch. PR #15 opened (draft). Independent two-lens review pending.
- 2026-07-12: T6 — full suite 563 pass / 0 fail / 0 skip / 0 warn;
  `devtools::check()` zero errors/warnings/notes. All tasks done → status review.
- 2026-07-12: T2–T5 — added `standardize_videos()` + `derive_standardized_names()`
  (`R/ffmpeg.R`), roxygen/`document()`, and `test-standardize-videos.R` (20 tests,
  all pass; gated E2E/verify/manifest ran locally with ffmpeg present). Compile-parity
  with the scalar verb confirmed byte-identical; per-row overrides for all five knobs;
  `_standardized` auto-naming with duplicate-input collision abort.
- 2026-07-12: T1 — extracted `standardize_pipeline()` from `standardize_video()`;
  scalar verb now composes it. Existing `test-ffmpeg.R` green (99 pass, 0 fail),
  confirming no behavior change (AC7).
- 2026-07-12: created by /milestone-plan. Absorbs the "Batch sibling
  standardize_videos()" candidate (from M12 scope). Auto-naming scheme
  (`_standardized` suffix + collision abort) and all-five-knobs per-row overrides
  chosen at the plan gate (user deferred both to recommendation); test bar mirrors
  the `segment_videos()` suite.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review

**Reviewed 2026-07-12 on branch `m13-batch-standardization` (PR #15).** Branch in
sync with `origin/master` (0/0), 4 commits ahead — not stale.

### Acceptance-criteria evidence (fresh)

- AC1 ✓ — multi-row jobs (explicit `output`) returns one `command` row per job;
  each byte-identical to the scalar `standardize_video()` call
  (`identical()` TRUE for a 2-row width-override table). Also covered by
  `test-standardize-videos.R` "command is byte-identical to the scalar verb".
- AC2 ✓ — per-row `fps`/`vcodec` columns reflected per row; rows/knobs without a
  column fall back to the scalar arg (`vcodec = "libx265"` applied to all,
  per-row `fps` overrode scalar). Tests: "honors per-row knob columns",
  "knob column overrides the scalar arg per row", "scalar arg applies to every
  row without a column".
- AC3 ✓ — no `output` → `<base>_standardized.<input-ext>` (mp4 and mkv cases);
  duplicated `input` with no `output` aborts (`expect_error`). Tests:
  "auto-names outputs…", "keeps the input extension…", "aborts on a duplicated
  input…".
- AC4 ✓ — rejects non-data-frame, empty, missing `input`, wrong-type knob column,
  `NA` in a dimension column, non-character `vcodec` — each `expect_error`
  (6 validation tests pass).
- AC5 ✓ — `verify = list(width, height)` records a `verified` column (all TRUE);
  `manifest = TRUE` yields a 2-row manifest via `ffm_manifest()` with
  `output_size > 0` (binary-gated, ran locally).
- AC6 ✓ — E2E: 2-row real jobs table with per-row width/height/fps, `run = TRUE`,
  outputs exist and probe to their requested spec (48×32@5, 32×32@10).
- AC7 ✓ — scalar `test-ffmpeg.R` unchanged: 99 pass / 0 fail after the
  `standardize_pipeline()` refactor.
- AC8 ✓ — full suite 563 pass / 0 fail / 0 skip / 0 warn; `devtools::check()`
  0 errors / 0 warnings / 0 notes (both re-run fresh at review).

### Consistency gate

- `cairn_validate.py`: the only FAIL is the pre-existing `iso date format (7)`
  false-positive on `check 0/0/0` shorthand in M02–M08 archives (documented benign,
  LESSONS M09). M13 introduces no new occurrence (its file uses "zero errors…").
  All other checks PASS. Not a blocker; upstream script bug.
- Coverage completeness ✓ — AC1–AC8 all map to existing tasks T1–T6.
- `devtools::document()` — no `man/`/`NAMESPACE` diff.
- NEWS.md — added a "Batch standardization across files" entry for
  `standardize_videos()` (gate gap fixed at review).
- `_pkgdown.yml` — added `standardize_videos` to the Layer-2 reference index;
  `pkgdown::check_pkgdown()` → "No problems found" (gate gap fixed at review).
- README.md in sync with README.Rmd (no function index touched).
- No new top-level files → no `.Rbuildignore` change; check() had 0 notes.

### Independent review

<!-- filled after two-lens review + scorer -->

