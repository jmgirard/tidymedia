# M107: A backend with no encoder for the codec's family is refused by the verb the caller typed, `fallback` or not

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — it moves the blamed frame of an abort exported verbs raise
- **Branch/PR:** `m107-out-of-table-refusal-at-the-verb`

## Goal

An out-of-table `(backend, codec family)` pair is refused by the verb the caller
typed on both `fallback` arms, and the encoder pools the codec seam's
instruments mock stop being nvenc-only literals.

## Scope

**In:** The blame fix in `check_hardware_available()` and its sweep over
`nvenc_hardware_exports()`; a no-regression pin on the fallback an absent
in-table encoder still gets; one derivation of the mocked encoder pools from
`hardware_backend_families()`, routed into the three sites that spell the
triple literally; the seam test's videotoolbox-present arm; a forward
videotoolbox baseline for the probe grid.

**Out:**
- Falling back to software for an out-of-table pair — the gate kept the abort;
  reopening it needs a new D-entry.
- Re-measuring M095's reorder contract under videotoolbox — unavailable in
  principle: the backend postdates the reorder, so no pre-reorder ref carries
  it. T5 records a forward baseline instead.
- Routing `format_for_web_pipeline()` and `anonymize_pipeline()`'s direct
  `resolve_hw_encoder()` calls through the codec seam → ROADMAP candidate
  (M106 Out).
- The `prores_videotoolbox` container guard and the other five hardware-surface
  leftovers → ROADMAP candidate (M100 Out).

## Acceptance criteria

- [ ] AC1: For every export `nvenc_hardware_exports()` returns, a `video_codec`
      whose family the named backend has no encoder for aborts naming that
      export's own frame under `fallback = TRUE`, as it already does under
      `fallback = FALSE`, over every `(backend, family)` pair
      `hardware_backends()` × `hardware_codec_families()` holds and
      `hardware_backend_families()` omits.
- [ ] AC2: An encoder `hardware_backend_families()` does hold but the build does
      not list still falls back to software with a message rather than aborting
      under `fallback = TRUE`, for every `(backend, family)` pair that table
      holds.
- [ ] AC3: `devtools::test()` clean, `devtools::document()` produces no diff,
      and `devtools::check()` reports 0 errors, 0 warnings and no new NOTEs.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T1, T2
- AC3 → T1, T2, T3, T4, T5, T6

## Tasks

- [x] T1: Write the sweep first, red. Build each export's valid argument cell
      from `tm_timeout_call_specs()` (`tests/testthat/helper-timeout-sweep.R`),
      never by hand — a hand-built cell aborts on a missing required argument
      and masks the case (measured 2026-09-04: 12 of 14 verbs did). Cross
      `nvenc_hardware_exports()` × the three omitted pairs × `fallback` in
      `{FALSE, TRUE}` for AC1, and × the five held pairs under an empty build
      for AC2. An export whose formals carry no `video_codec` is recorded
      unreachable by reading its formals, never skipped by name. Record the red
      set; do not predict it.
- [x] T2: Fix `check_hardware_available()` (`R/ffmpeg.R:3300-3320`) so the
      out-of-table refusal fires at the front door on both `fallback` arms while
      an absent in-table encoder still returns early under `fallback = TRUE`.
      Correct the early return's comment, whose premise ("`fallback = TRUE`
      returns above, so this call can only pass or abort") is false — the
      refusal reaches `hardware_encoder()` through the mapper first. T1 green.
- [ ] T3: Derive the mocked encoder pools from `hardware_backend_families()` in
      one helper, at three levels (nvenc-present, videotoolbox-present, absent),
      and route the three literal spellings through it: `tm_nvenc_encoder_pools()`
      (`tests/testthat/helper-timeout-sweep.R:1390`), `seam_pools()`
      (`tests/testthat/test-codec-seam-bound.R:24`), `nvenc_order_pools`
      (`data-raw/nvenc-probe-order-baseline.R:182`). Keep the existing
      nvenc-pool-under-videotoolbox cell, which
      `test-codec-seam-bound.R:19-23` records as the harder half.
- [ ] T4: Cross the third pool level into `test-codec-seam-bound.R`'s existing
      `hw` loop, keeping the zero-probe assertion and the discrimination control
      in every arm. Add a sixth wrong form to `tm_nvenc_wrong_forms()` — a
      well-formed clean token naming no codec — since the five held forms all
      vary malformedness and none reaches `codec_family()`; the seam test's
      `expect_setequal`/`expect_length` pins move with it.
- [ ] T5: Add `"videotoolbox"` to the probe grid's `hw` loop
      (`data-raw/nvenc-probe-order-baseline.R:224`) and regenerate
      `data-raw/nvenc-probe-order-merge-base.rds` as a forward baseline.
      Measured cost ≈1.5× on a 25 s / 3040-row working-tree run (2026-09-04).
      No criterion binds this: it is an instrument property.
- [ ] T6: Record D085 — an out-of-table pair is a wrong argument, not an absent
      encoder, so it is sited at the front door on both `fallback` arms.
      Annotate D035, D074 and D076 with no forward pointer (IP4). Update
      `NEWS.md`; confirm `@param fallback`'s existing wording still holds.

## Work log

- 2026-09-04: created by /milestone-plan. Absorbs the ROADMAP candidate row "The probe grid's mocked encoder pools are nvenc-only" (M106 Out; M106 review F5; D079).
- 2026-09-04: criteria audit ran in FULL mode (surface tier user-facing), two passes, fresh-context [O] reader both times. Pass 1 returned six findings and reshaped the milestone: AC3 as drafted (M095's reorder contract re-measured under videotoolbox at `b538e63`) was unsatisfiable, since that ref predates the backend, and it was dropped; the grid's member set was found to be a procedure seeded from a hand-list; AC1's stated gap was factually wrong (the seam test already loops both backends — the pool is what is nvenc-only); the drafted pool correlation would have deleted the harder cell; and AC2 re-promised ground `test-hardware-backends.R:127` already holds. The fifth finding surfaced a shipped defect and became the gate's first question. Pass 2 over the gate-changed wording returned seven more: AC1 carried an instrument sub-clause (moved to T1), AC2 bound an unexported function under a user-facing tier with hand-listed axes and a pool cross vacuous for its own clause (dropped as a criterion; T3/T4 keep the work), the five wrong forms vary malformedness only (T4 adds the sixth), T2's no-regression half was bound by no criterion (now AC2), T1's red-cell prediction was unverifiable (T1 now measures rather than predicts), and AC3 left NOTEs unbounded (now bounded).
- 2026-09-04: plan gate chose fixing the blame defect in this milestone over pinning it and routing the fix, and over `/hotfix`, because the fix needs a D-entry — `check_hardware_available()`'s early return encodes D035/D074's siting reasoning and its comment states a false premise — and `/hotfix` writes none; falsified by the fix landing with no rule worth recording.
- 2026-09-04: plan gate chose keeping the abort under `fallback = TRUE` over falling back to software, because it is current behaviour and what `@param fallback` documents at eleven blocks, so only the blamed frame changes; falsified by a report of a caller who set `fallback = TRUE` expecting a wrong backend/codec pair to be tolerated.
- 2026-09-04: plan gate chose bounding AC1's verb axis by `nvenc_hardware_exports()` over the 14 with a `video_codec` formal and over one exemplar pair, because a seventeenth verb then joins on its own; falsified by an export the NAMESPACE filter admits that cannot reach the check at all.
- 2026-09-04: implement gate chose fixing the unmappable-codec class alongside the out-of-table class, because the front door must infer the family before it can test the table, so both classes move together; falsified by a caller who wants an unrecognized `video_codec` tolerated under `fallback = TRUE`.
- 2026-09-04: implement gate chose adding the sixth wrong form to the shared `tm_nvenc_wrong_forms()` table over a seam-test-local one, accepting a re-measurement of `tm_nvenc_dropped_master()`, `tm_nvenc_mismatch_master()` and the two pinned sweep counts; falsified by the re-measurement proving unstable.
- 2026-09-04: T1 red, measured on the branch: 24 of the 84 AC1 cells (14 reachable members x 3 omitted pairs x 2 fallback arms) blamed `purrr::pmap` instead of the member -- all 24 under `fallback = TRUE`, at the 8 members that fan out (`anonymize_video_batch`, `compare_videos_batch`, `crop_video_batch`, `picture_in_picture_batch`, `segment_video`, `segment_video_batch`, `separate_audio_video_batch`, `standardize_video_batch`). AC2's 10 cells and the domain test were green already.
- 2026-09-04: T2 green. The family sweep moved above `check_hardware_available()`'s `fallback` early return and each family goes through `hardware_encoder()` there, so the table lookup runs on both arms while the availability probe below still returns early. All 24 red cells now name their own member. Both false comments corrected (the early return's, and `resolve_hw_encoder()`'s claim that `fallback = TRUE` always returns above). `devtools::test()`: 0 failures, 10 warnings, 18 skips, 12128 passes -- the M095/M096 argument-outranks-the-probe sweeps and their two pinned counts unchanged, so the new front-door refusals displaced no error a caller had already earned.
- 2026-09-04: `test-nvenc-front-door.R`'s AC4 test "fallback = TRUE never lets the front door refuse an unmappable codec" asserted the defect (blame NOT the verb) and was rewritten to assert the verb, plus a new sibling pinning that an in-table encoder the build lacks still reaches the per-row fallback. The section header narrowed from "reaches no front-door guard" to "reaches no AVAILABILITY guard".

## Decisions

## Review
