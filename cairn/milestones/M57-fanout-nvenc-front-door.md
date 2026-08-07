# M57: A missing nvenc encoder is refused at the front door, on every verb that fans out

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M54, M56
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m57-fanout-nvenc-front-door

## Goal

Make an unavailable nvenc encoder abort at the fan-out verb the user called, not
inside `purrr::pmap()`.

## Scope

**In:** the nine verbs that take `hardware = c("none", "nvenc")` *and* fan out
through `ffm_batch()` — `segment_video()` plus `anonymize_video_batch()`,
`segment_video_batch()`, `standardize_video_batch()`, `crop_video_batch()`,
`format_for_web_batch()`, `separate_audio_video_batch()`,
`compare_videos_batch()`, `picture_in_picture_batch()`. Each gains a front-door
availability check, placed last in its front-door block (M41), reaching the
abort through one shared helper that `resolve_hw_encoder()` also calls. A new
`cairn/DECISIONS.md` entry licenses the construction-time abort gate, which
D024 places outside its diagnostic licence and requires be recorded before it
is built.

**Out:** the other seven `hardware`-bearing verbs — they call their pipelines
directly, so `call =` already lands on the verb (M47 F8) and a second guard
would only move error text. Every *other* pipeline-level validation on these
nine verbs → AC6 enumerates them into ROADMAP candidate rows. Memoizing
`ffmpeg_encoders()` → stays the standing candidate row; its open question is
cache lifetime, not where the guard fires. Hoisting resolution to the front
door → rejected at the plan gate (work log).

## Acceptance criteria

- [ ] AC1: On each of `segment_video()`, `anonymize_video_batch()`,
      `segment_video_batch()`, `standardize_video_batch()`, `crop_video_batch()`,
      `format_for_web_batch()`, `separate_audio_video_batch()`,
      `compare_videos_batch()` and `picture_in_picture_batch()`, a call with
      `hardware = "nvenc"`, `fallback = FALSE` and a `tidymedia.nvenc_encoders`
      seam lacking the required encoder aborts before `ffm_batch()` is reached,
      with a message naming the unavailable encoder and `conditionCall()` naming
      that verb. Evidence: a sweep test, one cell per verb, each matching the
      message *before* reading `conditionCall()` (M54); nine of nine green, each
      cell recorded naming `purrr::pmap` on master, plus one cell at
      `parallel = TRUE`, whose master reading is `furrr::future_pmap` (D033).
- [ ] AC2: The unavailable-nvenc abort text is emitted from exactly one
      function, and `resolve_hw_encoder()` reaches it by calling that function
      rather than by carrying its own copy. Evidence: reading the two functions,
      plus a test asserting the front-door and pipeline messages are
      string-identical for one `(video_codec, hardware, fallback)` triple.
- [ ] AC3: On a `_batch` verb carrying a `video_codec` column, the guard checks
      every distinct family the column spells, with an `NA` cell and an absent
      column both spelling the h264 family that `resolve_hw_encoder()`'s `NULL`
      sentinel resolves to (`R/ffmpeg.R:2475-2480`, D022); a verb with no such
      column checks the argument alone. Evidence: a two-row table (H.264 + AV1)
      under a seam listing only `h264_nvenc` aborts naming the AV1 encoder, and
      compiles under a seam listing both; an all-`NA` column behaves as h264.
- [ ] AC4: `fallback = TRUE` reaches no front-door guard at all — including the
      column sweep, whose `codec_family()` call aborts on an unmappable codec
      regardless of `fallback` (`R/ffmpeg.R:2440-2452`). Evidence: a test on one
      fan-out verb asserting no abort and the *same count* of fallback
      `cli_inform()` messages as before the change; the pre-existing fallback
      tests green.
- [ ] AC5: No test file is re-baselined and no compiled command the suite
      exercises changes. Evidence: `git diff --stat tests/` shows additions
      only; `devtools::test()` green; `devtools::check()` reports `Status: OK`.
- [ ] AC6: Each `cli::cli_abort()` site that `grep -n "cli_abort(" R/` returns
      inside the nine verbs' `*_pipeline()` functions carries a recorded
      disposition — guarded here, ROADMAP candidate row, or left with a stated
      reason. Evidence: the grep output and its dispositions in the work log.
- [ ] AC7: `devtools::document()` produces no diff; `NEWS.md` carries the
      user-visible change; `R/ffmpeg.R`'s CRLF line endings survive every commit
      (M35/M48).

## Coverage

- AC1 → T2, T3, T4
- AC2 → T1
- AC3 → T4, T5
- AC4 → T6
- AC5 → T8
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1: Write the D-entry licensing a construction-time abort gate (D024's
      third excluded shape) — before any code, as D024 requires. Then extract
      the abort from `resolve_hw_encoder()` (`R/ffmpeg.R:2498-2506`) into a
      shared `check_nvenc_available()`; `resolve_hw_encoder()` calls it. No
      behavior change; suite green.
- [ ] T2: Write the nine-cell sweep test (message first, then
      `conditionCall()`), plus the `parallel = TRUE` cell. Record each cell's
      master reading. Red on master.
- [ ] T3: Add the front-door guard to `segment_video()`, last in its front-door
      block beside the M48 guards (`R/ffmpeg.R:2650-2662`).
- [ ] T4: Add it to the eight `_batch` verbs, reading the `hardware` argument
      and any `video_codec` column; place it after `check_batch_codec_col()` so
      a malformed codec still reports first (M41 precedence).
- [ ] T5: Column-spanning tests (H.264 + AV1; all-`NA`; no column), verified
      against each verb's real column names — `picture_in_picture_batch()` takes
      `overlay`, `compare_videos_batch()` an `inputs` list-column (M54).
- [ ] T6: `fallback = TRUE` test asserting no abort and the message count.
- [ ] T7: Run AC6's grep, record the dispositions in the work log, add the
      ROADMAP candidate rows it produces.
- [ ] T8: `@param hardware` wording on the nine verbs, `NEWS.md`,
      `devtools::document()`, `devtools::test()`, `devtools::check()`; check
      `grep -c $'\r' R/ffmpeg.R` against the default branch before every commit
      touching it (M35/M48).

## Work log

- 2026-08-07: created by /milestone-plan; promotes the M54 review F1/F4 candidate row.
- 2026-08-07: criteria audit ([O] reader) returned findings on all six drafted criteria — AC1 scoped itself to an absent section and ignored the `furrr::future_pmap` path; AC2 claimed a firing-condition invariance AC3 falsifies, over a grep matching two `cli_inform()` lines; AC3 was undefined for D022's `NA` cells; AC3/AC4 collided on `codec_family()` aborting regardless of `fallback`; AC4 asserted presence where count was meant; AC5's `.new`-file check was satisfied by the state it excluded; AC6 quantified over a call graph no grep computes. Six fixed at the gate, the hoist-vs-duplicate finding raised as a gate question.
- 2026-08-07: plan gate chose duplicating the check at the front door over hoisting resolution there, because hoisting re-forks the resolver seam for per-row `video_codec` columns and undoes M56's fix that made `standardize_pipeline()` hand `hardware` to the seam unresolved; falsified by a front-door guard and a pipeline guard observed firing on different inputs.
- 2026-08-07: plan gate chose nvenc availability alone over every pipeline-level validation on the nine verbs, because the wider cut trips the sizing tripwires; falsified by AC6's enumeration returning few enough sites to have been folded in.
- 2026-08-07: implement gate skipped — the plan gate settled hoist-vs-duplicate, scope, AC6 and the probe cache, and nothing left open was more than a helper signature.
- 2026-08-07 (T1): D035 written before any code, as D024 requires of a shape its third exclusion reserved. Abort extracted from `resolve_hw_encoder()` into `check_nvenc_available()`; the resolver now reaches it by calling it. `devtools::test()` FAIL 0 | PASS 3856, the same 4 warnings and 5 skips as before, all in test files this milestone does not touch. `R/ffmpeg.R` CRLF count 5749 -> 5791 for 42 net added lines, diffstat 55/13 (M35/M48).

## Decisions

- 2026-08-07 (T1): the shared guard takes `video_codec` as either one value or a LIST of values, so one function serves the scalar resolver and a `_batch` verb whose `video_codec` column spells several families in one call. `NULL` and its column form `NA` (D022) both resolve to the h264 family, matching `resolve_hw_encoder()`'s sentinel branch — the two readings must agree, or the front door would refuse a call the pipeline compiles, which is D035's second condition.
- 2026-08-07 (T1): `check_nvenc_available()` returns early on `fallback = TRUE` rather than sweeping and then suppressing. Sweeping a column would reach `codec_family()`, which aborts on an unmappable codec regardless of `fallback` (`R/ffmpeg.R:2440-2452`), so a `fallback = TRUE` call that falls back happily today would start being refused.

## Review
