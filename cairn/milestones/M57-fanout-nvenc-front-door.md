# M57: A missing nvenc encoder is refused at the front door, on every verb that fans out

- **Status:** review
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

- [x] AC1: On each of `segment_video()`, `anonymize_video_batch()`,
      `segment_video_batch()`, `standardize_video_batch()`, `crop_video_batch()`,
      `format_for_web_batch()`, `separate_audio_video_batch()`,
      `compare_videos_batch()` and `picture_in_picture_batch()`, a call with
      `hardware = "nvenc"`, `fallback = FALSE` and a `tidymedia.nvenc_encoders`
      seam lacking the required encoder aborts before `ffm_batch()` is reached,
      with a message naming the unavailable encoder and `conditionCall()` naming
      that verb. Evidence: a sweep test, one cell per verb, each matching the
      message *before* reading `conditionCall()` (M54); nine of nine green, each
      cell recorded naming `purrr::pmap` on master, plus one cell at
      `parallel = TRUE`, whose master reading is furrr's internal `...furrr_fn`
      closure rather than the `furrr::future_pmap` this criterion first
      predicted (D033).
- [x] AC2: The unavailable-nvenc abort text is emitted from exactly one
      function, and `resolve_hw_encoder()` reaches it by calling that function
      rather than by carrying its own copy. Evidence: reading the two functions,
      plus a test asserting the front-door and pipeline messages are
      string-identical for one `(video_codec, hardware, fallback)` triple.
- [x] AC3: On a `_batch` verb carrying a `video_codec` column, the guard checks
      every distinct family the column spells, with an `NA` cell and an absent
      column both spelling the h264 family that `resolve_hw_encoder()`'s `NULL`
      sentinel resolves to (`R/ffmpeg.R:2475-2480`, D022); a verb with no such
      column checks the argument alone. Evidence: a two-row table (H.264 + AV1)
      under a seam listing only `h264_nvenc` aborts naming the AV1 encoder, and
      compiles under a seam listing both; an all-`NA` column behaves as h264.
- [x] AC4: `fallback = TRUE` reaches no front-door guard at all — including the
      column sweep, whose `codec_family()` call aborts on an unmappable codec
      regardless of `fallback` (`R/ffmpeg.R:2440-2452`). Evidence: a test on one
      fan-out verb asserting no abort and the *same count* of fallback
      `cli_inform()` messages as before the change; the pre-existing fallback
      tests green.
- [x] AC5: No existing test is re-baselined and no compiled command the suite
      exercises changes, with one named exception: `test-nvenc.R`'s M54 blame
      test pins the misblame this milestone removes and its own comment
      anticipates going red here. Its three fan-out assertions flip from
      `purrr::pmap` to the verb named; its scalar control is untouched.
      Evidence: `git diff tests/` shows additions only outside that one test;
      `devtools::test()` green; `devtools::check()` reports `Status: OK`.
- [x] AC6: Each `cli::cli_abort()` site that `grep -n "cli_abort(" R/` returns
      inside the nine verbs' `*_pipeline()` functions carries a recorded
      disposition — guarded here, ROADMAP candidate row, or left with a stated
      reason. Evidence: the grep output and its dispositions in the work log.
- [x] AC7: `devtools::document()` produces no diff; `NEWS.md` carries the
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
- [x] T2: Write the nine-cell sweep test (message first, then
      `conditionCall()`), plus the `parallel = TRUE` cell. Record each cell's
      master reading. Red on master.
- [x] T3: Add the front-door guard to `segment_video()`, last in its front-door
      block beside the M48 guards (`R/ffmpeg.R:2650-2662`).
- [x] T4: Add it to the eight `_batch` verbs, reading the `hardware` argument
      and any `video_codec` column; place it after `check_batch_codec_col()` so
      a malformed codec still reports first (M41 precedence).
- [x] T5: Column-spanning tests (H.264 + AV1; all-`NA`; no column), verified
      against each verb's real column names — `picture_in_picture_batch()` takes
      `overlay`, `compare_videos_batch()` an `inputs` list-column (M54).
- [x] T6: `fallback = TRUE` test asserting no abort and the message count.
- [x] T7: Run AC6's grep, record the dispositions in the work log, add the
      ROADMAP candidate rows it produces.
- [x] T8: `@param hardware` wording on the nine verbs, `NEWS.md`,
      `devtools::document()`, `devtools::test()`, `devtools::check()`; check
      `grep -c $'\r' R/ffmpeg.R` against the default branch before every commit
      touching it (M35/M48).

## Work log

- 2026-08-07: created by /milestone-plan; promotes the M54 review F1/F4 candidate row.
- 2026-08-07: criteria audit ([O] reader) returned findings on all six drafted criteria — AC1 scoped itself to an absent section and ignored the `furrr::future_pmap` path; AC2 claimed a firing-condition invariance AC3 falsifies, over a grep matching two `cli_inform()` lines; AC3 was undefined for D022's `NA` cells; AC3/AC4 collided on `codec_family()` aborting regardless of `fallback`; AC4 asserted presence where count was meant; AC5's `.new`-file check was satisfied by the state it excluded; AC6 quantified over a call graph no grep computes. Six fixed at the gate, the hoist-vs-duplicate finding raised as a gate question.
- 2026-08-07: plan gate chose duplicating the check at the front door over hoisting resolution there, because hoisting re-forks the resolver seam for per-row `video_codec` columns and undoes M56's fix that made `standardize_pipeline()` hand `hardware` to the seam unresolved; falsified by a front-door guard and a pipeline guard observed firing on different inputs.
- 2026-08-07: plan gate chose nvenc availability alone over every pipeline-level validation on the nine verbs, because the wider cut trips the sizing tripwires; falsified by AC6's enumeration returning few enough sites to have been folded in.
- 2026-08-07: implement gate skipped — the plan gate settled hoist-vs-duplicate, scope, AC6 and the probe cache, and nothing left open was more than a helper signature.
- 2026-08-07 (T8): the availability note added to the nine fan-out verbs' `@param hardware` blocks; `devtools::document()` rewrote exactly those nine `.Rd` files and a second run produced no further change. The M54 NEWS paragraph that stated the fan-out limitation is rewritten, since M57 removes what it described. `devtools::test()` FAIL 0 | PASS 3918 with the same 4 warnings and 5 skips as at T1; `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes. `git diff master -- tests/` outside `test-nvenc.R` has 0 deleted lines, so AC5's exception is the only re-baseline. `R/ffmpeg.R` CRLF 5749 on master -> 5922 here for 173 added lines, 0 deleted (M35/M48).
- 2026-08-07 (T7): AC6 enumeration run. `grep -n "cli_abort(" R/` attributed to the enclosing function returns six sites inside the nine verbs' `*_pipeline()` functions, and each was measured on the branch rather than read off the source: `separate_stream_pipeline():592` (copy video codec against `hardware`), `segment_pipeline():2810` and `:2826` (the two `reencode = FALSE` contradictions), `compare_videos_pipeline():5251` (audio codec, no mapped audio) and `:5263` (the two-input `resize` limit), `picture_in_picture_pipeline():5396` (audio codec, no mapped audio). Disposition: all six blame `purrr::pmap` today and all six are out of M57's scope by the plan gate's nvenc-only cut, so they take one grouped ROADMAP candidate row. None is guarded here, and none is left without a row.
- 2026-08-07 (T7): the first `:5263` measurement recorded a `jobs` schema error, not the resize limit — `compare_videos_batch()` takes `output` as a column, never an argument. Re-measured against the verb's real column names before the disposition was written (M54).
- 2026-08-07 (T5/T6): column-spanning and fallback tests added. A two-row table spelling h264 and av1 under a seam holding only `h264_nvenc` aborts naming `av1_nvenc`, and compiles under a seam holding both; an `NA` cell and an absent column both read as h264; `format_for_web_batch()` checks h264. `fallback = TRUE` emits 2 fallback messages for a 2-row table on the branch and 2 on master, and an unmappable `prores` cell under `fallback = TRUE` still fails inside the fan-out rather than at the front door. Suite FAIL 0 | PASS 3918.
- 2026-08-07 (T5): the sweep helper used `utils::modifyList()` to apply per-test overrides, which merged a replacement `jobs` tibble column-wise into the template's (a tibble is a list) and would have deleted any `NULL`-valued override instead of setting it. Replaced with direct element assignment; two tests were erroring on it.
- 2026-08-07 (T4): eight `_batch` guards added immediately before each `ffm_batch()` call, which is where M41 puts a guard added for blame, rather than after `check_batch_codec_col()` as the plan said — on several verbs that anchor sits mid-block, so output derivation and duplicate-path checks would have started reporting after it. `separate_audio_video_batch()` takes its guard before the N->2N reshape, while `jobs` still carries the caller's `video_codec` column. New helper `batch_video_codecs()` yields the column's distinct cells, or the argument where the verb honours no column. `format_for_web_batch()` passes `"libx264"`: its recipe fixes the codec by identity. Sweep 46/46 green; full suite FAIL 0 | PASS 3902, the same 4 warnings and 5 skips as at T1. `R/ffmpeg.R` +90 lines, 0 deletions, CRLF 5805 -> 5895 (M35/M48).
- 2026-08-07 (T4): the M54 blame test flipped under AC5's amended exception — three fan-out assertions now name their verb, the scalar control untouched, and the `test_that()` title corrected, since it read "still blames the fan-out".
- 2026-08-07 (T3): `segment_video()` guarded; its sweep cell is green and the other eight plus the parallel cell stay red until T4. Two pipelines abort BEFORE reaching `resolve_hw_encoder()` — `segment_pipeline()` on a non-re-encoding cut naming an encoder, and the shared separation recipe on `video_codec = "copy"` — so each front door mirrors that precondition, with two tests asserting the pipeline's own message still reports there.
- 2026-08-07 (T3): amendment — AC5 forbade re-baselining any existing test, but `test-nvenc.R`'s M54 blame test asserts the misblame M57 removes and its own comment says it goes red when this lands. AC5 amended at a mini gate to name that one exception; the flip itself lands with T4.
- 2026-08-07 (T2): master readings recorded on a worktree at master — nine of nine fan-out verbs blame `purrr::pmap` with the nvenc-unavailable message, `separate_audio_video_batch` reporting "In index: 2" for a 1-row table because it reshapes N->2N (M45). Sweep red on the branch: nine blame cells plus the parallel cell; every message assertion already passes, which is what confirms these are the nvenc failure and not a schema error (M54).
- 2026-08-07 (T2): amendment — AC1 predicted a `furrr::future_pmap` master reading at `parallel = TRUE`; measured, it is furrr's internal `...furrr_fn` closure. Criterion amended at a mini gate to record the measurement and that the prediction was wrong.
- 2026-08-07 (T1): D035 written before any code, as D024 requires of a shape its third exclusion reserved. Abort extracted from `resolve_hw_encoder()` into `check_nvenc_available()`; the resolver now reaches it by calling it. `devtools::test()` FAIL 0 | PASS 3856, the same 4 warnings and 5 skips as before, all in test files this milestone does not touch. `R/ffmpeg.R` CRLF count 5749 -> 5791 for 42 net added lines, diffstat 55/13 (M35/M48).

## Decisions

- 2026-08-07 (T1): the shared guard takes `video_codec` as either one value or a LIST of values, so one function serves the scalar resolver and a `_batch` verb whose `video_codec` column spells several families in one call. `NULL` and its column form `NA` (D022) both resolve to the h264 family, matching `resolve_hw_encoder()`'s sentinel branch — the two readings must agree, or the front door would refuse a call the pipeline compiles, which is D035's second condition.
- 2026-08-07 (T1): `check_nvenc_available()` returns early on `fallback = TRUE` rather than sweeping and then suppressing. Sweeping a column would reach `codec_family()`, which aborts on an unmappable codec regardless of `fallback` (`R/ffmpeg.R:2440-2452`), so a `fallback = TRUE` call that falls back happily today would start being refused.

## Review
