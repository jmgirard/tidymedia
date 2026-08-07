# M54: Correct the `run = FALSE` purity claim for the nvenc encoder probe

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m54-nvenc-dry-run-purity

## Goal

Make the package's stated purity contract true by recording that resolving
`hardware = "nvenc"` probes FFmpeg while building the pipeline, `run` notwithstanding.

## Scope

**In:** a superseding D-entry restating the `run = FALSE` purity claim as a condition
on probe shape rather than a hand-list of exceptions; the matching correction to
`cairn/DESIGN.md`'s Conventions bullet; live `hardware = "nvenc"` cases in the three
`run = FALSE` purity tests, replacing the two comments that today exclude nvenc; the
probe stated on every `hardware`-bearing Rd topic, guarded by a test; and `call = call`
threaded at the two `resolve_hw_encoder()` sites that omit it
(`R/ffmpeg.R:1135`, `R/ffmpeg.R:1393`), so an nvenc-unavailable abort names the verb.

**Out:**
- Making the probe lazy — weighed and rejected at the plan gate; D034 records why.
- Caching `has_nvenc()` / `ffmpeg_encoders()` so a `_batch` run probes once rather than
  once per row → ROADMAP candidate row added by this plan.
- D024's `two_pass` normalization exception → unchanged; D034 restates, never narrows it.
- Any change to which encoder a call resolves to → no runtime behavior changes here.

## Acceptance criteria

- [ ] AC1 `cairn/DECISIONS.md` gains **D034**, which quotes D024's sentence "Every verb's
      `run = FALSE` call runs no binary — with **the two-pass normalization path the sole
      exception**" verbatim, names it superseded, and states the replacement as a
      *condition on probe shape* — a probe whose result enters the compiled command runs
      when the pipeline is built, which is D013's shape — rather than as a list of verbs,
      per D024's own "Scope: conditions, not a verb list".
- [ ] AC2 `cairn/DESIGN.md`'s Conventions bullet no longer claims a `run = FALSE` call runs
      no binary with the two-pass path as sole exception, and states D034's condition
      instead. Verified by `grep -n "sole exception" cairn/DESIGN.md` returning nothing.
- [ ] AC3 `tests/testthat/test-audio-stream-passthrough.R:198` and
      `test-audio-stream-crop-segment.R:325` replace their nvenc-excluding comments with
      live `hardware = "nvenc", run = FALSE` cases, and `test-audio-stream-format-web.R`'s
      purity block (`:129-152`) gains one. Each counts `find_ffmpeg()` invocations with
      `withr::local_options(tidymedia.nvenc_encoders = NULL)` pinning the option seam
      unset, and asserts the count exceeds zero. Each is shown to discriminate: with
      `has_nvenc()` stubbed to return `TRUE` without probing, it goes red.
- [ ] AC4 Every Rd topic whose argument names include `hardware` states that resolving
      `hardware = "nvenc"` probes the FFmpeg binary for the encoder, so such a call is not
      binary-free even under `run = FALSE`. Enumerated by a test reading `../../man/*.Rd`
      with `tools::Rd_db("tidymedia")` as fallback and splitting `\item{}` names on commas,
      both per `tests/testthat/test-audio-index-docs.R:20-40`; it asserts it found at
      least 17 such topics.
- [ ] AC5 `grep -n "resolve_hw_encoder(" R/*.R` shows every call site passing `call =`.
- [ ] AC6 PROFILE.md's verify slot clean — `devtools::check()` 0 errors / 0 warnings, read
      from `<pkg>.Rcheck/00check.log`'s `Status:` line — and `devtools::test()` passes.
- [ ] AC7 `grep -c $'\r' R/ffmpeg.R` on the branch tip equals 5652, the count on `master`.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5, T6
- AC5 → T1
- AC6 → T7
- AC7 → T1, T7

## Tasks

- [x] T1 Thread `call = call` into `resolve_hw_encoder()` at `R/ffmpeg.R:1135`
      (`format_for_web_pipeline()`) and `R/ffmpeg.R:1393` (`standardize_pipeline()`).
      `R/ffmpeg.R` is the repo's only CRLF file: read and write it as bytes restoring
      `\r\n`, and check that one file's diffstat before committing (LESSONS M35/M48).
- [x] T2 Write D034 in `cairn/DECISIONS.md` per AC1.
- [x] T3 Correct `cairn/DESIGN.md`'s Conventions bullet to match D034.
- [ ] T4 Extend the three purity tests per AC3; prove each new case discriminates by
      stubbing `has_nvenc()`.
- [ ] T5 Add the probe sentence to the shared `@param hardware` roxygen blocks; run
      `devtools::document()`.
- [ ] T6 Add the Rd guard test per AC4, reusing `rd_sources()` / `rd_param_names()` from
      `tests/testthat/test-audio-index-docs.R:20-40`.
- [ ] T7 Run `devtools::document()`, `devtools::test()`, `devtools::check()`; confirm the
      CRLF count and the `00check.log` `Status:` line.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned 9 findings on this milestone's criteria: DESIGN.md left uncorrected by AC1; AC1's verb-list scoping contradicting its own no-hand-list clause; "three" comment-excluding tests where only two exist; the `tidymedia.nvenc_encoders` option seam making AC3 red for the wrong reason; AC4's literal `\item{hardware}` scan missing six `\item{hardware, fallback}` topics; AC4's Rd-source order reversed against the cited precedent; and the mandated sentence contradicted on `segment_video(reencode = FALSE)`, which aborts before probing. All fixed before AC wording was written; none needed a gate question. AC5 and AC7 passed all three questions.
- 2026-08-06: plan gate chose correcting the record over making the probe lazy, because `resolve_hw_encoder()` is a probe whose result enters the compiled command — D024's own taxonomy calls that D013's analyze-then-build shape, already licensed — and because the only true lazy seam is `ffm_finish()`/`ffm_batch()`, the sole readers of `run`, which needs the pipeline-object hook D024/RR02 Q3 rejected, and would force a dry run on a GPU-less machine to print a command that aborts; falsified by a report of a dry run's compiled command differing from what a subsequent `run = TRUE` call executes.
- 2026-08-06: plan gate kept the per-row re-probe cost out, because caching `has_nvenc()` / `ffmpeg_encoders()` needs its own lifetime decision (a user installing FFmpeg mid-session), which is a separate question from whether the probe is licensed; falsified by a measured `_batch` stall attributable to repeated `ffmpeg -encoders` calls.
- 2026-08-06: T1 done. Reproduced the blame defect before fixing: with `tidymedia.nvenc_encoders = character(0)`, `standardize_video(hardware = "nvenc", run = FALSE)` blamed `standardize_pipeline(...)` and `format_for_web(...)` blamed `format_for_web_pipeline(...)`, while `crop_video` and `anonymize_video` already named the verb. Added a test to `tests/testthat/test-nvenc.R` carrying those two already-correct verbs as discriminating controls, confirmed red on exactly the two targets, then threaded `call = call` at `R/ffmpeg.R:1135,1393`. `devtools::test()`: 0 failures, 3458 passing, 5 skips; the 4 warnings are the pre-existing M44 dropped-track diagnostic in files this diff does not touch. `R/ffmpeg.R` edited as bytes: CRLF count 5652 unchanged, diffstat 2 insertions / 2 deletions.

- 2026-08-06: T2 done. D034 appended to `cairn/DECISIONS.md`. It states the rule as a condition on probe shape and enumerates today's instances by a stated grep over the execution seams (`run_program(`, `ffmpeg(`, `ffprobe(`, `mediainfo(`) filtered to build-time reachability, rather than by recall. That grep found exactly two: D013's loudnorm analysis (`R/loudnorm_two_pass.R:140,182`) and the nvenc resolver (`R/ffmpeg.R:2283`, sole internal caller `has_nvenc()` at `:2388`); `ffmpeg_codecs()` has no internal caller. Also established that D024's bullet was false on the day it was written — nvenc shipped at M31 on 2026-07-26, D024 is dated 2026-07-30 — so the entry records a list falsified by existing code, not by later work.

- 2026-08-06: T3 done. `cairn/DESIGN.md`'s Conventions bullet restated to match D034; `grep -n "sole exception" cairn/DESIGN.md` now returns nothing, satisfying AC2. The replacement also drops the old bullet's implication that D024's diagnostic probes are a `run = TRUE`-only *addition* to the same list, since they are a different shape entirely.

## Decisions

## Review
