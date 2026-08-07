# M56: A bad codec token names the verb's argument, never Layer 1's

- **Status:** planned
- **Priority:** normal
- **Depends on:** M54
- **Driving RR:** —
- **Principles touched:** IP1

## Goal

Make a malformed codec *token* blame the verb's own argument on the four verbs whose
pipelines hand a user value to `ffm_codec()` directly, as M41 already did for non-string
values.

## Scope

**In:** routing `extract_audio_pipeline()` (`R/ffmpeg.R:464`), `convert_audio_pipeline()`
(`:925`) and the video side of `standardize_pipeline()` (`:1419`) through
`apply_audio_codec()` / `apply_video_codec()` with `call =` threaded; passing `call =` at
`normalize_audio_pipeline()`'s seam call (`:2159`), the one seam call that omits it and so
blames the internal helper; and extending `test-codec-arg-front-door.R`'s family sweep,
whose `codec_front_door_bad` set is today only non-string shapes, with a malformed-but-string
token. Also makes D022's first bullet true, which already names the two seams as carrying
the family rule though these pipelines bypass them.

**Out:**
- Giving `ffm_codec()` `arg` / `call` parameters — weighed and rejected at the plan gate
  under IP1; recorded in the work log, no D-entry (it decides nothing new).
- `anonymize_pipeline()`'s direct `ffm_codec()` at `R/ffmpeg.R:1601` → stays; its value is
  pre-token-checked at `:1563` with `call =` threaded, placed there deliberately for
  error precedence (M41).
- The completeness check tying `helper-codec-family.R` to the exports → already exists at
  `tests/testthat/test-codec-arg-front-door.R:255-271`; nothing to add.
- `verify_media`'s codec arguments → excluded from the family sweep by design
  (`helper-codec-family.R:10-12`); unchanged.

## Acceptance criteria

- [ ] AC1 `grep -n "ffm_codec(" R/*.R` shows every remaining direct call passing either a
      package literal or a value already token-checked with `call =` threaded at that
      verb's front door. The three sites named in Scope no longer pass an unchecked user
      value, and `R/ffmpeg.R:2159` passes `call =`.
- [ ] AC2 `extract_audio()`, `convert_audio()`, `standardize_video()` and their `_batch`
      siblings, given a malformed-but-string codec token, emit a message naming the verb's
      own argument (`audio_codec` / `video_codec`), never Layer-1's `audio` / `video`, and
      blame the verb rather than `ffm_codec()` or `purrr::pmap()`. `normalize_audio(
      audio_codec = "aac -evil")` blames `normalize_audio()`, not `normalize_audio_pipeline()`.
- [ ] AC3 `codec_front_door_bad` (`tests/testthat/test-codec-arg-front-door.R:55-59`)
      gains `"aac -evil"`, and the file's four existing assertions — names the verb's own
      argument (`:86`), never Layer-1's (`:88-90`), blames the verb (`:93-95`), no
      `In index:` (`:98-99`) — pass for every verb × argument pair in
      `tests/testthat/helper-codec-family.R`. The new value is shown to discriminate on the
      four target verbs specifically: reverting each routing change turns it red. Verbs
      that already front-door with `check_token()` pass it unchanged, which is expected,
      not evidence.
- [ ] AC4 No compiled command changes. For each verb touched, the `run = FALSE` compiled
      string is byte-identical to `master`'s across a grid varying the codec value over
      that verb's legal set — `NULL`, a literal it accepts, and `"copy"` only where it
      accepts it (`helper-codec-family.R:100-104`) — with
      `withr::local_options(tidymedia.nvenc_encoders = ...)` pinned so any nvenc cell is
      machine-independent. `data-raw/codec-guard-baseline.R` is the instrument.
- [ ] AC5 PROFILE.md's verify slot clean — `devtools::check()` 0 errors / 0 warnings, read
      from `<pkg>.Rcheck/00check.log`'s `Status:` line — and `devtools::test()` passes.
- [ ] AC6 `grep -c $'\r' R/ffmpeg.R` on the branch tip equals 5652, the count on `master`.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T2, T5

## Tasks

- [ ] T1 Baseline first: capture the `run = FALSE` compiled commands on `master` for the
      affected verbs via `data-raw/codec-guard-baseline.R`, with `tidymedia.nvenc_encoders`
      pinned. Commit the baseline **before** mutating anything — probing uncommitted work
      reverts the feature itself (LESSONS M44).
- [ ] T2 Route the three direct sites through the seams and add `call =` at
      `R/ffmpeg.R:2159`. `R/ffmpeg.R` is the repo's only CRLF file: read and write it as
      bytes restoring `\r\n`, and check that one file's diffstat before committing
      (LESSONS M35/M48).
- [ ] T3 Extend `codec_front_door_bad` per AC3; prove discrimination by reverting each of
      the four changes in turn and confirming the sweep goes red for that verb.
- [ ] T4 Re-run the baseline and diff against T1's; any difference is a defect, not a
      re-baseline.
- [ ] T5 Run `devtools::document()`, `devtools::test()`, `devtools::check()`; confirm the
      CRLF count and the `00check.log` `Status:` line.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned 5 findings on this milestone's criteria. Two changed the scope: the drafted AC1 was internally unsatisfiable, since `anonymize_pipeline()` at `R/ffmpeg.R:1601` also hands `ffm_codec()` a user value and its pre-guard must stay put, so the grep claim was narrowed to "literal, or already token-checked with `call =`"; and a drafted AC mandating a `helper-codec-family.R` ↔ exports completeness test was **dropped**, that test already existing at `test-codec-arg-front-door.R:255-271`, and its asserted 21 = 20 equality being false besides (`verify_media` is excluded by design). Also fixed: AC3 passing vacuously on verbs that already front-door with `check_token()`, and AC4's grid containing cells that abort rather than compile plus an unpinned machine-dependent nvenc axis. All disposed before AC wording was written; none needed a gate question.
- 2026-08-06: investigation found a fourth leaking site the ROADMAP row did not name — `convert_audio_pipeline()` at `R/ffmpeg.R:925` — measured emitting "`audio` must be a single clean token" blamed on `ffm_codec()`. Folded into scope.
- 2026-08-06: plan gate chose routing through the existing seams over giving `ffm_codec()` `arg` / `call` parameters, because the seams already take `call =` and their `caller_arg()` resolves to `video_codec` / `audio_codec` — the correct public names for all four verbs — so the fix needs no Layer-1 change, where the alternative would start Layer 1 carrying Layer-2 argument names against IP1 and the boundary comment at `R/ffmpeg.R:2469-2470`; falsified by a verb whose public codec argument is named something other than `video_codec` / `audio_codec`, which the seams cannot then blame correctly.

## Decisions

## Review
