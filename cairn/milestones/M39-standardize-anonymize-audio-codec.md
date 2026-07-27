# M39: `audio_codec` for `standardize_video` and `anonymize_video` (+ batch)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** `m39-standardize-anonymize-audio-codec`

## Goal

Give the two remaining configurable video transforms a user-facing
`audio_codec`, so D017's documented remedy for the copy-into-an-incompatible-
container trap actually exists on them.

## Scope

**In:** `audio_codec = "copy"` on `standardize_video()`, `anonymize_video()`,
and both `_batch` siblings, the formal placed before `run` (M34 gate
precedent). Threaded into `standardize_pipeline()` (`R/ffmpeg.R:789`) and
`anonymize_pipeline()` (`R/ffmpeg.R:908`), replacing the literal
`audio = "copy"` at `R/ffmpeg.R:816` and its anonymize twin. `NULL` emits no
`-codec:a` (D017's escape hatch) — `ffm_codec()` already both skips NULL and
runs `check_token()`, so Layer 1 is untouched (IP1). Batch: a per-row
`audio_codec` column guarded by `check_batch_codec_col(jobs, "audio_codec")`
and resolved by `batch_codec_cell()`, `NA` → the sentinel (D017);
`hardware`/`fallback` stay batch-wide (D016). Roxygen `@param` + a NEWS entry.

Deliberate asymmetry, not to be "fixed": these batches keep their existing
no-`NA` inline guard for the `video_codec` column, because that argument
defaults to a literal `"libx264"` and has no sentinel, while `audio_codec`
does.

**Out:** `convert_audio`'s `format` → `audio_codec` rename, and the D-entry
closing the codec sweep → M40. `pixel_format` on the M34 verbs → stays
deferred under D016. A quality/rate-control knob → the standing ROADMAP
candidate row (M31 Q4). No new exports, so `_pkgdown.yml` is untouched.

## Acceptance criteria

- [ ] AC1 All four verbs accept `audio_codec`, default `"copy"`, formal before
      `run`; a test compiles each with a named encoder and asserts
      `-codec:a <name>` appears.
- [ ] AC2 Default output is unchanged: a test compiles a default call of each
      of the four verbs and compares byte-for-byte against the command the
      pre-milestone code produced.
- [ ] AC3 `audio_codec = NULL` emits no `-codec:a` on all four; tested.
- [ ] AC4 An invalid codec token aborts via `check_token()` — at the front door
      on the scalars, per row on the batches; the batch case tested with 2+
      rows (M18 lesson).
- [ ] AC5 A per-row `audio_codec` column overrides the scalar argument; an
      all-`NA` (logical) column is accepted and resolves to the sentinel; a
      numeric column aborts naming the column (M34 lesson). Both boundaries
      tested.
- [ ] AC6 An execution test proves `"copy"` stream-copies and a named encoder
      re-encodes, using a source codec that is not the container default
      (M35 lesson — the MP3-in-MP4 fixture), `skip_if` binaries absent.
- [ ] AC7 NEWS.md entry; `devtools::document()` no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T2, T4
- AC5 → T3, T4
- AC6 → T5
- AC7 → T6, T7

## Tasks

- [x] T1 Thread `audio_codec` through `standardize_pipeline()` and
      `anonymize_pipeline()`, replacing the hardcoded `audio = "copy"`; add the
      formal to `standardize_video()` and `anonymize_video()` before `run`.
- [x] T2 Compile tests for both scalars: named encoder emitted, `NULL` emits
      nothing, default command byte-identical to the recorded pre-milestone
      string, bad token aborts.
- [x] T3 Add `audio_codec` to `standardize_video_batch()` and
      `anonymize_video_batch()` — batch-wide argument plus per-row column via
      `check_batch_codec_col(jobs, "audio_codec")` and `batch_codec_cell()`.
- [x] T4 Batch tests: column overrides the scalar; all-`NA` logical column
      accepted; numeric column aborts; 2+ row abort message renders.
- [ ] T5 Execution test with the MP3-in-MP4 fixture proving copy vs re-encode
      on both verbs, skipped when the binaries are absent.
- [ ] T6 Roxygen `@param` on all four; NEWS.md entry; `devtools::document()`.
- [ ] T7 Full `devtools::check()`; `spelling::update_wordlist()` if new
      technical terms appear (M17 lesson — check `00check.log` for `Status: OK`).

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: pre-implementation gate — `audio_codec` sits next to `video_codec` (codec pair adjacent, as on all six sibling verbs) rather than immediately before `hardware`; `pixel_format` therefore separates the codecs from `hardware` on these two verbs alone.
- 2026-07-26: T1 done — both pipelines reuse M35's `apply_audio_codec()` seam rather than a second `ffm_codec(audio=)` call, so NULL handling and `check_token` call attribution come for free; Layer 1 untouched (IP1).
- 2026-07-26: T1 minor reorder — roxygen `@param`/`@details` for the two scalars written here rather than deferred to T6, so no intermediate commit documents a signature it does not have; T6 keeps NEWS + `document()`.
- 2026-07-26: T1 — pipeline formals default to `audio_codec = "copy"` (as `crop_video_pipeline` does) so the batch call sites keep compiling until T3 wires the real argument.
- 2026-07-26: T1 — recorded the pre-milestone default commands for both verbs and asserted the post-change defaults are `identical()` to them (AC2 evidence gathered early, re-run at review).
- 2026-07-26: T1 — `test-anonymize-video-batch.R:279` called `anonymize_pipeline()` positionally and broke on the new sixth formal; converted to named arguments.
- 2026-07-26: T1 — `R/ffmpeg.R` CRLF integrity checked after editing (4440 CR / 4440 lines, diff 54/27), per the M35 lesson.
- 2026-07-26: T2 done — 11 compile tests in `test-audio-codec.R`; the two default-command literals are pinned in helpers so AC2 checks against a literal, not against the code under test.
- 2026-07-26: T2 — extended M35's arg-spelling test from eight verbs to ten (the batches join at T3); its `audio_codec` before `hardware` assertion already covers the gate's placement decision.
- 2026-07-26: T3 done — both batches take `audio_codec` batch-wide plus a per-row column; the guard is `check_batch_codec_col`, kept out of the neighbouring `str_cols` loop (which rejects NA) with a comment saying why, since `video_codec` there has no sentinel.
- 2026-07-26: T3 — added a front-door `check_string(audio_codec, allow_null = TRUE)` to both batch verbs, matching `crop_video_batch`; neither had one for `video_codec` (a literal default), but NULL is legal here.
- 2026-07-26: T3 — `format_for_web_batch`'s roxygen claimed `standardize_video_batch` "stream-copies audio rather than exposing a codec for it", which M39 falsifies; rewritten.
- 2026-07-26: T3 — arg-spelling test now covers all twelve verbs that carry `audio_codec`.
- 2026-07-26: T4 done — six new batch tests plus M35's two shared column-guard tests extended to the M39 verbs; the per-row token test uses two rows so a cli count message cannot pass by hiding behind a single item (M18).

## Decisions

## Review
