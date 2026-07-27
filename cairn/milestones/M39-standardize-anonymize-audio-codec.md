# M39: `audio_codec` for `standardize_video` and `anonymize_video` (+ batch)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** `m39-standardize-anonymize-audio-codec` / https://github.com/jmgirard/tidymedia/pull/41

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

- [x] AC1 All four verbs accept `audio_codec`, default `"copy"`, formal before
      `run`; a test compiles each with a named encoder and asserts
      `-codec:a <name>` appears.
- [x] AC2 Default output is unchanged: a test compiles a default call of each
      of the four verbs and compares byte-for-byte against the command the
      pre-milestone code produced.
- [x] AC3 `audio_codec = NULL` emits no `-codec:a` on all four; tested.
- [x] AC4 An invalid codec token aborts via `check_token()` — at the front door
      on the scalars, per row on the batches; the batch case tested with 2+
      rows (M18 lesson).
- [x] AC5 A per-row `audio_codec` column overrides the scalar argument; an
      all-`NA` (logical) column is accepted and resolves to the sentinel; a
      numeric column aborts naming the column (M34 lesson). Both boundaries
      tested.
- [x] AC6 An execution test proves `"copy"` stream-copies and a named encoder
      re-encodes, using a source codec that is not the container default
      (M35 lesson — the MP3-in-MP4 fixture), `skip_if` binaries absent.
- [x] AC7 NEWS.md entry; `devtools::document()` no diff; `devtools::test()` and
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
- [x] T5 Execution test with the MP3-in-MP4 fixture proving copy vs re-encode
      on both verbs, skipped when the binaries are absent.
- [x] T6 Roxygen `@param` on all four; NEWS.md entry; `devtools::document()`.
- [x] T7 Full `devtools::check()`; `spelling::update_wordlist()` if new
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
- 2026-07-26: T5 done — three execution tests on the MP3-in-MP4 fixture prove copy keeps `mp3`, a named encoder yields `aac`, and NULL hands the choice back to the container; confirmed running (not skipped) against local ffmpeg/ffprobe.
- 2026-07-26: T6 done — NEWS entry under New features; `document()` regenerated five `.Rd` files and is idempotent on a second run. Vignettes, README.Rmd and `_pkgdown.yml` need no change (no new exports; the one vignette `audio_codec` mention is about the composite verbs).
- 2026-07-26: T7 done — first `check()` hit the M17 trap exactly (devtools said 0 notes, `00check.log` said `Status: 1 NOTE`, a spelling hit on "hardcoded" in NEWS); reworded rather than growing `inst/WORDLIST` for one occurrence. Re-run is `Status: OK`; `pkgdown::check_pkgdown()` clean.
- 2026-07-26: all tasks done, `check()` and `check_pkgdown()` clean, `R/ffmpeg.R` CRLF intact (4467/4467, diff 110 lines) — status to review.
- 2026-07-26: review — draft PR #41 opened; all seven criteria verified with fresh evidence; consistency gate clean; CI green on all nine checks; IP1 confirmed (Layer 1 zero diff).
- 2026-07-26: review — blame-history and prior-PR-comments lenses both returned clean; diff-bug lens still running, triage and merge gate pending (checkpoint, not final).
- 2026-07-26: review — diff-bug lens returned 4 findings; scorer gave 88/87/90/25. The three at or above 80 fixed on the branch, the 25 logged unactioned; gate re-run clean after the fixes.

## Decisions

## Review

**Verified 2026-07-26 on `m39-standardize-anonymize-audio-codec` @ 327d794, PR #41.** Evidence gathered by command, never recall.

- AC1 — `formals()` on all four verbs: `audio_codec` present, default `"copy"`, and its index precedes `hardware` on each (7<9, 6<8, 6<8, 4<6). All four compile `-codec:a aac` when given a named encoder.
- AC2 — master's own code was extracted with `git archive master` into a scratch tree and loaded, so the four default commands were compiled from *both* revisions on an identical input path and `diff`ed: zero bytes differ. Stronger than the planned literal comparison, which the tests also carry.
- AC3 — `audio_codec = NULL` on all four: no `-codec:a` token, `-codec:v libx264` still emitted.
- AC4 — `"aac -evil"` aborts on both scalars with `` `audio_codec` must be a single clean token ``; on both batches with a 2-row table it aborts at index 2.
- AC5 — column overrides the batch-wide argument (flac/aac, no `copy` survives); all-NA logical column accepted and resolves to unset; numeric and all-NA-numeric columns both abort naming `audio_codec`, on both verbs.
- AC6 — the three execution tests ran against real ffmpeg/ffprobe (`skipped=FALSE`, 4+3+2 passing expectations): copy keeps `mp3`, `"aac"` yields `aac`, `NULL` yields `aac` from the container.
- AC7 — `devtools::check()` → `Status: OK`, 0 errors / 0 warnings / 0 notes (read from the check log, not devtools' masked summary). NEWS entry present; `document()` leaves no diff.

**Independent review — three lenses, then a scorer.**

- **[S] blame-history:** clean. Traced the `audio = "copy"` literal in both pipelines to `a33f2cb4` (2026-07-12), the fix M12's review made after finding a bare re-encode was transcoding audio. M39 makes that guarantee an overridable default rather than weakening it — the same move D017 made for the four sibling verbs — and the default stays byte-identical.
- **[S] prior-PR-comments:** clean, no regressions. The GitHub inline-comment probe returned `[]`, so archived `## Review` sections were the evidence (as M91 measured for this repo). Specifically cleared M34-F2 (the all-NA column guard is reused, not reimplemented), M38-F3/M35 (the execution tests use the MP3-in-MP4 fixture), M38-F1/F2 (no new `cli_abort` hint text), M35-F2 and M34-F1 (no doc residue), M31-F1 (no new abort call sites).
- **[O] diff-bug:** 4 findings. It independently reproduced the byte-identity claim across 8 argument combinations × 2 verbs, both nvenc paths, 3 batch shapes.

**Findings, scored by a fresh [S] scorer that did not generate them.**

- **F1 (88) — fixed.** Both batch verbs' `@param jobs` still enumerated the honoured knob columns without `audio_codec` and closed with "Any other columns are ignored", which M39 made false: a user adding an `audio_codec` column believing it inert would silently change every row's encode. `normalize_audio_batch` and `extract_audio_batch` already spell the column out. Both enumerations rewritten, including the `NA`-means-unset meaning.
- **F2 (87) — fixed.** `standardize_pipeline()` called `apply_audio_codec()` without `call =`, so a bad token reported `Error in standardize_pipeline(...)` — an internal helper — while `anonymize_pipeline()` and `crop_video_pipeline()` name the verb the user called. The tests asserted only message text and condition class, so they could not see it. Added a `call = rlang::caller_env()` formal and threaded it; `conditionCall()` now reads `standardize_video(...)`. This also removes any ambiguity in AC4's "at the front door on the scalars".
- **F3 (90) — fixed.** The batch-wide `audio_codec` *argument* (as against the per-row column) was never tested with a non-default value on either verb; the existing default test would pass even if the argument never reached the pipeline, since the pipeline defaults to `"copy"` too. Proven by mutation in a scratch copy: hardcoding the fan-out to `"copy"` left the suite green, while the same mutation on `segment_video_batch` (covered by M35) went red. Added a test naming a non-default codec on both verbs plus the `NULL` case; re-running the mutation now fails on that test.
- **F4 (25) — logged, not actioned.** Inserting the formal mid-signature changes what positional callers bind: `standardize_video(f, o, 1280, 720, 30, "libx264", "yuv420p")` now binds `"yuv420p"` to `audio_codec`, compiling `-codec:a yuv420p -pix_fmt yuv420p`. Real and reproduced, but the placement was a deliberate gate decision under D014's clean break, it fails loudly at FFmpeg rather than silently, and the reviewer framed it as a NEWS-wording point only. Surfaced here rather than fixed.

**Consistency gate.** `cairn_validate` exit 0, all checks pass. Toolchain slot: `document()` no-diff, `pkgdown::check_pkgdown()` clean, NEWS entry present with no milestone numbers in user-facing text, no new top-level files, README untouched. CI green on all nine checks (macOS, Ubuntu release/devel/oldrel-1, Windows, pkgdown, coverage). Re-run after the F1/F2/F3 fixes: `check()` `Status: OK`, `pkgdown` clean, `cairn_validate` exit 0, suite green, AC2 byte-identity re-confirmed against master.
