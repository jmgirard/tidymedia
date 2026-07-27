# M37: codec args subsume `reencode` on `separate_audio_video` (+ batch)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** `m37-separate-av-codec-args`

## Goal

Replace `separate_audio_video()`'s `reencode` switch with per-stream
`audio_codec` / `video_codec` arguments defaulting to `"copy"`, so each output
stream's encoder can be named instead of left to the container default.

## Scope

**In:** `audio_codec = "copy"` and `video_codec = "copy"` on
`separate_audio_video()` (`R/ffmpeg.R:339`) and `separate_audio_video_batch()`
(`R/ffmpeg.R:3434`): `"copy"` reproduces today's `reencode = FALSE`, `NULL`
emits no `-codec` (today's `reencode = TRUE`), a named encoder pins it.
`reencode` is **removed** from both verbs and its per-row column replaced by
per-row `audio_codec` / `video_codec` columns (`NA` → unset) — a clean break
under D014's pre-0.2.0 policy with no `lifecycle` shim, at the user's explicit
waiver of the deprecation cycle (2026-07-26 plan gate; RB tripwire:
irreversible-api, settled there, D-entry appended at implementation).
`separate_stream_pipeline()` (`R/ffmpeg.R:299`) takes a per-stream codec instead
of `reencode`. Public-surface sweep per the M23 lesson, plus NEWS.

**Out:** `segment_video(reencode =)` and `ffm_seek(reencode =)` — different
verbs, governed by D016/D017/D018; untouched, and a criterion below pins that.
`normalize_audio`'s `audio_codec` → M36. A `hardware =` nvenc arg on this verb's
video re-encode path → a new candidate row (it needs D016's fixed-recipe vs
configurable-transform call applied to a demux verb first).

## Acceptance criteria

- [ ] AC1: the default call compiles the same two commands as `reencode = FALSE`
      on the default branch — `-codec:a copy` and `-codec:v copy`, byte-identical.
- [ ] AC2: `audio_codec = NULL, video_codec = NULL` compiles what
      `reencode = TRUE` compiled on the default branch (no `-codec` emitted).
- [ ] AC3: a named encoder per stream appears in that stream's command and only
      that one — an `audio_codec` never reaches the video command, nor the reverse.
- [ ] AC4: `reencode` is gone from both verbs (an `unused argument` error), and
      no reference to it survives under `R/`, `man/`, `vignettes/`, or
      `_pkgdown.yml` *for these two verbs*, while `segment_video`'s and
      `ffm_seek`'s own `reencode` are untouched.
- [ ] AC5: the batch honors per-row `audio_codec` / `video_codec` columns with
      `NA` → unset, routes each column to its own reshaped stream row, and
      rejects a wrong-typed column at both boundaries (M34 lesson).
- [ ] AC6: execution tests — the copy path preserves the source codec using the
      MP3-in-MP4 fixture `make_mp3_audio_video()`
      (`tests/testthat/helper-media.R:37`; an AAC fixture cannot discriminate,
      M35 lesson), and a named encoder transcodes. `skip_if` binaries absent.
- [ ] AC7: `NEWS.md` records the breaking change in user-facing terms, and the
      profile `verify` slot is clean — `devtools::test()` passes,
      `devtools::document()` no diff, `devtools::check()` 0 errors / 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T4, T5
- AC5 → T3
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: Rework `separate_stream_pipeline()` (`R/ffmpeg.R:299`) to take a
      per-stream codec (default `"copy"`, `NULL` = emit nothing), applying it to
      the audio or video slot by `stream`. Tests first. Edit `R/ffmpeg.R` as
      bytes — the repo's only CRLF file (M35 lesson).
- [x] T2: Swap `reencode` for `audio_codec` / `video_codec` on
      `separate_audio_video()` (`R/ffmpeg.R:339`) + roxygen; assert both
      default-branch parity cases (copy default, NULL/NULL).
- [x] T3: Swap the arg and per-row column on `separate_audio_video_batch()`
      (`R/ffmpeg.R:3434`), routing each codec column to its own stream row in the
      2N reshape (`R/ffmpeg.R:3467`); reuse `check_batch_codec_col(col =)` and
      `batch_codec_cell()`; test both column-type boundaries.
- [ ] T4: Delete the old per-row `reencode` column guard (`R/ffmpeg.R:3455`) and
      the tests that pin it; add the arg-is-gone assertions.
- [ ] T5: Public-surface sweep (M23 lesson) — grep `vignettes/`, roxygen
      `@examples`, `README.Rmd`, `_pkgdown.yml` for these verbs' `reencode` and
      update every hit; confirm the other verbs' `reencode` is left alone.
- [ ] T6: Execution tests for the copy and named-encoder paths using
      `make_mp3_audio_video()`; `skip_if` binaries absent.
- [ ] T7: `NEWS.md` breaking-change entry, `devtools::document()`; append the
      `DECISIONS.md` entry recording the subsumption and the D014 waiver.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: implement started on `m37-separate-av-codec-args`. Question gate: the batch's reshaped table carries a single `codec` column, present only when the jobs table supplies a codec column (mirrors today's `reencode` carry-through; reads beside the existing `stream` marker).
- 2026-07-26: T1 — `separate_stream_pipeline()` now takes a per-stream `codec` (default `"copy"`, `NULL` emits nothing), routed to the audio or video slot by `stream` via `apply_audio_codec()`/`apply_video_codec()`. Both call sites translate their still-public `reencode` at the boundary, so this task is a contract-preserving refactor pinned by the existing suite; the new behavior gets its tests at T2/T3 where it becomes publicly reachable. `devtools::test()` clean (0 failures).
- 2026-07-26: T2 — `separate_audio_video(audio_codec = "copy", video_codec = "copy")` replaces `reencode`; roxygen rewritten (+ an MP3 example). New `tests/testthat/test-separate-av-codec.R` asserts both default-branch parity cases byte-for-byte against commands captured from `master` before the swap, per-stream routing, one-stream-only unset, and the non-string/metacharacter rejections. `devtools::document()` + `devtools::test()` clean (1510 pass).
- 2026-07-26: T3 — `separate_audio_video_batch()` takes `audio_codec`/`video_codec` args plus per-row columns of the same names (`NA` = unset), guarded by `check_batch_codec_col(col =)`. The 2N reshape collapses the two input columns into one resolved `codec` column routed by `stream`, resolved via `batch_codec_cell()` in the runner; a jobs table naming no codec keeps the pre-M37 shape. Tests cover arg routing, per-row override, per-stream arg fallback, the carried-column shape, and both M34 column-type boundaries (all-NA logical accepted, all-NA numeric rejected). Minor plan refinement: T4's deletion half (the old per-row `reencode` guard and the tests pinning it) landed here, since leaving it would have been dead code on a removed argument; T4 keeps the arg-is-gone assertions. `devtools::test()` clean (1522 pass).

## Decisions

## Review
