# M37: codec args subsume `reencode` on `separate_audio_video` (+ batch)

- **Status:** review
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

- [x] AC1: the default call compiles the same two commands as `reencode = FALSE`
      on the default branch — `-codec:a copy` and `-codec:v copy`, byte-identical.
- [x] AC2: `audio_codec = NULL, video_codec = NULL` compiles what
      `reencode = TRUE` compiled on the default branch (no `-codec` emitted).
- [x] AC3: a named encoder per stream appears in that stream's command and only
      that one — an `audio_codec` never reaches the video command, nor the reverse.
- [x] AC4: `reencode` is gone from both verbs — the scalar errors with R's
      `unused argument`, and the batch (whose `...` would otherwise swallow it
      silently) aborts naming `audio_codec` / `video_codec` as the replacement
      — and no reference to it survives under `R/`, `man/`, `vignettes/`, or
      `_pkgdown.yml` *for these two verbs* beyond that guard, while
      `segment_video`'s and `ffm_seek`'s own `reencode` are untouched.
- [x] AC5: the batch honors per-row `audio_codec` / `video_codec` columns with
      `NA` → unset, routes each column to its own reshaped stream row, and
      rejects a wrong-typed column at both boundaries (M34 lesson).
- [x] AC6: execution tests — the copy path preserves the source codec using the
      MP3-in-MP4 fixture `make_mp3_audio_video()`
      (`tests/testthat/helper-media.R:37`; an AAC fixture cannot discriminate,
      M35 lesson), and a named encoder transcodes. `skip_if` binaries absent.
- [x] AC7: `NEWS.md` records the breaking change in user-facing terms, and the
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
- [x] T4: Delete the old per-row `reencode` column guard (`R/ffmpeg.R:3455`) and
      the tests that pin it; add the arg-is-gone assertions.
- [x] T5: Public-surface sweep (M23 lesson) — grep `vignettes/`, roxygen
      `@examples`, `README.Rmd`, `_pkgdown.yml` for these verbs' `reencode` and
      update every hit; confirm the other verbs' `reencode` is left alone.
- [x] T6: Execution tests for the copy and named-encoder paths using
      `make_mp3_audio_video()`; `skip_if` binaries absent.
- [x] T7: `NEWS.md` breaking-change entry, `devtools::document()`; append the
      `DECISIONS.md` entry recording the subsumption and the D014 waiver.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: implement started on `m37-separate-av-codec-args`. Question gate: the batch's reshaped table carries a single `codec` column, present only when the jobs table supplies a codec column (mirrors today's `reencode` carry-through; reads beside the existing `stream` marker).
- 2026-07-26: T1 — `separate_stream_pipeline()` now takes a per-stream `codec` (default `"copy"`, `NULL` emits nothing), routed to the audio or video slot by `stream` via `apply_audio_codec()`/`apply_video_codec()`. Both call sites translate their still-public `reencode` at the boundary, so this task is a contract-preserving refactor pinned by the existing suite; the new behavior gets its tests at T2/T3 where it becomes publicly reachable. `devtools::test()` clean (0 failures).
- 2026-07-26: T2 — `separate_audio_video(audio_codec = "copy", video_codec = "copy")` replaces `reencode`; roxygen rewritten (+ an MP3 example). New `tests/testthat/test-separate-av-codec.R` asserts both default-branch parity cases byte-for-byte against commands captured from `master` before the swap, per-stream routing, one-stream-only unset, and the non-string/metacharacter rejections. `devtools::document()` + `devtools::test()` clean (1510 pass).
- 2026-07-26: T3 — `separate_audio_video_batch()` takes `audio_codec`/`video_codec` args plus per-row columns of the same names (`NA` = unset), guarded by `check_batch_codec_col(col =)`. The 2N reshape collapses the two input columns into one resolved `codec` column routed by `stream`, resolved via `batch_codec_cell()` in the runner; a jobs table naming no codec keeps the pre-M37 shape. Tests cover arg routing, per-row override, per-stream arg fallback, the carried-column shape, and both M34 column-type boundaries (all-NA logical accepted, all-NA numeric rejected). Minor plan refinement: T4's deletion half (the old per-row `reencode` guard and the tests pinning it) landed here, since leaving it would have been dead code on a removed argument; T4 keeps the arg-is-gone assertions. `devtools::test()` clean (1522 pass).
- 2026-07-26: T4 — arg-is-gone assertions added. Substantive amendment (user-gated, option A): the scalar verb errors with R's own `unused argument`, but the batch verb's `...` swallowed a stale `reencode` silently and stream-copied output the caller asked to re-encode, so the batch now aborts naming `audio_codec`/`video_codec` as the replacement — a diagnostic, not a `lifecycle` shim; D014's clean break stands. AC4 amended accordingly (text shown in chat before this commit). `devtools::test()` clean (1527 pass).
- 2026-07-26: T5 — public-surface sweep clean, no edits owed. `reencode` count is 0 in both verbs' `.Rd` files; the only surviving mention under `R/` for these verbs is the T4 migration guard. `vignettes/batch.Rmd:98` calls `separate_audio_video()` without naming `reencode`, so it still compiles; `_pkgdown.yml` lists names only; `README.Rmd` never mentions either verb. Out-of-scope `reencode` intact: `man/segment_video.Rd` (4 hits), `man/segment_video_batch.Rd` (7), `man/ffm_seek.Rd` (6), and `vignettes/tidymedia.Rmd`'s `ffm_seek()` example.
- 2026-07-26: T6 — four binary-gated execution tests on the MP3-in-MP4 fixture: the copy default keeps `mp3` in the audio output (a re-encode into MP4 would yield `aac`) and preserves the video codec; `audio_codec = "aac"` transcodes while the video stream stays copied; `audio_codec = NULL` reproduces the pre-M37 container-default `aac`; and a per-row `audio_codec` column drives copy vs transcode across two rows of one batch. `devtools::test()` clean (1536 pass).
- 2026-07-26: review — draft PR #39 opened; AC1-AC7 verified with fresh evidence (AC1/AC2 byte-compared against a pristine `git archive master` tree, not the tests' hardcoded reference strings); consistency gate clean.
- 2026-07-26: T7 — `NEWS.md` breaking-change entry and `DECISIONS.md` D020 appended (both shown verbatim in chat before this commit); `devtools::document()` no further diff. `R CMD check` `Status: OK` — 0 errors / 0 warnings / 0 notes, read from the check log rather than the devtools summary (M17 lesson). Status → review.

## Decisions

- D020 (`cairn/DECISIONS.md`) records the subsumption, the `"copy"`-not-sentinel choice, the D014 waiver, the batch's stale-argument guard, and the single-`codec`-column reshape.

## Review

**PR:** https://github.com/jmgirard/tidymedia/pull/39 · reviewed 2026-07-26.

### Acceptance-criteria evidence

- **AC1 — PASS.** A pristine `master` tree was extracted with `git archive master`
  and loaded separately, and its `reencode = FALSE` commands compared to the
  branch's default call: `identical()` TRUE on both the audio and the video
  command. Byte-for-byte, not by substring, and against the default branch's own
  code rather than the reference strings hardcoded in the tests.
- **AC2 — PASS.** Same pristine-`master` comparison for `audio_codec = NULL,
  video_codec = NULL` against `reencode = TRUE`: `identical()` TRUE on both
  commands; neither carries a `-codec` option.
- **AC3 — PASS.** `audio_codec = "aac", video_codec = "libx264"` puts
  `-codec:a aac` in the audio command and `-codec:v libx264` in the video one;
  the audio command is free of `libx264` and the video command free of `aac`,
  so neither stream's choice reaches the other.
- **AC4 — PASS.** `reencode` count is 0 in both verbs' `.Rd` files, 0 in
  `_pkgdown.yml`, and 0 in the vignettes for these verbs. The scalar call errors
  `unused argument (reencode = TRUE)`; the batch call errors
  "`reencode` was removed from `separate_audio_video_batch()`". Formals are
  `infile, audiofile, videofile, audio_codec, video_codec, run` and
  `jobs, audio_codec, video_codec, run, parallel, ...`. The only surviving `R/`
  mentions for these verbs are the migration guard and its comment
  (`R/ffmpeg.R:3543-3553`), which the amended criterion permits. Out-of-scope
  `reencode` intact: `man/ffm_seek.Rd` 6 hits, `man/segment_video_batch.Rd` 7,
  `man/segment_video.Rd` 4.

- **AC5 — PASS.** A two-row table with `audio_codec = c("aac", NA)` and
  `video_codec = c(NA, "libx264")` resolves to a `codec` column of
  `aac | NA | NA | libx264` against a `stream` column of
  `audio | video | audio | video`: each column reaches its own stream's row,
  `NA` emits no `-codec`, and the named encoders land on rows 1 and 4. Both
  type boundaries hold — a numeric column aborts naming the column
  ("The audio_codec column of `jobs` must be character"), an all-NA numeric
  column also aborts, while the all-NA *logical* column R produces is accepted
  as "unset" (M34 lesson).
- **AC6 — PASS.** `devtools::test(filter = "separate")` 83 pass / 0 fail /
  0 skip with the binaries present. The MP3-in-MP4 fixture discriminates: the
  copy default writes `mp3` audio (an MP4 re-encode would yield `aac`) and
  preserves the video codec; `audio_codec = "aac"` writes `aac` while the video
  stays copied; `audio_codec = NULL` yields the container default `aac`; a
  per-row column drives copy vs transcode across two rows of one batch. All
  four `skip_if` on the binaries.
- **AC7 — PASS.** `NEWS.md` carries the breaking-change entry in user-facing
  terms with no milestone numbers. Profile `verify` slot clean:
  `devtools::test()` 1536 pass / 0 fail / 4 skip (nvenc absent);
  `devtools::document()` no diff; `devtools::check()` `Status: OK` —
  0 errors / 0 warnings / 0 notes, read from the check log rather than the
  devtools summary (M17 lesson).

### Independent fresh-context review

### Consistency gate

- `cairn_validate` — exit 0, all 16 checks PASS, 7 advisories OK.
- `cairn_impact` — skipped, no `DESIGN.md` principle changed.
- Profile `consistency-gate` (r-package): `devtools::document()` no diff;
  `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` carries the
  user-visible entry with no milestone numbers; no new top-level files.
