# M35: `audio_codec` for the four re-encode verbs — stream-copy by default

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP1
- **Branch/PR:** `m35-audio-codec-reencode-verbs`

## Goal

Give `crop_video`, `segment_video`, `compare_videos`, `picture_in_picture`
(+ `_batch` siblings) an `audio_codec` argument defaulting to `"copy"`, so they
stop silently re-encoding audio to the local FFmpeg build's container default.

## Scope

**In:** an `audio_codec` formal on the same eight verbs M34 gave `video_codec`,
placed beside it. Default `"copy"` (stream-copy, matching the norm
`standardize_video`/`anonymize_video` already document at R/ffmpeg.R:633);
a named encoder re-encodes; `NULL` emits no `-codec:a`. Batch: a per-row
`audio_codec` column, `NA` → unset, reusing `check_batch_codec_col(col=)`
(R/ffmpeg.R:2847) and `batch_codec_cell()` (R/ffmpeg.R:2866). Contradiction
guards on the stream-copy and no-audio-mapped paths. Riding along: the missing
`audio` column guard on `compare_videos_batch` (R/ffmpeg.R:3787–3800) and the
loose one on `picture_in_picture_batch` (R/ffmpeg.R:3937–3940); a doc
cross-reference that `format_for_web_batch` honors no per-row codec column.

**Out:** `separate_audio_video`'s `reencode = TRUE` path (R/ffmpeg.R:303) and
`normalize_audio`'s always-re-encoded audio (R/ffmpeg.R:1250) → a candidate row
each; both need their own arg-shape call against an existing `reencode` switch.
Audio `sample_rate`/`channels` on these four → not proposed. `pixel_format`
stays deferred (D016).

## Acceptance criteria

- [ ] AC1 All eight verbs carry a formal `audio_codec = "copy"` beside
      `video_codec` (exact D014 spelling; no `acodec`/`codec` alias), proven by
      a `formals()`-level test over all eight; `R/ffm.R` has no functional diff
      (IP1/IP3 — Layer 2 computes, Layer 1 unchanged).
- [ ] AC2 The only compiled-command change is the added `-codec:a` token:
      pinned literals for all eight assert the pre-M35 command plus
      `-codec:a copy`. `audio_codec = "aac"` compiles `-codec:a aac`; `NULL`
      compiles no `-codec:a`; a non-token value aborts via `check_token()`.
- [ ] AC3 `segment_video(reencode = FALSE)` aborts (cli, with a repair hint)
      for any `audio_codec` other than `"copy"`, enforced per row inside
      `segment_pipeline()` so `segment_video_batch` inherits it — tests cover
      the scalar arg and a mixed per-row `reencode` column.
- [ ] AC4 The composites emit `-codec:a` only when `audio` maps a stream, and
      abort when `audio = NULL` meets a named encoder. With `audio = 0` each
      compiles one command carrying `-filter_complex`, `[vout]`,
      `-map "[vout]"`, `-map 0:a`, and `-codec:a copy`.
- [ ] AC5 All four `_batch` siblings accept a per-row `audio_codec` column:
      character with `NA` cells (that row compiles no `-codec:a`), an all-`NA`
      logical column accepted, a numeric column aborted up front. Separately,
      `compare_videos_batch` gains an up-front `audio` column guard and
      `picture_in_picture_batch`'s is tightened to
      `is.numeric(x) || (is.logical(x) && all(is.na(x)))`; both boundaries
      tested.
- [ ] AC6 Execution evidence: cropping a `make_test_video()` fixture with
      default arguments yields an output whose audio stream codec, read via
      `probe_audio()`, is identical to the input's — `skip_if` the binaries are
      absent.
- [ ] AC7 `devtools::test()` and `devtools::check()` clean (0 errors,
      0 warnings; NOTEs justified), `devtools::document()` no diff; roxygen
      `@param audio_codec` on all eight plus the audio-behavior prose
      (R/ffmpeg.R:3485, 3595); a `NEWS.md` entry naming the changed default
      and its container caveat, with no milestone numbers; the
      `format_for_web_batch` cross-reference in place.

## Coverage

- AC1 → T1, T2, T3, T4, T5
- AC2 → T1, T2, T3, T4, T5
- AC3 → T3
- AC4 → T4, T5
- AC5 → T6, T7
- AC6 → T8
- AC7 → T9

## Tasks

- [x] T1 Add `apply_audio_codec()` beside `apply_video_codec()`
      (R/ffmpeg.R:1560) resolving `"copy"` / named / `NULL` onto
      `ffm_codec(audio =)`; place it above any roxygen block (M28 lesson).
- [x] T2 `crop_video` + `crop_video_pipeline` (R/ffmpeg.R:424–478): formal,
      thread T1, pinned compile tests.
- [x] T3 `segment_video` + `segment_pipeline` (R/ffmpeg.R:1639, 1733–1757):
      formal, thread on the re-encode path, per-row copy-conflict abort.
- [x] T4 `compare_videos` + pipeline (R/ffmpeg.R:3446–3472, 3523): formal,
      emit only when audio is mapped, abort on named-encoder-with-no-audio.
- [x] T5 `picture_in_picture` + pipeline (R/ffmpeg.R:3554–3585, 3638): same
      shape as T4.
- [x] T6 Remaining three `_batch` siblings (segment's landed in T3): `audio_codec` per-row column via `pick()` +
      `batch_codec_cell()`, guarded by
      `check_batch_codec_col(jobs, "audio_codec")`.
- [ ] T7 Batch `audio` column guards: add compare's missing check, tighten
      pip's to the M34 shape; test both boundaries.
- [ ] T8 Execution test: crop `make_test_video()`, `probe_audio()` input and
      output, assert the codec is unchanged.
- [ ] T9 Docs + gate: roxygen on all eight, audio-behavior prose,
      `format_for_web_batch` cross-reference, `NEWS.md` entry, `document()`,
      `test()`, `check()`.

## Work log

- 2026-07-26: created by /milestone-plan. Absorbs three candidate rows (RR01 Beyond-1, RR01 Beyond-3, the M34 review's pip guard-parity item); the plan-time audit widened the hole from the two composites to all four M34 verbs, and split two further verbs out to candidate rows. D017 records the arg shape.
- 2026-07-26: set in-progress; branch `m35-audio-codec-reencode-verbs` cut from master.
- 2026-07-26: T1 — `apply_audio_codec()` added beside `apply_video_codec()`; NULL returns the pipeline untouched, otherwise token-checked with the caller's `call` and threaded to `ffm_codec(audio =)`. Covered indirectly from T2 (internal helper, per the profile's test-doctrine). test() green: 1357 pass, 0 fail.
- 2026-07-26: T2 — `crop_video` gains `audio_codec = "copy"` after `video_codec`; new `tests/testthat/test-audio-codec.R` pins the default literal byte-for-byte (`-codec:a copy` lands between `-vf` and `-map 0`). Two pre-existing pins updated for the deliberate default change: `test-ffmpeg.R` no longer asserts filter/map adjacency, and M34's crop byte-pin narrows to its own claim (no `-codec:v`), pointing at the new file for the full literal. test() green: 1369 pass, 0 fail.
- 2026-07-26: T3 — `segment_video` + `segment_pipeline` gain `audio_codec`, applied after `ffm_copy()` so the copy path stays idempotent; the new per-row guard aborts when a stream copy meets anything but `"copy"` (NULL included, since `ffm_copy()` would overwrite it). Minor task refinement: `segment_video_batch`'s formal + per-row column landed here rather than in T6, because AC3's per-row evidence needs them; T6 now covers the remaining three siblings. M34's segment byte-pin narrowed like crop's. test() green: 1382 pass, 0 fail.
- 2026-07-26: T4+T5 — both composites gain `audio_codec`, applied only inside the `if (!is.null(audio))` branch so the default (`audio = NULL`, no track carried) still compiles no `-codec:a` and M34's composite byte-pins hold untouched. A named encoder with no audio mapped aborts; NULL stays legal there since it only ever means "emit nothing". Compile test pins the full complex shape: `-filter_complex` + `[vout]` + `-map "[vout]"` + `-map N:a` + both codecs in one command. test() green: 1403 pass, 0 fail.
- 2026-07-26: T6 — `crop_video_batch`, `compare_videos_batch`, `picture_in_picture_batch` gain the `audio_codec` formal plus the per-row column via `pick()`/`batch_codec_cell()`, guarded by `check_batch_codec_col(jobs, "audio_codec")` (M34's helper took a `col` argument already, so the all-NA-logical acceptance and the all-NA-numeric rejection come for free and are tested on both boundaries). test() green: 1417 pass, 0 fail.

## Decisions

## Review
