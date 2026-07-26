# M34: `video_codec` + `hardware=` for the four codec-less re-encode verbs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR01
- **Principles touched:** IP1, IP2, IP3, GP1
- **Branch/PR:** `m34-codec-hardware-reencode-verbs`

## Goal

Give `crop_video`, `segment_video`, `compare_videos`, `picture_in_picture` (and
their `_batch` siblings) a user-facing `video_codec` argument plus the M31
`hardware=` nvenc toggle, without changing any default output.

## Scope

**In:** Per RR01 — add `video_codec = NULL` (sentinel: emit no `-codec:v`,
preserving today's container-default behavior), `hardware = c("none","nvenc")`,
`fallback = FALSE` to the four verbs and batch siblings; thread the resolved
codec into `ffm_codec(video=)` only when non-NULL; extend `resolve_hw_encoder()`
with a `NULL` branch ordered before `codec_family()`; per-row abort on
`segment_video` stream-copy conflicts; `video_codec` as a per-row batch column
(NA→sentinel), `hardware`/`fallback` batch-wide. Reuses D-M31 machinery (IP1/IP2:
no new engine capability).

**Out:** `pixel_format` on these verbs (RR01 Q3 — purely additive later, no
demonstrated need). Composites' carried-audio re-encode default and the
`format_for_web_batch` doc cross-ref → ROADMAP candidates (RR01 Beyond-1/-3).
`anonymize_video` hardware → M33.

## Acceptance criteria

<!-- Driving RR: RR01 — BC1–BC10 ingested verbatim (binding-criteria check
     string-compares, whitespace-normalized). AC11 is the profile verify gate. -->

- [ ] AC1 (BC1): `crop_video`, `segment_video`, `compare_videos`,
      `picture_in_picture` and their `_batch` siblings each gain formals
      `video_codec = NULL`, `hardware = c("none", "nvenc")`, `fallback = FALSE`
      (exact D014 spellings; no `vcodec`/`codec` alias), verified by
      `formals()`-level or documented-usage evidence.
- [ ] AC2 (BC2): With all-default arguments, each of the four verbs (and batch
      siblings) compiles commands **byte-identical** to pre-M34: a passing
      regression test asserts the compiled string contains no `-codec:v` token
      and matches the pre-M34 literal for at least one single-input verb
      (`crop_video`) and one multi-input verb (`compare_videos`).
- [ ] AC3 (BC3): `crop_video(…, video_codec = "libx265", run = FALSE)` compiles
      a command containing `-codec:v libx265`; a non-token value (e.g.
      `"libx264 -evil"`) aborts via `check_token()`.
- [ ] AC4 (BC4): Under `withr::local_options(tidymedia.nvenc_encoders =
      "h264_nvenc")`, each of the four verbs with `hardware = "nvenc"` and
      default `video_codec` compiles `-codec:v h264_nvenc`; with
      `video_codec = "libx265"` and the option set to `"hevc_nvenc"`, compiles
      `-codec:v hevc_nvenc`.
- [ ] AC5 (BC5): Under an empty nvenc pool (`tidymedia.nvenc_encoders =
      character(0)`): `hardware = "nvenc"`, `fallback = FALSE` aborts;
      `fallback = TRUE` with default `video_codec` emits a message and compiles
      with **no** `-codec:v`; `fallback = TRUE` with `video_codec = "libx264"`
      emits a message and compiles `-codec:v libx264`.
- [ ] AC6 (BC6): `segment_video(…, reencode = FALSE, hardware = "nvenc")` and
      `segment_video(…, reencode = FALSE, video_codec = "libx264")` each abort
      with a `cli` error; in `segment_video_batch`, a jobs table whose per-row
      `reencode` column contains `FALSE` on a row with a non-NA resolved
      `video_codec` (column or batch-wide) aborts — evidenced by passing tests
      covering both the scalar and the per-row-column path.
- [ ] AC7 (BC7): `compare_videos` and `picture_in_picture` with `video_codec`
      set compile a single command containing all of `-filter_complex`, the
      `[vout]` label, `-map "[vout]"`, and `-codec:v <codec>` (compile-string
      test, no binary).
- [ ] AC8 (BC8): The four `_batch` siblings accept a per-row `video_codec`
      column: a character column may contain `NA` (that row compiles no
      `-codec:v`; non-NA rows compile their own codec); an **all-NA logical**
      column is accepted as all-default; a numeric `video_codec` column aborts
      up front. `hardware`/`fallback` are honored only as formals — a
      `hardware` jobs column does not alter per-row commands.
- [ ] AC9 (BC9): M34 changes to `R/ffm.R` are documentation-only or absent:
      `ffm_codec()` and the compile path (`ffm_groups`/`ffm_compile`) have no
      functional diff on the milestone branch (IP2: no new engine capability).
- [ ] AC10 (BC10): No `pixel_format` argument is added to any of the four verbs
      or their batch siblings in M34.
- [ ] AC11: Profile `verify`/consistency-gate clean — `devtools::test()` clean,
      `devtools::document()` no diff (new args documented on all eight
      functions), `devtools::check()` clean (0 errors/0 warnings).

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T1, T2, T3, T7
- AC3 → T2, T7
- AC4 → T1, T2, T3, T7
- AC5 → T1, T7
- AC6 → T4, T5, T7
- AC7 → T3, T7
- AC8 → T5, T7
- AC9 → T7, T8
- AC10 → T6, T7
- AC11 → T6, T8

## Tasks

- [x] T1 — Extend `resolve_hw_encoder()` (R/ffmpeg.R:1432) with an explicit
      `is.null(video_codec)` branch **before** `codec_family()` (which crashes
      on NULL, R/ffmpeg.R:1411): `hardware="none"`→`NULL`; `nvenc`+`NULL`→h264
      family; nvenc-unavailable+`fallback`+`NULL`→`NULL` with the existing
      message. One seam, no second resolver (D-M31).
- [x] T2 — `crop_video` + `crop_video_pipeline` (R/ffmpeg.R:451, :424): add the
      three formals; call `ffm_codec(video = resolve_hw_encoder(...))` only when
      the resolved codec is non-NULL (default path emits no `-codec:v`).
- [x] T3 — `compare_videos`/`picture_in_picture` + pipelines (R/ffmpeg.R:3269,
      :3355): same additions; confirm the codec composes with the
      `-filter_complex … [vout]` path (RR01 Q4).
- [x] T4 — `segment_video` + `segment_pipeline` (R/ffmpeg.R:1508, :1596): add
      formals; per-row guard in the shared pipeline aborting when
      `!reencode && (!is.null(video_codec) || hardware != "none")`, with a
      repair hint.
- [ ] T5 — The four `_batch` siblings: `video_codec` as a per-row `pick()`
      column, NA→sentinel, all-NA-logical accepted (audio-column pattern, not
      `check_batch_string_col`), numeric column rejected up front;
      `hardware`/`fallback` stay batch-wide captured scalars.
- [ ] T6 — Roxygen for all eight functions: document `video_codec` (sentinel),
      `hardware`/`fallback`, and the nvenc H.264-family assumption + non-H.264
      container caveat (RR01 R7); `devtools::document()`.
- [ ] T7 — Tests (binary-free compile-level) covering AC2–AC8 via the
      `tidymedia.nvenc_encoders` option seam; execution tests behind
      `skip_if_no_nvenc()`; assert no functional `R/ffm.R` diff (AC9) and no
      `pixel_format` formal (AC10).
- [ ] T8 — Run profile `verify`/consistency-gate: `devtools::test()`,
      `devtools::document()` (no diff), `devtools::check()`.

## Work log

- 2026-07-26: created by /milestone-brief RR01 ingestion; planned from RR01 (was a candidate, not a blocked milestone — see Decisions).
- 2026-07-26: question gate — new formals go before `run` (sibling parity, D014 clean break); sentinel fallback message names the container default rather than a codec.
- 2026-07-26: T1 — `resolve_hw_encoder()` NULL-sentinel branch before `codec_family()`; nvenc+sentinel→h264 family, fallback+sentinel→NULL with a container-default message; abort hint reworded to be codec-agnostic. 4 new resolver tests; `devtools::test()` clean.
- 2026-07-26: plan refinement (minor) — roxygen for each verb is written with its own task rather than batched in T6; T6 keeps the caveat-wording pass + final `document()`.
- 2026-07-26: T2 — `crop_video`/`crop_video_pipeline` gain the three formals (before `run`); pipeline `check_token()`s the user's codec up front so nvenc and software paths reject identically. New `test-video-codec.R` pins the pre-M34 literal byte-for-byte (AC2) plus AC3–AC5 for crop; `document()` + `devtools::test()` clean.
- 2026-07-26: T3 — `compare_videos`/`picture_in_picture` + pipelines gain the three formals; the sentinel/token/resolve logic extracted to one Layer-2 helper `apply_video_codec()` (crop refactored onto it) rather than duplicated four times. Codec composes cleanly with `-filter_complex … [vout]` (AC7 confirmed); both composites' pre-M34 literals pinned. `document()` + `devtools::test()` clean.
- 2026-07-26: T4 — `segment_video`/`segment_pipeline` gain the three formals; the stream-copy guard aborts in the shared pipeline (so both callers inherit it per row) when `!reencode` meets a codec or `hardware != "none"`. Note: the copy path already emits `-codec:v copy` via `ffm_copy()`, so its pre-M34 literal is pinned as-is. `document()` + `devtools::test()` clean.

## Decisions

- 2026-07-26 (RR01): Q1 — Option B (user-facing `video_codec`) on all four verbs, no split; boundary rule "fixed recipes hide the codec, configurable transforms expose it". Q2 — `NULL` sentinel default (container-default preserved; literal `"libx264"` rejected for the WebM trap, `"auto"` rejected for `check_token` namespace collision). Q3 — defer `pixel_format`. Q4 — composites compose cleanly, consistent with IP3/D009. Q5 — abort on stream-copy conflicts, per-row in the shared pipeline. Q6 — per-row `video_codec` column, `hardware`/`fallback` batch-wide. Cross-cutting API convention promoted to D016.

## Review
