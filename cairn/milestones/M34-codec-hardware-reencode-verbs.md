# M34: `video_codec` + `hardware=` for the four codec-less re-encode verbs

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR01
- **Principles touched:** IP1, IP2, IP3, GP1
- **Branch/PR:** `m34-codec-hardware-reencode-verbs` · https://github.com/jmgirard/tidymedia/pull/36

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

- [x] AC1 (BC1): `crop_video`, `segment_video`, `compare_videos`,
      `picture_in_picture` and their `_batch` siblings each gain formals
      `video_codec = NULL`, `hardware = c("none", "nvenc")`, `fallback = FALSE`
      (exact D014 spellings; no `vcodec`/`codec` alias), verified by
      `formals()`-level or documented-usage evidence.
- [x] AC2 (BC2): With all-default arguments, each of the four verbs (and batch
      siblings) compiles commands **byte-identical** to pre-M34: a passing
      regression test asserts the compiled string contains no `-codec:v` token
      and matches the pre-M34 literal for at least one single-input verb
      (`crop_video`) and one multi-input verb (`compare_videos`).
- [x] AC3 (BC3): `crop_video(…, video_codec = "libx265", run = FALSE)` compiles
      a command containing `-codec:v libx265`; a non-token value (e.g.
      `"libx264 -evil"`) aborts via `check_token()`.
- [x] AC4 (BC4): Under `withr::local_options(tidymedia.nvenc_encoders =
      "h264_nvenc")`, each of the four verbs with `hardware = "nvenc"` and
      default `video_codec` compiles `-codec:v h264_nvenc`; with
      `video_codec = "libx265"` and the option set to `"hevc_nvenc"`, compiles
      `-codec:v hevc_nvenc`.
- [x] AC5 (BC5): Under an empty nvenc pool (`tidymedia.nvenc_encoders =
      character(0)`): `hardware = "nvenc"`, `fallback = FALSE` aborts;
      `fallback = TRUE` with default `video_codec` emits a message and compiles
      with **no** `-codec:v`; `fallback = TRUE` with `video_codec = "libx264"`
      emits a message and compiles `-codec:v libx264`.
- [x] AC6 (BC6): `segment_video(…, reencode = FALSE, hardware = "nvenc")` and
      `segment_video(…, reencode = FALSE, video_codec = "libx264")` each abort
      with a `cli` error; in `segment_video_batch`, a jobs table whose per-row
      `reencode` column contains `FALSE` on a row with a non-NA resolved
      `video_codec` (column or batch-wide) aborts — evidenced by passing tests
      covering both the scalar and the per-row-column path.
- [x] AC7 (BC7): `compare_videos` and `picture_in_picture` with `video_codec`
      set compile a single command containing all of `-filter_complex`, the
      `[vout]` label, `-map "[vout]"`, and `-codec:v <codec>` (compile-string
      test, no binary).
- [x] AC8 (BC8): The four `_batch` siblings accept a per-row `video_codec`
      column: a character column may contain `NA` (that row compiles no
      `-codec:v`; non-NA rows compile their own codec); an **all-NA logical**
      column is accepted as all-default; a numeric `video_codec` column aborts
      up front. `hardware`/`fallback` are honored only as formals — a
      `hardware` jobs column does not alter per-row commands.
- [x] AC9 (BC9): M34 changes to `R/ffm.R` are documentation-only or absent:
      `ffm_codec()` and the compile path (`ffm_groups`/`ffm_compile`) have no
      functional diff on the milestone branch (IP2: no new engine capability).
- [x] AC10 (BC10): No `pixel_format` argument is added to any of the four verbs
      or their batch siblings in M34.
- [x] AC11: Profile `verify`/consistency-gate clean — `devtools::test()` clean,
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
- [x] T5 — The four `_batch` siblings: `video_codec` as a per-row `pick()`
      column, NA→sentinel, all-NA-logical accepted (audio-column pattern, not
      `check_batch_string_col`), numeric column rejected up front;
      `hardware`/`fallback` stay batch-wide captured scalars.
- [x] T6 — Roxygen for all eight functions: document `video_codec` (sentinel),
      `hardware`/`fallback`, and the nvenc H.264-family assumption + non-H.264
      container caveat (RR01 R7); `devtools::document()`.
- [x] T7 — Tests (binary-free compile-level) covering AC2–AC8 via the
      `tidymedia.nvenc_encoders` option seam; execution tests behind
      `skip_if_no_nvenc()`; assert no functional `R/ffm.R` diff (AC9) and no
      `pixel_format` formal (AC10).
- [x] T8 — Run profile `verify`/consistency-gate: `devtools::test()`,
      `devtools::document()` (no diff), `devtools::check()`.

## Work log

- 2026-07-26: created by /milestone-brief RR01 ingestion; planned from RR01 (was a candidate, not a blocked milestone — see Decisions).
- 2026-07-26: question gate — new formals go before `run` (sibling parity, D014 clean break); sentinel fallback message names the container default rather than a codec.
- 2026-07-26: T1 — `resolve_hw_encoder()` NULL-sentinel branch before `codec_family()`; nvenc+sentinel→h264 family, fallback+sentinel→NULL with a container-default message; abort hint reworded to be codec-agnostic. 4 new resolver tests; `devtools::test()` clean.
- 2026-07-26: plan refinement (minor) — roxygen for each verb is written with its own task rather than batched in T6; T6 keeps the caveat-wording pass + final `document()`.
- 2026-07-26: T2 — `crop_video`/`crop_video_pipeline` gain the three formals (before `run`); pipeline `check_token()`s the user's codec up front so nvenc and software paths reject identically. New `test-video-codec.R` pins the pre-M34 literal byte-for-byte (AC2) plus AC3–AC5 for crop; `document()` + `devtools::test()` clean.
- 2026-07-26: T3 — `compare_videos`/`picture_in_picture` + pipelines gain the three formals; the sentinel/token/resolve logic extracted to one Layer-2 helper `apply_video_codec()` (crop refactored onto it) rather than duplicated four times. Codec composes cleanly with `-filter_complex … [vout]` (AC7 confirmed); both composites' pre-M34 literals pinned. `document()` + `devtools::test()` clean.
- 2026-07-26: T4 — `segment_video`/`segment_pipeline` gain the three formals; the stream-copy guard aborts in the shared pipeline (so both callers inherit it per row) when `!reencode` meets a codec or `hardware != "none"`. Note: the copy path already emits `-codec:v copy` via `ffm_copy()`, so its pre-M34 literal is pinned as-is. `document()` + `devtools::test()` clean.
- 2026-07-26: T5 — the four `_batch` siblings gain the three formals; `video_codec` reads as a per-row column via two new helpers (`check_batch_codec_col()` NA-tolerant guard + `batch_codec_cell()` NA→sentinel), `hardware`/`fallback` stay batch-wide captured scalars. `segment_video_batch` inherits the stream-copy abort per row (scalar arg + column paths both tested). `document()` + `devtools::test()` clean (1301 pass).
- 2026-07-26: T6 — docs pass: the R7 caveat (nvenc + sentinel assumes H.264; non-H.264 containers need an explicit codec) on all four scalar verbs and in the `nvenc_encoder`/`has_nvenc` block, which now lists all seven toggle-carrying verbs; NEWS.md entry added. `document()` clean; formals verified on all eight (AC1) with no `pixel_format` anywhere (AC10).
- 2026-07-26: T7 — execution tests added (two real libx264/libx265 encodes behind `skip_if_no_ffprobe()`, two nvenc encodes behind `skip_if_no_nvenc()`) plus in-package formals guards for AC1 and AC10. AC9 evidence is a command, not a test: `git diff master...HEAD -- R/ffm.R` is empty, so the engine has zero diff (not merely doc-only). `test-video-codec.R` 117 pass / 2 skip; suite 1355 pass / 4 skip.
- 2026-07-26: T8 — profile gate clean: `devtools::test()` 1355 pass / 4 skip / 0 fail, `devtools::document()` no diff, `devtools::check()` 0 errors / 0 warnings / 0 notes (one spelling NOTE on "HEVC" cleared via `spelling::update_wordlist()`). Status → review.
- 2026-07-26: review — 11/11 acceptance criteria verified with fresh evidence; `cairn_validate` exit 0; toolchain gate clean; CI green on all 9 checks. Three-lens fan-out returned 3 findings, one scored >=80 and fixed on the branch (`check_batch_codec_col()` admitted an all-NA column of any type, contradicting AC8); two scored 25/58 and are logged unactioned.

## Decisions

- 2026-07-26 (RR01): Q1 — Option B (user-facing `video_codec`) on all four verbs, no split; boundary rule "fixed recipes hide the codec, configurable transforms expose it". Q2 — `NULL` sentinel default (container-default preserved; literal `"libx264"` rejected for the WebM trap, `"auto"` rejected for `check_token` namespace collision). Q3 — defer `pixel_format`. Q4 — composites compose cleanly, consistent with IP3/D009. Q5 — abort on stream-copy conflicts, per-row in the shared pipeline. Q6 — per-row `video_codec` column, `hardware`/`fallback` batch-wide. Cross-cutting API convention promoted to D016.

## Review

**2026-07-26 — /milestone-review M34.** PR https://github.com/jmgirard/tidymedia/pull/36
(draft → ready). Branch cut from a synced `master`; `master` had not moved, so no
merge-forward was needed. Every line below is fresh evidence run in the review
session, not recalled from implementation.

### Acceptance-criteria evidence

- AC1 — `formals()` read live for all eight verbs: 8/8 carry `video_codec = NULL`,
  `hardware = c("none","nvenc")`, `fallback = FALSE`; 0/8 carry a `vcodec`/`codec`
  alias. Documented usage regenerated in `man/` for all eight.
- AC2 — stronger than the criterion asks: master was checked out into a scratch
  copy and both versions compiled the same 12 all-default invocations (four scalar
  verbs incl. vertical/audio/center variants, the stream-copy cut, and all four
  batch siblings). `diff` of the two outputs is empty — zero byte differences.
  The in-suite regression tests additionally pin the `crop_video` and
  `compare_videos` literals and assert no `-codec:v` token.
- AC3 — `crop_video(video_codec = "libx265")` compiles `-codec:v libx265`;
  `video_codec = "libx264 -evil"` aborts, under both `hardware = "none"` and
  `"nvenc"` (the token check runs before family inference).
- AC4 — under `tidymedia.nvenc_encoders = "h264_nvenc"`, all four verbs with
  `hardware = "nvenc"` and the default codec compile `-codec:v h264_nvenc`; with
  `video_codec = "libx265"` and the pool set to `"hevc_nvenc"`, `-codec:v hevc_nvenc`.
- AC5 — under an empty pool: `fallback = FALSE` aborts; `fallback = TRUE` with the
  default codec emits a message and compiles no `-codec:v`; `fallback = TRUE` with
  `video_codec = "libx264"` emits a message and compiles `-codec:v libx264`.
- AC6 — `segment_video(reencode = FALSE, hardware = "nvenc")` and
  `(reencode = FALSE, video_codec = "libx264")` both abort with the cli error. In
  `segment_video_batch`, a per-row `reencode` column containing `FALSE` aborts
  against both a batch-wide `video_codec` argument and a per-row `video_codec`
  column; the same table with the codec on the re-encoding row alone succeeds.
- AC7 — `compare_videos` and `picture_in_picture` with `video_codec = "libx265"`
  each compile ONE command containing all of `-filter_complex`, `[vout]`,
  `-map "[vout]"`, and `-codec:v libx265`. Compile-level, no binary.
- AC8 — all four batch siblings: a character `video_codec` column with `NA` gives
  that row no `-codec:v` while non-NA rows carry their own codec; an all-NA
  (logical) column is accepted as all-default; a numeric column aborts up front.
  A `hardware` jobs column leaves the compiled commands untouched, while the
  `hardware` formal changes them — confirming the batch-wide split.
- AC9 — `git diff master..HEAD -- R/ffm.R` is empty (0 lines), and `--numstat`
  over `R/ffm.R` + `R/ffm_oop.R` returns no rows. The engine has zero diff, not
  merely a documentation-only one; `ffm_codec()` and the compile path are untouched.
- AC10 — `formals()` read live: 0/8 verbs carry a `pixel_format` argument. An
  in-suite guard test locks this.
- AC11 — `devtools::test()` 1355 pass / 4 skip / 0 fail; `devtools::document()`
  leaves `man/` and `NAMESPACE` clean; `devtools::check()` 0 errors / 0 warnings
  / 0 notes.

**Driving RR (RR01) projections:** RR01 records no numeric projection — its
output is ten binding criteria and ten recommendations, all qualitative — so the
projection-vs-outcome comparison no-ops with nothing to juxtapose.

### Consistency gate

`cairn_validate` exit 0 — every CHECK PASS. One advisory: `sizing (split
tripwires)` warns that M34 has 11 acceptance criteria against a >7 tripwire.
Not actioned: AC1–AC10 are RR01's BC1–BC10 ingested verbatim (the binding-criteria
check string-compares them, so they cannot be merged or trimmed) and AC11 is the
profile gate. The milestone is still one coherent reviewable PR touching one
feature; splitting it would have split a single API addition across two PRs.

Toolchain gate (`r-package` profile): `document()` no diff · generated files
clean · `pkgdown::check_pkgdown()` "No problems found" · NEWS.md entry present
and free of milestone numbers · no new top-level files · `check()` clean.

### Independent review — three lenses + scorer

Three fresh-context reviewers, distinct evidence bases, all ref-based git in the
shared checkout.

- **[O] diff-bug (Opus)** — 3 findings. It independently re-derived AC2 by
  sourcing master's `R/ffmpeg.R` beside the branch's and diffing 16 call shapes
  (all identical), and confirmed layer separation: `R/ffm.R`, `R/ffm_oop.R`,
  `R/ffm_batch.R`, `R/utils.R` all zero-diff.
- **[S] blame-history (Sonnet)** — no findings. Cleared the reworded nvenc abort
  hint (no test pinned the old wording), the `segment_pipeline` guard against
  D008's stream-copy intent, the codec step against M07/M13/M32 history (no
  "no codec" omission was ever recorded as deliberate), and the NA-tolerant
  batch column against `check_batch_string_col`'s stricter contract.
- **[S] prior-review record (Sonnet)** — no regressions. Checked M32 F2/F3
  (batch column bypassing a scalar guard; over-strict NA rejection), M10 F1
  (bare `is.logical()` admitting NA), M31's `skip_if_no_nvenc()` trial-encode
  fix, and M33 F1 (the nvenc helper docs should list every verb using the
  toggle) — the last of which this milestone honored proactively. The GitHub
  PR-thread probe returned `[]`, so the secondary surface was skipped.

**[S] scorer (Sonnet, fresh — did not generate the findings):**

- **F2 — score 80 — ACTIONED, fixed on the branch.** `check_batch_codec_col()`
  tested `!is.character(x) && !all(is.na(x))`, so the all-NA escape hatch built
  for the logical column also admitted an all-NA column of *any* type. The
  function's own comment and AC8 both promise a numeric column aborts up front;
  `jobs$video_codec <- c(NA_real_, NA_real_)` was silently accepted instead.
  Fixed to `is.character(x) || (is.logical(x) && all(is.na(x)))`, which also
  closes the converse hole (a non-NA `c(TRUE, FALSE)` column). Three assertions
  added to `test-video-codec.R`; re-verified all five column shapes and re-ran
  `devtools::test()` (1357 pass / 4 skip) and `devtools::check()` (0/0/0).
- **F1 — score 25 — logged, not actioned.** Batch-wide `hardware = "nvenc"`
  makes any `reencode = FALSE` row abort the whole `segment_video_batch` call.
  Scorer: this is exactly what D016 mandates, the repair is actionable, and the
  only real gap is that `segment_video_batch`'s `@param video_codec` names the
  codec conflict without also naming `hardware`.
- **F3 — score 58 — logged, not actioned.** The NEWS entry files the new formals
  under "New features" without a breaking-change note, though inserting them
  before `run` shifts positional-argument meaning on all eight verbs. Scorer: a
  changelog-classification nit — every affected call fails loudly, and the
  insertion point was a deliberate D014 clean break on a pre-1.0 package.

