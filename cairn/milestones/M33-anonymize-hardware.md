# M33: Wire `hardware=` nvenc into `anonymize_video`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m33-anonymize-hardware · https://github.com/jmgirard/tidymedia/pull/35

## Goal

Give `anonymize_video()` and `anonymize_video_batch()` the same opt-in
`hardware = "nvenc"` GPU-encoding toggle M31 shipped on `standardize_video`.

## Scope

**In:** Add `hardware = c("none", "nvenc")` + `fallback = FALSE` to
`anonymize_video()` (R/ffmpeg.R:759) and, batch-wide, to
`anonymize_video_batch()` (R/ffmpeg.R:959), reusing the M31 machinery
(`resolve_hw_encoder`/`codec_family`/`has_nvenc`). The verb already exposes a
user-controlled `video_codec` feeding `ffm_codec`, so this is a direct mirror
of the `standardize_video` change — one `resolve_hw_encoder()` line in
`anonymize_pipeline()` before `ffm_codec()`, Layer 1 untouched (IP1).

**Out:** The four codec-less re-encode verbs (`crop_video`, `segment_video`,
`compare_videos`, `picture_in_picture`) → M34; they need a new `video_codec`
arg first and its API shape is under a Fable review brief. New hardware
backends beyond nvenc, GPU decode, and the video-quality/rate-control knob stay
ROADMAP candidates (M31 Out).

## Acceptance criteria

- [x] AC1 — `anonymize_video()` and `anonymize_video_batch()` accept
      `hardware = c("none","nvenc")` and `fallback = FALSE`; with the default
      `hardware="none"` each compiles a command byte-identical to the pre-M33
      verb (no behavior change), asserted by a compile-level test.
- [x] AC2 — `hardware="nvenc"` rewrites the video encoder to the nvenc form of
      the `video_codec` family via `resolve_hw_encoder()`; unavailable nvenc
      aborts by default and re-encodes software with a message under
      `fallback=TRUE` (mirrors M31).
- [x] AC3 — a `video_codec` outside the h264/hevc/av1 families combined with
      `hardware="nvenc"` aborts via `codec_family()` (reused, not reimplemented).
- [x] AC4 — `anonymize_video_batch()` threads `hardware`/`fallback` batch-wide
      (captured scalars, **not** per-row job columns), documented as such,
      matching `standardize_video_batch`.
- [x] AC5 — tests cover AC1–AC4 binary-free via the `tidymedia.nvenc_encoders`
      option seam; any execution test guards with a runtime `skip_if_no_nvenc()`
      probe (M31 lesson: CI lists `h264_nvenc` without a GPU). `devtools::test()`
      clean.
- [x] AC6 — profile `verify` clean: `devtools::document()` produces no diff, new
      args documented in roxygen on both verbs (`@seealso has_nvenc()`), no new
      exports so `_pkgdown.yml` is unaffected.

## Coverage

- AC1 → T1, T2, T4
- AC2 → T1, T4
- AC3 → T1, T4
- AC4 → T2, T4
- AC5 → T4
- AC6 → T3, T5

## Tasks

- [x] T1 — Add `hardware`/`fallback` to `anonymize_video()` (R/ffmpeg.R:759):
      signature after `pixel_format`, `hardware <- rlang::arg_match(hardware)`,
      pass through. In `anonymize_pipeline()` (R/ffmpeg.R:783) add
      `hardware="none", fallback=FALSE` params and insert
      `video_codec <- resolve_hw_encoder(video_codec, hardware, fallback)`
      before `ffm_codec()`; keep the existing `check_token(video_codec)`.
- [x] T2 — Add batch-wide `hardware`/`fallback` to `anonymize_video_batch()`
      (R/ffmpeg.R:959), threaded to `anonymize_pipeline()` per row as captured
      scalars (no `pick()` over `...`), mirroring `standardize_video_batch`.
- [x] T3 — Roxygen: document `hardware`/`fallback` on both verbs (reuse M31
      wording from `standardize_video`, R/ffmpeg.R:626-639); `devtools::document()`.
- [x] T4 — Tests (tests/testthat): default no-op byte-identity (AC1), nvenc
      resolution via option seam (AC2), family rejection (AC3), batch-wide
      threading (AC4); execution tests behind `skip_if_no_nvenc()`.
- [x] T5 — Run profile `verify`: `devtools::test()` clean, `devtools::document()`
      no diff, `devtools::check()` if anything structural was touched.

## Work log

- 2026-07-26: created by /milestone-plan; split from the M31-follow-on candidate (sibling M34 covers the four codec-less verbs, API shape under a Fable RB).
- 2026-07-26: T1–T4 — added `hardware`/`fallback` to `anonymize_video` (+pipeline, +batch, batch-wide) mirroring M31; roxygen + `document()`; nvenc tests in test-nvenc.R. `devtools::test()` clean (0 fail, 2 GPU skips, 1233 pass).
- 2026-07-26: T5 — `devtools::check()` clean (0E/0W/0N); `document()` no diff. All tasks done → status `review`.

## Decisions

## Review

Reviewed 2026-07-26 on `m33-anonymize-hardware` (PR #35). Evidence fresh, by
command, this session.

**Acceptance-criterion evidence**

- AC1 — `names(formals())` shows `hardware`, `fallback` on both verbs (scalar
  after `pixel_format`, batch after `pixel_format`). Default compile of
  `anonymize_video(f, "out.mp4", regions, run = FALSE)` emits
  `-codec:v libx264 -codec:a copy -pix_fmt yuv420p`, no nvenc token. The
  diff-bug reviewer independently confirmed byte-identity by compiling master
  and HEAD in isolated copies and diffing (default + non-default
  color/codec/pixfmt, scalar and batch): identical.
- AC2 — with `tidymedia.nvenc_encoders = "h264_nvenc"`, compile emits
  `-codec:v h264_nvenc`; unavailable pool aborts "not available";
  `fallback = TRUE` messages "falling back" and compiles `-codec:v libx264`
  (tests in test-nvenc.R, all passing).
- AC3 — `video_codec = "prores"` + `hardware = "nvenc"` aborts "No nvenc
  encoder" from the reused `codec_family()`; `libx265` maps to `hevc_nvenc`.
- AC4 — batch applies nvenc to every row; a `hardware` column in `jobs` is
  ignored (batch-wide), asserted by a dedicated test. Reviewers confirmed
  `hardware`/`fallback` are captured scalars outside the `pick()`/dots path.
- AC5 — `devtools::test()`: 0 failures, 0 warnings, 1233 passing, 2 skipped
  (both GPU execution tests, `skip_if_no_nvenc()` runtime probe, no nvenc on
  this host). Compile tests are binary-free via the option seam.
- AC6 — `devtools::document()` produces no `man/`/`NAMESPACE` diff;
  `devtools::check()` 0 errors / 0 warnings / 0 notes; new args documented on
  both verbs with `@seealso has_nvenc()`; no new exports (`_pkgdown.yml`
  unaffected, `pkgdown::check_pkgdown()` clean).

**Consistency gate** — `cairn_validate` exit 0 (its one advisory, sizing, is on
M34, not this milestone). Profile `consistency-gate`: `document()` no diff,
generated files untouched by hand, `check_pkgdown()` clean, `check()` clean,
no new top-level files. CI on PR #35: all 6 jobs pass. No principle changed
(IP1 worked under, not modified) → `cairn_impact` skipped.

**Independent review** — three fresh-context lenses. Blame-history (Sonnet):
no findings; confirmed the M12 even-dimension guard, region validation, and
audio stream-copy keep their order, and that the M28 roxygen-placement hazard
and M32 per-row-column lesson do not apply. Prior-review (Sonnet): no
regressions; the M31 CI trap is not repeated (the shared runtime-probe
`skip_if_no_nvenc()` is reused), and M31's logged F1 call-attribution finding is
orthogonal (it concerns two pipelines this diff does not touch). Diff-bug
(Opus): implementation correct; three documentation-staleness findings.

**Findings scored** (Sonnet scorer, fresh agent, threshold 80):

- F2 (90) — **actioned, fixed in this review.** `NEWS.md` had no entry for this
  milestone and its nvenc bullet enumerated only `standardize_video()`/
  `format_for_web()`; the profile consistency-gate requires a changelog entry
  for user-visible changes. Extended the existing unreleased-cycle bullet to
  name `anonymize_video()` rather than adding a redundant second bullet.

- F1 (70) — **actioned at the user's direction at the merge gate**, though it
  scored below the 80 threshold. `has_nvenc()`/`nvenc_encoder()` roxygen said the
  toggle backs only `standardize_video()`/`format_for_web()`, leaving the new
  `@seealso` one-directional. Both the description paragraph and `@seealso` now
  name `anonymize_video()`; `man/nvenc_encoder.Rd` regenerated. Re-verified:
  `test()` 0 fail / 1233 pass, `check()` 0E/0W/0N, `check_pkgdown()` clean.

Below threshold — logged, not actioned (1):
- F3 (45) — `vignettes/workflow.Rmd` carries the same stale enumeration. Out of
  M33's declared scope (the vignette is untouched by this branch); the vignette
  pass shipped as M30.
