# Lessons

_Durable, append-only repo lessons (build quirks, testing tricks) — captured at
milestone end, surfaced at plan time. Capped at 50 lines (D-015); prune the
least-useful when full. Not status, not decisions (a choice is a D-entry)._

- 2026-07-12 (M10): `is.logical(x)` accepts vectors containing `NA`, so a
  bare `is.logical()` type-guard admits `NA` where `rlang::check_bool()`
  (scalar path) rejects it — pair it with `!anyNA()` when validating a logical
  column, or the `NA` leaks downstream to an internal check with a worse error.
- 2026-07-12 (M11): gathering fresh test evidence, `testthat::test_file()` under
  `library(tidymedia)` loads the *installed* package and misses new dev exports
  ("could not find function") — use `devtools::test(filter=)` / `load_all()`,
  which source the working tree.
- 2026-07-12 (M11): a table-driven sibling must mirror the scalar verb's
  *per-element* validation (wholeness, finiteness), not just column type — a
  column-type-only check silently admits values the scalar verb rejects and
  diverges from the sibling's own documented contract (review finding, score 82).
- 2026-07-12 (M12): a Layer-2 verb that re-encodes video (sets `-c:v`) will
  silently transcode audio and *reject odd dimensions* under libx264/yuv420p
  (0-byte output) unless it also stream-copies audio (`-c:a copy`) and
  floor-crops to even dimensions — mirror `format_for_web()`'s guards. Test
  re-encode verbs against an odd-dimensioned source and a non-target audio codec.
- 2026-07-12 (M13): when a scalar verb keeps its per-value validation *inside* a
  shared `*_pipeline()` helper (via `check_dim`/`check_token`/`ffm_files`), a
  batch sibling that reuses the helper inherits per-element parity for free —
  its front door only needs column type/NA guards, not re-implemented value
  checks. Contrast M11's `extract_frames()`, which had to duplicate finite/whole
  checks because `frame_pipeline()` did *not* contain them. So: extract the
  scalar body into a shared pipeline before writing the batch sibling.
- 2026-07-12 (M14): FFmpeg's single-pass `loudnorm` filter forces its output
  sample rate (advertises 192 kHz; the encoder then caps it, e.g. 96 kHz for
  AAC) — it never preserves the source rate. Any audio-filtering verb that omits
  `-ar` silently resamples; document that and let a `sample_rate` arg pin `-ar`.
  A "preserves the source rate" claim is a review trap — verify output rate by
  probing (`ffprobe … stream=sample_rate`), don't assume `-af`-only is a no-op.
- 2026-07-12 (M15): a new exported verb needs a `_pkgdown.yml` reference entry or
  `pkgdown::check_pkgdown()` fails ("topic missing"), but the DESIGN.md Layer-2
  verb list enumerates *scalar* verbs only — batch siblings (`*_videos`,
  `*_frames`, `normalize_audios`) are not listed there (covered by the D007
  batch-runner line). So: add the pkgdown entry, skip the DESIGN.md list; the
  `@family` roxygen tag is the machine-checkable family membership.
- 2026-07-12 (M16): a two-pass-loudnorm accuracy test needs a high-LRA source —
  the packaged `sample.mp4` is a steady tone (LRA 0) where single- and two-pass
  land identically, so a "closer than single-pass" assertion ties and fails.
  Generate loudness variation (e.g. `tremolo=f=0.2:d=0.9`) to expose the gap.
- 2026-07-12 (M16): `ffmpeg()` runs via `system()` (a shell), so a filter value
  with commas (e.g. `volume='if(lt(t,3),..)'`) needs the value double-quoted so
  ffmpeg's own quotes survive the shell; prefer a comma-free filter to avoid it.
