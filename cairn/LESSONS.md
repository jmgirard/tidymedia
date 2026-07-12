# Lessons

_Durable, append-only repo lessons (build quirks, testing tricks) — captured at
milestone end, surfaced at plan time. Capped at 50 lines (D-015); prune the
least-useful when full. Not status, not decisions (a choice is a D-entry)._

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
- 2026-07-12 (M16): a two-pass-loudnorm accuracy test needs a high-LRA source —
  the packaged `sample.mp4` is a steady tone (LRA 0) where single- and two-pass
  land identically, so a "closer than single-pass" assertion ties and fails.
  Generate loudness variation (e.g. `tremolo=f=0.2:d=0.9`) to expose the gap.
- 2026-07-12 (M16): `ffmpeg()` runs via `system()` (a shell), so a filter value
  with commas (e.g. `volume='if(lt(t,3),..)'`) needs the value double-quoted so
  ffmpeg's own quotes survive the shell; prefer a comma-free filter to avoid it.
- 2026-07-12 (M17): `devtools::check()` can print "0 notes" while raw `R CMD
  check` shows `Status: 1 NOTE` — the `tests/spelling.Rout` comparison NOTE for
  new technical terms is masked by the devtools summary. Run
  `spelling::update_wordlist()` and confirm `Status: OK` in `00check.log`.
- 2026-07-12 (M18): a cli `{?s}` governed by a `{.val {vector}}`/`{cli::qty(vec)}`
  throws `length(object) == 1` with 2+ items in a multi-line `cli_warn`/`abort`.
  Drive plurals off a scalar `{length(x)}` and list the vector without `{?s}`.
  Test cli count messages with 2+ items — a 1-item test hides the crash.
- 2026-07-12 (M19): a fast-path branch that skips the shared batch runner
  (`ffm_batch`) must re-synthesize that runner's *opt-in* outputs (`verified`
  column, manifest attr) or the return schema silently diverges from the normal
  path across calls. Trick: subsetting a canonical 0-row schema tibble pads to N
  all-NA rows for free (`col[rep(NA_integer_, n)]` gives n type-matched NAs).
  Test parity by comparing `names()`/types of the fast vs the normal path.
- 2026-07-12 (M20): `check_dim()` (`ffm_drawbox`/`ffm_crop`/`ffm_scale`) rejects
  bare integers — it accepts `is_double(n=1)` or a string, not `integer`. A
  Layer-2 verb passing table columns (e.g. a tibble's `1:2`) must coerce them to
  double first, or valid pixel coordinates abort as a bad FFmpeg expression.
