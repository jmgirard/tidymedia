# M23: API surface cleanup (clean-break renames, arg harmonization, un-exports)

- **Status:** done · **PR:** https://github.com/jmgirard/tidymedia/pull/25 · 2026-07-13

## Goal
Execute the approved M22 audit (§5–6, ratified in D014) — renames, argument
harmonization, un-exports — so the public API matches the ratified scheme.
Clean break: no `lifecycle` shims (pre-0.2.0).

## Outcome
- Batch verbs → `<verb>_batch` (segment/standardize/normalize/anonymize/extract_frame).
- Capability queries out of `get_*`: `ffmpeg_codecs()`/`ffmpeg_encoders()`.
- `audio_as_mp3` → `convert_audio(format = NULL)`: `NULL` = old command byte-for-byte;
  non-`NULL` pins the codec via `-codec:a` (drops `-q:a`).
- Getters `get_sample_rate()`/`get_frame_rate()`; args `audio_codec`/`video_codec`
  (incl. jobs-table columns) and `start`/`end` (was `ts_start`/`ts_stop`).
- Removed 7 exports: reexports `enquo`/`enquos`/`as_label`/`as_name`/`:=` (kept `.data`)
  + `pad_integers`/`convert_fractions` (now internal `@noRd`).
- `check()` OK (0/0/0); 873 tests pass; `check_pkgdown()` clean; review 0 findings/3 lenses.

## Key decisions
- `convert_audio` default `format = NULL` (no hard-coded mp3, plan gate); naming/policy
  per **D014** (M23 executes it, no new D-entry). Follow-up: M24 (docs gap-fill).
