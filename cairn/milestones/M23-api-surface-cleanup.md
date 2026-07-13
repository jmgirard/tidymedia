<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M23: API surface cleanup (clean-break renames, arg harmonization, un-exports)

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m23-api-surface-cleanup · https://github.com/jmgirard/tidymedia/pull/25   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Execute the approved M22 clean-break API changes — function renames, argument
harmonization, and un-exports (per D014) — so the public surface matches the
ratified naming scheme before docs and CRAN work.

## Scope

**In:** the irreversible public-API changes from the M22 audit
(`cairn/references/naming-docs-audit-M22.md §5–6`, ratified in **D014**), under
the clean-break policy (no `lifecycle` shims):

- **Function renames** (old names removed, not deprecated):
  - `get_codecs` → `ffmpeg_codecs`, `get_encoders` → `ffmpeg_encoders` (N1)
  - `audio_as_mp3` → `convert_audio` with a `format` arg (N2)
  - `get_samplingrate` → `get_sample_rate`, `get_framerate` → `get_frame_rate` (N3)
  - batch siblings → `<scalar_verb>_batch` (N4/N8): `segment_videos` →
    `segment_video_batch`, `standardize_videos` → `standardize_video_batch`,
    `normalize_audios` → `normalize_audio_batch`, `anonymize_videos` →
    `anonymize_video_batch`, `extract_frames` → `extract_frame_batch`
- **Argument harmonization** (T3 of the audit): `acodec`/`vcodec` →
  `audio_codec`/`video_codec`; `ts_start`/`ts_stop` → `start`/`end`
- **Un-exports / reexport cleanup** (N6/N7): drop the 4 unused tidy-eval quoting
  reexports (`enquo`, `enquos`, `as_label`, `as_name`) and `:=` (no internal
  use, verified); **keep `.data`** (used in `filter_streams()`); un-export
  `pad_integers` and `convert_fractions` (drop `@export`; both stay internal).
- Ripple every rename through `R/`, `tests/`, `vignettes/`, `README.Rmd`, and
  regenerate `NAMESPACE`/`man/`.

**Out:**
- The `@seealso` web, metadata-boundary prose, and batch-disambiguation prose →
  **M24** (docs gap-fill, depends on this milestone so cross-links use final names).
- CRAN release mechanics → the "CRAN readiness" candidate.
- Any new behavior beyond `convert_audio`'s `format` arg — this is a
  rename/signature milestone, not a feature milestone.

`(RB tripwire: irreversible-api)` — the whole milestone removes and renames
public surface with no compatibility shims.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1** All 11 renames applied: `NAMESPACE` exports the new names
      (`ffmpeg_codecs`, `ffmpeg_encoders`, `convert_audio`, `get_sample_rate`,
      `get_frame_rate`, `segment_video_batch`, `standardize_video_batch`,
      `normalize_audio_batch`, `anonymize_video_batch`, `extract_frame_batch`)
      and a repo-wide grep finds **zero** occurrences of every old name in
      `R/`, `tests/`, `vignettes/`, `README.Rmd`. `(RB tripwire: irreversible-api)`
- [ ] **AC2** `convert_audio(infile, outfile, format = NULL, run = TRUE)`: with
      `format = NULL` the compiled command is byte-for-byte identical to the old
      `audio_as_mp3` (map `a`, `-q:a 0`, container from the outfile extension); a
      non-`NULL` `format` pins the output format/codec. Verified by a
      compile-level test covering both branches.
- [ ] **AC3** Argument harmonization complete: `audio_codec`/`video_codec`
      replace `acodec`/`vcodec` (`extract_audio`, `standardize_video[_batch]`,
      `anonymize_video[_batch]`), and `start`/`end` replace `ts_start`/`ts_stop`
      (`segment_video[_batch]`), including `@param` docs, defaults, and job-table
      column names; no old arg name remains. Verified by signature checks +
      updated tests. `(RB tripwire: irreversible-api)`
- [ ] **AC4** Reexport/un-export cleanup done: `NAMESPACE` no longer exports
      `enquo`, `enquos`, `as_label`, `as_name`, `:=`, `pad_integers`,
      `convert_fractions`; it still exports `.data`; `pad_integers` and
      `convert_fractions` remain callable internally (their internal call sites
      still resolve). `(RB tripwire: irreversible-api)`
- [ ] **AC5** `devtools::document()` regenerates `man/` + `NAMESPACE`; the full
      test suite passes with the renamed identifiers; `devtools::check()` is
      clean — confirm `Status: OK` in `00check.log` (not just the devtools
      summary; `spelling::update_wordlist()` run first — LESSONS M17).
- [ ] **AC6** `vignettes/*.Rmd` (esp. `batch.Rmd`) and `README.Rmd` reference
      only the new names/args; `devtools::build_readme()` regenerates `README.md`.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2, T3, T4
- AC2 → T3
- AC3 → T5
- AC4 → T6
- AC5 → T7
- AC6 → T1, T2, T3, T4, T5, T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1** Rename the 5 batch siblings to `<scalar_verb>_batch`
      (`R/ffmpeg.R:614/1206/1316/1485/1691` + `R/loudnorm_two_pass.R`): update
      defs, roxygen, all call sites in `tests/`, `vignettes/batch.Rmd`,
      `README.Rmd`, and rename the matching `tests/testthat/test-*.R` files.
- [x] **T2** N1: rename `get_codecs`/`get_encoders` → `ffmpeg_codecs`/
      `ffmpeg_encoders` (`R/ffmpeg.R`) + roxygen `@family`/title + tests.
- [x] **T3** N2: replace `audio_as_mp3` (`R/ffmpeg.R:182`) with
      `convert_audio(infile, outfile, format = NULL, run = TRUE)` — `NULL`
      reproduces the old command, non-`NULL` pins format; add the compile-level
      parity + format test.
- [x] **T4** N3: rename `get_samplingrate`/`get_framerate` →
      `get_sample_rate`/`get_frame_rate` (`R/mediainfo.R:373/316`) + tests.
- [x] **T5** Arg harmonization: `acodec`/`vcodec` → `audio_codec`/`video_codec`
      and `ts_start`/`ts_stop` → `start`/`end` across `R/ffmpeg.R` (defs,
      defaults, `@param`, job-table column reads) and every test/vignette site.
- [x] **T6** N6/N7: in `R/utils-tidy-eval.R` drop the `@export` reexports of
      `enquo`/`enquos`/`as_label`/`as_name`/`:=` (keep `.data`); drop `@export`
      on `pad_integers` (`R/utils.R`) and `convert_fractions` (`R/ffprobe.R`),
      leaving both as internal helpers.
- [x] **T7** `devtools::document()`; `spelling::update_wordlist()`; sync any
      remaining `vignettes/*.Rmd` + `README.Rmd` and `devtools::build_readme()`;
      run `devtools::check()` and confirm `Status: OK` in `00check.log`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (executes M22 audit §5–6 / D014).
- 2026-07-12: T1 — renamed 5 batch siblings to `*_batch` across R/, tests (incl. test files), vignettes, README; document() regen; tests 0F/0W.
- 2026-07-12: T2 — `get_codecs`/`get_encoders` → `ffmpeg_codecs`/`ffmpeg_encoders` (R/, test-ffmpeg.R, README, ffm.R prose); tests 0F/0W.
- 2026-07-12: T3 — `audio_as_mp3` → `convert_audio(format = NULL)`; NULL keeps `-q:a 0`/map-a byte-for-byte, non-NULL emits `-codec:a <format>` (drops `-q:a`); added parity + format tests; tests 0F/0W.
- 2026-07-12: T4 — `get_samplingrate`/`get_framerate` → `get_sample_rate`/`get_frame_rate` (R/mediainfo.R + internal callers in R/ffmpeg.R + tests); tests 0F/0W.
- 2026-07-12: T5 — arg harmonization: `acodec`/`vcodec` → `audio_codec`/`video_codec` (args, `@param`, job-table columns `pick()`/`str_cols`, error strings), `ts_start`/`ts_stop` → `start`/`end` (segment_video scalar now matches the batch's `start`/`end` columns). Left the unrelated local `acodec()` ffprobe helper in test-ffmpeg.R untouched. Full suite 0F/0W.
- 2026-07-12: T6 — reexports trimmed to `.data` only (dropped `enquo`/`enquos`/`as_label`/`as_name`/`:=`); `pad_integers` + `convert_fractions` un-exported via `@noRd` (stale man pages removed, still callable internally). Full suite 0F/0W.
- 2026-07-12: T7 — document() + build_readme(); reframed `metadata.Rmd` to drop the now-internal `convert_fractions()` chunk; `spelling::update_wordlist()` (+VBR, −5 tidy-eval prose words). `devtools::check()` → **Status: OK** (0E/0W/0N), spelling.Rout comparison OK. All 6 ACs met → status review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (plan): `convert_audio` default `format = NULL` infers the format
  from the outfile extension (preserving old `audio_as_mp3` behavior); mp3 is
  **not** hard-coded as a default (user decision at plan gate).

## Review
<!-- owner: review · exclusive -->
