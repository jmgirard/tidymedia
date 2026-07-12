<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M20: Fixed-region box-fill anonymization verb

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, IP2
- **Branch/PR:** m20-anonymize-video-box-fill · https://github.com/jmgirard/tidymedia/pull/22

## Goal

Add a Layer-2 `anonymize_video()` verb that covers one or more fixed
rectangular regions of a video with opaque filled boxes for de-identification.

## Scope

**In:** a scalar task verb `anonymize_video(infile, outfile, regions, …)` that
takes a regions data frame (one row per box: `x`, `y`, `width`, `height`, plus
an optional per-row `color`), chains one filled `ffm_drawbox` per region into a
single sequential `-vf` chain, re-encodes video reproducibly (libx264/yuv420p
defaults, even-dimension floor guard), and stream-copies audio. A shared
Layer-2-internal `anonymize_pipeline()` helper holds all per-region validation
so the deferred M21 batch sibling inherits it (M13 extract-first lesson).

**Out:**
- Region *blur* (split→crop→boxblur→overlay) → candidate row; needs an IP2
  filtergraph design call and a new `ffm_boxblur` filter, not plannable yet.
- Batch sibling `anonymize_videos()` → M21 (depends on M20).
- Face/object tracking or any motion-following redaction → out of package
  scope (GP1); regions are fixed for the whole clip.
- Hollow outlines / configurable `thickness` → deliberately not exposed;
  anonymization means an opaque fill.

## Acceptance criteria

- [x] `anonymize_video()` is exported (NAMESPACE + `_pkgdown.yml` Layer-2 row)
      with roxygen docs and a runnable `run = FALSE` example.
- [x] Compiling with an N-row regions data frame emits exactly N filled
      `drawbox=…:t=fill` filters with the row's `x`/`y`/`w`/`h`, joined into a
      single `-vf` chain (no `-filter_complex`) — IP2-clean. (compilation test)
- [x] Audio is stream-copied (`-c:a copy`) and video re-encoded reproducibly
      (libx264/yuv420p defaults; odd source dimensions floored to even); the
      same input + regions compiles to a byte-identical command. (compilation test)
- [x] A per-row `color` column overrides the `color` argument; the default fill
      is opaque black. (compilation test)
- [x] Input validation aborts via `cli::cli_abort()` on each of: `regions` not a
      data frame, zero rows, a missing `x`/`y`/`width`/`height` column, `NA` in a
      region column, and non-positive `width`/`height` — every branch fired. (tests)
- [x] Executes end-to-end on the packaged `sample.mp4` producing a valid video
      output. (execution test, `skip_if` ffmpeg absent)
- [x] `devtools::check()` is clean: 0 errors / 0 warnings / 0 notes (spelling
      wordlist updated for any new terms).

## Coverage

- AC1 → T2, T5
- AC2 → T1, T2, T3
- AC3 → T1, T3
- AC4 → T1, T3
- AC5 → T1, T3
- AC6 → T4
- AC7 → T5

## Tasks

- [x] T1: Write internal `anonymize_pipeline(object, regions, color, vcodec,
      pixel_format)` (place beside the other Layer-2 helpers): validate the
      regions data frame and its columns, apply the even-dimension floor crop
      (mirror `standardize_video()` R/ffmpeg.R:310), chain one `ffm_drawbox(…,
      thickness = "fill")` per row (per-row `color` column overrides the arg),
      set `ffm_codec`/`ffm_pixel_format`, and `ffm_output_options("-c:a copy")`.
      Extract-first so M21 reuses it (M13 lesson).
- [x] T2: Front door `anonymize_video(infile, outfile, regions, color = "black",
      vcodec = "libx264", pixel_format = "yuv420p", run = TRUE)` over
      `ffm_files` → `anonymize_pipeline` → `ffm_map("0")` → `ffm_finish`
      (mirror `crop_video()` R/ffmpeg.R:213). Roxygen `@family task verb
      functions`; add the `_pkgdown.yml` Layer-2 row; `devtools::document()`.
- [x] T3: Compilation tests (pure, CI-safe): single region; N regions chain
      into one `-vf`; per-row + default color; `-c:a copy` and codec/pixfmt
      present; even-dim guard present; byte-identical recompile; plus every
      validation abort branch from AC5 (assert on the `cli` messages).
- [x] T4: Execution test (`skip_if` ffmpeg absent): run on `sample.mp4`, assert
      the output exists and is a valid video (probe or `verify_media`).
- [x] T5: `devtools::document()`, `devtools::test()`, then `devtools::check()`
      to OK (0/0/0); `spelling::update_wordlist()` if new terms; confirm
      `Status: OK` in `00check.log` (M17 masked-NOTE lesson).

## Work log

- 2026-07-12: created by /milestone-plan (box-fill scope chosen at gate;
  region blur deferred to candidate, batch sibling to M21).
- 2026-07-12: T1–T4 done — anonymize_video() + shared anonymize_pipeline() +
  check_regions() in R/ffmpeg.R; 25 tests pass (compile + validation +
  execution). pkgdown row + docs regenerated.
- 2026-07-12: T5 done — `devtools::check()` Status: OK (0/0/0); "Anonymize"
  added to WORDLIST (M17 masked-NOTE lesson). Status → review.

## Decisions

## Review

**Evidence (2026-07-12, PR #22, fresh run on branch @ HEAD).**

- AC1: `export(anonymize_video)` in NAMESPACE; `_pkgdown.yml` Layer-2 row;
  `man/anonymize_video.Rd` generated with a `run = FALSE` example;
  `pkgdown::check_pkgdown()` → "No problems found."
- AC2: test `chains N regions into a single -vf (no filter_complex)` passes —
  two filled `drawbox=…:t=fill` present, `-vf` yes, `-filter_complex` absent.
- AC3: test `re-encodes reproducibly and copies audio` passes — even-dim crop
  guard, `-codec:v libx264 -codec:a copy`, `-pix_fmt yuv420p`, byte-identical
  recompile.
- AC4: test `honors color argument, per-row color, and expressions` passes.
- AC5: test `rejects a malformed regions table` passes — all five abort
  branches (non-df, zero rows, missing column, NA, non-positive size) fired.
- AC6: test `runs end-to-end and writes a valid video` passes (ffmpeg present)
  — output exists, non-zero size, duration ≈ 2s.
- AC7: `devtools::check()` → Status: OK, 0 errors / 0 warnings / 0 notes.
  Full suite: 810 pass / 0 fail / 0 skip.

**Consistency gate.** `cairn_validate` exit 0 (all checks PASS) after
normalizing DESIGN.md principle-definition lines to the validator-parseable
`- IPn: …` form (format only; principle substance unchanged, no D-entry).
`devtools::document()` no further diff. `pkgdown::check_pkgdown()` clean. NEWS
entry added. No new top-level files (WORDLIST in inst/, test in tests/).

**Independent review (3 lenses, fresh context).** Zero actionable findings;
scorer no-op (no findings to score).
- [O] diff-bug (Opus): no defects — factor-column rejection, integer→double
  coercion, non-positive-size deferral to `check_dim`, color-column alignment,
  even-dim guard vs. region coordinates, audio-copy, and IP1/IP2 all verified
  clean.
- [S] blame-history: faithful to intent/history; honors M12 (even-dim +
  audio-copy) and M13 (extract-first) lessons; no D-entry contradicted;
  confirmed the DESIGN.md principle reformat aligns with cairn's canonical
  `- IPn:` style and loses no information.
- [S] prior-PR-comments: no prior-PR evidence (merged PRs on `ffmpeg.R` carry
  only Codecov bot comments).
- Considered and dropped (logged, not actioned): (1) character region values
  pass through `check_dim` unsanitized — pre-existing shared behavior of every
  dimension verb, not introduced here; (2) no `ffm_map("0")`, so a
  multi-audio-track source keeps only the default-selected audio — the
  deliberate `standardize_pipeline()` pattern M20 mirrors, and AC3 asks only
  for `-c:a copy`.
