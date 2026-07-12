# M15: Batch audio normalization verb

- **Status:** review
- **Priority:** normal
- **Depends on:** M14
- **Branch/PR:** m15-batch-audio-normalization / https://github.com/jmgirard/tidymedia/pull/17

## Goal

Add `normalize_audios()`, the tibble-driven batch sibling of `normalize_audio()`,
built over `ffm_batch()` (D007) — mirroring the M12 → M13 (scalar → batch) split.

## Scope

**In:**
- `normalize_audios(jobs, …)`: a fan-out Layer-2 wrapper that builds one
  `normalize_audio()` pipeline per jobs-tibble row via `ffm_batch()`, returning
  the jobs tibble plus a `command` column (and `success` when run).
- Per-element validation parity with the scalar verb, inherited from the shared
  `normalize_audio_pipeline()` helper (M14 T3) plus front-door column/NA guards.
- Pass-through of `verify`/`manifest`/`checksums`/`progress`/`parallel` to
  `ffm_batch()`.

**Out:**
- Any change to `normalize_audio()`'s loudness semantics or defaults — those are
  fixed in M14; this milestone only fans them out.
- Two-pass measured loudnorm → its own candidate (depends on M14).

## Acceptance criteria

- [x] AC1 — `normalize_audios(jobs, …)` fans out over the jobs tibble via
      `ffm_batch()`, emitting one reproducible command per row and returning the
      jobs tibble + `command` (+ `success` when run). Evidence: passing test.
- [x] AC2 — Per-row validation rejects the same invalid values the scalar verb
      rejects (via the shared pipeline helper + column/NA guards), per the
      M11/M13 parity lesson. Evidence: passing parity test.
- [x] AC3 — `verify`/`manifest`/`checksums`/`progress`/`parallel` forward to
      `ffm_batch()` and never leak into the per-row `.f` (M09 lesson — batch
      params sit after `...` and bind by name). Evidence: passing test.
- [x] AC4 — An execution test (`skip_if` ffmpeg absent) runs a 2-row jobs tibble
      and verifies non-empty, audio-decodable outputs. Evidence: passing
      skip-guarded test.
- [x] AC5 — `devtools::check()` clean (zero errors/warnings/notes).

## Coverage

- AC1 → T2
- AC2 → T1, T2
- AC3 → T2
- AC4 → T3
- AC5 → T4

## Tasks

- [x] T1 — Confirm/extract the shared `normalize_audio_pipeline()` helper (from
      M14 T3) carrying per-value validation, so the batch front door needs only
      column type/NA guards, not re-implemented value checks (M13 lesson).
- [x] T2 — Add `normalize_audios(jobs, run = TRUE, …)` batch sibling over
      `ffm_batch()`, forwarding batch params after `...` (M09 lesson);
      tests-first for compile + per-row validation parity.
- [x] T3 — Add an execution test (`skip_if` binary absent) on a 2-row jobs
      tibble producing non-empty, audio-decodable outputs.
- [x] T4 — Roxygen + `devtools::document()`; add to the `@family` lists and to
      DESIGN.md function families; `devtools::check()` clean.

## Work log

- 2026-07-12: created by /milestone-plan.
- 2026-07-12: T1 — verified `normalize_audio_pipeline()` (R/ffmpeg.R:438) already
  carries per-value validation (loudness via `ffm_loudnorm()`, whole
  channels/sample_rate via `check_number_whole()`); no extraction needed, batch
  front door only adds column type/NA guards.
- 2026-07-12: T2/T3 — added `normalize_audios()` + `derive_normalized_names()`
  (R/ffmpeg.R) as a thin `ffm_batch()` fan-out over the shared pipeline,
  modelled on `standardize_videos()`; tests in
  tests/testthat/test-normalize-audios.R (compile-parity, per-row knob
  overrides, auto-naming/collision, front-door + inherited per-element value
  parity, `...` forwarding, binary-gated decodable-output + verify). 64 pass,
  0 fail (ffmpeg present).
- 2026-07-12: T4 — `document()` regenerated NAMESPACE + man/normalize_audios.Rd;
  `devtools::check()` clean (zero errors/warnings/notes). Minor deviation: the
  DESIGN.md Layer-2 list enumerates scalar verbs only (the batch siblings
  `standardize_videos`/`extract_frames`/`segment_videos` are not individually
  listed — they're covered by the D007 batch-runner line), so `normalize_audios`
  was not added there to preserve that convention; the `@family task verb
  functions` roxygen tag is set.

## Decisions

## Review

**Fresh evidence (2026-07-12, PR #17, branch cut from unchanged master):**

- AC1 — `test-normalize-audios.R` "returns one command per job across multiple
  inputs" + "command is byte-identical to the scalar verb" + "default knobs match
  the scalar defaults" pass: fan-out returns a tibble with a `command` per row.
- AC2 — parity tests "rejects an out-of-range loudness value per row" and
  "rejects a non-whole channels value per row" (inherited from
  `normalize_audio_pipeline()`), plus front-door type/NA guard tests, all pass.
- AC3 — "forwards batch params after ... without leaking into .f" (`progress`)
  and the binary-gated "forwards verify" test pass; batch params bind by name
  after `...`.
- AC4 — binary-gated "writes non-empty, audio-decodable outputs" ran (ffmpeg
  present, 0 skips): both outputs exist, decode as audio, honor pinned
  `sample_rate`.
- AC5 — `devtools::check()` clean: 0 errors / 0 warnings / 0 notes.
- Whole file: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 46 ]`.

**Consistency gate:** `cairn_validate.py` exit 0 (all checks pass); Coverage
complete (AC1→T2, AC2→T1/T2, AC3→T2, AC4→T3, AC5→T4, all tasks exist);
`document()` no diff; README.md in sync; `pkgdown::check_pkgdown()` — "No problems
found" after adding `normalize_audios` to `_pkgdown.yml`; NEWS.md entry added
("Batch audio normalization across files"); no new top-level files; no DESIGN
principle changed.

**Independent fresh-context review (two lenses):**

- [O] diff-bug reviewer (Opus) — **no findings.** Verified per-row knob override,
  validation parity (front-door type/NA + inherited per-element checks), `...`
  batch-param forwarding without leaking into `.f`, auto-naming/collision, and
  edge cases (factor input, NULL defaults).
- [S] blame-history reviewer (Sonnet) — **no findings.** Change does not undo
  prior-milestone work or contradict D002/D007 or the M09/M10/M11/M13 lessons;
  NEWS/pkgdown treatment matches the `standardize_videos()` precedent.

Zero surviving findings → scorer not required. Two candidates the Opus reviewer
raised and dropped as pre-existing (not introduced by this diff), logged here for
transparency (IP3), no action:
1. No compile-time file-existence check on `input` — inherited
   `standardize_videos()` convention; surfaces at run time.
2. `as.character()` scientific-notation risk in `-ar <sample_rate>` for round
   values (e.g. `100000` → `"1e+05"`) — lives in the unmodified M14
   `normalize_audio_pipeline()`, not triggered by real audio rates
   (44100/48000/96000/192000 stringify plainly).
