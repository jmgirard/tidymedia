# M14: Audio loudness normalization verb

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** m14-audio-loudness-normalization · https://github.com/jmgirard/tidymedia/pull/16

## Goal

Add `normalize_audio()`, a Layer-2 verb that normalizes a file's loudness to an
EBU R128 target via single-pass `loudnorm` (with optional downmix and resample),
plus the first Layer-1 audio-filter primitive `ffm_loudnorm()` that writes the
engine's until-now-unused `filter_audio`/`-af` slot.

## Scope

**In:**
- A Layer-1 audio-filter primitive `ffm_loudnorm()` that appends a `loudnorm=…`
  token to `object$filter_audio` — the first verb to populate that slot, which
  `ffm_compile()` already emits as `-af` (R/ffm.R:1043).
- A scalar Layer-2 verb `normalize_audio(infile, outfile, …)`: single-pass
  EBU R128 loudness normalization with primary-source-cited default targets
  (integrated loudness / true peak / loudness range), optional `channels`
  (downmix) and `sample_rate` (resample), and the video stream copied unchanged
  (`-c:v copy`) so only audio is touched. `run = FALSE` returns a pure compiled
  command (no binary).

**Out:**
- Two-pass measured/`linear` loudnorm (analyze → parse JSON → apply) → candidate
  row. It cannot produce the final command without running the binary, so it
  breaks the pure/CI-safe `run = FALSE` invariant — a new analyze-then-build
  execution pattern that is its own milestone.
- Batch sibling `normalize_audios()` → M15 (planned, depends on M14).
- Audio mixing / multi-input audio (`amix`) → stays deferred (D009).

## Acceptance criteria

- [ ] AC1 — `ffm_loudnorm()` appends a `loudnorm=…` token to `filter_audio` and
      a single-input pipeline compiles to `-af "loudnorm=I=…:TP=…:LRA=…"`;
      compilation is pure (no binary invoked). Evidence: passing compile test.
- [ ] AC2 — `normalize_audio(infile, outfile, run = FALSE)` returns one
      reproducible compiled command applying EBU R128 loudnorm with the default
      targets and `-c:v copy`, invoking no binary. Evidence: passing test.
- [ ] AC3 — The default target values (integrated loudness `I`, true peak `TP`,
      loudness range `LRA`) match a cited EBU R128 / ITU-R BS.1770 primary
      source, and that source is named in both the roxygen and this milestone.
      Evidence: citation present; default values equal the cited targets.
      (RB tripwire: no-oracle)
- [ ] AC4 — Supplying `channels` and/or `sample_rate` adds the corresponding
      downmix/resample instruction to the compiled command, and a set
      `sample_rate` is honored in the output; leaving them `NULL` (default) omits
      both flags, preserving the source channel layout. (The output sample rate
      is *not* the source rate under `NULL`: single-pass `loudnorm` resamples,
      up to 192 kHz encoder-capped — documented, and pinned via `sample_rate`;
      amended 2026-07-12 after review.) Evidence: compile tests for both branches
      plus an exec assertion that a set `sample_rate` reaches the output.
- [ ] AC5 — Invalid arguments (missing `infile`, non-scalar or out-of-range
      `I`/`TP`/`LRA`, non-positive `channels`/`sample_rate`) raise cli/rlang
      errors; no new assertthat (D004). Evidence: passing validation tests.
- [ ] AC6 — An execution test (`skip_if` ffmpeg absent) runs `normalize_audio()`
      on `inst/extdata/sample.mp4` and verifies a non-empty output carrying a
      decodable audio stream. Evidence: passing skip-guarded test.
- [ ] AC7 — `devtools::check()` clean (zero errors/warnings/notes).

## Coverage

- AC1 → T1
- AC2 → T3
- AC3 → T2, T3
- AC4 → T3
- AC5 → T3
- AC6 → T4
- AC7 → T5

## Tasks

- [x] T1 — Add `ffm_loudnorm()` Layer-1 audio-filter primitive to `R/ffm.R`
      (mirror the `ffm_scale()` pattern near R/ffm.R:308, but append to
      `object$filter_audio`). Tests-first for the compiled `-af "loudnorm=…"`
      output and pure compilation.
- [x] T2 — Determine and record the EBU R128 / ITU-R BS.1770 default targets
      (`I`, `TP`, `LRA`) with a primary-source citation; capture the source and
      the numbers for the roxygen and this milestone. (RB tripwire: no-oracle)
- [x] T3 — Add `normalize_audio(infile, outfile, channels = NULL,
      sample_rate = NULL, …targets…, run = TRUE)` Layer-2 verb to `R/ffmpeg.R`
      composing `ffm_loudnorm()` + `-c:v copy` + optional downmix/resample; a
      thin wrapper (IP1) that glues no strings of its own. Structure the body via
      a shared `normalize_audio_pipeline()` helper carrying per-value validation
      so M15's batch sibling inherits parity for free (M13 lesson). Parameter
      validation via cli/rlang. Tests-first for compile + validation, covering
      both the with- and without-option branches. (RB tripwire: irreversible-api
      — the exported signature soaks toward CRAN)
- [x] T4 — Add an execution test in `tests/testthat/` (`skip_if` binary absent)
      verifying non-empty, audio-decodable output on the sample.
- [x] T5 — Roxygen + `devtools::document()`; add to the `@family` lists and to
      DESIGN.md function families; `devtools::check()` clean.

## Work log

- 2026-07-12: created by /milestone-plan.
- 2026-07-12: T1 — `ffm_loudnorm()` primitive added (first verb to write the
  `-af` slot); EBU R128 defaults, range-validated; 5 compile/validation tests pass.
- 2026-07-12: T2 — no-oracle tripwire resolved at the implement gate (user chose
  EBU R128 over escalation); targets I=-23/TP=-1/LRA=7 cited to EBU Rec. R 128
  (2014) + ITU-R BS.1770-4 in the `ffm_loudnorm()` `@references` and Decisions.
- 2026-07-12: T3+T4 — `normalize_audio()` verb + shared
  `normalize_audio_pipeline()` helper (video stream-copied, optional
  downmix/resample via `-ac`/`-ar`); descriptive param names. 9 tests incl. a
  binary-gated exec test that produced audio-decodable output; full suite 279
  pass / 0 fail / 1 skip.
- 2026-07-12: T5 — DESIGN.md function families updated (`ffm_loudnorm`,
  `normalize_audio`); `devtools::check()` clean (0 errors / 0 warnings / 0
  notes). All tasks done → status review.
- 2026-07-12: review — independent review found (score 95) that AC4's "leaving
  them NULL … preserves the source layout/rate" is false for the sample rate:
  single-pass `loudnorm` outputs 192 kHz (encoder-capped), empirically 44100→96000
  Hz under defaults. Criterion fails as written → status back to in-progress for
  a gated amendment. (Finding 2, downmix-after-loudnorm loudness drift, scored 72
  — below threshold, logged not actioned.)
- 2026-07-12: review amendment applied — AC4 reworded (gated), docs/NEWS
  corrected, exec test strengthened to assert a pinned `sample_rate`; suite +
  check re-run clean → status review.

## Decisions

- 2026-07-12 (gate): default targets are EBU R128 — `target_loudness = -23`
  LUFS, `true_peak = -1` dBTP, `loudness_range = 7` — cited to EBU Rec. R 128
  (2014) with loudness measured per ITU-R BS.1770-4 (`loudness_range = 7` is
  FFmpeg `loudnorm`'s own default, EBU R128 not prescribing a single value).
- 2026-07-12 (gate): loudness knobs use descriptive R-idiomatic names
  (`target_loudness` / `true_peak` / `loudness_range`) on both `ffm_loudnorm()`
  and `normalize_audio()`, not the terse FFmpeg `I`/`TP`/`LRA` (irreversible-api).
- 2026-07-12 (review amendment): single-pass `loudnorm` resamples output (up to
  192 kHz, encoder-capped), so `sample_rate = NULL` does not preserve the source
  rate. Chosen fix (user gate): document the behavior and let `sample_rate` pin
  the rate — not a runtime probe, which would break the pure-compile/`run=FALSE`
  invariant (the same tension that deferred two-pass). AC4 amended accordingly.

## Review

**Reviewed:** 2026-07-12 · PR #16 · branch `m14-audio-loudness-normalization`.

### Acceptance-criteria evidence (fresh)

- [x] AC1 — `test-ffm.R` "ffm_loudnorm()" block (5 tests) pass: appends
      `loudnorm=…` to `filter_audio`, compiles to `-af "loudnorm=I=-23:TP=-1:LRA=7"`,
      pure (no binary). Coexists with `-vf`; chains after an existing audio filter.
- [x] AC2 — `test-normalize-audio.R` "compiles the default EBU R128 command"
      asserts the exact `run = FALSE` string `-y -i "<f>" -af "loudnorm=…" -codec:v
      copy "out.mp4"`; no binary invoked. Pass.
- [x] AC3 — defaults `target_loudness=-23`, `true_peak=-1`, `loudness_range=7`
      cited to EBU Rec. R 128 (2014) + ITU-R BS.1770-4 in the `ffm_loudnorm()` and
      `normalize_audio()` roxygen `@references` and the milestone Decisions;
      compile tests confirm the emitted defaults equal the cited values.
- [x] AC4 (amended) — compile tests pass: "adds downmix and resample when
      requested" (emits `-ac 1 -ar 48000`) and "omits downmix/resample by default"
      (no `-ac`/`-ar`); the strengthened exec test pins `sample_rate = 22050` and
      probes the output at 22050 Hz (honored). Source-rate non-preservation under
      `NULL` is now documented, not claimed as preserved.
- [x] AC5 — validation tests pass: missing infile ("exist"), out-of-range
      `I`/`TP`/`LRA` (3), non-positive/fractional `channels`/`sample_rate` (3);
      all cli/rlang, no assertthat.
- [x] AC6 — binary-gated exec test ran (ffprobe present, SKIP 0): wrote a
      non-empty output carrying a decodable `audio` stream on a synthesized
      source with a sine track.
- [x] AC7 — `devtools::check()` clean: 0 errors / 0 warnings / 0 notes.

Full suite under `load_all()`: 279 pass / 0 fail / 1 skip (mediainfo-gated).
Note: running the suite via a bare `test_dir()` *after* `check()` in one session
loads the installed package and spuriously fails the new tests (M11 lesson) —
use `load_all()`.

### Consistency gate

- `cairn_validate.py` — exit 0, all 10 checks PASS.
- Coverage completeness — AC1→T1, AC2→T3, AC3→T2/T3, AC4→T3, AC5→T3, AC6→T4,
  AC7→T5; every criterion maps to an existing task. Pass.
- `devtools::document()` — no diff.
- DESIGN principles — none changed (verb is consistent with IP1/IP2); impact
  report skipped.
- README.Rmd — in sync with README.md (untouched; it uses one verb only as an
  illustrative example).
- NEWS.md — added an "Audio loudness normalization" entry (no milestone numbers).
- `_pkgdown.yml` — added `ffm_loudnorm` (Layer 1) and `normalize_audio`
  (Layer 2); `pkgdown::check_pkgdown()` — no problems found.
- No new top-level files.

### Independent review (two lenses + scorer)

- [O] diff-bug (Opus): 2 findings. [S] blame-history (Sonnet): no findings —
  confirmed no undoing of the `-af` slot, no M12-lesson resurrection (video is
  copied here), no D009/D007/D002 contradiction, M13 shared-helper pattern
  honored.
- **Finding 1 (score 95) — ACTIONED (fixed).** Docs/NEWS/AC4 claimed
  `sample_rate = NULL` preserves the source rate; single-pass `loudnorm` outputs
  up to 192 kHz (encoder-capped), empirically 44100→96000 Hz. Fixed by honest
  documentation + AC4 amendment (user gate: document, not runtime-probe, to keep
  pure-compile); exec test strengthened to assert a pinned rate is honored.
- **Finding 2 (score 72) — LOGGED, not actioned** (below the 80 threshold):
  `-ac` downmix is applied after `loudnorm`, so loudness is normalized pre-downmix
  and the final integrated loudness can drift slightly from `target_loudness`.
  Reviewer notes the effect is usually small (~-3 dB default downmix ≈ level-
  preserving) and fixing conflicts with the single-pass/no-DAG scope. Candidate
  for the deferred two-pass/measured work if precision downmixing is wanted.

### Re-verification after amendment

- normalize-audio tests: 9 pass / 0 fail / 0 skip (incl. strengthened exec).
- `devtools::check()`: 0 errors / 0 warnings / 0 notes. `document()`: no diff
  beyond `man/normalize_audio.Rd`.
