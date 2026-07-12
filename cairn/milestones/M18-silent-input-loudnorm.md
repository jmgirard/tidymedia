<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M18: Graceful silent-input handling for two-pass loudnorm

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M16, M17   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m18-silent-input-loudnorm   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Give silent input its own honest handling in the two-pass loudnorm path: a
clear silence-specific error for the scalar verb, and continue-and-mark
(don't abort the batch) for the table sibling.

## Scope

**In:**
- Detect the digital-silence signature in a `loudnorm` analysis-pass
  measurement (`input_i = "-inf"`, i.e. `input_i` parses to `-Inf`; FFmpeg
  also emits `target_offset = "inf"`, `normalization_type = "dynamic"`) and
  treat it as a distinct, recognized outcome — not a generic parse failure.
- `normalize_audio(two_pass = TRUE)` on a silent input: abort with a
  silence-specific message ("the input appears to be silent; loudness
  normalization to a target is undefined for silence"), replacing the current
  misleading "Could not parse the `loudnorm` measurement" abort
  (`R/loudnorm_two_pass.R:48-55`, reached via `run_loudnorm_analysis`).
- `normalize_audios(two_pass = TRUE)` on a jobs table containing silent
  rows: do **not** abort the whole run. Normalize the non-silent rows
  (outputs written, `success = TRUE`), set silent rows aside (marked in a
  new `silent` logical column, `success = FALSE`, no output written), and
  emit a single warning naming the silent rows. Mirrors `ffm_batch`'s
  record-and-continue idiom (D011).
- Share the silence detector between scalar and batch (one internal helper
  in `R/loudnorm_two_pass.R`; no duplicated regex — D011 spirit).
- Preserve fail-fast for *genuine* analysis failures (non-zero FFmpeg exit,
  or a non-silence measurement block that is missing/non-finite): those
  still abort naming the offending rows (`assemble_measured`,
  `R/loudnorm_two_pass.R:132-154`). Silence is the only condition promoted
  from "abort" to "continue-and-mark".

**Out:**
- Single-pass silence handling — the single-pass filter never runs an
  analysis pass, so it produces silent output without any parse step;
  detecting silence there would require adding an analysis pass, defeating
  the purpose. Single-pass on silence stays as-is (unchanged behavior).
- Passthrough/stream-copy semantics for silent input — considered and
  rejected at the plan gate in favor of a clear error (a "normalized"
  output that is actually untouched silence weakens the reproducibility
  guarantee). Not deferred; a decided design choice.
- Near-silence / very-quiet inputs — these yield a legitimate finite
  `input_i` (e.g. `-70`) and are normalized normally; no special handling.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1 — `normalize_audio(two_pass = TRUE)` on silent input aborts with a
      silence-specific error that names silence as the cause and is textually
      distinct from the generic "Could not parse" message. Verified by a pure
      (binary-free) test against a recorded real-FFmpeg silence fixture, and
      by a live `skip_if`-guarded execution test running `anullsrc` silence
      end-to-end.
- [ ] AC2 — No regression on genuinely unparseable / missing / non-finite
      (non-silence) measurement blocks: the existing generic "Could not parse"
      abort still fires. Verified by pure tests against the existing bad-input
      cases (missing block, non-numeric values).
- [ ] AC3 — `normalize_audios(two_pass = TRUE)` on a mixed jobs table (≥1
      silent, ≥1 non-silent) completes without aborting: non-silent rows are
      normalized (output files exist, `success = TRUE`); silent rows carry
      `silent = TRUE`, `success = FALSE`, and no output file; a single warning
      names the silent rows. Verified by a live `skip_if`-guarded mixed-batch
      execution test.
- [ ] AC4 — A genuine batch analysis failure (non-zero FFmpeg exit, or a
      non-silence unparseable block) still aborts fail-fast naming the
      offending rows — silence handling does not weaken real-error detection.
      Verified by a pure test feeding simulated bad analysis outputs to the
      batch assembly path.
- [ ] AC5 — Silent-input behavior (scalar error; batch `silent` column and
      continue-and-mark) is documented in the `normalize_audio` /
      `normalize_audios` roxygen, `NEWS.md` records the change, and
      `devtools::check()` is clean (0 errors / 0 warnings / 0 notes) with
      `Status: OK` in `00check.log` (wordlist updated if new terms — M17
      lesson).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. -->

- AC1 → T1, T2, T4
- AC2 → T1, T2
- AC3 → T3, T4
- AC4 → T3
- AC5 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [ ] T1 — (tests-first, pure) Record a real-FFmpeg silence measurement
      fixture at `tests/testthat/fixtures/loudnorm-analysis-silent.txt`
      (`ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t 1 -af
      loudnorm=...:print_format=json -f null -`). Add failing unit tests in
      `tests/testthat/test-loudnorm-two-pass.R`: the new silence detector
      classifies the fixture as silent; `parse_loudnorm_measurements()`
      aborts with a silence-specific message on it; the existing generic
      "Could not parse" abort still fires on missing/non-numeric blocks (AC1,
      AC2).
- [ ] T2 — (impl) Add an internal silence detector in
      `R/loudnorm_two_pass.R` keyed on `input_i` being `-Inf` (via
      `is.infinite() && < 0`, distinguishing silence from `NaN`/missing).
      Route `parse_loudnorm_measurements()` so silence raises the
      silence-specific error and all other non-finite/missing cases keep the
      existing generic abort. Turns T1 green; scalar `normalize_audio(
      two_pass = TRUE)` now errors clearly on silence (AC1, AC2).
- [ ] T3 — (tests-first + impl, batch) Extend the batch two-pass assembly
      (`assemble_measured()` and the `normalize_audios` two-pass block,
      `R/ffmpeg.R:1395-1425`) so silent rows are set aside rather than
      aborting: run the correction only on non-silent rows, re-bind results
      into the full jobs table with a `silent` logical column, `success =
      FALSE` and no output for silent rows, and a single warning naming them;
      genuine (non-silence) failures still abort fail-fast. Tests-first: a
      mixed-batch assembly test and a simulated genuine-failure test (AC3,
      AC4).
- [ ] T4 — (live tests + docs) Add `skip_if`-guarded execution tests: scalar
      silence → silence error; mixed batch → non-silent row normalized +
      silent row marked, no abort. Document the silent-input behavior and the
      `silent` result column in the `normalize_audio` / `normalize_audios`
      roxygen (`@param two_pass`, `@return`); add a `NEWS.md` entry;
      `devtools::document()`; run `spelling::update_wordlist()` if needed;
      confirm `devtools::check()` clean and `Status: OK` (AC1, AC3, AC5).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (promoted from the M16-review
  silent-input candidate, scored 68).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
