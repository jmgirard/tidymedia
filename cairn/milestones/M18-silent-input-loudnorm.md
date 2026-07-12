<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M18: Graceful silent-input handling for two-pass loudnorm

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M16, M17   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m18-silent-input-loudnorm · https://github.com/jmgirard/tidymedia/pull/20   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Give silent input its own honest handling in the two-pass loudnorm path: a
clear silence-specific error for the scalar verb, and continue-and-mark
(don't abort the batch) for the table sibling.

## Scope

**In:**
- Detect the digital-silence signature (`input_i = "-inf"`) as a distinct,
  recognized outcome — not a generic parse failure — via one internal helper
  shared by scalar and batch (no duplicated regex; D011 spirit).
- `normalize_audio(two_pass = TRUE)` on silent input: abort with a
  silence-specific message, replacing the misleading "Could not parse the
  `loudnorm` measurement" abort.
- `normalize_audios(two_pass = TRUE)` with silent rows: do **not** abort.
  Normalize the non-silent rows (`success = TRUE`), set silent rows aside
  (new `silent` logical column, `success = FALSE`, no output), and warn
  naming them. Mirrors `ffm_batch`'s record-and-continue idiom (D011).
- Preserve fail-fast for *genuine* failures (non-zero FFmpeg exit, or a
  non-silence missing/non-finite block): silence is the only condition
  promoted from "abort" to "continue-and-mark".

**Out:**
- Single-pass silence handling — single-pass never runs an analysis pass, so
  detecting silence there means adding one, defeating the purpose; stays as-is.
- Passthrough/stream-copy for silent input — rejected at the plan gate for a
  clear error (untouched-silence output weakens reproducibility). A decided
  design choice, not deferred.
- Near-silence / very-quiet inputs — legitimate finite `input_i`; normalized
  normally, no special handling.

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

- [x] T1 — (tests-first, pure) Record silence fixture
      `tests/testthat/fixtures/loudnorm-analysis-silent.txt`; add failing tests
      in `test-loudnorm-two-pass.R` for the silence detector + silence-specific
      abort, keeping the existing generic-parse abort (AC1, AC2).
- [x] T2 — (impl) Add `classify_loudnorm_output()` in `R/loudnorm_two_pass.R`
      keyed on `input_i = -Inf`; route `parse_loudnorm_measurements()` so
      silence raises its own error and other bad blocks keep the generic abort
      (AC1, AC2).
- [x] T3 — (tests-first + impl, batch) Set silent rows aside in
      `assemble_measured()` (returns `list(measured, silent)`); add pure
      `bind_two_pass_result()`; rewire the `normalize_audios` two-pass block to
      correct non-silent rows, warn, and emit a `silent` column; genuine
      failures still abort (AC3, AC4).
- [x] T4 — (live tests + docs) Add `make_silent_audio()` helper +
      `skip_if`-guarded scalar and mixed-batch execution tests; document silent
      behavior + `silent` column in both verbs' roxygen; `NEWS.md`;
      `document()`; wordlist; `devtools::check()` clean (AC1, AC3, AC5).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (promoted from the M16-review
  silent-input candidate, scored 68).
- 2026-07-12: set in-progress; branch m18-silent-input-loudnorm.
- 2026-07-12: T1/T2 done. Recorded silence fixture
  (`loudnorm-analysis-silent.txt`); added shared `classify_loudnorm_output()`
  (silent / unparseable / ok) keyed on `input_i = -Inf`; routed
  `parse_loudnorm_measurements()` so silence aborts with its own message and
  other bad blocks keep the generic parse abort. 16 parser tests pass.
- 2026-07-12: T3 done. `assemble_measured()` now returns `list(measured,
  silent)`, setting silent rows aside (measured NA) instead of aborting;
  genuine failures still abort. Added `bind_two_pass_result()` (pure row-order
  reassembly). `normalize_audios(two_pass=TRUE)` corrects only non-silent rows,
  warns naming silent ones, and always returns a `silent` column. Full suite
  725 pass.
- 2026-07-12: T4 done. Added `make_silent_audio()` helper + live tests (scalar
  silence errors; mixed batch marks silent row, normalizes the real one, no
  abort). Documented silent behavior in both verbs' roxygen + `silent` column
  in `@return`; NEWS entry; `document()`; added `loudnorm` to WORDLIST.
  `devtools::check()` 0/0/0, `Status: OK` in 00check.log. All tasks complete.
- 2026-07-12: review found 3 real defects (back to in-progress). (a) silent-row
  warning crashes with 2+ silent rows — cli `{?s}` governed by a `{.val
  {vector}}` across message elements throws `length(object) == 1`; (b) the
  `assemble_measured()` genuine-failure abort has the same `{cli::qty(bad)}`
  crash with 2+ bad rows (my T3 rewrite); (c) manifest row mismatch (scored 92)
  — `manifest = TRUE` with silent rows attaches a manifest covering only
  non-silent rows to the full-row result, breaking ffm_manifest()'s one-row-
  per-job contract (D011/M08). Live tests used a single silent/bad row so (a)/(b)
  slipped through. Excluded (scored 78, logged): all-silent + `verify=` omits the
  `verified` column while a mixed batch includes it.
- 2026-07-12: fixed all three defects. (a)+(b) rewrote both cli messages to
  drive pluralization off a scalar `{length()}` and list rows without a
  vector-governed `{?s}`; (c) added `expand_manifest_rows()` to pad the manifest
  back to full row order in `bind_two_pass_result()`. Added regression tests
  (2-bad-row clean abort; manifest one-row-per-job; live 2-silent-row batch with
  manifest). Full suite 752 pass. Back to review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
