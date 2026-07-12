# M18: Graceful silent-input handling for two-pass loudnorm

- **Status:** done · **Depends on:** M16, M17 · **PR:** https://github.com/jmgirard/tidymedia/pull/20

## Goal

Give silent input honest handling in the two-pass loudnorm path: a clear
silence-specific error for the scalar verb, continue-and-mark for the batch.

## Outcome

Digital silence (`input_i = "-inf"`) is now a recognized outcome, not a parse
failure. Shared `classify_loudnorm_output()` (silent/unparseable/ok) drives both:
`normalize_audio(two_pass=TRUE)` aborts with a silence-specific message;
`normalize_audios(two_pass=TRUE)` normalizes the non-silent rows and marks silent
rows in an always-present `silent` column (`success=FALSE`, no output) with a
warning, via new pure `bind_two_pass_result()`. Genuine failures still fail fast.
`assemble_measured()` return changed to `list(measured, silent)`. Single-pass
byte-for-byte unchanged (D013). check 0/0/0; 752 tests pass.

Independent review found + fixed three defects live tests missed (single-row
only): manifest row mismatch (score 92 → `expand_manifest_rows()` pads to
one-row-per-job, D011) and two cli-pluralization crashes with 2+ rows (score-surfaced).
Below-threshold all-silent + `verify=`/`manifest=` edge (score 78) → ROADMAP candidate.
