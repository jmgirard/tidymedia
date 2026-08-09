# M66: A batch value refusal names the offending row

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m66-batch-row-locator

## Goal

Every `_batch` front-door per-row refusal of a jobs-column value names the
caller's 1-indexed row, via one first-offender locator bullet appended to an
otherwise-unchanged message.

## Scope

**In:** The per-row value and contradiction sweeps reachable from the 15
`_batch` verbs' front doors (site list derived by AC1's procedure — the M58
contradiction, M59 vocab/margin/audio, M64 dim/token/fps, and M65
region/scale/loudness families, plus the shared per-cell column helpers
`check_batch_codec_col()` / `check_batch_vocab_col()` / `check_batch_string_col()`
/ `check_batch_audio_col()`); one shared locator mechanism written once; the
blame grids, locator-remover, and mutation harness extended to carry it.

**Out:** The normalize pair's copy/`audio_stream` form divergence → its
candidate ROADMAP row (part (b) of the row this milestone absorbs). Input-path
sweeps — a path is its own locator; their attribution gaps stay with the
M62/M63 leftovers candidate row. Column *type*/NA guards, which name the
column — the type-vs-value boundary candidate row (M61 H11). Unifying the two
row-locator conventions (all-offenders at `check_batch_stream_values()`,
first-offender here) — not attempted; disclosed in the triage.

## Acceptance criteria

- [ ] AC1. Every column-delivered per-row value or contradiction refusal on
      the committed site list names the caller's 1-indexed jobs row. The site
      list is produced by enumerating every abort site reachable from each of
      the 15 `_batch` verbs' front doors before `ffm_batch()` — the recorded
      grep `cli_abort\(|abort\(|stop\(|stopifnot\(|match\.arg\(|check_[a-z_]+\(|arg_match|check_number_`
      over each front-door body and its callee closure, expanded to fixpoint
      and terminating at `ffm_batch()` and at exported `ffm_*` builders
      (Layer-1 blame by D042, excluded by rule), the function list committed
      beside the site list — triaging every hit as swept, already-located, or
      excluded with a stated reason; each swept site has a grid cell placing
      the bad value in a jobs column at a row other than 1 and asserting the
      refusal names that row.
- [ ] AC2. Each swept refusal is its merge-base refusal plus one
      first-offender locator bullet: per grid cell, removing the locator from
      the branch message yields the cell's merge-base message byte-for-byte.
      The locator wording is written at one new site, names the first
      offending row only, drives no `{?s}` pluralization off a vector,
      contains no substring "index", and matches none of the precedence
      instruments' marker patterns (the enumerated marker list looped against
      a rendered locator). The two already-located
      `check_batch_stream_values()` sites are triaged as such, wording
      byte-unchanged, the two-convention asymmetry disclosed in the triage.
      An argument-delivered batch refusal carries no locator and stays
      byte-identical to its merge-base message. On
      `separate_audio_video_batch`, whose swept contradiction site sweeps
      `jobs` rather than the reshaped `long` table, a grid cell places the
      bad cell at caller row 2 (reshaped index 3) and asserts the message
      names 2 and never 3.
- [ ] AC3. Nothing else moves: across the merge-base and the branch, every
      scalar-form grid cell's message is byte-identical, every cell (scalar
      and batch) blames the verb the user called with no `pmap` or
      `In index:` leak, and the precedence instruments re-run with zero
      flips — no call changes which error reports first.
- [ ] AC4. The instruments stay honest: the cross-form equality test compares
      scalar and batch messages after a locator-remover verified in both
      directions — two messages differing only in the row number compare
      equal, and two differing outside the locator (in the sentence preceding
      it, and in the last pre-existing bullet) compare different; and the
      mutation ledger, derived from the committed triage which the harness
      re-derives at mutation time (re-running AC1's grep + closure, failing
      on any difference), mutates each swept site's row-index pass to the
      constant `1L`, each mutation caught red by that site's
      row-other-than-1 grid cell.
- [ ] AC5. The r-package profile's verify slot clean: `devtools::test()` all
      green and `devtools::check()` 0 errors / 0 warnings.

## Coverage

- AC1 → T1, T3, T4, T5
- AC2 → T2, T3, T4, T5
- AC3 → T7
- AC4 → T5, T6
- AC5 → T7

## Tasks

- [x] T1. Derive and commit the site list: run AC1's recorded grep + callee
      closure over the 15 front doors (`R/ffmpeg.R`); triage every hit as
      swept / already-located / excluded-with-reason; disclose the
      two-convention asymmetry. Known sites from planning: 5 loops with `i`
      bound unused (3304, 3659, 4328, 6272/6310, 6507), 6 value-loops needing
      index conversion (3930, 3934, 5244–5252, 6538, 6549, 6562), 2 needing
      rework (1992 regions, 4607 codec col).
- [x] T2. Write the locator mechanism at one new site — catch a per-row
      refusal and re-abort with the row bullet appended, preserving message,
      class, and blamed call (the rlang-owned wordings of
      `check_number_decimal()`/`arg_match()` admit no suffix parameter, so
      the wrapper is the uniform mechanism) — with unit tests and the
      marker-list check of AC2.
- [x] T3. Thread it through the value-checker sweeps (M64/M65 families),
      converting value-loops to index loops.
- [x] T4. Thread it through the M58/M59 contradiction and rlang sweeps and
      the shared per-cell column helpers (index-preserving `which()` rework
      at `check_batch_codec_col()`).
- [ ] T5. Extend the blame grids/spec lists with a row-≥-2 cell per swept
      site (incl. the reshape-discriminating `separate_audio_video_batch`
      cell); add the locator-remover with both verification directions; amend
      the cross-form equality test (`test-builder-blame-front-door.R:237`) to
      compare post-removal.
- [ ] T6. Extend the mutation harness: re-derive the site set at run time,
      compare against the committed triage, mutate each row-index pass to
      `1L`, attribute each red.
- [ ] T7. Re-run baseline + precedence instruments at both refs (zero flips,
      scalar byte-identity, no leak); NEWS entry narrowed to what the grid
      enforces; profile verify clean.

## Work log

- 2026-08-08: created by /milestone-plan from the M65-leftovers candidate row
  (F10, scored 80); part (b) of that row stays behind as its own candidate.
- 2026-08-08: criteria audit round 1 returned 8 findings (unbounded universal;
  arg-delivered cells unsatisfiable; marker-match proxy evidence; an
  already-true clause; mutation grain; one-directional remover; classifier
  fed by the change; one-site conflict with the existing locator) — six fixed
  in the wording, two routed to the gate. Round 2 over the amended wording
  returned 4 (pre-reshape premise false; grep pattern missing base-R aborts;
  callee closure underivable; triage drift) — all fixed in the wording.
- 2026-08-08: plan gate chose the full nine-verb sweep over the candidate
  row's three checkers because one mechanism covers all and a narrower cut
  re-ships the same complaint; falsified by a swept family whose refusal
  cannot share the mechanism.
- 2026-08-08: plan gate chose a new one-site locator over unifying on
  `check_batch_stream_values()`'s wording because unification re-pins the
  multitrack tests and its collect-all shape does not fit abort-at-first-cell
  sweeps; falsified by a report of the two conventions confusing a caller.
- 2026-08-08: plan gate chose first-offender over all-offenders-per-column
  because it preserves stopping behavior and message-head identity; falsified
  by a report of serial re-runs on a large table to find successive bad rows.
- 2026-08-08: plan chose a catch-and-reabort wrapper over threading a row
  parameter into the shared checkers because the rlang-owned wordings admit
  no suffix; falsified by a wrapper unable to preserve a refusal's class or
  blamed call.
- 2026-08-08: implement started; branch m66-batch-row-locator; step-3 gate
  skipped (nothing open: plan gate settled scope/wording/semantics; triage
  home data-raw/ per the M65 spec-list lesson, derivation executable per AC4).
- 2026-08-08: T1 done — 274 abort sites derived (data-raw/m66-derive-sites.py;
  --check gates triage sync); triage: 27 swept, 19 swept-helper, 5
  already-located, 223 excluded by class (data-raw/m66-site-triage.tsv). The
  procedure surfaced two swept sites no prior map had: extract_frame_batch's
  timestamp-finite (ffmpeg.R:3435) and frame-whole (3447) column value checks.
- 2026-08-08: T2 done — check_batch_cell() (R/utils.R) appends the plain-text
  first-offender bullet to the thrown condition's body and re-raises: head,
  class, and blamed call byte-preserved (measured); strip_row_locator()
  (helper-blame.R) verified both directions; 17 unit tests; suite 0 fail.
- 2026-08-08: T3 done — locator threaded through the M64/M65 value sweeps
  (crop dims, standardize dims/pixel_format, sample rate loop, anonymize
  regions, pip scale, normalize loudnorm/copy-column/two-pass token and
  channels checks); NA row = argument-delivered pass-through, so locators fire
  only for column-delivered values (measured, 9 sites). The cross-form
  equality test now compares after strip_row_locator() and asserts each batch
  column cell carries the locator and every other cell does not (T5 part);
  suite 0 fail, 5637 pass.
- 2026-08-08: T4 done — locator on the M58/M59 sweeps (segment reencode pair,
  compare needs-audio/resize/audio-bound, pip needs-audio/margin/audio,
  separate hardware) and the shared helpers (vocab col; codec col token loop
  reworked to which(); extract_frame finite/whole checks). compare's
  audio-bound and resize locators are unconditional: the per-row input count
  makes those refusals row-dependent even argument-delivered (measured).
  Suite 0 fail, 5637 pass.

## Decisions

## Review
