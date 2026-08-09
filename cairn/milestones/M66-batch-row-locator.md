# M66: A batch value refusal names the offending row

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m66-batch-row-locator · https://github.com/jmgirard/tidymedia/pull/69

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

- [x] AC1. Every column-delivered per-row value or contradiction refusal on
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
- [x] AC2. Each swept refusal is its merge-base refusal plus one
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
- [x] AC3. Nothing else moves: across the merge-base and the branch, every
      scalar-form grid cell's message is byte-identical, every cell (scalar
      and batch) blames the verb the user called with no `pmap` or
      `In index:` leak, and the precedence instruments re-run with zero
      flips — no call changes which error reports first.
- [x] AC4. The instruments stay honest: the cross-form equality test compares
      scalar and batch messages after a locator-remover verified in both
      directions — two messages differing only in the row number compare
      equal, and two differing outside the locator (in the sentence preceding
      it, and in the last pre-existing bullet) compare different; and the
      mutation ledger, derived from the committed triage which the harness
      re-derives at mutation time (re-running AC1's grep + closure, failing
      on any difference), mutates each swept site's row-index pass to the
      constant `1L`, each mutation caught red by that site's
      row-other-than-1 grid cell.
- [x] AC5. The r-package profile's verify slot clean: `devtools::test()` all
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
- [x] T5. Extend the blame grids/spec lists with a row-≥-2 cell per swept
      site (incl. the reshape-discriminating `separate_audio_video_batch`
      cell); add the locator-remover with both verification directions; amend
      the cross-form equality test (`test-builder-blame-front-door.R:237`) to
      compare post-removal.
- [x] T6. Extend the mutation harness: re-derive the site set at run time,
      compare against the committed triage, mutate each row-index pass to
      `1L`, attribute each red.
- [x] T7. Re-run baseline + precedence instruments at both refs (zero flips,
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
- 2026-08-08: T5 done — test-row-locator-grid.R: 47 cells (42 locator incl.
  the reshape-discriminating separate cell asserting row 2 and never 3, and
  17 codec-column cells across the calling verbs; 5 argument-delivered
  complements asserting NO locator), each pinning the guard's own marker
  before reading the locator; 237 assertions green. Cell authoring caught
  one semantics fact: an NA audio_codec cell under reencode=FALSE is itself
  the contradiction, so its clean row must carry "copy".
- 2026-08-08: T6 done — triage regenerated against the wrapped tree (300
  sites, --check green; a first join inherited by first-match and flattened 5
  duplicate-path aborts into shape guards — caught by diffing dispositions,
  fixed by ordered consumption). blame-guard-mutations-m66.py re-derives the
  site set, refuses drift, mutates each row-index pass to 1L: 24/24 owned
  reds. Its first run measured normalize's two-pass token wrapper reds
  nothing — shadowed by check_batch_codec_col's earlier locator loop — so
  that site is triaged excluded-backstop with that reason, and the grid keeps
  its behavior cell (the locator the user sees comes from the codec-col loop).
- 2026-08-08: minor amendment (T7 prep) — blame_message_drift()'s strip
  gains the locator pattern beside its In-index strip: an added bullet is
  not a rewording; same regex as the verified strip_row_locator().
- 2026-08-08: T7 done — instruments at master vs tree: blame baseline 0
  vacuous / 0 moves / 0 drift, 10/10 scalar cells byte-identical; M64 + M65
  precedence: 0 dead controls, 0 unresolved, 0 flips; 48 markers enumerated,
  0 match the rendered locator. NEWS bullet added (claims scoped to what
  test-row-locator-grid.R enforces). devtools::test 0 fail; devtools::check
  0 errors / 0 warnings / 0 notes. Status → review.
- 2026-08-08: review return 1 (floor, defect-return count 1): AC2's complement
  demonstrated failing on compare_videos_batch (F1 90, F2 92) — argument-
  delivered audio/resize refusals carried a locator naming a row where
  nothing is distinct. Fix: both locators now gate on their own column's
  presence (M66-D1); grid resize cell moved to column delivery; the two
  missing complement cells added. Sub-threshold fixes taken with it, each
  logged: F9 72 (harness mutation regex bounded to its line), F12/F13 72/72
  (two-ref and instrument evidence runners committed to data-raw/), F3 65 +
  F6 35 (wrapper materializes a function body before appending; malformed
  row degrades to pass-through). Status → in-progress for the fix;
  re-verification battery re-running.

## Decisions

- 2026-08-08 M66-D1: `compare_videos_batch`'s audio-bound and resize locators
  gate on their OWN column's presence, like every other site — the T4 claim
  that the per-row input count makes those refusals row-dependent even when
  argument-delivered was falsified by the uniform-table case (review F1,
  scored 90): with a scalar `audio`/`resize` every row offends alike and
  "row 1" sends the caller to a row where nothing is distinct. A scalar
  value against a RAGGED `inputs` column now carries no locator either —
  the disclosed cost of the uniform rule, revisit on the first report of a
  caller needing it.

## Review

Fresh evidence, 2026-08-08, branch m66-batch-row-locator @ PR #69:

- AC1: `python3 data-raw/m66-derive-sites.py --check` → "triage in sync: 300
  sites" (exit 0), triage committed with every hit dispositioned;
  test-row-locator-grid.R green in the fresh suite run and the harness's
  OWNER map covers all 24 swept wrapper sites with a row-≠-1 cell each (one
  wrapper triaged excluded-backstop with stated reason, measured redding
  nothing). Ticked.
- AC2: two-ref runner over all 49 grid cells at master vs branch: 49/49
  abort at both refs; 44/44 locator cells strip to the master message
  byte-for-byte; 5/5 argument-delivered complements byte-identical
  unstripped. Locator written once (check_batch_cell, R/utils.R); wording
  contains no "index", no vector pluralization (unit tests); 48 instrument
  markers enumerated, 0 match the rendered locator; reshape cell asserts
  row 2 present and row 3 absent. Ticked.
- AC3: blame baseline master→branch: 0 vacuous, 0 blame moves, 0 message
  drift; 10/10 scalar cells byte-identical raw; M64 and M65 precedence
  instruments both: 0 dead controls, 0 unresolved, 0 flips; grid asserts no
  `pmap`/`In index:` leak per cell. Ticked.
- AC4: strip_row_locator verified both directions (test-check-batch-cell.R,
  fresh suite green); blame-guard-mutations-m66.py re-derives the site set
  (refuses drift), mutates each of 24 row-index passes to 1L: 24/24 owned
  reds on a clean tree. (An earlier same-session run crashed — the
  blame-history reviewer had invoked the harness concurrently, each
  mutating R/ffmpeg.R under the other; verified as interference, rerun
  clean.) Ticked.
- AC5: devtools::test fresh: 0 fail / 5884 pass (4 warns are the suite's
  intentional warning paths, 5 skips binary-gated); devtools::check on this
  code tree: 0 errors / 0 warnings / 0 notes. Ticked.

Consistency gate: cairn_validate exit 0 (advisory work-log wrap WARNs only);
document() no diff; README newer than Rmd; pkgdown check_pkgdown clean; NEWS
entry present, no milestone numbers; no new top-level files needing
.Rbuildignore (data-raw covered). Driving RR: — (no-op).
