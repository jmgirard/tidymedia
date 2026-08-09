# M66: A batch value refusal names the offending row

**Status:** done (2026-08-08, PR #69 https://github.com/jmgirard/tidymedia/pull/69)

**Goal:** Every `_batch` front-door per-row refusal of a jobs-column value
names the caller's 1-indexed row, via one appended first-offender bullet.

**Outcome:** `check_batch_cell()` (R/utils.R) catches a per-row refusal and
re-raises it with "First offending jobs row: N." appended — head, class, and
blame preserved; an NA row is the argument-delivered pass-through (no
locator). Threaded at 24 swept sites across 12 verbs plus the codec/vocab
column helpers. Instruments: site list derived by data-raw/m66-derive-sites.py
(recorded grep + callee closure; 300 sites triaged, drift-refusing --check),
51-cell test-row-locator-grid.R, two-direction-verified strip_row_locator(),
blame-guard-mutations-m66.py (each row-index pass → 1L, 24/24 owned),
committed two-ref and instrument evidence runners.

**Decisions:** M66-D1 — compare's audio/resize locators gate on their OWN
column's presence ("row-dependent via inputs" falsified by the uniform table).
normalize's two-pass token wrapper: excluded-backstop, shadowed, reds nothing.

**Review:** One floor return — F1 (90) argument-delivered locators on compare,
F2 (92) their complement cells missing — both fixed. Fifteen logged 12–72,
five fixed anyway (bounded harness regex, evidence runners, wrapper guards).
Nothing retired.
