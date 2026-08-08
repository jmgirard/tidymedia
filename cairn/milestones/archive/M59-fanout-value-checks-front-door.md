# M59: Six per-row value checks are refused at the fan-out verb's front door

**Status:** done (2026-08-08, PR #62 https://github.com/jmgirard/tidymedia/pull/62)

**Goal:** Make six per-row value validations report from the fan-out verb the user called, not from inside the fan-out.

**Outcome:** Front-door sweeps on four `_batch` verbs, rows resolved through
`batch_arg_rows()`: `check_dim()` for crop `width`/`height`, `check_regions()`
per `regions` cell, `check_number_whole()` for pip `margin` and compare's
per-row `audio` index against that row's own inputs, and a new
`check_vocab_arg()` / `check_batch_vocab_col()` pair for the `direction` and
`position` column VALUES, never checked before. Two unreachable closure
re-checks retire; vocabularies single-sourced into `stack_directions()` /
`pip_positions()` for CHECKING only. Evidence: `data-raw/value-guard-baseline.R`
(38 cells, 0 refusals changed, 17 blame moves), `value-guard-mutations.py` (10/10).

**Decisions:** D037, D038 (cross-cutting). Local: M59-D1 (crop calls
`check_dim()` directly over threading `call` through exported `ffm_crop()`),
M59-D2 (the six abort sites), M59-D3 (single-sourcing is for checking, not
display; supersedes D2's vocabulary paragraph).

**Review:** Four passes, three defect returns, every one on a *description* not
behavior: `arg_match0()` leaking its own call, AC5(a) unbounded over form, help
pages and `formals()`, two successive false precedence sentences, stale
references. AC5(a) re-cut out to M61. 31 logged; extended M51's lesson.
