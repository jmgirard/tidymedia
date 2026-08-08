# M61: A value error and a contradiction resolve the same way in both forms

**Status:** done (2026-08-08, PR #64 https://github.com/jmgirard/tidymedia/pull/64)

**Goal:** On `compare_videos_batch()` and `picture_in_picture_batch()`, make a value error and an argument contradiction resolve identically whether the value arrived as an argument or in a `jobs` column.

**Outcome:** Four front-door value guards — `direction` (compare), `position` and `margin` (pip), and the per-row `audio` bound (both) — moved below their verb's M58 contradiction sweep in the two `_batch` verbs and both `*_pipeline()` functions, so both forms now answer alike. `picture_in_picture_batch()` gained a front-door per-row `audio` sweep and retired the fan-out closure's copy, which had reported against `purrr::pmap()` naming the local `aud`. The shared pipeline carries the same reordering to the scalar `compare_videos()` / `picture_in_picture()`. No refusal changed; 14 cells changed which error they report, all in NEWS. `data-raw/value-guard-baseline.R` grew the ordering dimension and now GENERATES its crossings from declared (verb, value) pairs x forms x per-verb crossings, with `value_guard_uncovered()` reporting any combination with no cell; `tests/testthat/test-front-door-ordering.R` and `helper-blame.R` are new.

**Decisions:** M61-D1 (the four-guard set closed by inspection at `1d54b20`, the displaced errors, what stays above and why); D039 promoted, superseding D038 and restoring D036 unconditionally.

**Review:** Four rounds. Round 1 an amendment return on AC1 (a cell recorded impossible was reachable at `audio = NA`); rounds 2 and 3 defect returns on AC2, each a missing hand-written crossing, firing thrash trigger (b) — remedied by generating the product rather than listing it. Round 4: 17 findings, none at threshold; F3 (76) and F4 (68) fixed at the user's direction at the gate, both verified by mutation. One candidate row added (a wrongly-typed value still answers by form). M41's grid lesson extended with this milestone's third instance.
