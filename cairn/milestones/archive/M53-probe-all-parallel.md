# M53: Give `probe_all()` a `parallel =` argument

**Status:** done (2026-08-06, PR #56 https://github.com/jmgirard/tidymedia/pull/56)

**Goal:** Let `probe_all()` fan its per-file probes out across workers, so a large corpus is
bounded by the active future plan rather than by a `for` loop.

**Outcome:** `probe_all()` and the four `probe_*()` shortcuts take `parallel = FALSE`; `TRUE` maps
`probe_one()` through `furrr::future_map()` under the active `future::plan()`, `FALSE` through
`purrr::map()`. Only `probe_one()` fans out — the `failed` accumulator and the single end-of-call
`cli_warn()` stay in the parent, so the one-warning contract survives, and assembly from
`probes[[i]]` keeps rows in INPUT order. `check_installed("furrr")` and `warn_if_sequential_plan()`
fire on the parallel branch only: `probe_all()` is a terminal entry point, so unlike loudnorm's
Phase 1 it emits that guard itself. `resolve_probe()` gained `parallel`, consumed where `typed` is.

**Decisions:** D033 — furrr fan-out crosses execution → metadata; D007's single-runner rule is not
violated, and the entry states what may not follow. None milestone-local. One gated amendment: AC6's
"two places rather than one" was false before the milestone began (three pre-existing execution-side
sites), re-cut to record the side-crossing and name the enumerating grep.

**Review:** Passed first round; 18 findings, two at or above 80. F1 (88), five tests taking the real
parallel path without `skip_if_not_installed("furrr")`, fixed and proven load-bearing (ambient
`plan(multisession)`: 5 errors pre-fix, 0 post-fix). F5 (85), `load_all()` plus a real plan cannot
resolve the non-exported `probe_one` in a worker — documented as a lesson, not coded around, after
measuring the installed package correct under `plan(multisession, workers = 3)`. F7/F9 → candidates.
