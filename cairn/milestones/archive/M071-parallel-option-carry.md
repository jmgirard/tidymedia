# M071: A parallel worker sees the settings the caller set

**Status:** done (2026-08-27, PR #75 https://github.com/jmgirard/tidymedia/pull/75)

**Goal:** A parallel worker runs under the `tidymedia` settings the caller set.

**Outcome:** `carry_options()` / `carried_option_values()` in `R/timeout.R`: a
parent-captured wrapper installing the caller's resolved `tidymedia.timeout`
and `tidymedia.nvenc_encoders` in the worker for the mapped call, restoring
that worker's prior values via `on.exit(options(prior))` on returning and on
error alike. Wired at all four `furrr::future_*` sites (`R/ffm_batch.R` build
and run, `R/ffprobe.R`, `R/loudnorm_two_pass.R`), parallel branches only, each
threading `call`. `ffm_batch()` resolves the limit in its validation block, so
a bad one is refused before dispatch, not as a bare per-row `success = FALSE`.
M70's batch warning is reachable at `parallel = TRUE` now; the memo is not.

**Decisions:** D050 (supersedes D047's "Disclosed, not fixed" bullet and D044's
seeding rejection; the resolved limit's no-limit sentinel is the one value here
the package chooses rather than the caller).

**Review:** Three-lens fan-out; both [S] lenses clean, [O] returned ten. Five
fixed at the gate, the load-bearing two being the carrier authoring
`tidymedia.timeout = 0` against D050's own wording and a refusal naming
`probe_all_impl()` against M64/M65's blame rule. CI then reddened on
`test-coverage` alone. Five instrument findings went to a candidate row.
