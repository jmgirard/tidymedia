# M108: `install_on_win()` refuses on a platform it cannot install for, before it spends anything

**Status:** done (2026-09-05, PR #112 https://github.com/jmgirard/tidymedia/pull/112)

**Goal:** Make D084's claim that Windows is the only platform `install_on_win()`
runs on true at its top, and point a caller elsewhere at the route they have.

**Outcome:** `tm_os()` (`R/program_management.R`) returns the running host
lowercased from `Sys.info()[["sysname"]]`, falling back to `.Platform$OS.type`;
both are arguments, so the fallback is test-reachable. An allow-list gate in
`install_on_win()`, below the four argument checks and above every cost (the
unverified-source `cli_inform()`, `tm_confirm()`, `dir.create()`, the first
`tm_fetch()`), aborts `tidymedia_wrong_platform` carrying `tm_platform` (D062),
naming the platform, its `tm_install_routes` route where it has one, and
`set_program()`. `test-install-platform.R` skips nothing, so every CI runner
asserts its own verdict.

**Decisions:** **D086** — the one-platform surface, the gate's siting, the
allow-list, and why the seam carries an unmocked test.

**Review:** Two passes, full three-lens fan-out each (tier user-facing). Pass 1,
nine diff-lens findings: O1/O2/O4/O5/O6/O9 fixed, O7/O8 rejected, O3 the one
amendment return — AC5 pinned `@return`'s count at seven, and T7 rewrote it to
eight, the addition being `tidymedia_confirmation_unavailable`. Pass 2: P1 and
P6 fixed at the gate, P2 a candidate row, P4/P5/P7 rejected. Defect returns 0.
