# M074: The floor says what was measured

**Status:** done (2026-08-27, PR #78 https://github.com/jmgirard/tidymedia/pull/78)

**Goal:** Establish, by measurement rather than assumption, whether `local_timeout()` behaves as documented on the oldest `withr` DESCRIPTION permits, and make the floor state what was measured.

**Outcome:** `DESCRIPTION`'s `withr (>= 2.5.0)` is unchanged and now measured. `data-raw/withr-floor.R` (developer-only, `.Rbuildignore`
`^data-raw$`) installs a version into its own library and drives eight child sessions per version, each asserting the `withr` it loaded came
FROM that library, not merely that the version string matches; a failing block or non-zero child status stops the run. On 2.5.0 and 3.0.3
alike: all 35 `test_that()` blocks of `test-local-timeout.R` and `test-with-timeout.R` pass, the four documented `@details` claims read as
written, the `withr::defer()`/`local_options()`/`with_options()` comparisons the docs make hold, and both named top-level forms agree. One
difference found — `source(file, local = TRUE)` from a function frame, `30` on 2.5.0 against `99` on 3.0.3, which no `@details` claim covers.
withr 3.0.0's rewritten `globalenv()` branch is reached from the `Rscript` form only; under `source()` both versions redirect to `source()`'s
own frame first. `local_timeout()`'s help page names both versions; `NEWS.md` states what ran and what did not.

**Decisions:** D053 (extends D052, leaving it standing) — the floor stays 2.5.0 because it was measured, with what was and was not measured, and its falsifier.

**Review:** Four rounds; three defect returns, all in one slot — an acceptance criterion binding `NEWS.md` to state what was measured, which
failed by overclaim, then overclaim plus omission, then an understatement that misplaced four passing blocks. The thrash rule fired trigger (b)
at round 2 (the recorded alternative, widening AC1's test domain, was put to the user and held) and trigger (a) at round 3, where the user chose
descope over park, retry or escalation: the NEWS-accuracy half exited to the `Imports`-floors candidate row and the milestone narrowed to its
four verified criteria. Round 3 also failed the `weight caps` gate at 157 plan-owned lines, shed by compressing Tasks. Round 4: four criteria
met, gate clean, three lenses, eight findings — six fixed at the gate (the per-spawned-program coverage sentence in NEWS and D053 cited a
process-lifetime block and a claim no test writes; three NEWS enumerations tightened and dated; the harness header named its load-bearing pin
control), two rejected. The measurement was never in question in any round.
