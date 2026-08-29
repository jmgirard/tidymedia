# M084: LESSONS gets back under its budget, and its biggest family graduates

**Status:** done (2026-08-28, PR #88 https://github.com/jmgirard/tidymedia/pull/88)

**Goal:** Bring `cairn/LESSONS.md` under its 20,000-byte budget by graduating the false-green family whole into a doctrine module and trimming or retiring the rest.

**Outcome:** `cairn/LESSONS.md` went from 42,232 bytes over 49 lines to 19,860 over 33. Seventeen of its 44 entries — one family, "a test or control that reads green for the wrong reason" — **graduated whole** under the maturation exit into the new `cairn/references/false-greens.md` (53 lines / 24,959 bytes against a budget its own header states, < 60 / < 26,000), verbatim, under five themed headings. Six more were **trimmed to the remainder each still owns**: the `.aac`-muxer, otherwise-valid-grid and half-domain clauses moved into the module; the front-door-guard half dropped to `tests/testthat/test-builder-blame-front-door.R`, the blame-config half to `CLAUDE.md`, the timeout escalation figures to D056. The other 21 kept. The branch-point enumeration, per-entry byte lengths, `cut -c1-120` keys and exit classification are at `cairn/references/lessons-baseline-M084.md`. No change to the R package.

**Decisions:** the 44-entry classification (17 graduate / 6 trim / 21 keep) with its per-entry reasoning; `references/` over a new `cairn/doctrine/` directory; one module holding the family whole rather than two.

**Review:** one fresh-context reviewer (docs-only internal route). AC1–AC4 all passed on independently re-derived evidence; 14 findings, none a criterion failure or a shipped-behavior defect. Nine fixed on the branch — severed clauses and a dangling ordinal left by the trims, restoration of the M48-F1 converse limit and M45's reshaped-index fact, the module pointer in the `LESSONS.md` header, a stale candidate row. Findings 4 and 5 (both files clear their budgets by less than one average entry) became a candidate row; 9 and 14 rejected with reason; 8 discharged here.
