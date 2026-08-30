# M089: The guard-ordering family graduates and the tracking files get headroom

**Status:** done (2026-08-29, PR #93 https://github.com/jmgirard/tidymedia/pull/93)

**Goal:** `LESSONS.md` and `ROADMAP.md` each clear their byte budget by more than
one average entry, by graduating the front-door guard-ordering family into its
own doctrine module rather than raising a figure this repo does not own.

**Outcome:** The 2026-07-29 (M41) and 2026-07-30 (M47) guard-ordering entries —
3,219 bytes over two lines — left `LESSONS.md` byte-identical for the repo's
second doctrine module, `cairn/references/guard-ordering.md`, whose own header
budgets it at under 11,000 bytes over under 31 lines, room for about three more
members. `LESSONS.md` went 30 lines / 19,979 bytes → 28 / 16,835; `ROADMAP.md`
48 / 23,989 → 44 / 21,981, by absorbing the budget and second-doctrine-module
rows and grouping the M31 encoder-surface trio and the M67 memoization pair.

**Decisions:** D067 — two of the three tracked-file budgets are the cairn
plugin's and one is repo-owned; each got the remedy its own owner states.

**Review:** Single [O] diff-bug lens (internal tier, docs-only diff), run inline
under this session's no-unrequested-subagents instruction. Eight of eight
criteria verified; `devtools::check()` `Status: OK`; `cairn_validate` clean; ten
CI green. Two findings, neither a criterion failure: a 7-byte stale figure in
D067, fixed before merge; the absorbed budget row's live condition
(`false-greens.md`, 190 bytes of headroom) filed as a successor candidate row.
