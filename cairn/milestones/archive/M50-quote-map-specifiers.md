# M50: Quote map specifiers in the compiled command string

**Status:** done (2026-07-31, PR #53 https://github.com/jmgirard/tidymedia/pull/53)

**Goal:** Make the compiled command string paste-safe by quoting every map
specifier, so M47's `-map 0:v?` stops failing when pasted into a shell.

**Outcome:** Both `-map` branches of `ffm_groups()` now pass `quote = 2L`, so every
specifier renders double-quoted — the simple branch and the complex branch's
explicit-map line, beside the auto `[vout]` map already quoted since M06. Quoting
lives in each group's `display`; `ffm_run()` executes `ffm_args()`, proved untouched
by a fourteen-pipeline args snapshot recorded before the change. `-map` literals
across 19 test files and three `_snaps/ffm.md` lines were re-baselined, `README.md`
re-knitted, and `ffm_copy()`'s `@param streams` prose de-staled.

**Decisions:** D031 — which token classes the display string quotes, which stay
bare and why, `quote=` as a positional index and not a level, and why execution
(`shQuote` via `system2`) cannot see it. The bare classes became a candidate row.

**Review:** Three lenses, 14 findings, one at or above 80: F1 (92), a "fifteen
pipelines" count that was fourteen in D031 and four other places. Six verified
sub-80 findings actioned (F7/F9/F4/F6/F14/F13); seven logged. Nothing retired.
