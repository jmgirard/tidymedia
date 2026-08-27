# M073: The timeout wrapper's tail

**Status:** done (2026-08-27, PR #77 https://github.com/jmgirard/tidymedia/pull/77)

**Goal:** Guard `with_timeout()`'s two arguments evenly, add the `local_*` half of the pair, and stop the FIFO test helper leaving a process behind.

**Outcome:** `local_timeout(seconds, .local_envir = parent.frame())` is exported — the statement
form of the same seam, establishing `tidymedia.timeout` to the end of the frame it binds to and
restoring the caller's prior state on every exit, over `withr::defer()` for its LIFO ordering. It
reads the prior, registers the undo, then writes the limit, so a failed registration leaves the
session as found. `with_timeout()` gains `rlang::check_required(expr)` above the option write, so
both formals refuse alike. `tm_release_fifo()` polls a per-call cancel file `withr::defer()` touches
at frame exit and exits when the session's tempdir goes; `tm_pgrep()` queries a `shQuote()`d
`tm_[f]ifo_` ERE so the querying shell cannot match itself. `withr` moved Suggests → Imports.

**Decisions:** D052 (extends D051) — the limit may also be set as a statement, and `withr` becomes a
hard dependency.

**Review:** Two passes. The first returned it (defect return 1), AC3/AC8 red on all three Linux jobs:
`tm_pgrep()`'s own `sh -c` matched the marker, so on dash the query could never return empty;
confirmed on a Linux runner before fixing. Second pass: eight criteria and nine checks green; three
lenses, ten findings, all from the diff lens. Six fixed at the gate (the option-before-undo leak, two
false undo-loss claims, the unquoted `pgrep` pattern, the unpinned unset-prior return, the pkgdown
blurb, the missing package-level mention), five rejected, AC3's wording deviation recorded not amended,
`withr`'s floor filed as a candidate.
