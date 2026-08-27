# M072: One call can carry its own time limit

**Status:** done (2026-08-27, PR #76 https://github.com/jmgirard/tidymedia/pull/76)

**Goal:** Let a caller bound one call's wall-clock time without changing the session's limit.

**Outcome:** `with_timeout(expr, seconds)` is exported from `R/timeout.R`: it applies
`resolve_timeout()`'s own `check_number_whole()` to `seconds` eagerly, then
`prior <- options(tidymedia.timeout = ...)` with `on.exit(options(prior), add = TRUE)`, and forces
the `expr` promise once in the caller's frame. The option is process-global, so all four spawn
sites (`ffmpeg`, `ffprobe`, `mediainfo`, `run_program`), `ffm_batch()`'s up-front
`resolve_timeout()` and D050's parallel carrier read the per-call value with nothing threaded and no
signature changed; an unset option is restored unset. Tests add `tm_spawn_sites()` to `helper-timeout-sweep.R` with a set-equality drift test, a
FIFO-anchored real-kill cell carrying its own outer bound, and three timed parallel cells under
`local_carry_harness()`. Docs: roxygen topic, `_pkgdown.yml` "Bounding a run", NEWS, landing topic.

**Decisions:** D051 — the per-call grain; discharges D047's session-grain falsifier clause while
leaving its rejection of per-verb `timeout =` arguments standing, and records the code-first
argument order and the name's place outside D014's families as the irreversible halves.

**Review:** Three-lens fan-out; blame-history and prior-PR-comments lenses found nothing. Diff-bug
lens: nine, ranked. F1 (help text citing a vignette section that does not exist), F2 (a comment
overstating the `seconds`/option equivalence, false for `NULL`) and F9 (D014 silent on the name)
fixed on the branch; F3 (a missing `expr` gives a base-R error) and F6 (an unreaped 90 s shell in
the FIFO cell) deferred to a candidate row; F4, F5, F7, F8 rejected.
