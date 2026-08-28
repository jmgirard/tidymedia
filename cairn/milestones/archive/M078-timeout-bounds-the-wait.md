# M078: The limit bounds the wait, not the process

**Status:** done (2026-08-28, PR #82 https://github.com/jmgirard/tidymedia/pull/82)

**Goal:** Measure what `options(tidymedia.timeout = )` actually does on the platform where it was
seen to fail, in a container the repo can rebuild, and correct every place the package tells a reader the limit is a bound.

**Outcome:** base R's `timeout=` bounds R's WAIT, not the program: SIGINT at the limit, SIGTERM at
+20 s, SIGKILL at +40 s. Reading the child's stdout pipe holds R until the pipe closes — limit + 40,
measured 42.0 s under a 2 s limit on Linux and macOS alike; not reading one returns at limit + 20
**with the program still running**, a path no tidymedia call takes (`run_program()` passes
`stdout = TRUE`, the Layer 0 hatches `intern = TRUE`). The macOS/Linux split is the FFmpeg build,
not R: 9.0.1 answers the first signal, 6.1.1 blocked on a FIFO does not. `?with_timeout`,
`?local_timeout`, `?tidymedia` and `NEWS.md` now say the limit bounds the wait and give the
arithmetic and the number; `is_timeout()`, `guard_timeout()`, `R/ffprobe.R` and `R/ffm_manifest.R`
comments stop saying the limit "killed" anything. New `data-raw/Dockerfile.floors` rebuilds
`tidymedia-floors:r443` (absorbing F21 from the floor-script row); `data-raw/timeout-bound.R` runs
the seven-case grid with its own liveness control. A guard fences "42.0 seconds". No runtime change.

**Decisions:** D056 — M69 return 2's "bounded 42 s" premise CONFIRMED, no replacement mechanism decided; D055 item 3's 191.8 s and wedged suite unreproduced, not disproven.

**Review:** One round, five criteria fresh — grid and the three excluded fixture files re-measured at
review, six of seven cases within 0.2 s. Three lenses in-session (delegation disabled), five findings:
two fixed at the gate (`?tidymedia` still said the batch warning reports jobs "the limit killed"; the
D-entry's falsifier was met by its own C1 row), one deferred, two rejected. No defect or amendment return.
