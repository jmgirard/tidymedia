# M071: A parallel worker sees the settings the caller set

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m071-parallel-option-carry`

## Goal

A parallel worker runs under the same `tidymedia` option settings the caller set
in the parent process.

## Scope

Surface tier: **user-facing** — the deliverable changes what a caller's
`parallel = TRUE` call does at run time.

**In:** an internal carrier in `R/timeout.R` that captures the parent's resolved
`tidymedia.timeout` and its `tidymedia.nvenc_encoders`, sets both inside each
worker for the duration of the mapped call, and restores the worker's prior
values on exit; wiring it at the fan-out sites `grep -rn "furrr::future_" R/`
reports (today `R/ffm_batch.R:102`, `:140`, `R/ffprobe.R:124`,
`R/loudnorm_two_pass.R:197`); resolving `tidymedia.timeout` once in
`ffm_batch()`'s validation block so a bad value is refused before dispatch on
both branches; the batch timeout warning becoming reachable at
`parallel = TRUE`; the docs that currently disclose the gap.

**Out:** per-call `timeout =` arguments on the run-capable verbs — stays the
candidate row it has been since M69. The per-process *capability memo*
(`R/cache.R`) still does not cross to workers; only the caller's override does
— stays its own candidate row. A tighter kill than base R's `timeout=` —
candidate row. `find_ffmpeg()` memoization — candidate row. M70's
timeout-silence sweep (`tests/testthat/helper-timeout-sweep.R`) is not touched:
its domain, its recorded list and its promise ship as M70 left them.

## Acceptance criteria

- [ ] AC1 With a two-worker `future::multisession` plan, `options(tidymedia.timeout = 1)`
      set in the parent, and a media program on the workers' path that would run for
      30 seconds, the program is killed inside the worker at each of the three fan-out
      sites where a worker-side spawn is reachable, each reporting in its own
      documented shape: `ffm_batch(parallel = TRUE, run = TRUE)` (`R/ffm_batch.R:140`)
      marks the row `success = FALSE`; `probe_all(parallel = TRUE)`
      (`R/ffprobe.R:124`) gives an `NA` row and one end-of-call warning naming the
      file as timed out rather than unreadable; `normalize_audio_batch(two_pass = TRUE,
      parallel = TRUE)` (`R/loudnorm_two_pass.R:197`) aborts with class
      `tidymedia_timeout`. The fourth site that grep reports, the pipeline build at
      `R/ffm_batch.R:102`, is covered by AC2 instead: its only worker-side spawn is
      the encoder probe, which every `_batch` verb's front door has already answered
      in the parent.
- [ ] AC2 With a two-worker `future::multisession` plan and a `hardware = "nvenc"`
      batch built at `parallel = TRUE`: with `options(tidymedia.nvenc_encoders =
      "h264_nvenc")` set in the parent, no worker spawns FFmpeg to ask for the encoder
      list and the compiled commands equal those `parallel = FALSE` produces; with the
      option unset, the same batch shows one encoder-list spawn per worker.
- [ ] AC3 Under a two-worker `future::multisession` plan and a limit the spawned
      program exceeds, `ffm_batch(parallel = TRUE, run = TRUE)` over an N-row jobs
      table marks all N rows `success = FALSE` and signals one condition of class
      `tidymedia_batch_timeout` stating that the limit killed N jobs.
- [ ] AC4 A worker whose `tidymedia.timeout` and `tidymedia.nvenc_encoders` held
      values of their own before a carried fan-out holds those same values after it,
      in the same process, both when the mapped call returns and when it raises an
      error.
- [ ] AC5 Each of four invalid `tidymedia.timeout` values — `0.5`, `-1`, `NA`, `"2"`
      — is refused by `ffm_batch()` before any job is dispatched, with the same
      condition at `parallel = TRUE, run = FALSE` as at `parallel = FALSE,
      run = TRUE`, and with no worker having executed `.f`.
- [ ] AC6 Neither the "Bounding a run that hangs" section of `?tidymedia` nor the
      development-version entry in `NEWS.md` states that `parallel = TRUE` workers do
      not see the limit; each states instead that tidymedia's own `parallel = TRUE`
      paths are bounded by the same limit as their sequential paths.
      `refresh_ffmpeg_capabilities()`'s documentation states that the caller's encoder
      override reaches a worker, and conditions its existing "asks FFmpeg W times"
      sentence on that override being unset.
- [ ] AC7 `Rscript -e 'devtools::test()'` is clean and `Rscript -e 'devtools::check()'`
      reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T3, T4, T5, T6
- AC2 → T1, T3, T5, T7
- AC3 → T3, T5, T7
- AC4 → T1, T8
- AC5 → T2, T8
- AC6 → T9
- AC7 → T10

## Tasks

- [x] T1 Carrier in `R/timeout.R`: capture the parent's resolved
      `tidymedia.timeout` and its `tidymedia.nvenc_encoders`; the returned wrapper
      sets both inside the worker and restores the prior values on return and on
      error. In-process unit tests with five mutants shown red — drop the timeout
      restore, drop the encoder restore, restore on return but not on error (each
      option), and remove the parent-side resolve.
- [x] T2 Resolve `tidymedia.timeout` once in `ffm_batch()`'s validation block
      (`R/ffm_batch.R:88-98`), before either branch maps. Today it is read only inside
      `run_program()` (`R/program_management.R:122`), below `run_one`'s `tryCatch`, so
      a bad value surfaces as a silent `success = FALSE` — and not at all when no
      binary is found (`R/program_management.R:111-113`).
- [x] T3 Wire the carrier at `R/ffm_batch.R:102` and `:140`, parallel branch only;
      the sequential branches are unchanged.
- [x] T4 Wire `R/ffprobe.R:124` and `R/loudnorm_two_pass.R:197`.
- [x] T5 New `tests/testthat/test-parallel-option-carry.R` harness: fake
      `ffmpeg`/`ffprobe` shell scripts that append their invocation to a log and
      `sleep 30`, placed first on `PATH`, then a `multisession` plan booted after the
      `PATH` edit — `future` caches its cluster for the session, so assert from a
      worker that `Sys.which("ffmpeg")` is the fake before trusting any cell.
      `chunk_size = 1` so every worker is visited; `skip_on_os("windows")`,
      `skip_on_cran()`, `skip_if_not_installed("furrr")`; and skip unless the worker's
      loaded namespace matches the source under test, since `devtools::test()`'s
      workers load the installed package. AC1–AC5 evidence must come from a run in
      which this file's tests execute rather than skip.
- [x] T6 AC1's three kill cases, each asserting its own documented shape, plus a
      guard that `grep -rn "furrr::future_" R/` returns no site absent from the case
      table. Each red on master.
- [ ] T7 AC2's override case and its option-unset control, and AC3's warning over an
      N-row table. Each red on master.
- [ ] T8 AC4's restoration: stamp a per-worker sentinel keyed by `Sys.getpid()`
      before the fan-out and assert per-PID equality after, for both options and both
      the returning and erroring mapped call. AC5's four invalid values on both paths,
      asserting the fake's log is empty.
- [ ] T9 Docs: the two `?tidymedia` sentences, the `NEWS.md` development entry, and
      `refresh_ffmpeg_capabilities()`'s paragraph plus the internal comment at
      `R/cache.R:11`; `devtools::document()` no diff.
- [ ] T10 D-entry: supersedes D047's "Disclosed, not fixed" bullet, and supersedes
      D044's seeding rejection — stating that re-establishing a value the caller set
      and restoring the prior one differs from the package authoring one, and that
      D044's capability *memo* gap stays disclosed and unfixed. Full
      `devtools::check()`; NOTEs justified in this file.

## Work log

- 2026-08-26: created by /milestone-plan, promoting part (b) of the "Two timeout residues M69 leaves out" candidate row.
- 2026-08-26: premise re-measured before planning on it — future 1.75.0 / furrr 0.4.0, a multisession worker read `tidymedia.timeout` as UNSET against `42` in the parent; a parent-captured closure that sets and restores it read `42`.
- 2026-08-26: criteria audit ran in FULL mode (user-facing tier), two passes in fresh-context [O] readers; pass 1 returned 13 findings, pass 2 returned 11 against the revised wording. Ten of pass 1 and eight of pass 2 were fixed here; three of pass 1 went to the question gate. Pass 2's blocking finding was AC5's sequential leg being unsatisfiable, which added T2.
- 2026-08-26: pass 2's "add a criterion binding the two supersessions" was declined as a finding of its own kind — a criterion mandating a recording act is instrument-bound (D-120); the supersessions stay in T10. Pass 2's "AC7 requires zero skips" was likewise moved to T5's evidence clause rather than into AC7.
- 2026-08-26: plan gate chose carrying the caller's option values into the worker and restoring the prior ones over threading a resolved limit through the internal spawn signatures, because the latter changes every spawn site's contract and loses the read-at-spawn-time property; falsified by a report of a worker-side option write colliding with a caller's own worker configuration.
- 2026-08-26: plan gate chose covering both option seams over the timeout seam alone, because the carrier is the same code either way and the encoder override diverges silently by form today; falsified by a report that a worker honoring the parent's encoder override is the wrong answer for that caller.
- 2026-08-26: plan gate chose refusing an invalid limit up front on both branches over carrying the raw value and resolving at the spawn site, because the compile-only parallel path never reads it today and so never reports it; falsified by a compile-only batch that legitimately needs to build under an invalid limit.

- 2026-08-26: T1 — `carry_options()` + `carried_option_values()` in `R/timeout.R`; carries the resolved limit and the encoder override, restores the worker's prior values via `on.exit(options(prior))`. An unset name is carried as unset (`options(x = NULL)` removes the entry, measured R 4.6.1). Six in-process tests, 14 assertions; all five mutants red (2, 3, 1, 1, 4 failures).

- 2026-08-26: T2 — `ffm_batch()` calls `resolve_timeout()` in its validation block, before either branch maps. Measured: `tidymedia.timeout = 0.5` at `run = FALSE` now aborts with `` `tidymedia.timeout` must be a whole number, not the number 0.5 `` and `conditionCall()` naming `ffm_batch()`. Suite 6345 pass / 0 fail.

- 2026-08-26: T3/T4 — carrier wired at all four sites `grep -rn "furrr::future_" R/` reports (`R/ffm_batch.R:112`, `:150`, `R/ffprobe.R:124`, `R/loudnorm_two_pass.R:197`), parallel branches only. Suite 6345 pass / 0 fail.

- 2026-08-26: T5/T6 — `tests/testthat/test-parallel-option-carry.R` harness: fake `ffmpeg`/`ffprobe` that log every invocation and `sleep 30` (answering `-encoders` at once), prefixed onto `PATH`, then a fresh two-worker PSOCK cluster per test; workers assert the fake is what `Sys.which("ffmpeg")` returns, and skip unless their `carry_options` body matches the parent's. AC1's three cases green with the wiring (30 assertions, no skips) and red without it: the batch signals no `tidymedia_batch_timeout`, `probe_all()` reports unreadable rather than timed out, and the two-pass loudnorm throws something other than `tidymedia_timeout`.

## Decisions

## Review
