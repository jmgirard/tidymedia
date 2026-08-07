# M53: Give `probe_all()` a `parallel =` argument

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M52
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m53-probe-all-parallel`

## Goal

Let `probe_all()` fan its per-file probes out across workers, so a large corpus
is bounded by the active future plan rather than by a `for` loop.

## Scope

**In:** a `parallel = FALSE` argument on `probe_all()` and on the four
`probe_*()` shortcuts that reprobe via `infile`, mapping over files with
`furrr` and honoring the active `future::plan()` as `ffm_batch()` does
(`R/ffm_batch.R:95-102`, D007/D012); `rlang::check_installed("furrr")` on the
parallel path only, so the Suggests dependency stays optional; a D-entry
recording that `furrr` fan-out now crosses from the execution side to the
metadata side; roxygen, NEWS.

**Out:** any change to `probe_one()`'s per-file cost, which is M52's. The batch
verbs' up-front dropped-track probe → the standing candidate row.
Parallelizing the `mediainfo_*()` readers or the `get_*()` helpers → new
candidate row. Changing the default, which stays sequential.

## Acceptance criteria

- [ ] AC1 `probe_all(parallel = TRUE)` returns output identical to
      `parallel = FALSE` — same tibbles, same types, and rows aligned to the
      **input vector's** order, which is what `probe_all()` guarantees today by
      assigning into a preallocated list at `[[i]]` (`R/ffprobe.R:68-87`).
      Tested on a multi-file vector containing an unprobeable file and a
      duplicated path, the case the suite already exercises
      (`tests/testthat/test-ffprobe.R:55-58`).
- [ ] AC2 The `parallel` argument reaches the fan-out rather than being
      accepted and ignored: proven by mutating the implementation to drop it
      and observing the test go red (M39's mutation trick, which the repo
      adopted precisely because asserting a default passes either way).
- [ ] AC3 The unprobeable-file warning contract survives the fan-out: a vector
      containing two unprobeable files raises exactly **one** such warning
      naming both, under either `parallel` value — not one per worker and not
      none. Stated as "one unprobeable-file warning" deliberately: if T1 adopts
      `ffm_batch()`'s sequential-plan guard (`R/ffm_batch.R:174-184`), a
      `parallel = TRUE` call under the default sequential plan also emits that
      guard's warning, and the test must assert on the class or text of the
      file warning rather than on a total count.
- [ ] AC4 `furrr` is required only on the parallel path: with `furrr` masked as
      unavailable, `parallel = FALSE` completes normally and `parallel = TRUE`
      raises `rlang`'s `check_installed()` condition, asserted by that
      condition's class rather than by its message text.
- [ ] AC5 The four `probe_*()` shortcuts pass `parallel` through when they
      reprobe via `infile` and ignore it when given a `probe` object, matching
      how they already treat `typed` (`R/ffprobe.R:259-267`, consumed only on
      the `infile` branch at `:265`).
- [ ] AC6 A `cairn/DECISIONS.md` entry records that `furrr` fan-out now
      crosses from the execution side to the metadata side, and states what
      keeps that one concept. It records the side-crossing rather than a site
      count: `grep -rn "furrr::" R/` returns three pre-existing call sites in
      two files (`R/ffm_batch.R:102`, `:132`, `R/loudnorm_two_pass.R:197`),
      all execution-side, so this criterion's original "two places rather
      than one" was false. The entry states that D007 — "Batch processing is
      a single tibble-in/tibble-out runner", ruling out "vectorizing
      individual verbs" — is not violated, `probe_all()` being a metadata
      reader already vectorized over files; the entry is what stops that
      reading eroding.
- [ ] AC7 `devtools::document()` produces no diff, `devtools::test()` clean,
      `devtools::check()` reports 0 errors / 0 warnings; NEWS entry.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T1, T3
- AC4 → T4
- AC5 → T5
- AC6 → T1, T6
- AC7 → T6

## Tasks

- [x] T1 Read `ffm_batch()`'s parallel seam (`R/ffm_batch.R:95-105`,
      `:174-184`) and decide whether to reuse its sequential-plan guard here;
      log the decision, since AC3's wording depends on it and D012 exists for
      that guard.
- [x] T2 Tests first: parity between the two `parallel` values, plus the
      mutation probe that proves the argument is load-bearing.
- [x] T3 Replace `probe_all()`'s `for` loop (`R/ffprobe.R:72-87`) with the
      mapped form, preserving the failure accumulation and the single
      end-of-call warning (`:89-94`).
- [x] T4 `check_installed("furrr")` on the parallel path only, with a test that
      the sequential path never reaches it.
- [x] T5 Thread `parallel` through `resolve_probe()` and the four shortcuts.
- [ ] T6 Append the D-entry; roxygen, NEWS, `devtools::document()`; run the
      profile's verify slot and `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose two milestones over one covering both axes because the goal sentence needed "and" and the two ship independently — M52 changes no exported signature while this one adds an argument; falsified by M52 turning out to force a signature change anyway.
- 2026-07-31: sequenced after M52 so the parallel path fans out the one-spawn probe rather than the N+1 one, which would otherwise multiply workers against a cost M52 removes.
- 2026-07-31: plan chose to reuse `ffm_batch()`'s existing `furrr` seam over introducing a second parallel mechanism, and to keep the default sequential; D014 already fixes `parallel` as the spelling and seventeen verbs carry it, so no naming question was open.
- 2026-07-31: criteria audit ([O], fresh context) returned four findings, all fixed above. AC3 was unreachable under T1's own design, because reusing `warn_if_sequential_plan()` emits a second warning under `parallel = TRUE` on the default sequential plan that a test run always has. AC1 said row order was "keyed by `file`" where it is actually input order, and justified itself by a scrambling risk furrr does not pose. AC4 carried an unfalsifiable clause asserting `Suggests` placement this milestone cannot change. And the audit flagged that a second `furrr` fan-out wants a D-entry against D007's single-runner reading, which is now AC6. Two cites corrected: `R/ffm_batch.R:95-102` and `:174-184`.

- 2026-08-06: re-planning run over the already-planned M53 confirmed the scope
  and criteria stand; no re-cut. Every code cite was re-verified against the
  post-M52 tree and two were refreshed for drift: AC5's `resolve_probe()` span
  (`R/ffprobe.R:264-272`, branch `:270` → `:259-267`, branch `:265`) and T1's
  `ffm_batch()` seam, which was clipped mid-`if`/`else` (`:95-102` → `:95-105`).
  AC1's `:72-87`/`:89-94`, AC3's `R/ffm_batch.R:174-184`, and the four
  shortcuts at `:216-241` all still land where the plan says.

- 2026-08-06: T1 — `probe_all()`'s parallel path WILL call
  `warn_if_sequential_plan()`. Rejected the alternative of following
  `run_loudnorm_analysis_batch()`'s shape, which fans out silently and leaves
  the warning to "the Phase 2 `ffm_batch()` call so it fires exactly once"
  (`R/loudnorm_two_pass.R:162-171`): that rationale rests on a downstream
  call that warns, and `probe_all()` is a terminal entry point with none, so
  copying it would make `parallel = TRUE` under the default sequential plan
  silently no-op — the case D012 added the guard for. Falsified by a user
  report that the doubled warning (guard + unprobeable-file) is noisier than
  the silent no-speedup it prevents. AC3 already fixes the test shape for
  this branch: assert on the file warning's class or text, never a count.
- 2026-08-06: implement gate amended AC6 and the Scope line it mirrors. AC6
  required a D-entry recording "two places rather than one"; that count was
  false before the milestone started — `grep -rn "furrr::" R/` returns three
  call sites in two files (`R/ffm_batch.R:102`, `:132`,
  `R/loudnorm_two_pass.R:197`), all execution-side. Amended to record the
  execution→metadata side-crossing instead, and to name that grep as the
  procedure enumerating the domain. Rejected fixing only the count, which
  would have kept site-counting as the thing the D-entry records.

- 2026-08-06: T2–T5 landed in one commit rather than four. Minor amendment: the
  profile's verify slot requires `devtools::test()` clean before a task is
  checked off, and T2's tests cannot go green until T3/T5 exist — so splitting
  them would have committed a red suite. Task wording and ordering unchanged.
- 2026-08-06: T3 fans out `probe_one()` alone and leaves the assembly loop in
  the parent process. The rejected alternative was mapping the whole per-file
  body (assembly included) and reducing the results: it would have moved the
  `failed` accumulator and the end-of-call `cli_warn()` into workers, where
  AC3's "exactly one warning naming both" becomes one warning per worker or
  none. Only `probe_one()` shells out, so the discarded parallelism is free.
  Falsified by a profile showing the parent-side assembly dominating probe time.
- 2026-08-06: AC2 mutation probe run and recorded. With
  `probes <- if (parallel)` mutated to `if (FALSE)` the suite goes 5 failed /
  23 passed, the failures landing exactly on the two argument-load-bearing
  tests ("routes through furrr, parallel = FALSE does not", 1; "shortcuts pass
  `parallel` through on the infile branch", 4). Restored: 28 passed, 0 failed.
- 2026-08-06: AC4's furrr masking is staged by stubbing `rlang::check_installed`
  rather than by hiding the installed package. Rejected mocking
  `rlang::is_installed` or `detect_installed`: `check_installed()` short-circuits
  on `.getNamespace(x)` before it reaches either, so with furrr loaded neither
  stub can make it report missing. The stub raises rlang's real
  `rlib_error_package_not_found` class and the test asserts that class, as AC4
  requires. Falsified by a future rlang that stops emitting that class.

## Decisions

## Review
