# M53: Give `probe_all()` a `parallel =` argument

- **Status:** planned
- **Priority:** normal
- **Depends on:** M52
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Let `probe_all()` fan its per-file probes out across workers, so a large corpus
is bounded by the active future plan rather than by a `for` loop.

## Scope

**In:** a `parallel = FALSE` argument on `probe_all()` and on the four
`probe_*()` shortcuts that reprobe via `infile`, mapping over files with
`furrr` and honoring the active `future::plan()` as `ffm_batch()` does
(`R/ffm_batch.R:95-102`, D007/D012); `rlang::check_installed("furrr")` on the
parallel path only, so the Suggests dependency stays optional; a D-entry
recording that the package now has a second `furrr` fan-out; roxygen, NEWS.

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
      how they already treat `typed` (`R/ffprobe.R:264-272`, consumed only on
      the `infile` branch at `:270`).
- [ ] AC6 A `cairn/DECISIONS.md` entry records that the package now fans out
      with `furrr` in two places rather than one, and states what keeps them
      one concept: D007 says "Batch processing is a single tibble-in/tibble-out
      runner" and rules out "vectorizing individual verbs", neither of which
      this violates — `probe_all()` is a metadata reader already vectorized
      over files — but the entry is what stops that reading eroding.
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

- [ ] T1 Read `ffm_batch()`'s parallel seam (`R/ffm_batch.R:95-102`,
      `:174-184`) and decide whether to reuse its sequential-plan guard here;
      log the decision, since AC3's wording depends on it and D012 exists for
      that guard.
- [ ] T2 Tests first: parity between the two `parallel` values, plus the
      mutation probe that proves the argument is load-bearing.
- [ ] T3 Replace `probe_all()`'s `for` loop (`R/ffprobe.R:72-87`) with the
      mapped form, preserving the failure accumulation and the single
      end-of-call warning (`:89-94`).
- [ ] T4 `check_installed("furrr")` on the parallel path only, with a test that
      the sequential path never reaches it.
- [ ] T5 Thread `parallel` through `resolve_probe()` and the four shortcuts.
- [ ] T6 Append the D-entry; roxygen, NEWS, `devtools::document()`; run the
      profile's verify slot and `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose two milestones over one covering both axes because the goal sentence needed "and" and the two ship independently — M52 changes no exported signature while this one adds an argument; falsified by M52 turning out to force a signature change anyway.
- 2026-07-31: sequenced after M52 so the parallel path fans out the one-spawn probe rather than the N+1 one, which would otherwise multiply workers against a cost M52 removes.
- 2026-07-31: plan chose to reuse `ffm_batch()`'s existing `furrr` seam over introducing a second parallel mechanism, and to keep the default sequential; D014 already fixes `parallel` as the spelling and seventeen verbs carry it, so no naming question was open.
- 2026-07-31: criteria audit ([O], fresh context) returned four findings, all fixed above. AC3 was unreachable under T1's own design, because reusing `warn_if_sequential_plan()` emits a second warning under `parallel = TRUE` on the default sequential plan that a test run always has. AC1 said row order was "keyed by `file`" where it is actually input order, and justified itself by a scrambling risk furrr does not pose. AC4 carried an unfalsifiable clause asserting `Suggests` placement this milestone cannot change. And the audit flagged that a second `furrr` fan-out wants a D-entry against D007's single-runner reading, which is now AC6. Two cites corrected: `R/ffm_batch.R:95-102` and `:174-184`.

## Decisions

## Review
