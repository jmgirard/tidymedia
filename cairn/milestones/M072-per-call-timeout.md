# M072: One call can carry its own time limit

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m072-per-call-timeout`

## Goal

Let a caller bound one call's wall-clock time without changing the session's limit.

## Scope

Surface tier: **user-facing** — the deliverable is a new exported function callers write.

**In:** One new export, `with_timeout(expr, seconds)`, in `R/timeout.R`. It
establishes `tidymedia.timeout` for the dynamic extent of `expr` and puts the
caller's prior option state back on every exit, using the base
`options()`/`on.exit()` pair `carry_options()` already uses (`R/timeout.R:221`)
— the only `options()` write and the only `on.exit()` in `R/`. Because R options
are process-global, all four spawn sites and `carried_option_values()` read the
per-call value with nothing threaded. A bad `seconds` is refused eagerly, naming
`seconds`. Plus the roxygen topic, a `_pkgdown.yml` row, a NEWS entry, and a
D-entry.

**Out:**
- `timeout =` arguments on the verbs → stays its ROADMAP candidate row,
  unchanged. D047's rejection of that shape stands and is not superseded here;
  only its session-grain falsifier clause is discharged.
- `local_timeout(seconds, .local_envir)`, the other half of withr's pair → new
  ROADMAP candidate row, promotable on the first request for it.
- The sequential-plan carry rollback (a `parallel = TRUE` batch under a
  sequential plan rolling back option writes made by the caller's own `.f`) →
  stays on M071's instrument-findings candidate row (F9).

## Acceptance criteria

- [ ] AC1. `with_timeout(expr, seconds)` evaluates `expr` exactly once, in the
      environment of the caller, and returns its value; while `expr` is being
      evaluated `getOption("tidymedia.timeout")` equals `as.numeric(seconds)`.
- [ ] AC2. On each of three paths a test drives — `expr` returning normally,
      `expr` signalling an `rlang::abort()`, and `expr` signalling a
      `tidymedia_timeout` abort — `getOption("tidymedia.timeout")` after
      `with_timeout()` leaves is what it was immediately before the call: the
      same value where one was set, and absent where the option was unset.
- [ ] AC3. A new `tm_spawn_sites()` in `tests/testthat/helper-timeout-sweep.R`
      returns the namespace functions whose own body names `system` or
      `system2`, and a test asserts that set equals the four recorded today, so
      a new spawn site reddens it. With `tidymedia.timeout` unset for the
      session, each member hands `as.numeric(seconds)` to `guard_timeout()` for
      a call made inside `with_timeout(expr, seconds)`, and `ffm_batch()`'s
      up-front `resolve_timeout()` (`R/ffm_batch.R:100`) reads the same value.
- [ ] AC4. With `tidymedia.timeout` unset for the session, a media program hung
      on the mkfifo anchor (`local_blocking_input()`,
      `tests/testthat/helper-timeout-sweep.R:295`) inside
      `with_timeout(expr, 2)` aborts with class `tidymedia_timeout` naming a 2
      second limit. The cell carries its own outer bound, so an implementation
      that establishes nothing fails it rather than hanging the runner.
- [ ] AC5. Under M071's `local_carry_harness()`, with the session
      `tidymedia.timeout` set to a value that is not 2, a fake program that
      sleeps 30 seconds inside `with_timeout(expr, 2)` is killed at 2 seconds in
      a `parallel = TRUE` worker, for each of the three parallel entry points
      that harness covers — `ffm_batch()`, `probe_all()`, and the two-pass
      loudnorm path. Those reach three of the four `furrr::future_*` sites the
      `R/*.R` grep at `tests/testthat/test-parallel-option-carry.R:266` returns;
      the fourth (`R/ffm_batch.R:112`, the pipeline build) spawns only the
      encoder probe and carries no kill, which the test records.
- [ ] AC6. `with_timeout()` refuses a bad `seconds` before `expr` is evaluated —
      a probe whose `expr` writes a marker file asserts the marker is absent on
      every refusal — with a message naming `seconds` and not
      `tidymedia.timeout`. Over the probe vector
      `list(0, 1L, 60, 0.5, -1, NA, NA_real_, "2", c(1, 2), Inf, TRUE,
      integer(0), factor("2"))` it accepts exactly those values
      `resolve_timeout()` accepts under `options(tidymedia.timeout = v)`;
      `seconds` is required, and `NULL` and a missing argument are refused.
- [ ] AC7. `cairn/DECISIONS.md` carries a new D-entry stating the per-call
      grain, superseding D047's falsifier clause on the per-verb-argument bullet
      while leaving that bullet's rejection standing, with its header saying so.
      `?with_timeout` renders with an example that runs with no media binary
      present; `_pkgdown.yml` carries a reference row for `with_timeout`;
      `NEWS.md` has an entry for the change; `devtools::document()` produces no
      diff.
- [ ] AC8. `devtools::test()` and `devtools::check()` are clean — 0 errors, 0
      warnings, any NOTE justified.

## Coverage

- AC1 → T2, T3
- AC2 → T3
- AC3 → T1, T5
- AC4 → T6
- AC5 → T7
- AC6 → T2, T4
- AC7 → T2, T8, T9
- AC8 → T10

## Tasks

- [x] T1. Add `tm_spawn_sites()` to `tests/testthat/helper-timeout-sweep.R`
      returning the seed set `tm_reaches_spawn()` computes its closure from
      (`helper-timeout-sweep.R:62-65` inlines it today and returns only the
      closure), plus the set-equality drift test against the four recorded.
- [ ] T2. Write `with_timeout()` in `R/timeout.R`: eager
      `rlang::check_number_whole(seconds, min = 0, arg = "seconds")`, then
      `prior <- options(...)` / `on.exit(options(prior), add = TRUE)`, `expr`
      forced once in the caller's frame, its value returned. Roxygen topic with
      an example that runs with no media binary. `devtools::document()`.
      (RB tripwire: irreversible-api)
- [ ] T3. Unit tests for the payload (AC1) and the three restore paths (AC2),
      covering both a previously-set and a previously-unset option.
- [ ] T4. Refusal tests: the probe vector against `resolve_timeout()`'s verdict,
      the marker-file eagerness probe, and missing/`NULL` `seconds`.
- [ ] T5. Spawn-site test: mock `guard_timeout()` to record `limit`, drive one
      call per `tm_spawn_sites()` member inside `with_timeout()` with the
      session option unset, and assert `ffm_batch()`'s up-front read too.
- [ ] T6. FIFO anchor cell through `with_timeout()` with the session option
      unset, carrying its own outer bound. Budget limit + 40 s per M69's lesson
      on base R's SIGINT/SIGTERM/SIGKILL escalation; `skip_on_cran()`.
- [ ] T7. Parallel cells under `local_carry_harness()` for the three entry
      points, and record the pipeline-build site's no-kill status.
- [ ] T8. Docs: `_pkgdown.yml` reference row, `NEWS.md` entry, and the timeout
      paragraph in the package landing topic (`R/tidymedia-package.R:18`).
- [ ] T9. Append the D-entry to `cairn/DECISIONS.md`.
- [ ] T10. `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: plan gate chose an exported `with_timeout()` wrapper over `timeout =` arguments on the run-capable verbs because the wrapper reaches all 53 members of `tm_timeout_domain()` where the argument reaches 32 — the 22 exports with no `run =` (`ffm_run()`, the Layer 0 hatches, the probe and MediaInfo readers, `verify_media()`) have no channel for one — and because it costs one irreversible export against seventeen signature changes; falsified by a report that wrapping an expression is the wrong ergonomics for a script, or by a caller needing a limit that varies per row within one batch.
- 2026-08-27: plan gate chose base `options()`/`on.exit()` over promoting `withr` from Suggests to Imports because `carry_options()` (`R/timeout.R:221`) already establishes options this way and needs no dependency; falsified by a restore path base `on.exit()` misses that `withr` handles.
- 2026-08-27: plan gate chose shipping `with_timeout()` alone over the full withr pair because `local_timeout()` is a second irreversible export for an idiom `with_timeout({ ... }, n)` already expresses; falsified by a request to bound the rest of a function body without wrapping it.
- 2026-08-27: plan gate chose the spawn-site value check plus the FIFO anchor over the mocked silence grid because `tm_force_timeout()` (`helper-timeout-sweep.R:250`) mocks `run_program()` and `guard_timeout()` to abort unconditionally, so no limit is ever read and a `with_timeout()` that set nothing would pass every cell; falsified by the anchor proving unrunnable on enough platforms to leave the real kill unobserved.
- 2026-08-27: criteria audit ran in FULL mode (user-facing tier; irreversible-api tripwire on T2), twice — a fresh-context reader on the pre-gate draft returned 13 findings plus one set-level gap, and a second reader on the post-gate wording returned 11 more; all were fixed at the gate, none escalated to a question. Load-bearing repairs: the draft's silence-grid criterion was unexercisable (mocked bindings read no limit) and re-verified D049 rather than this deliverable; the parallel criterion stood one exemplar in for four `carry_options()` sites and went degenerate when the session value equalled the per-call value; the refusal criterion specified eager and lazy validation at once; AC3 cited a procedure that returns a closure rather than the seed set (T1 now builds one); AC4's unbounded control would have hung the runner forever and was cut; AC5's FIFO anchor was inert under the fake-binary harness and became the harness's own sleeping fake; and the probe vector's `NULL` contradicted AC1, since `options(x = NULL)` removes the name.

- 2026-08-27: T1 — `tm_spawn_sites()` named out of `tm_reaches_spawn()`'s inlined first statement; the recorded set is `ffmpeg`/`ffprobe`/`mediainfo`/`run_program`, and a planted fifth site reddens the drift test.

## Decisions

## Review
