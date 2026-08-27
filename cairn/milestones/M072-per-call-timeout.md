# M072: One call can carry its own time limit

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m072-per-call-timeout` / https://github.com/jmgirard/tidymedia/pull/76

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

- [x] AC1. `with_timeout(expr, seconds)` evaluates `expr` exactly once, in the
      environment of the caller, and returns its value; while `expr` is being
      evaluated `getOption("tidymedia.timeout")` equals `as.numeric(seconds)`.
- [x] AC2. On each of three paths a test drives — `expr` returning normally,
      `expr` signalling an `rlang::abort()`, and `expr` signalling a
      `tidymedia_timeout` abort — `getOption("tidymedia.timeout")` after
      `with_timeout()` leaves is what it was immediately before the call: the
      same value where one was set, and absent where the option was unset.
- [x] AC3. A new `tm_spawn_sites()` in `tests/testthat/helper-timeout-sweep.R`
      returns the namespace functions whose own body names `system` or
      `system2`, and a test asserts that set equals the four recorded today, so
      a new spawn site reddens it. With `tidymedia.timeout` unset for the
      session, each member hands `as.numeric(seconds)` to `guard_timeout()` for
      a call made inside `with_timeout(expr, seconds)`, and `ffm_batch()`'s
      up-front `resolve_timeout()` (`R/ffm_batch.R:100`) reads the same value.
- [x] AC4. With `tidymedia.timeout` unset for the session, a media program hung
      on the mkfifo anchor (`local_blocking_input()`,
      `tests/testthat/helper-timeout-sweep.R:295`) inside
      `with_timeout(expr, 2)` aborts with class `tidymedia_timeout` naming a 2
      second limit. The cell carries its own outer bound, so an implementation
      that establishes nothing fails it rather than hanging the runner.
- [x] AC5. Under M071's `local_carry_harness()`, with the session
      `tidymedia.timeout` set to a value that is not 2, a fake program that
      sleeps 30 seconds inside `with_timeout(expr, 2)` is killed at 2 seconds in
      a `parallel = TRUE` worker, for each of the three parallel entry points
      that harness covers — `ffm_batch()`, `probe_all()`, and the two-pass
      loudnorm path. Those reach three of the four `furrr::future_*` sites the
      `R/*.R` grep at `tests/testthat/test-parallel-option-carry.R:266` returns;
      the fourth (`R/ffm_batch.R:112`, the pipeline build) spawns only the
      encoder probe and carries no kill, which the test records.
- [x] AC6. `with_timeout()` refuses a bad `seconds` before `expr` is evaluated —
      a probe whose `expr` writes a marker file asserts the marker is absent on
      every refusal — with a message naming `seconds` and not
      `tidymedia.timeout`. Over the probe vector
      `list(0, 1L, 60, 0.5, -1, NA, NA_real_, "2", c(1, 2), Inf, TRUE,
      integer(0), factor("2"))` it accepts exactly those values
      `resolve_timeout()` accepts under `options(tidymedia.timeout = v)`;
      `seconds` is required, and `NULL` and a missing argument are refused.
- [x] AC7. `cairn/DECISIONS.md` carries a new D-entry stating the per-call
      grain, superseding D047's falsifier clause on the per-verb-argument bullet
      while leaving that bullet's rejection standing, with its header saying so.
      `?with_timeout` renders with an example that runs with no media binary
      present; `_pkgdown.yml` carries a reference row for `with_timeout`;
      `NEWS.md` has an entry for the change; `devtools::document()` produces no
      diff.
- [x] AC8. `devtools::test()` and `devtools::check()` are clean — 0 errors, 0
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
- [x] T2. Write `with_timeout()` in `R/timeout.R`: eager
      `rlang::check_number_whole(seconds, min = 0, arg = "seconds")`, then
      `prior <- options(...)` / `on.exit(options(prior), add = TRUE)`, `expr`
      forced once in the caller's frame, its value returned. Roxygen topic with
      an example that runs with no media binary. `devtools::document()`.
      (RB tripwire: irreversible-api)
- [x] T3. Unit tests for the payload (AC1) and the three restore paths (AC2),
      covering both a previously-set and a previously-unset option.
- [x] T4. Refusal tests: the probe vector against `resolve_timeout()`'s verdict,
      the marker-file eagerness probe, and missing/`NULL` `seconds`.
- [x] T5. Spawn-site test: mock `guard_timeout()` to record `limit`, drive one
      call per `tm_spawn_sites()` member inside `with_timeout()` with the
      session option unset, and assert `ffm_batch()`'s up-front read too.
- [x] T6. FIFO anchor cell through `with_timeout()` with the session option
      unset, carrying its own outer bound. Budget limit + 40 s per M69's lesson
      on base R's SIGINT/SIGTERM/SIGKILL escalation; `skip_on_cran()`.
- [x] T7. Parallel cells under `local_carry_harness()` for the three entry
      points, and record the pipeline-build site's no-kill status.
- [x] T8. Docs: `_pkgdown.yml` reference row, `NEWS.md` entry, and the timeout
      paragraph in the package landing topic (`R/tidymedia-package.R:18`).
- [x] T9. Append the D-entry to `cairn/DECISIONS.md`.
- [x] T10. `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: plan gate chose an exported `with_timeout()` wrapper over `timeout =` arguments on the run-capable verbs because the wrapper reaches all 53 members of `tm_timeout_domain()` where the argument reaches 32 — the 22 exports with no `run =` (`ffm_run()`, the Layer 0 hatches, the probe and MediaInfo readers, `verify_media()`) have no channel for one — and because it costs one irreversible export against seventeen signature changes; falsified by a report that wrapping an expression is the wrong ergonomics for a script, or by a caller needing a limit that varies per row within one batch.
- 2026-08-27: plan gate chose base `options()`/`on.exit()` over promoting `withr` from Suggests to Imports because `carry_options()` (`R/timeout.R:221`) already establishes options this way and needs no dependency; falsified by a restore path base `on.exit()` misses that `withr` handles.
- 2026-08-27: plan gate chose shipping `with_timeout()` alone over the full withr pair because `local_timeout()` is a second irreversible export for an idiom `with_timeout({ ... }, n)` already expresses; falsified by a request to bound the rest of a function body without wrapping it.
- 2026-08-27: plan gate chose the spawn-site value check plus the FIFO anchor over the mocked silence grid because `tm_force_timeout()` (`helper-timeout-sweep.R:250`) mocks `run_program()` and `guard_timeout()` to abort unconditionally, so no limit is ever read and a `with_timeout()` that set nothing would pass every cell; falsified by the anchor proving unrunnable on enough platforms to leave the real kill unobserved.
- 2026-08-27: criteria audit ran in FULL mode (user-facing tier; irreversible-api tripwire on T2), twice — a fresh-context reader on the pre-gate draft returned 13 findings plus one set-level gap, and a second reader on the post-gate wording returned 11 more; all were fixed at the gate, none escalated to a question. Load-bearing repairs: the draft's silence-grid criterion was unexercisable (mocked bindings read no limit) and re-verified D049 rather than this deliverable; the parallel criterion stood one exemplar in for four `carry_options()` sites and went degenerate when the session value equalled the per-call value; the refusal criterion specified eager and lazy validation at once; AC3 cited a procedure that returns a closure rather than the seed set (T1 now builds one); AC4's unbounded control would have hung the runner forever and was cut; AC5's FIFO anchor was inert under the fake-binary harness and became the harness's own sleeping fake; and the probe vector's `NULL` contradicted AC1, since `options(x = NULL)` removes the name.

- 2026-08-27: T1 — `tm_spawn_sites()` named out of `tm_reaches_spawn()`'s inlined first statement; the recorded set is `ffmpeg`/`ffprobe`/`mediainfo`/`run_program`, and a planted fifth site reddens the drift test.

- 2026-08-27: T2 — `with_timeout(expr, seconds)` exported (gate chose the code-first order over withr's value-first, matching `R.utils::withTimeout()`); measured: the option reads 30 inside the call and is unset again after, and a prior 99 comes back on both the returning and the erroring path.

- 2026-08-27: T3 — payload and restore tests; deleting the `on.exit()` restore turns 7 of the 26 cells red, so the restore claim is the thing they measure.

- 2026-08-27: T4 — the probe vector is scored against `resolve_timeout()`'s own verdict (3 accepted, 10 refused); deferring the check to exit makes the marker file appear and reddens the eagerness cells.

- 2026-08-27: T5 — all four spawn sites plus `ffm_batch()`'s up-front read are handed 7 inside `with_timeout(expr, 7)` and 0 outside it; a wrapper that establishes nothing reddens 10 cells.

- 2026-08-27: T6 — FIFO anchor through `with_timeout(expr, 2)` with the session option unset aborts in ~2 s on macOS; the cell's outer bound is a background writer that releases the FIFO at 90 s, and with the wrapper broken the run failed at 91 s instead of hanging (a first draft of that writer carried a duplicate `&` and left the bound unarmed for seven minutes).

- 2026-08-27: T7 — the three parallel entry points are killed at 2 s under a session limit of 25 s (whole file 17 s); with the wrapper broken the same three cells take the session's 25 s and go red, so the timing is what they measure. The pipeline-build fan-out is recorded as no-kill.

- 2026-08-27: T8 — `_pkgdown.yml` gains a "Bounding a run" section holding `with_timeout` (`pkgdown::check_pkgdown()` clean), NEWS.md a new-features bullet, and the landing topic a wrapped-call paragraph beside the session-wide one.

- 2026-08-27: T9 — D051 appended: the per-call grain, D047's falsifier clause discharged with its rejection left standing, and the code-first argument order the gate chose.

- 2026-08-27: T10 — `devtools::check()` 0 errors / 0 warnings / 0 notes (2m18s); `devtools::document()` leaves no diff; `cairn_validate` passes with the plan's 8-criteria advisory unchanged. Status to review.

## Decisions

## Review

- 2026-08-27: sync — `git fetch`; `master` had not moved since the branch was
  cut (0 commits either way), so no merge was needed. Branch pushed; draft PR
  #76 opened.
- 2026-08-27: full suite, fresh — `devtools::test()`: **FAIL 0 | WARN 4 |
  SKIP 5 | PASS 6501**. The 4 warnings and 5 skips are all in files this diff
  does not touch (`test-audio-stream.R`, `test-ffmpeg.R`, `test-nvenc.R`,
  `test-video-codec.R`); no cell in `test-with-timeout.R` or
  `test-parallel-option-carry.R` was skipped or warned.

### Acceptance criteria

- AC1 — green. `test-with-timeout.R:34` ("expr is evaluated once, in the
  caller's frame, and its value returned") and `:54` ("the limit in force
  inside the call is `as.numeric(seconds)`") pass in the fresh run; `:64`
  covers the displaced-session case.
- AC2 — green. `test-with-timeout.R:83` (previously-set) and `:97`
  (previously-unset) each drive the three exits, and `:116` asserts each exit
  path signals what its name says. All pass.
- AC3 — green. `tm_spawn_sites()` is present in
  `helper-timeout-sweep.R:66`; `test-with-timeout.R:17` asserts the recorded
  four (`ffmpeg`, `ffprobe`, `mediainfo`, `run_program`) and `:24` proves
  membership is read off the body. `:222` drives one call per member with the
  session option unset and `:246` asserts `ffm_batch()`'s up-front
  `resolve_timeout()` reads the same value. All pass.
- AC4 — green. `test-with-timeout.R:310` ("a per-call limit kills a hung
  program with no session limit set") **ran** in the fresh `devtools::test()`
  run — it is absent from that run's five-skip list — and passed: the FIFO-
  anchored FFmpeg aborts `tidymedia_timeout` naming "2 seconds" inside the
  cell's own 60 s bound, with the session option absent afterwards. (Run
  standalone via `testthat::test_file()` it skips on `skip_on_cran()`, which is
  why the devtools run is the evidence.)
- AC5 — green. The three timed cells in `test-parallel-option-carry.R`
  (`ffm_batch` worker, `probe_all` worker, two-pass loudnorm worker) each pass
  under a session limit of 25 with a per-call 2, and the fourth cell ("every
  parallel fan-out is accounted for under a per-call limit") passes, recording
  the pipeline-build site as no-kill.
- AC6 — green. `test-with-timeout.R:147` scores the 13-value probe vector
  against `resolve_timeout()`'s verdict and asserts both verdicts occur;
  `:163` asserts the marker file is absent on every refusal; `:178` asserts
  the message names `seconds` and not `tidymedia.timeout`; `:188` refuses a
  missing `seconds` and `NULL`. All pass.
- AC7 — green. `cairn/DECISIONS.md` carries **D051**, whose header names the
  supersession of D047's session-grain falsifier clause and states the
  rejection stands. `man/with_timeout.Rd` renders with a binary-free example
  (`with_timeout(getOption("tidymedia.timeout"), 30)`); `R CMD check`'s
  "checking examples" step is OK. `_pkgdown.yml` carries a "Bounding a run"
  section holding `with_timeout`, and `pkgdown::check_pkgdown()` reports "No
  problems found." `NEWS.md` has the entry. `devtools::document()` re-run at
  review leaves no diff (`git status` clean apart from this file).
- AC8 — green. `devtools::test()` FAIL 0 (above); `devtools::check()` re-run
  at review: **0 errors, 0 warnings, 0 notes** (2m19s, `Status: OK`),
  `testthat.R` OK in 66s.

### Consistency gate

- `cairn_validate.py` — exit 0, all checks pass; one advisory (`sizing`:
  M072's 8 acceptance criteria against the 7 tripwire), unchanged from plan.
- `cairn_impact.py` — skipped: no DESIGN principle changed (`Principles
  touched: —`).
- Toolchain (`r-package` profile `consistency-gate`): `devtools::document()`
  no diff — pass. `pkgdown::check_pkgdown()` — pass. `NEWS.md` entry for the
  user-visible change — present, no milestone numbers. `README.Rmd`/`README.md`
  — untouched by the diff. No new top-level files, so no `.Rbuildignore`
  entries needed.

### Independent review

Full three-lens fan-out (executable surface touched, user-facing tier).
Blame-history lens and prior-PR-comments lens each reported **no findings**
(the latter located four relevant archived `## Review` records and found the
GitHub inline-comment surface empty via the probe). The diff-bug lens reported
nine, ranked; each is logged with its disposition below.

### Findings and disposition

Diff-bug lens, in the order it ranked them:

- **F1. `?with_timeout` points readers at documentation that does not exist.**
  `R/timeout.R:67-69` (and `man/with_timeout.Rd:38-40`) says what a reached
  limit does is "described in `vignette("tidymedia")` and under 'Bounding a
  run that hangs' in `[tidymedia-package]`". The second half is right; the
  first is not — `grep -rin timeout vignettes/` returns nothing across all
  four vignettes. Confirmed at review. Disposition: **fixed now** — the
  vignette clause dropped from the roxygen `@details`, `document()` re-run.
- **F2. The code comment justifying the `seconds` check overstates the
  equivalence.** `R/timeout.R:96-97` claims "this function accepts exactly the
  values the option accepts", which is false for `NULL`:
  `options(tidymedia.timeout = NULL)` removes the name and `resolve_timeout()`
  then returns the `0` default, while `with_timeout(expr, NULL)` aborts. The
  behaviour is the one AC1 and AC6 require; only the comment's universal claim
  is wrong. Disposition: **fixed now** — the comment now states the `NULL`
  exception.
- **F3. A missing `expr` produces a bare base-R error rather than a cli one.**
  `with_timeout(seconds = 5)` writes the option, then fails with `argument
  "expr" is missing, with no default` from the promise force; `on.exit`
  restores, so nothing leaks. A missing `seconds` is handled by
  `check_number_whole()`, so the two arguments differ. AC6 covers only
  `seconds`, so this is not an AC failure. Disposition: **follow-up** —
  ROADMAP candidate row.
- **F4. AC6's headline agreement test is near-tautological.**
  `test-with-timeout.R:147` scores each probe by `resolve_timeout()` under
  `with_options()` and by `with_timeout(NULL, v)`, but both bottom out in the
  same `rlang::check_number_whole(v, min = 0)` call, so agreement holds by
  construction; the `any(verdicts)` guard proves only that the vector is
  mixed. Disposition: **rejected** — the shared check is the mechanism AC6
  asks for ("accepts exactly those values `resolve_timeout()` accepts"), and
  the cell would still redden if `with_timeout()` coerced before checking,
  which is the drift it is placed against.
- **F5. The fan-out inventory test is line-insensitive and can
  misattribute.** `test-parallel-option-carry.R:530-548` computes
  `basename:lineno` for each `furrr::future_` hit, then asserts only the
  basename set and the count, discarding the line numbers; a comment
  containing the string, or a simultaneous add-and-remove across two files,
  would not redden it. Disposition: **rejected** — the grep-and-count shape
  is M071's, extended rather than weakened here, and tightening it is work on
  an inherited instrument rather than a defect in this diff.
- **F6. `tm_release_fifo()` leaves an unreaped background shell behind on the
  passing path.** `test-with-timeout.R:294-306` starts `(sleep 90; ...)` with
  `wait = FALSE`; on the normal ~2 s path that subshell outlives the suite by
  ~88 s. Output is redirected and the `[ -p ]` guard covers the deleted
  tempfile, so it is harmless, but nothing defers a kill. Disposition:
  **follow-up** — folded into the same ROADMAP candidate row as F3.
- **F7. AC5's parallel cells assert wall-clock time with a 20 s budget against
  a 2 s kill.** `tm_per_call_budget <- 20`. Disposition: **rejected** —
  intentional and reasoned in the test's own comment: the budget has to sit
  between the 2 s per-call kill and the 25 s session limit, which is the gap
  that discriminates, and the harness pre-warms the cluster before timing.
- **F8. `tm_reaches_spawn()`'s seed order changed as a side effect.**
  `tm_spawn_sites()` applies `sort()`, which the inlined version did not.
  No caller depends on the order (checked). Disposition: **rejected** —
  informational; a sorted set is the right shape for the set-equality
  assertion the helper now serves.
- **F9. `with_timeout` sits outside D014's naming scheme with nothing recorded
  about it.** D014 governs `verb_object`, `ffm_*`, `get_*`/`probe_*`/
  `mediainfo_*` and an argument vocabulary; `with_timeout(expr, seconds)` is
  in none of those families, and D051 records only the argument order as the
  irreversible half. Disposition: **fixed now** — D051 gains a sentence
  naming the name as deliberately outside D014's families, borrowed from
  `withr`/`R.utils` rather than coined.

Blame-history lens: no findings. Prior-PR-comments lens: no findings (four
archived `## Review` records located and checked; the GitHub inline-comment
surface probed and empty).

Return floor: no finding demonstrates an acceptance criterion failing, and
none is a load-bearing defect in what the package does for its users — F1 is
a wrong cross-reference in shipped help text, fixed on the branch rather than
returned. Status stays `review`.
