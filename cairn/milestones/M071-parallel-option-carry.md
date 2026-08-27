# M071: A parallel worker sees the settings the caller set

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m071-parallel-option-carry` / https://github.com/jmgirard/tidymedia/pull/75

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

- [x] AC1 With a two-worker `future::multisession` plan, `options(tidymedia.timeout = 1)`
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
- [x] AC2 With a two-worker `future::multisession` plan and a `hardware = "nvenc"`
      batch built at `parallel = TRUE`: with `options(tidymedia.nvenc_encoders =
      "h264_nvenc")` set in the parent, no worker spawns FFmpeg to ask for the encoder
      list and the compiled commands equal those `parallel = FALSE` produces; with the
      option unset, the same batch shows one encoder-list spawn per worker.
- [x] AC3 Under a two-worker `future::multisession` plan and a limit the spawned
      program exceeds, `ffm_batch(parallel = TRUE, run = TRUE)` over an N-row jobs
      table marks all N rows `success = FALSE` and signals one condition of class
      `tidymedia_batch_timeout` stating that the limit killed N jobs.
- [x] AC4 A worker whose `tidymedia.timeout` and `tidymedia.nvenc_encoders` held
      values of their own before a carried fan-out holds those same values after it,
      in the same process, both when the mapped call returns and when it raises an
      error.
- [x] AC5 Each of four invalid `tidymedia.timeout` values — `0.5`, `-1`, `NA`, `"2"`
      — is refused by `ffm_batch()` before any job is dispatched, with the same
      condition at `parallel = TRUE, run = FALSE` as at `parallel = FALSE,
      run = TRUE`, and with no worker having executed `.f`.
- [x] AC6 Neither the "Bounding a run that hangs" section of `?tidymedia` nor the
      development-version entry in `NEWS.md` states that `parallel = TRUE` workers do
      not see the limit; each states instead that tidymedia's own `parallel = TRUE`
      paths are bounded by the same limit as their sequential paths.
      `refresh_ffmpeg_capabilities()`'s documentation states that the caller's encoder
      override reaches a worker, and conditions its existing "asks FFmpeg W times"
      sentence on that override being unset.
- [x] AC7 `Rscript -e 'devtools::test()'` is clean and `Rscript -e 'devtools::check()'`
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
- [x] T7 AC2's override case and its option-unset control, and AC3's warning over an
      N-row table. The override case and AC3's warning red on master; the
      option-unset control is a control, so it is green on both.
- [x] T8 AC4's restoration: stamp a per-worker sentinel keyed by `Sys.getpid()`
      before the fan-out and assert per-PID equality after, for both options and both
      the returning and erroring mapped call. AC5's four invalid values on both paths,
      asserting the fake's log is empty.
- [x] T9 Docs: the two `?tidymedia` sentences, the `NEWS.md` development entry, and
      `refresh_ffmpeg_capabilities()`'s paragraph plus the internal comment at
      `R/cache.R:11`; `devtools::document()` no diff.
- [x] T10 D-entry: supersedes D047's "Disclosed, not fixed" bullet, and supersedes
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

- 2026-08-26: T7 — AC2's override case, its option-unset control, and AC3's N-row warning. Red without the wiring: the override case sees `-encoders` in the fake's log, and AC3 gets three `success = TRUE` rows and no warning. The option-unset control is green on both, which is what a control is for — task wording amended to say so (minor).

- 2026-08-27: T8 — AC4's per-PID restoration (returning and erroring mapped calls, both options) and AC5's four invalid values on both branches. 65 assertions in the file, no skips. AC5 red without `ffm_batch()`'s up-front resolve: the `parallel = TRUE, run = FALSE` call returns a tibble instead of a condition.

- 2026-08-27: T9 — `?tidymedia` no longer says workers miss the limit (both sentences rewritten), `NEWS.md`'s development entry likewise plus a new bullet for the carry and the up-front refusal, `refresh_ffmpeg_capabilities()` conditions its "asks FFmpeg W times" sentence on the override being unset, and `R/cache.R`'s comment separates the carried override from the uncarried memo. `devtools::document()` no diff; suite 6396 pass / 0 fail / 5 skips (the same five as before this milestone).

- 2026-08-27: T10 — D050 appended: supersedes D047's "Disclosed, not fixed" bullet and D044's seeding rejection, states why re-establishing the caller's value is not the package authoring one, and keeps D044's capability-memo gap disclosed. `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-08-27: the harness first used `parallelly::makeClusterPSOCK()`, which `check()` flagged as an undeclared dependency. Swapped to base R's `parallel::makePSOCKcluster()` — same fresh cluster, no dependency change, so no gate was owed.
- 2026-08-27: all tasks done; status set to review.

## Decisions

## Review

Reviewed 2026-08-27 on branch `m071-parallel-option-carry`, PR
https://github.com/jmgirard/tidymedia/pull/75. Branch cut from `origin/master`
d7e09f4 and `origin/master` has not moved since (merge-base equals its tip), so
no merge was owed before gathering evidence.

Suite-wide evidence for every criterion below comes from one
`Rscript -e 'devtools::test()'` run: **6396 pass / 0 fail / 5 skips**, the five
being the pre-existing `nvenc encoder not listed` hardware skips in
`test-nvenc.R` and `test-video-codec.R`. `test-parallel-option-carry.R` run on
its own reports **65 assertions, 0 skips**, which is T5's evidence clause met:
AC1-AC5's cases executed rather than skipped.

### Acceptance criteria

- **AC1 — the limit reaches the worker at each site, in that site's own shape.**
  Three cases, all green in the run above. `ffm_batch(parallel = TRUE,
  run = TRUE)` over a 2-row table under `tidymedia.timeout = 1` returns
  `success = c(FALSE, FALSE)` and the fake binary's log shows an `ffmpeg`
  invocation, so the FALSE is a kill and not some other failure;
  `probe_all(parallel = TRUE)` warns matching `timed out rather than being
  unreadable` and returns a 2-row container carrying only the `file` column;
  `normalize_audio_batch(two_pass = TRUE, parallel = TRUE)` aborts with class
  `tidymedia_timeout`. The site-completeness guard re-derives the fan-out list
  by grepping `furrr::future_` over `R/` at run time, asserts the list is
  non-empty, and asserts it equals the four-entry case table — the fourth site
  (the pipeline build) routed to AC2 exactly as the criterion says.

- **AC2 — the encoder override reaches the worker.** With
  `tidymedia.nvenc_encoders = "h264_nvenc"` set in the parent, a 4-row
  `hardware = "nvenc"` build at `parallel = TRUE` leaves no `-encoders`
  invocation in the fake's log, and `par$command` equals `seq$command` from the
  same build at `parallel = FALSE`; the commands are separately asserted to
  name `h264_nvenc`, so equality is not equality-of-two-wrong-answers. The
  option-unset control on the same fixture logs exactly 2 `-encoders` probes on
  a two-worker plan — one per worker.

- **AC3 — one batch condition naming the count.** A 3-row table under
  `tidymedia.timeout = 1` at `parallel = TRUE, run = TRUE` returns
  `success = c(FALSE, FALSE, FALSE)`; a `withCallingHandlers` collector on class
  `tidymedia_batch_timeout` catches exactly one condition, whose message matches
  `3 jobs timed out`.

- **AC4 — the worker's own settings come back.** Each worker stamps
  `tidymedia.timeout = 5` and a PID-keyed encoder string, readings are indexed
  by `Sys.getpid()` so two fan-outs compare worker-for-worker rather than
  position-for-position, and at least two distinct workers are asserted present.
  During the carried fan-out every worker reads the parent's `1` /
  `"parent_only"` — so the restoration claim is about a carry that happened —
  and the per-PID readings after the fan-out equal those before, both when the
  mapped call returns and when it raises.

- **AC5 — a bad limit is refused before dispatch.** All four values (`0.5`,
  `-1`, `NA`, `"2"`) refused at `parallel = TRUE, run = FALSE` and at
  `parallel = FALSE, run = TRUE` with identical condition class vectors and
  identical messages. The pipeline builder writes a marker file when it runs;
  the marker does not exist afterwards and the fake binary's log is empty, so no
  worker executed `.f` and no program was spawned.

- **AC6 — the docs no longer disclose a gap that is closed.**
  `grep -rn` over `R/`, `NEWS.md` and `man/` finds no surviving "workers do not
  see the limit" claim. `?tidymedia`'s two sites now read "at `parallel = TRUE`
  no differently from sequentially" and "tidymedia's own `parallel = TRUE` paths
  are bounded by the same limit as their sequential ones"; the `NEWS.md`
  development entry carries the same wording plus a new bullet for the carry and
  the up-front refusal. `?refresh_ffmpeg_capabilities` states that the caller's
  override is carried into each worker and the worker's own value put back, and
  its "asks FFmpeg `W` times" sentence is now conditioned on "unless you have set
  `tidymedia.nvenc_encoders` yourself".

- **AC7 — clean test and check.** `devtools::test()` 6396 pass / 0 fail /
  5 skips (above). `Rscript -e 'devtools::check()'` — Status: OK, **0 errors /
  0 warnings / 0 notes**, 2m 23.1s, including the vignette re-build and both
  test files under `R CMD check`.

### Consistency gate

- `cairn_validate.py` — exit 0, all checks passed; no advisory fired, including
  `release window`.
- `cairn_impact.py` — not run: the milestone touches no DESIGN principle
  (`Principles touched: —`), so the check no-ops.
- `devtools::document()` — no diff (working tree clean but for this milestone
  file).
- `pkgdown::check_pkgdown()` — "No problems found."
- Generated files — `NAMESPACE`, `man/` regenerate; the no-diff `document()`
  run above is the check. `README.Rmd` untouched, so no re-knit owed.
- `NEWS.md` — carries an entry for this milestone's user-visible changes, with
  no milestone numbers in the user-facing text.
- New top-level files — none; nothing owed to `.Rbuildignore`.
- `devtools::check()` — 0 errors / 0 warnings / 0 notes (AC7 above).

### Independent fresh-context review

Executable surface touched and the tier is user-facing, so the full three-lens
fan-out ran.

**[S] blame-history — no findings.** D044's and D047's disclosed gaps are closed
through a new D-entry that names itself as superseding them rather than
silently, D044's "no worker-side option writes" objection is honored (the
carried value originates with the caller and is withdrawn on exit, including on
the error path), the capability memo is left untouched exactly as D044 has it,
and all four fan-out sites are wired identically against D033's site inventory.

**[S] prior-review record — no prior-review evidence contradicted.**
`gh api .../pulls/comments` returns `[]`, so the GitHub surface holds nothing to
walk; the archive's `## Review` sections summarize outcomes without preserving
per-file finding text. Three `LESSONS.md` lines whose shape bears on this diff
were checked rather than assumed clear — M53's furrr/`load_all()` trap (met by
the harness's carrier-source fingerprint check), the M67 memo-counting trap (met
by a fresh cluster per test), and M41's front-door guard-precedence rule (the new
`resolve_timeout()` sits after all eight existing checks).

**[O] diff-bug — ten findings**, listed below in the reviewer's own severity
order. The reviewer also cleared a suspected serialization blow-up:
`carry_options()`'s `call` argument stays an unforced promise on the success
path, so the wrapper serializes to 0.065 MB with a 9 MB jobs table in scope.

### Findings and dispositions

Ten from [O], in the reviewer's own order. F1, F2 and F6 were reproduced at the
gate before triage rather than taken on the reviewer's account. No finding
demonstrated an acceptance criterion failing, so the step-5 return floor did not
fire; the maintainer triaged at the gate and status stayed `review`.

**F1 — FIXED. The carrier authors a `tidymedia.timeout` value the caller never
set, which contradicts D050 and can silently disable a worker's own limit.**
`carried_option_values()` carries `resolve_timeout()`, which returns `0` when the
option is unset, so `options()` in the worker always installs a concrete
`tidymedia.timeout = 0`. D050 states "Nothing here originates with the package:
the value installed in the worker is the caller's own", and the comment above
`carry_options()` states "a name unset in the parent is unset in the worker for
the duration of the call — one rule, no split behavior". Both are false for this
seam. Reproduced: with the option unset in the parent,
`carried_option_values()` returns `$tidymedia.timeout: num 0` beside
`$tidymedia.nvenc_encoders: NULL`. Fixed as a record correction rather than a
behavior change — the displacement is what D050's falsifier already names, so
what was wrong was the claim: D050 gains a paragraph stating that the no-limit
sentinel is the one value the package chooses, that the two seams are therefore
asymmetric, and that a worker's own plan-hook limit is *removed* rather than
changed for the duration; the two carrier comments say the same.

**F2 — FIXED. The new fan-out abort names an internal function, against the
blame rule the same file guards.** `carry_options()` defaults
`call = rlang::caller_env()`, which at `R/ffprobe.R:124` is `probe_all_impl()`'s
frame. `R/ffprobe.R:93-95` carries an explicit comment that an error here "would
read `Error in probe_all_impl()` and name a function the caller has no way to
reach (M64/M65's blame rule)", and every other check in that function threads
`call = call`. `R/loudnorm_two_pass.R:197` has the same shape. Reproduced before
the fix: `probe_all(..., parallel = TRUE)` under `tidymedia.timeout = 0.5` gave
`conditionCall` `probe_all_impl(infile, typed, parallel)`. Fixed by
`carry_options(probe_one, call = call)` and a `call = rlang::caller_env()`
parameter on `run_loudnorm_analysis_batch()`. Measured after: the two calls now
read `probe_all(c("a.mp4", "b.mp4"), parallel = TRUE)` and
`normalize_audio_batch(j, two_pass = TRUE, parallel = TRUE)`.

**F3 — FIXED. `?tidymedia` overclaims the up-front refusal.** The new paragraph
ended "A limit the underlying `timeout=` could not use ... is refused before any
job is dispatched, on either path", where only `ffm_batch()` refuses before
dispatch — `probe_all(parallel = FALSE)` still reaches the per-file spawn first.
`NEWS.md` scopes the same claim correctly. Fixed by naming `ffm_batch()` in the
sentence and scoping "either path" to its two branches.

**F4 — FOLLOW-UP. AC2's control test hard-codes a probe count that depends on
furrr's default chunking.** `expect_equal(length(probes), 2L)` over 4 jobs
assumes future hands at least one job to each of the two workers. T5 promises
`chunk_size = 1`, but `ffm_batch()`'s internal maps take no `.options`, so that
setting reaches only the harness's own probe maps. Green today; a scheduling
assumption, not an asserted invariant.

**F5 — FOLLOW-UP. The fan-out domain guard cannot detect an unwired site.** It
compares only the set of file basenames containing `furrr::future_` plus a total
count of 4, so deleting `carry_options(...)` from any of the four sites leaves it
green. Only the behavioral AC1 tests catch an unwiring, and those skip on
Windows, on CRAN, and without furrr.

**F6 — FIXED. The harness errors rather than skips when a worker cannot load the
namespace.** `tm_carry_fingerprint()` returns `NA_character_` on a failed
`asNamespace()`; `all(fingerprints == ...)` is then `NA`, and
`if (!all(...))` raises "missing value where TRUE/FALSE needed" instead of taking
the intended skip. Fixed with `isTRUE()`.

**F7 — FOLLOW-UP. Weak condition assertions on the refusal paths.** Both the
carrier-build test and the AC5 test assert only `rlang_error` plus class-and-
message equality between the two branches, so a regression failing both branches
identically for an unrelated reason would pass. Asserting the message or `arg`
would close it.

**F8 — FOLLOW-UP. AC1's `probe_all` case does not assert what the AC asks.** AC1
requires "one end-of-call warning naming the file as timed out rather than
unreadable"; the test matches only `regexp = "timed out rather than being
unreadable"` and never checks a filename appears in the warning.

**F9 — FOLLOW-UP. Under a sequential plan with `parallel = TRUE`, the carrier now
writes options into the caller's own session.** tidymedia supports that
combination (warn and carry on), and there `carry_options()` runs in-process: it
installs the resolved values and reverts them on exit, so any `options(tidymedia.*)`
the user's own `.f` sets during the batch is silently rolled back. D050's "the
sequential branches are untouched" does not cover it.

**F10 — FIXED. Ragged roxygen rewrap and a comma splice** at
`R/tidymedia-package.R:35-37`, where the replacement was dropped in without
rewrapping. Rewrapped, em dash restored.

**CI-1 (found red on CI after the fix-now push, fixed on the branch).** The
AC1 site-completeness guard failed three assertions on the `test-coverage`
runner (`length(sites)` 0 against 4), where all six `R CMD check` platforms
passed. Cause: `covr` runs the suite from an INSTALLED copy, whose `R/`
directory holds the lazyload database and no `.R` file, so the guard's
`dir.exists(r_dir)` skip condition was satisfied while the grep domain was
empty — it asserted about a domain it could not see instead of skipping. This
is the fragility F5 named, reaching a red build. Fixed by keying the skip on
finding the sources the guard actually greps (a non-empty `.R` listing
containing `ffm_batch.R`) rather than on the directory. Verified both ways: the
guard still executes under `devtools::test()` (65 assertions, 0 skips, so AC1's
site-completeness evidence stands), and against a simulated installed layout the
old condition would not have skipped where the new one does.

**Re-verification after the fix-now work and CI-1.** `devtools::test()` 6396 pass / 0 fail
/ 5 skips (the same five hardware skips); `devtools::check()` Status: OK,
0 errors / 0 warnings / 0 notes; `devtools::document()` no diff;
`cairn_validate.py` exit 0, all checks passed.

F4, F5, F7, F8 and F9 go to a grouped candidate row — instrument weaknesses in
this milestone's own carry harness, none a defect in shipped behavior, and the
same shape as the M70 guard-strength row they sit beside.

- 2026-08-27: review — every acceptance criterion verified with fresh evidence and every gate check green; three-lens fan-out returned ten findings from [O] and none from the two [S] lenses. F1, F2, F6 reproduced at the gate. No finding failed a criterion, so the return floor did not fire; maintainer triaged F1/F2/F3/F6/F10 fix-now and F4/F5/F7/F8/F9 to a candidate row. Fixes committed and re-verified before the approval marker.
- 2026-08-27: CI came back red on `test-coverage` after the fix-now push while all six `R CMD check` platforms passed — the AC1 site-completeness guard asserting over an empty domain because covr runs from an installed copy whose `R/` holds no `.R` files. Guard's skip re-keyed onto the sources it greps; suite and check re-run clean, approval re-requested.
