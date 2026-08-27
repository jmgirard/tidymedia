<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M073: The timeout wrapper's tail

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m073-timeout-wrapper-tail`

## Goal

Guard `with_timeout()`'s two arguments evenly, add the `local_*` half of the
pair, and stop the FIFO test helper leaving a process behind.

## Scope

**Surface tier: user-facing** — AC1–AC2 and AC5–AC7 change or add exported
behaviour and its docs; AC3–AC4 are an internal test-harness deliverable
carried inside a user-facing milestone.

**In:** (a) `with_timeout(seconds = 5)` is refused by the package rather than
by base R's `argument "expr" is missing, with no default`, so both formals are
guarded the same way (M072 review F3). (b) `tm_release_fifo()`
(`tests/testthat/test-with-timeout.R:295`) cancels the writer it starts, so no
process outlives the frame that called it (F6). (c) `local_timeout(seconds,
.local_envir)` is exported — the `local_*` half of withr's pair, bounding the
rest of the calling frame rather than a wrapped expression.

**Out:** per-verb `timeout =` arguments — still a ROADMAP candidate; D047's
rejection stands unsuperseded and D051 discharges only its grain clause.
Changing the documented `NULL` asymmetry between `seconds` and the option
(`with_timeout(expr, NULL)` refused, `options(tidymedia.timeout = NULL)`
accepted as unset) → not in this milestone; it is recorded behaviour, and
AC6 states it rather than altering it. The other M072-family candidate rows
(M071's five carry-harness findings, M70's eight guard-strength findings) stay
where they are.

## Acceptance criteria

- [ ] AC1. `with_timeout(seconds = 5)` signals an `rlang_error` whose
      `ansi_strip()`ed message names `expr` and does not contain the string
      `argument "expr" is missing, with no default`.
- [ ] AC2. Every formal of `with_timeout()` is guarded alike: for each name in
      `names(formals(with_timeout))`, a call omitting exactly that name and
      supplying every other formal with a valid value signals an `rlang_error`
      naming the omitted argument. The test derives its cases from `formals()`
      at run time, not from a written list. Regression clause (holds today,
      pinned so it keeps holding): after each such call the session limit is
      unchanged — unset beforehand reads `"absent"` after, `99` reads `99`.
- [ ] AC3. No process started by `tm_release_fifo()` outlives the frame that
      called it. The helper's command carries a unique marker token; a test
      asserts `pgrep -f <marker>` matches inside the frame and matches nothing
      within 5 s of the frame exiting, for three cases: a frame exiting by
      return, a frame exiting by abort, and a frame calling the helper twice.
      Skipped on Windows and on CRAN.
- [ ] AC4. The cancellation does not disarm the outer bound early: the
      FIFO-anchored kill cell still reaches its `tidymedia_timeout` abort, and
      the bound the helper provides still sits outside limit + 40 s (M69's
      lesson; a 2 s limit is 42 s worst case on Linux CI).
- [ ] AC5. `local_timeout(seconds, .local_envir = parent.frame())` is exported
      and bounds the rest of its calling frame: inside that frame after the
      call `getOption("tidymedia.timeout")` is `as.numeric(seconds)`, and on
      frame exit the caller's prior state is back — a previously-set `99` reads
      `99`, a previously-unset option reads absent. Probed on four axes: exit
      by return, exit by abort, two `local_timeout()` calls nested in one
      frame, and a non-default `.local_envir`.
- [ ] AC6. `local_timeout()` refuses `seconds` by the rule `with_timeout()`
      applies — `rlang::check_number_whole(seconds, min = 0, arg = "seconds")`
      — over the same value set `with_timeout()`'s own refusal test enumerates,
      plus `0` and `1` on the accepted side. The `NULL` asymmetry with the
      option seam is stated in the roxygen, not removed.
- [ ] AC7. Docs and record: a roxygen topic for `local_timeout()` cross-linked
      from `with_timeout()`, a `_pkgdown.yml` row under "Bounding a run", a
      NEWS.md bullet for each of AC1 and AC5, and a `cairn/DECISIONS.md` entry
      extending D051 that records the second export, its place outside D014's
      families, and that it discharges D051's own "a statement, not a wrapper"
      falsifier.
- [ ] AC8. `devtools::check()` reports 0 errors, 0 warnings and no notes not
      present on `master`; `devtools::test()` is green.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T2
- AC5 → T3, T4
- AC6 → T3, T4
- AC7 → T5
- AC8 → T6

## Tasks

- [x] T1. Test-first: add the `formals()`-derived missing-argument cases and
      the unchanged-session clause to `tests/testthat/test-with-timeout.R`;
      then guard `expr` in `with_timeout()` (`R/timeout.R`) with
      `rlang::check_required(expr)`, placed **above** the `options()` write so
      no unobservable ordering is left to chance.
- [x] T2. Rework `tm_release_fifo()` to a cancel-file poll carrying a unique
      marker: a loop checking for the cancel file each second, `withr::defer`
      touching it at frame exit. Add the three-case `pgrep -f` reaping test and
      re-run the FIFO-anchored kill cell for AC4.
- [x] T3. Test-first: write `local_timeout()`'s four-axis restore tests and its
      refusal tests, then implement it in `R/timeout.R` over
      `withr::defer(options(prior), envir = .local_envir)`.
- [x] T4. Verify the pair together — `local_timeout()` inside a
      `with_timeout()` and vice versa — so the LIFO of prior values is exercised
      rather than assumed.
- [x] T5. Roxygen topic + cross-links, `_pkgdown.yml` row, two NEWS bullets,
      `devtools::document()`, and the D-entry extending D051.
- [x] T6. `devtools::check()` and `devtools::test()`; record the note delta
      against `master`.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: criteria audit ran in FULL mode (surface tier user-facing) and returned nine findings; fixed here: AC3-as-drafted certified nothing (the unchanged-session state already holds, since `on.exit` restores before the base-R error escapes) and folded into AC2 as a labelled regression clause, AC2's "supplying the other" was arity-locked while its own `formals()` procedure was not, AC7-as-drafted claimed `seconds` is refused by "exactly the rule the option applies" which `R/timeout.R` explicitly denies for `NULL`, AC5-as-drafted ("the cell still passes") was an instrument restatement of AC8 and was cut with its real content moved to AC4, AC6's third exit route was mechanically identical to its second and was replaced by nesting and a non-default `.local_envir`, no D-entry was required though `local_timeout()` discharges D051's own falsifier so one was added, and AC9's "notes attributable to this milestone" named no attribution procedure; raised to the gate: AC4's PID mechanism, unreachable as drafted (`system(wait = FALSE)` returns the shell's exit status, not a PID) and satisfiable with the bug intact (killing the recorded subshell orphans its `sleep`, measured).
- 2026-08-27: plan gate chose a cancel-file poll over a pidfile plus process-group kill for the FIFO writer, because killing the recorded subshell leaves `sleep` orphaned (measured by the audit) and process-group signalling is the part that varies most across shells and platforms; falsified by a platform where the poll loop's own second-scale `sleep` children themselves outlive the suite. Shortening `after = 90` was rejected outright: M69's lesson puts the bound outside limit + 40 s.
- 2026-08-27: plan gate chose to ship `local_timeout()` now over leaving it a ROADMAP candidate, on the user's call and against the row's stated trigger — no request to bound a function body without wrapping one has arrived, and D051 names exactly that report as its falsifier; the trade taken is D014's pre-0.2.0 clean break, which keeps the export withdrawable. Falsified by nobody using it before 0.2.0, at which point the export becomes permanent unused surface.
- 2026-08-27: plan gate chose to promise the refusal's message shape over building machinery to observe the refuse-before-write ordering, because `on.exit` makes "never written" and "written and restored" indistinguishable to any caller; falsified by an ordering bug that changes what a caller sees.
- 2026-08-27: implementation gate chose to move `withr` from Suggests to Imports and undo `local_timeout()`'s change with `withr::defer()`, on the user's selection; the base-R `on.exit` alternative was measured to lose the restore silently when the calling frame writes its own `on.exit()` without `add = TRUE` (option left at the wrapper's value after the frame exited). Dependency change; D-entry at T5.
- 2026-08-27: T1 done — `rlang::check_required(expr)` added above the `options()` write in `with_timeout()`; `formals()`-derived guard cases plus the unchanged-session regression clause added to `tests/testthat/test-with-timeout.R`. Red first for the right reason (base R's `missingArgError`, the exact string AC1 forbids), green after. `devtools::test()` on the file: 97 pass, 0 fail.
- 2026-08-27: T2 done — `tm_release_fifo()` now polls for a per-call cancel file that `withr::defer()` touches when the arming frame exits, and returns a unique marker so a test can watch the process. Three-case `pgrep -f` reaping test added (return, abort, twice in one frame); discriminating against the old helper, whose "armed" control passes while the "gone" assertion fails. AC4 re-run: the FIFO-anchored kill cell still reaches its `tidymedia_timeout` abort, `after = 90` unchanged and still outside 2 + 40 s. `test-with-timeout.R` with `NOT_CRAN=true`: 112 pass, 0 fail, 0 skip.
- 2026-08-27: T2 found and fixed a shell-quoting trap while rewriting the helper: `system(wait = FALSE)` appends `&`, which binds to the LAST command of the string, so a multi-command poll loop without enclosing parentheses runs in the foreground and blocks R for the full `after` (measured 91.8 s against 1.1 s with them). The parentheses are now commented as load-bearing.
- 2026-08-27: T3 done — `local_timeout(seconds, .local_envir = parent.frame())` added to `R/timeout.R` over `withr::defer()`, exported and documented; `withr` moved from Suggests to Imports per the gate (D-entry at T5). `tests/testthat/test-local-timeout.R` covers the four restore axes and the refusal set; the shared probe vector moved to `tests/testthat/helper-timeout-probes.R` so both refusal tests score against one list. Red first with "could not find function local_timeout". Full suite with `NOT_CRAN=true`: 6618 pass, 0 fail.
- 2026-08-27: T4 done — three pair cells added. `local_timeout()` then `with_timeout()` in one frame unwinds cleanly (2 inside the wrapper, 5 after it, 99 after the frame). A `local_timeout()` written directly inside `with_timeout()`'s `expr` binds to the frame that wrote the call, so its undo runs after the wrapper's and leaves the wrapper's limit behind (measured: 2 after the frame, against 99 before). Pinned rather than fixed: withr's own `with_options`/`local_options` pair was measured doing the same thing, and the control is in the cell.
- 2026-08-27: minor amendment (discovered sub-task, no criterion changed): T4's crossing case is stated in `local_timeout()`'s roxygen `@details`, with the safe shape (put the inner limit in its own function) named.
- 2026-08-27: T5 done — `local_timeout()` topic written and cross-linked both ways with `with_timeout()`, `_pkgdown.yml` row added under "Bounding a run" (`pkgdown::check_pkgdown()`: no problems found), two NEWS bullets added (the statement form; the omitted-`expr` refusal), `devtools::document()` run, and D052 appended to `cairn/DECISIONS.md` extending D051 and recording the `withr` Suggests-to-Imports move.
- 2026-08-27: T6 done — `devtools::check()` on the branch: 0 errors, 0 warnings, 0 notes, so the note delta against `master` is empty by construction. `devtools::test()`: 6618 pass, 0 fail, 5 skip, 4 warn; all four warnings are the pre-existing dropped-audio-track messages in `test-audio-stream.R` and `test-ffmpeg.R`, files this branch does not touch.
- 2026-08-27: sizing tripwire fired at 8 acceptance criteria (>7) and was disposed here rather than by splitting: the eighth is the mandatory profile-check criterion, the six tasks are each well under a working session, and `local_timeout()` is ~10 lines plus a topic, so a second milestone would add tracking ceremony an order larger than the work it carries.

## Decisions

## Review
