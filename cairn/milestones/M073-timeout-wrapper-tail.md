<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M073: The timeout wrapper's tail

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m073-timeout-wrapper-tail` / https://github.com/jmgirard/tidymedia/pull/77

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

- [x] AC1. `with_timeout(seconds = 5)` signals an `rlang_error` whose
      `ansi_strip()`ed message names `expr` and does not contain the string
      `argument "expr" is missing, with no default`.
- [x] AC2. Every formal of `with_timeout()` is guarded alike: for each name in
      `names(formals(with_timeout))`, a call omitting exactly that name and
      supplying every other formal with a valid value signals an `rlang_error`
      naming the omitted argument. The test derives its cases from `formals()`
      at run time, not from a written list. Regression clause (holds today,
      pinned so it keeps holding): after each such call the session limit is
      unchanged — unset beforehand reads `"absent"` after, `99` reads `99`.
- [x] AC3. No process started by `tm_release_fifo()` outlives the frame that
      called it. The helper's command carries a unique marker token; a test
      asserts `pgrep -f <marker>` matches inside the frame and matches nothing
      within 5 s of the frame exiting, for three cases: a frame exiting by
      return, a frame exiting by abort, and a frame calling the helper twice.
      Skipped on Windows and on CRAN.
- [x] AC4. The cancellation does not disarm the outer bound early: the
      FIFO-anchored kill cell still reaches its `tidymedia_timeout` abort, and
      the bound the helper provides still sits outside limit + 40 s (M69's
      lesson; a 2 s limit is 42 s worst case on Linux CI).
- [x] AC5. `local_timeout(seconds, .local_envir = parent.frame())` is exported
      and bounds the rest of its calling frame: inside that frame after the
      call `getOption("tidymedia.timeout")` is `as.numeric(seconds)`, and on
      frame exit the caller's prior state is back — a previously-set `99` reads
      `99`, a previously-unset option reads absent. Probed on four axes: exit
      by return, exit by abort, two `local_timeout()` calls nested in one
      frame, and a non-default `.local_envir`.
- [x] AC6. `local_timeout()` refuses `seconds` by the rule `with_timeout()`
      applies — `rlang::check_number_whole(seconds, min = 0, arg = "seconds")`
      — over the same value set `with_timeout()`'s own refusal test enumerates,
      plus `0` and `1` on the accepted side. The `NULL` asymmetry with the
      option seam is stated in the roxygen, not removed.
- [x] AC7. Docs and record: a roxygen topic for `local_timeout()` cross-linked
      from `with_timeout()`, a `_pkgdown.yml` row under "Bounding a run", a
      NEWS.md bullet for each of AC1 and AC5, and a `cairn/DECISIONS.md` entry
      extending D051 that records the second export, its place outside D014's
      families, and that it discharges D051's own "a statement, not a wrapper"
      falsifier.
- [x] AC8. `devtools::check()` reports 0 errors, 0 warnings and no notes not
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
- 2026-08-27: review returned the milestone to in-progress (defect return 1). AC3 and AC8 failed on Linux CI: all three Ubuntu jobs of PR #77 report `Status: 1 ERROR` from `test-with-timeout.R`'s reaping cell — `[ FAIL 4 | WARN 4 | SKIP 9 | PASS 6609 ]`, the four failures being every `present = FALSE` assertion (`:416:3` return, `:426:3` abort, `:439:5` twice), while every `armed` assertion passes. macOS, Windows and the local macOS `devtools::check()` are green. AC1, AC2, AC4, AC5, AC6, AC7 stay verified. The [O] review lens's F6 (`tm_pgrep()` self-matching its own `sh -c` command line) predicts this signature and is the first thing to confirm. Two further CONFIRMED findings ride back with it: F1, the FIFO cancel file lives under `tempdir()` so a writer can outlive the R session by up to ~89 s (reproduced), and F2, D052's and `R/timeout.R:208-212`'s claim that `withr::defer()` cannot be clobbered is false — `defer` ends in `base::on.exit(thunk, TRUE, after)` and a caller's bare `on.exit()` discards the restore, measured on withr 3.0.3.

- 2026-08-27: defect return 1, cause confirmed on Linux before fixing (F6, not a cancel failure). A throwaway `ubuntu-latest` job ran the query and the writer loop side by side: `/bin/sh -c "pgrep -af tm_fifo_selfmatch_A"` returned its OWN pid (`2125 /bin/sh -c pgrep -af tm_fifo_selfmatch_A`) with no writer running at all, on dash and procps-ng 4.0.4 — while the emulated helper's writer was genuinely gone 4 s after the cancel file appeared (`ps -ef | grep` empty). So the cancellation works on Linux and `tm_pgrep()` simply could never return empty there, which is exactly the observed signature. Fix: `tm_pgrep()` queries `tm_[f]ifo_...`, the `grep -v grep` idiom — an ERE that matches the writer's literal marker but not the pattern text in the querying shell's own command line. The cell gains a non-vacuity guard that fails on the bug directly: a marker of the real shape that nothing ever started must match nothing. That guard cannot go red on macOS (whose `sh` execs the query away), which is why the evidence for AC3 is the CI run, not the local one. The temporary diagnostic workflow was deleted in this commit.
- 2026-08-27: F1 fixed in the same edit — the poll loop gains `[ -d <tempdir> ] || exit 0`, so losing the session's tempdir means what losing the cancel file means. Reproduced first: an `Rscript` that armed the helper, cancelled and exited left `sh -c (i=0; ...` in the process table 5 s after R was gone, because R removes `tempdir()` (and the cancel file with it) inside the poll's one-second window. No criterion changed; AC3's three cases all keep the session alive and do not reach this.
- 2026-08-27: implementation gate (F2) chose to keep `withr` in Imports and correct the record, on the user's selection. The premise D052 was taken on is false, measured here on withr 3.0.3: `withr::defer()` ends in `do.call(base::on.exit, list(thunk, TRUE, after), envir = envir)`, so `f <- function() { local_timeout(5); on.exit(invisible(NULL)); invisible(NULL) }` leaves the option at `5` where the caller had `99` (control without the `on.exit`: `99`). What `defer()` does buy is LIFO ordering (`after = FALSE`) and globalenv/knitr target handling, which is now what D052 and `R/timeout.R` say. The hole itself is documented in `local_timeout()`'s `@details` and the "by any route" promise is qualified in both the roxygen and NEWS.
- 2026-08-27: F7 fixed — `NEWS.md` gains a bullet for `withr` moving Suggests → Imports, an install-surface change every user sees. F3, F5, F8 and F9 are untouched and go to the gate on the next review pass, as the return said.
- 2026-08-27: local re-run after the fixes — `test-with-timeout.R` alone: 114 pass, 0 fail, 0 skip; `devtools::test()`: 6628 pass, 0 fail, 5 skip, 4 warn (the same pre-existing dropped-audio-track warnings in `test-audio-stream.R` and `test-ffmpeg.R`). AC3 and AC8 stay unticked until the Linux jobs of PR #77 are green.

- 2026-08-27: AC3 green on the platform that failed it. Run 33108301183 of PR #77, all five jobs success: `ubuntu-latest (release)` reports `[ FAIL 0 | WARN 4 | SKIP 9 | PASS 6615 ]` where the returned run reported `FAIL 4`, and `ubuntu-latest` devel and oldrel-1, macOS and Windows are green too. The four warnings are the same pre-existing dropped-audio-track messages.
- 2026-08-27: that run also surfaced a NOTE this milestone introduced, on Linux only — `checking tests` NOTE from `spelling.R`, whose `.Rout.save` comparison flagged the word `withr` in `local_timeout.Rd:53` and `NEWS.md:79`, both prose written for the F2 and F7 fixes above. The local macOS `devtools::check()` had not caught it. `withr` added to `inst/WORDLIST`; `spelling::spell_check_package(".")` now reports "No spelling errors found". AC8 is ticked against the CI run following this fix, not the one that carried the NOTE.

## Decisions

## Review

Reviewed 2026-08-27 on `m073-timeout-wrapper-tail`, PR
https://github.com/jmgirard/tidymedia/pull/77. `master` had not moved since the
branch was cut (`git rev-list --left-right --count master...origin/master` = 0/0;
branch 6 ahead, 0 behind), so no merge-forward was needed and the evidence below
is from the branch as it stands.

### Acceptance-criteria evidence

- **AC1 — verified.** `with_timeout(seconds = 5)` in a fresh session signals a
  condition of class `rlang_error`; the `ansi_strip()`ed message reads
  ``` `expr` is absent but must be supplied. ``` — it contains `expr` and does
  not contain `argument "expr" is missing, with no default`. Pinned by
  "an omitted expr is refused by the package, not by base R" in
  `tests/testthat/test-with-timeout.R`.
- **AC2 — verified.** `names(formals(with_timeout))` is `c("expr", "seconds")`
  (measured, not read from a list). The test
  "every formal of with_timeout() is guarded alike" derives its cases from
  `formals()` at run time, asserts the map covers the real signature
  (`expect_setequal`) and that there is more than one formal, and for each
  omitted name signals an `rlang_error` whose stripped message names it. The
  regression clause runs against both prior states: unset reads `"absent"`
  after, `99` reads `99`. Green in the run below.
- **AC3 — verified.** `tm_release_fifo()` returns a per-call marker token
  carried in the command line; the test "no process tm_release_fifo() starts
  outlives the frame" asserts `pgrep -f <marker>` matches inside the frame
  (an explicit armed-check, so the gone-assertion cannot pass vacuously) and
  matches nothing within 5 s of the frame exiting, for a frame exiting by
  return, one exiting by abort, and one arming the helper twice (two distinct
  markers asserted). Guarded by `skip_on_cran()`, `skip_on_os("windows")` and a
  `pgrep`-on-PATH skip. It ran here rather than skipping: the timeout file pair
  reported 0 skips.
  **Falsified on Linux CI — AC3 is NOT verified and its box is unticked.** All
  three Ubuntu jobs of https://github.com/jmgirard/tidymedia/pull/77 fail this
  test: `[ FAIL 4 | WARN 4 | SKIP 9 | PASS 6609 ]`, the four failures being
  every `present = FALSE` assertion in the cell —
  `test-with-timeout.R:416:3` (return), `:426:3` (abort) and `:439:5` twice
  (both markers of the twice-in-one-frame case). Every `armed`
  (`present = TRUE`) assertion passes. macOS and Windows are green; the local
  macOS run above is green. This is M69's lesson shape exactly: green on the
  dev machine, red only on Linux.
- **AC4 — verified.** `after = 90` is unchanged at
  [test-with-timeout.R:342](tests/testthat/test-with-timeout.R:342), outside
  2 + 40 s. The FIFO-anchored cell "a per-call limit kills a hung program with
  no session limit set" still reaches its `tidymedia_timeout` abort, still
  names FFmpeg and "2 seconds", still completes under its 60 s budget, and
  still leaves the session unbounded. Green in the run below.
- **AC5 — verified.** `local_timeout` is in `getNamespaceExports("tidymedia")`
  and `NAMESPACE:59`; its signature is
  `function(seconds, .local_envir = parent.frame())`. Measured directly, four
  axes, each against both a previously-unset and a previously-`99` option:
  exit by return (inside `7`, after `absent` / `99`), exit by abort (after
  `99`), two calls nested in one frame (inside `4` — the second displaces the
  first — after `99`), and a non-default `.local_envir` naming an outer frame
  (still in force after the inner call returns, gone when the outer frame ends).
  All four are also cells in `tests/testthat/test-local-timeout.R`, green below.
- **AC6 — verified.** Scored every value in the shared probe vector
  (`tests/testthat/helper-timeout-probes.R`: `0, 1L, 60, 0.5, -1, NA,
  NA_real_, "2", c(1, 2), Inf, TRUE, integer(0), factor("2")`) against the
  option's own verdict: 13/13 agree, with both verdicts occurring (3 accepted,
  10 refused), so the agreement is not vacuous. `0` and `1` are accepted and
  the limit in force is `as.numeric(seconds)`. The `NULL` asymmetry is intact
  and stated, not removed: the option accepts `NULL` (it removes the name),
  `local_timeout(NULL)` refuses, and `man/local_timeout.Rd:56-58` says so.
- **AC7 — verified.** `man/local_timeout.Rd` exists and cross-links
  `with_timeout()` both ways (`man/with_timeout.Rd:56` → `local_timeout()`,
  `man/local_timeout.Rd:30` → `with_timeout()`); `_pkgdown.yml` gains a
  `local_timeout` row under the "Bounding a run" section; `NEWS.md` gains one
  bullet for `local_timeout()` (AC5) and one for the omitted-`expr` refusal
  (AC1); `cairn/DECISIONS.md` gains D052, which extends D051 without
  superseding it, records the second export and its place outside D014's
  families, records that it discharges D051's "a statement, not a wrapper"
  falsifier, and additionally records the `withr` Suggests-to-Imports move.
- **AC8 — verified.** `devtools::check()` on the branch: `Status: OK`,
  0 errors / 0 warnings / 0 notes, 2m20.6s, tests run inside the check
  (`testthat.R [63s/119s]`). With zero notes on the branch, the note delta
  against `master` is empty by construction. `devtools::test()`: 6626 pass,
  0 fail, 5 skip, 4 warn — all four warnings are the pre-existing
  dropped-audio-track messages in `test-audio-stream.R` and `test-ffmpeg.R`,
  neither file touched by this branch.
  **Not verified: the same check is an ERROR on Linux CI.** `R CMD check` on
  all three Ubuntu jobs reports `Status: 1 ERROR` from the AC3 test failures
  above. AC8 names no platform, so the local macOS result does not discharge
  it. Box unticked.

### Consistency gate

- `cairn_validate.py`: exit 0, all checks passed. One advisory —
  `sizing (split tripwires)`, M073 at 8 acceptance criteria — which is an
  advisory, not a gate failure, and is disposed in the work log (the eighth is
  the mandatory profile-check criterion).
- `cairn_impact.py`: skipped, no DESIGN.md principle changed
  (`git diff origin/master...HEAD -- cairn/DESIGN.md` is empty; the header's
  `Principles touched` is `—`).
- `r-package` profile `consistency-gate` slot: `devtools::document()` produces
  no diff (`git status --porcelain` clean afterwards apart from this milestone
  file); `NAMESPACE` and `man/` regenerate rather than being hand-edited, which
  that no-diff run confirms; `README.Rmd`/`README.md` are untouched by the
  branch and in sync; `pkgdown::check_pkgdown()` reports "No problems found";
  `NEWS.md` (the declared changelog) carries entries for both user-visible
  changes, with no milestone numbers in the user-facing text; no new top-level
  files, so no `.Rbuildignore` entries were needed and `check()` raised no NOTE;
  the full `devtools::check()` is clean as recorded under AC8.

### Independent review

Full three-lens fan-out — the diff touches executable surface (`R/`, `tests/`)
and the declared tier is user-facing. Each lens ran fresh-context on its own
evidence base.

**[S] blame-history lens — no findings.** Ran `git log`/`git blame` over the
modified regions of `R/timeout.R`, `test-with-timeout.R` and `DESCRIPTION`
against D047–D052 and `cairn/LESSONS.md`. Confirmed: the check ordering matches
T1's instruction and D047/D051's refuse-before-run rule; the `withr` promotion
is required by `local_timeout()` rather than a stray bump (no other `withr::`
use in `R/`); `after = 90` is unchanged and M69's limit + 40 s lesson intact;
the `NULL` asymmetry is stated, not silently changed; and leaving
`with_timeout()` on base `on.exit()` is consistent, since it defers only into
its own frame.

**[S] prior-review lens — no findings.** Archived `## Review` sections for M69,
M70, M071 and M072 are the primary surface. This diff is the fix for M072's
deferred F3 and F6, not a regression of them; M072's fixed F2 (a comment
overstating the `seconds`/option equivalence) is not repeated, since the new
roxygen carves out `NULL` explicitly; M70's eight deferred findings touch files
this diff does not. The GitHub probe
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`, so
no real inline review threads exist anywhere in the repo and the per-PR walk was
correctly skipped.

**[O] diff-bug lens — nine findings, ranked as reported.** Each verified below
against the implementation rather than against the reviewer's account.

- **F1 (CONFIRMED). `tm_release_fifo()`'s cancellation is not sticky: the writer
  can outlive the R session by up to ~89 s.** The cancel file lives under
  `tempdir()`, which R removes at session exit, and the shell re-reads it only
  once a second — so if R exits inside that one-second window the file is gone
  before the loop sees it and the poll runs its remaining iterations.
  Reproduced here deterministically: an `Rscript` that arms the helper, touches
  the cancel file and exits immediately left `sh -c (i=0; while [ $i -lt 90 ]…`
  in the process table 5 s after R was gone. AC3's three cases do not catch it
  because all three keep the session alive. Named fix: a
  `[ ! -d <tempdir> ] && exit 0` clause in the loop, or a cancel path outside
  the session tempdir.
- **F2 (CONFIRMED). The stated reason for making `withr` a hard dependency does
  not hold.** `R/timeout.R:208-212` and D052's last paragraph both say
  `withr::defer()` "keeps its own handler stack, so it cannot be clobbered".
  It does not: `withr::defer` ends in
  `do.call(base::on.exit, list(thunk, TRUE, after), envir = envir)`, and the
  cited failure mode reproduces through it. Measured on withr 3.0.3 —
  `f <- function() { local_timeout(5); on.exit(invisible(NULL)); invisible(NULL) }`
  leaves the option at `5` where the caller had `99`; `withr::local_options()`
  loses it identically, and so does a bare base `on.exit()` written into the
  caller's frame. What `defer()` does still buy is LIFO ordering
  (`after = FALSE`), which plain `on.exit(add = TRUE)` does not give. So D052
  records a measurement that does not reproduce, and `local_timeout()`'s
  roxygen promise — "when the function ends, by any route, whatever the caller
  had set before is back" — has a real, undocumented hole.
- **F3 (CONFIRMED, matches the family it copies).** A non-frame `.local_envir`
  (`local_timeout(5, .local_envir = new.env())`) sets the option and the undo
  never runs, on return or on gc. `withr::local_options()` behaves identically,
  so this is not a divergence — but `@param .local_envir` gives no hint, and
  AC5's non-default axis is tested only with another call frame.
- **F4 (PLAUSIBLE, unverified against old withr).** The `withr (>= 2.5.0)` floor
  may understate what the top-level behaviour was measured on: withr 3.0.3
  routes a globalenv target through `is_top_level_global_env()` into
  `global_defer()`, a branch absent from older `defer()` implementations, where
  a globalenv target would be silently dropped. Not measured on 2.5.0 here.
- **F5 (CONFIRMED, low).** AC3's three cases are ordinary function frames; the
  only non-test caller defers onto a `test_that()` block environment. That does
  work, but nothing in the test would have noticed if it did not.
- **F6 (CONFIRMED as a portability risk, fails red not false-pass).**
  `tm_pgrep()` shells out through `sh -c`, whose own command line contains the
  marker, so a shell that does not `exec`-optimize a single simple command
  leaves a matching parent in the table.
- **F7 (CONFIRMED).** `NEWS.md` does not mention that `withr` moved
  Suggests → Imports — an install-surface change for every user. `grep -i withr
  NEWS.md` is empty. D052 records it; the changelog does not.
- **F8 (CONFIRMED).** `R/tidymedia-package.R`'s "Bounding a run that hangs"
  section still offers `with_timeout()` as the only way to bound one call rather
  than the session, yet both timeout topics send readers there as the canonical
  description. AC7 did not require it; the section is now incomplete.
- **F9 (CONFIRMED, minor).** `tm_release_fifo()` leaves one `*.cancel` file per
  call in `tempdir()`, never removed. (The same report's note that AC8 was
  unticked was a snapshot of this review in progress; AC8 is ticked above
  against its evidence.)

Nothing was found wrong with: `check_required(expr)`'s placement and promise
semantics, the `formals()`-derived AC2 cases and their non-vacuity guards, the
shared probe helper, LIFO stacking, the abort-path restore, the `NULL`
asymmetry, `local_timeout`'s name against D014, the `_pkgdown.yml` row, or the
two NEWS bullets AC7 names.

### Gate outcome — returned to `in-progress` (defect return 1)

AC3 and AC8 fail on Linux CI. Under the return floor this is a defect return,
not an amendment return: the failure is inside AC3's own named procedure's
domain — the very three cases the criterion enumerates, run by the test the
criterion names — rather than outside it. Status is back to `in-progress`,
AC3 and AC8 are unticked, and review stops here. AC1, AC2, AC4, AC5, AC6 and
AC7 remain verified against the evidence recorded above and are left ticked.

**Leading hypothesis, for the implementer to confirm rather than assume.** The
[O] lens's F6 predicts this failure shape precisely: `tm_pgrep()` shells out
through `sh -c`, whose own command line contains the marker, so on a shell that
does not `exec`-optimize the command away, `pgrep -f <marker>` matches that
shell and never returns empty. The observed signature fits — every
`present = TRUE` assertion passes and every `present = FALSE` assertion fails,
on the platform whose `/bin/sh` differs from the dev machine's, and F6 called
it "a portability flake, not a false pass" before CI ran. Confirm on a Linux
runner before fixing; the alternative worth ruling out is the cancel file never
being seen (a `tempdir()` visibility or timing difference), which would also
leave the process up. Whatever the cause, `tm_pgrep()` wants a self-match guard
(exclude the querying shell's own PID / use `pgrep -f` with a pattern the query
itself cannot contain).

**The other eight findings are not re-triaged here** — they go to the gate on
the next review pass, with F1 and F2 (both CONFIRMED above) the ones needing a
maintainer decision, and F1 plausibly fixable in the same edit as this return.
