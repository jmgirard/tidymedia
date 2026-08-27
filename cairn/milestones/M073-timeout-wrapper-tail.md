<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M073: The timeout wrapper's tail

- **Status:** review
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

- 2026-08-27: return-fix verification closed. Run 33109601529 (head `4ec99da`, PR #77): all five jobs `Status: OK` — `ubuntu-latest` release/devel/oldrel-1, macOS and Windows — zero notes anywhere, with `ubuntu-latest (release)` at `[ FAIL 0 | WARN 4 | SKIP 9 | PASS 6615 ]`. Local `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes, 2m 39.9s. Status back to `review`.
- 2026-08-27: review pass 2 — all eight criteria verified with fresh evidence, all nine CI checks green on PR #77. Three lenses ran; blame-history and prior-review found nothing, the diff lens found ten. Maintainer triaged at the gate: six fixed on the branch (the option-before-undo leak in `local_timeout()`, two false doc claims about how the undo can be lost, the unquoted `pgrep` pattern, the unpinned unset-prior return shape, the pkgdown blurb, and `local_timeout()`'s absence from the package-level docs), five rejected with reasons, AC3's wording deviation recorded rather than amended, and the `withr` floor question filed as a candidate row.

## Decisions

## Review

Second review pass, 2026-08-27, on `m073-timeout-wrapper-tail` at `e4dfcaf`, PR
https://github.com/jmgirard/tidymedia/pull/77. The first pass returned the
milestone to `in-progress` (defect return 1) on AC3 and AC8 failing on Linux CI;
that pass's evidence is superseded by the fresh evidence below, and the return
itself is recorded in the work log. `master` has not moved since the branch was
cut (`git rev-list --left-right --count master...origin/master` = 0/0; branch 12
ahead, 0 behind), so no merge-forward was needed.

### Acceptance-criteria evidence

- **AC1 — verified.** `with_timeout(seconds = 5)` in a fresh session signals a
  condition of class `rlang_error`; the `ansi_strip()`ed message reads
  `` `expr` is absent but must be supplied. `` — it contains `expr` and does not
  contain `argument "expr" is missing, with no default`. Measured directly this
  pass, and pinned by "an omitted expr is refused by the package, not by base R"
  in `tests/testthat/test-with-timeout.R`.
- **AC2 — verified.** `names(formals(with_timeout))` measured as
  `c("expr", "seconds")`, not read from a list. Omitting each name in turn while
  supplying the other signals an `rlang_error` whose stripped message names the
  omitted argument: both cases measured true. The regression clause holds for
  both prior states and both omissions — unset reads `absent` after, `99` reads
  `99`. The test derives its cases from `formals()` at run time and guards
  non-vacuity with `expect_setequal` plus a more-than-one-formal assertion.
- **AC3 — verified, on the platform that falsified it last pass.** All three
  Ubuntu jobs of run 33111081845 (head `e4dfcaf`) report `Status: OK` and
  `[ FAIL 0 | WARN 4 | SKIP 9 | PASS 6615 ]`, where the returned run reported
  `FAIL 4` from exactly this cell. macOS (`5907` pass) and Windows (`5676` pass)
  are green too. Locally `test-with-timeout.R` alone is 114 pass, 0 fail,
  0 skip — the cell ran rather than skipping. The cell asserts the writer is
  present inside the frame and gone within 5 s of the frame exiting, for a frame
  exiting by return, one exiting by abort, and one arming the helper twice (two
  distinct markers). Non-vacuity is guarded directly against the bug that caused
  the return: a marker of the real shape that nothing ever started must match
  nothing. Deviation noted for the record: the query runs
  `pgrep -f tm_[f]ifo_<hex>` rather than the bare `pgrep -f <marker>` the
  criterion's prose names — an ERE matching the writer's literal marker but not
  the pattern text in the querying shell's own command line, which is what makes
  the criterion's promise testable on Linux at all. Put to the maintainer at the
  gate rather than reinterpreted here.
- **AC4 — verified.** `after = 90` is unchanged at
  `tests/testthat/test-with-timeout.R:351`, outside 2 + 40 s. The FIFO-anchored
  cell "a per-call limit kills a hung program with no session limit set" reaches
  its `tidymedia_timeout` abort and completes within budget: green in the local
  file run and in every CI job above.
- **AC5 — verified.** `local_timeout` is in `getNamespaceExports("tidymedia")`;
  its signature is `function(seconds, .local_envir = parent.frame())`. Measured
  this pass on all four axes, each against both a previously-unset and a
  previously-`99` option: exit by return (inside `7`; after `absent` / `99`),
  exit by abort (after `absent` / `99`), two calls nested in one frame (inside
  `4` — the second displaces the first; after `absent` / `99`), and a
  non-default `.local_envir` naming an outer frame (`11` still in force after
  the inner call returns, `absent` once the outer frame ends). All four are also
  cells in `tests/testthat/test-local-timeout.R`: 98 pass, 0 fail, 0 skip.
- **AC6 — verified.** Scored every value in the shared probe vector
  (`tests/testthat/helper-timeout-probes.R`: `0, 1L, 60, 0.5, -1, NA, NA_real_,
  "2", c(1, 2), Inf, TRUE, integer(0), factor("2")`) against the option's own
  verdict via `tm_option_accepts()`: 13/13 agree, with both verdicts occurring
  (3 accepted, 10 refused), so the agreement is not vacuous. `0` and `1L` are
  accepted and the limit in force is `as.numeric(seconds)` (`0` and `1`). The
  `NULL` asymmetry is intact and stated, not removed: the option accepts `NULL`
  (it removes the name), `local_timeout(NULL)` refuses, and
  `man/local_timeout.Rd:66-68` says so.
- **AC7 — verified.** `man/local_timeout.Rd` exists and cross-links
  `with_timeout()` both ways (`man/with_timeout.Rd:56` → `local_timeout()`;
  `man/local_timeout.Rd:31` and `:97` → `with_timeout()`). `_pkgdown.yml:22-23`
  carries a `local_timeout` row under "Bounding a run". `NEWS.md` carries one
  bullet for `local_timeout()` (AC5) and one for the omitted-`expr` refusal
  (AC1), plus a third for the `withr` install-surface change. `cairn/DECISIONS.md`
  carries D052, which extends D051 without superseding it, records the second
  export and its place outside D014's families, records that it discharges
  D051's "a statement, not a wrapper" falsifier, and records the `withr`
  Suggests-to-Imports move.
- **AC8 — verified, on every platform.** Local `devtools::check()` on the
  branch: `Status: OK`, 0 errors / 0 warnings / 0 notes, 2m 29.8s. All five CI
  check jobs of run 33111081845 report `Status: OK` with zero NOTE lines, so the
  note delta against `master` is empty by construction on every platform.
  `devtools::test()` locally: 6628 pass, 0 fail, 5 skip, 4 warn — all four
  warnings are the pre-existing dropped-audio-track messages at
  `test-audio-stream.R:249`, `:289`, `:355` and `test-ffmpeg.R:178`, neither
  file touched by this branch.

### Consistency gate

- `cairn_validate.py`: exit 0, all checks passed. One advisory —
  `sizing (split tripwires)`, M073 at 8 acceptance criteria — an advisory, not a
  gate failure, disposed in the work log (the eighth is the mandatory
  profile-check criterion).
- `cairn_impact.py`: skipped, no DESIGN.md principle changed (the header's
  `Principles touched` is `—` and the branch does not touch `cairn/DESIGN.md`).
- `r-package` profile `consistency-gate` slot: `devtools::document()` leaves the
  working tree clean, which is also the check that `NAMESPACE` and `man/` were
  regenerated rather than hand-edited; `README.Rmd`/`README.md` are untouched by
  the branch; `pkgdown::check_pkgdown()` reports "No problems found"; `NEWS.md`
  (the declared changelog) carries entries for all three user-visible changes,
  with no milestone numbers in the user-facing text; the branch adds no
  top-level files, so no `.Rbuildignore` entries were needed and no NOTE was
  raised; the full `devtools::check()` is clean as recorded under AC8.

### Independent review

Full three-lens fan-out — the diff touches executable surface (`R/`, `tests/`)
and the declared tier is user-facing. Each lens ran fresh-context on its own
evidence base, and none was shown the prior pass's Review section.

**[S] blame-history lens — no findings.** Ran `git log`/`git blame` over the
modified regions against D047–D052 and `cairn/LESSONS.md`. Confirmed: `after =
90` is unchanged and M69's limit + 40 s lesson intact; the `NULL` asymmetry is
stated, not collapsed; both functions still check arguments before writing the
option; D052 extends rather than supersedes D051, matching the diff; the `withr`
promotion is required by `local_timeout()` rather than a stray bump. It raised
one phrasing nitpick — NEWS.md's "withr itself depends on nothing outside base
R" against withr's `Imports: graphics, grDevices` — see B1 below.

**[S] prior-review lens — no findings.** Archived `## Review` sections for M69,
M70, M071, M072 and M46 are the primary surface. This diff is the fix for M072's
deferred F3 and F6, not a regression of them; M69's `is_timeout()` status-code
rule and M070's warning paths are untouched; M071's carrier contract is left
alone. The GitHub probe
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`, so
no real inline review threads exist anywhere in the repo and the per-PR walk was
correctly skipped.

**[O] diff-bug lens — ten findings, ranked as reported.** Each verified below
against the implementation rather than against the reviewer's account.

- **O1 (CONFIRMED). `local_timeout()` writes the option before registering the
  undo, so a failed registration leaks the limit for the whole session.**
  `R/timeout.R:216-231` runs `prior <- options(...)` and only then
  `withr::defer(..., envir = .local_envir)`. If `defer()` errors, nothing puts
  the caller's value back. Reproduced here: with the option at `99`,
  `f <- function() local_timeout(5, .local_envir = "nope")` aborts with
  `'envir' must be an environment` and leaves the option at `5`. Control: the
  same shape through `withr::local_options(tm_probe = 5, .local_envir = "nope")`
  leaves its option at `99` — `withr` orders it read → defer → write, so this is
  tidymedia's own hole rather than an inherited one. Fix is a reorder, or an
  environment check above the write.
- **O2 (CONFIRMED). The roxygen's "There is one way the restore can be lost" is
  false.** `R/timeout.R:157` (rendered at `man/local_timeout.Rd:44-52`) names
  the caller's bare `on.exit()` as the single loss route. At least two more
  exist: O1 above, and a `.local_envir` that is not a live frame. Reproduced:
  with the option at `99`,
  `g <- function() { e <- new.env(); local_timeout(5, .local_envir = e); invisible(NULL) }`
  leaves the option at `5` after `g()` returns and after `gc()`, silently and
  with no error. AC5's fourth axis probes only a live enclosing frame, so the
  tests cannot see it. D052 repeats the same framing.
- **O3 (CONFIRMED, doc gap). `.local_envir`'s documented contract does not say
  it must be a frame on the stack.** `R/timeout.R:151-153` /
  `man/local_timeout.Rd:15-18` invite the helper-on-behalf-of-its-caller use,
  which works, but nothing warns that any other environment silently no-ops.
  Same measurement as O2.
- **O4 (CONFIRMED as a latent regression risk). `tm_pgrep()`'s self-match guard
  is one stray filename away from reverting: the bracketed pattern is passed
  through the shell unquoted.** `tests/testthat/test-with-timeout.R:399-410`.
  `system2("pgrep", c("-f", pattern), stdout = TRUE)` does not quote its
  arguments and `tm_[f]ifo_<hex>` is a valid shell glob. Reproduced: with a file
  named `tm_fifo_deadbeef` in the working directory,
  `system2("echo", c("-f", "tm_[f]ifo_deadbeef"), stdout = TRUE)` returns
  `-f tm_fifo_deadbeef` — the bracket gone, which is exactly the dash self-match
  that reddened all three cases last pass. Not reachable today:
  `tempfile("tm_fifo_")` never creates the file, and the test working directory
  is `tests/testthat`. `shQuote()` on the pattern removes the exposure.
- **O5 (CONFIRMED, cosmetic). `.cancel` files accumulate in `tempdir()` and are
  never removed.** `tests/testthat/test-with-timeout.R:353-354` creates one empty
  file per call that nothing deletes; only the session-exit sweep of `tempdir()`
  collects them. The milestone's actual concern — a live process outliving its
  frame — is fixed; no `tm_fifo` writer survived the suite.
- **O6 (CONFIRMED, instrument weakness, no action proposed). AC2's regression
  clause is near-vacuous for the ordering T1 says it pins.**
  `tests/testthat/test-with-timeout.R:196-201`. Because
  `on.exit(options(prior), add = TRUE)` is registered immediately after the write
  and fires on the error path, moving either check below `on.exit()` still
  restores and still passes. AC2 as literally worded is satisfied; the work log
  already concedes the clause certifies less than it looks like.
- **O7 (CONFIRMED, criterion-text drift). AC3's assertion runs the bracketed
  pattern, not the bare `<marker>` the criterion's prose names.** Recorded under
  AC3 above and put to the maintainer at the gate.
- **O8 (PLAUSIBLE, unpinned behaviour). `local_timeout()`'s documented `@return`
  is asserted only for the previously-set case.**
  `tests/testthat/test-local-timeout.R:105-110` covers
  `list(tidymedia.timeout = 99)`; the unset case, where `options()` hands back
  `list(tidymedia.timeout = NULL)`, is never asserted, though every other cell
  in the file runs both priors. The behaviour is correct, just unpinned.
- **O9 (CONFIRMED, cosmetic). `_pkgdown.yml`'s "Bounding a run" blurb still
  describes only the wrapper** — "a limit on one call" — while the section now
  also holds `local_timeout()`, which bounds a frame. AC7 asks only for the row,
  which is present.
- **O10 (CONFIRMED, cosmetic). The `expr`-refusal NEWS bullet sits under "New
  features"** (`NEWS.md:82-86`) though it is a bug fix; the file has no bug-fix
  section this cycle. AC7 asks only that the bullet exist, which it does.

**Carried from the first pass, still open and re-verified here.**

- **P1 (CONFIRMED, was F8). `R/tidymedia-package.R`'s "Bounding a run that
  hangs" section still offers `with_timeout()` as the only way to bound one call
  rather than the session** (`R/tidymedia-package.R:13-25`), yet both timeout
  topics send readers there as the canonical description, and `local_timeout()`
  appears nowhere in the package-level docs (grep is empty). AC7 did not require
  it; the section is now incomplete.
- **P2 (PLAUSIBLE, was F4). The `withr (>= 2.5.0)` floor may understate what the
  top-level behaviour was measured on.** withr 3.0.3 routes a globalenv target
  through `is_top_level_global_env()` into `global_defer()`, a branch absent from
  older `defer()` implementations. Not measured on 2.5.0. Raising the floor is a
  dependency re-pin and so needs its own question gate and D-entry.
- **P3 (CONFIRMED, low, was F5). AC3's three cases are ordinary function frames;
  the only non-test caller defers onto a `test_that()` block environment.** That
  does work, but nothing in the test would notice if it stopped working.

**B1 (REJECTED at triage, from the blame lens).** NEWS.md's "withr itself
depends on nothing outside base R" against withr's `Imports: graphics,
grDevices`. Both are base-distribution packages shipped with R itself, so the
sentence is true as a statement about the install surface, which is what the
bullet is about. No change.

### Gate triage and fix-now work

The maintainer took all three questions at the gate: fix the six actionable
findings on the branch; record AC3's wording deviation rather than amending the
criterion; leave the `withr` floor at 2.5.0 and file P2 as a candidate.

- **O1 — fixed.** `local_timeout()` now READS the prior value, REGISTERS the undo,
  and only then WRITES the limit — `withr::local_options()`'s own order, so a
  failure below the read leaves the session as it was found. Pinned by a new
  cell, "a failed undo registration leaves the session as it was found", which
  is red against the previous ordering (measured before the fix: the option was
  left at `5` where the caller had `99`; it now reads `99`).
- **O2, O3 — fixed.** The `@details` now says there are two loss routes, names
  the non-live-frame one, and states what cannot happen (the limit set with no
  undo registered). `@param .local_envir` now says the environment must be a
  frame still on the call stack and what happens when it is not. D052 is left
  standing: it names the `on.exit()` route without claiming exclusivity, so
  nothing in it is false and history is not edited (IP4).
- **O4 — fixed.** `tm_pgrep()` now `shQuote()`s the bracketed pattern, so a file
  matching the glob in the working directory can no longer expand the bracket
  away and restore the self-match that reddened Linux last pass.
- **O8 — fixed.** A new cell asserts the unset-prior return shape
  (`list(tidymedia.timeout = NULL)`, length 1, named, NULL entry) and that
  feeding it back to `options()` really does restore the option to unset.
- **O9 — fixed.** The `_pkgdown.yml` "Bounding a run" blurb now covers both
  forms.
- **P1 — fixed.** `R/tidymedia-package.R`'s "Bounding a run that hangs" section
  now shows `local_timeout()` beside `with_timeout()`, with a worked example.
- **O5, O6, O10, P3, B1 — rejected**, reasons recorded above with each finding:
  cosmetic tempdir residue swept at session exit; an instrument weakness the
  work log already concedes, with AC2 as written satisfied; a defensible NEWS
  section placement; a low-severity coverage note nothing acts on; and a NEWS
  sentence that is true as a statement about the install surface.
- **O7 — recorded, not amended**, on the maintainer's decision at the gate.
- **P2 — filed as a candidate row** rather than fixed, on the maintainer's
  decision: raising the floor is a dependency re-pin needing its own gate.

**Re-verification after the fixes.** `devtools::document()` regenerated
`man/local_timeout.Rd` and `man/tidymedia-package.Rd` and then leaves the tree
clean. `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes,
2m 41.2s. `devtools::test()`: 6635 pass, 0 fail, 5 skip, 4 warn — 7 more passes
than before the fixes, the same four pre-existing warnings.
`pkgdown::check_pkgdown()`: no problems. `spelling::spell_check_package(".")`:
no spelling errors. The two timeout files alone: 105 and 114 pass, 0 fail,
0 skip.

Nothing was found wrong with: `check_required(expr)`'s placement and promise
semantics (`with_timeout({side <<- TRUE; 1}, 0.5)` refuses with `side` still
`FALSE`), the `formals()`-derived AC2 cases and their non-vacuity guards, the
shared probe helper, LIFO stacking, the abort-path restore, the `NULL`
asymmetry, `local_timeout`'s name against D014, the poll loop's parentheses and
`[ -d tempdir ]` clause as POSIX/dash-safe, defer ordering in the AC4 cell,
`man/` matching the roxygen, or `inst/WORDLIST` ordering.
