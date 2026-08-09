# M69: A hung media program stops the call, not the session

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m69-runtime-timeout · https://github.com/jmgirard/tidymedia/pull/72

## Goal

Give callers a wall-clock limit on every process tidymedia spawns, so a hung
FFmpeg aborts the call instead of blocking the R session forever.

## Scope

**In:** one option seam, `tidymedia.timeout` (seconds; `0`, the default, means
no limit), resolved once and passed to the `timeout=` argument of all four
process-spawn sites — `R/ffmpeg.R:28` (`ffmpeg()`), `R/ffprobe.R:21`
(`ffprobe()`), `R/mediainfo.R:26` (`mediainfo()`) and `R/program_management.R:119`
(`run_program()`, through which every task verb, `ffm_run()`, both loudnorm
analysis passes and all metadata readers funnel). Reaching the limit aborts,
naming the program and the limit. A D-entry records the shape.

**Out:**
- Per-call `timeout =` arguments on the ~60 exported verbs and the Layer-1
  runners → ROADMAP candidate row (an irreversible-API commitment, still open
  under D014's pre-0.2.0 clean-break window; the option seam does not foreclose it).
- Making `parallel = TRUE` workers see the parent's option → ROADMAP candidate
  row, disclosed in the D-entry. Measured 2026-08-09 on future 1.70.0: a
  `multisession` worker reading an option set to `42` in the parent got `UNSET`.
  Same disclosure shape as D044's per-process memo gap.
- The test-only `run_ffmpeg_fixture()` helper (`tests/testthat/helper-media.R:26`)
  keeps its own hard-coded 120 s limit and is not rewired → stays where M46 put it.

## Acceptance criteria

- [x] AC1 Each of the four spawn sites — `R/ffmpeg.R:28`, `R/ffprobe.R:21`,
      `R/mediainfo.R:26`, `R/program_management.R:119` — passes the resolved
      limit to its `timeout=` argument.
- [x] AC2 With `tidymedia.timeout` unset, the resolver returns `0`, and each of
      the four sites named in AC1 therefore passes `timeout = 0`.
- [x] AC3 With `options(tidymedia.timeout = 2)`, `ffmpeg()`, `ffprobe()` and
      `ffm_run()` each abort within 10 wall-clock seconds on a command that
      would otherwise run for 120 s, and each abort names the program and the
      limit in seconds. `mediainfo()` is covered by AC1 and the AC2 resolver
      test only: no 120-second MediaInfo invocation can be named.
- [x] AC4 The timeout branch's condition is a comparison of the `status`
      attribute to `124L` — not a match against the text of R's timeout
      warning, whose wording is translated under a non-English locale (M46).
      Evidence: the branch's source.
- [x] AC5 A timed-out `ffm_run()` applies D046's existing output-disposition
      rule unchanged, and the abort names which disposition applied.
- [x] AC6 Given a synthetic result carrying `status = 124` and a resolved limit
      of `0`, the internal classifier reports an ordinary non-zero exit and its
      message does not mention a timeout.
- [x] AC7 No warning at all is signalled to the caller from the three entry
      points of AC3 when they time out — asserted locale-free with
      `expect_no_warning()`, never by matching `timed out after`, since R's
      warning embeds the full command line and the `input=` temp path.
- [x] AC8 `NEWS.md` carries an entry, and the `?tidymedia` Rd topic documents
      the option's name, unit (seconds), default (`0`, no limit), and that
      reaching it aborts.
- [x] AC9 `cairn/DECISIONS.md` gains a D-entry recording the option-seam shape
      and the per-verb argument it rejects, off-by-default, abort-not-warn, the
      disclosed `parallel = TRUE` worker gap, and the falsifier.
- [x] AC10 The `verify` slot of `cairn/PROFILE.md` is clean —
      `devtools::document()`, `devtools::test()` and `devtools::check()` (0
      errors, 0 warnings).

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T4
- AC4 → T2, T4
- AC5 → T3, T4
- AC6 → T2, T4
- AC7 → T4
- AC8 → T5
- AC9 → T6
- AC10 → T7

## Tasks

- [x] T1 Add the resolver (`resolve_timeout()`, new or in `R/utils.R`): reads
      `getOption("tidymedia.timeout", 0)`, validates it is a single
      non-negative number, returns `0` when unset. Tests first.
- [x] T2 Thread it into the three Layer 0 sites (`R/ffmpeg.R:28`,
      `R/ffprobe.R:21`, `R/mediainfo.R:26`) and into `run_program()`
      (`R/program_management.R:108-122`); add the shared timeout classifier
      that keys on `status == 124L` and is inert when the resolved limit is `0`.
- [x] T3 Wire `ffm_run()` (`R/ffm.R:1548-1562`): a timeout aborts naming the
      program and limit, and reaches `remove_failed_output()` on the same path
      a non-zero exit does, so D046's disposition rule is applied unchanged.
- [x] T4 Execution tests for AC3/AC5/AC7 (`skip_if` no binaries; a
      `-f lavfi -i testsrc=duration=120` encode is the long command) and unit
      tests for AC4/AC6 against the classifier. Mutation-probe each new
      assertion — delete the guard it fences and confirm it reddens (M44).
- [x] T5 Roxygen: document the option in `R/tidymedia-package.R`'s `@details`,
      `devtools::document()`, add the `NEWS.md` entry.
- [x] T6 Write the D-entry (the two candidate rows for Scope Out are already on
      the ROADMAP, added by this plan).
- [x] T7 Run the `verify` slot end to end; `spelling::update_wordlist()` if the
      check NOTEs on new terms (M17).

## Work log

- 2026-08-09: created by /milestone-plan.
- 2026-08-09: plan gate chose an option seam (`tidymedia.timeout`) over a per-call `timeout =` argument on the run-capable verbs because the seam commits no exported signature and so is not the irreversible-API change the argument would be, while leaving that argument available under D014's pre-0.2.0 window; falsified by a report that the whole-session granularity is the wrong grain — one batch needing a different limit from the next within a single script.
- 2026-08-09: plan gate chose `0` (no limit, off) over a generous default ceiling because a ceiling would abort a legitimate multi-hour transcode that finishes today, changing the default behavior of every existing pipeline; falsified by a report of a hang from a caller who had read the docs and still expected a bound.
- 2026-08-09: plan gate chose abort over warn-and-return because a killed run leaves a truncated output that looks finished, and `ffm_batch()` records per-row errors, so a warning would make a timed-out row indistinguishable from a successful one in the results tibble; falsified by a caller wanting a partial output kept and the batch continued.
- 2026-08-09: plan gate chose all four spawn sites over the FFmpeg execution path alone because a hung FFprobe inside `probe_all()` over a corpus is an equally realistic hang and a narrower rule would ship stated with an exception; falsified by a measured cost to the metadata readers from the extra argument.
- 2026-08-09: implement gate chose refusing a fractional `tidymedia.timeout` over rounding it up, because base R truncates toward zero and a value below 1 becomes 0 — its own "no limit" sentinel — so `0.5` left a 6 s child unbounded (measured, R 4.6.1); rounding up would instead substitute a limit the caller never asked for. Falsified by a report of a legitimately computed fractional limit being refused.
- 2026-08-09: implement gate chose giving the timeout abort a distinct `tidymedia_timeout` class while letting `count_audio_streams()` and `probe_one()` absorb it exactly as they absorb any other error today, over making those two readers re-raise it; re-raising would change `probe_all()`'s error contract and D024 licenses the dropped-track probe only while its outcome changes nothing but whether a warning fires. Falsified by a report of a bounded-but-silent hang inside `probe_all()` being the reported problem.
- 2026-08-09: T1 — `R/timeout.R` adds `resolve_timeout()`, `is_timeout()` and `abort_timeout()`; 26 assertions in `tests/testthat/test-runtime-timeout.R`, all green.
- 2026-08-09: criteria audit ([O] fresh-context reader) returned 8 findings — AC1 and AC4 backed universals with proxy enumerations (a `system2?\(` regex; a four-spelling hand-list), AC2 was unsatisfiable because that sweep hits a comment at `R/program_management.R:104`, AC4's "held warning text" was vacuous under `run_program()`'s existing `suppressWarnings()`, AC5 conflicted with D046's `overwrite = FALSE` guard (`cairn/DECISIONS.md:1930`) and miscounted D046's cases, AC7's `timed out after` match was defeated by the translated warning the criterion itself cited, and AC3/AC6 named no reachable instance for `mediainfo()` and for a genuine 124 exit. All 8 had one clear right answer and were fixed before the gate; none of the gate's four answers changed a criterion, so no criterion needed re-auditing.

- 2026-08-09: T2 — `guard_timeout()` added; all four spawn sites resolve and pass the limit. `suppress=` follows each site's existing behavior (`run_program()` keeps its `suppressWarnings()` semantics; the three Layer 0 hatches keep letting a non-zero exit warn). Full suite FAIL 0 / PASS 6088 / SKIP 5.
- 2026-08-09: T2 — the suite's 4 warnings are pre-existing `warn_dropped_audio()` calls, not M69's: a `master` worktree and the branch both report warn 4 / fail 0 / pass 195 over `test-audio-stream.R` + `test-ffmpeg.R`.
- 2026-08-09: T1 mutation probe — 7 mutations of `R/timeout.R` each reddened the suite (1/3/3/1/1/1/1 failures), and the differing failure sets rule out M44's identical-set tell.

- 2026-08-09: T3/T4 — `ffm_run()` catches `tidymedia_timeout` and re-raises it with D046's disposition appended; `abort_timeout()` gained `extra=`/`.envir=` so `remove_failed_output()`'s `{.file {output}}` bullets interpolate in the caller's frame rather than being re-glued from a formatted message (M44's brace trap).
- 2026-08-09: T4 — the hang is produced by a FIFO with no writer, which blocks FFmpeg deterministically, rather than by racing a long encode against the limit on an unknown host (the M31/M46 failure mode). Windows skips (no mkfifo); the fixture is built inside the gate and the gate skips rather than `fail()`s (M68).
- 2026-08-09: T4 — FFmpeg blocks on the FIFO before opening its output, so the half-written-output half of AC5 is unreachable that way; it is proven instead by injecting the kill at the `run_program()` seam with a call-counting mock, leaving the cleanup path real. Filter green: 67 pass, 0 fail. Full suite FAIL 0 / PASS 6112.
- 2026-08-09: T3 mutation probe — dropping the handler, skipping `remove_failed_output()`, and moving the snapshot after the run each reddened 3 tests, with the feature verified still present in the tree.

- 2026-08-09: T5 — `?tidymedia` gained a "Bounding a run that hangs" section; NEWS.md entry added; `devtools::document()` run. Two doc guards added, reading Rd through the shared two-shape reader so they run under `R CMD check` too (M51).
- 2026-08-09: T6 — D047 appended; the two Scope Out candidate rows were already added by the plan commit. `cairn_validate` green.

- 2026-08-09: T7 — verify slot clean: `devtools::document()` no diff, `devtools::test()` FAIL 0 / PASS 6118 / SKIP 5, `devtools::check()` `Status: OK` (0/0/0, read from the real status line, not devtools' summary — M17).
- 2026-08-09: T7 — both doc guards verified to RUN under `R CMD check` rather than skip (M51): with the package installed, `tools::Rd_db("tidymedia")` yields the `tidymedia-package` topic carrying all four asserted strings, and `system.file("NEWS.md")` resolves, so the NEWS guard was given the same installed fallback rather than left source-tree-only.

- 2026-08-09: review — PR #72 opened as draft; fresh evidence recorded for AC1-AC10 and all ten boxes ticked against it; consistency gate green (`cairn_validate` exit 0, 16 PASS + 1 advisory on the 10-AC sizing tripwire; `cairn_impact` no-op, no principle touched).
- 2026-08-09: review — [S] blame-history lens: no resurrected bug, contradicted D-entry or weakened guard; one nuance to score (Layer 0 re-raised warnings lose the original condition's call context via `warning(msg, call. = FALSE)`). [S] prior-review lens: no prior-review regression; GitHub inline-comment probe empty, threads not walked. [O] diff-bug lens still running — triage and scoring pending.

## Decisions

## Review

**Evidence** (fresh, 2026-08-09, branch `m69-runtime-timeout` @ `afaf950`):

- AC1 — all four sites carry the wiring: `R/ffmpeg.R`, `R/ffprobe.R`,
  `R/mediainfo.R`, `R/program_management.R` each show one `resolve_timeout(`,
  one `timeout = limit`, one `guard_timeout(`. Body-reading test asserts the
  same three per site.
- AC2 — `resolve_timeout()` with the option unset returns `0` (run directly).
  `devtools::test()` FAIL 0 / PASS 6118 / SKIP 5.
- AC3 — real hang, real kill: `ffmpeg()` on a writer-less FIFO under
  `tidymedia.timeout = 2` aborted in **2.09 s** with class
  `tidymedia_timeout` and message `FFmpeg timed out after 2 seconds.`
  `mediainfo()` covered by AC1 + the resolver test, as the criterion states.
- AC4 — the branch is `identical(as.integer(status), 124L)`; a grep of
  `R/timeout.R` for `grepl|regexpr|regmatches|gsub|sub\(|startsWith|grep\(`
  returns nothing, so no text match exists to go stale under a locale.
- AC5 — timeout path reaches D046 unchanged: on a pre-existing output the
  abort added `... was left as it was: FFmpeg never wrote to it.` and the file
  survived byte-identical. The written-output case is proven by injecting the
  kill at the `run_program()` seam with a call-counting mock (FFmpeg blocks on
  the FIFO before opening its output, so that route cannot produce a partial
  file); the disposition read `was removed` and the file was gone.
- AC6 — `is_timeout(status = 124, limit = 0)` is `FALSE`; `limit = 2` is
  `TRUE`.
- AC7 — no warning escapes: asserted with `expect_no_warning()`, never a text
  match. The abort message contains no `tempdir()` substring. (The caller's own
  output path does appear, via D046's pre-existing `{.file {output}}` bullet —
  that is the user's own file, not R's command line.)
- AC8 — `man/tidymedia-package.Rd` contains `tidymedia.timeout`, `second`,
  `no limit`, `abort`; `NEWS.md` carries the entry. Both guards verified to run
  under `R CMD check` (installed `Rd_db` and `system.file("NEWS.md")` both
  resolve), not merely under `devtools::test()`.
- AC9 — `cairn/DECISIONS.md:1941`, D047.
- AC10 — `devtools::document()` no diff; `devtools::test()` FAIL 0 / PASS 6118;
  `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes.

**Consistency gate:** `cairn_validate` exit 0, all 16 checks PASS, one advisory
(`M69: 10 acceptance criteria (>7 tripwire)`). `cairn_impact` not run —
Principles touched is `—`. Toolchain slot: `document()` no diff · generated
files unedited · README untouched, in sync · `pkgdown::check_pkgdown()` no
problems · NEWS.md entry present with no milestone or decision ids in
user-facing text · no new top-level files · `check()` clean.

