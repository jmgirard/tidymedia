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
  runners → ROADMAP candidate row (an irreversible-API commitment still open
  under D014's pre-0.2.0 window; the seam forecloses none of it).
- Making absorption uniform — the two no-warning paths AC8 discloses, plus J2's
  program-literal split and J7's `tm_timed_out` attribute → **M70**.
- Making `parallel = TRUE` workers see the parent's option → ROADMAP candidate
  row, disclosed in the D-entry (measured 2026-08-09, future 1.70.0: a
  `multisession` worker got `UNSET`). D044's disclosure shape.
- The test-only `run_ffmpeg_fixture()` helper (`tests/testthat/helper-media.R:26`)
  keeps its own hard-coded 120 s limit and is not rewired → stays where M46 put it.

## Acceptance criteria

- [x] AC1 Each of the four spawn sites — `R/ffmpeg.R:28`, `R/ffprobe.R:21`,
      `R/mediainfo.R:26`, `R/program_management.R:119` — passes the resolved
      limit to its `timeout=` argument.
- [x] AC2 With `tidymedia.timeout` unset, the resolver returns `0`, and each of
      the four sites named in AC1 therefore passes `timeout = 0`.
- [x] AC3 With `options(tidymedia.timeout = 2)`, `ffmpeg()`, `ffprobe()` and
      `ffm_run()` each abort within 60 wall-clock seconds of the call on a
      writer-less FIFO input (`local_blocking_input()`), each naming the
      program and the limit in seconds. Linux and macOS only: the fixture
      cannot be built on Windows, where R terminates the child directly. The
      bound is 60 s rather than the limit, and is per spawned program, because
      base R escalates SIGINT/SIGTERM/SIGKILL across limit + 40 s (ladder and
      measurements in the work log, 2026-08-09). `mediainfo()` is covered by
      AC1 and the AC2 resolver test only: no 120-second MediaInfo invocation
      can be named.
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
- [x] AC8 `?tidymedia` and `NEWS.md` both describe a reached limit as three
      behaviors rather than two. **Abort:** the task verbs, `ffm_run()`, and the
      Layer 0 hatches `ffmpeg()`, `ffprobe()`, `mediainfo()`. **Absorb, with an
      NA row and one end-of-call warning naming how many timed out:**
      `probe_all()`, the `probe_*()` accessors, `mediainfo_parameter()`,
      `mediainfo_query()`, `mediainfo_template()`, the `get_*()` helpers.
      **Absorb with no warning at all:** `count_audio_streams()` — reached by
      `extract_audio()`, `convert_audio()`, `separate_audio_video()` and their
      `_batch` siblings — and `tool_versions()`, reached by `ffm_batch()`.
      Both files
      state that this describes the calls it names and is not a partition of
      the package. `verify_media()` is stated as re-raising. Verified by two
      guards: one reddens when `A call that reaches the limit aborts` is
      restored ahead of the scoped paragraph, the other when the no-warning
      disclosure is removed.
- [x] AC9 `cairn/DECISIONS.md` gains a D-entry superseding D047's readers
      bullet and recording the shape actually shipped: `probe_one()`'s
      sentinel, `probe_all()` keeping the NA row while its warning counts
      timeouts apart from unreadable files, `verify_media()` re-raising rather
      than absorbing, and the two no-warning paths disclosed rather than fixed.
      Evidence: a sweep of the `^## D0` headings finds D047 the only entry
      asserting the uniform-absorption shape, and the new entry's heading names
      it superseded in that half.
- [ ] AC10 The `verify` slot of `cairn/PROFILE.md` is clean —
      `devtools::document()`, `devtools::test()` and `devtools::check()` (0
      errors, 0 warnings).

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T4, T9, T11
- AC4 → T2, T4
- AC5 → T3, T4
- AC6 → T2, T4
- AC7 → T4
- AC8 → T5, T10, T13, T15
- AC9 → T6, T16
- AC10 → T7, T17

## Tasks

_T1-T14 are done; their detail is in the work log and in the branch's commits._

- [x] T1 `resolve_timeout()` — read, validate, default `0`. Tests first.
- [x] T2 Thread the limit into all four spawn sites; add the `status == 124L` classifier.
- [x] T3 Wire `ffm_run()`: abort naming program and limit, reaching D046's disposition.
- [x] T4 Execution tests (AC3/AC5/AC7) and classifier unit tests (AC4/AC6), mutation-probed.
- [x] T5 Roxygen the option in `R/tidymedia-package.R`; `document()`; NEWS entry.
- [x] T6 Write D047.
- [x] T7 Run the `verify` slot end to end.
- [x] T8 (return 1, F1/F3) `absorb_timeout()` in `probe_one()`, `mediainfo_parameter()`, `mediainfo_read()`.
- [x] T9 (return 1, F15) Tighten the AC3 tests to the criterion as written.
- [x] T10 (return 1, F7) Narrow the partial-output claim to calls that know their output.
- [x] T11 (return 2, AC3) Relax every wall-clock assertion to 60 s; `skip_on_cran()` the FIFO tests.
- [x] T12 (return 2, G2/H4) Timeout sentinel; `probe_all()` counts timeouts apart; `verify_media()` re-raises.
- [x] T13 (return 2, G1/AC8) Rewrite the abort claim as one scoped claim; replace the substring guard.
- [x] T14 (return 2, G6/P2) `mediainfo_read()` absorber test via `mediainfo_query()`.
- [x] T15 (re-plan) Rewrite both doc sections to AC8's three-way shape; keep
      the scoping guard, add the second guard fencing the no-warning
      disclosure. Mutation-probe both.
- [x] T16 (re-plan) Append the superseding D-entry per AC9; run the `^## D0`
      heading sweep as its evidence.
- [x] T17 (re-plan) Re-run the `verify` slot end to end after T15/T16, and
      confirm CI green on PR #72.

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

- 2026-08-09: review RETURN -> in-progress. What failed: (1) F1, `probe_all()` aborts on a timeout instead of yielding an NA row, falsifying its own `@return` and D047's readers bullet (measured: `purrr_error_indexed`, `tidymedia_timeout` only as `$parent`); (2) F15, AC3 unverified as written — tests assert `expect_lt(..., 20)` against the criterion's 10 s, the ffprobe test omits the limit, the ffm_run test has no wall-clock assertion; (3) F3, `mediainfo_parameter()` aborts mid-loop against its `@return`; (4) F7, Layer 0 timeouts leave a playable truncated output (measured 107 s of a 600 s source) while `?tidymedia` and NEWS.md say the partial file is removed. AC3 unticked; AC1/AC2/AC4-AC10 evidence stands.

- 2026-08-09: return amendment (minor) — T8/T9/T10 added for the three actioned findings that are code or doc defects; AC3 and every other criterion stay as written, since F15 found the tests wrong rather than the criterion. Coverage gains AC3 → T9 and AC8 → T10; T8 maps to no criterion (D047's readers bullet, not an AC). One checkpoint commit covers all three: they share `tests/testthat/test-runtime-timeout.R` and one verify-slot run.
- 2026-08-09: T8 (F1/F3) — `absorb_timeout()` added to `R/timeout.R` and wired into `probe_one()`, `mediainfo_parameter()` and `mediainfo_read()` (the last shares F3's defect and was fixed with it). A hung file now yields the NA row and the existing end-of-call warning; every non-timeout error still propagates. Mutation probe: removing each absorber reddened exactly its own test and no other.
- 2026-08-09: T9 (F15) — AC3 verified as written. Measured on the FIFO fixture under a 2 s limit: `ffmpeg()` 2.06 s, `ffprobe()` 2.04 s, `ffm_run()` 2.06 s, each message naming its program and `2 seconds`. The bound is now the criterion's 10 s, and all three tests assert the limit. Mutation probe: dropping the limit from `abort_timeout()`'s message reddened the ffprobe and ffm_run tests, which it did not before.
- 2026-08-09: T10 (F7) — `?tidymedia` and NEWS.md now scope the partial-output removal to the calls that know their own output and state that the raw `ffmpeg()` hatch leaves the file; the hatch parses no argument string, so cleanup there is not available under D002. Fenced by a new test using a stand-in binary that writes and then blocks, which produces the partial output a FIFO cannot. Mutation probe: adding the rejected cleanup to `ffmpeg()` reddened it.
- 2026-08-09: verify slot after the return — `devtools::document()` rewrote only `man/tidymedia-package.Rd`; `devtools::test()` FAIL 0 / PASS 6132 / SKIP 5 (the 4 warnings are T2's pre-existing `warn_dropped_audio()` calls); `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes (read from the real status line, not devtools' summary — M17). `cairn_validate` exit 0, one pre-existing advisory on the 10-AC sizing tripwire. Status back to `review`.

- 2026-08-09: correction, superseding the T8 line above — that line's "removing each absorber reddened exactly its own test and no other" covers three absorbers but only two were probed. `mediainfo_read()`'s absorber has no test in the suite at all (`grep mediainfo_read tests/` is empty), so no mutation of it could redden anything. Raised independently by the prior-review and diff-bug lenses (G6/P2, scored 78).
- 2026-08-09: review RETURN -> in-progress (second defect return). What failed: (1) step-4 gate — AC3 fails on Linux, where `ffmpeg()` and `ffm_run()` take 42.1 s to abort under a 2 s limit against the criterion's 10 s, and PR #72's R-CMD-check has been red on every Linux job since `afaf950` while the first review pass recorded no CI evidence; (2) G2/H4 (88), a timed-out probe inside `verify_media()` is reported as a property mismatch so `ffm_run(verify=)` blames a successful encode — a regression T8 introduced on the D011 path; (3) G1 (85), `?tidymedia` and NEWS.md still claim every timed-out call aborts, false for every metadata reader after T8. AC3 and AC10 unticked; AC1/AC2/AC4-AC9 evidence stands. Thrash trigger (b) fires on AC3's second failure; escalation offered per instance.

- 2026-08-09: mechanism identified for the AC3 gate failure — base R's `timeout=` escalates SIGINT at the limit, SIGTERM at +20 s and SIGKILL at +40 s. Measured locally (macOS, R 4.6.1) under a 2 s limit: a child that dies on SIGINT takes 2.01 s, one ignoring SIGINT 22.01 s, one ignoring SIGINT and SIGTERM 42.01 s. Linux CI's 42.0-42.1 s is the third rung. Not a wiring fault: `?system` states termination "works for typical commands, but is not guaranteed".
- 2026-08-09: implement gate chose keeping base R's `timeout=` and amending AC3 to the real guarantee over adding `processx` for direct process control, because the Goal — a hung program stops the call rather than the session — is met by a bounded 42 s exactly as by a bounded 2 s, while a dependency plus a rewiring of all four spawn sites is a fresh round of risk at the second return. Falsified by a report that the limit-plus-40s lag is itself the reported problem. The tighter-kill work is a ROADMAP candidate.
- 2026-08-09: implement gate chose making a timeout distinguishable inside `probe_all()` — sentinel from `probe_one()`, NA row and warning kept, the warning naming the timeout — over giving `verify_media()` a private non-absorbing probe, because the same change closes both the misleading `ffm_run(verify=)` abort (G2/H4, 88) and the indistinguishable-hang gap (G4), and it leaves D047's return contract untouched. Falsified by a caller wanting the timeout invisible to every reader path.
- 2026-08-09: criteria audit ([O] fresh-context reader) of the two amended criteria returned 10 findings, all with one clear right answer and all fixed before the text was written: AC3 named a 120-second command the suite never uses (the fixture is a writer-less FIFO), was silent on Windows where it is vacuous, conflated entry points with spawned programs, left the measurement start point unstated, and asserted a cause its own cited evidence contradicts — `ffprobe()` aborts in ~2 s on the same Linux FIFO, so the discriminator is not "FFmpeg blocks in a syscall"; AC8 left `NEWS.md` unconstrained though G1 condemned both files, listed "aborts" and "readers absorb" as two coexisting sentences that a contradictory doc satisfies literally, quantified over an unnamed "metadata readers" set, and kept the substring grep that let G1 ship green. The auditor also flagged that the 60 s relaxation must reach the other wall-clock assertions in the file and that six FIFO tests at ~42 s each is a CRAN budget problem — both taken as T11.
- 2026-08-09: amendment (substantive) — AC3's "within 10 wall-clock seconds" becomes the audited 60 s bound scoped to Linux and macOS, per spawned program, with the escalation ladder and its measurements stated; AC8 gains the scoped abort-vs-absorb claim over both files plus the lag disclosure, and requires a guard that reddens on the unqualified sentence. AC8 unticked, since its evidence no longer covers what it now asks. Made under a defect return, not an amendment return, so it does not enter the amendment-return count.

- 2026-08-09: T11 — every wall-clock assertion in the file now reads 60 s (four sites). The FIFO fixture gained `skip_on_cran()`: five of these tests ride base R's full escalation ladder on Linux at ~42 s each, and four minutes of waiting is not a reasonable ask of CRAN's machines. `devtools::check()` and the CI workflow both set NOT_CRAN, so the release gate and every push still run them.
- 2026-08-09: T12 — `absorb_timeout()` now returns a classed sentinel carrying the program and limit instead of a bare `NULL`, so a hung file and an unreadable one stop being the same fact. `probe_all()` counts them apart, keeps the NA row, and its warning says how many timed out; the sentinel rides out on a `tm_timed_out` attribute so the documented `list(container, streams)` shape is unchanged. `verify_media()` reads that attribute and re-raises — a probe that never answered is not an answer of "no", and absorbing it was what made `ffm_run(verify=)` blame a successful encode. Mutation probes: dropping the re-raise reddened only the verify test, dropping the warning clause only the naming test.
- 2026-08-09: T12 — the first cut of the re-raise left the suite at WARN 5 rather than the pre-existing 4: `probe_all()` warned and `verify_media()` then aborted about the same hang, telling the caller twice. `verify_media()` now holds the probe's warnings and replays them only when it does not re-raise, replaying the condition objects themselves so class and call survive (F17's trap, avoided). Caught by counting the suite's warnings rather than reading FAIL 0.
- 2026-08-09: T13 — `?tidymedia` and NEWS.md now carry one scoped claim naming which calls abort and which absorb, plus the lag disclosure and base R's own non-guarantee. The substring-grep guard is joined by two that fence the scoped sentence; mutation probe: restoring "A call that reaches the limit aborts" ahead of the scoped paragraph reddened the scoping guard, which is the check AC8 asks for by name.
- 2026-08-09: CI after return 2 — all three Ubuntu jobs PASS, with macOS, Windows, pkgdown and test-coverage green; the AC3 Linux failure that drove this return is gone. Codecov reported patch 94.69% against a 95.82% target and project -0.04%, traced to `verify_media()`'s warning-replay path: it runs only when the abort does NOT fire, so no test reached it. Closed with a test that an unreadable (non-timeout) file still warns through `verify_media()` — a real gap, since holding the probe's warning is only correct because it is replayed. Mutation probe: deleting the replay reddens exactly that test.
- 2026-08-09: local `covr` run named the uncovered lines rather than guessing at the codecov delta: none belong to return 2's changes — the sentinel, `probe_all()`'s timeout counting, `verify_media()`'s hold/replay/re-raise and both MediaInfo branches are all covered. The patch shortfall is `mediainfo()`'s own `resolve_timeout()`/`guard_timeout()` lines from T2, uncovered because the Layer 0 hatch had no execution test at all — its happy path was unexercised. Closed with `mediainfo("--Version")`, which the repo's every-exported-function rule wanted regardless; AC3's carve-out is about a 120-second MediaInfo invocation and is untouched by a version call.
- 2026-08-09: verify slot after return 2 — `devtools::document()` no diff, `devtools::test()` FAIL 0 / PASS 6160 / SKIP 5 with the suite's warning count back to its pre-existing 4, `devtools::check()` `Status: OK` (0/0/0, read from the real status line — M17). `cairn_validate` exit 0. Status back to `review`; CI on PR #72 is the gate that caught the AC3 failure and has not yet run on the fix.
- 2026-08-09: T14 — `mediainfo_query()` test added, covering the `mediainfo_read()` absorber the first return shipped untested. Mutation probe: removing that absorber reddens it, which is the claim the corrected T8 line could not make.

- 2026-08-09: review RETURN -> in-progress (third defect return). What failed: (1) J4 (90), AC8 names `count_audio_streams()` among the readers that absorb a timeout but neither doc mentions it, and measured, it absorbs with zero warnings against the docs' uniform "an NA row and one warning" — an AC8 failure and a user-facing invisible hang under `remove_audio()`; (2) J3 (95), D047 still asserts the uniform-absorption shape T12 replaced inside this same milestone, so AC9's evidence cites a stale entry. Also actioned: J2 (87), the real `verify_media()` re-raise says lowercase `ffprobe` and its fencing test asserts its own mock's literal; J7 (82), the `tm_timed_out` attribute leaks into `print()` and breaks `@param parallel`'s identity promise. AC8 and AC9 unticked; AC1-AC7 and AC10 stand, CI green on all nine checks. Sixteen sub-threshold findings logged. Thrash trigger (a) fires on the third return: no further retry under this plan, routing to `/milestone-plan`.

- 2026-08-26: /milestone-plan re-cut after thrash trigger (a). Investigation found the third return's defect is wider than the review saw: `capture_version()` (`R/ffm_manifest.R:127`) also swallows a timeout into an `NA` version string with no warning, and it appears in no AC list, no doc sentence and none of the sixteen logged sub-threshold findings. AC8's hand-list has now been beaten by a fresh member on every pass, which is the bounded-promise rule's proxy-enumeration failure rather than three unrelated misses.
- 2026-08-26: re-plan gate chose splitting — M69 ships the mechanism with docs narrowed to what is true today, M70 makes absorption uniform — over one re-cut carrying everything on this branch, because AC1-AC7 and AC10 are proven with fresh evidence and green CI on nine checks and their value does not depend on the reader family being uniform, while a single re-cut would put 13+ criteria on the milestone that already thrashed three times. Rejected also: shedding the readers entirely back to master's behavior, which would reopen F1 (one hung file discarding a 500-file corpus). Falsified by M70 finding the split boundary forces a doc rewrite in both milestones rather than one.
- 2026-08-26: re-plan gate chose narrowing AC8 to a three-way description that states it is not a partition of the package, over a fourth attempt at an exhaustive two-way partition, because no procedure this milestone names can enumerate the partition's domain and the repair for a beaten enumeration is a narrower promise, never a longer list (M118/M130; M39's `@param jobs` lesson is the same shape). Falsified by a caller misreading the three-way description as complete.
- 2026-08-26: criteria audit ([O] reader NOT used — this session is configured not to dispatch subagents, so the audit was self-administered by the plan author, which is weaker than the mandate and is recorded as such rather than left to imply an independent read). Full mode, user-facing tier. Eight findings across the M69 and M70 drafts; five fixed before the gate — an instrument-bound criterion over the enumerating sweep (D-118) moved to a task; a swallow-site domain defined by grepping `tryCatch|try_fetch|try(` replaced with a positive call-graph closure, since a wrapper-spelling list fixes membership by recollection; a forced-timeout universal left vacuous on Windows scoped to Linux and macOS, the omission return 2 paid for on AC3; a negative universal over doc prose narrowed to the retired sentence its guard actually fences (M50); and a bare "a D-entry exists" narrowed to a content claim over the `^## D0` headings. One became gate question 4 (the J7 disjunction, satisfiable by whichever branch was cheaper on the day). One went to an M70 task note: the J2 mutation probe must vary the program literal, not merely delete the assertion.

- 2026-08-26: amendment (substantive) — AC8's no-warning clause named `remove_audio()`, which this package does not export; the name came from the third return's own J4 finding text and was carried into the plan unchecked. It also credited the version capture to the manifest, where `tool_versions()` is reached from `ffm_batch()` (`R/ffm_batch.R:158`). Corrected to `extract_audio()`, `convert_audio()`, `separate_audio_video()` and their `_batch` siblings — the five call sites are `R/ffmpeg.R:438,545,638,748,1017` — and to `tool_versions()`/`ffm_batch()`. Promise, domain and behavior bound are unchanged, so this is a naming correction inside AC8's existing scope and not a widening under D-118. Amended wording cleared by the plan author, not by a fresh-context [O] reader: this session cannot dispatch subagents, recorded here rather than left implied.
- 2026-08-26: T15 — both doc sections rewritten to AC8's three-way shape; `devtools::document()` regenerated `man/tidymedia-package.Rd`. The new guard was written FIRST and observed red on all four assertions in both `rd` and `news` (8 failures), which is the "disclosure removed" mutation by construction. Mutation probe for the other half: restoring `A call that reaches the limit aborts` ahead of the scoped paragraph reddened exactly one assertion, `test-runtime-timeout.R:568`, the `expect_no_match` AC8 names — and nothing else; reverted and re-confirmed green. The guard asserts `separate_audio_video` rather than `extract_audio`/`convert_audio`, which the absorbing paragraph already names, so it cannot pass against the old two-way text.
- 2026-08-26: environment drift since 2026-08-09, none of it M69's. `archive` (a declared Import, `R/program_management.R:253`) and `furrr` were missing from the local library and were installed. `mediainfo` is absent as a system binary, so 10 tests skip by design. Local FFmpeg is now 9.0.1, and under it `normalize_audio()` to `.flac` and `.oga` fails at exit 234 ("Could not open encoder before EOF") — 6 failures at `test-audio-stream-normalize.R:462,463,466`. Verified pre-existing by running that file on a clean `origin/master` worktree: identical 6 failures, same lines, same two extensions. Suite on the branch: FAIL 6 / WARN 6 / SKIP 15 / PASS 6139; the 6 warnings are the pre-existing 4 `warn_dropped_audio()` calls plus 2 ffprobe warnings collateral to those same two failures. `devtools::document()` also bumped `Config/roxygen2/version` 8.0.0 -> 8.1.0, a local toolchain bump kept so the recorded version matches the roxygen that generated `man/`.

- 2026-08-26: T16 — D048 appended, superseding D047's readers bullet and recording the shape that actually shipped: `probe_one()`'s sentinel, `probe_all()` keeping the NA row while its warning counts timeouts apart, `verify_media()` re-raising, and the two no-warning paths disclosed rather than fixed. AC9's evidence run: 47 `^## D0` headings; the two entries whose bodies contain "absorb" are D025 and D047, and D025's use is "the milestone absorbed one verb from the pass-through candidate" — unrelated. So D047 is the only entry asserting the uniform-absorption shape, and D048's heading names it superseded in that half while leaving the rest of D047 standing.

- 2026-08-26: T17 — `devtools::document()` no diff; `pkgdown::check_pkgdown()` no problems; `devtools::check()` `Status: 1 ERROR`. The ERROR is read from the check run's own `testthat.Rout.fail`, not inferred: all six failures are `test-audio-stream-normalize.R:462,463,466`, twice, which is the pre-existing FFmpeg 9.0.1 `.flac`/`.oga` regression in M49's container loop, verified earlier against a clean `origin/master` worktree. Suite under check: FAIL 6 / WARN 6 / SKIP 6 / PASS 6165, the 6 warnings being the pre-existing 4 `warn_dropped_audio()` calls plus 2 collateral to those same failures. `mediainfo` was installed at the maintainer's offer, so the 10 MediaInfo tests that had been skipping now run and pass; the remaining 6 skips are 5 nvenc plus one unrelated source-readability skip. No M69 test skipped: every timeout test, the FIFO aborts and both M51-shaped doc guards ran under `R CMD check` against the INSTALLED package and passed, which is the shape the release gate uses.
- 2026-08-26: T17 — CI green on all nine checks of PR #72 (both codecov, macOS, Windows, pkgdown, test-coverage, Ubuntu devel/oldrel-1/release). Ubuntu is where return 2's AC3 failure surfaced and it is green. macOS CI passing the same `normalize_audio()` container loop that fails locally is independent confirmation that the six failures are local FFmpeg-version drift and not a package defect this branch introduced.
- 2026-08-26: AC10 left UNTICKED, deliberately. Its text asks for `devtools::check()` at 0 errors and the local check reports 1. Nothing M69 owns fails, and CI is green on all nine checks — but ticking a box whose criterion says "0 errors" against a run that says "1 ERROR" would be a false record, and this milestone has already been returned three times for evidence that did not match its criteria. Whether CI's green satisfies AC10, or whether the FFmpeg 9 hotfix (now a ROADMAP candidate row) must land first, is a review judgment and is put to review rather than pre-empted here.
- 2026-08-26: status -> review. AC1-AC9 hold with fresh evidence; AC10 open as above.

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

**Independent review — 3 lenses, 20 findings, scored by a fourth agent.**
[S] blame-history: no resurrected bug, contradicted D-entry or weakened guard
(`input = ""` untouched, the D046 snapshot ordering preserved,
`guard_timeout(suppress = TRUE)` net-equivalent to the `suppressWarnings()` it
replaced). [S] prior-review: no prior-review regression; the GitHub inline-
comment probe returned empty so PR threads were not walked. [O] diff-bug: 20
findings, four scoring >=80.

**Actioned (>=80).** All four verified independently before triage:

- **F1 (90) — `probe_all()` aborts on a timeout instead of returning an NA
  row.** `probe_one()` calls `run_program()` with no handler and `probe_all()`
  maps it bare, so the error propagates. Measured:
  `probe_all(c(good, fifo, good))` under a 2 s limit died with
  `purrr_error_indexed`, and `tryCatch(..., tidymedia_timeout = )` does not fire
  because the class survives only as `$parent`. Falsifies `probe_all()`'s
  `@return` and D047's "the readers absorb it exactly as they absorb any other
  error ... a `probe_all()` row reads 'unreadable'". A 500-file corpus with one
  hung file loses every other file's result. **Fix now.**
- **F15 (88) — AC3 is not verified as written.** The criterion says "within 10
  wall-clock seconds"; the tests assert `expect_lt(..., 20)`. The criterion says
  each abort "names the program and the limit"; the `ffprobe()` test checks only
  `"FFprobe"`, and the `ffm_run()` test never asserts `"2 seconds"` and carries
  no wall-clock assertion at all. **Fix now** (the criterion is right, the tests
  are wrong -- a defect return, not an amendment).
- **F3 (85) — `mediainfo_parameter()` aborts mid-loop on a timeout**, discarding
  the partially filled result, where its `@return` promises a warning rather
  than an abort for unreadable files. **Fix now.**
- **F7 (85) — the Layer 0 hatches leave a silently truncated output, and the
  docs say otherwise.** `remove_failed_output()` is wired only into `ffm_run()`.
  Measured: a `ffmpeg()` timeout under a 3 s limit left a **playable 107-second
  MP4** from a 600-second source (620,527 bytes) -- an output that looks
  complete. `?tidymedia` ("Every tidymedia call that touches FFmpeg, FFprobe or
  MediaInfo ... that partial file is removed") and NEWS.md ("any partial output
  the killed run had written is removed") are both false on that branch.
  **Fix now** -- either wire the cleanup or narrow the claim.

**Logged, below the 80 threshold (16).** F4 (78) the re-raised timeout's `call`
resolves to the `try_fetch` handler frame, so the abort reads
``Error in `handlers[[1L]]()` `` and no `parent` is chained; F8 (78) the new
`@section` was inserted before the trailing prose, so roxygen absorbed the
vignette-pointer paragraph into it (confirmed in the rendered Rd); F17 (72)
held warnings are replayed as fresh `simpleWarning`s, losing the original
condition's class and call (raised independently by the blame lens); F5 (70)
`resolve_timeout()` has no upper bound, so `3e9` passes and base R then errors
`invalid 'timeout' argument`; F2 (65) `tidymedia_timeout` does not survive
`probe_all()`'s purrr wrapper; F6 (65) `guard_timeout()` drops held warnings if
`expr` errors; F16 (60) the `expect_no_match(msg, tempdir())` assertion is
near-tautological; F12 (55) the test named "each site resolves a limit of 0" is
a verbatim duplicate of the resolver test and touches no spawn site; F14 (55)
the "control" test calls `system2()` directly, bypassing every package
function, so its stated control property cannot hold; F9 (45) a malformed
option is absorbed silently by `ffm_batch()` and `count_audio_streams()`;
F13 (38) the AC1 test is a source-text grep; F18 (38) `Inf` is refused;
F20 (35) the "byte-for-byte" comment overstates; F10 (30) program-name casing
differs between the reader and Layer 0 paths; F11 (25) three tests are named
`timeout_status()` for a function called `is_timeout()`; F19 (22) a stale
comment says `expr` is evaluated "inside the handler".

**Return.** Two independent triggers: F1 scores >=90 on a defect in what the
package does for its users, and F15 demonstrates AC3 failing as written. Status
back to `in-progress`; AC3 unticked. First defect return for M69 -- the thrash
rule's trigger (a) (third return) and trigger (b) (same AC twice) are both
unfired.

---

## Second pass (2026-08-09, branch `m69-runtime-timeout` @ `9154bc3`)

**Evidence** (fresh; the four returned findings were re-verified first):

- AC1 — all four sites carry one `resolve_timeout(`, one `timeout = limit` and
  one `guard_timeout(` each (counted per file).
- AC2 — `resolve_timeout()` with the option unset returns `0` (run directly).
- **AC3 — FAILS on Linux.** See the gate failure below. On macOS the three
  entry points abort in 2.08 s / 2.04 s / 2.04 s, each naming its program and
  `2 seconds`; on Linux `ffmpeg()` and `ffm_run()` take **42.1 s** against the
  criterion's 10 s. Unticked.
- AC4 — the branch is `identical(as.integer(status), 124L)`; a grep of
  `R/timeout.R` for `grepl|regexpr|regmatches|gsub|sub\(|startsWith|grep\(`
  returns nothing.
- AC5 — both D046 dispositions observed: a pre-existing output survives
  byte-identical with `was left as it was`, and the injected-kill case reads
  `was removed` with the file gone.
- AC6 — `is_timeout(status = 124, limit = 0)` is `FALSE`; `limit = 2` is `TRUE`.
- AC7 — zero warnings signalled across all three entry points; the `ffmpeg()`
  abort message contains no `tempdir()` substring and no command line.
- AC8 — `man/tidymedia-package.Rd` carries `tidymedia.timeout`, `second`,
  `no limit`, `abort`; `NEWS.md` carries the entry. (The claim behind the
  `abort` substring is now over-broad — G1 below.)
- AC9 — `cairn/DECISIONS.md:1941`, D047.
- **AC10 — unticked.** `devtools::document()` no diff, `devtools::test()`
  FAIL 0 / PASS 6132 / SKIP 5 and `devtools::check()` `Status: OK` (0/0/0) on
  the dev machine, but the same suite is FAIL 2 on Linux, so the slot is not
  clean everywhere.

**Consistency gate:** `cairn_validate` exit 0, all checks PASS, one advisory
(`M69: 10 acceptance criteria (>7 tripwire)`). `cairn_impact` not run —
Principles touched is `—`. Toolchain slot: `document()` no diff · generated
files unedited · README untouched · `pkgdown::check_pkgdown()` no problems ·
NEWS.md entry present with no milestone or decision ids · no new top-level
files. **`devtools::check()` clean locally, red on CI.**

**GATE FAILURE — AC3 fails on Linux, and CI has been red since `afaf950`.**
PR #72's R-CMD-check has failed on every Linux job for all four branch commits;
macOS, Windows and pkgdown pass. At `afaf950` the single failure was
`test-runtime-timeout.R:202` (`ffmpeg()`) at **42.0 s against the then-20 s
bound**; tightening the bound to the criterion's 10 s at `9154bc3` made it two
failures, `ffmpeg()` and `ffm_run()`, both at **42.1 s**. The first review pass
recorded no CI evidence and did not look. Three Linux jobs (release, devel,
oldrel-1) agree; macOS aborts in ~2 s, so this is platform-specific and not
reproducible on the dev machine. Unverified hypothesis for implement to settle:
the two failing entry points are exactly the two that pass `input = ""`
(`ffmpeg()` via `system()`, `ffm_run()` via `run_program()`), while `ffprobe()`,
which passes no input, aborts inside the bound and passes on Linux — and
`?system2` states a timeout runs the command with stdin redirected from
`/dev/null`, which is a second redirection of the same channel.

**Independent review — 3 lenses, 16 findings, scored by a fourth agent.**
[S] blame-history: no resurrected bug, contradicted D-entry or weakened guard;
D002/D024/D046 boundaries respected and the four returned findings correctly
fixed. [S] prior-review: `cairn/milestones/archive/` holds no `## Review`
findings on these files, and the `gh api .../pulls/comments` probe returned
empty, so PR threads were not walked; two candidates, both also raised by
another lens. [O] diff-bug: ten findings, two scoring >=80.

**Actioned (>=80).**

- **G2/H4 (88) — a timed-out probe inside `verify_media()` is reported as a
  property mismatch, not a timeout.** `R/verify.R:81` calls `probe_all()`,
  which this branch's T8 fix made absorb the timeout; the tibbles come back NA
  and `compare_expectations()` marks every check failed with `actual = NA`.
  `ffm_run(verify = list(width = 1920))` then aborts with "expected 1920, got
  NA", blaming a successful encode for a hung FFprobe. Verified by both the
  diff-bug and blame-history lenses independently, the former by execution.
  A regression T8 introduced on the D011 verification path, which no AC and no
  part of D047 considers. **Fix in the return.**
- **G1 (85) — `?tidymedia` and `NEWS.md` still claim every timed-out call
  aborts.** T10 narrowed the *partial-output* claim but left "A call that
  reaches the limit aborts, naming the program and the limit" as an unqualified
  universal. After T8 that is false for every metadata reader — `probe_all()`,
  `mediainfo_*()`, the `get_*()` helpers — which warn and return NA. Same shape
  as F7, at the same two files, created by fixing F1. AC8's test greps only for
  the substring `abort`, so it passes while the claim is over-broad.
  **Fix in the return.**

**Logged, below the 80 threshold (12).** G6/P2 (78) `mediainfo_read()`'s
absorber ships with no test at all, and the T8 work-log line's "removing each
absorber reddened exactly its own test" is therefore false as written; P1 (76)
T10 appended more prose to the `@section` that F8 already showed swallows the
vignette-pointer paragraph, leaving F8 unaddressed and less isolated; H2 (75)
= F4, the re-raised timeout's `call` is the handler frame; G3 (68)
`run_loudnorm_analysis_batch()` is a fourth site where a timeout still kills
the whole fan-out, milder than F1 because `purrr_error_indexed` carries the
index; H1 (68) = F17, held warnings replayed as fresh `simpleWarning`s; H3 (65)
= F5, no upper bound in `resolve_timeout()`; G9 (55) the timeout abort omits
the "failing command" bullet its exit-status sibling carries; G7 (45) the new
F7 test is near-tautological on its own and mocks `find_ffmpeg()` to a
`shQuote()`d value it could never return; G4 (35) a hung file stays
indistinguishable from a corrupt one — D047's own disclosed falsifier; G8 (30)
the `input=` comments no longer describe the only mechanism at work; G5 (15)
`probe_one()`'s `if (is.null(out)) return(NULL)` is dead code; G10 (12) a
non-ASCII byte in an R comment.

**Return (pass 2).** Step-4 gate failure: AC3 fails on Linux with fresh CI evidence, and
the PR's checks are red. G2/H4 and G1 are actioned into the same return. Status
back to `in-progress`; AC3 and AC10 unticked, AC1/AC2/AC4–AC9 evidence stands.
Second defect return for M69. Thrash trigger (a) (third return) is unfired.
**Thrash trigger (b) FIRES** — AC3 has now failed twice, by two mechanisms of
one shape: pass 1 found the tests did not measure what the criterion says, and
pass 2 finds the package does not do what the criterion says on Linux. The
remedy is to reconsider the alternative the plan gate recorded against; the
gate recorded none about the timeout *mechanism* — whether base R's `timeout=`
can deliver a bounded abort on every platform, or whether that needs real
process control — so escalation via `/milestone-brief` is offered per instance
(D-004), never automatically.


---

## Third pass (2026-08-09, branch `m69-runtime-timeout` @ `fd988de`)

**Evidence** (fresh; AC3 and AC8 were amended since pass 2, so their pass-2
evidence no longer applies and was re-gathered against the new wording):

- AC1 — all four spawn sites carry one `resolve_timeout(`, one
  `timeout = limit` and one `guard_timeout(` each, counted per file.
- AC2 — `resolve_timeout()` with the option unset returns `0` (run directly).
- AC3 (amended) — on the FIFO fixture under a 2 s limit: `ffmpeg()` 2.08 s,
  `ffprobe()` 2.05 s, `ffm_run()` 2.05 s, all inside the 60 s bound, each
  aborting with class `tidymedia_timeout` and a message naming its program and
  `2 seconds`. The bound's reason for being 60 rather than 2 is the escalation
  ladder measured in the work log; Linux CI now passes on all three jobs, which
  is the platform the criterion was amended for.
- AC4 — the branch is `!is.null(status) && identical(as.integer(status), 124L)`;
  `R/timeout.R` contains zero text-matching calls.
- AC5 — both D046 dispositions observed: a pre-existing output survives
  byte-identical with `was left as it was: FFmpeg never wrote to it.`, and the
  injected-kill case reads `The incomplete ... was removed.` with the file gone.
- AC6 — `is_timeout(status = 124, limit = 0)` is `FALSE`; `limit = 2` is `TRUE`.
- AC7 — zero warnings signalled across all three entry points.
- AC8 (amended) — both `man/tidymedia-package.Rd` and `NEWS.md` carry
  `tidymedia.timeout`, `second`, `no limit`, `abort`, `absorb`, `probe_all`,
  `verify_media`, `40 seconds` and `guarantee`, and neither contains the
  unqualified `A call that reaches the limit aborts`. The criterion's own
  verification clause was executed rather than cited: restoring that sentence
  ahead of the scoped paragraph reddened `both docs scope the abort and name
  the readers that absorb instead`, and only that test.
- AC9 — `cairn/DECISIONS.md:1941`, D047.
- AC10 — `devtools::document()` no diff; `devtools::test()` FAIL 0 / PASS 6166 /
  SKIP 5 with the warning count at its pre-existing 4; `devtools::check()`
  `Status: OK`, 0 errors / 0 warnings / 0 notes. CI at `fd988de` is green on all
  nine checks — the three Ubuntu jobs that failed pass 2 among them, plus both
  codecov statuses, which the new `mediainfo("--Version")` test carried over the
  patch-coverage line.

**Consistency gate:** `cairn_validate` exit 0, all checks PASS, two advisories
(`M69: 10 acceptance criteria (>7 tripwire)`, `M69: 14 tasks (>10 tripwire)`) —
both grew across two defect returns and are recorded rather than acted on, since
splitting a milestone at its merge gate would discard the branch. `cairn_impact`
not run — Principles touched is `—`. Toolchain slot: `document()` no diff ·
generated files unedited · README untouched · `pkgdown::check_pkgdown()` no
problems · NEWS.md entry present with no milestone or decision ids · no new
top-level files.

**Independent review — 3 lenses, 20 findings, scored by a fourth agent.**
[S] blame-history: no resurrected bug or weakened guard; D002/D011/D024/D046
boundaries respected, F7's and G1's narrowings both intact, and
`verify_media()`'s replay correctly avoids F17's trap by replaying the condition
object. [S] prior-review: the `gh api .../pulls/comments` probe returned empty
and no archived `## Review` findings touch these files. [O] diff-bug: eighteen
findings. Four score >=80, and D047's staleness was raised independently by all
three lenses.

**Actioned (>=80).** The first two were verified by the reviewing session's own
execution before triage, not taken on the lens's word:

- **J3 (95) — D047 is stale; it asserts the behaviour T12 replaced.** D047 says
  "The readers absorb it exactly as they absorb any other error… Making them
  re-raise would change `probe_all()`'s error contract… The distinct class is
  what leaves that available later without re-deciding anything now." T12
  re-decided it inside this same milestone: `verify_media()` re-raises and
  `probe_all()` distinguishes timeouts in its warning and on an attribute.
  **Demonstrates AC9 failing** — AC9's evidence cites D047 as recording the
  shape, and the shape recorded is no longer the branch's. D047 is also silent
  on return 2's load-bearing choice (keeping base R's ladder over `processx`),
  which lives only in a work log that gets archived. Raised by all three lenses.
- **J4 (90) — AC8's absorb set names `count_audio_streams()`; neither doc does,
  and it absorbs silently.** Measured: `count_audio_streams()` returns
  `NA_integer_` with **zero warnings** on a timeout, while both docs describe
  absorption uniformly as "an `NA` row and one warning at the end of the call".
  **Demonstrates AC8 failing**, inside the domain of the two files AC8 names,
  and it is also a user-facing defect: a hung dropped-track probe under
  `remove_audio()` is invisible.
- **J2 (87) — the real re-raise says `ffprobe`, and the test fencing it asserts
  its own mock.** Measured on a real FIFO: `verify_media()` aborts with
  `ffprobe timed out after 2 seconds.` in lowercase against the Layer 0 hatch's
  `FFprobe`. The test mocks `run_program` with a hard-coded
  `abort_timeout("FFprobe", 2)`, so `expect_match(msg, "FFprobe")` tests the
  mock's literal, not the code path. No AC failure; a vacuous assertion plus a
  message inconsistency.
- **J7 (82) — `probe_all()`'s `tm_timed_out` attribute leaks into print and
  breaks the parallel-vs-sequential identity promise.** Attached to a bare list
  with no print method, so `print(probe_all(hung))` dumps the attribute and its
  class. `@param parallel` promises "Output is identical either way"; workers
  never see the option, so only the sequential path carries the attribute. No AC
  failure — no criterion polices that docstring — but a real behavioral surprise.

**Logged, below the 80 threshold (16).** J1 (78) a timed-out
`ffm_batch(verify=)` is completely silent, the hold-and-replay plus
`ffm_batch()`'s `tryCatch` erasing both the warning and the abort; L1 (78) the
`@section` still swallows the vignette-pointer paragraph and T13 made it longer
— a third consecutive pass raising F8; J6 (76) = F4, `ffm_run()`'s re-raise
reads `Error in handlers[[1L]](cnd)`; J13 (72) = F17, `guard_timeout()` replays
bare `simpleWarning`s while `verify_media()` in the same milestone does it
correctly; J9 (65) = F5, no upper bound in `resolve_timeout()`; J5 (65) the
`*_batch` verbs do not abort, arguably over-broadening "the task verbs abort";
J11 (58) AC8's positive doc guards are still substring greps and one negative
guard can never redden; J17 (50) the new `mediainfo("--Version")` test assumes
stdout; J10 (48) `?verify_media` and `?probe_all` never mention the timeout;
J15 (45) the sentinel swap weakened two `is.null()` guards, latent only; J16
(42) the sentinel protocol rests on three hand-written call-site checks; J12
(40) the fixture's Linux cost comment undercounts; J8 (35) "1 of these" reads
wrong at n=1; J18 (30) not findings; K1 (25) `skip_on_cran()` weighed against
M46 and judged non-conflicting; J14 (20) = F19, a stale comment.

**Return (pass 3).** Two findings demonstrate criteria failing: J4 (90) on AC8
and J3 (95) on AC9, both inside the domain of what those criteria name. J4 is
independently a >=90 user-facing defect. AC8 and AC9 unticked; AC1-AC7 and AC10
evidence stands, and CI at `fd988de` is green on all nine checks.

**Thrash rule — trigger (a) FIRES: this is the third defect return.** The
threshold holds once reached, so no further retry is queued under the current
plan; the milestone routes to `/milestone-plan` for a re-cut or split. No
re-plan or split has been spent on M69 yet, so re-plan-or-split is still the
available remedy rather than the move that already failed. Two independent
corroborations that the milestone is mis-sized rather than merely unlucky:
`cairn_validate` now warns on both split tripwires (10 acceptance criteria
against >7, 14 tasks against >10), and each return has been in a different
area — the readers, then the platform, now the decision record and the doc set.
Trigger (b) fired at pass 2 on AC3's second failure; its diagnosis and its
per-instance `/milestone-brief` escalation offer carry into that routing rather
than being discarded.
