<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M086: The catchable failure reaches the two paths M085 left behind

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m086-catchable-failure-leftovers`

## Goal

`?tidymedia` promises that a failed FFmpeg run raises `tidymedia_ffmpeg_exit`
carrying `tm_status`; two shipped paths break that promise, and this milestone
makes both honour it.

## Scope

**Surface tier: user-facing** — the deliverable is condition classes and help
text that package callers write `tryCatch()` handlers against.

**In:** (a) `separate_audio_video()`'s multi-track diagnostic
(`R/ffmpeg.R:668-687`) gains `tidymedia_ffmpeg_exit` in its class vector and
the `tm_status` field, keeping its enrichment and its `parent` chain intact;
(b) `normalize_audio_batch(two_pass = TRUE)`'s analysis-pass abort stops
discarding the exit number at `R/loudnorm_two_pass.R:219` and gains its own
event class at `R/loudnorm_two_pass.R:234`; (c) the roxygen and NEWS that
tell a caller which class catches what.

**Out:**
- The package-wide class sweep — 160 of 173 `cli_abort()` sites carry no
  class → ROADMAP candidate row. It needs an enumeration procedure that a
  grep does not provide, and the mixed-cause sites cannot honestly carry
  `tidymedia_ffmpeg_exit` at all.
- `separate_audio_video_batch()`'s post-fan-out warning carrying a status →
  ROADMAP candidate row. `ffm_batch()`'s `run_one()` reduces each row to
  `list(success =, timed_out =)` and discards the condition, so no status
  exists at that site; the warning also fires for any failure cause, which
  D062's event rule forbids classing as an exit, and a `tryCatch()` on it
  would unwind the batch against D007.
- Renaming `tm_status` or reshaping `ffm_run()`'s documented length-one field.

## Acceptance criteria

- [ ] AC1 A `separate_audio_video()` call whose audio output FFmpeg refuses on
      a multi-track input with no `audio_stream` named raises a condition that
      (i) a `tryCatch(tidymedia_ffmpeg_exit = ...)` handler catches, (ii) still
      inherits `tidymedia_multitrack_separation`, (iii) still renders the track
      count and both ways out in its message, and (iv) carries `tm_status`
      equal to the integer status of its `parent` condition.
      (RB tripwire: irreversible-api)
- [ ] AC2 Each of these four failures, provoked through a real
      `separate_audio_video()` call rather than by constructing a condition
      and asserting on it, raises a condition that does not inherit
      `tidymedia_multitrack_separation`, whose message matches none of
      `audio tracks`, `audio_stream` or `.mka`, and which is the original
      condition object re-raised unchanged in class and message: (a) FFmpeg
      unresolvable on a multi-track input, taking `run_separation_audio()`'s
      `is.na(status)` fail-open branch, whose condition is `run_program()`'s
      own abort carrying no `tidymedia_*` class and no `tm_status`;
      (b) FFprobe unresolvable (`is.na(n)`); (c) a single-track input
      (`n <= 1L`); (d) a timeout forced at the spawn site
      (`tm_force_timeout()`), whose condition is `abort_timeout()`'s,
      inheriting `tidymedia_timeout`. Cases (b) and (c) re-raise
      `ffm_run()`'s non-zero-exit condition and so do inherit
      `tidymedia_ffmpeg_exit`; cases (a) and (d) do not.
- [ ] AC3 `?separate_audio_video`'s *When the audio output fails* section
      states both class names, says which one an exit-status handler catches
      and which the enriched diagnostic answers to, and shows a handler that
      fires on this path; the claim is verified by running that handler against
      a real failing call, not by inspection.
- [ ] AC4 `normalize_audio_batch(two_pass = TRUE)` aborting on offending rows
      raises class `tidymedia_loudnorm_analysis` carrying `tm_rows` (the
      1-indexed offending rows, in the order the message names them) and
      `tm_row_status` (an integer vector aligned to `tm_rows`, the row's FFmpeg
      exit status, `NA_integer_` where the row exited zero but printed no
      parseable block). Asserted over three batches: exit-failures only,
      unparseable only, and one of each.
- [ ] AC5 `?ffm_run`, `?tidymedia`'s exit-class paragraph
      (`R/tidymedia-package.R:113-117`) and `NEWS.md` each name the two paths
      AC1 and AC4 change and the class each now signals; `?tidymedia` no longer
      states a promise that the `separate_audio_video()` path contradicts.
- [ ] AC6 `devtools::check()` reports 0 errors and 0 warnings, and the AC1–AC4
      tests are recorded as having RUN (not skipped) on a machine with ffmpeg
      and ffprobe present — the skip count for those files is quoted in review.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T5
- AC4 → T3, T4
- AC5 → T5, T6
- AC6 → T7

## Tasks

- [x] T1 Extend `tests/testthat/test-ffmpeg-exit-condition.R` with the scalar
      separation grid, red before T2: the enriched path varied over container
      (`.aac`, `.mp3`, `.wav`) and over at least two distinct exit statuses,
      plus AC2's four near-miss cases (the three fail-open branches and a
      forced timeout). Fixture via `make_multitrack_video()`
      (`tests/testthat/helper-media.R:197`); `skip_if` the binaries are absent.
- [x] T2 `R/ffmpeg.R:675` — class vector becomes
      `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")` and the
      abort gains `tm_status = status`, which the branch already holds
      non-`NA` at `R/ffmpeg.R:653`. Message text unchanged.
- [x] T3 New tests for the two-pass batch abort, red before T4: the three
      batches of AC4, asserting class, `tm_rows`, and `tm_row_status`
      including the `NA_integer_` element.
- [x] T4 `R/loudnorm_two_pass.R:219` — carry the status instead of collapsing
      to `list(status = "error")`; `R/loudnorm_two_pass.R:234` — the abort
      gains `class = "tidymedia_loudnorm_analysis"`, `tm_rows` and
      `tm_row_status`. Message text unchanged.
- [x] T5 Roxygen: `R/ffmpeg.R:846` (the scalar *When the audio output fails*
      section, per AC3), `R/ffm.R:1539-1549` (`ffm_run()`'s class paragraph),
      `R/tidymedia-package.R:113-117`; then `devtools::document()`.
- [x] T6 `NEWS.md` bullet under the development version naming the two paths.
- [x] T7 `devtools::check()`; record the run/skip counts AC6 asks for.

## Work log

- 2026-08-29: created by /milestone-plan. Promoted from the standing `M45 review F1/F5` candidate row (M085 Out; M085 review F4); that row is narrowed, not deleted, and keeps its unpromoted gaps.
- 2026-08-29: criteria audit ran in FULL mode (user-facing tier plus an `irreversible-api` tripwire on AC1), fresh-context `[O]` reader over seven drafted criteria. It returned findings on all seven. Cut at the gate: the draft AC2 (batch warning) as unsatisfiable — no status exists at that site under D007 — and the draft AC5 ("every abort site") as an unbounded promise over a domain no named procedure enumerates. Fixed at the gate: the draft AC1 was satisfiable by deleting the diagnostic and by rewording the message, its probe was one exemplar for a four-axis family, and its "pre-change test failing" clause bound a harness-history property; the draft AC3 anchored to generated `man/*.Rd` line numbers and to a warning recipe on an error path; the draft AC7 was green-compatible with zero evidence on a binary-less machine.
- 2026-08-29: amendment (AC2, substantive, user-approved at the mini gate). The shipped AC2 demanded that all five near-miss cases not inherit `tidymedia_ffmpeg_exit` while also demanding the fail-open cases re-raise the original condition unchanged in class; measured on ffmpeg 9.0.1, the `is.na(n)` and `n <= 1L` branches re-raise `ffm_run()`'s abort, whose class vector is `c("tidymedia_ffmpeg_exit","rlang_error","error","condition")`, so those two clauses contradicted each other. Amended text: `amendment: AC2 — "Each of these four failures, provoked through a real separate_audio_video() call rather than by constructing a condition and asserting on it, raises a condition that does not inherit tidymedia_multitrack_separation, whose message matches none of `audio tracks`, `audio_stream` or `.mka`, and which is the original condition object re-raised unchanged in class and message"`, with the four cases enumerated in the criterion and the exit-class claim split per case. T1's wording follows (minor).
- 2026-08-29: criteria audit re-ran in FULL mode over the amended AC2, two fresh-context `[O]` readers (the second over the wording repaired from the first). First reader: the criterion bound an instrument (a test file's contents and comment headings) rather than the deliverable, left the domain open at the top, stood one exemplar in for the `is.na(status)` family, and left the two not-this-failure cases constrained only negatively. Second reader over the repair: the "enrichment's three clauses" phrasing was unsatisfiable by referent and is now three literal strings; "unchanged in class and message" was restored after the repair had weakened it to a class-vector comparison, which D024's fail-open consequence requires; the direct `run_program()` case was folded into (a) as below the user-facing tier and duplicative; the timeout case now travels a real `separate_audio_video()` call via `tm_force_timeout()` instead of asserting on a constructed condition; "unclassed" became "no `tidymedia_*` class". Both readers' remaining findings were fixed before the gate.
- 2026-08-29: plan gate chose adding `tidymedia_ffmpeg_exit` to the multi-track abort's class vector over documenting the `cnd$parent` chain, because it makes `?tidymedia`'s shipped promise true rather than walking it back, and D062 leaves class hierarchies open; falsified by a caller needing the two sites — the error and the batch warning — to answer to one handler pair, which this deliberately breaks.
- 2026-08-29: plan gate chose a new `tidymedia_loudnorm_analysis` event class for the two-pass batch abort over narrowing `tidymedia_ffmpeg_exit` to the exit-only case, because the abort also fires on rows that exited zero and D062 requires a class to name the fact that occurred; falsified by the package finding a second site with the same mixed-cause shape where one class per cause reads better than one per event.

- 2026-08-29: T1+T2 in one commit so the branch never checkpoints red. T1's AC1 grid was red first (the `tidymedia_ffmpeg_exit` handler did not catch the enriched abort); T2 added the class and `tm_status`, and `devtools::test()` is clean at 8306 passes, 0 failures, 5 skips.
- 2026-08-29: T3+T4 in one commit, same red-then-green shape. The three AC4 batches drive the exported `normalize_audio_batch(two_pass = TRUE)` with `run_loudnorm_analysis_batch()` mocked to recorded stderr, so the abort under test is the verb's own and no binary is needed; `devtools::test()` clean at 8318 passes.
- 2026-08-29: T5+T6. Roxygen on `?separate_audio_video`, `?ffm_run`, `?tidymedia` and `?normalize_audio_batch` (the last is where AC4's class is user-visible, added under Scope In (c)); `devtools::document()` rewrote four Rd files. AC3's handler is verified by executing it against a real failing call, not by reading the page; the Rd/NEWS guard was proven able to fail by planting `tidymedia_PLANTED_DEFECT` over the class name in `man/ffm_run.Rd` and watching it go red, then restoring the file.
- 2026-08-29: `NEWS.md`'s M085 bullet said "Two paths deliberately do not signal it", naming `separate_audio_video()` as one; that became false on this branch and was corrected in place (the changelog describes an unreleased development version). The new bullet names both changed paths and their classes. `devtools::test()` clean at 8334 passes, 0 failures, 5 skips.
- 2026-08-29: T7. First `devtools::check()` came back `Status: 1 NOTE` — the spelling test flagged "unresolvable" in the new `separate_audio_video` prose; reworded to "an FFmpeg the package cannot locate" rather than growing `inst/WORDLIST`, re-documented, re-checked. Second run: `Status: OK` (0 errors, 0 warnings, 0 notes).
- 2026-08-29: AC6 counts on this machine (ffmpeg 9.0.1 and ffprobe both on PATH), each file run on its own: `test-ffmpeg-exit-condition.R` 101 passes / 0 skips, `test-normalize-audios-two-pass.R` 78 / 0, `test-separate-av-multitrack.R` 75 / 0. Full suite 8334 passes, 0 failures, 5 skips (none in those three files). Status set to review.
## Decisions

## Review
