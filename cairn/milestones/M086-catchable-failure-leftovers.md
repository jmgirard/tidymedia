<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M086: The catchable failure reaches the two paths M085 left behind

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m086-catchable-failure-leftovers` / https://github.com/jmgirard/tidymedia/pull/90

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

- [x] AC1 A `separate_audio_video()` call whose audio output FFmpeg refuses on
      a multi-track input with no `audio_stream` named raises a condition that
      (i) a `tryCatch(tidymedia_ffmpeg_exit = ...)` handler catches, (ii) still
      inherits `tidymedia_multitrack_separation`, (iii) still renders the track
      count and both ways out in its message, and (iv) carries `tm_status`
      equal to the integer status of its `parent` condition.
      (RB tripwire: irreversible-api)
- [x] AC2 Each of these four failures, provoked through a real
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
- [x] AC3 `?separate_audio_video`'s *When the audio output fails* section
      states both class names, says which one an exit-status handler catches
      and which the enriched diagnostic answers to, and shows a handler that
      fires on this path; the claim is verified by running that handler against
      a real failing call, not by inspection.
- [x] AC4 `normalize_audio_batch(two_pass = TRUE)` aborting on offending rows
      raises class `tidymedia_loudnorm_analysis` carrying `tm_rows` (the
      1-indexed offending rows, in the order the message names them) and
      `tm_row_status` (an integer vector aligned to `tm_rows`, the row's FFmpeg
      exit status, `NA_integer_` where the row exited zero but printed no
      parseable block). Asserted over three batches: exit-failures only,
      unparseable only, and one of each.
- [x] AC5 `?ffm_run`, `?tidymedia`'s exit-class paragraph
      (`R/tidymedia-package.R:113-117`) and `NEWS.md` each name the two paths
      AC1 and AC4 change and the class each now signals; `?tidymedia` no longer
      states a promise that the `separate_audio_video()` path contradicts.
- [x] AC6 `devtools::check()` reports 0 errors and 0 warnings, and the AC1–AC4
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

Reviewed 2026-08-29 on branch `m086-catchable-failure-leftovers` at d095a1d,
PR https://github.com/jmgirard/tidymedia/pull/90. `origin/master` had not moved
since the branch was cut, so no merge was needed. Machine: macOS 26.5 / arm64,
R 4.6.1, ffmpeg 9.0.1 and ffprobe both on PATH. No Driving RR, so no
projection-vs-outcome pairs are owed.

### Acceptance-criteria evidence

- AC1 — verified. A 3-track `.mkv` separated into `.mp3` and into `.wav` was
  caught by `tryCatch(tidymedia_ffmpeg_exit = )` in both cases; the class
  vector was `tidymedia_multitrack_separation, tidymedia_ffmpeg_exit,
  rlang_error, error, condition`; the message carried `3 audio tracks`,
  `audio_stream` and `.mka`; `tm_status` was 234 and identical to
  `cnd$parent$tm_status`. A fourth cause on the same path — a non-existent
  output directory — gave `tm_status` 254, so the field tracks the parent
  rather than a pinned constant. Run outside the harness as well as in
  `test-ffmpeg-exit-condition.R`.
- AC2 — verified, all four cases provoked through real `separate_audio_video()`
  calls. None inherited `tidymedia_multitrack_separation` and none of the three
  phrases appeared in any message. (a) FFmpeg unresolvable: class
  `rlang_error, error, condition`, no `tidymedia_*` class, `tm_status` NULL,
  and both class vector and message identical to `run_program()`'s own abort
  captured independently. (b) FFprobe unresolvable and (c) single-track input:
  class exactly `tidymedia_ffmpeg_exit, rlang_error, error, condition` with
  integer `tm_status` 234 — the per-case exit-class split the criterion states.
  (d) `tm_force_timeout()`: class `tidymedia_timeout, rlang_error, error,
  condition`, no exit class, `tm_status` NULL.
- AC3 — verified by execution, not inspection. The handler printed in
  `?separate_audio_video`'s *When the audio output fails* section was run
  verbatim against a real failing call and returned integer 234, length 1. The
  section names both `tidymedia_ffmpeg_exit` (the exit-status handler's class,
  with the number on `tm_status`) and `tidymedia_multitrack_separation` (the
  enriched diagnostic's own), and says which catches which.
- AC4 — verified over the three batches, driving the exported
  `normalize_audio_batch(two_pass = TRUE)` with Phase 1 mocked to recorded
  outputs. Exit-failures only: `tm_rows` 1, 3 and `tm_row_status` 1L, 234L.
  Unparseable only: `tm_rows` 2, 3 and `tm_row_status` NA, NA. One of each:
  `tm_rows` 1, 3 and `tm_row_status` NA, 69L. Both fields are integer vectors,
  aligned, and `tm_rows` matches the rows the message names
  (`Offending rows (1-indexed): 1 and 3`). The condition does not inherit
  `tidymedia_ffmpeg_exit` and carries no `tm_status`, including on the
  exit-only batch.
- AC5 — verified against the generated Rd files and NEWS. `man/ffm_run.Rd`
  names `separate_audio_video`, `tidymedia_loudnorm_analysis` and
  `tm_row_status`; `man/tidymedia-package.Rd` names `separate_audio_video` and
  `tidymedia_loudnorm_analysis`; `man/normalize_audio_batch.Rd` names the
  class and `tm_rows`; `man/separate_audio_video.Rd` names both classes and
  `tm_status`. `NEWS.md` carries both new class names, and no longer says
  "Two paths deliberately do not signal it" — the promise the separation path
  now contradicts. No milestone numbers appear in NEWS.
- AC6 — verified. `devtools::check()` on this branch: `Status: OK`, 0 errors,
  0 warnings, 0 notes, 2m 40.1s. The AC1–AC4 test files were RUN, not skipped,
  on this machine (ffmpeg 9.0.1 and ffprobe on PATH), each run on its own:
  `test-ffmpeg-exit-condition.R` 101 passes / 0 skips,
  `test-normalize-audios-two-pass.R` 78 / 0, `test-separate-av-multitrack.R`
  75 / 0. Full suite `[ FAIL 0 | WARN 12 | SKIP 5 | PASS 8334 ]`; none of the
  five skips falls in those three files. The suite's 12 testthat warnings are
  pre-existing: the three files above report `warn=0` when run individually,
  and `check()` itself reports 0 warnings.

### Consistency gate

- `cairn_validate.py` — exit 0, all checks passed; no advisory fired, the
  `release window` advisory included.
- No `DESIGN.md` principle changed, so `cairn_impact.py` was not run.
- `r-package` profile `consistency-gate` slot: `devtools::document()` produced
  no diff; no generated file was hand-edited; `README.Rmd` is untouched by this
  branch so `README.md` is in sync; `pkgdown::check_pkgdown()` reported no
  problems; `NEWS.md` carries the user-visible entry with no milestone numbers;
  no new top-level files, and `check()` raised no `.Rbuildignore` note;
  `devtools::check()` clean as recorded under AC6.

### Independent review

Three fresh-context lenses, none having seen the implementation, each on a
distinct evidence base. The `[S]` prior-review lens ran its probe
(`gh api .../pulls/comments?per_page=1`) and found no real inline PR threads,
so it worked from the archived `## Review` sections, `cairn/LESSONS.md` and
`cairn/references/false-greens.md`; it reported no prior-review evidence
regressed and contributed zero findings. The `[S]` blame-history lens
confirmed the change is additive — message text byte-for-byte unchanged from
`master` at both sites — and found no undone milestone work and no
contradicted D-entry. The `[O]` diff-bug lens returned ten findings.

Every finding and its disposition:

- F1 (`[O]` 1, `[S]` blame 1 — the same defect from two lenses) Stale comments
  that state the contract the branch just deleted.
  `tests/testthat/test-ffmpeg-exit-condition.R:85-86` says "A timeout, and the
  multi-track diagnostic: both are tidymedia conditions and neither is a
  non-zero exit"; the real diagnostic now is one. The assertion below it still
  holds — it builds a bare `tidymedia_multitrack_separation` condition with no
  `tm_status`, which is a valid probe of `ffmpeg_exit_status()`'s class guard —
  but the comment names it as the shipped diagnostic, which it no longer is.
  `R/ffmpeg.R:781-782` has the same shape: it lists `ffm_run()` and the
  loudnorm analysis pass as the raisers of `tidymedia_ffmpeg_exit`, and this
  branch made the separation diagnostic a third. Verified by reading both
  sites. Disposition: fix now.
- F2 (`[O]` 4) `NEWS.md:38` contradicts the bullet two lines below it. The
  M085 bullet was amended from "Two paths deliberately do not signal it" to
  "One path", naming only the `ffm_batch()` family — but the same commit
  created a second such path, `normalize_audio_batch(two_pass = TRUE)`, whose
  new test asserts it does not inherit `tidymedia_ffmpeg_exit` even when every
  offending row is an exit failure. Verified by reading `NEWS.md:30-48`
  against `test-normalize-audios-two-pass.R`. A caller who reads the first
  bullet and wraps a two-pass batch in one exit handler is not caught.
  Disposition: fix now.
- F3 (`[O]` 2) `tests/testthat/test-ffmpeg-exit-condition.R:198`'s
  `expect_gt(length(unique(statuses)), 1L)` asserts a property of the local
  FFmpeg build, not of the package: it holds here only because ffmpeg 9.0.1
  exits 234 on a muxer refusal and 254 on a failed output open. A build that
  returns the same number for both makes the test red on correct code.
  Disposition: see the CI evidence line below.
- F4 (`[O]` 3) `adts_refuses_multistream()`
  (`test-ffmpeg-exit-condition.R:135-147`) calls `system2("ffmpeg", ...)`
  directly rather than through `find_ffmpeg()`, and reads any non-zero result
  — including "could not run ffmpeg at all" — as "the muxer refuses". On a
  machine with FFmpeg configured off-PATH via `set_ffmpeg()`, the guard
  wrongly appends the `.aac` case, the call succeeds, and the failure surfaces
  as a class assertion against a character vector. Disposition: see below.
- F5 (`[O]` 5) `tidymedia_loudnorm_analysis` names a phase rather than an
  event, and the scalar `normalize_audio(two_pass = TRUE)` analysis abort
  (`R/loudnorm_two_pass.R:151`) does not carry it — so the obvious handler
  written on the class name fires only for the batch. Real ergonomics gap, but
  the batch/scalar split is what the plan gate chose and Scope In names only
  the batch site. Disposition: follow-up candidate row.
- F6 (`[O]` 9) The scalar error site now carries two classes while the batch
  warning site (`R/ffmpeg.R:742`) still carries one, and
  `?separate_audio_video_batch` still tells callers to suppress by the single
  name. Correct as shipped, but this is the severity split D062 named as its
  own falsifier, and nothing records that the branch widened it. Disposition:
  fold into the existing batch-warning candidate row.
- F7 (`[O]` 8) The new `?separate_audio_video` paragraph says a reached limit
  "answers to neither class" without naming `tidymedia_timeout`, which is the
  class it does answer to and which AC2(d) tests. Disposition: fix now (one
  clause).
- F8 (`[O]` 6) `as.integer(status)` at `R/loudnorm_two_pass.R:228` is
  unguarded, so a non-coercible `status` attribute would land `NA` in
  `tm_row_status` and be indistinguishable from the documented "exited zero"
  meaning. Latent: `run_program()` returns an integer `status` today, verified.
  Disposition: reject — a pre-existing property of `run_program()`'s contract
  the diff did not introduce.
- F9 (`[O]` 7) AC4's grid runs against a mocked Phase 1, so nothing in the
  suite ties `assemble_measured()`'s expected input shape to what
  `run_program()` actually returns. The `[O]` lens checked it by hand against
  real binaries (a corrupt input gave `tm_rows = 1L`, `tm_row_status = 183L`),
  so this is a coverage gap, not a defect. Disposition: follow-up candidate row.
- F10 (`[O]` 10) `bad` keeps `which()`'s names in the message while `tm_rows`
  is `unname()`d, so a named `outputs` would make the two disagree.
  Theoretical: `purrr::pmap()` over unnamed vectors returns an unnamed list.
  Disposition: reject — unreachable on any current call path, and the message
  side is pre-existing.

### Fix-now work at the gate

F1, F2 and F7 were fixed on the branch before the approval marker:

- `R/ffmpeg.R:781-782` now names all three raisers of `tidymedia_ffmpeg_exit`.
- `tests/testthat/test-ffmpeg-exit-condition.R:85` now says what its two probes
  are — a timeout and a hand-built bare `tidymedia_multitrack_separation` — and
  why the second is hand-built rather than the shipped diagnostic.
- `NEWS.md:38` now names both non-signalling paths. The sentence deliberately
  reads "Two paths still do not signal it" rather than the "Two paths
  deliberately do not signal it" wording the AC5 guard forbids: that guard
  exists to catch the stale M085 claim naming `separate_audio_video()`, and it
  stays discriminating.
- `R/ffmpeg.R:880-883` now names `tidymedia_timeout` as the class a reached
  limit answers to, instead of leaving it as "neither class".

Re-verified after the fixes: `devtools::document()` rewrote
`man/separate_audio_video.Rd` and nothing else; the three files still run
101 / 78 / 75 passes with 0 skips; full suite
`[ FAIL 0 | WARN 12 | SKIP 5 | PASS 8334 ]`; `devtools::check()` `Status: OK`,
0 errors / 0 warnings / 0 notes, 2m 34.5s.

Follow-ups (F5, F6, F9, and F3/F4 if CI does not settle them) are routed at the
post-merge hygiene pass. No new ROADMAP row is proposed: `cairn/ROADMAP.md`
stands at 23,990 bytes against its 24,000 cap, and search-first puts each of
these inside an existing row — F5 in the unclassed-aborts row, F6 in the
batch-warning row, and the test-instrument findings in
`cairn/references/instrument-findings.md` under its grouped row.

### Cross-build evidence on F3 and F4

The implementation commit d095a1d passed the full CI matrix. The Ubuntu
runners install FFmpeg from apt, so `R-CMD-check` there ran the new tests
against **ffmpeg 6.1.1-3ubuntu5**, three major versions below the local
9.0.1: `Status: OK`, `[ FAIL 0 | WARN 12 | SKIP 9 | PASS 8312 ]`. The AC1
loop's `expect_gt(length(unique(statuses)), 1L)` is not among the skips —
that test skips only when ffprobe is absent, and it is installed — so the
status-variation assertion F3 called build-dependent holds on both builds
measured. F3 is therefore not a live defect; it remains a latent dependency
on FFmpeg's exit numbering. F4's scenario needs FFmpeg configured off-PATH
via `set_ffmpeg()`, which CI does not exercise, so it stays latent too. Both
route to `cairn/references/instrument-findings.md` at hygiene rather than
blocking the merge.

### Gate disposition

No finding demonstrates an acceptance criterion failing, and none is a
load-bearing defect in shipped behavior: the two user-facing documentation
defects (F1, F2) plus F7 were fixed on the branch before the approval marker.
The return floor is therefore not met and the milestone does not return to
`in-progress`. This is the milestone's first review; no defect returns are on
its record, and the one amendment return it carries (AC2, logged 2026-08-29)
is on the separate amendment track.
