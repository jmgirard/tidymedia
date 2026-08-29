# M085: A failed FFmpeg run is a condition you can catch

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** — (RR04 is advisory; no binding criteria requested)
- **Principles touched:** —
- **Branch/PR:** `m085-ffmpeg-error-condition` / https://github.com/jmgirard/tidymedia/pull/89

## Goal

A non-zero FFmpeg exit raises a classed condition carrying the exit status as a
field, so a caller can catch a failed run programmatically and the package reads
the status from that field instead of scanning the abort's formatted message.

## Scope

Surface tier: **user-facing** — the deliverable is a condition class and field
callers write `tryCatch()` against.

**In:** `ffm_run()`'s non-zero-exit abort (`R/ffm.R:1588-1595`); the `loudnorm`
analysis pass's abort (`R/loudnorm_two_pass.R:143-150`); `ffmpeg_exit_status()`
(`R/ffmpeg.R:779-784`) and the prose at `R/ffmpeg.R:650-660` and `776-778` that
describes the retired parse; `ffm_run()`'s roxygen; the condition-handling
paragraph on the package-level help page (`R/tidymedia-package.R:62-68`), one
sentence naming the new class alongside the timeout family already discussed
there; NEWS.

**Out:**
- Reporting the exit number on `normalize_audio_batch(two_pass = TRUE)`'s
  per-row failures, which today records `list(status = "error")` and discards it
  (`R/loudnorm_two_pass.R:219`) → candidate row.
- Classing the package's other unclassed aborts → candidate row.
- Changing `ffm_batch()`'s `success = FALSE` contract (D007) → not planned.
- Whether `separate_audio_video()` should attempt the video once the audio
  command failed → the standing candidate row (M45 Out), unchanged.

## Acceptance criteria

- [x] AC1: A non-zero FFmpeg exit from `ffm_run()` raises a condition whose class
      vector is exactly `c("tidymedia_ffmpeg_exit", "rlang_error", "error",
      "condition")` — no parent or sibling class of any prefix (RB tripwire:
      irreversible-api; name settled by RR04) — carrying field `tm_status`. A
      test runs a command FFmpeg refuses, catches it with
      `tryCatch(tidymedia_ffmpeg_exit = )` alone, and asserts: `class(cnd)` is
      `identical()` to that vector; `is.integer(cnd$tm_status)`,
      `length(cnd$tm_status) == 1L`, and the value is non-zero; and that the
      value is `identical()` to the `"status"` attribute of
      `system2(find_ffmpeg(), shQuote(args, type = <run_program()'s type>),
      stdout = TRUE, stderr = "", input = "", timeout = 0)` — the same form
      `run_program()` uses (`R/program_management.R:117-127`) — that oracle call
      writing to a fresh `tempfile()`, so the first run's leftovers cannot change
      the status it returns.
- [x] AC2: The `loudnorm` analysis pass's non-zero-exit abort carries the same
      class and the same `tm_status` field, its message text unchanged. A test
      asserts both on a failing analysis pass, catching by class alone.
- [x] AC3: `ffmpeg_exit_status()`'s only inputs are `inherits(cnd,
      "tidymedia_ffmpeg_exit")` and `cnd$tm_status`; it reads no other property
      of the condition and passes `cnd` to no function other than `inherits()`.
      Verified by reading the function, whose body is the whole domain of this
      claim.
- [x] AC4: `ffmpeg_exit_status()` returns the status for a
      `tidymedia_ffmpeg_exit` constructed directly with no message at all, and
      `NA_integer_` for: a `tidymedia_ffmpeg_exit` carrying no `tm_status`; the
      condition an actual `run_program()` call raises with an unresolvable binary
      (caught from that call, not asserted by class, since that abort carries
      none); a `tidymedia_timeout`; a `tidymedia_multitrack_separation`; and a
      bare `simpleError` whose message contains "exited with status 3". The
      fourth is an intended change — that condition's own message carries the
      phrase, so the helper returns a status for it today — and is unobservable
      outside the package, the helper being unexported and called only from
      `run_separation_audio()`.
- [x] AC5: The multi-track enrichment in `run_separation_audio()` still fires on
      a failed audio command over a multi-track input naming no track, and still
      fails open — re-raising the original condition with its message, class and
      trace unchanged — on a single-track input and on a missing-binary failure.
- [x] AC6: No shipped prose asserts that the exit status is obtained by parsing a
      message, except the NEWS entry's description of the change itself.
      `ffm_run()`'s roxygen names `tidymedia_ffmpeg_exit` and `tm_status`, states
      that the `loudnorm` analysis pass behind `normalize_audio(two_pass = TRUE)`
      raises the same class, and says the status is whatever `system2()`
      reported, a signal-terminated FFmpeg included; the package-level help page
      names the class alongside the timeout family it already discusses; the NEWS
      entry names the class and the field; the comment inside
      `run_separation_audio()`'s failure handler and the comment above
      `ffmpeg_exit_status()` state what the code now does; and the
      wording-coupling test at
      `tests/testthat/test-separate-av-multitrack.R:132-146` is deleted, the
      coupling it pinned having ceased to exist. Verified by reading those six
      sites, plus a repo-wide search for `exited with status`,
      `ffmpeg_exit_status`, `exit status`, `parse`, `regexpr` and `regmatches`
      over `R/`, `man/`, `tests/`, `vignettes/`, `NEWS.md`, `README.Rmd` and
      `README.md`, confirming no further site describes the status as parsed.
- [x] AC7: `devtools::document()` leaves `man/` in sync; `devtools::check()`
      reports 0 errors and 0 warnings; `devtools::test()` passes.

## Coverage

- AC1 → T1, T4, T5
- AC2 → T2, T4, T5
- AC3 → T3
- AC4 → T3, T4, T5
- AC5 → T4
- AC6 → T3, T6
- AC7 → T6

## Tasks

- [x] T1: Add `class = "tidymedia_ffmpeg_exit"` and `tm_status =
      as.integer(status)` to the abort at `R/ffm.R:1588-1595`, following the
      field convention `R/timeout.R:372` sets (`tm_program`, `tm_limit`); message
      text unchanged. The coercion is a no-op today — the attribute `system2()`
      sets is already integer — and is kept so the field's contract is the
      coercion rather than the attribute plumbing (RR04 rec 3).
- [x] T2: Same at `R/loudnorm_two_pass.R:143-150`.
- [x] T3: Rewrite `ffmpeg_exit_status()` (`R/ffmpeg.R:779-784`) to read class and
      field only; restate the comment blocks at `R/ffmpeg.R:650-660` and
      `776-778`; delete the wording-coupling test at
      `tests/testthat/test-separate-av-multitrack.R:132-146`.
- [x] T4: Tests for AC1, AC2, AC4 and AC5; run the existing multi-track suite.
- [x] T5: Planted-defect run, one probe per axis the condition is free in:
      (a) drop `tm_status` from the T1 abort; (b) drop its class, keeping the
      field; (c) store the status as `as.character(status)`; (d) drop the class
      from the T2 abort; (e) drop `ffmpeg_exit_status()`'s field-absent guard,
      the axis AC4 pins that no probe on an abort site reaches (added on
      measurement — see the work log). Confirm (a)-(c) redden the AC1 test, (d)
      the AC2 test and (e) the AC4 test. Restore; record all five outcomes in
      one work-log line.
- [x] T6: `ffm_run()` roxygen; the package-level help-page sentence; NEWS entry;
      `devtools::document()`;
      `devtools::check()` and `devtools::test()`.

## Work log

- 2026-08-29: created by /milestone-plan.
- 2026-08-29: plan gate chose a classed condition with a `tm_status` field over keeping the message parse and merely hardening its regex, because a parse cannot tell `ffm_run()`'s abort from the two other sites composing the same sentence and gives callers nothing to catch; falsified by a caller needing the status from a condition this milestone leaves unclassed.
- 2026-08-29: plan gate chose to include the `loudnorm` analysis abort over scoping to `ffm_run()` alone, because two aborts reading identically to a reader and differently to a caller is the divergence this repo has fixed twice; falsified by the second site needing a distinct class.
- 2026-08-29: criteria audit ran in FULL mode (declared tier user-facing) in a fresh-context [O] reader that authored none of the criteria. Returned seven findings, all fixed at the gate, none re-gated: a call-name blacklist that could not enumerate "parses text" (AC3, restated positively); a missing-binary case asserted by a class that abort does not carry (AC4); an unmarked live return-value change on `tidymedia_multitrack_separation` (AC4, named as intended); the dead wording-coupling test and two now-false comment blocks left unretired (new AC6); a NEWS provenance clause binding an authoring act rather than the deliverable (dropped); no criterion putting the class in the documentation despite the user-facing tier (AC6); and an AC1 oracle whose `system2()` form was unnamed and would not have run the same command (pinned).
- 2026-08-29: implementation gate chose a single class over a second, broader parent class shared with the package's other aborts, because the other aborts stay unclassed after this milestone and a parent they do not answer to promises a handler that would not fire; falsified by a later milestone classing those aborts and wanting one handler over all of them.
- 2026-08-29: implementation gate escalated the class name to /milestone-brief at the user's selection (AC1's irreversible-api tripwire); the name a caller writes into tryCatch() cannot be changed once shipped.
- 2026-08-29: blocked on RB04 (`cairn/reviews/RB04-ffmpeg-error-class-name.md`), six questions on the class name, the class-vector shape, the `tm_status` field name and type, the second abort site, the documentation minimum, and what the design forecloses; advisory, no binding criteria requested.
- 2026-08-29: ingested RR04. Triage of its nine recommendations: applied 1 (class name `tidymedia_ffmpeg_exit`), 2 (one flat class), 3 (`tm_status`, `as.integer()` at both sites, no positivity check, one sentence on signal-terminated statuses), 4 (same class on the `loudnorm` abort), 5 (roxygen + NEWS) and 6 (the package-level help-page sentence, promoted from consider at the gate); applied the second half of 7 (the wording-coupling test is replaced by a class/field test, not merely deleted) and rejected its first half — collapsing `run_separation_audio()` to catch by class would retire the helper AC3 and AC4 quantify over, so the helper is deliberately retained; applied the first half of 8 as D062 and left its CRAN-release naming sweep undecided there; recorded 9's two rejections in D062's own reasoning.
- 2026-08-29: amendment gate adopted RR04's class name over the plan's `tidymedia_ffmpeg_error`, amending AC1, AC3, AC4 and AC6; widened AC6 to require the docs to name the `loudnorm` path and say what the status means when FFmpeg is signal-terminated; and added `R/tidymedia-package.R`'s condition-handling paragraph to Scope In. Coverage gains `AC2 → T5`; T1, T2, T5 and T6 took minor edits.
- 2026-08-29: criteria audit ran in FULL mode (declared tier user-facing) over the amended wording, twice, each in a fresh-context [O] reader that did not author what it read. The first returned eight findings: T1 still naming the rejected class; AC1 constraining the class present rather than the class vector; AC6 omitting the `loudnorm` documentation clause; AC1's oracle testing value but not type; AC6's universal quantified over all shipped prose but enumerated four sites; AC3's domain being reads-of-the-condition, not lines-of-the-function; four line-number anchors T3's own edits invalidate; and an undefined field-absent case. All eight were fixed. The fixed wording re-entered with a second fresh reader, which returned seven more: T5's type probe was dead, because `system2()`'s `"status"` attribute is already integer (measured this session), so `as.integer()` could not be falsified; AC3 forbade the `inherits()` call it required; AC6 forbade the NEWS sentence describing the change and missed prose using neither search term (`R/ffmpeg.R:850`, shipped as `man/separate_audio_video.Rd`); AC1's oracle re-ran FFmpeg without pinning a fresh output; "no other `tidymedia_*` class" permitted an unprefixed parent; T5 varied form but not site; and the classed-but-fieldless return was unpinned. All seven were fixed at the user's selection; no criterion took a third pass.
- 2026-08-29: RB04/RR04 archived; status back to in-progress.
- 2026-08-29: T1 — `ffm_run()`'s non-zero-exit `cli_abort()` now passes `class = "tidymedia_ffmpeg_exit"` and `tm_status = as.integer(status)`; the message vector is byte-identical, only its indentation changed with the reformat. `devtools::test()`: 0 failures, 8223 passing, 5 skips.
- 2026-08-29: T2 — the `loudnorm` analysis pass's abort now passes the same `class` and `tm_status` arguments; its rendered message is unchanged (both lines re-rendered under the new indentation and compared against the text above). `devtools::test()`: 0 failures, 8223 passing.
- 2026-08-29: T3 — `ffmpeg_exit_status()`'s body is now `inherits(cnd, "tidymedia_ffmpeg_exit")`, a read of `cnd$tm_status`, and a `NULL` guard; no regex, no `conditionMessage()`. Both comment blocks (`R/ffmpeg.R:649-654` and the block above the helper) restate the class-and-field read, and the wording-coupling test in `tests/testthat/test-separate-av-multitrack.R` is deleted. `devtools::test()`: 0 failures, 8221 passing (2 fewer expectations, the deleted test's).
- 2026-08-29: T4 — `tests/testthat/test-ffmpeg-exit-condition.R` adds four tests: AC1 (class vector `identical()`, integer scalar non-zero `tm_status`, and the value `identical()` to the `"status"` attribute of the same command re-spawned `run_program()`-style against a fresh `tempfile()`); AC2 (the `loudnorm` pass on an undemuxable input, caught by class alone, message text pinned `fixed = TRUE`); AC4 (all six cases, the missing-binary one caught from a real `run_program(NULL, ...)`); and AC5's missing-binary fall-open with `find_ffmpeg` mocked to `NULL`. 19 passing in the new file; full `devtools::test()` 0 failures, 8240 passing, 5 skips, the existing multi-track suite included.
- 2026-08-29: T5 — five planted defects, each applied alone and reverted, the tree restored clean between runs. (a) `tm_status` dropped from the `ffm_run()` abort: 5 failures — 3 in the AC1 test, 2 in the multi-track enrichment suite. (b) the class dropped, field kept: 8 failures — the AC1 test errors at its own `tryCatch()` (nothing catches it), and the enrichment falls open on every site that reads a status. (c) the status stored as `as.character(status)`: 2 failures, the AC1 type assertion and the oracle comparison. (d) the class dropped from the `loudnorm` abort: 1 error, the AC2 test's `tryCatch()`. (e) `ffmpeg_exit_status()`'s `is.null()` guard dropped: 1 failure, AC4's classed-but-fieldless case. Probe (e) was added after measuring that (a)-(c) do not reach the AC4 test, whose conditions are all constructed directly or caught from `run_program()` and so are unaffected by the `ffm_run()` abort — the plan's expectation that they would redden AC4 is false, and what they redden besides AC1 is the AC5 enrichment suite, which reads the status off a real `ffm_run()` condition. Minor task amendment: T5 gains probe (e), which keeps `AC4 → T5` true without a Coverage amendment.
- 2026-08-29: T6 — `ffm_run()` gains a `When FFmpeg exits non-zero` roxygen section naming the class and the field, showing the `tryCatch()` form, stating that the status is `system2()`'s and that a signal-terminated FFmpeg encodes the signal there, and naming the `loudnorm` path; `?tidymedia`'s timeout section gains a closing paragraph distinguishing a refused run from a killed one and naming the class and field; NEWS gains a New features entry. `devtools::document()` rewrote `man/ffm_run.Rd` and `man/tidymedia-package.Rd` and a second run produced no further diff. `devtools::check()`: 0 errors, 0 warnings, 0 notes. `devtools::test()`: 0 failures, 8240 passing, 5 skips. AC6's repo-wide sweep over `R/`, `man/`, `tests/`, `vignettes/`, `NEWS.md` and both READMEs for `exited with status`, `ffmpeg_exit_status`, `exit status`, `parse`, `regexpr` and `regmatches` leaves no site describing the status as parsed; the surviving `regexpr`/`regmatches` hits are the codec-table and `volumedetect` readers.
- 2026-08-29: all tasks complete; status → review.

## Decisions

- 2026-08-29 (M085-D1, from RR04 Q1/Q6): the catchable class is
  `tidymedia_ffmpeg_exit`. Promoted to D062, which states the convention it
  instances — a condition class names the event, never the severity, and its
  data fields carry the `tm_` prefix.
- 2026-08-29 (M085-D2, from RR04 Q2): one flat class, no parent. A
  `tidymedia_ffmpeg_failure` parent would have exactly one member at ship time,
  and appending a parent class later breaks no handler written against the
  child, so the hierarchy waits for a second member and a caller need. Falsified
  by a caller wanting one handler over the package's FFmpeg failure modes before
  those modes are classed.
- 2026-08-29 (M085-D3, from RR04 Q4): both abort sites carry the same class.
  The `loudnorm` analysis pass reports the identical fact and differs only in
  prose, in `call`, and in which command it prints; a caller's reason for
  catching applies to both, and splitting them would make every such caller
  enumerate two names forever. A narrower class can be prepended at that site
  later without breaking anything. Falsified by a caller needing to dispatch on
  the analysis pass specifically.
- 2026-08-29 (M085-D4, from RR04 rec 7, rejected): `ffmpeg_exit_status()` is
  retained rather than collapsed into a `tryCatch(tidymedia_ffmpeg_exit = )` in
  `run_separation_audio()`. The collapse would make the fail-open behavior
  structural rather than a branch, which RR04 marks as its advantage, but AC3
  and AC4 quantify over a helper that continues to exist, and retiring it is a
  larger change than this milestone promised. Falsified by the helper acquiring
  a second caller, at which point the branch is the thing being duplicated.

## Review

Reviewed 2026-08-29 on `m085-ffmpeg-error-condition` at `a0988c2`, PR #89.
`master` had not moved since the branch was cut, so no merge was needed and the
evidence below is from the branch tip.

### Acceptance-criterion evidence

- **AC1** — `tests/testthat/test-ffmpeg-exit-condition.R`, "a non-zero exit from
  `ffm_run()` is catchable by class alone", run fresh this session: caught with
  `tryCatch(tidymedia_ffmpeg_exit = )` alone; `identical(class(cnd), c(...))`
  against the exact four-element vector; `tm_status` integer, length one,
  non-zero; and `identical()` to the `"status"` attribute of the same command
  respawned `run_program()`-style (`shQuote(..., type = "sh")`, `stdout = TRUE`,
  `stderr = ""`, `input = ""`, `timeout = 0`) writing to a fresh `tempfile()`.
  0 failures.
- **AC2** — same file, "the `loudnorm` analysis pass raises the same class and
  field": caught by class alone off an undemuxable input; integer length-one
  non-zero `tm_status`; message text pinned `fixed = TRUE`. Class vector measured
  directly this session as `tidymedia_ffmpeg_exit / rlang_error / error /
  condition`. 0 failures.
- **AC3** — `R/ffmpeg.R:781-786` read whole. The body is
  `inherits(cnd, "tidymedia_ffmpeg_exit")`, `status <- cnd$tm_status`, an
  `is.null(status)` guard, and `status`. No other property of `cnd` is read and
  `cnd` is passed to no function but `inherits()`; no `conditionMessage()`, no
  regex.
- **AC4** — same file, "`ffmpeg_exit_status()` reads the class and the field,
  nothing else": all six cases pass — the message-less constructed condition
  returns `3L`; the classed-but-fieldless one, a real `run_program(NULL, ...)`
  missing-binary abort caught from the call, a `tidymedia_timeout`, a
  `tidymedia_multitrack_separation` whose own message carries the phrase, and a
  bare `simpleError` carrying it all return `NA_integer_`.
- **AC5** — `tests/testthat/test-separate-av-multitrack.R` plus the new
  missing-binary fall-open test, run together fresh: 94 passing, 0 failures,
  0 skips. The enrichment still fires on a failed audio command over a
  multi-track input naming no track, and re-raises the original condition
  untouched on a single-track input and with `find_ffmpeg` mocked to `NULL`.
- **AC6** — the six sites read: `ffm_run()`'s "When FFmpeg exits non-zero"
  roxygen (names the class and field, shows the `tryCatch()` form, names the
  `loudnorm` path, states the status is `system2()`'s including a
  signal-terminated FFmpeg); `?tidymedia`'s closing timeout paragraph; the NEWS
  entry; the comment in `run_separation_audio()`'s handler; the comment above
  `ffmpeg_exit_status()`; and the deleted wording-coupling test. Repo-wide sweep
  re-run this session for `exited with status`, `ffmpeg_exit_status`,
  `exit status`, `parse`, `regexpr` and `regmatches` over `R/`, `man/`, `tests/`,
  `vignettes/`, `NEWS.md`, `README.Rmd` and `README.md`: no site describes the
  exit status as parsed. The surviving `regexpr`/`regmatches` hits are the
  codec-table readers, the ffprobe key parser, the `-version` token reader and
  `volumedetect`; the surviving "parse" prose is the `loudnorm` measurement
  block. Only the NEWS entry mentions the retired parse, describing the change.
- **AC7** — `devtools::document()` rewrote nothing (working tree clean but for
  this milestone file); `devtools::check()`: 0 errors, 0 warnings, 0 notes;
  `devtools::test()`: 0 failures, 8240 passing, 5 skips. Re-run after the
  fix-now work below: `document()` rewrote `man/ffm_run.Rd` for the corrected
  roxygen and a second run produced no further diff; `check()` 0 errors,
  0 warnings, 0 notes; `test()` 0 failures, 8241 passing, 5 skips.

No Driving RR is declared (RR04 was advisory, no binding criteria requested), so
the projection-vs-outcome record is empty.

### Consistency gate

`cairn_validate.py`: 16 checks PASS, 7 advisories OK — the `release window`
advisory did not fire. No DESIGN principle changed, so `cairn_impact.py` was not
run. Toolchain checks from the `r-package` profile's `consistency-gate` slot:
`document()` produces no diff; `NAMESPACE`/`man/` regenerate cleanly;
`README.Rmd` untouched by the branch and in sync; `pkgdown::check_pkgdown()`
reports no problems; `NEWS.md` carries an entry for the user-visible change with
no milestone number in it; no new top-level files; `check()` clean.

### Independent review

Three fresh-context reviewers, none having seen the implementation, each on a
distinct evidence base. The blame-history [S] lens reported no defects: the
deleted wording-coupling test is replaced rather than lost, the two abort sites
are purely additive to message text and to D046's cleanup wiring, and D062 does
not contradict a prior entry. The prior-review [S] lens found no regression —
the GitHub inline-comment probe returned empty, and the archived reviews on the
touched files (M68's cleanup contract, M44's brace trap, M46/M69's ban on
classifying failures by matching text) are resolved rather than reintroduced by
retiring the regex parse. The diff-bug [O] lens returned nine ranked findings.


### Findings and disposition

Every finding the [O] lens reported is listed, ranked as it ranked them, with
the disposition taken. Each was verified against the implementation, not against
the reviewer's account of it.

- **F1 — the NEWS entry overclaims the class's reach.** It says `ffm_run()`
  "and every task verb that runs through it" aborts with `tidymedia_ffmpeg_exit`.
  False on two live paths: `run_separation_audio()` (`R/ffmpeg.R:656-679`)
  catches the exit condition and re-signals `tidymedia_multitrack_separation`
  with the original as `parent`, so a `tryCatch(tidymedia_ffmpeg_exit = )` around
  `separate_audio_video()` on a multi-track input naming no track does not fire;
  and the `*_batch()` verbs record `success = FALSE` per D007 rather than
  propagating. Verified by reading both sites. **Fixed** — the sentence now names
  what actually raises the class and what does not.
- **F2 — `ffmpeg_exit_status()`'s class guard is unfalsifiable by the suite.**
  Every AC4 case that expects `NA_integer_` also has a `NULL` `tm_status`, so
  deleting the `inherits()` line leaves all of them green via the `is.null()`
  guard. Measured this session: `rlang::error_cnd("tidymedia_timeout",
  tm_status = 3L)` returns `NA_integer_` today and would return `3L` without the
  guard. **Fixed** — that condition is added to the AC4 test as a discriminating
  case.
- **F3 — the helper no longer guarantees a length-one integer return.** It
  returns `cnd$tm_status` verbatim, and its only caller interpolates it into a
  message after an `is.na()` test. **Rejected**: both in-package construction
  sites coerce with `as.integer()`, the helper is unexported, and no path
  reaches it with a malformed field. A speculative contract about conditions
  nothing constructs.
- **F4 — `separate_audio_video()`'s "When the audio output fails" Rd section
  was not updated.** A reader of the new promise is not told that this path's
  condition is `tidymedia_multitrack_separation`. `R/ffmpeg.R:846-856` is not in
  this milestone's Scope In. **Follow-up** — absorbed into the standing M45
  review F1/F5 candidate row, which already covers this verb's failure path.
- **F5 — the `loudnorm` site's class vector exactness is unpinned.** AC2's test
  uses `expect_s3_class()`, so a prepended parent there would redden nothing,
  where the same change at `ffm_run()` is caught. Measured this session: the
  vector is already exactly the four M085-D2 requires. **Fixed** — the AC2 test
  now pins it with `identical()`, so M085-D2 is enforced at both sites.
- **F6 — the signal-termination wording attributes the encoding to the wrong
  layer.** `R/ffm.R` said "the values R uses for a signal-terminated FFmpeg";
  the 128-plus-signal encoding is the shell's, which `system2()` passes through.
  **Fixed** at both the roxygen and NEWS sites.
- **F7 — the AC1 oracle hardcodes `timeout = 0` where `run_program()` passes
  `resolve_timeout()`.** They agree today (`resolve_timeout()` returns 0 by
  default, measured this session) and diverge only under a set
  `tidymedia.timeout`. AC1 specified `timeout = 0`, so the test is faithful to
  the criterion. **Fixed** in the test rather than the criterion — the test now
  pins the option to 0 for its duration, making the specified form the same form
  `run_program()` uses in every session.
- **F8 — `cnd$tm_status` partial-matches.** Confirmed: a condition carrying only
  `tm_status_source` returns that value. **Rejected**: `$` on condition fields is
  this package's idiom (`cnd$tm_program`, `cnd$tm_limit` at the timeout site) and
  is the exact expression AC3 names; `[[` would also pass `cnd` to a second
  function, which AC3 forbids.
- **F9 — no test composes a real `ffm_run()` condition with the helper.** The
  integration is covered transitively by the multi-track enrichment suite, which
  cannot enrich unless the helper returns a non-NA status off a real condition —
  which is what T5's probes (a) and (b) measured. **Noted, no action**; the
  reviewer raised it to name the coupling, not to report a gap.

The five fixes were verified after the fact: F2's new expectation was confirmed
discriminating by dropping `ffmpeg_exit_status()`'s class guard, which reddens
the file, and restoring it; F1's two claims were confirmed by execution before
the prose was written — a `tryCatch(tidymedia_ffmpeg_exit = )` around
`separate_audio_video()` on a three-track input written to `.mp3` does not fire
and the condition is `tidymedia_multitrack_separation` with the exit condition as
its parent, and `ffm_batch()` on a refused row returns a tibble with
`success = FALSE` and signals nothing. F4's candidate-row extension is left to
the post-merge hygiene pass, where the row's disposition and the ROADMAP's byte
budget are judged together.

No finding demonstrated an acceptance criterion failing, and none showed a
criterion itself to be wrong, so the return floor was not reached and no
amendment return was convened.
