# M085: A failed FFmpeg run is a condition you can catch

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** — (RR04 is advisory; no binding criteria requested)
- **Principles touched:** —
- **Branch/PR:** `m085-ffmpeg-error-condition`

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

- [ ] AC1: A non-zero FFmpeg exit from `ffm_run()` raises a condition whose class
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
- [ ] AC2: The `loudnorm` analysis pass's non-zero-exit abort carries the same
      class and the same `tm_status` field, its message text unchanged. A test
      asserts both on a failing analysis pass, catching by class alone.
- [ ] AC3: `ffmpeg_exit_status()`'s only inputs are `inherits(cnd,
      "tidymedia_ffmpeg_exit")` and `cnd$tm_status`; it reads no other property
      of the condition and passes `cnd` to no function other than `inherits()`.
      Verified by reading the function, whose body is the whole domain of this
      claim.
- [ ] AC4: `ffmpeg_exit_status()` returns the status for a
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
- [ ] AC5: The multi-track enrichment in `run_separation_audio()` still fires on
      a failed audio command over a multi-track input naming no track, and still
      fails open — re-raising the original condition with its message, class and
      trace unchanged — on a single-track input and on a missing-binary failure.
- [ ] AC6: No shipped prose asserts that the exit status is obtained by parsing a
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
- [ ] AC7: `devtools::document()` leaves `man/` in sync; `devtools::check()`
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
