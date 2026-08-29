# M085: A failed FFmpeg run is a condition you can catch

- **Status:** blocked
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
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
describes the retired parse; `ffm_run()`'s roxygen; NEWS.

**Out:**
- Reporting the exit number on `normalize_audio_batch(two_pass = TRUE)`'s
  per-row failures, which today records `list(status = "error")` and discards it
  (`R/loudnorm_two_pass.R:219`) → candidate row.
- Classing the package's other unclassed aborts → candidate row.
- Changing `ffm_batch()`'s `success = FALSE` contract (D007) → not planned.
- Whether `separate_audio_video()` should attempt the video once the audio
  command failed → the standing candidate row (M45 Out), unchanged.

## Acceptance criteria

- [ ] AC1: A non-zero FFmpeg exit from `ffm_run()` raises a condition of class
      `tidymedia_ffmpeg_error` (RB tripwire: irreversible-api) carrying field
      `tm_status`, an integer equal to the status FFmpeg returned. A test runs a
      command FFmpeg refuses, catches it with `tryCatch(tidymedia_ffmpeg_error
      = )` alone, and asserts `cnd$tm_status` equals the status returned by
      `system2(find_ffmpeg(), shQuote(args, type = <run_program()'s type>),
      stdout = TRUE, stderr = "", input = "")` read off that call's own
      `"status"` attribute — the same form `run_program()` uses
      (`R/program_management.R:117-127`).
- [ ] AC2: The `loudnorm` analysis pass's non-zero-exit abort carries the same
      class and the same `tm_status` field, its message text unchanged. A test
      asserts both on a failing analysis pass, catching by class alone.
- [ ] AC3: `ffmpeg_exit_status()`'s only inputs are `inherits(cnd,
      "tidymedia_ffmpeg_error")` and `cnd$tm_status`; it reads no other property
      of the condition. Verified by reading the function, whose body is the
      whole domain of this claim.
- [ ] AC4: `ffmpeg_exit_status()` returns the status for a
      `tidymedia_ffmpeg_error` constructed directly with no message at all, and
      `NA_integer_` for: the condition an actual `run_program()` call raises with
      an unresolvable binary (caught from that call, not asserted by class,
      since that abort carries none); a `tidymedia_timeout`; a
      `tidymedia_multitrack_separation`; and a bare `simpleError` whose message
      contains "exited with status 3". The third is an intended change — that
      condition's own message carries the phrase, so the helper returns a status
      for it today — and is unobservable outside the package, the helper being
      unexported and called only at `R/ffmpeg.R:656`.
- [ ] AC5: The multi-track enrichment in `run_separation_audio()` still fires on
      a failed audio command over a multi-track input naming no track, and still
      fails open — re-raising the original condition with its message, class and
      trace unchanged — on a single-track input and on a missing-binary failure.
- [ ] AC6: No shipped prose describes the retired parse: `ffm_run()`'s roxygen
      names `tidymedia_ffmpeg_error` and `tm_status`; the NEWS entry names both;
      the comment blocks at `R/ffmpeg.R:650-660` and `776-778` state what the
      code now does; and the wording-coupling test at
      `tests/testthat/test-separate-av-multitrack.R:132-146` is deleted, the
      coupling it pinned having ceased to exist.
- [ ] AC7: `devtools::document()` leaves `man/` in sync; `devtools::check()`
      reports 0 errors and 0 warnings; `devtools::test()` passes.

## Coverage

- AC1 → T1, T4, T5
- AC2 → T2, T4
- AC3 → T3
- AC4 → T3, T4, T5
- AC5 → T4
- AC6 → T3, T6
- AC7 → T6

## Tasks

- [ ] T1: Add `class = "tidymedia_ffmpeg_error"` and `tm_status = status` to the
      abort at `R/ffm.R:1588-1595`, following the field convention
      `R/timeout.R:372` sets (`tm_program`, `tm_limit`); message text unchanged.
- [ ] T2: Same at `R/loudnorm_two_pass.R:143-150`.
- [ ] T3: Rewrite `ffmpeg_exit_status()` (`R/ffmpeg.R:779-784`) to read class and
      field only; restate the comment blocks at `R/ffmpeg.R:650-660` and
      `776-778`; delete the wording-coupling test at
      `tests/testthat/test-separate-av-multitrack.R:132-146`.
- [ ] T4: Tests for AC1, AC2, AC4 and AC5; run the existing multi-track suite.
- [ ] T5: Planted-defect run: drop `tm_status` from the T1 abort and confirm the
      AC1 and AC4 tests redden; then drop the class instead, keeping the field,
      and confirm they redden again — the two axes the condition is free in.
      Restore; record both outcomes in one work-log line.
- [ ] T6: `ffm_run()` roxygen; NEWS entry; `devtools::document()`;
      `devtools::check()` and `devtools::test()`.

## Work log

- 2026-08-29: created by /milestone-plan.
- 2026-08-29: plan gate chose a classed condition with a `tm_status` field over keeping the message parse and merely hardening its regex, because a parse cannot tell `ffm_run()`'s abort from the two other sites composing the same sentence and gives callers nothing to catch; falsified by a caller needing the status from a condition this milestone leaves unclassed.
- 2026-08-29: plan gate chose to include the `loudnorm` analysis abort over scoping to `ffm_run()` alone, because two aborts reading identically to a reader and differently to a caller is the divergence this repo has fixed twice; falsified by the second site needing a distinct class.
- 2026-08-29: criteria audit ran in FULL mode (declared tier user-facing) in a fresh-context [O] reader that authored none of the criteria. Returned seven findings, all fixed at the gate, none re-gated: a call-name blacklist that could not enumerate "parses text" (AC3, restated positively); a missing-binary case asserted by a class that abort does not carry (AC4); an unmarked live return-value change on `tidymedia_multitrack_separation` (AC4, named as intended); the dead wording-coupling test and two now-false comment blocks left unretired (new AC6); a NEWS provenance clause binding an authoring act rather than the deliverable (dropped); no criterion putting the class in the documentation despite the user-facing tier (AC6); and an AC1 oracle whose `system2()` form was unnamed and would not have run the same command (pinned).
- 2026-08-29: implementation gate chose a single class over a second, broader parent class shared with the package's other aborts, because the other aborts stay unclassed after this milestone and a parent they do not answer to promises a handler that would not fire; falsified by a later milestone classing those aborts and wanting one handler over all of them.
- 2026-08-29: implementation gate escalated the class name to /milestone-brief at the user's selection (AC1's irreversible-api tripwire); the name a caller writes into tryCatch() cannot be changed once shipped.
- 2026-08-29: blocked on RB04 (`cairn/reviews/RB04-ffmpeg-error-class-name.md`), six questions on the class name, the class-vector shape, the `tm_status` field name and type, the second abort site, the documentation minimum, and what the design forecloses; advisory, no binding criteria requested.

## Decisions

## Review
