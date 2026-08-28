<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M075: The silent narrowing announces itself

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m075-normalize-audio-drop-warning` / [#79](https://github.com/jmgirard/tidymedia/pull/79)

## Goal

Give `normalize_audio()` and `normalize_audio_batch()` the dropped-audio-track
warning the rest of the audio-producing family already carries, so a
multi-track input whose extra tracks the output discards says so.

## Scope

**Surface tier: user-facing** — the deliverable is a new runtime warning on two
exported verbs and the roxygen that documents it.

**In:** wire `warn_dropped_audio()` into `normalize_audio()` (two mutually
exclusive sites, so the two-pass path warns before its analysis pass runs) and
`warn_dropped_audio_batch()` into `normalize_audio_batch()` (above the
`two_pass` block, so it warns before Phase 1). Same builder, same class, no new
wording. Tests, roxygen, NEWS, and the D-entry recording what was rejected.

**Out:**
- Any signal for the discarded **video** — rejected at the plan gate and
  recorded as D054, not deferred. `extract_audio()`/`convert_audio()` discard
  video silently too, and D030 states the discard in `?normalize_audio`'s first
  sentence.
- An opt-out for the probe (`check_tracks =`, an option seam, lazy per-row) →
  stays on the existing ROADMAP candidate row, widened there from four verbs to
  six. Its open question is API shape, not this milestone's question.
- Normalizing audio while keeping the picture → the existing `normalize_audio()`
  extensions candidate row (part b).
- The copy/`audio_stream` form divergence → its own existing candidate row.

## Acceptance criteria

- [ ] AC1 — On a `run = TRUE` call naming no `audio_stream`,
      `normalize_audio()` signals exactly one condition of class
      `tidymedia_dropped_audio` for a multi-track input, whose text carries the
      track count, the number dropped, `audio_stream`, and `probe_audio`'s two
      readings. Evidence: `devtools::test(filter = "audio-track-drop")` output
      with a passing test that, on `make_multitrack_video()`, counts exactly one
      such condition at `two_pass = FALSE` and at `two_pass = TRUE`, and at both
      matches `3 audio tracks`, `drops 2`, `audio_stream`, `probe_audio`,
      `1, 2, 3` and `0, 1, 2` in that condition's message.
- [ ] AC2 — `normalize_audio_batch()` signals exactly one such condition per
      call, naming every affected row. Evidence: a passing test on a two-row
      jobs table of multi-track inputs whose message matches `Row 1` and
      `Row 2`.
- [ ] AC3 — The warning is silent in each of five cases: `audio_stream` given
      as an argument (scalar and batch), given as a batch cell on every row,
      `run = FALSE` (at both `two_pass` values), and a single-track input.
      Evidence: one passing `expect_no_warning()` test per case.
- [ ] AC4 — On the two-pass path both verbs warn *before* the analysis pass
      runs. Evidence: two passing tests that mock `run_loudnorm_analysis()` /
      the batch Phase 1 to `stop()`, catch the warning with
      `withCallingHandlers()`, and assert both that the warning arrived and
      that the mock's error propagated.
- [ ] AC5 — A wrong value still refuses before anything warns: on
      `make_multitrack_video()`, `normalize_audio(infile, out,
      target_loudness = 999)` and `normalize_audio(infile, out,
      audio_codec = "copy")` abort with the same message strings the existing
      guard tests assert and signal no `tidymedia_dropped_audio` condition, at
      both `two_pass` values; `normalize_audio_batch()` does the same for the
      `target_loudness` and `audio_codec` columns. Evidence: one passing test
      per case (eight).
- [ ] AC6 — `?normalize_audio` and `?normalize_audio_batch` each state that a
      multi-track input warns, that naming `audio_stream` silences it, and that
      the check costs one FFprobe call per distinct input — the batch form
      adding that its probes run serially before the fan-out, so `parallel`
      does not reach them. Evidence: the rendered `man/normalize_audio.Rd` and
      `man/normalize_audio_batch.Rd` excerpts.
- [ ] AC7 — `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::check()'` clean (0 errors, 0 warnings; NOTEs
      justified). Evidence: both outputs.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T4
- AC3 → T2, T3, T4
- AC4 → T2, T3, T4
- AC5 → T2, T3, T4
- AC6 → T5
- AC7 → T8

## Tasks

- [x] T1 — Pin the three call sites against the family: `warn_dropped_audio()`
      (`R/ffmpeg.R:366`), `warn_dropped_audio_batch()` (`R/ffmpeg.R:427`), and
      the five existing sites (`R/ffmpeg.R:544`, `1015`, `637`, `4981`,
      `5122`). Record the chosen lines in this file's Decisions section.
- [x] T2 — Tests first, extending `tests/testthat/test-audio-track-drop.R` so
      every wording assertion in the package stays in one file: AC1's message
      test, AC2's row-naming test, AC3's five silence cases, AC4's two mocked
      ordering tests, AC5's eight refusal-before-warning cases. Confirm red.
- [x] T3 — Wire `normalize_audio()`: two mutually exclusive calls, each gated
      `isTRUE(run) && is.null(audio_stream)` — one inside the `if (two_pass)`
      block below `check_token(audio_codec)` and above
      `run_loudnorm_analysis()` (`R/ffmpeg.R:2189-2192`), one on the
      single-pass path below `rlang::check_string(audio_codec,
      allow_null = TRUE)` (`R/ffmpeg.R:2207`). Comment why there are two.
- [x] T4 — Wire `normalize_audio_batch()`: one
      `if (isTRUE(run)) warn_dropped_audio_batch(jobs, audio_stream)` below the
      per-row loudness-target sweep and above the `if (two_pass)` block
      (`R/ffmpeg.R:4383`), so it precedes Phase 1.
- [x] T5 — Roxygen for both verbs per AC6; `devtools::document()`.
- [x] T6 — One `NEWS.md` bullet naming the two verbs, the warning, and the
      probe cost — claiming only what T2's tests measure (M074's lesson).
- [x] T7 — Append D054 to `cairn/DECISIONS.md` (the video discard stays
      silent: the rule, the two-verb parity reason, D030's existing sentence,
      what it rules out, and its falsifier — a report of a caller surprised by
      the lost picture). Absorb the dropped-track-parity candidate row and
      widen the probe-cost row from four verbs to six.
- [x] T8 — Gate: `devtools::document()`, `devtools::test()`,
      `devtools::check()`.
- [x] T9 — Round-1 return, tests first: extend AC1's test to loop over both
      `two_pass` values, counting the conditions at each. Confirm red at
      `two_pass = TRUE`.
- [x] T10 — Round-1 return, fix: gate the single-pass
      `check_audio_codec_not_copy()` + probe pair on `!two_pass` so the two
      sites are actually mutually exclusive, and correct the comments that
      claimed an exclusivity the fall-through did not give. Re-run the T8 gate.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: criteria audit ran self-authored, not by the mandated fresh-context [O] reader, because a session instruction forbade the Agent tool — disclosed rather than skipped. Two findings, both fixed before the criteria were written: a draft AC promising "every message a wrong-valued call produced before, it still produces" made a universal claim over a domain no named procedure enumerates (bounded-promise rule) and was narrowed to AC5's eight named cases; a draft AC1 binding "the same builder, so the wording cannot drift" bound a code-structure property rather than the deliverable's behavior (D-118) and was narrowed to the warning's observable text, the shared-builder requirement moving to T3/T4. A third draft criterion binding D054's authorship was moved out to T7 on the same instrument ground.
- 2026-08-27: plan gate chose no video signal over a second video-discard warning because the two verbs this parity restores (`extract_audio()`, `convert_audio()`) also discard video silently and D030 already states the discard in the verb's first sentence; falsified by a report of a caller surprised by the lost picture.
- 2026-08-27: plan gate chose two mutually exclusive call sites in `normalize_audio()` over one site below every guard because one site warns after the analysis pass on `two_pass = TRUE` while the batch form warns before Phase 1, a scalar/batch ordering divergence of the kind D039 exists to prevent; the third option, hoisting `check_string(audio_codec)`, was rejected because M41's own review (A3r3) backed that hoist out for changing which complaint a doubly-wrong two-pass call gets. Falsified by a maintainer finding the two sites drift apart in gating.
- 2026-08-27: plan gate chose accepting the added FFprobe spawn over designing an opt-out here because the opt-out's open question is API shape across six verbs and its lazy-per-row option reopens the `ffm_batch()` hook D024/RR02 Q3 rejected; falsified by a measured batch stall attributable to these two verbs' probes.
- 2026-08-27: T1 pinned the builder, its batch form and the five existing call sites; recorded in Decisions.
- 2026-08-27: question gate chose hoisting `check_audio_codec_not_copy()` onto the single-pass path over narrowing AC5 to `two_pass = TRUE`, because the single-pass path's only copy guard runs inside `ffm_finish()`'s argument, after the probe, so AC5's scalar `"copy"` case would otherwise warn before aborting; falsified by an existing `"copy"` guard test changing what it reads.
- 2026-08-27: T2 wrote the 17 new tests into `tests/testthat/test-audio-track-drop.R` and confirmed red: the four wiring tests (AC1, AC2, AC4's two) fail with 0 drop warnings collected; AC3's five silence cases and AC5's eight refusal cases pass already, standing as regression guards over the wiring T3/T4 add. T2 is checked off with the wiring, since its own verify run is red by design.
- 2026-08-27: T3/T4 wired the three sites; `devtools::test()` clean (exit 0), all 17 new tests green. T3 also hoisted `check_audio_codec_not_copy()` onto the single-pass path per the question gate. Two pre-existing tests in `test-parallel-option-carry.R` (lines 259, 533) now also emit the front-door probe's fail-open timeout warning alongside the `tidymedia_timeout` abort they assert; both still pass, and the extra warning is D024's documented fail-open, matching the noise the four existing verbs' tests already carry.
- 2026-08-27: T5 documented the warning on both verbs per AC6 and ran `devtools::document()`; `man/normalize_audio.Rd` and `man/normalize_audio_batch.Rd` are the only Rd files that changed.
- 2026-08-27: T6 added one `NEWS.md` bullet naming both verbs, the shared class and wording, the one-FFprobe-call-per-distinct-input cost, and the pre-analysis ordering — every clause of it measured by a T2 test (AC1's wording match, AC2's row naming, AC3's silence cases, AC4's ordering mocks, AC5's refusals).
- 2026-08-27: T7 appended D054 (the discarded video stays silent on all six audio-producing verbs) and widened the probe-cost/opt-out candidate row from four verbs to six. The dropped-track-parity row needed no absorbing: `/milestone-plan` had already replaced it with M075's own table row in commit 32280dc.
- 2026-08-27: T8 gate — `devtools::document()` produces no diff, and `devtools::check()` is `Status: OK` (0 errors, 0 warnings, 0 notes; full suite run inside it in 118s). Status set to review.
- 2026-08-27: plan gate chose a `stop()`ing mock of `run_loudnorm_analysis()` over dropping AC4 because the call site is not wrapped in `tryCatch(error =)`, the condition M44's lesson names as defeating such a mock; falsified by the mock passing with the wiring removed.
- 2026-08-27: review round 1 returned M075 to `in-progress` on an AC1 failure: `normalize_audio(infile, out, two_pass = TRUE)` on a multi-track input signals two `tidymedia_dropped_audio` conditions, not one, because the `if (two_pass)` block falls through to the single-pass probe site. AC2-AC5 verified green; AC6/AC7, the consistency gate and the review lenses were not reached.
- 2026-08-27: amendment (substantive) — AC1's Evidence clause now names both
  `two_pass` values, executing review round 1's direction. The promise sentence
  is unchanged and already bound every `run = TRUE` call, so the criteria set
  neither widens nor narrows (D-118). The mandated fresh-context [O] audit of
  the amended wording could not run: a session instruction forbids the Agent
  tool — disclosed rather than skipped, as at the plan gate.
- 2026-08-27: question gate chose gating the single-pass guard-and-probe pair on
  `!two_pass` over an early `return()` from the two-pass branch or a single
  probe site above the block, because the early return duplicates the shared
  `ffm_finish()` tail and the single site requires the
  `check_audio_codec_not_copy()` hoist A3r3 backed out; falsified by a caller
  needing the probe on a path this gate now skips.
- 2026-08-27: T9 extended AC1's test to both `two_pass` values and confirmed
  red: `two_pass = TRUE` collected 2 conditions, `two_pass = FALSE` 1.
- 2026-08-27: T10 gated the pair on `!two_pass`; `test_local(filter =
  "audio-track-drop")` green (35 tests, 106 assertions, 0 failures, 0 skips),
  up from 99 assertions on the added loop iteration.
- 2026-08-27: T8 gate re-run after the round-1 fix — `devtools::document()`
  produces no diff and `devtools::check()` is `Status: OK` (0 errors, 0 warnings,
  0 notes; full suite run inside it, 158s). Status set back to review.

## Decisions

- **T1 — the three call sites, pinned against the family.** The builder is
  `warn_dropped_audio()` (`R/ffmpeg.R:366`) and its batch form
  `warn_dropped_audio_batch()` (`R/ffmpeg.R:407`, calling the builder at `439`).
  The five existing sites are `extract_audio()` `R/ffmpeg.R:544`,
  `convert_audio()` `R/ffmpeg.R:1015`, `ffm_run()`'s fail-open re-raise
  `R/ffmpeg.R:637`, `extract_audio_batch()` `R/ffmpeg.R:4981`, and
  `convert_audio_batch()` `R/ffmpeg.R:5122`. The scalar sites gate on
  `isTRUE(run) && is.null(audio_stream)`; the batch sites gate on `isTRUE(run)`
  alone and leave the per-row `audio_stream` decision to the batch builder.
  M075's three new sites take the same two gates.
- **Question gate — the single-pass copy guard is hoisted.** AC5 asks a
  `"copy"` call to refuse before anything warns at both `two_pass` values.
  `two_pass = TRUE` already did (`check_audio_codec_not_copy()` at
  `R/ffmpeg.R:2185`); the single-pass path's only copy guard is inside
  `normalize_audio_pipeline()` (`R/ffmpeg.R:2259`), evaluated as an argument
  to `ffm_finish()` and therefore after the probe. The guard is hoisted to the
  front door on that path too, below `rlang::check_string(audio_codec)`.
  Rejected: narrowing AC5 to `two_pass = TRUE`, which would leave the scalar
  verb warning about a drop that never happens while the batch verb (whose
  copy guards sit at `R/ffmpeg.R:4325-4328`, above its probe) refuses first.
  Falsified by an existing `"copy"` guard test changing what it reads.

## Review

**Round 1 — 2026-08-27. Returned to `in-progress` on an AC1 failure.**
PR [#79](https://github.com/jmgirard/tidymedia/pull/79) (draft). `origin/master`
had not moved since the branch was cut (`git rev-list --left-right --count
origin/master...HEAD` = `0 7`), so no merge was needed before gathering
evidence.

- **AC1 — FAILS.** `testthat::test_local(filter = "audio-track-drop")` is green
  (35 tests, 99 assertions, 0 failures, 0 skips), and its AC1 test collects
  exactly one condition with every required substring. But that test calls the
  default `two_pass = FALSE`, and AC1 quantifies over every `run = TRUE` call
  naming no `audio_stream`. Measured directly on `make_multitrack_video()` with
  a counting `withCallingHandlers()`: scalar `two_pass = FALSE` 1 condition,
  scalar `two_pass = TRUE` **2**, batch `two_pass = FALSE` 1, batch
  `two_pass = TRUE` 1. The scalar verb's two sites are not mutually exclusive —
  the `if (two_pass)` block at `R/ffmpeg.R:2189` falls through rather than
  returning, so the single-pass site at `R/ffmpeg.R:2237` runs a second time on
  the two-pass path. AC4's two-pass test does not catch it because its
  `run_loudnorm_analysis()` mock `stop()`s before control reaches the second
  site. Same root cause, no behavioral effect: the hoisted
  `check_audio_codec_not_copy()` at `R/ffmpeg.R:2233` also runs on both paths,
  and its comment's "on the single-pass path only" and the probe comment's "so
  exactly one runs" are both false as written.
- **AC2 — evidence recorded, not ticked.** `normalize_audio_batch()` on a
  two-row multi-track jobs table signals one condition matching `Row 1` and
  `Row 2` (3 assertions, green).
- **AC3 — evidence recorded, not ticked.** All five silence cases green
  (`audio_stream` scalar; `audio_stream` batch argument; a cell on every row;
  `run = FALSE` at both `two_pass` values; single-track input).
- **AC4 — evidence recorded, not ticked.** Both mocked ordering tests green
  (3 assertions each): the warning arrives and the mock's error propagates.
- **AC5 — evidence recorded, not ticked.** All eight refusal cases green; the
  asserted strings (`target_loudness`, `audio_codec`) are the ones the existing
  guard tests assert (`test-normalize-audio-batch.R:180`,
  `test-normalize-audio.R:264`).
- **AC6, AC7 — not reached.** Round 1 stopped at the AC1 failure before the
  Rd excerpts and the full `devtools::check()` were re-run.

Consistency gate and the fresh-context review lenses were not reached.

**Fix directed to `/milestone-implement`:** make the two probe sites actually
mutually exclusive (gate the single-pass site on `!two_pass`, or return from the
two-pass branch), correct the two comments that claim exclusivity, and extend
AC1's coverage to `two_pass = TRUE` so the case that failed is asserted.
