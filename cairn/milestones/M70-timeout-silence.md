# M70: No timeout is silent

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M69
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m070-timeout-silence

## Goal

Close the gap M69 disclosed rather than fixed: some call paths absorb a
wall-clock timeout with no warning at all, so a bounded hang under
`remove_audio()` or in a provenance manifest is invisible to the caller. After
this, every call a reached limit kills either aborts or says so.

**Surface tier: user-facing** — exported readers and verbs, their runtime
behavior, and published documentation.

## Scope

**In:** the two no-warning paths M69's AC8 discloses — `count_audio_streams()`
(`R/ffprobe.R:199`, reached from `R/ffmpeg.R:438,545,638,748,1017`) and
`capture_version()` (`R/ffm_manifest.R:127`); a call-graph sweep that
enumerates the domain the silence rule quantifies over, replacing M69's
hand-list; J2's split program literal (`probe_one()` says `ffprobe`,
`R/ffprobe.R:204,242`, where the Layer 0 hatch says `FFprobe`,
`R/ffprobe.R:22`); J7's `tm_timed_out` attribute (`R/ffprobe.R:158`) leaking
onto a public return against `@param parallel`'s identity promise; the third
no-warning path the T1 sweep found and M69's hand-list did not: `ffm_batch()`
(`R/ffm_batch.R:127`) records every job failure as `success = FALSE` and signals
no condition, so a reached limit is silent through it and through the 15
`_batch` verbs and `segment_video()`, which fan out through it; the doc
rewrite that retires M69's disclosure; a D-entry.

**Out:**
- Per-call `timeout =` arguments, and making `parallel = TRUE` workers see the
  option → the existing ROADMAP candidate row.
- A tighter kill than base R's SIGINT/SIGTERM/SIGKILL ladder (`processx` or
  equivalent) → new ROADMAP candidate row. M69's return-2 gate rejected it and
  the row was never written; this plan writes it.
- Changing what `count_audio_streams()` RETURNS. D024 licenses the probe only
  while its outcome changes nothing but whether a warning fires, and a warning
  is inside that licence where a changed count is not.

## Acceptance criteria

- [ ] AC1 For every function the T1 sweep returns, a forced timeout through it
      signals at least one condition the caller can see — an abort of class
      `tidymedia_timeout`, or a warning naming the timeout. None absorbs it
      silently. Linux and macOS only, since the writer-less FIFO fixture cannot
      be built on Windows (M69 AC3); the sweep itself runs on every platform.
- [ ] AC2 `count_audio_streams()`'s timeout warning fires once per call at the
      point the counts are assembled, naming how many inputs timed out — not
      once per file. Evidence: a 3-input batch with 2 timed-out inputs signals
      exactly one warning, and the counts it returns are identical to those the
      silent version returned, so D024's licence still holds.
- [ ] AC3 `probe_all()`'s return carries no `tm_timed_out` attribute;
      `verify_media()` reaches its refusal through its own non-absorbing probe.
      Evidence: `identical()` holds between the `parallel = TRUE` and
      `parallel = FALSE` returns as `@param parallel` promises, and a hung
      input still makes `verify_media()` abort rather than report a mismatch.
- [ ] AC4 Each program is named by one literal across every path that can abort
      about it, asserted against the real call path rather than a mock's
      literal. Evidence: a timeout raised through `probe_one()` and one raised
      through `ffprobe()` name the program identically.
- [ ] AC5 `?tidymedia` and `NEWS.md` state the uniform rule — a reached limit
      either aborts or warns, never passes unremarked — and M69's no-warning
      disclosure is gone from both, along with the guard that fenced it.
- [ ] AC6 `cairn/DECISIONS.md` gains a D-entry recording the silence rule, the
      per-call warning grain chosen over per-file, D024's licence as the reason
      a warning is available where a changed return is not, and the falsifier.
      It supersedes the disclosure half of M69's D-entry.
- [ ] AC7 The `verify` slot of `cairn/PROFILE.md` is clean —
      `devtools::document()`, `devtools::test()` and `devtools::check()` (0
      errors, 0 warnings) — and CI is green on the PR.

## Coverage

- AC1 → T1, T2, T3, T9
- AC2 → T2
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1 Build the sweep that enumerates the silence rule's domain: deparse
      `mget(ls(asNamespace("tidymedia"), all.names = TRUE))` — M59's route, so
      it runs under `R CMD check` as well as `devtools::test()` — close the
      call graph to every function reaching `run_program()`, `system()` or
      `system2()`, and keep those that install a condition handler around it.
      The handler set is R's own condition API, recorded in the guard; a
      function installing none propagates by R's semantics and is out of the
      domain for that stated reason. Record current membership; mutation-probe.
- [x] T2 `count_audio_streams()`: signal the timeout at the aggregation sites
      (`R/ffmpeg.R:438,748` and the scalar sites `545,1017,638`), once per
      call. Tests first, including the unchanged-counts assertion AC2 needs.
- [x] T3 `capture_version()`: signal a timeout rather than recording `NA`
      silently. Tests first.
- [x] T4 Drop the `tm_timed_out` attribute; give `verify_media()` its own
      non-absorbing probe. Tests first, including the `identical()` assertion.
- [x] T5 One literal per program across the abort paths. Mutation probe must
      VARY the literal, not merely delete the assertion — deleting it
      re-certifies the mock, which is the defect J2 found.
- [x] T6 Rewrite both doc sections to the uniform rule; retire M69's
      disclosure guard with the disclosure. Mutation-probe the new guard.
- [ ] T7 Write the D-entry.
- [ ] T8 Run the `verify` slot end to end; open the PR and confirm CI green
      before review, which is the gate that caught M69's AC3 failure.
- [x] T9 `ffm_batch()`: keep `run_one()`'s caught error object, test it for
      class `tidymedia_timeout`, and warn once at the end of the run naming how
      many jobs timed out. Non-timeout job failures keep today's silent
      `success = FALSE`. Tests first.

## Work log

- 2026-08-26: created by /milestone-plan, splitting M69 after its third defect return fired thrash trigger (a).
- 2026-08-26: implement gate — AC1 forcing = injected `abort_timeout()` condition per swept member, with the writer-less-FIFO fixture kept for anchor paths; AC3 shape = internal `absorb=` flag on the shared `probe_all()` worker; AC4 literal = display case (`FFmpeg`/`FFprobe`/`MediaInfo`) everywhere.
- 2026-08-26: T1 — sweep built (`tests/testthat/helper-timeout-sweep.R`, `test-timeout-silence.R`). Domain = the 53 exported functions reaching `system`/`system2` through a symbol-mention closure over the namespace; membership recorded, mutation-probed (empty seed set collapses it; `ffm_compile` and the pure builders stay out; `run_program()` is derived, not seeded), and every member carries a call spec so a member with no way to be driven fails rather than being skipped. Deliberately NOT M62's call-head graph: `probe_all()` reaches FFprobe only via `purrr::map(infile, probe_one)`, so a head-only walk drops it, the four `probe_*()` accessors and `verify_media()`. Pinned as a test.
- 2026-08-26: T2 — `count_audio_streams()` returns the absorbed-timeout sentinel instead of a silent NA, and a new `count_audio_streams_all()` assembles the counts for all five sites (`R/ffmpeg.R:437,543,636,745,1014`), warning once per call with class `tidymedia_probe_timeout` and the count of timed-out inputs. Returned counts unchanged (NA for a killed probe), so D024's licence still holds.
- 2026-08-26: T3 — `capture_version()` returns the sentinel; `tool_versions()` warns once per call naming which tools the limit killed (class `tidymedia_probe_timeout`). Recorded manifest value unchanged (NA). A missing binary is still a silent NA.
- 2026-08-26: T9 — `ffm_batch()` warns once per run (class `tidymedia_batch_timeout`) naming how many jobs, and how many verifications, the limit killed; `run_one()` now returns a per-job record so the fact survives `unlist()` off the parallel workers. Non-timeout failures keep today's silent `success = FALSE`. AC1's 53-member grid now passes, with a mutation probe standing a swallowing implementation in front of one member, and three real-FIFO anchors (`ffprobe()` condition identity, `count_audio_streams_all()`, `ffm_batch()`). `local_blocking_input()` moved to `helper-timeout-sweep.R` so both suites share one fixture.
- 2026-08-26: T4 — `probe_all()`'s body factored into `probe_all_impl(absorb =)`; `absorb = FALSE` re-raises, so `verify_media()` gets its refusal from the shared body instead of a `tm_timed_out` attribute on a public return. Attribute gone; `identical()` holds between the `parallel = TRUE` and `parallel = FALSE` returns with a hung input.
- 2026-08-26: T5 — `probe_one()`/`count_audio_streams()` (`ffprobe`), the MediaInfo readers (`mediainfo`) and `capture_version()` now use the display literals the Layer 0 hatches already used. `tm_program_literals()` reads the `program` argument out of every `run_program`/`guard_timeout`/`abort_timeout`/`capture_version` call node in the namespace (named or positional) and the guard asserts exactly `FFmpeg`/`FFprobe`/`MediaInfo`; the mutation probe VARIES a literal rather than deleting the assertion. Real-path evidence: a FIFO-hung `probe_one()` and `ffprobe()` name the program identically.
- 2026-08-26: T6 — `?tidymedia` and `NEWS.md` now state the uniform rule ("a reached limit is never silent") over two lists, and say the lists are derived from the call graph rather than recalled. M69's scoped-claim and no-warning-disclosure guards retired from `test-runtime-timeout.R`; the replacement guard in `test-timeout-silence.R` asserts the new rule, asserts the retired text is absent, and mutation-probes both against a stand-in carrying M69's sentences. `doc_timeout_sources()` moved to `helper-rd.R` so both suites share one reader.
- 2026-08-26: substantive amendment (Scope In + T9 + Coverage AC1). The T1 sweep found a third no-warning path M69's hand-list missed — `ffm_batch()` absorbs every job failure into `success = FALSE` and signals nothing, leaving 17 of the 53 swept exports silent under a forced timeout. AC1 as written already binds them; Scope gains the path, T9 gains the fix (warn on timed-out jobs only; non-timeout failures unchanged).

## Decisions

## Review
