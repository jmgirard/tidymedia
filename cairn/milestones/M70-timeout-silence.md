# M70: No timeout is silent

- **Status:** planned
- **Priority:** normal
- **Depends on:** M69
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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
onto a public return against `@param parallel`'s identity promise; the doc
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

- AC1 → T1, T2, T3
- AC2 → T2
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T8

## Tasks

- [ ] T1 Build the sweep that enumerates the silence rule's domain: deparse
      `mget(ls(asNamespace("tidymedia"), all.names = TRUE))` — M59's route, so
      it runs under `R CMD check` as well as `devtools::test()` — close the
      call graph to every function reaching `run_program()`, `system()` or
      `system2()`, and keep those that install a condition handler around it.
      The handler set is R's own condition API, recorded in the guard; a
      function installing none propagates by R's semantics and is out of the
      domain for that stated reason. Record current membership; mutation-probe.
- [ ] T2 `count_audio_streams()`: signal the timeout at the aggregation sites
      (`R/ffmpeg.R:438,748` and the scalar sites `545,1017,638`), once per
      call. Tests first, including the unchanged-counts assertion AC2 needs.
- [ ] T3 `capture_version()`: signal a timeout rather than recording `NA`
      silently. Tests first.
- [ ] T4 Drop the `tm_timed_out` attribute; give `verify_media()` its own
      non-absorbing probe. Tests first, including the `identical()` assertion.
- [ ] T5 One literal per program across the abort paths. Mutation probe must
      VARY the literal, not merely delete the assertion — deleting it
      re-certifies the mock, which is the defect J2 found.
- [ ] T6 Rewrite both doc sections to the uniform rule; retire M69's
      disclosure guard with the disclosure. Mutation-probe the new guard.
- [ ] T7 Write the D-entry.
- [ ] T8 Run the `verify` slot end to end; open the PR and confirm CI green
      before review, which is the gate that caught M69's AC3 failure.

## Work log

- 2026-08-26: created by /milestone-plan, splitting M69 after its third defect return fired thrash trigger (a).

## Decisions

## Review
