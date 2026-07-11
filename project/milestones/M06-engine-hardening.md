# M06: Engine hardening & safe execution

- **Status:** review <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

Retire the engine debts deferred from M02–M04 before building anything else on
top of them. Chief among them: `ffm_run()` still executes via
`system(glue(...))` on a shell string (R/ffmpeg.R:25), so paths containing
`"`, `` ` ``, or `$` break or worse — the metadata layer got safe arg-vector
execution in M04 (`run_program()`), the ffmpeg side never did. Also closes the
smaller logged items: `ffm_map()` silently overridden in complex mode (M02 F3),
`separate_audio_video()` re-encoding where `-c copy` suffices (M03), the two
validation TODOs in R/ffm.R, and the covr-reports-0% instrumentation issue
open since M01.

## Scope

**In:** arg-vector execution path for Layer 1 (`ffm_run()`, `ffm_batch()`);
`ffm_map()` vs auto-map resolution; `separate_audio_video()` copy fast path;
validation TODOs at R/ffm.R:350 (codec) and R/ffm.R:458 (format); coverage
instrumentation fix.

**Out:** no new filters or verbs (M07/M09); Layer 0 string hatches
(`ffmpeg()`, `ffprobe()`, `mediainfo()`) keep their raw-string interface —
they are the escape hatch, quoting is the caller's job there; no output
verification layer (M08); no changes to the compiled-command *string* format
beyond what the shared assembly path requires.

## Acceptance criteria

- [ ] Binary-gated E2E test: a pipeline whose input **and** output paths
      contain spaces and shell metacharacters (`$`, `` ` ``, `'`) runs
      successfully through `ffm_run()` and `ffm_batch()`.
- [ ] Pure tests cover the arg-vector representation (no shell involved, no
      quoting applied); existing command-string snapshots still pass —
      `ffm_compile()`'s string output remains the reproducibility artifact.
- [ ] Test proves an explicit `ffm_map()` in complex mode is honored or
      rejected loudly (per D-M06 decision below), never silently overridden.
- [ ] `separate_audio_video()` compiles to stream-copy by default (snapshot),
      with re-encode still reachable; binary-gated E2E confirms playable
      outputs.
- [ ] Both R/ffm.R validation TODOs resolved: implemented, or rejected with a
      logged Decisions entry (no TODO comments remain).
- [ ] Coverage CI reports a real (nonzero) percentage.
- [ ] `devtools::check()` 0 errors / 0 warnings / 0 notes; full CI matrix
      green.

## Plan

- [x] T1: Shared assembly: refactor `ffm_compile()` internals so one ordered
      argument model renders both the display string and an argument vector
      (e.g. internal `ffm_args()`); pure tests for both renderings.
- [x] T2: Execution: route `ffm_run()` (and thus `ffm_batch(run = TRUE)`)
      through `run_program()` with the arg vector; keep the `command` string
      column; add hostile-path E2E tests (binary-gated).
- [x] T3: F3 fix: decide override-vs-abort for explicit `ffm_map()` in
      complex mode; implement + tests + roxygen.
- [x] T4: `separate_audio_video()` copy fast path (default copy, opt-out
      re-encode); snapshot + E2E tests.
- [x] T5: Resolve R/ffm.R:350 (codec validation) and R/ffm.R:458 (format
      validation) — implement or reject with logged decision.
- [x] T6: Diagnose covr 0% (likely instrumentation/srcref issue); fix and
      verify a real percentage on CI.
- [x] T7: `devtools::document()`, NEWS entry (0.1.0.9000+), final check.

## Work log

- 2026-07-10: Milestone planned (with M07–M10 queued as ideas).
- 2026-07-10: T1–T5 done: ffm_groups() shared assembly + internal ffm_args();
  ffm_run/ffm_batch/ffm_finish execute arg vectors via run_program(); map
  combine (D-M06-1); separate_audio_video copy default; check_token
  validation. Tests 280 pass / 0 fail (snapshots unchanged). Next: T6 covr.
- 2026-07-10: T6 root-caused: the empty R/zzz.R (alphabetically last R file)
  hits a covr split_on_line_directives bug (reversed range → NA chunk →
  parse-data crash → silent 0%). Deleted zzz.R; local coverage now 87.7%
  (615 traced expressions, was 7). Upstream covr bug worth reporting.
- 2026-07-10: T7 done: NEWS (dev version 0.1.0.9000), WORDLIST, check()
  0/0/0, tests 280/0, spelling clean. All tasks complete → status review.

## Decisions

- D-M06-1 (2026-07-10): Complex mode **combines** auto `-map "[vout]"` with
  explicit `ffm_map()` maps (enables stack-video + source-audio); nothing is
  silently dropped.
- D-M06-2 (2026-07-10): Arg-vector representation is **internal-only**
  (`ffm_args()`); `ffm_compile()` keeps returning the display string.
- D-M06-3 (2026-07-10): Codec/format validation is **cheap sanity checks**
  (single clean token, no whitespace/shell metacharacters); ffmpeg stays
  authoritative on existence.
- D-M06-4 (2026-07-10): `separate_audio_video()` **stream-copies by default**
  (lossless); re-encode is opt-in. Breaking post-0.1.0, logged in NEWS.

## Review

Filled in by `/milestone review`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
