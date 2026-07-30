# M44: Say something when audio tracks are dropped

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M43
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m44-implicit-track-drop-warning`

## Goal

Warn a caller whose input carried audio tracks the output did not, instead of
losing them in silence.

## Scope

**In:** a warning on the executing path when the input has more audio tracks
than the output receives and the caller named no `audio_stream`, across
`extract_audio()`, `convert_audio()` and both `_batch` siblings. Counting tracks
needs ffprobe at build time, which the repo's conventions forbid on the
compilation path — DESIGN.md: "Command **compilation** is pure and CI-safe (no
binaries)" — and which D013's carve-out authorizes for exactly one verb
(`normalize_audio(two_pass = TRUE)`) and for building a later command, not for
emitting a warning. So the D-entry extending D013 is in scope and lands before
the probe does.

**Out:** any probe on the `run = FALSE` path — compilation stays binary-free, and
AC2 is the proof. Warning about dropped *video* or *subtitle* streams → not in
scope; `strip_metadata()`'s `-map 0` keeps every stream and the carry-through
verbs are the candidate row M43 opened. Erroring rather than warning → rejected
here: a multi-track input is legal, and the selector is how a caller resolves it.

## Acceptance criteria

- [ ] AC1: On the executing path, when the input carries more audio tracks than
      the output receives and the caller named no `audio_stream`, the verb warns
      once per input, stating how many tracks were dropped, naming
      `audio_stream`, and stating that `probe_audio()`'s `index` is the absolute
      stream index while `audio_stream` counts audio streams from 0 — the two
      differ (`1,2,3` vs `0,1,2` on M43's three-track fixture), so a message
      naming `probe_audio()` without the offset walks a reader into an
      off-by-one. A caller who named `audio_stream` gets no warning.
- [ ] AC2: `run = FALSE` runs no binary. A test with ffmpeg and ffprobe masked
      off `PATH` (`Sys.which() == ""`, M30's trick) compiles every affected call
      cleanly, and the ungated roxygen `@examples` plus
      `vignettes/tidymedia.Rmd:49` still build with the binaries masked.
- [ ] AC3: The warning is skipped without error when ffprobe is absent or the
      input cannot be probed — an unprobeable input still runs and still warns
      about nothing.
- [ ] AC4: `_batch` siblings warn per row, and a batch whose rows all name
      `audio_stream` (by argument or column) performs no probe at all.
- [ ] AC5: A `cairn/DECISIONS.md` entry extends D013, recording which paths may
      run a binary while building a command, that this extends the carve-out from
      one verb to four and from command-building to warning, and that
      compilation stays binary-free. It quotes the DESIGN.md convention it
      qualifies.
- [ ] AC6: `devtools::document()` no-diff; `devtools::test()` and
      `devtools::check()` clean — 0 errors, 0 warnings. NEWS records the new
      warning.

## Coverage

- AC1 → T2, T4
- AC2 → T3, T4
- AC3 → T2, T4
- AC4 → T3, T4
- AC5 → T1
- AC6 → T5

## Tasks

- [ ] T1: Draft the D013-extending D-entry and surface it at the implement
      question gate before any probe lands; update DESIGN.md's Conventions line
      if the qualification belongs there too.
      *(RB tripwire: ip-touching)*
- [ ] T2: The scalar path — probe the input beside `ffm_run()` where D013 put the
      two-pass orchestrator, count audio streams, emit one `cli_warn()` carrying
      the count, `audio_stream`, and the `probe_audio()` index offset. Skip
      silently when ffprobe is absent or the probe fails.
- [ ] T3: The batch path — warn per row, and skip the probe entirely for rows
      that name `audio_stream` by argument or column.
- [ ] T4: Tests: the warning fires once on M43's three-track fixture and not at
      all when `audio_stream` is given; the `PATH`-masked compile test for AC2;
      an unprobeable input still runs. Prove the warning test discriminates by
      making the count unconditional — it must go red (M39 lesson).
- [ ] T5: NEWS entry; `devtools::document()`, `test()`, `check()`.

## Work log

- 2026-07-29: created by /milestone-plan.
- 2026-07-29: plan gate chose warning only on the executing path over no warning at all and over scalar-verbs-only, because it keeps compilation binary-free while still surfacing silent track loss, and divergent scalar/batch behavior is a defect this repo has fixed twice (M19, M35); falsified by the per-row probe cost (~1.2 s measured per probe, all incurred up front since `ffm_batch` builds every pipeline before running any) making a large batch unusable.
- 2026-07-29: plan chose warning over erroring on an implicit drop, because a multi-track input is legal input and `audio_stream` is the caller's resolution; falsified by a silent-drop incident where a warning was present and still missed.
- 2026-07-29: split from M43 because 9 acceptance criteria hit the sizing tripwire; this half is what needs a convention decision, so separating it lets the selector ship first.
- 2026-07-30: /milestone-implement started; branch `m44-implicit-track-drop-warning` cut from master.
- 2026-07-30: implement question gate settled two of three — the track count uses a narrow one-shot `ffprobe -select_streams a` call rather than `probe_audio()`, because a failed probe then returns nothing (AC3's silent skip) with no `probe_all()` warning to suppress and roughly half the invocations at the ~1.2 s per-input cost the plan measured; and the `_batch` verbs emit one aggregated warning naming every affected row rather than one per row, so a large batch never hits R's 50-warning collapse.
- 2026-07-30: T1 escalated to /milestone-brief at the user's choice on the plan's `ip-touching` tripwire — the drafted D024 (extending D013's executing-path carve-out from one verb to four and from command-building to diagnostics, plus a DESIGN.md Conventions cross-reference) goes to Fable-level review before any probe code lands; T2–T5 wait on it because Scope puts the D-entry ahead of the probe.
- 2026-07-30: blocked on RB02 (`cairn/reviews/RB02-binary-on-executing-path.md`) — the drafted D024 and its six questions go to independent review before T2 lands.

## Decisions

## Review
