# M44: Say something when audio tracks are dropped

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M43
- **Driving RR:** RR02
- **Principles touched:** IP1
- **Branch/PR:** `m44-implicit-track-drop-warning`

## Goal

Warn a caller whose input carried audio tracks the output did not, instead of
losing them in silence.

## Scope

**In:** a warning on the executing path when the input has more audio tracks
than the output receives and the caller named no `audio_stream`, across
`extract_audio()`, `convert_audio()` and both `_batch` siblings. Counting tracks
needs ffprobe, and this plan read the repo's convention — DESIGN.md: "Command
**compilation** is pure and CI-safe (no binaries)" — as forbidding that. RR02
found the convention never governed a verb's executing path, D011's `verify=`
having probed there since it shipped, so the D-entry in scope is a clarification
of that boundary rather than an extension of D013's carve-out, and it lands
before the probe does.

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
- [ ] AC4: The `_batch` siblings emit one aggregated warning naming every
      affected row and its dropped-track count, and a batch whose rows all name
      `audio_stream` (by argument or column) performs no probe at all.
- [ ] AC5: A `cairn/DECISIONS.md` entry clarifies the boundary the purity
      convention always drew rather than extending D013's carve-out, records
      D013's two-pass path as the sole `run = FALSE` exception, and states which
      paths may run a binary. It quotes the DESIGN.md convention it qualifies,
      and a companion DESIGN.md Conventions line names that boundary.
- [ ] AC6: `devtools::document()` no-diff; `devtools::test()` and
      `devtools::check()` clean — 0 errors, 0 warnings. NEWS records the new
      warning.
- [ ] AC7 (BC1): The ratified D024 entry is framed as a clarification and
      asserts all three of: (i) `ffm_compile()` and every `ffm_*` builder run no
      binary from any path; (ii) every verb's `run = FALSE` call runs no binary,
      with `normalize_audio(two_pass = TRUE)` (D013) named as the sole
      exception; (iii) a `run = TRUE` call may run a binary before or after
      compilation provided the probe's outcome changes nothing observable except
      a diagnostic condition. It contains no sentence claiming `run = FALSE` is
      binary-free on *every* verb without the D013 exception attached.
- [ ] AC8 (BC2): The entry's operative rule is effect-based: it licenses only
      probes whose outcome affects nothing but a diagnostic condition, and it
      states that a probe whose result changes the compiled command, resolves a
      default, decides whether execution proceeds, or selects between pipelines
      is outside the licence and requires its own decision entry.
- [ ] AC9 (BC3): The batch probe runs in the Layer-2 batch verbs before
      `ffm_batch()` is called, only when `run = TRUE`; `ffm_batch()`'s signature
      and behavior are unchanged by M44 (its formals are identical before and
      after the milestone diff).
- [ ] AC10 (BC4): Exactly one internal helper performs the stream-count probe,
      it lives in `R/ffprobe.R`, and no FFprobe token vector is assembled in any
      Layer-2 verb body (grep for `-select_streams` outside `R/ffprobe.R`
      returns no R-code hits).
- [ ] AC11 (BC5): The track-drop warning carries a documented condition class,
      and a test asserts the class; a test with the FFprobe locator mocked
      absent shows the probe path emits no error and no warning (a
      once-per-session `rlang::inform(.frequency = "once")` message is
      permitted).
- [ ] AC12 (BC6): The roxygen for all four verbs states the warning is
      best-effort: emitted when FFprobe is available and the input can be
      probed, silently skipped otherwise.

### Deviations from RR02

| BC | Departure | Why |
|---|---|---|
| BC1 | Clause (i)'s "every `ffm_*` builder" is read as the builders `ffm_compile()` walks; clause (ii)'s sole exception is the two-pass *path*, `normalize_audio_batch()` included. | `ffm_run()` and `ffm_batch()` carry `@family builder functions` (`R/ffm.R:1299`, `R/ffm_batch.R:57`) and both run binaries; `normalize_audio_batch(two_pass = TRUE, run = FALSE)` runs Phase 1 (`R/ffmpeg.R:3217`). Literal ingestion would put two code-contradicted claims in an append-only file. |
| BC4 | The grep is scoped to `R/` rather than to all R code. | As written it returns 9 hits today, every one a Layer-0 `ffprobe()` assertion helper under `tests/testthat/` (e.g. `test-audio-stream.R:259`), and T4 will add more. Over `R/` it is the check RR02 Q4 intends and passes today with one hit. |
| BC5/BC6 | The once-per-session `rlang::inform()` BC5 permits is not taken. | BC6 requires the docs to say the check is "silently skipped otherwise"; taking the notice makes that word false. RR02 marks the notice *consider*, not *apply*. |

## Coverage

- AC1 → T2, T4
- AC2 → T3, T4
- AC3 → T2, T4
- AC4 → T3, T4
- AC5 → T1
- AC6 → T5
- AC7 → T1
- AC8 → T1
- AC9 → T3
- AC10 → T2
- AC11 → T2, T4
- AC12 → T2, T3

## Tasks

- [ ] T1: Draft the D013-extending D-entry and surface it at the implement
      question gate before any probe lands; update DESIGN.md's Conventions line
      if the qualification belongs there too.
      *(RB tripwire: ip-touching)*
- [ ] T2: The scalar path — one stream-count helper in `R/ffprobe.R` beside
      `probe_one()`, reached through a quiet locator that trips neither
      `find_program()`'s warning nor `run_program()`'s abort, plus one shared
      warning-builder emitting a classed `cli_warn()` carrying the count,
      `audio_stream`, and the `probe_audio()` index offset. Skip silently when
      ffprobe is absent or the probe fails; roxygen says best-effort.
- [ ] T3: The batch path — probe up front in the Layer-2 verb before
      `ffm_batch()`, gated on `run`, skipping rows that name `audio_stream` by
      argument or column and probing each unique input once; one aggregated
      warning naming every affected row. `ffm_batch()` itself is not touched.
- [ ] T4: Tests: the warning fires once on M43's three-track fixture and not at
      all when `audio_stream` is given; the `PATH`-masked compile test for AC2;
      the FFprobe-absent case via `local_mocked_bindings()` on the quiet locator,
      not PATH masking, which cannot make ffprobe absent while ffmpeg is present;
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
- 2026-07-30: RR02 returned and was ingested — verdict is write D024 as a clarification of the purity boundary rather than an extension of D013's carve-out, tighten the licence to an effect-based test, probe up front in the batch verbs with no `ffm_batch()` hook, and put one helper in `R/ffprobe.R`; BC1–BC6 ingested verbatim as AC7–AC12 with `Driving RR: RR02`.
- 2026-07-30: amendment gate — AC4 amended to the aggregated batch warning (it contradicted the implement gate's own settlement), AC5 amended from "extends D013" to the clarification framing, and Scope In reworded because RR02 falsified its premise; user approved all three. T2/T3/T4 refined for the helper placement, the up-front batch probe and the mocked-locator test (minor).
- 2026-07-30: fresh-context [O] audit of BC1–BC6 before ingestion found BC4 false on arrival (its grep returns 9 test-side hits today) and BC1's clauses (i)/(ii) contradicted by `R/ffm.R:1299` and `R/ffmpeg.R:3217`; both ingested verbatim with three rows in the Deviations from RR02 table rather than softened.
- 2026-07-30: RB02/RR02 archived; M44 back to in-progress with 12 acceptance criteria — past the >~7 split tripwire, flagged to the user at the gate and left as one milestone by their choice.

## Decisions

- 2026-07-30 (RR02 ingest): **D024 is a clarification, not a second carve-out.**
  RR02 Q1 found DESIGN.md's purity convention constrains *compilation* and never
  governed a verb's executing path — the strong evidence being D011's `verify=`,
  which has run FFprobe on these very verbs' executing path since it shipped
  (reachable today as `extract_audio_batch(..., verify = ...)`) with no purity
  carve-out ever written for it. So nothing is being widened. D013 keeps one real
  content: it is the sole path that runs a binary under `run = FALSE`. Rules out
  writing a second carve-out, which would ratify the reading that every future
  probe needs its own entry, and rules out the draft's "widens from one verb to
  four" sentence, which also miscounted.
- 2026-07-30 (RR02 ingest): **the licence is effect-based, not
  destination-based.** A probe may run on a `run = TRUE` path only when its
  outcome — run, skip, succeed, fail — changes nothing observable except whether
  a diagnostic condition is signalled. RR02 Q2 showed the drafted test ("its
  result is not in the command") leaks: an abort-gating probe passes it and is a
  gate, not a diagnostic. Fail-open then follows as a consequence rather than a
  policy, since failing closed would give the probe a second effect. A probe that
  feeds a command, gates execution, resolves a default, or picks between
  pipelines is outside the licence and needs its own entry.
- 2026-07-30 (RR02 ingest): **"executing path" is modal, not temporal** — it
  means the call has `run = TRUE`, not that compilation has already returned.
  RR02 Q3: compilation is pure, so no pure function can observe whether a probe
  ran before or after it, and a rule hanging on that ordering protects nothing.
  This is what lets the batch verbs probe up front, before `ffm_batch()`. An
  `ffm_batch()` probe hook is rejected — verb-specific meaning in a generic
  runner, and an engine-contract change for one diagnostic (D011 already settled
  that verb-agnostic verification may live in the runner and verb-specific
  meaning may not).
- 2026-07-30 (RR02 ingest): **one probe helper in `R/ffprobe.R` beside
  `probe_one()`**, no FFprobe token literal in any Layer-2 verb body, and one
  shared warning-builder so the scalar and batch messages cannot drift (the
  M19/M35 divergence this milestone's plan already cites). No separate file:
  D013 earned `R/loudnorm_two_pass.R` with a 339-line analyze-parse-assemble
  subsystem; this is a counter.
- 2026-07-30 (RR02 ingest): **the once-per-session "check unavailable" notice is
  not taken.** RR02 marks it *consider*, and BC6 requires the docs to say the
  check is "silently skipped otherwise" — taking the notice would make that word
  false. The honest contract is carried by documentation (AC12) and a
  suppressible condition class (AC11) instead.
- 2026-07-30 (RR02 ingest audit): a fresh-context [O] audit of BC1–BC6 found BC4
  false on arrival and BC1's clauses (i) and (ii) contradicted by the code. Both
  are ingested verbatim with departures recorded in the Deviations from RR02
  table rather than reworded, because `cairn_validate`'s binding-criteria check
  string-compares the block. Confirmed by direct measurement:
  `grep -rn select_streams R/ tests/` returns 10 hits, 9 of them test-side
  Layer-0 assertion helpers; `R/ffm.R:1299` tags `ffm_run()`
  `@family builder functions`; `R/ffmpeg.R:3217` runs Phase 1 under
  `run = FALSE` in `normalize_audio_batch()`.

## Review
