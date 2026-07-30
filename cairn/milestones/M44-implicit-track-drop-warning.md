# M44: Say something when audio tracks are dropped

- **Status:** review
- **Priority:** normal
- **Depends on:** M43
- **Driving RR:** RR02
- **Principles touched:** IP1
- **Branch/PR:** `m44-implicit-track-drop-warning` · https://github.com/jmgirard/tidymedia/pull/47

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

- [x] AC1: On the executing path, when the input carries more audio tracks than
      the output receives and the caller named no `audio_stream`, the verb warns
      once per input, stating how many tracks were dropped, naming
      `audio_stream`, and stating that `probe_audio()`'s `index` is the absolute
      stream index while `audio_stream` counts audio streams from 0 — the two
      differ (`1,2,3` vs `0,1,2` on M43's three-track fixture), so a message
      naming `probe_audio()` without the offset walks a reader into an
      off-by-one. A caller who named `audio_stream` gets no warning.
- [x] AC2: `run = FALSE` runs no binary. A test with ffmpeg and ffprobe masked
      off `PATH` (`Sys.which() == ""`, M30's trick) compiles every affected call
      cleanly, and the ungated roxygen `@examples` plus
      `vignettes/tidymedia.Rmd:49` still build with the binaries masked.
- [x] AC3: The warning is skipped without error when ffprobe is absent or the
      input cannot be probed — an unprobeable input still runs and still warns
      about nothing.
- [x] AC4: The `_batch` siblings emit one aggregated warning naming every
      affected row and its dropped-track count, and a batch whose rows all name
      `audio_stream` (by argument or column) performs no probe at all.
- [x] AC5: A `cairn/DECISIONS.md` entry clarifies the boundary the purity
      convention always drew rather than extending D013's carve-out, records
      D013's two-pass path as the sole `run = FALSE` exception, and states which
      paths may run a binary. It quotes the DESIGN.md convention it qualifies,
      and a companion DESIGN.md Conventions line names that boundary.
- [x] AC6: `devtools::document()` no-diff; `devtools::test()` and
      `devtools::check()` clean — 0 errors, 0 warnings. NEWS records the new
      warning.
- [x] AC7 (BC1): The ratified D024 entry is framed as a clarification and
      asserts all three of: (i) `ffm_compile()` and every `ffm_*` builder run no
      binary from any path; (ii) every verb's `run = FALSE` call runs no binary,
      with `normalize_audio(two_pass = TRUE)` (D013) named as the sole
      exception; (iii) a `run = TRUE` call may run a binary before or after
      compilation provided the probe's outcome changes nothing observable except
      a diagnostic condition. It contains no sentence claiming `run = FALSE` is
      binary-free on *every* verb without the D013 exception attached.
- [x] AC8 (BC2): The entry's operative rule is effect-based: it licenses only
      probes whose outcome affects nothing but a diagnostic condition, and it
      states that a probe whose result changes the compiled command, resolves a
      default, decides whether execution proceeds, or selects between pipelines
      is outside the licence and requires its own decision entry.
- [x] AC9 (BC3): The batch probe runs in the Layer-2 batch verbs before
      `ffm_batch()` is called, only when `run = TRUE`; `ffm_batch()`'s signature
      and behavior are unchanged by M44 (its formals are identical before and
      after the milestone diff).
- [x] AC10 (BC4): Exactly one internal helper performs the stream-count probe,
      it lives in `R/ffprobe.R`, and no FFprobe token vector is assembled in any
      Layer-2 verb body (grep for `-select_streams` outside `R/ffprobe.R`
      returns no R-code hits).
- [x] AC11 (BC5): The track-drop warning carries a documented condition class,
      and a test asserts the class; a test with the FFprobe locator mocked
      absent shows the probe path emits no error and no warning (a
      once-per-session `rlang::inform(.frequency = "once")` message is
      permitted).
- [x] AC12 (BC6): The roxygen for all four verbs states the warning is
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

- [x] T1: Draft the D013-extending D-entry and surface it at the implement
      question gate before any probe lands; update DESIGN.md's Conventions line
      if the qualification belongs there too.
      *(RB tripwire: ip-touching)*
- [x] T2: The scalar path — one stream-count helper in `R/ffprobe.R` beside
      `probe_one()`, reached through a quiet locator that trips neither
      `find_program()`'s warning nor `run_program()`'s abort, plus one shared
      warning-builder emitting a classed `cli_warn()` carrying the count,
      `audio_stream`, and the `probe_audio()` index offset. Skip silently when
      ffprobe is absent or the probe fails; roxygen says best-effort.
- [x] T3: The batch path — probe up front in the Layer-2 verb before
      `ffm_batch()`, gated on `run`, skipping rows that name `audio_stream` by
      argument or column and probing each unique input once; one aggregated
      warning naming every affected row. `ffm_batch()` itself is not touched.
- [x] T4: Tests: the warning fires once on M43's three-track fixture and not at
      all when `audio_stream` is given; the `PATH`-masked compile test for AC2;
      the FFprobe-absent case via `local_mocked_bindings()` on the quiet locator,
      not PATH masking, which cannot make ffprobe absent while ffmpeg is present;
      an unprobeable input still runs. Prove the warning test discriminates by
      making the count unconditional — it must go red (M39 lesson).
- [x] T5: NEWS entry; `devtools::document()`, `test()`, `check()`.

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
- 2026-07-30: T1 done — D024 written as a clarification per AC7/AC8: the pure surface is compilation plus `run = FALSE` (D013's two-pass path the sole exception, batch sibling included), the licence is effect-based with four named out-of-licence probe shapes each needing its own entry, and scope is stated by four conditions with the audio verbs as first instances. DESIGN.md's Conventions line now names that boundary. `cairn_validate` green.

- 2026-07-30: T2/T3 done — `count_audio_streams()` in `R/ffprobe.R` (one narrow ffprobe call, NA on every failure path), a shared `warn_dropped_audio()` builder carrying the count, `audio_stream` and the `probe_audio()` index offset under class `tidymedia_dropped_audio`, wired into both scalar verbs behind `isTRUE(run) && is.null(audio_stream)` and into both batch verbs up front before `ffm_batch()`, whose formals are untouched.
- 2026-07-30: a mid-task `git checkout R/ffmpeg.R` during the first mutation probe reverted T2/T3's then-uncommitted work, making that probe's four identical "red" results meaningless — they only showed the feature was absent; reapplied, committed, and re-probed against a committed baseline.
- 2026-07-30: T4 done — 15 tests, all passing. The M39 discrimination probe mutated four gates and each went red: unconditional count (4 tests), dropping the `audio_stream` gate (1), dropping the `run` gate (1), dropping the unique-input dedup (1). Two probe findings fixed in place — the first AC2 test was vacuous because `count_audio_streams()`'s `tryCatch` swallows a `stop()`ing mock, so it now counts invocations instead; and the absent-ffprobe short-circuit proved redundant against that `tryCatch`, so its comment now records that rather than claiming to be the guarantee.
- 2026-07-30: full `devtools::test()` clean (exit 0, no failures); `devtools::document()` regenerated the four verbs' `.Rd` files; NEWS entry written.

- 2026-07-30: T5 done — `devtools::check()` clean (0 errors, 0 warnings, 0 notes; vignette rebuilt OK) and `devtools::document()` no-diff. AC10's grep, scoped to `R/` per the Deviations table, finds `-select_streams` only in `R/ffprobe.R` (the new counter and `probe_one()`); every ungated `@examples` line on the four verbs uses `run = FALSE`, so no example can trigger a probe.
- 2026-07-30: all tasks checked; status to review.
- 2026-07-30: `R/ffmpeg.R` is a CRLF file and Python text-mode edits silently rewrote it to LF, turning the branch diff into a 9869-line whole-file rewrite; endings restored with `perl -pi -e 's/\n/\r\n/'` and `devtools::check()` re-run against the restored file (still 0/0/0), so the recorded check result matches what is on the branch. Diff is now +149 lines in that file.
- 2026-07-30: review fan-out (3 lenses + scorer) returned 15 findings, 2 at or above the 80 threshold and both fixed on the branch — F1 (96) cli glue-interpolated file paths in the warning bullets, so `my{video}.mkv` aborted the verb and `{n}.mkv` silently printed a nonexistent name; F2 (82) `find_ffprobe()`'s error channel escaped the probe's `tryCatch`, aborting a call that previously ran. Both pinned by discriminating tests (18 tests / 51 assertions); the other 13 findings are logged in the Review section. Final `devtools::check()` after both fixes: Status OK, 0 errors / 0 warnings / 0 notes, 1m31s.

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

Evidence gathered 2026-07-30 on `m44-implicit-track-drop-warning` at PR #47.
Fresh `test_local(filter = "audio-track-drop")`: 15 tests, 37 assertions, 0
failed, 0 errors, 0 skipped.

- **AC1** — `extract_audio()` on the 3-track fixture emits one
  `tidymedia_dropped_audio` warning reading "Dropping 2 audio tracks from 1
  input", naming the file and its count, pointing at `audio_stream`, and
  spelling the offset ("its index column counts ALL streams while
  `audio_stream` counts audio streams from 0 -- ... those read 1, 2, 3 there and
  0, 1, 2 here"). The offset claim is oracled by its own test:
  `probe_audio(infile = fixture)$index` is `1, 2, 3`. `audio_stream = 1` on
  `extract_audio()` and `= 2` on `convert_audio()` both emit nothing; a
  single-track input emits nothing. 11 assertions across 5 tests.
- **AC2** — the strong test counts `run_program()` invocations across all four
  verbs at `run = FALSE`: 0. Deleting the `run` gate turns that test red
  (measured at T4). Separately, with `PATH` emptied (`Sys.which()` returns `""`
  for both binaries) all four compile with no warning, and the vignette's
  `extract_audio(video, "audio.m4a", run = FALSE)` compiles clean under the same
  mask. Every ungated `@examples` line on the four verbs passes `run = FALSE`,
  so no example can probe.
- **AC3** — with `find_ffprobe` mocked to `NULL`, a real `run = TRUE`
  `extract_audio()` on the 3-track fixture runs with no error and no warning.
  `count_audio_streams()` returns `NA_integer_` and signals nothing for both an
  unreadable input and an absent locator; `warn_dropped_audio()` on an `NA`
  count warns nothing.
- **AC4** — a 3-row batch (rows 1 and 3 multi-track, row 2 single-track) emits
  exactly one warning: "Dropping 4 audio tracks from 2 inputs", naming Row 1 and
  Row 3 and not Row 2. A jobs table whose every row names `audio_stream`, and
  one covered by the scalar argument, each record zero calls into the probe; an
  `NA` cell is probed, per D023's column semantics. A repeated input is probed
  once (`c("a.mkv", "a.mkv", "b.mkv")` -> two probes).
- **AC5** — `cairn/DECISIONS.md` D024 quotes the DESIGN.md Conventions line
  verbatim in its opening paragraph, frames itself as a clarification rather
  than an extension of D013's carve-out, names D013's two-pass path as the sole
  `run = FALSE` exception, and states which paths may run a binary. The
  companion DESIGN.md Conventions line names that boundary.
- **AC6** — `devtools::check()` at this commit: Status OK, 0 errors / 0 warnings
  / 0 notes, duration 1m36s, full test suite run inside it and vignettes
  rebuilt. `devtools::document()` re-run afterwards leaves no diff in `man/`,
  `NAMESPACE`, `R/`, `tests/` or `NEWS.md`. NEWS records the new warning under
  New features, including the condition class and the best-effort contract.
- **AC7 (BC1)** — D024 asserts clause (i) ("`ffm_compile()` and every builder it
  walks run no binary from any path", with the doc-tag narrowing recorded in the
  Deviations table), clause (ii) with the exception attached in the same
  sentence, and clause (iii) ("A `run = TRUE` call may run a binary before or
  after compilation, provided the conditions below hold"). Every `run = FALSE`
  mention in the entry was read: none states binary-freedom on every verb
  without the D013 exception attached.
- **AC8 (BC2)** — D024's operative rule is effect-based ("changes nothing
  observable except whether a diagnostic condition is signalled") and lists the
  four out-of-licence shapes (result enters the command / resolves a default /
  decides whether execution proceeds / selects between pipelines), each stated
  to need "its own decision entry before it is built".
- **AC9 (BC3)** — `git diff --stat master..HEAD -- R/ffm_batch.R` is empty; the
  file is untouched. `names(formals(ffm_batch))` is the nine expected names, and
  a test pins them. Both batch verbs call `warn_dropped_audio_batch()` above
  their `ffm_batch()` call, guarded by `isTRUE(run)`.
- **AC10 (BC4)** — scoped to `R/` per the Deviations table: `-select_streams`
  appears at `R/ffprobe.R:136` (the new counter) and `R/ffprobe.R:169`
  (`probe_one()`, pre-existing). No hit in any Layer-2 verb body. Exactly one
  helper performs the probe.
- **AC11 (BC5)** — the warning carries class `tidymedia_dropped_audio`; four
  roxygen blocks document it as a `suppressWarnings(classes = ...)` handle; the
  class is asserted by `expect_s3_class()` in two tests and by
  `expect_warning(class = )` in four more. The mocked-absent-locator test shows
  no error and no warning; no once-per-session notice was taken.
- **AC12 (BC6)** — all four generated `.Rd` files carry the best-effort
  sentence: emitted when FFprobe is available and the input can be probed,
  silently skipped otherwise.

### Independent review (3 lenses + scorer)

Three fresh-context reviewers with distinct evidence bases, then a Sonnet scorer
that did not generate the findings. 15 findings reported, 2 scored >=80.

**Actioned (>=80), both fixed on the branch:**

- **F1 (96) — cli glue-interpolates file paths in the warning bullets.**
  `sprintf()` built each bullet and handed it to `cli_warn()`, which
  glue-interpolates in `warn_dropped_audio()`'s own frame. Reproduced by two
  agents independently: `extract_audio("my{video}.mkv", ...)` ABORTED the verb
  ("Could not evaluate cli '{}' expression"), and `{n}.mkv` -- naming a local of
  that function -- silently printed "3.mkv", a file that does not exist. Both
  give the probe an effect beyond its diagnostic, which is what D024 licenses it
  on not having. Fixed by escaping braces after `sprintf()`; a test over four
  hostile paths plus the batch builder pins it, and deleting the escape turns
  that test red.
- **F2 (82) — locator errors escaped the probe.** `find_ffprobe()` sat inside
  `suppressWarnings()` but OUTSIDE the `tryCatch()`. `find_program()` reads a
  `set_ffprobe()` user config with `readLines()` then tests it, so an empty
  config gives `if (logical(0))` and a two-line one a length-2 condition --
  both aborting a verb that previously just ran. Scored against the code, not
  the report: both throws confirmed by execution. Fixed by wrapping the locator
  in its own `tryCatch()` and guarding `length(loc) != 1L` first (a
  `character(0)` locator would have made `is.na(loc)` return `logical(0)` and
  thrown on the guard itself). Two tests pin it; removing the wrap turns one
  red.

**Logged, below the 80 threshold, not actioned (13):**

- F4 (72) probe cost is unconditional, serial in batch, precedes `ffm_batch()`
  so it ignores `parallel = TRUE`, and has no opt-out; not documented in roxygen
  or NEWS. The scorer read it as an accepted trade-off RR02 settled.
- F3 (68) the batch summary says "from N inputs" while counting affected ROWS;
  a repeated input makes the arithmetic read as impossible. Test pins the
  current wording.
- F8 (66) batch bullets use `basename()` where the scalar path uses the full
  path, so `/a/take1.mkv` and `/b/take1.mkv` are indistinguishable.
- F11 (60) test gaps; its two strongest cases were F1 and F2, now covered.
- F7 (50) / F14 (22) the column-vs-argument rule is derived in
  `warn_dropped_audio_batch()` and again via `batch_stream_cell()`; they agree
  today.
- F9 (45) the documented `suppressWarnings(classes = ...)` snippet omits `expr`
  and is not runnable as literal code (it is prose inside `\code{}`, not an
  executed example).
- F10 (30) the message's worked example ("1, 2, 3 there and 0, 1, 2 here") holds
  only when one non-audio stream precedes the audio; AC1 mandated those exact
  numbers.
- F6 (22) `rows[keep]` is correct as written; flagged as order-fragile.
- F5 (18) under `options(warn = 2)` a successful probe stops execution and a
  failed one does not; generic R warning semantics, not specific to this diff.
- F12 (15) AC7 and AC10 hold via the Deviations table rather than literally --
  raised so the gate signs off on the deviations explicitly.
- F15 (10) / F13 (8) a code comment's M19/M35 citation is broader than the two
  archives strictly support; blank lines and 81-84 column lines.

The blame-history lens found no historical conflict and verified the CRLF commit
by measurement (content-only diff empty, both sides 100% CRLF). The
prior-review lens found no regression against any archived `## Review` section
or `LESSONS.md` entry; its GitHub inline-comment probe returned `[]`, so that
surface was skipped.

**Note for hygiene:** the CRLF incident this milestone hit is a recurrence of
M35's F1 (scored 92) and is already covered verbatim by a `LESSONS.md` line
naming "a Python `open(p, \"w\").write()` round-trip" as the cause. The lesson
existed and was not consulted; no new lesson is owed for it.

### Consistency gate

- `cairn_validate` exit 0, every check PASS. One advisory: `sizing` warns that
  M44 carries 12 acceptance criteria against the >7 tripwire — expected, raised
  with the user at the RR02 ingest gate and kept as one milestone by their
  choice.
- `cairn_impact` skipped: the milestone changed no `DESIGN.md` IP/GP principle,
  only a Conventions line (`git diff master..HEAD -- cairn/DESIGN.md` confirms).
