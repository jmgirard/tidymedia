# M45: Give a multi-track `separate_audio_video()` caller a way out

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M43
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m45-separate-av-multitrack`

## Goal

Let a caller separate a multi-track file into a single-stream audio container,
and say why when FFmpeg refuses.

## Scope

**In:** an `audio_stream` selector on `separate_audio_video()` and
`separate_audio_video_batch()` (batch-wide argument plus per-row column),
narrowing the audio output to one track. The `NULL` default keeps **every**
audio track — today's `-map 0:a`, which succeeds into `.mka`/`.m4a` carrying all
three tracks and fails into `.aac`/`.mp3`/`.wav` with exit 234 and a zero-byte
file (measured 2026-07-30, ffmpeg 8.1.2). Plus an enriched abort raised *after*
FFmpeg fails, naming the track count and the way out, and a `DECISIONS.md` entry
extending D023, whose `NULL` bullet reads the other way for the extraction verbs.

**Out:** the four remaining pass-through verbs (`standardize_video`,
`crop_video`, `segment_video`, `anonymize_video`) → they stay on the
`audio_stream`-carry candidate row, which this milestone edits to record that it
absorbed the fifth. Any probe before FFmpeg runs → compilation stays binary-free
(DESIGN.md Conventions) and no D013 extension is needed; AC3 is the proof. A
symmetric `video_stream` for `-map 0:v` → a candidate row; measured
2026-07-30, `-map 0:v` on a two-video-stream input into `.mp4` carries both and
no verb default writes video to a single-video-stream container. Deleting the
zero-byte output FFmpeg leaves, and running the video command after the audio one
failed → one grouped candidate row.

## Acceptance criteria

- [ ] AC1: `separate_audio_video(audio_stream = 1)` compiles the audio command
      containing the literal `-map 0:a:1` and the video command containing the
      literal `-map 0:v`; `audio_stream = 1L` compiles the identical pair. With
      `audio_stream` absent or `NULL`, both compiled strings equal the pre-change
      baseline T1 records from commit b548902 — the every-track `-map 0:a` form
      is unchanged.
- [ ] AC2: On the executing path, when the caller named no `audio_stream`, the
      audio command exits non-zero, and ffprobe counts more than one audio track
      in `infile`, `separate_audio_video()` aborts with a message stating that
      count, naming `audio_stream` as the way to take one track, and naming
      Matroska (`.mka`) as a container that accepts several — each clause true of
      *any* non-zero exit, not only a muxer refusal (M38's lesson). The abort
      carries FFmpeg's exit status. When the caller named a track, with one audio
      track, with ffprobe absent, or when the probe fails, the abort is the one
      `ffm_run()` raises today, unchanged in text and condition class.
- [ ] AC3: Under `run = FALSE` neither verb invokes a binary. Evidence: a test
      stubbing `find_ffmpeg()` and `find_ffprobe()` to abort — masking `PATH` is
      not enough, since `find_program()` falls back to a stored `rappdirs`
      config location — which compiles every documented call of both verbs,
      including each ungated roxygen `@examples` line, without error.
- [ ] AC4: `separate_audio_video_batch(audio_stream = n)` applies `n` to every
      row's audio command and to no row's video command; an `audio_stream`
      column overrides the argument per row, and an `NA` cell keeps that row on
      every audio track. A failing row still records `success = FALSE` without
      aborting the batch, and the batch emits ONE warning naming every failed
      audio row that named no track, carrying AC2's text (M44's aggregation, so
      a large jobs table cannot bury it under R's warning collapse).
- [ ] AC5: A `cairn/DECISIONS.md` entry extends D023, recording that
      `audio_stream = NULL` means every audio track on this verb against the
      first audio track on `extract_audio()`/`convert_audio()`. It quotes the
      D023 bullet it departs from, states why `0:a` and `0:a:<n>` are different
      questions, and records that this milestone absorbed `separate_audio_video`
      from the pass-through-selector candidate.
- [ ] AC6: `devtools::document()` leaves no diff once the milestone's roxygen
      changes are documented; `devtools::test()` and `devtools::check()` report
      0 errors and 0 warnings on a machine carrying ffmpeg and ffprobe, so AC2's
      and AC4's execution tests run rather than skip. NEWS records the argument,
      the abort, and the batch warning.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T5
- AC3 → T5, T6
- AC4 → T3, T5
- AC5 → T4
- AC6 → T6

## Tasks

- [x] T1: Record the pre-change compiled strings from b548902 as AC1's baseline,
      then add `audio_stream` to `separate_audio_video()` — front-door
      `check_number_whole(min = 0, allow_null = TRUE)`, threaded through
      `separate_stream_pipeline()` (`R/ffmpeg.R:379`) to the audio branch only;
      `NULL` keeps `0:a`, `n` compiles `0:a:<n>`. *(RB tripwire: irreversible-api)*
- [x] T2: The enriched abort in the verb, not in `ffm_run()` — keeping the
      Layer-2 argument name out of the engine (IP1). Wrap the audio `ffm_run()`
      (`R/ffmpeg.R:495`); on a non-zero exit probe `infile`'s audio-stream count
      and re-raise. Fall through to today's abort when the caller named a track,
      when the count is 1, when ffprobe is absent, or when the probe fails.
- [x] T3: The batch sibling — `audio_stream` argument plus per-row column
      through the 2N reshape, audio rows only, `NA` meaning every track via a
      parameterized `check_batch_audio_col(na_means = )`. After `ffm_batch()`
      returns, emit one aggregated warning naming every failed audio row that
      named no track, carrying T2's text.
- [ ] T4: The D-entry extending D023; surface it at the implement question gate
      before code lands.
- [ ] T5: Tests for AC1–AC4 — the compile pins for both `audio_stream`
      spellings and the unchanged default; the three-track execution test on
      `make_multitrack_video()` into `.aac`; the binary-free stub test; the batch
      argument/column/`NA` matrix and the per-row warning. Prove the AC2 test
      discriminates by making the enrichment unconditional — it must go red
      (M39's lesson).
- [ ] T6: Docs — `@param audio_stream` on both verbs and the batch
      `@param jobs` column enumeration (M39's lesson); NEWS; `document()`,
      `test()`, `check()` with the binaries present.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose keeping every audio track by default plus a selector over defaulting to the first track (the convert_audio hotfix's shape), because `.mka`/`.m4a` callers measurably receive all three tracks today and M44's drop warning does not cover this verb, so that default would drop tracks silently; falsified by a report that the every-track default is what a caller wanted narrowed, or by the divergent `NULL` meaning confusing a caller who uses both verb families.
- 2026-07-30: plan gate chose absorbing `separate_audio_video` alone from the `audio_stream`-carry candidate over promoting the whole five-verb row, because the row's promotion condition has not fired and the full carry trips the sizing tripwire; falsified by a second pass-through verb needing the selector before that row is promoted, which would make two milestones of one.
- 2026-07-30: plan gate chose the enriched abort in the verb with a post-`ffm_batch()` per-row warning over placing it in `ffm_run()`, because the Layer-1 abort would carry the Layer-2 name `audio_stream` (IP1), and over a scalar-only message, because scalar/batch divergence is a defect this repo has fixed twice (M19, M35); falsified by the per-row probe cost making a large failed batch slow to report.
- 2026-07-30: criteria audit ([O], fresh context, authored none of the criteria) returned findings on all six. Fixed before the gate: AC1's "byte-identical to master's" pinned to literal tokens and a recorded baseline; AC2's "FFmpeg's error text stays visible" dropped as unverifiable (`run_program(stderr = "")` streams to console, never into the condition) and its hint reworded to hold on any non-zero exit; AC3 scoped to these two verbs (package-wide it contradicts D013), its already-gated vignette clause dropped, and `PATH`-masking replaced by stubbing the finders past `find_program()`'s `rappdirs` fallback; AC6 changed to no-diff *after* documenting; AC6 now requires the binaries present so AC2/AC4 evidence cannot come from skipped tests. Routed to the gate: AC4's argument-plus-column ambiguity, AC2's enrichment site, and D023's split of selector from abort.
- 2026-07-30: implement started; branch `m45-separate-av-multitrack` cut from master at e885859.
- 2026-07-30: implement gate kept the plan's `audio_stream` name with its every-track `NULL` (over a D023-uniform first-track default, which would silently narrow the `.mka`/`.m4a` callers who receive all tracks today, and over a second argument name for the same counting base); irreversible-api tripwire offered escalation and it was declined.
- 2026-07-30: AMENDMENT (substantive, gated) — AC2 and T2 now fall through to today's plain `ffm_run()` abort when the caller NAMED a track: with `0:a:<n>` mapped the failure is not a multi-track refusal, so "name a track with `audio_stream`" would be false under the branch that fired it (M38's twice-learned lesson). AC4/T3 amended from "warns once per failed audio row" to ONE aggregated warning naming every failed no-track audio row, matching M44's aggregation so R's 50-warning collapse cannot bury a large batch's message.
- 2026-07-30: T1 done — `audio_stream` on `separate_audio_video()`, threaded to the audio branch of `separate_stream_pipeline()` only (the video call is never passed the value, so the video map cannot narrow by mistake). `audio_stream_map()` gained a `null_map` parameter rather than a second helper: one guard site keeps the `check_number_whole` wording identical across verb families, and `null_map = "0:a"` is what makes this verb's `NULL` mean every track (the `check_batch_audio_col(na_means=)` shape from M43/M40). AC1's baseline recorded verbatim in the new test file's header, provenance `b548902`, with `git diff b548902 HEAD -- R/ffmpeg.R` confirming no separation code moved in between. Minor refinement: AC1's compile tests were written here with the code rather than deferred to T5 (tests-first), so T5 now carries AC2–AC4 only. `document()` clean, `test()` 0 failures / 2751 passing (the 4 warnings are M44's drop diagnostic in pre-existing tests, unchanged).
- 2026-07-30: T2 done — `run_separation_audio()` wraps the audio `ffm_run()`; on a non-zero exit it probes the input's audio-stream count and re-raises with the count, `audio_stream`, and `.mka`/`.m4a` as the two ways out, chaining the original as `parent` so FFmpeg's status and failing command survive. A non-zero EXIT is told apart from every other failure (missing binary, unreadable path) by parsing ffm_run()'s own status wording, with a test pinning that coupling so a reword there fails loudly instead of silently retiring the branch. Mutation probe (M39): deleting the named-track early return reddens the fall-through test; deleting the fail-open line reddens the single-track and ffprobe-absent tests — two distinct failure sets, not the identical one M44 flagged as the tell for a bad probe. Baseline was copied aside rather than restored with `git checkout` (M44's trap). D024 adoption recorded as M45-D1. `test()` 0 failures / 2769 passing.
- 2026-07-30: T3 done — `audio_stream` on `separate_audio_video_batch()`: batch-wide argument plus per-row column, materialized into the 2N reshape on each input's AUDIO row only (video rows carry NA, and the video branch never reads the value — two independent reasons a video map cannot narrow). `NA` cell keeps that row on every track via `check_batch_audio_col(na_means = "keep every audio track")`. After `ffm_batch()` returns, `warn_failed_separation_batch()` probes only the failed no-track audio rows and emits ONE warning naming each affected INPUT row (not the 2N result row) with the scalar abort's three clauses. Discrimination probe (M39): making the fan-out ignore the argument reddens 4 tests. `@param jobs` column enumeration updated in the same task rather than at T6 (M39's lesson), plus a Failed-audio-outputs docs section. `document()` clean, `test()` 0 failures / 2797 passing.

## Decisions

### M45-D1 — The failed-separation probe adopts D024's licence rather than stretching it (2026-07-30)

D024 asks a verb adopting its diagnostic probe to record the adoption in its own
milestone's decision log, and a probe stretching any of its four conditions to
take a new D-entry. `run_separation_audio()` adopts; it stretches nothing.

- (i) The outcome affects nothing but which condition is signalled. The probe
  runs only after FFmpeg has already exited non-zero, so the call aborts under
  every outcome — ran, skipped, succeeded, failed. What moves is the abort's
  wording and class, never whether there is one.
- (ii) It fails open. No parsed exit status, no probe answer, or a single-track
  input all re-raise the original condition object, so `ffm_run()`'s message,
  class and trace stay the ones today's caller sees.
- (iii) It never runs on the `run = FALSE` path: it sits inside `if (run)`,
  behind a failure that cannot occur without a run.
- (iv) It never runs from `ffm_compile()` or any builder it walks.

D024's third exclusion — "a probe that decides whether execution proceeds" — is
the one worth naming, because this probe reads close to it. It is not one: an
abort gate probes to decide *whether* to stop, and this probe runs when the stop
is already certain. Execution has ended under either branch.

D024 also anticipated this verb by name, ruling out "a predicate about narrowing
a multi-track input" as its licence condition precisely so that M45 — where
`NULL` means every track and nothing narrows by default — would not read itself
as excluded from diagnostics. This entry is that reading applied.

### M45-D2 — The batch's failed-row probe adopts D024 too, and runs after the fan-out (2026-07-30)

`warn_failed_separation_batch()` adopts D024 on the same four conditions M45-D1
records for the scalar verb: the outcome moves nothing but whether a warning is
signalled, an unanswerable count is skipped in silence, it sits inside
`if (isTRUE(run))`, and no builder reaches it.

Where it differs from M44's sibling diagnostic is *when* it runs. M44 probes
**before** `ffm_batch()`, deliberately, so the warning lands while the caller can
still stop the fan-out. This one probes **after**, because its occasion is a row
that *failed*, which is not knowable until the row runs. The consequence is the
better half of the trade: a batch whose rows all succeed spawns no FFprobe at
all, where M44's up-front probe pays for every unique input regardless (the cost
the M44 review logged as F4). Probing before would mean probing rows that were
never going to fail.

The engine is untouched either way — `ffm_batch()`'s signature is unchanged, the
same hook D024/RR02 Q3 declined to add.

## Review
