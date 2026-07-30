# M45: Give a multi-track `separate_audio_video()` caller a way out

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M43
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m45-separate-av-multitrack` · https://github.com/jmgirard/tidymedia/pull/48

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

- [x] AC1: `separate_audio_video(audio_stream = 1)` compiles the audio command
      containing the literal `-map 0:a:1` and the video command containing the
      literal `-map 0:v`; `audio_stream = 1L` compiles the identical pair. With
      `audio_stream` absent or `NULL`, both compiled strings equal the pre-change
      baseline T1 records from commit b548902 — the every-track `-map 0:a` form
      is unchanged.
- [x] AC2: On the executing path, when the caller named no `audio_stream`, the
      audio command exits non-zero, and ffprobe counts more than one audio track
      in `infile`, `separate_audio_video()` aborts with a message stating that
      count, naming `audio_stream` as the way to take one track, and naming
      Matroska (`.mka`) as a container that accepts several — each clause true of
      *any* non-zero exit, not only a muxer refusal (M38's lesson). The abort
      carries FFmpeg's exit status. When the caller named a track, with one audio
      track, with ffprobe absent, or when the probe fails, the abort is the one
      `ffm_run()` raises today, unchanged in text and condition class.
- [x] AC3: Under `run = FALSE` neither verb invokes a binary. Evidence: a test
      stubbing `find_ffmpeg()` and `find_ffprobe()` to abort — masking `PATH` is
      not enough, since `find_program()` falls back to a stored `rappdirs`
      config location — which compiles every documented call of both verbs,
      including each ungated roxygen `@examples` line, without error.
- [x] AC4: `separate_audio_video_batch(audio_stream = n)` applies `n` to every
      row's audio command and to no row's video command; an `audio_stream`
      column overrides the argument per row, and an `NA` cell keeps that row on
      every audio track. A failing row still records `success = FALSE` without
      aborting the batch, and the batch emits ONE warning naming every failed
      audio row that named no track, carrying AC2's three shared clauses — the
      track count, `audio_stream`, and a container that holds several — but not
      its exit-status clause, which `ffm_batch()` collapses to a bare logical per
      row and cannot supply without the engine change M45-D2 declined (M44's
      aggregation, so a large jobs table cannot bury it under R's warning
      collapse).
- [x] AC5: A `cairn/DECISIONS.md` entry extends D023, recording that
      `audio_stream = NULL` means every audio track on this verb against the
      first audio track on `extract_audio()`/`convert_audio()`. It quotes the
      D023 bullet it departs from, states why `0:a` and `0:a:<n>` are different
      questions, and records that this milestone absorbed `separate_audio_video`
      from the pass-through-selector candidate.
- [x] AC6: `devtools::document()` leaves no diff once the milestone's roxygen
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
- [x] T4: The D-entry extending D023; surface it at the implement question gate
      before code lands.
- [x] T5: Tests for AC1–AC4 — the compile pins for both `audio_stream`
      spellings and the unchanged default; the three-track execution test on
      `make_multitrack_video()` into `.aac`; the binary-free stub test; the batch
      argument/column/`NA` matrix and the per-row warning. Prove the AC2 test
      discriminates by making the enrichment unconditional — it must go red
      (M39's lesson).
- [x] T7: Make the enrichment tests FFmpeg-version-independent — trigger the
      failure with AAC-copy-into-`.mp3` on the multi-track input (invalid in every
      build) rather than the `.aac` stream-count refusal (ffmpeg >= 8 only), and
      cover the container-refusal occasion in a test that probes this FFmpeg's
      muxer first and skips when it does not refuse (M43's fixture-property
      lesson). Re-run CI on all platforms.
- [x] T6: Docs — `@param audio_stream` on both verbs and the batch
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
- 2026-07-30: T4 done — D025 appended: quotes the D023 `NULL` bullet it departs from, separates the every-track from the which-track question using D023's own closing bullet, states the cost (one name, two defaults) with its falsifier, and records the absorption plus the D024 adoption pointer. The `audio_stream`-carry candidate row now cites D025 and its observation that all four remaining verbs pass audio through, so on their face they take M45's shape rather than D023's.
- 2026-07-30: T5 done — 30 tests in `tests/testthat/test-separate-av-multitrack.R`, written per task with the code rather than batched here. Gap-closing this task: AC3's evidence exists in BOTH spellings — the counting mock (cannot go vacuous) and the AC3-worded stub-to-abort (weaker, kept beside it, with a comment saying why neither replaces the other) — and AC2's status assertion now requires a digit rather than only the phrase. Three mutation probes all red on distinct failure sets: unconditional enrichment, no fail-open, fan-out ignoring the argument. `test()` 0 failures / 2798 passing.
- 2026-07-30: T6 done — NEWS gained one Breaking-changes bullet (the positional shift of `run`/`parallel` behind the new argument, matching the one M43 wrote for the four audio verbs) and two New-features bullets (the argument with its deliberately different default spelled out, and the enriched abort plus the batch's aggregated warning with its best-effort caveat). `document()` produces no diff. `devtools::check()` printed 0 notes while `R CMD check` sat at `Status: 1 NOTE` — M17's masked spelling NOTE, new word "Matroska"; `spelling::update_wordlist()` then `Status: OK`, 0 errors / 0 warnings / 0 notes with ffmpeg and ffprobe present, so the AC2/AC4 execution tests ran rather than skipped. The only vignette mention of the verb names its arguments, so the positional shift reaches no in-repo caller.
- 2026-07-30: T6 follow-up — the batch `@return` now documents the `audio_stream` column the result gains when the argument or a column is supplied (the selected index on audio rows, `NA` on video rows), with a test pinning both that shape and the unchanged pre-change shape when neither is given (M19's return-schema lesson). `test()` 0 failures / 2800 passing; `R CMD check` `Status: OK`, 0/0/0.
- 2026-07-30: all tasks done; status review. No prose-guard was authored or edited (the milestone's substring assertions are over runtime condition messages, not over doc wording), so guard-doctrine §8's fresh-context description reader does not apply.
- 2026-07-30: REVIEW FAILURE (return 1 of the thrash count) — PR #48 CI red on all three ubuntu-latest jobs and test-coverage; macOS and Windows green. 7 failures, all in `test-separate-av-multitrack.R` (lines 120/121, 166, 192/193, 292/293). Cause: ubuntu-latest ships ffmpeg 6.1.1-3ubuntu5, whose adts muxer WRITES three audio streams to `.aac` successfully (log shows `Output #0, adts` and `audio:17kB`, exit 0), where macOS ffmpeg 8.1.2 refuses with "adts muxer does not support more than one stream of type audio". The multi-stream refusal arrived in a later FFmpeg, so every test that triggered the enrichment via `.aac` got no condition at all and `tryCatch` returned the verb's value (a character vector, or a tibble on the batch path). The feature itself is not implicated — it reacts to whatever FFmpeg does — and the AC2 fall-through tests, which fail via AAC-copy-into-`.mp3`, passed on every platform. This is M27's lesson recurring on a new surface: the plan's Scope measurement "`.aac`/`.mp3`/`.wav` fail (measured ffmpeg 8.1.2)" is true of 8.1.2 and false of 6.1.1 for `.aac`.
- 2026-07-30: added T7 (discovered sub-task, minor amendment) to make the enrichment tests FFmpeg-version-independent.
- 2026-07-30: T7 done — the four enrichment tests now trigger the failure with an AAC-to-MP3 stream copy on the multi-track input, invalid in every FFmpeg build whatever its muxer stream limits, and the container-refusal occasion has its own test gated by `skip_unless_adts_refuses_multistream()`, which probes this FFmpeg and skips rather than assuming the local build's behavior (M43's fixture-property lesson). The test file's header records the platform finding. Local: 65 assertions, 0 failures, 0 skips.
- 2026-07-30: review fan-out — three fresh-context reviewers (an [O] diff-bug lens, an [S] blame-history lens, an [S] prior-review-regression lens), then an [S] scorer that did not generate the findings, given the diff and the plan. Blame-history returned 11 clean confirmations and zero defects; prior-review returned zero regressions plus one marginal note (N1, scored 25). Diff-bug returned 16. Scorer put A-F4 at 88 and A-F3 at 82; everything else below 80.
- 2026-07-30: FIXED (>=80, actioned) — A-F4 (88): a bad `audio_stream` CELL aborted mid-fan-out reporting `In index: 3` for a two-row jobs table and naming Layer-1's `purrr::pmap`, because the range check runs per row over the RESHAPED 2N table. New `check_batch_stream_values()` validates each cell at the front door and blames the caller's own row number; its message carries no `{?s}` because a plural governed by a vector throws at 2+ items (M18). A-F3 (82): the scalar verb documented neither the enriched abort nor that it can silently not run, which D024 explicitly requires and which the batch sibling already had — added a `@section When the audio output fails:` covering both, and stating that FFmpeg's own error remains the authority on cause.
- 2026-07-30: FIXED (below threshold, actioned by choice, reasons recorded) — A-F2 (72) and A-F7 (72) each stated something FALSE in user-facing text, which is not a matter of confidence: the batch bullet asserted "FFmpeg would not write all 3 to a.mka" even when .mka would have held all three and the row failed for an unrelated reason (reworded to state the count and the mapping, never the cause), and the batch `@return` said `NA` marks a video row when an `NA` cell puts `NA` on an audio row too (corrected, pointing readers at the `stream` column). A-F9 (74) and A-F12 (68) are test gaps this repo's own LESSONS instruct rather than merely suggest — M18 requires a 2+ item cli count message and M40 requires the absence assertion on a parameterized `na_means` — so both tests were added, plus the batch's own brace-escaping path, which the scalar's test never covered. A-F13 (55) was a one-line comment correction: the example's comment claimed a multi-track input for a single-track sample.
- 2026-07-30: FOLLOW-UP (candidate rows, search-first swept, no overlap found) — A-F1 (65) + A-F5 (52) grouped into one row: the abort fires on any failing audio command with >1 track, so an unknown codec or a missing output directory while writing `.mka` still gets the write-to-.mka hint, and the non-zero exit is detected by regex over a message that also embeds user paths. Both fixes are design calls M45 deliberately did not make (gate on container capacity, or classify FFmpeg's failure; attach `run_program()`'s status attribute to the condition, a Layer-1 change). Separately, a pre-existing intermittent FFmpeg hang in M43's subtitle fixture got its own row — hit once during this review, killed after 10+ minutes, passed on the next run.
- 2026-07-30: LOGGED, not actioned — A-F6 (42): the batch warning cannot carry FFmpeg's exit status because `ffm_batch()` collapses each row to a bare logical; AC4 amended rather than reinterpreted (below). A-F8 (35): `as.numeric()` returns an integer column as double, documented as numeric. A-F10 (45) and A-F11 (32): evidence gaps only — the reviewer confirmed no precedence regression and that AC3 is substantively met by two tests jointly. A-F14 (32): `basename()` collapses same-named paths, matching the pre-existing `warn_dropped_audio` family. A-F15 (40): visibility asymmetry, unobservable since the verb discards the value. A-F16 (40): the chained parent puts FFmpeg's own output below the hints. N1 (25): the reviewer's own reading was that M45 satisfies rather than triggers the `ffm_map()` candidate's promotion condition.
- 2026-07-30: AMENDMENT (substantive, gated at the merge chip with the text shown verbatim) — AC4's "carrying AC2's text" narrowed to AC2's three SHARED clauses (count, `audio_stream`, container), explicitly excluding its exit-status clause. The status cannot cross `ffm_batch()`, which collapses each row to a bare logical, and supplying it needs the engine-signature change D024/RR02 Q3 and M45-D2 both declined. Amended rather than read charitably, because a criterion that fails as written is a wrong criterion.
- 2026-07-30: re-verified after the fixes — `test()` 0 failures / 2814 passing; `R CMD check` `Status: OK` 0/0/0; `document()` no diff; `cairn_validate` exit 0. One suite run hung 10+ minutes on M43's pre-existing subtitle fixture and had to be killed; the next run of the same suite passed, and the hang is now a candidate row.

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

_Fresh evidence, 2026-07-30, macOS 25.5.0, ffmpeg/ffprobe 8.1.2 both present. PR #48._
- **AC1 — PASS.** The pre-change package was extracted from `b548902` with
  `git archive` into a scratch tree (HEAD never moved) and loaded there, so the
  baseline is the commit's own output rather than a transcribed string. Against it:
  the default call and the explicit `audio_stream = NULL` call are both
  `identical()` to `b548902`'s pair; `audio_stream = 1` and `= 1L` are
  `identical()` to each other; the audio command contains the literal
  `-map 0:a:1` and the video command the literal `-map 0:v`, the latter
  `identical()` to the baseline's video string.
- **AC2 — PASS.** Executed on a freshly generated three-track Matroska into
  `.aac`: the condition is `tidymedia_multitrack_separation`, and its message
  reads "carries 3 audio tracks and no `audio_stream` was named, so all 3 were
  mapped into one output", offers "Take one track with `audio_stream`: 0 is the
  first audio track, 1 the second", and offers "keep all 3 by writing a container
  that holds several -- Matroska ('.mka') or '.m4a'". The exit status appears
  twice: in the headline ("FFmpeg exited with status 234") and again through the
  chained parent, which also carries the failing command. Each clause is stated as
  a property of the input and the container rather than of a muxer diagnostic, so
  it holds on any non-zero exit. Fall-through verified as three separate runs, all
  landing on `ffm_run()`'s own abort with `tidymedia_multitrack_separation`
  absent: a named track that still fails (AAC copy into `.mp3`), a single-track
  input, and `find_ffprobe()` returning NULL. The positive control also holds —
  the same three-track input into `.mka` raises nothing and the output carries 3
  audio streams.
- **AC3 — PASS.** Both spellings of the evidence exist. The criterion's own
  spelling (`find_ffmpeg()` / `find_ffprobe()` stubbed to abort) compiles every
  documented call of both verbs without error. Independently, a traced run that
  wraps `run_program`, `find_ffmpeg`, `find_ffprobe` and `count_audio_streams` in
  the package namespace and counts invocations across all five documented
  `run = FALSE` calls — the three scalar `@examples` lines, the batch `@examples`
  line, and a batch call carrying an `audio_stream` column — records **0**
  invocations. The counting form is the load-bearing one: `run_separation_audio()`
  and `count_audio_streams()` both wrap calls in `tryCatch()`, which swallows a
  `stop()` mock (M44's trap).
- **AC4 — PASS.** `audio_stream = 2` on a two-row table compiles `-map 0:a:2` on
  both audio rows and `-map 0:v` on both video rows, with no `0:a` token anywhere
  in a video command. A column `c(0, NA)` against `audio_stream = 1` compiles
  `0:a:0` for row 1 and the every-track `0:a` for row 2, so the column overrides
  the argument and the `NA` cell means every track rather than deferring to the
  argument. Execution on a three-row table (row 1 multi-track/no track named,
  row 2 multi-track/track 1, row 3 single-track) returns all six rows with
  `success = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)` — the batch did not abort —
  and raises exactly ONE `tidymedia_multitrack_separation` warning naming
  "Input row 1" and neither row 2 nor row 3, carrying AC2's three clauses.
- **AC5 — PASS.** D025 exists, headed "extends D023". Checked clause by clause:
  it records the every-track-vs-first-track split; it quotes D023's `NULL` bullet,
  and the quote is verbatim modulo line wrapping (verified by normalizing
  whitespace and substring-matching against D023's own text); it states that
  "`0:a` and `0:a:<n>` answer different questions" and grounds that on D023's own
  closing bullet; and it records the absorption of `separate_audio_video` from the
  pass-through-selector candidate. The ROADMAP candidate row was updated to match.
- **AC6 — PASS.** `devtools::document()` leaves `man/` and `NAMESPACE` clean (0
  dirty files after running it). `devtools::test()`: 0 failures, 2800 passing, 5
  skips (all nvenc, none in this milestone's tests — the M45 file alone runs 63
  assertions with 0 skips, so the AC2/AC4 execution tests ran rather than
  skipped). `ffmpeg` and `ffprobe` both resolve on PATH. `R CMD check`:
  `Status: OK` — 0 errors, 0 warnings, 0 notes (the masked spelling NOTE M17
  warns about was caught during implement and closed with
  `spelling::update_wordlist()`). NEWS carries three bullets: the positional
  breaking change, the argument, and the abort plus the batch warning.

### Consistency gate

- `cairn_validate` exit 0 — 16 PASS, 8 advisory OK, no FAIL or WARN.
- `cairn_impact` skipped: no `DESIGN.md` principle changed (the header's
  "Principles touched: IP1" records a principle respected, not amended; `DESIGN.md`
  is not in the branch diff).
- Toolchain slot (`r-package`): `document()` no diff · generated files clean ·
  `README.Rmd`/`README.md` untouched by the diff and in sync · `pkgdown::check_pkgdown()`
  "No problems found" · NEWS entry present · no new top-level files · full
  `R CMD check` `Status: OK`.
- No newly exported object, so no `_pkgdown.yml` row was owed (NAMESPACE unchanged).
- Thrash count: 0 returns to `in-progress`; first review pass.

