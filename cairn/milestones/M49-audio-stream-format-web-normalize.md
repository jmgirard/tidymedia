# M49: Finish D026 on `format_for_web()` and `normalize_audio()`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m49-audio-stream-format-web-normalize` · https://github.com/jmgirard/tidymedia/pull/52

## Goal

Close D026's last gap by making `format_for_web()` and `normalize_audio()`
state which audio tracks they carry instead of inheriting FFmpeg's
DEFAULT-disposition heuristic.

## Scope

**In:** `audio_stream` on `format_for_web()` / `format_for_web_batch()` under
D026's every-track `NULL` (`-map 0:v? -map 0:a?`), and on `normalize_audio()` /
`normalize_audio_batch()` under a **first-track** `NULL` spelled `0:a:0?` —
carried onto the two-pass analysis command as well as the correction one, with
the video half omitted when the output names an audio-only container (added
2026-07-31 at review send-back). The
D-entry recording that split and its measurement; the map-count invariant
table; roxygen, NEWS, `inst/WORDLIST`.

**Out:** refreshing the sibling-verb enumeration inside the fourteen existing
`@param audio_stream` blocks → M51. Per-track two-pass loudnorm (one measured
set per mapped track, which is what an every-track `NULL` here would require,
and which needs per-stream filter options the linear builder has no slot for) →
new candidate row. `subtitle_stream` / `video_stream` selectors → the standing
candidate row. Quoting the emitted specifiers → M50.

## Acceptance criteria

_Compressed in one pass 2026-07-31 for the cap; no criterion's substance
changed. AC2/AC4/AC5/AC8 amended at the second review send-back — see the work
log for what changed and why._

- [x] AC1 `format_for_web()` / `_batch` accept `audio_stream`; compiled command
      carries `-map 0:v? -map 0:a?` under `NULL`, `-map 0:v? -map 0:a:<n>` when
      named. Compile level, both entry points, from the argument and from a
      column whose `NA` cell keeps that row on every track.
- [x] AC2 `normalize_audio()` / `_batch` accept `audio_stream`; the correction
      command carries exactly one map — `-map 0:a:0` under `NULL`,
      `-map 0:a:<n>` when named — **never a video map, never a `-codec:v`**, and
      does not vary with the output container. Compile level, both entry points,
      over audio and video output extensions, and from a column whose `NA` cell
      keeps that row on the first track.
- [x] AC3 The analysis command names the same audio track the correction command
      normalizes, under `NULL` and a named track. Asserted on
      `loudnorm_analysis_pipeline()`'s compiled output directly, never through a
      verb call: D013 runs that pass before `run` is consulted.
- [x] AC4 Execution on a 3-audio-track fixture whose DEFAULT is asserted to sit
      on track 2 first (M43's fixture-took check): `format_for_web()` carries all
      three, `normalize_audio()` carries track 0, against T1's recorded baseline
      rather than a re-run of the old code. Two error checks: `audio_stream = 9`
      on a 3-track input, and an input with **no audio**, are both FFmpeg errors
      rather than R ones or silent video copies. `skip_if` FFmpeg is absent.
- [ ] AC5 The map-count invariant test (`tests/testthat/test-ffm.R`) keys rows on
      *compiled commands* rather than verbs — branches compiling different
      commands get a row each — so the analysis and correction commands are
      separate rows, and those two are `normalize_audio()`'s only compiled
      commands. Its rule statement describes the verbs absent from the table
      accurately.
- [x] AC6 A `cairn/DECISIONS.md` entry records the split and its measured reason:
      under `-map 0:a?` the analysis pass prints one JSON block per mapped track
      while `classify_loudnorm_output()` reads `hit[[1]]`, so every mapped track
      would be corrected with track 0's measurements, silently. It cites T1's
      measurement, not the plan's.
- [x] AC7 `devtools::document()` no diff, `devtools::test()` clean,
      `devtools::check()` 0 errors / 0 warnings; NEWS describes the changes in
      user-facing terms, names the capability `normalize_audio()` loses, and
      names the two argument-surface breaks (`run` moved position in four
      signatures; `audio` is no longer an unambiguous partial match).
- [x] AC8 The output container does not affect whether the call works. Execution
      on a video input across seventeen extensions spanning audio and video
      containers — including the six an enumerated list missed (`.w64`, `.mpa`,
      `.voc`, `.sbc`, `.latm`, `.adts`) — each exits 0, writes a non-empty file,
      and carries one audio stream and no video. `.wma` is excluded, recorded as
      failing on master too. `skip_if` FFmpeg is absent.

## Coverage

- AC1 → T2, T6
- AC2 → T3, T6, T13
- AC3 → T4
- AC4 → T1, T7
- AC5 → T5
- AC6 → T1, T9
- AC7 → T8, T9, T14, T15
- AC8 → T11, T13

## Tasks

_Compressed in one pass 2026-07-31; the work log carries what each one did._

- [x] T1 Commit the pre-change evidence before any source edit, so AC4 and AC6
      have a reference the branch cannot destroy (M44's lesson).
- [x] T2 `audio_stream` through `format_for_web_pipeline()` /
      `format_for_web()` / `format_for_web_batch()`, incl. the batch front-door
      guard and column support M47 established.
- [x] T3 A first-track variant of the map pair, wired into
      `normalize_audio_pipeline()` / `normalize_audio()` /
      `normalize_audio_batch()`.
- [x] T4 The same selection into `loudnorm_analysis_pipeline()` and its two
      callers.
- [x] T5 Rewrite the map-count invariant table onto compiled commands.
- [x] T6 Re-baseline the exact-command assertions this breaks, including the M16
      characterization baseline (overwritten deliberately).
- [x] T7 Execution tests against T1's baseline, plus the error checks.
- [x] T8 Roxygen on the four new entry points, the `@param jobs` column
      enumerations, NEWS, `inst/WORDLIST`, `devtools::document()`.
- [x] T9 D028; the per-track-two-pass candidate row; verify slot and
      `devtools::check()`.
- [x] T10 (superseded by T13) An audio-only-container predicate gating the video
      map.
- [x] T11 Execution evidence across the container matrix.
- [x] T12 (superseded by T14) NEWS for the container rule and the two
      argument-surface breaks; D029.
- [x] T13 Drop the video map, the container predicate and the dead
      `-codec:v copy`; drop the trailing `?` from both passes. Tests: container
      independence, no video map ever, a no-audio input erroring.
- [x] T14 Docs for the audio-only contract (`@description`, `@param outfile`,
      `@param audio_stream` on both verbs), the NEWS rewrite, D030, and the
      ROADMAP candidate row for the capability this removes.
- [x] T15 Re-baseline what the `-codec:v copy` removal breaks; verify slot and
      `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose a first-track `NULL` for `normalize_audio()` over D026's every-track `NULL` because an every-track map makes the two-pass analysis print one JSON block per track while the parser reads only the first, silently correcting every track with track 0's measurements; falsified by a per-stream filter-options seam that lets one measured set attach to each mapped track, which would make uniformity affordable again.
- 2026-07-31: plan gate chose one milestone over splitting `format_for_web` and `normalize_audio` apart because the D-entry records a single split decision across both verbs and half of it is incoherent alone; falsified by the task count crossing the ~10 tripwire during implementation.
- 2026-07-31: plan measurements (ffmpeg 8.1.2, macOS; a 3-audio-track `.mkv` with DEFAULT disposition moved to track 2 and languages eng/deu/fra): `format_for_web()` and `normalize_audio()` each carry only `fra` today; `-map 0:v? -map 0:a?` carries all three on both; `-map 0:v? -map 0:a:0?` carries `eng`; the analysis pass prints 3 JSON blocks under `0:a?` and 1 under `0:a:0?` or a named track; on a video-only input `0:a:0` exits 234 while `0:a:0?` and no-map both exit 0. T1 re-records these on the branch.
- 2026-07-31: T1 baseline recorded on-branch before any source edit (ffmpeg 8.1.2, macOS; 3-audio-track `.mkv`, languages eng/spa/fra, DEFAULT flags `0,0,1` asserted before any result was read). Today `format_for_web()` and `normalize_audio()` each compile ZERO `-map` arguments and each output carries only `fra` — the DEFAULT track, not the first.
- 2026-07-31: T1 baseline, analysis pass: today's `loudnorm_analysis_pipeline()` compiles no `-map` either, and FFmpeg's implicit selection sends stream `#0:3` (`fra`) to `loudnorm`, printing 1 JSON block — so analysis and correction currently agree by accident, both landing on whichever track carries DEFAULT. Under `-map 0:a?` the same pipeline prints **3** blocks (one per mapped track) while `classify_loudnorm_output()` reads `hit[[1]]` (`R/loudnorm_two_pass.R:48`) — AC6's measured reason for the first-track carve-out. Under `0:a:0?` or a named track it prints 1.
- 2026-07-31: T1 baseline, no-regression references: `normalize_audio()` on a video-only `.mp4` exits 0 today; on the analysis pass an audio-only `-map 0:a:0?` also exits 0 there (empty null-sink output is legal), while a named `-map 0:a:1` exits 234.
- 2026-07-31: implement gate chose an AUDIO-ONLY map on the analysis pass (`0:a:0?` / `0:a:<n>`, no `0:v?`) over mirroring the correction command's pair, because that pass writes to `-f null` and has no output for a video selection to describe; measured indistinguishable on exit code and block count for every input tried, and 0.356 s vs 0.372 s per run on a 20 s 720p file.
- 2026-07-31: T2 done — `format_for_web()` / `format_for_web_batch()` carry `audio_stream` under D026's every-track rule; tests in a new `test-audio-stream-format-web.R` (M49's tests are split by verb, one file each, because the two verbs take different `NULL` rules and a shared file could not be green until both landed). `devtools::test()` clean, 0 failures. No existing exact-command test named this verb's command, so T6's re-baselining list is untouched by this task.
- 2026-07-31: minor plan refinement — T5's map-count table is being updated per verb as each lands rather than rewritten once at the end, so the suite stays green at every checkpoint; `format_for_web` moved 0 → 2 here and the full rewrite onto compiled commands still happens in T5.
- 2026-07-31: T3–T7 landed in one checkpoint (minor plan amendment). T3 and T4 are two halves of one contract — AC3 is the assertion that the analysis and correction commands name the same track — so a checkpoint carrying only one of them would have shipped a normalize path whose two passes disagreed. T5 and T6 then had to ride along: the suite cannot be green until the map-count table and the exact-command baselines match the new commands.
- 2026-07-31: T3 — `pass_through_maps()` gained a `null_map` parameter rather than a sibling helper being added beside it (plan wording said "beside"). Same shape `audio_stream_map()` already had for the same reason, and it keeps one comment block explaining both spellings instead of two that can drift.
- 2026-07-31: T3 — discovered sub-task, done: `normalize_audio_batch()` also gets `check_batch_stream_values()`, which the pass-through batch verbs do not need. Its two-pass path corrects `jobs[!silent, ]`, so a per-row abort from inside the fan-out would name a row of the reshaped table rather than the caller's (M45 review F4).
- 2026-07-31: T4 — `audio_stream` threaded through `loudnorm_analysis_pipeline()`, `run_loudnorm_analysis()` and `run_loudnorm_analysis_batch()`; the batch form expands a scalar/NULL argument to one value per row itself, because `col_or()` would collapse a NULL default to NULL rather than to a per-row vector.
- 2026-07-31: T6 — the re-baselining list was shorter than planned. Six exact-command assertions moved (`test-normalize-audio.R` ×4 including the M16 characterization baseline, `test-normalize-audio-batch.R` ×2, `test-loudnorm-two-pass.R` ×1); `test-normalize-audios-two-pass.R` and the `format_for_web` scalar/batch byte-identity test needed no change, the first because its assertions are containment rather than equality and the second because it compares the two entry points to each other. Also corrected against the plan: `ffm_compile()` emits maps AFTER the output options (`-ac`/`-ar`/`-f null`) and immediately before the output URL, not before them.
- 2026-07-31: T7 — execution evidence green on a 3-track fixture with DEFAULT asserted on track 2 before any result was read: `format_for_web()` carries eng/spa/fra, `normalize_audio()` carries eng, a named track lands on both, `normalize_audio()` still exits 0 on a video-only input, `audio_stream = 9` is an FFmpeg error, and the two-pass path measures and corrects the same track on both the scalar and batch entry points.
- 2026-07-31: T8 — `@param audio_stream` on all four new entry points (each naming which family reads `NULL` the other way), the `@param jobs` column enumeration on both `_batch` verbs, and a NEWS entry describing both changes and why the two verbs read an unset selector differently. `devtools::document()` produces no diff and `spelling::spell_check_package()` is clean, so `inst/WORDLIST` needs no additions. `@seealso` deliberately unchanged: the package puts these cross-references inside the `@param audio_stream` block (crop_video's precedent), and refreshing the sibling enumerations in the fourteen existing blocks is M51's, not this milestone's.
- 2026-07-31: T9 — D028 appended to `cairn/DECISIONS.md` (narrows D026; extends D023/D025), and the per-track-two-pass candidate row added to the ROADMAP with its promotion condition stated as the class of evidence that would falsify the first-track choice. Counted rather than assumed for the D-entry's cost bullet: exactly **18** exported entry points now take `audio_stream` (6 first-track, 12 every-track), enumerated from the namespace.
- 2026-07-31: verify slot clean and `devtools::check()` reports 0 errors / 0 warnings / 0 notes; `devtools::test()` 3098 passing, 0 failures (4 warnings and 5 skips, all pre-existing and in files this milestone does not touch); `devtools::document()` produces no diff. `cairn_validate` passes every check; its two advisories are both pre-existing — the ROADMAP hygiene stamp is over the density cap (left for review's post-merge hygiene pass, which owns that stamp) and M51 carries 8 acceptance criteria.
- 2026-07-31: status → review.
- 2026-07-31: criteria audit ([O], fresh context) returned seven findings: AC3 and AC5 demanded a compiled analysis command that D013/D024 make unreachable from a verb call; AC4's pre-change comparison had no surviving reference and no task; the `test-ffm.R` rule statement, `hit[[1]]`, and both `loudnorm_two_pass.R` caller lines were cited off by one to six lines; and "keyed on command shape" was undefined. All fixed above; none became a gate question.

- 2026-07-31: review returned the milestone to `in-progress` (return 1 of 3). All seven criteria pass with fresh evidence and every gate is clean — `cairn_validate` 0, `check()` 0/0/0, CI green on all nine checks — but the [O] diff-bug lens found a verified regression no criterion covers: `normalize_audio()` writing to an audio-only container (`.wav`, `.mp3`) now aborts at FFmpeg exit 234, because `-map 0:v?` forces video into a muxer that refuses it and the `?` only covers an ABSENT stream, not a rejecting muxer. Reproduced at review against master's command (exit 0, 392428 bytes) and this branch's (exit 234, 0 bytes). Also actioned: adding `audio_stream` beside `audio_codec` makes `audio =` an ambiguous partial match on the normalize verbs, and `run` shifted position in four exported signatures — both unmentioned in NEWS.
- 2026-07-31: the F1 fix is a design choice that changes AC2, so it routes to `/milestone-implement`'s amendment gate rather than a review-side patch. No test in the suite normalizes to an audio container, which is why a green suite sat over the regression; whatever fix is chosen needs one.

- 2026-07-31: amendment gate (substantive) — the F1 regression is fixed by making `normalize_audio()`'s VIDEO map follow the output container, chosen over an opt-out argument (leaves the default broken) and over documenting the loss (a capability master had). AC2 amended, AC8 added, AC7 extended to name the two argument-surface breaks; Scope In extended; T10–T12 added; the Acceptance criteria section was compressed in one pass to stay under the 150-line cap.
- 2026-07-31: T10 — `AUDIO_ONLY_CONTAINERS` + `audio_only_container()` gate the `-map 0:v?` half of `normalize_audio_pipeline()`, keyed on the OUTPUT path so the compile stays binary-free (D024). Measured first: the regression set is `.wav`/`.mp3`/`.aac`/`.opus` failing at exit 234 and `.mka` silently gaining a video stream; `.ogg`/`.webm` already failed on master and are not M49's. Also measured: `-codec:v copy` is inert when no video is mapped, so it stays.
- 2026-07-31: T11 — the container matrix is now walked under execution across nine extensions, asserting exit status, non-empty output, and the presence or absence of a video stream; plus a loudness assertion on the `.wav` output so a future change that drops the filter fails here rather than passing on file size. Mutation-checked: reverting the fix turns six tests red, including this one.
- 2026-07-31: two sub-threshold review findings fixed while the branch was open. F7 (68) — the two-pass batch test now puts a SILENT row first, so `jobs[!silent, ]` genuinely reshapes and a one-row column misalignment would read `eng` instead of `fra`; it previously asserted nothing about the seam it named. F3 (62) — `test-ffm.R`'s "there is no longer a zero category" claim was false (`extract_frame()`/`sample_frames()` compile no map) and is replaced by an accurate statement of why they are absent from the table.
- 2026-07-31: T12 — D029 appended, narrowing D028's video half while leaving its first-track audio rule and measured reason standing (D028 is history and is not edited — IP4). NEWS gains the container rule and both argument-surface breaks. `devtools::check()` 0/0/0, `devtools::test()` 3181 passing 0 failures, `document()` no diff, spelling clean.
- 2026-07-31: status → review (second time).

- 2026-07-31: review round 2 returned the milestone to `in-progress` (return 2 of 3). AC1/AC2/AC3/AC4/AC6/AC7 pass with fresh evidence and every mechanical gate is clean (check 0/0/0, CI green on nine jobs, cairn_validate 0). AC5 and AC8 fail AS WRITTEN: the map-count table has no row for `normalize_audio()`'s audio-container branch though AC5's own rule demands one, and AC8's absolute "no output container that worked before fails after" is false for `.w64`, `.mpa`, `.voc`, `.sbc`, `.latm` and `.adts` — all measured 0 → 234 (or 176). One finding actioned at 85: AC8's execution test guards on ffprobe where AC8 says ffmpeg.
- 2026-07-31: thrash trigger (b) fired — the same failure shape ("a container that worked before now fails") missed twice by a new mechanism each round: first no rule at all, then an incomplete enumeration plus an untouched `format_for_web()`. The remedy is to reconsider the alternatives the plan gate recorded against (an opt-out argument; documenting the loss), plus one it did not consider (dropping video passthrough from `normalize_audio()` entirely, which needs no list) — not another pass at the enumeration.

- 2026-07-31: amendment gate (substantive, second) — `normalize_audio()` now produces audio and NO video, chosen over extending the container list (the thrash rule identifies that as buying the next missing extension) and over an opt-out argument. AC2/AC4/AC5/AC8 amended, T13–T15 added, and both the Acceptance criteria and Tasks sections compressed one pass each for the cap. The user also chose to finish M49 as one milestone despite both split tripwires firing (8 criteria, 15 tasks): the `format_for_web` half is done and verified, and splitting a nearly-finished milestone buys ceremony.
- 2026-07-31: T13 — `AUDIO_ONLY_CONTAINERS`/`audio_only_container()` deleted; `normalize_audio_pipeline()` emits one audio map and no `-codec:v copy` (which named a stream that is never mapped). NEW MEASUREMENT that decided the `?`: when EVERY map specifier is optional and matches nothing, FFmpeg discards the maps and reverts to default stream selection — `-map 0:a:5?` on a video+audio file writes video AND audio. This verb emits one map, so a silent screen recording would have exited 0 while writing video through, via the very heuristic M49 removes. The `?` is therefore dropped from both passes and a no-audio input is now an FFmpeg error. This also supplies the measured reason behind D026's rule that named specifiers carry no `?`.
- 2026-07-31: T14 — `@description`, `@param outfile` and both `@param audio_stream` blocks rewritten for the audio-only contract; NEWS leads with the capability removed and the no-audio input becoming an error; D030 appended (supersedes D029, narrows D028's video half); ROADMAP candidate row created for normalize-and-keep-picture, which also records `format_for_web()`'s identical latent break into an audio container, deliberately not fixed here.
- 2026-07-31: T15 — eleven exact-command assertions re-baselined for the `-codec:v copy` removal across five test files; two tests inverted rather than deleted (`normalize_audio() stream-copies video` now asserts no `-codec:v` and no `0:v`; the batch default-knobs check asserts the audio map instead). `devtools::test()` 3215 passing, 0 failures.
- 2026-07-31: review round 2's other findings — G6 (85, actioned) fixed by guarding the AC8 test on ffmpeg as well as ffprobe. G8/G11/G7/G3 dissolved with the predicate they were about. G1 (`format_for_web()`'s identical break) is recorded on the new candidate row rather than fixed, since its product is a web video file. G4/G5 (stale `@description`, undocumented batch rule) fixed in T14.
- 2026-07-31: status → review (third time).

## Decisions

### M49-D1 — The two-pass analysis pass maps audio alone (2026-07-31)

`loudnorm_analysis_pipeline()` emits `-map 0:a:0?` (or `-map 0:a:<n>`) and no
`-map 0:v?`, unlike the correction command it feeds, which emits both halves.

The pass writes to `-f null` and produces no output file, so there is no output
for a video selection to describe; mapping video decodes a stream the pass
discards. Measured on-branch: the two spellings are indistinguishable in exit
code and in JSON-block count on a 3-audio-track `.mkv` and on a video-only
`.mp4` alike, and the pair runs 0.372 s against 0.356 s per run on a 20 s 720p
file. So the choice was not settled by behavior or by speed but by what the
command states.

Rules out mirroring the correction command's map pair for readability. The
invariant that matters is not that the two commands *look* alike — it is that
they name the same **audio** track, which is asserted directly rather than
inferred from their shapes. Making the analysis pass carry `0:v?` would have
made a mismatch on the audio half no easier to see.

This is why the map-count invariant table now has a row per compiled command
rather than per verb: keyed on the verb, the analysis pass had no row at all,
and it is the one command in the package whose map must *agree* with another
command's rather than merely be well-formed.

## Review

Reviewed 2026-07-31 on PR #52. Every criterion executed fresh on this branch;
no result below is recalled from implementation.

**AC1 — `format_for_web()` / `_batch` accept `audio_stream`.** Verified by
compiling all four cases at `run = FALSE`. Scalar: `NULL` →
`… -movflags +faststart -map 0:v? -map 0:a? "o.mp4"`, `audio_stream = 2` →
the same with `-map 0:a:2`. Batch from the argument: both rows carry
`-map 0:v? -map 0:a:2`. Batch from a column `c(1, NA)`: row 1 carries
`-map 0:a:1` and the `NA` row carries `-map 0:a?`, i.e. every track, not the
argument's value. PASS.

**AC2 — `normalize_audio()` / `_batch` accept `audio_stream`.** Same method.
Scalar: `NULL` → `… -codec:v copy -map 0:v? -map 0:a:0? "o.mkv"`,
`audio_stream = 2` → `-map 0:v? -map 0:a:2`. Batch from the argument: both rows
carry `-map 0:a:2`. Batch from a column `c(1, NA)`: row 1 carries `-map 0:a:1`
and the `NA` row carries `-map 0:a:0?`, the first track. PASS.

**AC3 — analysis names the track correction normalizes.** Asserted on
`loudnorm_analysis_pipeline()`'s compiled output directly, never through a verb
call, exactly as the criterion requires. Across selectors `NULL`, `0`, `1`, `2`
the audio specifier is `0:a:0?`, `0:a:0`, `0:a:1`, `0:a:2` on the analysis side
and identical on the correction side — four agreements, zero mismatches. The
compiled analysis command under `NULL` is
`-y -i "<in>" -af "loudnorm=…:print_format=json" -f null -map 0:a:0? "-"`. PASS.

**AC4 — execution evidence.** Fixture-took check first: the 3-track `.mkv`
reports DEFAULT flags `0,0,1` and languages `eng,spa,fra`, so the disposition
sits on track 2 and no result below is a coincidence of the heuristic agreeing
with the first track. Against T1's recorded baseline (both verbs carried `fra`):
`format_for_web()` now writes `eng,spa,fra`; `normalize_audio()` now writes
`eng`. The comparison is T1's committed record, not a re-run of the old code.
Both no-regression checks hold: `normalize_audio()` on a video-only `.mp4`
exits 0 and writes the file, and `normalize_audio(audio_stream = 9)` on the
3-track input aborts naming FFmpeg's exit status 234 — an FFmpeg error, not an
R range check. PASS.

**AC5 — map-count invariant on compiled commands.** The table now carries
fourteen rows keyed on compiled commands, `normalize_audio(correction)` = 2 and
`normalize_audio(analysis)` = 1 among them, and the analysis row is compiled via
`ffm_compile(loudnorm_analysis_pipeline(f))` so the test stays binary-free
(D013/D024). The zero category is gone from both the rule statement and the
data: no row is `0L`, and the rule now says a new verb that states nothing fails
here rather than being pinned as a known gap. PASS.

**AC6 — D-entry with the measured reason.** D028 records the split. Its
measurement is T1's, not the plan's: three JSON blocks under `-map 0:a?` on the
eng/spa/fra fixture against one under `0:a:0?`, with `classify_loudnorm_output()`
reading `hit[[1]]`. It also carries the correction T1 supplied and planning did
not have — that today's analysis pass measures the DEFAULT track implicitly, so
the two passes agreed only by consulting the same heuristic. PASS.

**AC7 — toolchain gate.** `devtools::document()` produces no diff (`man/` and
`NAMESPACE` clean after a regenerate). `devtools::test()`: 3098 passing, 0
failures; the 4 warnings and 5 skips are pre-existing and sit in files this
branch does not touch (`test-audio-stream.R`, `test-ffmpeg.R`, `test-nvenc.R`,
`test-video-codec.R`; the skips are all nvenc-unavailable).
`devtools::check()`: 0 errors, 0 warnings, 0 notes. NEWS carries a user-facing
entry naming both changes and why the two verbs read an unset selector
differently. PASS.

### Consistency gate

`cairn_validate.py` exits 0 — every CHECK passes, including `coverage complete`,
`binding criteria`, `mirror agreement` and `weight caps`. Two advisories, both
pre-existing and neither introduced here: the ROADMAP `Last hygiene check` stamp
is 620 chars against a 400 cap (this milestone's post-merge hygiene pass
replaces that stamp, which is where it is owned), and M51 carries 8 acceptance
criteria against the 7 split tripwire. No `DESIGN.md` principle changed, so
`cairn_impact.py` does not apply.

Toolchain `consistency-gate` slot: `document()` no diff ✓ · generated files not
hand-edited ✓ · README.md in sync ✓ · `pkgdown::check_pkgdown()` "No problems
found" ✓ · NEWS.md entry present ✓ · no new top-level files, so no
`.Rbuildignore` additions needed ✓ · `check()` clean ✓.

CI on PR #52: all nine checks green — ubuntu release/devel/oldrel-1, macOS
release, Windows release, pkgdown, test-coverage, and both codecov reports.

### Independent review — three lenses plus a scorer

**[S] prior-PR-comments lens: no regressions found.** The GitHub inline-comment
probe returned empty, so no PR-thread walk was performed; the archived `## Review`
sections were the evidence base. It reported each prior lesson bearing on these
files as affirmatively satisfied rather than merely un-regressed: M41/M47-F8
front-door ordering (no new scalar guard on `format_for_web()`, matching
`crop_video()`), M45-F4 caller-row blame (`check_batch_stream_values()` added to
the reshaping batch verb and correctly omitted from the non-reshaping one),
M18 `{?s}` pluralization avoided in the new abort, M39/M44 counting mocks proven
in-scope by a positive trip, D024 purity (the map-count test compiles the
analysis pipeline directly), and M47's "would this pass against pre-change code"
(both new test files pin pre-M49 templates as static strings).

**[S] blame-history lens: no findings.** It confirmed `pass_through_maps()`'s new
`null_map` parameter leaves every M47/M48 call site byte-identical; that D028 is
a recorded carve-out from D026 rather than a silent contradiction; and that all
six re-baselined assertions moved by exact equality rather than being weakened to
containment. On the M16 "must not drift" baseline specifically: the delta is
isolated to the added `-map` pair and the deliberate overwrite is flagged in the
test's own name and comment. It read the map-count restructure as a
strengthening, since the per-verb key hid the analysis pass's map entirely.

**[O] diff-bug lens:** twelve findings, scored by a fresh [S] scorer that did
not generate them. Two clear the 80 bar and are actioned; ten are logged below.

**F1 (scored 95) — ACTIONED, send-back.** `normalize_audio()` now hard-fails on
any audio-only output container. `pass_through_maps()` prepends `-map 0:v?`,
and the `?` makes a map optional when the *stream* is absent, never when the
*muxer* rejects it. Verified independently at review, not taken on the
reviewer's word (ffmpeg 8.1.2, `inst/extdata/sample.mp4`): master's mapless
command writes a 392428-byte `.wav` at exit 0, while this branch's command with
`-map 0:v? -map 0:a:0?` aborts at exit 234, "wav muxer does not support any
stream of type video", leaving a 0-byte file. `.mp3` breaks the same way;
`.m4a` and `.flac` still succeed. `normalize_audio("interview.mp4",
"interview.wav")` is an ordinary research call, `@param infile` is documented as
"a media file (with audio)" with no constraint on the output container, and no
test in the suite normalizes to an audio container — which is why the suite is
green over a real regression. AC4's two no-regression checks cover a video-only
*input*, not an audio-only *output*, so no criterion caught it.

**F6 (scored 82) — ACTIONED, send-back.** Adding `audio_stream` beside the
existing `audio_codec` makes any partial argument name shorter than `audio_c` /
`audio_s` ambiguous on the normalize verbs. Verified:
`normalize_audio(f, "o.mp4", audio = "aac", run = FALSE)` now errors "argument 3
matches multiple formal arguments" where it worked on master. Not mentioned in
NEWS.

**Logged below the 80 threshold (10), surfaced not discarded:**
- F5 (78) `run` shifted position in four exported signatures (`format_for_web`
  5→6, `normalize_audio` 10→11, and both `_batch` verbs), so a positional call
  supplying `run` now binds it to `audio_stream`; verified, and unmentioned in
  NEWS. Scored just under the bar but shares F6's remedy — one NEWS paragraph
  covers both, so it rides along with the send-back.
- F7 (68) the two-pass batch test's fixture has no silent row, so the
  `jobs[!silent, ]` reshape seam its comment claims to exercise is the identity;
  the alignment logic itself was traced and is correct, but the claimed coverage
  does not exist.
- F3 (62) `test-ffm.R`'s new "there is no longer a zero category" statement is
  factually wrong — `extract_frame()` and `sample_frames()` still compile zero
  maps — and the table is a hardcoded list, so a new zero-map verb is absent
  rather than failing. AC5 asked that the zero-map rule statement stop
  describing an empty category; deleting the category rather than reconciling it
  converts a pinned gap into an unpinned one.
- F12 (55) D028 cites `hit[[1]]` by filename with no line number where AC6
  specifies one.
- F11 (38) `pass_through_maps()`'s opening sentence still says "the map pair the
  PASS-THROUGH verbs compile" though `normalize_audio()` is now a caller taking
  the opposite rule; the trailing paragraph added by this diff does document the
  carve-out.
- F2 (25) the reviewer reads "determinism, not cardinality" as false for video,
  since `0:v?` maps every video stream where implicit selection picked one;
  scored low as an attack on the command shape AC2 mandates, though the
  observation about attached-picture streams is real.
- F4 (25) `compare_videos()`, `picture_in_picture()` and the nvenc branch have
  no map-count row; pre-existing and outside M49's Scope.
- F8 (22) two `expect_false` assertions are vacuously true against pre-change
  code; paired with discriminating `expect_identical` assertions nearby.
- F9 (20) `rep_len()` would silently recycle a length-mismatched `audio_stream`;
  no current caller can reach it.
- F10 (18) per-row value errors in `format_for_web_batch()` blame an anonymous
  pmap frame; matches the existing `crop_video_batch()` convention.

### Disposition

**Gate result: send back to `/milestone-implement`.** Every acceptance criterion
passes as written and every mechanical gate is clean, but F1 is a verified
user-facing regression the criteria did not anticipate, and its fix is a design
choice rather than a patch: `normalize_audio()` must either stop mapping video
(dropping it from video-container outputs, contradicting the documented
`-codec:v copy` pass-through), keep the map and document the lost audio-container
outputs, or derive the video half from the output container — the third being new
machinery and a decision D028 does not cover. That belongs at the implement
amendment gate with the maintainer, not in a review-side edit, and it will
change what AC2 demands.

First return for this milestone (thrash count 1 of 3).

---

## Review — round 2 (2026-07-31, after the container-rule fix)

**AC1 PASS.** Re-verified fresh: `format_for_web()` compiles `-map 0:v? -map 0:a?`
under `NULL` and `-map 0:v? -map 0:a:2` when named; batch from the argument
carries `0:a:2` on both rows, and from a `c(1, NA)` column carries `0:a:1` then
`0:a?`.

**AC2 PASS (amended criterion).** `normalize_audio()` compiles
`-map 0:v? -map 0:a:0?` into `.mkv` and `-map 0:a:0?` alone into `.wav`; a named
track narrows the audio half in both. The batch verb applies it per row — a jobs
table of `a.wav`/`b.mkv` compiles 1 map then 2. Predicate edge cases verified:
`o.WAV` takes the audio shape (case-insensitive), `dir.wav/o.mkv` and `noext`
keep the video map (a dot in a directory name does not fool it), and an unknown
extension keeps the video map.

**AC3 PASS.** Analysis and correction agree on the audio specifier across
`NULL`/`0`/`1`/`2` — four agreements, zero mismatches — asserted on
`loudnorm_analysis_pipeline()`'s compiled output directly.

**AC4 PASS.** Fixture-took check `0,0,1` first; `format_for_web()` writes
`eng,spa,fra` and `normalize_audio()` writes `eng` against T1's recorded
baseline of `fra`. Video-only input exits 0; `audio_stream = 9` aborts naming
FFmpeg's status 234.

**AC5 FAIL.** The table is keyed on compiled commands and carries fourteen rows
with no zero row, but AC5's own rule — "a verb whose branches compile different
commands gets a row each" — is not satisfied by the table it now describes.
`normalize_audio()` compiles two different commands depending on the output
container (2 maps for `out.mp4`, 1 for `out.wav`) and only the 2-map branch has
a row. A change making the audio-container branch emit two maps again would not
fail here.

**AC6 PASS.** D028 records the split with T1's measurement; D029 narrows its
video half.

**AC7 PASS (amended criterion).** `document()` no diff, `test()` 3181 passing /
0 failures, `check()` 0 errors / 0 warnings / 0 notes, spelling clean. NEWS
carries the container rule and both argument-surface breaks.

**AC8 FAIL.** The nine-container matrix passes exactly as enumerated: every one
that exited 0 on master exits 0 here with a non-empty file, audio-only ones
carry no video, video ones still do. But the criterion is written absolutely —
"No output container that worked before this milestone fails after it" — and
that is false. Measured on the same input: `.w64`, `.mpa`, `.sbc`, `.latm` and
`.adts` all go from exit 0 to exit 234, and `.voc` to exit 176. They are
audio-only containers absent from `AUDIO_ONLY_CONTAINERS`. The criterion's own
execution matrix cannot see them because it walks only the nine extensions it
names.

The code matches D029, which pre-declares the list one-directional and names
this exact tradeoff. The criterion contradicts the decision it implements. Per
the never-reinterpret rule, that makes the criterion wrong rather than the work
wrong — and it returns to `/milestone-implement` for a gated amendment, not a
review-side reading.

### Consistency gate

`cairn_validate` exits 0; `check()` 0/0/0; `pkgdown` clean; `document()` no
diff; CI green on all nine jobs. Advisories: the pre-existing ROADMAP stamp
density, and **M49 now trips both split tripwires (8 criteria, 12 tasks)** — the
plan's own work log named "the task count crossing the ~10 tripwire" as the
falsifier for keeping both verbs in one milestone, and it has fired.

### Independent review — round 2

**[S] prior-review lens: no regressions.** Checked all twelve round-1 findings
against the current diff: F1/F6/F5/F3 fixed as triaged; the eight logged below
threshold verified still merely logged and not worsened.

**[S] blame-history lens: one candidate.** Confirmed D028 was appended to and
never edited (IP4 intact), that `audio_only_container()` is called from
`normalize_audio_pipeline()` alone so no other verb's D026 contract moved, that
`ffm_map()` is still called once with a pre-computed vector (M43's append), and
that no assertion was loosened. Its candidate is G10 below.

**[O] diff-bug lens: twelve findings, scored by a fresh [S] scorer.** One clears
80.

**G6 (85) — ACTIONED.** AC8's own execution test guards with
`skip_if_no_ffprobe()` but calls `normalize_audio()`, which needs ffmpeg; AC8
says "`skip_if` FFmpeg is absent", and every sibling execution test in the file
uses `skip_if_no_ffmpeg()`. On a machine with ffprobe and no ffmpeg it errors
instead of skipping.

**Logged below threshold (11), surfaced not discarded:**
- G8 (78) the map-count table lacks a row for the audio-container branch — the
  same gap AC5 fails on above, so it rides along with the send-back.
- G1 (75) `format_for_web()` carries the identical mechanism and was not fixed:
  its command shape into `.wav` goes from exit 0 to exit 234, verified. Weighed
  down because the verb's whole recipe is H.264/AAC/faststart web delivery, so
  the call is arguably nonsense — but the inconsistency is real and undocumented.
- G4 (75) `normalize_audio()`'s `@description` still says "The video stream is
  copied unchanged", now true only for video containers; the rule is documented
  only inside `@param audio_stream`.
- G2 (65) the six unlisted audio containers — the substance of AC8's failure,
  scored as arguable only because D029 declares the tradeoff.
- G5 (60) the container rule is undocumented on the batch verb.
- G7 (38) `if (audio_only_container(output))` is not scalar-guarded; both front
  doors pass a scalar, so reachability is low.
- G10 (30) the two-pass batch rewrite verifies one non-default index end to end
  where the version it replaced verified two; it fixed a real seam gap but is
  not a pure superset.
- G11 (25) a trailing space or query string in the output path defeats the
  extension match.
- G9 (18) three container-rule tests are non-discriminating alone; each is
  paired with a discriminating assertion, and six tests were mutation-verified
  to go red without the fix.
- G3 (12) `.m4a`/`.ogg` muxers do accept video, so classifying them audio-only
  drops a stream silently — scored low because D029 and NEWS both declare it
  deliberately, though the drop is announced nowhere at runtime.
- G12 (6) an artifact of the reviewer reading the file mid-review.

### Disposition

**Gate result: send back to `/milestone-implement`.** AC5 and AC8 both fail as
written. Return 2 of 3.

**Thrash trigger (b) has fired.** The same failure shape — "an output container
that worked before now fails" — has now been missed twice, by a new mechanism
each time: round 1 stated the video map with no rule at all; round 2 stated it
with an enumerated list that is incomplete, and left `format_for_web()`
untouched. Re-cutting around an enumeration buys the next missing extension,
which is precisely what round 2 bought. The plan gate recorded two alternatives
against the chosen approach — an opt-out argument, and documenting the loss —
and they are the ones to reconsider, alongside a third the gate did not
consider: dropping video passthrough from `normalize_audio()` entirely, which
needs no list.

---

## Review — round 3 (2026-07-31, after the audio-only rebuild)

**AC1 PASS.** `format_for_web()` compiles `-map 0:v? -map 0:a?` under `NULL`,
`-map 0:v? -map 0:a:2` when named; batch column `c(1, NA)` gives `0:a:1` then
`0:a?`.

**AC2 PASS (amended).** One map, never `0:v`, never `-codec:v`. The compiled
command minus the output path is **byte-identical across all 17 extensions**
tried, which is the property that replaced the container list. Batch applies it
per row.

**AC3 PASS.** Analysis and correction agree on the audio specifier across
`NULL`/`0`/`1`/`2`, asserted on the compiled pipeline directly.

**AC4 PASS (amended).** Fixture-took `0,0,1` first. `format_for_web()` →
`eng,spa,fra`; `normalize_audio()` → `eng`, against T1's baseline of `fra`. Both
error paths hold: `audio_stream = 9` and a no-audio input each abort naming
FFmpeg, and the no-audio case writes nothing.

**AC5 FAIL — criterion wording, not the work.** The table keys rows on compiled
commands and normalize_audio's analysis and correction rows are correct and
separate. But AC5 also says "branches compiling different commands get a row
each", and `normalize_audio_pipeline()` branches on `measured`: the two-pass
correction compiles a materially different string (`…measured_I=…:linear=true`)
with no row, as does `format_for_web(hardware = "nvenc")`. Read strictly, that
clause requires a row per codec/hardware branch package-wide, which is not what
a **map-count** invariant is for — every one of those branches has the same map
count, so nothing is unguarded. The criterion overreaches; the amendment is at
the gate.

**AC6 PASS.** D028 records the split with T1's measurement; D029 and D030 narrow
and then supersede its video half.

**AC7 PASS (amended).** `document()` no diff, `test()` 3215 passing / 0 failures,
`check()` 0/0/0, spelling clean, pkgdown clean. NEWS names the removed
capability, the no-audio input becoming an error, and both argument-surface
breaks.

**AC8 PASS (amended).** All 17 containers exit 0 with a non-empty file carrying
audio and no video — including the six the enumeration missed. Strengthened at
this review to assert exactly one audio stream, and to run the multi-track
fixture, since the single-track matrix could not tell "one stream" from "has a
stream".

### Consistency gate

`cairn_validate` 0; `check()` 0/0/0; `document()` no diff; pkgdown clean; CI
green on all nine jobs. Advisories: the pre-existing ROADMAP stamp density, and
M49's split tripwires (the user chose to finish as one milestone; recorded).

### Independent review — round 3, three lenses plus a scorer

**[S] prior-review lens.** Walked all 24 findings from rounds 1 and 2: fixed,
correctly dissolved with the deleted predicate, or unchanged and sub-threshold.
One genuinely new: `pass_through_maps()`'s comment now describes a call site T13
deleted.

**[S] blame-history lens.** Confirmed `-codec:v copy` entered at M14 and no
milestone depends on it; D026's pass-through verbs keep their `?`; DECISIONS.md
is append-only with D028/D029 byte-unchanged; no assertion weakened; NEWS
verified against runtime behavior. Found three more stale comments.

**[O] diff-bug lens.** Found no correctness defect in the audio-only contract —
it independently re-derived D030's load-bearing measurement (`-map 0:a:5?`
reverts to default selection) and confirmed six new tests fail against the
round-2 shape. Its findings are the seventeen scored below.

**Actioned (≥80), all fixed on the branch:**
- **H1 (90) — `R/ffmpeg.R` was silently rewritten CRLF→LF** by a scripted edit in
  `86d7925`, inflating the diff to 5777/5618 for a 215-line change and making
  `git blame` attribute all 5777 lines to one commit — blinding the milestone
  process's own blame lens on the package's largest file. Restored to CRLF; the
  diff is 193/31 and the squash-merge will carry correct blame. Only this file
  was affected.
- **H2 (88)** `pass_through_maps()`'s comment asserted the inverse of D030 and
  its `null_map` parameter was dead (no caller passed it). Parameter removed;
  comment now warns against copying the `0:a?` spelling into an audio verb.
- **H4 (85)** `@examples` demonstrated `normalize_audio(video, "normalized.mp4")`
  — the exact call NEWS says no longer does what it looks like. Now audio outputs.
- **H12 (85)** `@seealso` still called `standardize_video()` its "video-side
  complement", the framing T14 removed from `@description`. Repointed to
  `extract_audio()`/`convert_audio()`.
- **H8 (82)** `run_normalize_correction()`'s contract comment still listed
  `-codec:v copy` as inherited.
- **H7 (80)** the map-count rule statement named `0:a:0?`, a spelling D030
  deleted from the package.

**Also fixed though sub-threshold**, being the same defect class (a comment or
doc asserting the opposite of shipped behavior) in files this milestone rewrote:
H16 (78) the normalize test-file header, H17 (75) a two-pass comment, H11 (78)
the AC8 test's stream-count weakness, H10 (72) NEWS's unqualified "anything
FFmpeg can mux" (`.wma` still fails, as on master), H13 (65) the batch verb's
derived-name docs.

**Logged, not actioned:**
- H9 (78) the milestone's **Scope** section still describes the superseded
  round-2 container-predicate design — plan-owned, at the gate.
- H6 (55) AC5's overreaching clause — plan-owned, at the gate.
- H3 (55) `vignettes/workflow.Rmd` still teaches `normalize_audio()` as
  video-preserving — outside Scope as written; at the gate.
- H5 (65) `normalize_audio()` took the extraction family's shape without its
  `warn_dropped_audio()` diagnostic → new ROADMAP candidate row.
- H15 (22) `format_for_web()`'s identical audio-container break → already on a
  candidate row. H14 (20) an unreachable `rep_len()` recycle → still logged.
