# M49: Finish D026 on `format_for_web()` and `normalize_audio()`

- **Status:** in-progress
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
carried onto the two-pass analysis command as well as the correction one. The
D-entry recording that split and its measurement; the map-count invariant
table; roxygen, NEWS, `inst/WORDLIST`.

**Out:** refreshing the sibling-verb enumeration inside the fourteen existing
`@param audio_stream` blocks → M51. Per-track two-pass loudnorm (one measured
set per mapped track, which is what an every-track `NULL` here would require,
and which needs per-stream filter options the linear builder has no slot for) →
new candidate row. `subtitle_stream` / `video_stream` selectors → the standing
candidate row. Quoting the emitted specifiers → M50.

## Acceptance criteria

- [x] AC1 `format_for_web()` and `format_for_web_batch()` accept `audio_stream`;
      the compiled command carries `-map 0:v? -map 0:a?` when it is `NULL` and
      `-map 0:v? -map 0:a:<n>` when a track is named. Asserted at compile level
      (`run = FALSE`) on both entry points, and for the batch verb both from the
      argument and from a jobs `audio_stream` column whose `NA` cell keeps that
      row on every track.
- [x] AC2 `normalize_audio()` and `normalize_audio_batch()` accept
      `audio_stream`; the compiled correction command carries
      `-map 0:v? -map 0:a:0?` when it is `NULL` and `-map 0:v? -map 0:a:<n>`
      when a track is named — asserted at compile level on both entry points,
      and for the batch verb from an `audio_stream` column whose `NA` cell keeps
      that row on the first track.
- [x] AC3 The analysis command names the same audio track the correction
      command normalizes, under both `NULL` and a named track. Asserted on
      `loudnorm_analysis_pipeline()`'s compiled output directly, not through a
      verb call: D013 makes the analysis pass run before `run` is consulted, so
      no `two_pass = TRUE` verb call can yield that command without executing
      FFmpeg, and this criterion must not require one.
- [x] AC4 Execution evidence on a 3-audio-track fixture whose DEFAULT
      disposition is asserted to sit on track 2 before any result is trusted
      (M43's fixture-took check): `format_for_web()` carries all three tracks
      and `normalize_audio()` carries track 0. The pre-change comparison is the
      recorded baseline from T1, not a re-run of the old code. On the same
      branch, two no-regression checks: `normalize_audio()` still exits 0 on a
      video-only input (measured at plan time — `0:a:0?` exits 0 where a bare
      `0:a:0` exits 234), and `normalize_audio(audio_stream = 9)` on a 3-track
      input is an FFmpeg error rather than an R one. `skip_if` FFmpeg is absent.
- [x] AC5 The map-count invariant test (`tests/testthat/test-ffm.R:537-604`)
      covers both verbs with one row per *compiled command* rather than one per
      verb — a verb whose branches compile different commands gets a row each,
      named verb-plus-branch — so `normalize_audio()`'s analysis and correction
      commands are separate rows. Its zero-map rule statement at `:547-550` no
      longer describes an empty category.
- [x] AC6 A `cairn/DECISIONS.md` entry records the split and its measured
      reason: under `-map 0:a?` the analysis pass prints one JSON block per
      mapped audio track, while `classify_loudnorm_output()` reads `hit[[1]]`
      (`R/loudnorm_two_pass.R:48`), so every mapped track would be corrected
      with track 0's measurements, silently. The measurement it cites is the
      one T1 records, not the plan's.
- [x] AC7 `devtools::document()` produces no diff, `devtools::test()` clean, and
      `devtools::check()` reports 0 errors / 0 warnings; NEWS carries an entry
      describing both changes in user-facing terms.

## Coverage

- AC1 → T2, T6
- AC2 → T3, T6
- AC3 → T4
- AC4 → T1, T7
- AC5 → T5
- AC6 → T1, T9
- AC7 → T8, T9

## Tasks

- [x] T1 Commit the pre-change evidence first, before any source edit, so AC4
      and AC6 have a fixed reference the branch cannot destroy (M44's lesson):
      the disposition-shifted 3-track fixture, what each verb carries from it
      today, and the analysis pass's JSON-block count under `-map 0:a?`.
- [x] T2 Tests first, then thread `audio_stream` through
      `format_for_web_pipeline()` (`R/ffmpeg.R:1154-1165`) via
      `pass_through_maps()` (`R/ffmpeg.R:326-329`), `format_for_web()`
      (`R/ffmpeg.R:1192`) and `format_for_web_batch()` (`R/ffmpeg.R:4619`),
      including the batch front-door guard and `check_batch_audio_col()` column
      support that M47 established (`R/ffmpeg.R:1906-1908` pattern).
- [x] T3 Tests first, then add a first-track optional variant beside
      `pass_through_maps()` (`null_map = "0:a:0?"`) and wire it into
      `normalize_audio_pipeline()` (`R/ffmpeg.R:2103-2145`),
      `normalize_audio()` (`R/ffmpeg.R:2022`) and `normalize_audio_batch()`
      (`R/ffmpeg.R:3722`).
- [x] T4 Tests first, then carry the same selection into
      `loudnorm_analysis_pipeline()` (`R/loudnorm_two_pass.R:16-24`) and its
      callers `run_loudnorm_analysis()` (`:105`) and
      `run_loudnorm_analysis_batch()` (`:136`).
- [x] T5 Rewrite the map-count invariant table (`tests/testthat/test-ffm.R:537-604`)
      onto compiled commands and update its rule statement.
- [x] T6 Re-baseline the exact-command assertions this breaks:
      `tests/testthat/test-normalize-audio.R:8-15`, `:20-32` (the M16
      characterization baseline — overwrite it deliberately, its comment at
      `:17-19` forbids drift), `:210-222`, `:224-236`;
      `tests/testthat/test-normalize-audio-batch.R:62-72`, `:529-530`;
      `tests/testthat/test-normalize-audios-two-pass.R`; and the
      `format_for_web` scalar/batch byte-identity test
      (`tests/testthat/test-format-for-web-batch.R:18-24`).
- [x] T7 Execution tests against T1's baseline, plus the two no-regression
      checks (video-only input, out-of-range named track).
- [x] T8 Roxygen `@param audio_stream` on the four new entry points, the
      `@param jobs` column enumeration on both `_batch` verbs,
      `@seealso`/NEWS/`inst/WORDLIST`; `devtools::document()`.
- [x] T9 Append the D-entry; add the per-track-two-pass candidate row named in
      Out; run the profile's verify slot and `devtools::check()`.

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

