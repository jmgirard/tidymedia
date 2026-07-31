# M49: Finish D026 on `format_for_web()` and `normalize_audio()`

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** —

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

- [ ] AC1 `format_for_web()` and `format_for_web_batch()` accept `audio_stream`;
      the compiled command carries `-map 0:v? -map 0:a?` when it is `NULL` and
      `-map 0:v? -map 0:a:<n>` when a track is named. Asserted at compile level
      (`run = FALSE`) on both entry points, and for the batch verb both from the
      argument and from a jobs `audio_stream` column whose `NA` cell keeps that
      row on every track.
- [ ] AC2 `normalize_audio()` and `normalize_audio_batch()` accept
      `audio_stream`; the compiled correction command carries
      `-map 0:v? -map 0:a:0?` when it is `NULL` and `-map 0:v? -map 0:a:<n>`
      when a track is named — asserted at compile level on both entry points,
      and for the batch verb from an `audio_stream` column whose `NA` cell keeps
      that row on the first track.
- [ ] AC3 The analysis command names the same audio track the correction
      command normalizes, under both `NULL` and a named track. Asserted on
      `loudnorm_analysis_pipeline()`'s compiled output directly, not through a
      verb call: D013 makes the analysis pass run before `run` is consulted, so
      no `two_pass = TRUE` verb call can yield that command without executing
      FFmpeg, and this criterion must not require one.
- [ ] AC4 Execution evidence on a 3-audio-track fixture whose DEFAULT
      disposition is asserted to sit on track 2 before any result is trusted
      (M43's fixture-took check): `format_for_web()` carries all three tracks
      and `normalize_audio()` carries track 0. The pre-change comparison is the
      recorded baseline from T1, not a re-run of the old code. On the same
      branch, two no-regression checks: `normalize_audio()` still exits 0 on a
      video-only input (measured at plan time — `0:a:0?` exits 0 where a bare
      `0:a:0` exits 234), and `normalize_audio(audio_stream = 9)` on a 3-track
      input is an FFmpeg error rather than an R one. `skip_if` FFmpeg is absent.
- [ ] AC5 The map-count invariant test (`tests/testthat/test-ffm.R:537-604`)
      covers both verbs with one row per *compiled command* rather than one per
      verb — a verb whose branches compile different commands gets a row each,
      named verb-plus-branch — so `normalize_audio()`'s analysis and correction
      commands are separate rows. Its zero-map rule statement at `:547-550` no
      longer describes an empty category.
- [ ] AC6 A `cairn/DECISIONS.md` entry records the split and its measured
      reason: under `-map 0:a?` the analysis pass prints one JSON block per
      mapped audio track, while `classify_loudnorm_output()` reads `hit[[1]]`
      (`R/loudnorm_two_pass.R:48`), so every mapped track would be corrected
      with track 0's measurements, silently. The measurement it cites is the
      one T1 records, not the plan's.
- [ ] AC7 `devtools::document()` produces no diff, `devtools::test()` clean, and
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

- [ ] T1 Commit the pre-change evidence first, before any source edit, so AC4
      and AC6 have a fixed reference the branch cannot destroy (M44's lesson):
      the disposition-shifted 3-track fixture, what each verb carries from it
      today, and the analysis pass's JSON-block count under `-map 0:a?`.
- [ ] T2 Tests first, then thread `audio_stream` through
      `format_for_web_pipeline()` (`R/ffmpeg.R:1154-1165`) via
      `pass_through_maps()` (`R/ffmpeg.R:326-329`), `format_for_web()`
      (`R/ffmpeg.R:1192`) and `format_for_web_batch()` (`R/ffmpeg.R:4619`),
      including the batch front-door guard and `check_batch_audio_col()` column
      support that M47 established (`R/ffmpeg.R:1906-1908` pattern).
- [ ] T3 Tests first, then add a first-track optional variant beside
      `pass_through_maps()` (`null_map = "0:a:0?"`) and wire it into
      `normalize_audio_pipeline()` (`R/ffmpeg.R:2103-2145`),
      `normalize_audio()` (`R/ffmpeg.R:2022`) and `normalize_audio_batch()`
      (`R/ffmpeg.R:3722`).
- [ ] T4 Tests first, then carry the same selection into
      `loudnorm_analysis_pipeline()` (`R/loudnorm_two_pass.R:16-24`) and its
      callers `run_loudnorm_analysis()` (`:105`) and
      `run_loudnorm_analysis_batch()` (`:136`).
- [ ] T5 Rewrite the map-count invariant table (`tests/testthat/test-ffm.R:537-604`)
      onto compiled commands and update its rule statement.
- [ ] T6 Re-baseline the exact-command assertions this breaks:
      `tests/testthat/test-normalize-audio.R:8-15`, `:20-32` (the M16
      characterization baseline — overwrite it deliberately, its comment at
      `:17-19` forbids drift), `:210-222`, `:224-236`;
      `tests/testthat/test-normalize-audio-batch.R:62-72`, `:529-530`;
      `tests/testthat/test-normalize-audios-two-pass.R`; and the
      `format_for_web` scalar/batch byte-identity test
      (`tests/testthat/test-format-for-web-batch.R:18-24`).
- [ ] T7 Execution tests against T1's baseline, plus the two no-regression
      checks (video-only input, out-of-range named track).
- [ ] T8 Roxygen `@param audio_stream` on the four new entry points, the
      `@param jobs` column enumeration on both `_batch` verbs,
      `@seealso`/NEWS/`inst/WORDLIST`; `devtools::document()`.
- [ ] T9 Append the D-entry; add the per-track-two-pass candidate row named in
      Out; run the profile's verify slot and `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose a first-track `NULL` for `normalize_audio()` over D026's every-track `NULL` because an every-track map makes the two-pass analysis print one JSON block per track while the parser reads only the first, silently correcting every track with track 0's measurements; falsified by a per-stream filter-options seam that lets one measured set attach to each mapped track, which would make uniformity affordable again.
- 2026-07-31: plan gate chose one milestone over splitting `format_for_web` and `normalize_audio` apart because the D-entry records a single split decision across both verbs and half of it is incoherent alone; falsified by the task count crossing the ~10 tripwire during implementation.
- 2026-07-31: plan measurements (ffmpeg 8.1.2, macOS; a 3-audio-track `.mkv` with DEFAULT disposition moved to track 2 and languages eng/deu/fra): `format_for_web()` and `normalize_audio()` each carry only `fra` today; `-map 0:v? -map 0:a?` carries all three on both; `-map 0:v? -map 0:a:0?` carries `eng`; the analysis pass prints 3 JSON blocks under `0:a?` and 1 under `0:a:0?` or a named track; on a video-only input `0:a:0` exits 234 while `0:a:0?` and no-map both exit 0. T1 re-records these on the branch.
- 2026-07-31: criteria audit ([O], fresh context) returned seven findings: AC3 and AC5 demanded a compiled analysis command that D013/D024 make unreachable from a verb call; AC4's pre-change comparison had no surviving reference and no task; the `test-ffm.R` rule statement, `hit[[1]]`, and both `loudnorm_two_pass.R` caller lines were cited off by one to six lines; and "keyed on command shape" was undefined. All fixed above; none became a gate question.

## Decisions

## Review
