# M47: Stop `standardize_video()` and `anonymize_video()` picking an audio track by disposition

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m47-audio-stream-standardize-anonymize · https://github.com/jmgirard/tidymedia/pull/50

## Goal

Give `standardize_video()` and `anonymize_video()` (+ `_batch`) an explicit audio
map on every call and an `audio_stream` selector, so which tracks survive stops
being a property of the input's flags.

## Scope

**In:** `audio_stream` on `standardize_video()`, `anonymize_video()`,
`standardize_video_batch()`, `anonymize_video_batch()` — 0-based among the
input's audio streams (D023), scalar argument plus a per-row `audio_stream`
jobs column. Both pipelines emit an explicit map on every call:
`-map 0:v -map 0:a` when `audio_stream` is `NULL`, `-map 0:v -map 0:a:<n>` when
a track is named. A `cairn/DECISIONS.md` entry fixing the pass-through family's
rule and answering the question D025's fifth bullet left open. NEWS.

**Out:**
- `crop_video` / `segment_video` → M48 (planned now, depends on this).
- `ffm_copy()`/`ffm_concat()` idempotence → M48, the first milestone that
  narrows an `ffm_copy()` map.
- Carrying subtitle or data streams through these verbs, and a
  `subtitle_stream`/`video_stream` selector → the existing M45-Out candidate row.
- Carrying M44's dropped-audio-track warning here — not needed: the default now
  carries every track, and on the named path the caller chose it (the gate
  `extract_audio()` uses, `R/ffmpeg.R:476`).
- `run = FALSE` running `ffmpeg -encoders` under `hardware = "nvenc"` → new
  candidate row; it also falsifies D024's "sole exception" sentence.
- The `audio =` (D009) documentation reconciliation → new candidate row.

## Acceptance criteria

- [x] AC1 With `audio_stream` unset, each verb compiles exactly two `-map`
      arguments, `-map 0:v?` then `-map 0:a?`, asserted as a committed literal
      command string; the invariant test at `tests/testthat/test-ffm.R:438`
      is restated to the rule these verbs follow and gains both of them.
- [x] AC2 With `audio_stream = 2`, each verb compiles exactly two `-map`
      arguments, `-map 0:v?` then `-map 0:a:2` — the named track carries no
      `?`, so naming a track the input lacks stays an FFmpeg error rather than
      silently producing no audio (D023).
- [x] AC3 On both verbs a value that is non-numeric, non-whole, negative, `NA`,
      or longer than one aborts naming `audio_stream`, and `conditionCall()`
      resolves to the verb the caller wrote, not to a Layer-1 helper.
- [x] AC4 Both `_batch` siblings take an `audio_stream` argument and an
      `audio_stream` jobs column that overrides it per row, where a cell of `NA`
      is the column form of `NULL`; a one-row batch call compiles a command
      byte-identical to the scalar call with the same arguments.
- [x] AC5 A wrongly typed `audio_stream` column aborts before any row runs,
      naming the column and saying `NA` keeps every audio track; the message
      does not carry the extraction family's "keep the first audio track".
- [x] AC6 With ffmpeg present, on a 3-audio-track `.mkv` whose DEFAULT
      disposition sits on track 1, `standardize_video(audio_stream = 2)` writes
      exactly one audio stream and it is `fra`; the same call with `audio_stream`
      unset writes all three (master writes one, `spa`). A video-only input and
      an audio-only input both still succeed, as they do on master.
- [x] AC7 At the default `hardware`, no entry point runs a binary when
      `run = FALSE`: a counting mock over `run_program()`, `find_ffmpeg()` and
      `find_ffprobe()` records zero invocations across all four.
- [ ] AC8 `cairn/DECISIONS.md` gains an entry recording the pass-through rule and
      why it diverges from D023's first-track `NULL`; each `@param audio_stream`
      names the other two families' `NULL` (D025's stated cost); `NEWS.md`
      records the argument and both breaking changes; `devtools::document()`
      produces no diff, `devtools::test()` is clean, and `devtools::check()`
      reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T3, T4, T7
- AC2 → T2, T3, T4
- AC3 → T3, T4
- AC4 → T5, T6
- AC5 → T5, T6
- AC6 → T1, T8
- AC7 → T3, T4, T5, T6
- AC8 → T7, T8

## Tasks

- [x] T1 Record both verbs' current compiled commands as committed literals and
      add the failing-first compile tests. Extend `make_multitrack_video()`
      (`tests/testthat/helper-media.R:158`) to put the DEFAULT disposition on
      track 1 — it sets none today — and assert the fixture's own disposition
      flags before trusting any result, skipping if they did not take (M43).
- [x] T2 Add a pass-through map resolver beside `audio_stream_map()`
      (`R/ffmpeg.R:273`) returning `c("0:v", "0:a")` for `NULL` and
      `c("0:v", "0:a:<n>")` for a named track; unit-test it.
- [x] T3 `standardize_video()` / `standardize_pipeline()` (`R/ffmpeg.R:1265`,
      `:1298`): argument before `run` (M45's precedent), guard at the END of the
      front-door block so precedence does not move (M41), map in the pipeline.
- [x] T4 Same for `anonymize_video()` / `anonymize_pipeline()`
      (`R/ffmpeg.R:1410`, `:1437`) — its front door is thin (`:1417-1419`) and
      most validation lives in the pipeline with `call =` threaded.
- [x] T5 `standardize_video_batch()` (`R/ffmpeg.R:3115`): argument,
      `check_batch_audio_col(jobs, "audio_stream", na_means = …)`,
      `batch_stream_cell()` in the closure. No reshape, so
      `check_batch_stream_values()` is not needed (`R/ffmpeg.R:3793-3801`).
- [x] T6 Same for `anonymize_video_batch()` (`R/ffmpeg.R:1661`); its closure
      names `regions` explicitly (`:1774`), so the new column arrives via `dots`.
- [x] T7 Roxygen on all four, the `@param jobs` column enumerations (M39), and
      the D025 cross-references; `devtools::document()`.
- [x] T8 Execution tests on the multi-track fixture; the D-entry; NEWS.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose an always-emitted explicit map (`NULL` → `0:v` + `0:a`) over leaving `NULL` as today's command, because the latter keeps FFmpeg's DEFAULT-disposition heuristic as the resolved default — measured 3 audio tracks in, the second one out — which D023's second bullet rules out in terms that are not verb-scoped; falsified by a report of a caller relying on these verbs carrying subtitle or data streams.
- 2026-07-30: plan gate chose `0:v` + `0:a` over `-map 0` for the `NULL` case because `-map 0` into `.mp4` on a subtitle-bearing input fails outright (measured exit 8, ffmpeg 8.1.2), which would newly break both verbs on the package's flagship container; falsified by a default output container that accepts subtitles, or an FFmpeg build that stream-copies unencodable streams.
- 2026-07-30: plan gate chose two milestones by verb pair over one eight-entry-point milestone, because eight entry points is roughly twice M43's proven size and trips the >~7-criteria and >~10-task tripwires; falsified by M47 landing in well under one working session.
- 2026-07-30: criteria audit ([O], fresh context) returned 13 findings; 10 with one clear answer were fixed before the gate (non-discriminating execution criterion, an evaporating `master` baseline, unnamed output containers, four bundled criteria, omitted NEWS and `@param` obligations, the falsified `ffm_copy()` prose, undetermined scalar `NA`, the unmentioned map invariant, and a false `run = FALSE` purity claim under `hardware = "nvenc"` that I reproduced); the remaining 3 collapsed into the gate's first question.

- 2026-07-30: T1 — `make_multitrack_video()` gained `default_track =` rather than moving the disposition in place: 22 existing call sites use the fixture, and a defaulted parameter leaves every one of them compiling the identical command. `NULL` emits no `-disposition` flags at all.
- 2026-07-30: T1 — the fixture clears track 0's DEFAULT before setting the requested one; `-disposition:a:1 default` alone ADDS the flag, leaving two default tracks and FFmpeg back on its own preference. Verified `1 0 0` unchanged vs `0 1 0` with `default_track = 1`.
- 2026-07-30: T1 — 7 tests red for the right reason (`unused argument (audio_stream = 2)`), 36 green.

- 2026-07-30: T3/T4 — AMENDMENT (gated). AC1 and AC2 pinned `-map 0:v` / `-map 0:a`; both break on an input missing a stream type. Measured ffmpeg 8.1.2: a bare `-map 0:a` on a video-only input exits 234 ("Stream map '' matches no streams"), and a bare `-map 0:v` on an audio-only input exits 234, where master — emitting no map — exits 0 and passes the stream through. The unselected specifiers now carry `?`; the NAMED one deliberately does not, so `audio_stream = 9` on a 3-track input stays an FFmpeg error, which is what every `@param audio_stream` in the package promises (D023). Ruled out at the gate: `?` on the named map too (turns a mistyped index into a silently audio-less output), and an FFprobe guard (a probe whose result enters the compiled command is outside D024's licence and would break AC7).
- 2026-07-30: T3/T4 — the suite caught only the video-only half, via one existing test that happens to standardize a silent fixture (`test-ffmpeg.R:311`); the audio-only half was found by probing for it. Both are now regression tests over new `make_silent_video()` / existing `make_silent_audio()` fixtures.
- 2026-07-30: T2 — `pass_through_maps()` reuses `audio_stream_map(null_map = "0:a?")` rather than re-deriving the specifier, so the argument's guard, its `arg =` and its `call` threading are inherited rather than duplicated. One `ffm_map()` call with both specifiers, never two: `ffm_map()` appends, so two calls are indistinguishable from a pipeline that mapped twice by accident.
- 2026-07-30: T3/T4 — full suite 0 failures, 2890 pass (4 warnings, 5 skips — both counts unchanged from master). `R/ffmpeg.R` still 5429/5429 CRLF, diff 79/4, so the M35 whole-file-rewrite trap did not fire.

- 2026-07-30: T5/T6 — both batch verbs take the argument and an `audio_stream` column; `na_means = "keep every audio track"`, which is a third wording beside the composite verbs' "drop audio" and the extraction verbs' "keep the first audio track", and the tests assert the other two are ABSENT (M40). Neither verb reshapes its jobs table, so `check_batch_stream_values()` is deliberately not called — pmap's index already is the caller's row (M45 review F4).
- 2026-07-30: T5/T6 — full suite 0 failures, 2911 pass.

- 2026-07-30: T7 — the D025 cross-reference obligation was honoured on all TEN blocks that carry `audio_stream`, not only M47's four: after this milestone `NULL` means "first track" on the four extraction entry points and "every track" on the six others, so a block stating only its own reading leaves a reader who meets two of them with no way to tell. The four extraction blocks now point at the every-track families and `separate_audio_video()`'s names the two new ones.
- 2026-07-30: T7 — both new batch verbs' `@param jobs` enumerations gained the `audio_stream` column; each closes "Any other columns are ignored", which a reader believes (M39).
- 2026-07-30: T7 — `devtools::document()` is idempotent (second run touches nothing) and `run_examples()` is clean.

- 2026-07-30: T8 — the `test-ffm.R:438` invariant was pinned as `all(maps) <= 1L`, which M47 falsifies; rewritten as an exact per-verb count table rather than a looser bound, so a wrong count fails in either direction. Writing it revealed `segment_video(reencode = TRUE)` emits ZERO maps, which the old bound had hidden and which M48 must handle.
- 2026-07-30: T8 — D026 appended, answering D025's fifth bullet with M45's every-track reading and recording the `?` asymmetry, the rejected `-map 0`, and the subtitle-carriage change. NEWS carries the breaking change (bigger outputs on multi-track inputs; subtitles no longer carried into `.mkv`) and the new argument.

- 2026-07-30: all 8 tasks done; `devtools::check()` Status: OK (0 errors, 0 warnings, 0 notes; `spelling.Rout` matched, so M17's masked-NOTE trap did not fire), `devtools::test()` 2911 pass / 0 fail, `pkgdown::check_pkgdown()` clean, `document()` idempotent. Status -> review.

## Decisions

- 2026-07-30 (M47-D1): `standardize_video()`/`anonymize_video()` adopt no diagnostic probe. M44's dropped-audio-track warning covers the extraction verbs because their default silently narrows; after D026 these two carry every track by default, so there is nothing implicit to warn about, and on the named path the caller chose the track — the same gate `extract_audio()` uses (`R/ffmpeg.R:476`). Nothing here touches D024's licence, and no FFprobe call was added to either verb.

## Review

**Reviewed 2026-07-30. PR #50. Evidence gathered fresh on this branch, by
running the verbs directly rather than by re-reading the tests.**

### Acceptance criteria

- **AC1 ✓** `standardize_video(f, "out.mp4", run = FALSE)` compiles
  `… -movflags +faststart -map 0:v? -map 0:a? "out.mp4"`; `anonymize_video()`
  the same without `+faststart`. Two `-map` arguments each, counted. The
  `test-ffm.R` invariant was rewritten from `all(maps) <= 1L` to an exact
  per-verb count table and now covers both verbs (2 each); it passes, and
  writing it surfaced that `segment_video(reencode = TRUE)` emits 0 maps.
- **AC2 ✓** `audio_stream = 2` compiles `-map 0:v? -map 0:a:2` on both verbs,
  two maps each. The named specifier carries no `?` — asserted directly.
- **AC3 ✓** `"1"`, `1.5`, `-1`, `NA`, `NA_integer_` and `c(0, 1)` each abort on
  both verbs; every message names `audio_stream` and `conditionCall()` resolves
  to `standardize_video` / `anonymize_video`, never a Layer-1 helper.
- **AC4 ✓** A jobs table with `audio_stream = c(1, NA)` under
  `audio_stream = 2` compiles `-map 0:a:1` on row 1 and `-map 0:a?` on row 2 —
  the column overrides the argument and `NA` is the column form of `NULL`. A
  one-row batch call is byte-identical to the scalar call (`identical()` TRUE).
- **AC5 ✓** A character `audio_stream` column aborts before any row runs with
  "The audio_stream column of `jobs` must be numeric (NA to keep every audio
  track)." Tests assert the extraction family's "first audio track" and the
  composite verbs' "drop audio" are both ABSENT.
- **AC6 ✓** On a 3-track `.mkv` whose DEFAULT disposition sits on track 1
  (fixture flags verified `0 0 1 0` before use): unset writes
  `video audio audio audio` / `eng spa fra`, where master writes one track
  (`spa`); `audio_stream = 2` writes `video audio` / `fra` — a track that is
  neither the first nor the DEFAULT one, so no implicit selection produces it.
  A video-only input and an audio-only input both exit 0 and pass their stream
  through, as on master. `audio_stream = 9` still errors.
- **AC7 ✓** A counting mock over `run_program()`, `find_ffmpeg()` and
  `find_ffprobe()` records **0** invocations across six `run = FALSE` calls
  (both scalars and both batches, named and unset). Scoped to the default
  `hardware` by AC wording: `hardware = "nvenc"` does shell out, which is the
  candidate row this milestone opened.

- **AC8 — FAILED on first pass, see finding F2.** My first evidence line for
  this criterion counted `@param audio_stream` occurrences file-wide and
  concluded all ten blocks carried the cross-reference. That grep cannot see
  per-block coverage, and the diff-bug lens caught what it missed:
  `separate_audio_video_batch()`'s block (`R/ffmpeg.R:4580`) names only
  `separate_audio_video()`, never the extraction family, and
  `man/separate_audio_video_batch.Rd` is one of ten man files the branch does
  NOT touch. Re-verified by reading the block. Ticked only after the fix below.
- **AC8 (after fix) ✓** `cairn/DECISIONS.md` gains D026, answering D025's fifth bullet with
  M45's every-track reading and recording the `?` asymmetry, the rejected
  `-map 0`, and the subtitle-carriage change. All ten `@param audio_stream`
  blocks now name the families reading `NULL` the other way (attributed one by
  one to their verbs; an eleventh grep hit is a prose comment quoting the
  phrase, not a block). `NEWS.md` carries both the breaking change and the new
  argument, with no milestone numbers in user-facing text.
  `devtools::document()` no diff · `devtools::test()` **2911 pass / 0 fail**
  (4 warnings, 5 skips — both unchanged from master) · `devtools::check()`
  **Status: OK, 0 errors / 0 warnings / 0 notes**, with `spelling.Rout`
  matching, so M17's masked-NOTE trap did not fire.

### Consistency gate

- `cairn_validate.py` — all checks passed; 2 advisory warnings (8 criteria on
  M47 and M48, tripwire 7), not gate failures.
- `cairn_impact.py` — skipped: `cairn/DESIGN.md` is untouched, no principle
  changed.
- `devtools::document()` — no diff (`man/`, `NAMESPACE` clean after a re-run).
- `pkgdown::check_pkgdown()` — no problems found.
- `NEWS.md` — breaking-change and new-feature entries present, no milestone
  numbers in user-facing text.
- No new top-level files, so no `.Rbuildignore` entry needed. README untouched.
- CI on PR #50: all 7 checks green — macOS release, Windows release, Ubuntu
  release/devel/oldrel-1, pkgdown, test-coverage. The three Ubuntu runners are
  the ffmpeg-6.1.1 platform M45's lesson warns about (macOS brew ships 8.x);
  the new disposition fixture and the `?` map suffix behave identically there.

