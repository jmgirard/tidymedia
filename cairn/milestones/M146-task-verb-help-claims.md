# M146: The crop, web-format, standardize and strip-metadata help pages state what the code does

- **Status:** review
- **Priority:** normal
- **Depends on:** M145
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — help pages that package users read
- **Branch/PR:** m146-task-verb-help-claims

## Goal

Ten task-function help-page claims that the M130 claim audit found false now say what the code does.

## Scope

**In:** correct the text of these claims. Line numbers in `R/ffmpeg.R` are as read on 2026-10-18.

- (a) `?format_for_web` says odd dimensions are padded down to even values, but the recipe
  crops them with `ffm_crop()` (`:1549-1550`, `:1517`, the comment at `:1509`).
- (b) `?format_for_web_batch` says the command has no per-row settings, but it reads per-row
  `audio_stream` and `quality` columns (`:6708`, `:6804-6805`).
- (c) The See Also of `?crop_video_batch` and `?format_for_web_batch` says each wraps the
  single-file function, but both call the shared `*_pipeline()` recipe (`:6564`, `:6741`).
- (d) The See Also of `?standardize_video_batch` and `?strip_metadata_batch` names two
  functions as "the other" batch task functions, out of the 15 that `R/ffmpeg.R` defines
  (`:4982-4985`, `:5251-5254`).
- (e) `?strip_metadata` Details says per-stream tags such as `handler_name` and `language`
  survive and need a re-encode to remove, but FFmpeg 9.0.1 removed both with the function's
  arguments (`:1639-1645`, the comment at `:1616`).
- (f) `?crop_video` and `?segment_video` say a FLAC stream copy into `.mp4` fails, but FFmpeg
  9.0.1 copied it (`:1441-1443`, `:4045-4046`).
- (g) `?standardize_video` says every part of the standard is an argument, but `+faststart`
  is always set (`:1684-1685`, `:1896`).
- (h) `?format_for_web_batch` says the sibling batch functions read `video_codec` and
  `audio_codec` columns, but `strip_metadata_batch()` reads neither (`:6725-6727`).
- (i) `?crop_video` See Also names `ffm_crop()` as the one pipeline function it wraps, but it
  also calls `ffm_files()` and `ffm_map()` (`:1451`, `:1405-1417`).
- (j) `?crop_video_batch` says a per-row `width` or `height` that is neither a positive
  number nor an FFmpeg expression is refused, but any single string passes and an integer
  such as `5L` is refused, and the page never says what an FFmpeg expression is
  (`:6557-6558`, `R/utils.R:295-318`).

Also in: a code comment that repeats one of these claims, and an edit to the series'
`NEWS.md` entry.

**Out:** the setup and audio page claims → M144. The `ffm_*()` page claims → M145. The
integer refusal behind (j) → the `check_dim()` candidate row in `cairn/ROADMAP.md`.

## Acceptance criteria

- [x] AC1: For each claim (a) to (j) under Scope In, the roxygen source and every
      `man/*.Rd` page generated from it no longer make the claim, and the replacement text
      states what the code at head does, read against the code site that claim names.
- [x] AC2: The replacements for claims (e) and (f) state the behavior that runs on this
      machine show, name the containers and the FFmpeg version those runs used, and say that
      other FFmpeg versions can differ.
- [x] AC3: Every hunk of `git diff <merge-base>..HEAD -- R/ man/` changes, or re-wraps within
      an edited paragraph, a roxygen line, a code comment, a string literal in a documentation
      helper whose output feeds only roxygen, or a generated Rd line, each carrying one of
      claims (a) to (j) or its replacement. No other R line changes.
- [x] AC4: The series' `NEWS.md` bullet under `## Documentation` also names the functions or
      groups of functions this milestone corrects, no second bullet is added, and the bullet
      names no milestone.
- [ ] AC5: `devtools::document()` leaves no diff, `devtools::test()` passes, and
      `devtools::check()` gives 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2
- AC3 → T1, T2, T3, T5
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Correct claims (a), (b), (g) and (h) in `R/ffmpeg.R`, with the comment at `:1509`.
- [x] T2: Run `strip_metadata()` on an `.mp4` and an `.mkv` fixture with `handler_name` and
      `language` stream tags, and run `crop_video()` and `segment_video(reencode = FALSE)` with
      a FLAC track into `.mp4`. Record each result and the FFmpeg version in the work log, then
      correct claims (e) and (f), with the comment at `:1616`.
- [x] T3: Correct claims (c), (d), (i) and (j) in `R/ffmpeg.R`.
- [x] T4: Edit the series' `NEWS.md` bullet to name these pages. Avoid the phrases that
      `test-timeout-silence.R` fences out of `NEWS.md`.
- [x] T5: Run `devtools::document()`, then the full `devtools::test()` and `devtools::check()`.
      Read each changed sentence against its code site and record one line per claim in the
      work log.

## Work log

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: feature series: M144 -> M145 -> M146
- 2026-10-18: plan gate chose text-only correction over changing the code to match the text, at the user's choice; falsified by a corrected page that must describe a behavior no caller can rely on.
- 2026-10-18: the M130 ledger's `?anonymize_video` copy of claim (f) is gone at head; the same sentence is on `?segment_video`, so (f) covers that page.
- 2026-10-18: criteria audit (full mode, fresh Opus reader; logged in full in M144) findings for M146, fixed: (e)'s run covers `.mp4` and `.mkv`, since they store `language` differently; (f)'s run adds `segment_video(reencode = FALSE)`, a different copy path; AC2 names the containers tested; AC3 allows doc-helper literals and re-wrapping; AC4 names functions or groups and no milestone.
- 2026-10-18: absorbs the M130 part of candidate row "Six help-text claims on the `ffm_*()` input, output and run pages that were false before M128, kept as they were.".
- 2026-10-18: T1 done: (a) `?format_for_web` says an odd width or height is cropped down; (b) `?format_for_web_batch` names `audio_stream` and `quality` as its per-row settings; (g) `?standardize_video` names its arguments and says `+faststart` is always set; (h) the sibling sentence now says other batch functions read the codec columns, with two named; the recipe comment and the batch body comment that repeated (a)/(b) corrected. Tests: 0 failed, 18801 passed.
- 2026-10-18: T2 runs, FFmpeg 9.0.2 (Homebrew, macOS; the plan's 9.0.1 is no longer installed), fixtures built with lavfi `testsrc` + `sine`, libx264 + aac, stream tags set by `-metadata:s`: `strip_metadata()` on `.mp4` turned `handler_name` SecretCam/SecretMic into VideoHandler/SoundHandler and `language` fra/deu into und; on `.mkv` both tags were gone (only DURATION left). An h264 + FLAC `.mkv` into `.mp4`: `crop_video()`, `segment_video(reencode = FALSE)` and `segment_video(reencode = TRUE)` each wrote a FLAC track, no error.
- 2026-10-18: T2 done: (e) `?strip_metadata` Details states the run's per-container result and the version; (f) `?crop_video` and `?segment_video` drop the FLAC example and state the run; the `:1616` comment corrected. Tests: 0 failed, 18801 passed.
- 2026-10-18: T3 done: (c) both batch See Also lines say the single-input form "uses the same command recipe"; (d) "two other batch task functions"; (i) `?crop_video` names `ffm_files()`, `ffm_crop()`, `ffm_map()` "among the pipeline functions it wraps", matching `?format_for_web`'s phrasing; (j) lists the refused values and defines an FFmpeg expression. (j) read by a run: 5L, NA, -1, 0 refused, "in_w/2", "banana", 5 accepted, as column and argument. Tests: 0 failed, 18801 passed.
- 2026-10-18: T4 done: one sentence added to the series bullet naming the nine task pages; no fenced phrase and no milestone number in `NEWS.md`; no behavior claim added, so no test is owed. The simple-english hook flagged 211 hits over the whole file, all in older entries outside AC3's scope; left unchanged. Tests: 0 failed, 18801 passed.
- 2026-10-18: T5 read, one line per claim against code at head: (a) `ffm_crop(width = "floor(in_w/2)*2", …)` is w-1 for odd w, so "cropped down to the even value below it" holds; (b) the batch fan-out reads `quality` and `audio_stream` from the row, other picks absent, so the per-row sentence holds once it excepts the paths (refined); (c) both batch bodies call the shared `*_pipeline()`; (d) "two other" holds over 15 batch functions; (e) per the T2 runs; (f) per the T2 runs; (g) `ffm_output_options(p, "-movflags +faststart")` is unconditional in `standardize_pipeline()`; (h) `standardize_video_batch()` and `crop_video_batch()` both pick `video_codec` and `audio_codec`; (i) `crop_video_pipeline()` calls `ffm_files()`, `ffm_crop()`, `ffm_map()`; (j) per the T3 run and `check_dim()`. The recipe comment's "only knobs" now names `hardware` and `fallback` too (refined).
- 2026-10-18: claim audit: 25 claims read, 0 corrected — R/ffmpeg.R, man/crop_video.Rd, man/crop_video_batch.Rd, man/format_for_web.Rd, man/format_for_web_batch.Rd, man/segment_video.Rd, man/standardize_video.Rd, man/standardize_video_batch.Rd, man/strip_metadata.Rd, man/strip_metadata_batch.Rd, NEWS.md (fresh Opus reader; it also re-ran FFmpeg 9.0.2 and found `-map_metadata -1` is the option that resets the `.mp4` stream tags).
- 2026-10-18: T5 done: `devtools::document()` run twice, the second wrote nothing; `devtools::test()` 0 failed, 18801 passed; `devtools::check()` 0 errors, 0 warnings, 0 notes in its summary. The spelling test's Rout differs on "encodable" (`ffm_batch.Rd`, `ffm_run.Rd`) and "filesystem" (`NEWS.md` 0.2.2 entry), all present on master, so not from this branch. Status → review.

## Decisions

## Review

- 2026-10-18 review pass 1. Branch synced: merge-base = origin/master = ce2dcf9f, nothing to merge. No PR open, Copilot round off.
- AC1 evidence: `git grep` at HEAD over R/ and man/ for each claim's base wording (padded down, no per-row settings, sibling batch functions read, single-input form it wraps, for the other batch, FLAC in .mp4, every part of the standard, the pipeline function it wraps, neither a positive number nor an FFmpeg expression) finds none on the nine scoped pages; master has them. The remaining hits are on pages outside (a)-(j): `?anonymize_video_batch`, `?extract_audio_batch`, `?convert_audio_batch`, `?separate_audio_video_batch` (follow-up). Each replacement was read against its code site in the T5 work-log line and by the fresh claim auditor (25 claims, 0 wrong). Pass.
- AC2 evidence: `man/strip_metadata.Rd` names FFmpeg 9.0.2, the `.mp4` result (VideoHandler/SoundHandler, und) and the `.mkv` result (neither tag), and says other versions can differ; `man/crop_video.Rd` and `man/segment_video.Rd` name FFmpeg 9.0.2, `.mkv` into `.mp4`, and the same sentence. These match the T2 runs on this machine (FFmpeg 9.0.2). Pass.
- AC3 evidence: `git diff master...HEAD -- R/` filtered to +/- lines that do not start with `#` after whitespace prints nothing, so every changed R line is roxygen or a comment. The three comment hunks carry (a)/(b), (e) and (b); every roxygen hunk carries one of (a)-(j). The 9 Rd files changed are the nine scoped pages. Pass.
- AC4 evidence: the dev section's `## Documentation` has 1 bullet (count by awk); the diff adds one sentence to it naming the nine task pages; no `M<NN>` token in the dev section. Pass.
- Gate: `cairn_validate.py` all checks passed (exit 0); no DESIGN principle changed, so no `cairn_impact`; `pkgdown::check_pkgdown()` no problems; README.Rmd not touched; no new top-level file; changelog entry present (AC4). Prose sweep (`tools/doc_prose_report.R`) over the nine pages: base 0 findings, head 1 (`[41 words]`, prior-review #1), after the fix 0, exit 0.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: `?strip_metadata` says the tags "were removed", then that the `.mp4` output kept generic values — fix now (reworded: source values did not reach the output; `.mp4` had generic values, `.mkv` neither tag)
- diff-bug #2: the `:1620` comment says the options "dropped" the tags from `.mp4` too — fix now (names `-map_metadata -1`, reset in `.mp4`, dropped in `.mkv`)
- diff-bug #3: "the only per-row settings" overreaches, since `ffm_batch()`'s `verify` can read job columns — fix now ("the only per-row settings of each command")
- diff-bug #4: the (j) refusal list sits under `@param hardware,fallback`, not `width,height` — follow-up (placement predates the branch; row "Batch See Also claims left after M146")
- diff-bug #5: "the codec" is singular though both codecs are arguments; the even-crop step is not named — fix now for the first half ("the video and audio codecs"); reject the second half (false: the sentence does not claim to list every step, and Details states the crop)
- diff-bug #6: FFmpeg-version sentences omit the platform that M145's pages name — fix now ("on macOS" added on all three pages and the comment)
- diff-bug #7: ragged or long roxygen and NEWS wraps — reject (style; the 140-character line was re-wrapped by fix #3 anyway)
- blame-history #1: D017 names FLAC into `.mp4` as its failing example, which the new help text contradicts — fix now (D107 annotates D017)
- blame-history #2: `?strip_metadata` no longer says the function does not ensure per-stream removal or what a user can do — fix now (adds "Other FFmpeg versions can differ, so if these tags matter, read them in the output with `ffprobe()`")
- blame-history #3: the comment contradicts the doc on `.mp4` — fix now (same fix as diff-bug #2)
- blame-history #4: the (j) text documents a gap that #62's hotfix listed as refused — follow-up (existing row "`check_dim()` integer refusal")
- blame-history #5: the explicit "sibling batch functions … this one does not" contrast was folded away — reject (planned change: that contrast was claim (h), which was false)
- prior-review #1: the new (j) sentence is 41 words, over the plain-docs 25-word cap — fix now (split into four sentences; sweep clean)
- prior-review #2: "were removed" vs "kept generic values" on `?strip_metadata` and the comment — fix now (same fix as diff-bug #1, #2)
- prior-review #3: platform omitted from the FFmpeg-version sentences — fix now (same fix as diff-bug #6)
- prior-review #4: measured run results in user docs, against plain-docs rule 4 — reject (planned change: AC2 requires the version and containers on the page)
- prior-review #5: ragged and long roxygen wraps — reject (style)
- prior-review #6: NEWS orphan line — reject (style)
- prior-review #7: "wraps" applied to `crop_video()` — reject (false: `crop_video_pipeline()` calls all three named functions, read at T5 and by the claim auditor)
- Implementer's AC1 grep found the same false claim shapes on four pages outside (a)-(j) — follow-up (new row "Batch See Also claims left after M146")
