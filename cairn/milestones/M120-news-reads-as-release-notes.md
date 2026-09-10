# M120: NEWS.md reads as release notes

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — NEWS.md ships and is the first thing a reader checks
- **Branch/PR:** `m120-news-release-notes`

## Goal

The development section of `NEWS.md` reads as one set of release notes rather than
an append-only per-milestone log.

## Scope

**In:** collapsing the 1,968-line development section into one section with each
heading appearing once, covering every export added or removed and every rename
since the 0.1.0 heading.

**Out:** naming a release version number, cutting a release section, or bumping
`DESCRIPTION` → the release walk's act (`/cairn-release`), which this milestone
leaves untouched. Rewriting the four already-released sections → not needed; each
already has no repeated heading.

## Acceptance criteria

- [x] AC1: `NEWS.md` opens with a single development-version `#` heading, and the
      development section names no release version number.
- [x] AC2: A sweep over every `#` section of `NEWS.md` finds no `##` heading repeated
      within any one section. Today the development section carries `## New features`
      at lines 3, 267 and 1402, `## Breaking changes` at 48, 523 and 1943, `## Bug
      fixes` at 868, 1845 and 1961, and `## Documentation` at 20 and 1666.
- [x] AC3: The development section names every export in the symmetric difference
      between `NAMESPACE` at head and `NAMESPACE` at commit `4b04fad9`, the commit
      that introduced the `# tidymedia 0.1.0` heading — 40 added and 14 removed as
      measured 2026-09-07.
- [x] AC4: It names every rename recorded in `cairn/DECISIONS.md` D014, D077 and D078
      since that commit which is not itself a `NAMESPACE` entry — the argument and
      option renames.
- [x] AC5: The `verify` slot of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T1, T3
- AC4 → T1, T3
- AC5 → T4

## Tasks

- [x] T1: Compute the symmetric difference of the two `NAMESPACE`s and read D014,
      D077 and D078 for the renames that are not `NAMESPACE` entries. The removed set
      measured 2026-09-07 is `:=`, `as_label`, `as_name`, `audio_as_mp3`,
      `convert_fractions`, `enquo`, `enquos`, `ffm`, `get_codecs`, `get_encoders`,
      `get_framerate`, `get_samplingrate`, `mediainfo_summary`, `pad_integers` —
      breaking changes for any 0.1.0 caller.
- [x] T2: Collapse the development section: one occurrence of each `##` heading,
      entries merged under it in reader order rather than milestone order.
- [x] T3: Check the collapsed section against T1's two lists and fill what is missing.
- [x] T4: Confirm the four released sections below are untouched; run the profile's
      `verify` slot.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: AC1's "no release version number is named" had no stated domain and was unsatisfiable over the whole file, which holds four real release headings (repaired — scoped to the development section); AC3's one-way set difference named only the 40 additions while AC4's two decision entries recovered some but not all of the 14 removals, leaving `ffm`, `pad_integers`, `convert_fractions`, `mediainfo_summary` and the `get_framerate`/`get_samplingrate` renames covered by neither (repaired — AC3 made symmetric, AC4 given D078 and narrowed to non-`NAMESPACE` renames). AC2 and AC5 passed all six questions clean.
- 2026-09-09: T1. Symmetric difference of `NAMESPACE` at head against `4b04fad9` measured
  with `comm` over the sorted `export()` lines: 40 added, 14 removed, matching the plan's
  2026-09-07 counts and its removed list exactly. All 14 removed names are already named
  once each in the development section; of the 40 added, three are named nowhere --
  `concatenate_videos_batch`, `strip_metadata`, `strip_metadata_batch` -- so T3's fill is
  those three. Non-`NAMESPACE` renames read from D014 (`acodec`/`vcodec` ->
  `audio_codec`/`video_codec`, `ts_start`/`ts_stop` -> `start`/`end`, the full-word
  compounds `pixel_format`/`sample_rate`, and `infile`/`outfile`/`infiles`/`file`), D077
  (`audio` -> `audio_input` on the four fan-in verbs; `tidymedia.nvenc_encoders` ->
  `tidymedia.hardware_encoders`; helper argument `hardware =`) and D078 (states the rule
  behind D077's applications; adds no rename of its own). Every one is already named in
  the section, so AC4 needs no fill, only preservation through the collapse.
- 2026-09-09: question gate. Three choices, all taken at the recommendation. (1) The
  development section ends with six headings -- Breaking changes, New features, Bug fixes,
  Performance, Documentation, Requirements -- so the 13 one-off announcement headings and
  the Configuration, Standardized function and argument names, Verification & provenance,
  Multi-input verbs and Safe execution headings dissolve into them; 27 headings become 6.
  (2) Bullets describing one feature at successive stages of the cycle merge into one entry
  rather than being re-filed unchanged, so prose is rewritten and the section shrinks well
  below 1,982 lines. (3) The eight stale function names the section still announces --
  `normalize_audios`, `segment_videos`, `has_nvenc`, `anonymize_videos`, `extract_frames`,
  `nvenc_encoder`, `standardize_videos`, `write_mediainfo_template`, 19 mentions -- are
  corrected to the shipped names; the `find_program()` cross-reference split is left to the
  [high] candidate row cut to hold it.
- 2026-09-09: AC2's illustrative line numbers have drifted 13 lines since 2026-09-07 (M119
  added content above them): `## New features` is at 3, 280 and 1415 rather than 3, 267 and
  1402, and the other three sets moved likewise. The criterion's binding sentence -- the
  sweep finding no `##` heading repeated within any one `#` section -- is unaffected, and
  the counts it asserts (three, three, three and two occurrences) are correct as measured
  today, so the wording is left alone rather than amended.
- 2026-09-09: T2+T3 (minor amendment: run as one editing pass rather than two, since
  the collapse rewrites the very bullets the fill lands in; task order and wording
  otherwise unchanged). The development section went from 1,982 lines under 27 `##`
  headings to 1,362 under six -- Breaking changes, New features, Bug fixes,
  Performance, Documentation, Requirements. The 13 one-off announcement headings and
  the Configuration, Standardized function and argument names, Verification &
  provenance, Multi-input verbs and Safe execution headings all dissolved into those
  six. Same-feature bullets merged: the four loudnorm headings became one two-pass
  entry, the twelve "reported against the function you called" bullets became one
  entry with a checks list and an ordering paragraph, and the eleven scattered
  `audio_stream` bullets became one Breaking entry for the behavior change plus one
  New-features entry for the argument. Intra-cycle renames were dropped rather than
  reported as breaking changes: `has_nvenc`/`nvenc_encoder`, `normalize_audios` and
  the other four plural batch names, and `convert_audio()`'s `format` argument all
  arrived and were renamed inside this cycle, so a 0.1.0 reader never saw them; the
  `_batch` verbs and the hardware helpers are now announced as the new exports they
  are. T3's fill added `strip_metadata()`, `strip_metadata_batch()` and
  `concatenate_videos_batch()`, whose prose was derived from their roxygen at
  `R/ffmpeg.R:1630-1668`, `:4888-4926` and `:7215-7250`, not composed.
- 2026-09-09: the eight stale function names the gate dispositioned are gone (sweep
  over the section for `\bname\b` returns zero for each). The eighth,
  `write_mediainfo_template()`, turned out to name no function anywhere in the
  package: `NAMESPACE` exports it not and `R/` defines it not, and the real caller of
  `check_file_exists()` is `mediainfo_template()` (`R/mediainfo.R:214`). The release
  note now names that. `R/utils.R:165-172`'s comment carries the same dead name and
  is where the note had copied it from; comment-only and outside this milestone's
  scope, so it was folded into the existing [high] `find_program()` candidate row
  rather than fixed here or given a row of its own (ROADMAP is at 59 of 60 lines).
- 2026-09-09: two tests assert on `NEWS.md`'s own wording and both failed on the
  first `verify` run -- a real regression this milestone introduced, not a
  pre-existing failure. `test-check-tracks-docs.R:126` matches the literal
  `defaults to TRUE`, which the rewrite had marked up as `` `TRUE` ``;
  `test-front-door-ordering.R:435` matches an ordering sentence the merge had
  dropped, one the two `_batch` verbs' help pages also carry. Both restored, the
  ordering sentence back in the Bug-fixes ordering paragraph where the merge should
  have put it. This is the guard the criteria could not supply: AC1-AC4 measure
  headings, exports and renames, and neither would have caught either loss.
- 2026-09-09: T4. The four released sections below the development heading are
  byte-for-byte identical to `HEAD:NEWS.md` from `# tidymedia 0.1.0` down (diff of
  the two tails is empty), so nothing below the line moved. `verify` slot clean:
  `devtools::test()` at FAIL 0 | WARN 12 | SKIP 5 | PASS 13,266. The five skips are
  all hardware-encoder probes (`test-nvenc.R:435,446,458`,
  `test-video-codec.R:480,489`); none reads `NEWS.md`. No roxygen changed, so
  `document()` was not run.
- 2026-09-09: acceptance measured. AC1: one `# tidymedia (development version)`
  heading and no tidymedia release version number in the section -- the lone `0.2.0`
  it carried ("a version of tidymedia before 0.2.0") is reworded to "an earlier
  version", which is also what Scope Out reserves for the release walk; the version
  numbers that remain are dependency and FFmpeg versions (1.1.0, 1.2.0, 2.5.0,
  3.0.3, 4.1.0, 4.5.0, 6.1.1, 9.0.1). AC2: a sweep over every `#` section finds no
  `##` heading repeated within any one of them, the four released sections included.
  AC3: all 40 added and all 14 removed exports named, matched on word boundaries.
  AC4: `audio_codec`/`video_codec` (from `acodec`/`vcodec`), `start`/`end` (from
  `ts_start`/`ts_stop`), `audio_input` (from `audio`) and
  `tidymedia.hardware_encoders` (from `tidymedia.nvenc_encoders`, now at zero
  occurrences) each named as renames; `hardware =` named. AC5 as above.
- 2026-09-09: status -> review.
