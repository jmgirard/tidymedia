<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M27: Metadata scrubbing for de-identification (`strip_metadata` + `_batch`)

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1, GP1   <!-- owner: plan · works under; adds no principle -->
- **Branch/PR:** `m27-strip-metadata` · https://github.com/jmgirard/tidymedia/pull/29   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add task verbs that strip identifying container/global metadata and chapters
via lossless stream-copy — the IRB/de-identification front door (M25 survey K2).

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**

- `strip_metadata(infile, outfile, run = TRUE)` — zero-config strip-all Layer-2
  wrapper (IP1) over `ffm_files` → `ffm_copy` → `ffm_output_options("-map_metadata -1",
  "-map_chapters -1", "-fflags +bitexact")`; binary-free compile.
- `strip_metadata_batch(jobs, run, parallel, ...)` — table-driven sibling (D014
  `_batch`), thin `ffm_batch` wrapper reusing shared `strip_metadata_pipeline()`
  for per-row parity (M13); `input` column required, optional `output` overrides
  derived `<base>_stripped.<ext>`; duplicated **resolved** outputs rejected (M26).
- Empirical guard: global tags (`creation_time`, `location`, `make`, `model`,
  `title`, `comment`) + chapters gone; bitexact stops re-added `creation_time`/
  `encoder`; A/V streams + rotation matrix survive.
- Docs: roxygen (`@family`, `@seealso` `anonymize_video()`/`probe_*`/`mediainfo_*`)
  documenting the boundary — per-stream tags (`handler_name`, `language`) and
  codec-embedded identifiers are *not* removed by a stream copy.

**Out:**

- Per-stream metadata clearing → documented limitation (would need probing stream
  count, breaking binary-free compile, D002/D013); candidate row if later wanted.
- Re-encode scrubbing of codec-embedded identifiers → out (GP1/D001; escape hatch).
- Visual de-identification (faces/regions) → `anonymize_video()` + region-blur candidate.
- New `_pkgdown.yml`/vignette prose beyond the two verbs' index entries → routine
  doc sync only (M23).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] **AC1 (compile, binary-free):** `strip_metadata(infile, outfile, run = FALSE)`
      returns a command containing `-map_metadata -1`, `-map_chapters -1`,
      `-fflags +bitexact`, `-codec:v copy`, `-codec:a copy`, and `-map 0`, with no
      FFmpeg invocation. Tested purely.
- [x] **AC2 (tags clear — the "which tags clear" guard):** on a fixture carrying
      known global tags (`creation_time`, `location`, `make`, `model`, `title`,
      `comment`) plus chapters, the stripped output's ffprobe `format` and stream
      `tags` contain **none** of those tags and **no** chapters, and carry **no**
      re-added `encoder` or `creation_time` tag. Execution test; `skip_if` no ffmpeg.
- [x] **AC3 (A/V preserved — the "does copy preserve A/V" guard):** the stripped
      output keeps the source video and audio streams (codecs, dimensions,
      duration within tolerance) and, when the source carries a rotation display
      matrix, that rotation survives. Verified by probing. Execution test.
- [x] **AC4 (batch):** `strip_metadata_batch(jobs, run = FALSE)` returns the jobs
      tibble plus a `command` column with one stream-copy strip command per row;
      output names derive to `<base>_stripped.<ext>` when no `output` column is
      given; duplicated resolved output paths are rejected (M26). Tested purely.
- [x] **AC5 (suite + check):** `devtools::test()` passes with the new tests;
      `devtools::check()` reports **0 errors / 0 warnings / 0 notes** — confirm via
      `00check.log` `Status: OK` after `spelling::update_wordlist()` for any new
      technical terms (M17 lesson).
- [x] **AC6 (docs):** both verbs `@export`ed and documented with the boundary
      prose and `@seealso` web above; `devtools::document()` clean and
      `pkgdown::check_pkgdown()` clean (M23 lesson); README regenerated only if a
      documented example changed (path-only churn reverted — M24 lesson).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. -->

- AC1 → T1
- AC2 → T1, T2
- AC3 → T2
- AC4 → T3
- AC5 → T2, T3, T4
- AC6 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1 — scalar verb + shared pipeline.** `strip_metadata_pipeline()` (`ffm_files`
      → `ffm_copy` → `ffm_output_options` with the three flags) + `strip_metadata()`
      thin wrapper with front-door guards; pure compile test (AC1). (`R/ffmpeg.R`)
- [x] **T2 — execution guard + fixture.** `test-strip-metadata.R`: tagged+rotated
      fixture, assert tags/chapters gone, no re-added encoder/creation_time (AC2),
      A/V + rotation survive (AC3).
- [x] **T3 — batch sibling.** `derive_stripped_names()` + `strip_metadata_batch()`
      reusing the pipeline, type/NA + duplicate-resolved-path guard (M26);
      `test-strip-metadata-batch.R` (AC4).
- [x] **T4 — docs + gate.** Roxygen (boundary prose, `@seealso`, `@family`);
      `document()`; `pkgdown::check_pkgdown()`; `spelling::update_wordlist()`;
      `check()` 0/0/0 + full `test()` green (AC5, AC6).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-13: created by /milestone-plan (gate: global+chapters+`-fflags +bitexact`
  scrub, batch sibling in, zero-config surface; per-stream stripping ruled out to
  keep compile binary-free).
- 2026-07-13: T1 — `strip_metadata_pipeline()` + `strip_metadata()` (`R/ffmpeg.R`)
  = `ffm_copy` + `ffm_output_options("-map_metadata -1","-map_chapters -1","-fflags +bitexact")`; compile test (AC1).
- 2026-07-13: T2 — `make_tagged_video()`/`probe_format_tags()`/`probe_rotation()`
  helpers + execution guard (AC2/AC3); confirmed tags+chapters+encoder clear, A/V+rotation survive.
- 2026-07-13: T3 — `derive_stripped_names()` + `strip_metadata_batch()` reusing the
  pipeline (byte-parity), duplicated-resolved-output + NA guards (M26); 31 tests (AC4).
- 2026-07-13: T4 — `document()`; both verbs into `_pkgdown.yml` (M23); wordlist +De,IRB,
  bitstream,de,muxed; `check()` 0/0/0; `test()` 973 pass. README untouched (M24). → review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review

_Reviewed 2026-07-13 on branch `m27-strip-metadata` (PR #29), cut from
`master`@590b32a (up to date, no merge needed)._

**AC evidence (fresh):** AC1 — compiles to `-codec:v copy -codec:a copy
-map_metadata -1 -map_chapters -1 -fflags +bitexact -map 0`, no ffmpeg call.
AC2 — stripped fixture: `format_tags` only ISO brand fields; *stream* tags cleared
of `name`/`title`/`creation_time`/`encoder` (F1 test); no chapters. AC3 — codecs
`h264,aac` + duration unchanged, rotation `90` preserved. AC4 — batch returns
`input,output,command`, auto-names `<base>_stripped.<ext>`, colliding resolved
outputs abort (M26), NA guards. AC5 — `check()` **0/0/0**, suite **979 pass / 0
fail / 1 skip**, wordlist updated (M17). AC6 — exported + documented, `document()`
+ `pkgdown::check_pkgdown()` clean (M23), README untouched (M24).

**Gate:** `cairn_validate.py` PASS; Coverage complete (AC1→T1, AC2→T1/T2, AC3→T2,
AC4→T3, AC5→T2/T3/T4, AC6→T4); toolchain checks recorded above; no DESIGN
principle changed → `cairn_impact` skipped.

**Independent review (3 lenses + scorer):** blame-history clean (follows
M13/M26/D014/D007/D002/M23); prior-PR clean no-op (Codecov-only). Diff-bug raised
**F1 (88) — FIXED**: AC2 test probed only container `format_tags`, not stream
`tags`; verb behavior correct (per-stream tag *is* cleared) so a coverage gap —
added per-stream fixture tag, `probe_stream_tags()`, stream-tag assertions (972→979).
**F2 (35) — logged, not actioned**: rotation exact `== 90` may be cross-version
brittle; reviewer flagged not-a-defect, correct on tested build.

