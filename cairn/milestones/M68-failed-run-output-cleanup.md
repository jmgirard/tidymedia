# M68: A failed run removes the broken output it wrote

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m68-failed-run-output-cleanup`

## Goal

An FFmpeg run that fails leaves no output file behind.

## Scope

**In:** `ffm_run()` deletes the pipeline's output path when its FFmpeg
invocation exits non-zero, and says so in the abort it already raises
(`R/ffm.R:1384-1406`). One rule, whatever the path held before: FFmpeg
truncates a pre-existing output to zero bytes before failing anyway
(measured 2026-08-09, ffmpeg 8.1.2 macOS), so there is nothing left to
preserve. The single exception is `overwrite = FALSE` against a path that
already existed, where the package promised not to replace it (AC8). Every
execution path inherits this, `ffm_batch()` included, because each reaches
this one site.

**Out:**
- `separate_audio_video()`'s video command still never runs once its audio
  command aborts → stays the candidate row this milestone was split from.
- The concat verbs' unconditional `-map 0`, which fails on a subtitle-bearing
  `.mkv` into `.mp4` → same candidate row.
- Any partial-output policy for a run the user interrupts (SIGINT), which is
  not a non-zero FFmpeg exit → no row; raise one if it is ever reported.
- Classifying *why* FFmpeg failed → the existing separation-diagnostics
  candidate row.

## Acceptance criteria

- [ ] **AC1.** When `ffm_run()`'s FFmpeg invocation exits non-zero and the
      pipeline allows overwriting, the pipeline's output path does not exist
      after the abort. Evidence: an execution test provoking an AAC-to-MP3
      stream copy — a refusal no FFmpeg build can avoid, unlike the
      version-dependent adts multi-stream refusal M45 paid for — twice, once
      with the output path absent beforehand and once with it pre-written with
      content, asserting the path is absent after each abort; and the same
      test re-run with the removal stubbed out, red, with its failure output
      quoted.
- [ ] **AC2.** The change stays confined: one removal site, no user-facing
      switch. Evidence: `grep -rn "unlink(\|file.remove(" R/` returns the
      pre-existing `R/program_management.R` line and exactly one new line,
      inside `ffm_run()`; `grep -rn "ffm_run(" R/` shows every caller reaches
      it rather than removing anything itself; `git diff master..HEAD --
      NAMESPACE man/` adds no export and no `\usage` argument; and
      `grep -rn "getOption(" R/` returns the same single site on both refs.
- [ ] **AC3.** The abort `ffm_run()` raises at `R/ffm.R:1398` names the file
      it removed. Evidence: `expect_snapshot()` of that abort, recorded under
      `devtools::test()` (never `test_file()` — M50's lesson).
- [ ] **AC4.** `separate_audio_video()`'s multi-track abort still carries
      `ffm_run()`'s condition as its `parent`, so AC3's sentence reaches that
      caller. Evidence: a test asserting the caught condition's class is
      `tidymedia_multitrack_separation` and its `$parent` message matches
      AC3's wording.
- [ ] **AC5.** A failed batch row's output path is absent and a succeeding
      row's is present. Evidence: a two-row execution test through
      `ffm_batch()` asserting both paths and `success == c(FALSE, TRUE)`.
- [ ] **AC6.** `NEWS.md` carries an entry stating the removal, naming no
      milestone number, and AC1's stubbed-out run is the test that fails
      without the behavior the entry asserts.
- [ ] **AC7.** The profile's `verify` slot is clean and its fuller
      pre-review check passes: `devtools::document()` no diff,
      `devtools::test()` clean, `devtools::check()` 0 errors / 0 warnings.
- [ ] **AC8.** `overwrite = FALSE` never costs the caller a file, and never
      strands one either: the removal is skipped only for an output that
      existed before the run. Evidence: direct tests of the removal helper
      over the four combinations of `overwrite` (`TRUE`/`FALSE`) and
      pre-existence, asserting removal in three and preservation only for
      `overwrite = FALSE` against a pre-existing path.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T5, T6
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7
- AC8 → T2, T8

## Tasks

- [ ] **T1.** Write the failing regression test: provoke an AAC-to-MP3 stream
      copy through a compiled pipeline, both pre-states, asserting the output
      path is absent. Confirm red against `master` before T2.
- [ ] **T2.** Add `remove_failed_output()` beside `ffm_run()` in `R/ffm.R` and
      call it from `ffm_run()` (`R/ffm.R:1396-1403`) on a non-zero status
      before raising; name the removed file in the abort's bullets, and say
      so when the removal itself fails.
- [ ] **T3.** Snapshot the new abort; record under `devtools::test()`.
- [ ] **T4.** Add the parent-chain test through `separate_audio_video()`
      against `run_separation_audio()` (`R/ffmpeg.R:617-656`).
- [ ] **T5.** Add the two-row batch execution test; confirm `run_one()`
      (`R/ffm_batch.R:127`) needs no removal code of its own, and run AC2's
      two greps.
- [ ] **T6.** NEWS.md entry; run AC6's diff and grep against `master`.
- [ ] **T7.** `devtools::document()`, `devtools::test()`, `devtools::check()`;
      record a decision entry for reading and writing the filesystem after a
      failed run — it is not a probe under the executing-path licence, which
      governs running a binary, and D040 already licensed a filesystem read.
- [ ] **T8.** Unit-test `remove_failed_output()` over the four `overwrite` ×
      pre-existence combinations (AC8).

## Work log

- 2026-08-09: created by /milestone-plan.
- 2026-08-09: implement started; branch `m68-failed-run-output-cleanup` cut from master at dfa219d.
- 2026-08-09: plan gate chose one unconditional removal over deleting only a path the run itself created because FFmpeg truncates a pre-existing output to zero before failing, so preservation preserves nothing while shipping two behaviors and a pre-run stat; falsified by a measured FFmpeg failure mode leaving a pre-existing output's bytes intact.
- 2026-08-09: plan chose siting the removal in `ffm_run()` over each Layer-2 verb because IP1 keeps execution in Layer 1 once; falsified by a verb needing to fail without removing, e.g. one keeping the partial file for diagnosis.
- 2026-08-09: amendment gate — AC1's adts trigger replaced by an AAC-to-MP3 stream copy, since ffmpeg 6.1.1 on ubuntu-latest writes the multi-stream .aac and exits 0 (M45's lesson, recorded in test-separate-av-multitrack.R); measured 234 with a zero-byte leftover on 8.1.2 for the replacement.
- 2026-08-09: amendment gate — added AC8 and T8 for the `overwrite = FALSE` guard chosen at the implement gate, narrowed to a pre-existing output so a non-overwriting run that creates a zero-byte file still gets it removed.
- 2026-08-09: criteria audit ([O], fresh context) returned 11 findings; ten fixed in the drafted wording (unbounded promises in AC1/AC3/AC4/AC5, a non-discriminating control, AC3 snapshotting the Layer-2 abort rather than `ffm_run()`'s, "unconditional" contradicting a two-disposition design, an unevidenced counterfactual), and its AC2 satisfiability finding became the gate question the first line above records.

## Decisions

## Review
