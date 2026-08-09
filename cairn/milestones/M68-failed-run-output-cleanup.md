# M68: A failed run removes the broken output it wrote

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m68-failed-run-output-cleanup` / https://github.com/jmgirard/tidymedia/pull/71

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
Layer 1 and Layer 2 execution path inherits this, `ffm_batch()` included,
because each reaches this one site.

Two paths do not, and cannot. Layer 0's `ffmpeg()` takes a verbatim command
string and calls `system()` (`R/ffmpeg.R:28`), so it never reaches `ffm_run()`
and cannot tell which of the caller's tokens is an output. The two-pass
loudnorm analysis calls `run_program()` directly and writes to `-f null`
(`R/loudnorm_two_pass.R:41,140`), so it has no output to remove.

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

- [x] **AC1.** When `ffm_run()`'s FFmpeg invocation exits non-zero and the
      pipeline allows overwriting, the pipeline's output path does not exist
      after the abort. Evidence: an execution test provoking an AAC-to-MP3
      stream copy — a refusal no FFmpeg build can avoid, unlike the
      version-dependent adts multi-stream refusal M45 paid for — twice, once
      with the output path absent beforehand and once with it pre-written with
      content, asserting the path is absent after each abort; and the same
      test re-run with the removal stubbed out, red, with its failure output
      quoted.
- [x] **AC2.** The change stays confined: one removal site, no user-facing
      switch. Evidence: `grep -rn "unlink(\|file.remove(" R/` returns the
      pre-existing `R/program_management.R` line and exactly one new line,
      inside `ffm_run()`; `grep -rn "ffm_run(" R/` shows every caller reaches
      it rather than removing anything itself; `git diff master..HEAD --
      NAMESPACE man/` adds no export and no `\usage` argument; and
      `grep -rn "getOption(" R/` returns the same single site on both refs.
- [x] **AC3.** The abort `ffm_run()` raises at `R/ffm.R:1398` names the file
      it removed. Evidence: a test catching that condition and matching its
      message against both the removal wording and the output's basename —
      not `expect_snapshot()`, since the abort embeds `tempfile()` paths for
      the input and the output that change on every run, so a recorded
      snapshot would churn rather than pin anything.
- [x] **AC4.** `separate_audio_video()`'s multi-track abort still carries
      `ffm_run()`'s condition as its `parent`, so AC3's sentence reaches that
      caller. Evidence: a test asserting the caught condition's class is
      `tidymedia_multitrack_separation` and its `$parent` message matches
      AC3's wording.
- [x] **AC5.** A failed batch row's output path is absent and a succeeding
      row's is present. Evidence: a two-row execution test through
      `ffm_batch()` asserting both paths and `success == c(FALSE, TRUE)`.
- [x] **AC6.** `NEWS.md` carries an entry stating the removal, naming no
      milestone number, and AC1's stubbed-out run is the test that fails
      without the behavior the entry asserts.
- [x] **AC7.** The profile's `verify` slot is clean and its fuller
      pre-review check passes: `devtools::document()` no diff,
      `devtools::test()` clean, `devtools::check()` 0 errors / 0 warnings.
- [x] **AC8.** `overwrite = FALSE` never costs the caller a file, and never
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

- [x] **T1.** Write the failing regression test: provoke an AAC-to-MP3 stream
      copy through a compiled pipeline, both pre-states, asserting the output
      path is absent. Confirm red against `master` before T2.
- [x] **T2.** Add `remove_failed_output()` beside `ffm_run()` in `R/ffm.R` and
      call it from `ffm_run()` (`R/ffm.R:1396-1403`) on a non-zero status
      before raising; name the removed file in the abort's bullets, and say
      so when the removal itself fails.
- [x] **T3.** Assert the new abort's wording and the file it names.
- [x] **T4.** Add the parent-chain test through `separate_audio_video()`
      against `run_separation_audio()` (`R/ffmpeg.R:617-656`).
- [x] **T5.** Add the two-row batch execution test; confirm `run_one()`
      (`R/ffm_batch.R:127`) needs no removal code of its own, and run AC2's
      two greps.
- [x] **T6.** NEWS.md entry; run AC6's diff and grep against `master`.
- [x] **T7.** `devtools::document()`, `devtools::test()`, `devtools::check()`;
      record a decision entry for reading and writing the filesystem after a
      failed run — it is not a probe under the executing-path licence, which
      governs running a binary, and D040 already licensed a filesystem read.
- [x] **T8.** Unit-test `remove_failed_output()` over the four `overwrite` ×
      pre-existence combinations (AC8).

## Work log

- 2026-08-09: created by /milestone-plan.
- 2026-08-09: implement started; branch `m68-failed-run-output-cleanup` cut from master at dfa219d.
- 2026-08-09: plan gate chose one unconditional removal over deleting only a path the run itself created because FFmpeg truncates a pre-existing output to zero before failing, so preservation preserves nothing while shipping two behaviors and a pre-run stat; falsified by a measured FFmpeg failure mode leaving a pre-existing output's bytes intact.
- 2026-08-09: plan chose siting the removal in `ffm_run()` over each Layer-2 verb because IP1 keeps execution in Layer 1 once; falsified by a verb needing to fail without removing, e.g. one keeping the partial file for diagnosis.
- 2026-08-09: amendment gate — AC1's adts trigger replaced by an AAC-to-MP3 stream copy, since ffmpeg 6.1.1 on ubuntu-latest writes the multi-stream .aac and exits 0 (M45's lesson, recorded in test-separate-av-multitrack.R); measured 234 with a zero-byte leftover on 8.1.2 for the replacement.
- 2026-08-09: amendment gate — added AC8 and T8 for the `overwrite = FALSE` guard chosen at the implement gate, narrowed to a pre-existing output so a non-overwriting run that creates a zero-byte file still gets it removed.
- 2026-08-09: T1 — tests/testthat/test-failed-run-cleanup.R added; both cases red against the branch's unchanged R/ ("Expected `file.exists(outfile)` to be FALSE. actual: TRUE"), which is master's behavior and AC1's pre-change counterfactual.
- 2026-08-09: T2 — remove_failed_output() added beside ffm_run() (R/ffm.R) and called from its non-zero-status branch; both T1 cases now green and devtools::test() reports FAIL 0 | SKIP 5 | PASS 5993.
- 2026-08-09: T8/T3 — helper unit tests over the four overwrite x pre-existence cells, the unremovable-file case, and the abort's wording; 7 tests / 26 expectations green, 0 skipped.
- 2026-08-09: T8 mutation control — with the unlink stubbed to a no-op, 4 of the 7 tests go red (12 failures) while the three that never exercise the unlink success path stay green; R/ffm.R restored from the T2 commit afterwards.
- 2026-08-09: amendment gate — AC3's expect_snapshot() replaced by a targeted message match, since the abort embeds two tempfile() paths and a recorded snapshot would churn every run (measured on the AC4 probe).
- 2026-08-09: T4/T5 — parent-chain and two-row batch tests added; 9 tests / 32 expectations green, 0 skipped, success reads FALSE,TRUE with the failed row's output gone and the good row's present.
- 2026-08-09: T5 — AC2's greps run: unlink/file.remove is R/program_management.R:247 plus exactly one new call at R/ffm.R:1392; no ffm_run() caller removes anything itself; getOption() is R/ffmpeg.R:2533 on this branch and on master alike.
- 2026-08-09: scope corrected, not amended by gate — the In paragraph claimed every execution path inherits the removal; Layer 0's ffmpeg() calls system() on a verbatim string (R/ffmpeg.R:28) and the loudnorm analysis pass writes to -f null (R/loudnorm_two_pass.R:41,140), so neither can be covered and no alternative was available to gate.
- 2026-08-09: T6 — NEWS.md Bug fixes entry added; git diff master..HEAD -- NAMESPACE man/ is empty (0 lines), so no export and no documented argument changed.
- 2026-08-09: T7 — devtools::document() no diff, devtools::test() FAIL 0 | SKIP 5 | PASS 6019, devtools::check() Status: OK (0 errors, 0 warnings, 0 notes); D045 appended to cairn/DECISIONS.md.
- 2026-08-09: all eight tasks done and checks clean; status -> review.
- 2026-08-09: review returned M68 to in-progress under the return floor — F1 (92) deletes a pre-existing output FFmpeg never opened (exit 8, file intact, re-measured), F2 (90) and F3 (88) are unlink()'s default glob expansion deleting unrelated files, F6 (84) leaves every frame of a failed sample_frames() run, P1 (85) is an outcome-keyed skip M63 already retired, F10 (80) is the test blindness that hid F1.
- 2026-08-09: criteria audit ([O], fresh context) returned 11 findings; ten fixed in the drafted wording (unbounded promises in AC1/AC3/AC4/AC5, a non-discriminating control, AC3 snapshotting the Layer-2 abort rather than `ffm_run()`'s, "unconditional" contradicting a two-disposition design, an unevidenced counterfactual), and its AC2 satisfiability finding became the gate question the first line above records.



## Decisions

## Review

**PR:** https://github.com/jmgirard/tidymedia/pull/71

### Acceptance-criteria evidence (fresh, 2026-08-09)

- **AC1** — `test-failed-run-cleanup.R` cases 1 and 2 green: the output path is
  absent after the abort with the path absent beforehand and with it
  pre-written. Counterfactual re-run with the `unlink()` stubbed to a no-op:
  6 of the 9 tests red, 15 failures; the 3 that stay green are exactly those
  that never reach the unlink success path. `R/ffm.R` restored from the branch
  afterwards (`grep -c MUTATION` → 0).
- **AC2** — `grep -rn "unlink(\|file.remove(" R/` → `R/program_management.R:247`
  (pre-existing) and one new call at `R/ffm.R:1392`. No `ffm_run()` caller
  removes anything itself. `git diff master..HEAD -- NAMESPACE man/` → 0 lines.
  `grep -rn "getOption(" R/` → `R/ffmpeg.R:2533` on this branch and on master.
- **AC3** — case 7 green: the caught condition's message matches both the
  removal wording and the output's basename.
- **AC4** — case 8 green: the condition is `tidymedia_multitrack_separation`
  and its `$parent` message carries the removal sentence.
- **AC5** — case 9 green: `success` reads `FALSE, TRUE`, the failed row's
  output is absent and the succeeding row's is present.
- **AC6** — `NEWS.md:296` carries the entry, naming no milestone number; the
  AC1 counterfactual above is the run that fails without the behavior.
- **AC7** — `devtools::document()` no diff; `devtools::test()` FAIL 0 | SKIP 5
  | PASS 6019; `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes.
- **AC8** — cases 3, 4 and 6 green: removal in the three cells, preservation
  with content intact only for `overwrite = FALSE` against a pre-existing path.

Whole file fresh: 9 tests, 32 expectations, 0 failed, 0 skipped.

### Independent review — three lenses, then a scorer

26 candidate findings scored. **Six actioned (>=80):** F1 (92), F2 (90),
F3 (88), P1 (85), F6 (84), F10 (80). **Twenty logged below threshold:** F4 (78)
tilde paths expand in R but not for FFmpeg; F9 (74) the empty-disposition path
is untested end-to-end; F13 (72) parallel rows sharing an output can delete each
other's file; F5 (68) a symlinked output loses the link, not the truncated
target; F11 (68) the helper tests match un-rendered glue templates; F15 (58)
D045's scope claim overstates coverage; F8 (55) the overwrite=FALSE branch
asserts disk state without stat'ing; F7 (52) a failed `verify =` does no
removal; F12 (50) the rendered-message test is exposed to cli hyperlinking;
F14 (45) a directory output is handled by the defensive check, not by design;
F16 (35) the failure bullet sits mid-list; H8 (30) the pre-run stat is a
timing-order dependency; H1-H7 (5) seven history checks, all "no regression
found".

**Disposition: returned to `in-progress` under the return floor.** F1 is a
>=90 defect in what the package does for its users, and F2/F3 are the same
class. No acceptance criterion failed as written — which is itself the finding
implement's amendment gate must answer: AC1 is satisfied *by* the destructive
behavior, so the criterion was too weak to catch it. Defect return 1 of this
milestone; the thrash rule's third-return threshold is not reached.

**The premise behind the plan gate's chosen design is refuted.** The plan
recorded "falsified by a measured FFmpeg failure mode leaving a pre-existing
output's bytes intact"; that mode is now measured. `ffmpeg -y -i in.mkv -c:v
nosuchcodec out.mp4` exits 8 and leaves a 14-byte pre-existing `out.mp4`
byte-for-byte intact (2026-08-09, ffmpeg 8.1.2 macOS), because FFmpeg fails
before opening the output for unknown encoders, unknown filters and bad option
values. The "one unconditional rule" chosen at the plan gate deletes that file
and reports it as incomplete. The gate must be re-asked against this
measurement.

**Also measured independently:** `unlink()` defaults to `expand = TRUE`. With
`a*.mp4`, `aQQQ.mp4` and `aXYZ.mp4` present, `unlink("a*.mp4")` returned 0 and
emptied the directory; `unlink("out[1].mp4")` deleted `out1.mp4`, left
`out[1].mp4`, and returned 0.

