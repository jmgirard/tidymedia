# M68: A failed run removes the broken output it wrote

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m68-failed-run-output-cleanup` / https://github.com/jmgirard/tidymedia/pull/71

## Goal

An FFmpeg run that fails leaves no output file behind.

## Scope

**In:** `ffm_run()` removes what its FFmpeg invocation wrote when that
invocation exits non-zero, and says so in the abort it already raises
(`R/ffm.R:1384-1406`). The rule is *this run wrote it* — the output is stat'd
before the run and after the failure, and a file goes only where the run created
it or changed its size or timestamp; D046 carries the measurement behind it, the
`overwrite = FALSE` guard (AC8), and the image2 pattern case
(`sample_frames()`), where the rule applies to the files the pattern matches in
its own directory.
Every Layer 1 and Layer 2 path inherits this, `ffm_batch()`
included; Layer 0's `ffmpeg()` (`R/ffmpeg.R:28`) and the loudnorm analysis pass
(`R/loudnorm_two_pass.R:41,140`) do not and cannot — one runs a verbatim string
through `system()`, the other writes to `-f null`.

**Out:** `separate_audio_video()`'s video command still never runs once its
audio command aborts, and the concat verbs' unconditional `-map 0` still fails
on a subtitle-bearing `.mkv` → both stay the candidate row this milestone was
split from. Classifying *why* FFmpeg failed → the existing
separation-diagnostics candidate row. A partial-output policy for a run the user
interrupts (SIGINT) is not a non-zero exit → no row; raise one if reported.

## Acceptance criteria

- [x] **AC1.** When `ffm_run()`'s FFmpeg invocation exits non-zero and the run
      wrote to the output path, that path does not exist after the abort.
      Evidence: an execution test provoking an AAC-to-MP3 stream copy — a
      refusal no FFmpeg build can avoid, unlike the version-dependent adts
      refusal M45 paid for — with the path absent beforehand and with it
      pre-written, asserting absence after each abort; and the same test with
      the removal stubbed out, red, with its failure output quoted.
- [x] **AC2.** The change stays confined: one removal site, no user-facing
      switch. Evidence: `grep -rn "unlink(\|file.remove(" R/` returns the
      pre-existing `R/program_management.R` line and exactly one new line,
      inside `ffm_run()`; `grep -rn "ffm_run(" R/` shows every caller reaches
      it rather than removing anything itself; `git diff master..HEAD --
      NAMESPACE man/` adds no export and no `\usage` argument; and
      `grep -rn "getOption(" R/` returns the same single site on both refs.
- [x] **AC3.** The abort `ffm_run()` raises names the file it removed.
      Evidence: a test catching that condition and matching its message against
      both the removal wording and the output's basename — not
      `expect_snapshot()`, since the abort embeds `tempfile()` paths that
      change on every run.
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
- [x] **AC8.** `overwrite = FALSE` never costs the caller a file: a pre-existing
      output is preserved under it by a guard of its own, whatever FFmpeg did,
      while an output the run created is removed whatever `overwrite` said.
      Evidence: direct tests of the removal helper over the four combinations of
      `overwrite` (`TRUE`/`FALSE`) and pre-existence, asserting removal in three
      and preservation with the content intact only for `overwrite = FALSE`
      against a pre-existing path.
- [x] **AC9.** A failed run leaves a pre-existing output FFmpeg never opened
      exactly as it was. Evidence: an execution test provoking an unknown
      encoder — exit 8, raised before the output is opened — against a
      pre-written output, asserting the bytes and the mtime are unchanged and
      that the abort says the file was left alone; red against the removal this
      milestone first shipped, with that failure output quoted.
- [x] **AC10.** The removal deletes what the run wrote and nothing beside it.
      Evidence: a direct test that an output named `a*.mp4` costs neither
      `aQQQ.mp4` nor `aXYZ.mp4`, and that `out[1].mp4` goes while `out1.mp4`
      stays (R's `unlink()` globs by default, measured at review); and an
      execution test over a `%06d` frame pattern where a failed sampling run
      removes the frames it wrote while an earlier run's frames and an unrelated
      file in the same directory survive.
- [x] **AC11.** No test here skips on the outcome of the operation it tests.
      Evidence: the unremovable-file case verifies its fixture with
      `file.access(dir, mode = 2)` rather than by attempting the unlink, and
      skips only on Windows or as root, failing anywhere else — the shape
      `tm_require_unreadable()` (M63) already holds for unreadable inputs.

## Coverage

- AC1 → T1, T2, T10
- AC2 → T2, T5, T6
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5
- AC6 → T6, T13
- AC7 → T7, T13
- AC8 → T2, T8
- AC9 → T9, T10
- AC10 → T9, T10, T11
- AC11 → T12

## Tasks

- [x] **T1.** Failing regression test: an AAC-to-MP3 stream copy through a
      compiled pipeline, both pre-states, asserting the output is absent.
- [x] **T2.** Add `remove_failed_output()` beside `ffm_run()` in `R/ffm.R`,
      call it on a non-zero status before raising, name the removed file in
      the abort's bullets, and say so when the removal itself fails.
- [x] **T3.** Assert the new abort's wording and the file it names.
- [x] **T4.** Parent-chain test through `separate_audio_video()`
      (`R/ffmpeg.R:617-656`).
- [x] **T5.** Two-row batch execution test; confirm `run_one()`
      (`R/ffm_batch.R:127`) needs no removal code of its own; run AC2's greps.
- [x] **T6.** NEWS.md entry; run AC6's diff and grep against `master`.
- [x] **T7.** `devtools::document()`, `devtools::test()`, `devtools::check()`;
      record a decision entry for reading and writing the filesystem after a
      failed run — not a probe under the executing-path licence (D045).
- [x] **T8.** Unit-test `remove_failed_output()` over the four `overwrite` ×
      pre-existence combinations (AC8).
- [x] **T9.** Failing tests first for the two defects the review measured: a
      pre-existing output an unknown-encoder run never opened (AC9), and the
      glob and frame-pattern cases (AC10). Confirm each red against the branch.
- [x] **T10.** Replace the pre-run existence flag with a snapshot of the files
      the output designates (path, size, mtime) and remove, after a failure,
      only those the run created or changed; `unlink(expand = FALSE)`.
- [x] **T11.** Match an image2 `%0Nd` pattern to its own files in its own
      directory, so a frame sequence is snapshotted and removed as a set.
- [x] **T12.** Rebuild the unremovable-file test's gate on the M63 shape (AC11).
- [x] **T13.** Supersede D045 with the write-detection rule, update the NEWS
      entry to what the package now does, and re-run document/test/check.

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
- 2026-08-09: F1 re-measured this session — `ffmpeg -y -i in.mp4 -c:v nosuchcodec out.mp4` exits 8 with a pre-existing 13-byte output byte-for-byte intact (same md5, same mtime), and a pre-existing zero-byte output that FFmpeg *did* truncate came back with an unchanged size but a bumped mtime, so "did this run write here?" is answerable from a size+mtime stat.
- 2026-08-09: implement gate re-asked against that measurement — chose removing only what the run wrote over never touching a pre-existing output (which strands the zero-byte truncation), and chose deleting a failed frame run's own frames over leaving them; escalation was offered and declined.
- 2026-08-09: amendment gate — Scope In rewritten to the write-detection rule, AC1/AC8 amended, AC9/AC10/AC11 and T9-T13 added, every AC box unticked because the behavior beneath them changed; D046 appended superseding D045's unconditional half; sizing advisory now warns 11 criteria / 13 tasks, a return-driven expansion of an in-flight milestone rather than a split.
- 2026-08-09: T9 — three failing tests added: an unknown-encoder run (exit 8) against a pre-written output, the `a*.mp4` and `out[1].mp4` neighbour cases, and a frame run blocked at its third frame by a directory sitting where that file must go (exit 235 with two frames written, measured 2026-08-09 on ffmpeg 8.1.2 macOS — a trigger no build can accept, unlike a codec refusal).
- 2026-08-09: T10/T11 — `output_targets()`/`output_snapshot()` added beside `ffm_run()`; the removal now compares a pre-run snapshot with a post-failure one and unlinks only what moved, with `expand = FALSE`, and a `%0Nd` output is matched as an escaped regex over its own directory so the set is snapshotted and removed together.
- 2026-08-09: T12 — the unremovable-file case now verifies its fixture with `tm_require_unwritable_dir()` (helper-skip.R), which asks `file.access(dir, mode = 2)` and fails anywhere but Windows or root, replacing the skip keyed on the unlink under test; the file's 13 tests run with 0 skipped.
- 2026-08-09: T9-T12 controls — three mutations, each reddening only the tests whose claim it breaks: removing everything at the output rather than what the run wrote reddens the never-opened case and the frames case (an earlier run's frame goes with it); `unlink()` with its default globbing reddens the two neighbour tests; treating every output as a literal path reddens the frames case. R/ffm.R restored after each (grep -c MUTATION -> 0).
- 2026-08-09: T10 refinement — a file at the literal output path is read as the output whatever its name looks like, so a caller's `100%d.mp4` is not searched for as a pattern; a fourth control (dropping that reading) reddens only the new test for it.
- 2026-08-09: T13 — NEWS.md entry rewritten to what the package now does (what the run wrote, the untouched-output case, the literal-name rule, the frame sequence); `devtools::document()` no diff, `devtools::test()` FAIL 0 | WARN 4 | SKIP 5 | PASS 6035, `devtools::check()` Status: OK (0/0/0). The 4 warnings are the package's own "Dropping N audio tracks" warnings in test-audio-stream.R and test-ffmpeg.R, unrelated to this branch.
- 2026-08-09: T13 — a first check run reported 1 NOTE, the spelling test on "neighbouring" in NEWS.md; the package spells US ("behavior" x20), so the branch does too. A sweep that also touched two unrelated files' comments was reverted, keeping the diff confined (AC2).
- 2026-08-09: all thirteen tasks done and checks clean; the review's six actioned findings are answered (F1/F10 by the write-detection rule and its never-opened test, F2/F3 by `expand = FALSE` and the two neighbor tests, F6 by the frame-pattern set, P1 by the fixture-verified gate); status -> review.
- 2026-08-09: review round 2 — CI red on windows-latest: the `a*.mp4` fixture cannot be created there (`*` is illegal in a Windows filename; `writeLines()` fails with "cannot open the connection" at test-failed-run-cleanup.R:167). Gated with `tm_require_wildcard_name()`, which builds and verifies the fixture and skips only on Windows, failing anywhere else; the test still reddens under the default-globbing control locally.
- 2026-08-09: review round 2 — 34 findings scored, one actioned (F20, 82: the mtime half of the rule was untested) and fixed, with F1/F21/F22 fixed alongside it as one-line neighbors; F1 was the localtime-formatted mtime that would delete an untouched file across a TZ change. Not a return: 82 < 90 and no criterion failed as written. Full check Status: OK, 16 tests / 53 expectations / 0 skipped.



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


### Round 2 — acceptance-criteria evidence (fresh, 2026-08-09)

Whole file fresh: 14 tests, 50 expectations, 0 failed, **0 skipped**.

- **AC1** — cases 1 and 2 green: the output path is absent after the abort with
  the path absent beforehand and with it pre-written with content. Counterfactual
  with the `unlink()` stubbed to `invisible(NULL)`: 10 of 14 tests red, 21
  failures, first one quoted — `Expected file.exists(outfile) to be FALSE.
  actual: TRUE`. `R/ffm.R` restored (`grep -c MUTATION` -> 0, `git diff --stat`
  empty).
- **AC2** — `grep -rn "unlink(\|file.remove(" R/` -> `R/program_management.R:247`
  (pre-existing) and one new call at `R/ffm.R:1464`, inside
  `remove_failed_output()`. The five `ffm_run()` call sites (`R/ffm_batch.R:127`,
  `R/ffm.R:1589`, `R/ffmpeg.R:619,621,887`) remove nothing themselves — the grep
  above finds no `unlink()` in either file. `git diff master..HEAD --
  NAMESPACE man/` -> 0 lines. `grep -rn "getOption(" R/` -> `R/ffmpeg.R:2533` on
  this branch and on `master` alike.
- **AC3** — case 12 green, and the rendered abort read directly: `The incomplete
  '<tmp>/filee7386d32cc27.mp3' was removed.` alongside `FFmpeg exited with
  status 234.` The match is on the wording and the basename, not a snapshot.
- **AC4** — case 13 green: the caught condition is
  `tidymedia_multitrack_separation` and its `$parent` message carries the
  removal sentence.
- **AC5** — case 14 green: `success` reads `FALSE, TRUE`, the failed row's output
  is absent and the succeeding row's is present.
- **AC6** — `NEWS.md:296` carries the entry, naming no milestone number. Each
  behavior it asserts has a run that fails without it: the removal itself by
  AC1's stub above; "only what the run wrote" by the control that removes
  everything at the output (reddens the never-opened case and the frames case);
  the literal-name rule by `unlink()`'s default globbing (reddens the two
  neighbor cases) and by dropping the literal-first reading (reddens its own
  case); the frame sequence by treating every output as a literal path (reddens
  the frames case).
- **AC7** — `devtools::document()` no diff (re-run, `git status` clean);
  `devtools::test()` FAIL 0 | WARN 4 | SKIP 5 | PASS 6035, the 4 warnings being
  the package's own "Dropping N audio tracks" warnings in `test-audio-stream.R`
  and `test-ffmpeg.R`, on neither this branch's files nor its behavior;
  `devtools::check()` Status: OK — 0 errors, 0 warnings, 0 notes.
- **AC8** — cases 3 and 4 green: removal in the three cells where the run wrote,
  and preservation with the content intact only for `overwrite = FALSE` against
  a pre-existing path — asserted against a fixture where the file WAS written
  after the snapshot, so the guard is shown to hold independently of what FFmpeg
  did rather than by riding on the write-detection rule.
- **AC9** — case 7 green: after an unknown-encoder run the pre-written output
  still exists, `readLines()` returns its original line, and its size and mtime
  are `identical()` to the pre-run stat. The rendered abort read directly:
  `'<tmp>/filee73821f94c73.mp4' was left as it was: FFmpeg never wrote to it.`
  alongside `FFmpeg exited with status 8.` The control that removes everything
  at the output reddens exactly this case and the frames case, and nothing else.
- **AC10** — cases 8, 9, 10 and 11 green: `a*.mp4` goes while `aQQQ.mp4` and
  `aXYZ.mp4` stay; `out[1].mp4` goes while `out1.mp4` stays; a literal
  `100%d.mp4` goes while `1005.mp4` stays; and a failed `sample_frames()` run —
  blocked at its third frame by a directory sitting where that file must go,
  exit 235 with two frames written — loses `f_000001.png` and `f_000002.png`
  while the blocking directory, an earlier run's `f_000010.png` (size and mtime
  `identical()`) and a non-matching `notes.txt` all survive. Three controls
  discriminate: default globbing reddens the two neighbor cases, dropping the
  literal-first reading reddens the `100%d.mp4` case, and treating every output
  as a literal path reddens the frames case.
- **AC11** — the whole file runs with 0 skipped on this machine. The
  unremovable-file case calls `tm_require_unwritable_dir()`
  (`tests/testthat/helper-skip.R`), which asks `file.access(dir, mode = 2)`,
  skips only on Windows or as root, and calls `testthat::fail()` anywhere else —
  no skip anywhere in the file is keyed on the outcome of the operation under
  test.

**Consistency gate.** `cairn_validate.py` exit 0, all checks passed (2 advisory
warnings: the >7-criteria and >10-task sizing tripwires, a return-driven
expansion of work already in flight rather than a split). No `DESIGN.md`
principle changed, so `cairn_impact` is skipped. Profile `consistency-gate`
slot: `document()` no diff; `README.Rmd`/`README.md` untouched by this branch (0
lines); `pkgdown::check_pkgdown()` "No problems found."; `NEWS.md` entry present
and naming no milestone number; the only top-level file the branch touches is
`NEWS.md`, already tracked; `devtools::check()` Status: OK.

### Round 2 — independent review: three lenses, then a scorer

34 candidate findings scored (33 from the diff lens, 1 from the prior-review
lens; the blame-history lens reported no regression in any area it checked, and
the prior-review lens found the GitHub comment surface empty and every one of
round 1's six actioned findings answered).

**One actioned (>=80): F20 (82)** — the mtime half of the write-detection rule
was untested: dropping mtime from `output_snapshot()` left all 14 tests green,
so nothing pinned the size-equal/mtime-differs case D046 names as the reason
mtime is there at all. **Fixed now.** Not a return under the floor: 82 rather
than >=90, and no criterion fails as written — none promises a test of that
case.

**Three sub-threshold findings fixed alongside it**, because they sit in the
same lines and each cost one line: **F1 (78)**, the snapshot recording mtime as
a localtime-formatted string, so a `TZ` change or DST crossing between the two
snapshots made an untouched file compare unequal and be deleted — now epoch
seconds at microsecond precision, verified by switching `TZ` between the
snapshots and watching the file survive; **F21 (78)**, the directory filter in
`output_targets()` being unpinned (its mutation also left the suite green);
**F22 (62)**, AC9's test matching `"left as it was"`, wording shared with the
`overwrite = FALSE` guard, now matching `"FFmpeg never wrote to it"`.

Both previously-green mutations now redden exactly one test each, and the file
runs 16 tests / 53 expectations / 0 failed / 0 skipped.

**Thirty logged below threshold:** F1/F21 (78, fixed anyway as above); F2 (72)
and F3 (66) symlinked outputs — `unlink()` removes the link, not the target;
F22 (62, fixed anyway); F6 (62) `list.files()` cannot see a dot-prefixed frame
sequence; F13 (58) a partial removal reports only what stuck; F33 (55) NEWS
overstates coverage against the `verify =` path; F4 (55) a concurrent writer
into a shared pattern directory; F27 (52) `testthat::fail()` does not halt, so
the unwritable-dir gate falls through on a platform where the chmod did not
take; F30 (52) AC2 says "inside `ffm_run()`" where the call sits in
`remove_failed_output()`; F32 (52) the frame test's `notes.txt` assertion is a
tautology; F23 (50) helper tests match un-rendered cli templates; F29 (48) AC1
and AC8 are in tension on a combination no measured build reaches; F7 (48) the
`overwrite = FALSE` guard does not apply to pattern outputs; F12 (45) a failed
`verify =` does no removal; F25 (45) three helper tests pass an empty `before`;
F9 (42) a zero-length output errors; F15 (42) hand-rolled pluralization against
the cli convention; F26 (40) the empty-disposition path is untested end-to-end;
F10 (35) a TOCTOU window inside the snapshot; F11 (32) the literal/pattern
reading can flip across a run; F5 (30) the `overwrite = FALSE` bullet asserts
disk state it does not stat; F16 (28) sprintf-in-a-cli-template idiom; F14 (28)
`unlink()`'s return value discarded; F28 (25) `system("id -u")` in a skip
helper; F8 (22) coarse-timestamp filesystems; F24 (22) `info =` on one of three
expectations; F17 (20) helper placement; F19 (18) the snapshot runs on
successful runs too; P-cand-1 (15) the disposition bullet's position, already
declined at round 1; F31 (10) stale review evidence, moot once round 2 was
written; F18 (12) D045 lacking a superseded marker, refuted by D046's heading.

**CI.** `windows-latest (release)` was red: the `a*.mp4` fixture cannot be
created there — `*` is illegal in a Windows filename — so the test errored
rather than skipped. Gated with `tm_require_wildcard_name()`; all eight build
jobs green afterwards. `codecov/patch` and `codecov/project` report `fail`
against their coverage thresholds, as they do on this repo generally; no build
or check job is red.
