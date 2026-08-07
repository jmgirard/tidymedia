# M54: Correct the `run = FALSE` purity claim for the nvenc encoder probe

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m54-nvenc-dry-run-purity · PR #57 https://github.com/jmgirard/tidymedia/pull/57

## Goal

Make the package's stated purity contract true by recording that resolving
`hardware = "nvenc"` probes FFmpeg while building the pipeline, `run` notwithstanding.

## Scope

**In:** a superseding D-entry restating the `run = FALSE` purity claim as a condition
on probe shape rather than a hand-list of exceptions; the matching correction to
`cairn/DESIGN.md`'s Conventions bullet; live `hardware = "nvenc"` cases in the three
`run = FALSE` purity tests, replacing the two comments that today exclude nvenc; the
probe stated on every `hardware`-bearing Rd topic, guarded by a test; and `call = call`
threaded at the two `resolve_hw_encoder()` sites that omit it
(`R/ffmpeg.R:1135`, `R/ffmpeg.R:1393`), so an nvenc-unavailable abort names the verb.

**Out:**
- Making the probe lazy — weighed and rejected at the plan gate; D034 records why.
- Caching `has_nvenc()` / `ffmpeg_encoders()` so a `_batch` run probes once rather than
  once per row → ROADMAP candidate row added by this plan.
- D024's `two_pass` normalization exception → unchanged; D034 restates, never narrows it.
- Any change to which encoder a call resolves to → no runtime behavior changes here.

## Acceptance criteria

- [x] AC1 `cairn/DECISIONS.md` gains **D034**, which quotes D024's sentence "Every verb's
      `run = FALSE` call runs no binary — with **the two-pass normalization path the sole
      exception**" verbatim, names it superseded, and states the replacement as a
      *condition on probe shape* — a probe whose result enters the compiled command runs
      when the pipeline is built, which is D013's shape — rather than as a list of verbs,
      per D024's own "Scope: conditions, not a verb list".
- [x] AC2 `cairn/DESIGN.md`'s Conventions bullet no longer claims a `run = FALSE` call runs
      no binary with the two-pass path as sole exception, and states D034's condition
      instead. Verified by `grep -n "sole exception" cairn/DESIGN.md` returning nothing.
- [x] AC3 `tests/testthat/test-audio-stream-passthrough.R:198` and
      `test-audio-stream-crop-segment.R:325` replace their nvenc-excluding comments with
      live `hardware = "nvenc", run = FALSE` cases, and `test-audio-stream-format-web.R`'s
      purity block (`:129-152`) gains one. Each counts invocations of `ffmpeg_encoders()` —
      the seam that actually shells out, and the one `has_nvenc()` reaches when
      `getOption("tidymedia.nvenc_encoders")` is unset — asserting the count exceeds zero
      under `hardware = "nvenc", run = FALSE` while staying zero at the default hardware in
      the same block. Each is shown to discriminate: with `resolve_hw_encoder()`'s
      `hardware == "none"` early return forced to fire unconditionally, it goes red.
- [x] AC4 Every Rd topic whose argument names include `hardware` states that resolving
      `hardware = "nvenc"` probes the FFmpeg binary for the encoder, so such a call is not
      binary-free even under `run = FALSE`. Enumerated by a test reading `../../man/*.Rd`
      with `tools::Rd_db("tidymedia")` as fallback and splitting `\item{}` names on commas,
      both per `tests/testthat/test-audio-index-docs.R:20-40`; it asserts it found at
      least 16 such topics.
- [x] AC5 `grep -n "resolve_hw_encoder(" R/*.R` shows every call site passing `call =`.
- [x] AC6 PROFILE.md's verify slot clean — `devtools::check()` 0 errors / 0 warnings, read
      from `<pkg>.Rcheck/00check.log`'s `Status:` line — and `devtools::test()` passes.
- [x] AC7 `R/ffmpeg.R` stays wholly CRLF and is never rewritten wholesale: no line in it
      ends in a bare LF, and `git diff --stat master -- R/ffmpeg.R` reports fewer than 100
      changed lines (a line-ending rewrite of this ~5700-line file reports ~5700).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5, T6
- AC5 → T1
- AC6 → T7
- AC7 → T1, T7

## Tasks

- [x] T1 Thread `call = call` into `resolve_hw_encoder()` at `R/ffmpeg.R:1135`
      (`format_for_web_pipeline()`) and `R/ffmpeg.R:1393` (`standardize_pipeline()`).
      `R/ffmpeg.R` is the repo's only CRLF file: read and write it as bytes restoring
      `\r\n`, and check that one file's diffstat before committing (LESSONS M35/M48).
- [x] T2 Write D034 in `cairn/DECISIONS.md` per AC1.
- [x] T3 Correct `cairn/DESIGN.md`'s Conventions bullet to match D034.
- [x] T4 Extend the three purity tests per AC3; prove each new case discriminates by
      stubbing `has_nvenc()`.
- [x] T5 Add the probe sentence to the shared `@param hardware` roxygen blocks; run
      `devtools::document()`.
- [x] T6 Add the Rd guard test per AC4, reusing `rd_sources()` / `rd_param_names()` from
      `tests/testthat/test-audio-index-docs.R:20-40`.
- [x] T7 Run `devtools::document()`, `devtools::test()`, `devtools::check()`; confirm the
      CRLF count and the `00check.log` `Status:` line.
- [x] T8 (review return, F2) Make the roxygen probe sentence conditional on re-encoding and
      state the stream-copy exception on the four topics it applies to; guard both the
      wording and the behavior it describes.
- [x] T9 (review return, F1) Narrow the NEWS blame entry to what the scalar verbs do, and
      add a ROADMAP candidate row for the `_batch` fan-out blame gap.
- [x] T10 Re-run `devtools::document()`, `devtools::test()`, `devtools::check()`; confirm
      line-ending integrity and the `00check.log` `Status:` line.
- [x] T11 (review return, D1/D2/D3) Re-measure blame attribution across all sixteen
      `hardware`-bearing verbs with well-formed inputs, verifying each abort is the nvenc
      one and not a schema error; drop the false `picture_in_picture_batch()` exclusion from
      the NEWS entry and the ROADMAP candidate row; repair `test-nvenc.R`'s vacuous control.
- [x] T12 (review return, D4) Carry T8's conditional wording into `NEWS.md`'s Documentation
      entry, which still states round 1's unconditional proposition.
- [x] T13 Re-run `devtools::document()`, `devtools::test()`, `devtools::check()`; confirm
      line-ending integrity and the `00check.log` `Status:` line.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned 9 findings on this milestone's criteria: DESIGN.md left uncorrected by AC1; AC1's verb-list scoping contradicting its own no-hand-list clause; "three" comment-excluding tests where only two exist; the `tidymedia.nvenc_encoders` option seam making AC3 red for the wrong reason; AC4's literal `\item{hardware}` scan missing six `\item{hardware, fallback}` topics; AC4's Rd-source order reversed against the cited precedent; and the mandated sentence contradicted on `segment_video(reencode = FALSE)`, which aborts before probing. All fixed before AC wording was written; none needed a gate question. AC5 and AC7 passed all three questions.
- 2026-08-06: plan gate chose correcting the record over making the probe lazy, because `resolve_hw_encoder()` is a probe whose result enters the compiled command — D024's own taxonomy calls that D013's analyze-then-build shape, already licensed — and because the only true lazy seam is `ffm_finish()`/`ffm_batch()`, the sole readers of `run`, which needs the pipeline-object hook D024/RR02 Q3 rejected, and would force a dry run on a GPU-less machine to print a command that aborts; falsified by a report of a dry run's compiled command differing from what a subsequent `run = TRUE` call executes.
- 2026-08-06: plan gate kept the per-row re-probe cost out, because caching `has_nvenc()` / `ffmpeg_encoders()` needs its own lifetime decision (a user installing FFmpeg mid-session), which is a separate question from whether the probe is licensed; falsified by a measured `_batch` stall attributable to repeated `ffmpeg -encoders` calls.
- 2026-08-06: T1 done. Reproduced the blame defect before fixing: with `tidymedia.nvenc_encoders = character(0)`, `standardize_video(hardware = "nvenc", run = FALSE)` blamed `standardize_pipeline(...)` and `format_for_web(...)` blamed `format_for_web_pipeline(...)`, while `crop_video` and `anonymize_video` already named the verb. Added a test to `tests/testthat/test-nvenc.R` carrying those two already-correct verbs as discriminating controls, confirmed red on exactly the two targets, then threaded `call = call` at `R/ffmpeg.R:1135,1393`. `devtools::test()`: 0 failures, 3458 passing, 5 skips; the 4 warnings are the pre-existing M44 dropped-track diagnostic in files this diff does not touch. `R/ffmpeg.R` edited as bytes: CRLF count 5652 unchanged, diffstat 2 insertions / 2 deletions.

- 2026-08-06: T2 done. D034 appended to `cairn/DECISIONS.md`. It states the rule as a condition on probe shape and enumerates today's instances by a stated grep over the execution seams (`run_program(`, `ffmpeg(`, `ffprobe(`, `mediainfo(`) filtered to build-time reachability, rather than by recall. That grep found exactly two: D013's loudnorm analysis (`R/loudnorm_two_pass.R:140,182`) and the nvenc resolver (`R/ffmpeg.R:2283`, sole internal caller `has_nvenc()` at `:2388`); `ffmpeg_codecs()` has no internal caller. Also established that D024's bullet was false on the day it was written — nvenc shipped at M31 on 2026-07-26, D024 is dated 2026-07-30 — so the entry records a list falsified by existing code, not by later work.

- 2026-08-06: T3 done. `cairn/DESIGN.md`'s Conventions bullet restated to match D034; `grep -n "sole exception" cairn/DESIGN.md` now returns nothing, satisfying AC2. The replacement also drops the old bullet's implication that D024's diagnostic probes are a `run = TRUE`-only *addition* to the same list, since they are a different shape entirely.

- 2026-08-06: amendment (substantive, gated). AC3's counting seam changed from `find_ffmpeg()` to `ffmpeg_encoders()`. `ffmpeg()` reaches the binary through `system(glue('{find_ffmpeg()} {command}'))` at `R/ffmpeg.R:28`, not through `run_program()`, so the existing counting mock cannot intercept it: counting `find_ffmpeg()` alone would leave a real `ffmpeg -encoders` process running inside a test whose whole point is that it is binary-free, and would fail for an unrelated reason wherever FFmpeg is absent. Counting at `ffmpeg_encoders()` pins the identical claim deterministically. Rejected: stubbing `ffmpeg()` with canned `-encoders` output, which would make the test depend on a hand-written fixture of FFmpeg's encoder-table format that LESSONS M52 records as going stale across tool versions.

- 2026-08-06: T4 done. The two nvenc-excluding comments (`test-audio-stream-passthrough.R`, `test-audio-stream-crop-segment.R`) are replaced by live D034 blocks and `test-audio-stream-format-web.R` gained one; each asserts zero probes at the default hardware and a rising count under `hardware = "nvenc", run = FALSE`, across the scalar verbs and one `_batch` sibling. Green: 85 / 86 / 30 passing, 0 failures. Discrimination proven by mutation — forcing `resolve_hw_encoder()`'s `hardware == "none"` early return to fire unconditionally turned exactly the three new blocks red (3 / 3 / 2 failing expectations) and nothing else in those files; `R/ffmpeg.R` restored from the index afterward with CRLF 5652 intact.

- 2026-08-06: amendment (substantive, gated). Two criteria corrected. AC4's floor 17 -> 16: the true count of Rd topics carrying a `hardware` argument is 16, measured by comma-splitting `\item{}` names; the 17 came from misreading the plan-time audit's own parenthetical, which listed "six more" topics while noting one of them was already inside the eleven it had just counted. AC7's exact CRLF equality (5652) -> a line-ending-integrity plus no-wholesale-rewrite check: the exact form forbade adding any line to `R/ffmpeg.R`, which T5's 48 roxygen lines necessarily do, so AC4 and AC7 as written could not both hold. The rewritten AC7 guards what the original was for -- LESSONS M35/M48's whole-file normalization -- without pinning the file's length.

- 2026-08-06: T5 done. The probe sentence added to all 16 `@param hardware` blocks in `R/ffmpeg.R` (there is no `@inheritParams` tying them together -- the docs are themselves the kind of hand-list D024 went stale as). Byte-level edit: 48 insertions = 16 blocks x 3 lines, CRLF 5652 -> 5700, zero bare LF. `devtools::document()` regenerated; all 16 `hardware`-bearing Rd topics carry it, measured by comma-splitting `\item{}` names.
- 2026-08-06: T6 done. `rd_sources()`, `rd_param_names()` and `topics_documenting()` lifted out of `test-audio-index-docs.R` into a new `tests/testthat/helper-rd.R`, so M51's guard and this one share one implementation rather than duplicating the two-shape Rd source; `links_to_topic()` stayed behind as audio-specific. New `tests/testthat/test-nvenc-docs.R` asserts the sentence on every `hardware`-documenting topic plus the converse (the sentence never appears on a topic without the argument), so a package-wide paste cannot make it pass vacuously. Green, and M51's guard still passes (43 expectations). Discrimination proven: deleting the sentence from `man/crop_video.Rd` turned it red naming that topic.

- 2026-08-06: T7 done, and it caught a real defect in the T5+T6 commit. Mutation-probing the Rd guard, I restored `man/crop_video.Rd` with `git checkout` -- which restores from the INDEX, and T5's `document()` output was not staged yet, so the restore reverted the generated sentence rather than only my mutation, and 77dce2e shipped that file without it. This is precisely the trap LESSONS M44 records ("commit the baseline before mutation-probing"); the guard written at T6 is what surfaced it. Verified by stashing back to 77dce2e: FAIL 1, and FAIL 0 with the regenerated file. `devtools::document()` now produces no further diff.
- 2026-08-06: T7 evidence. `devtools::test()`: FAIL 0, PASS 3472, SKIP 5. `devtools::check()`: `Status: OK` read from `00check.log:68`, 0 errors / 0 warnings / 0 notes. The doc guard was confirmed to RUN under check rather than skip (LESSONS M51): check-run totals are identical to local (PASS 3472, SKIP 5) and all 5 skips are `test-nvenc.R`'s GPU gate, so the `Rd_db()` fallback path works. AC5: all four `resolve_hw_encoder()` call sites pass `call =` (the one grep hit lacking it is a comment). AC7: 0 bare-LF endings, 5700 CRLF, diffstat vs master 50 insertions / 2 deletions. NEWS.md gained a Bug fixes entry for the corrected blame and a Documentation entry for the stated probe.

- 2026-08-06: review in progress. AC1-AC7 verified with fresh evidence and ticked; consistency gate green (cairn_validate exit 0, `document()` no diff, `pkgdown::check_pkgdown()` clean, NEWS entries present, no new root files); CI 9/9 on PR #57. Two of three review lenses reported: prior-review found no regressions (its PR-comment probe returned empty, so archived `## Review` sections were the whole surface; M31 had logged this call-attribution gap at 74 and this diff closes it), blame-history found no defects across 7 checks. Still outstanding: the [O] diff-bug lens, a fresh re-run of AC3's mutation probe (held back so it cannot corrupt that reviewer's read of the shared tree), and the scorer pass.

- 2026-08-06: REVIEW RETURNED to in-progress. Two findings at 92. (1) `NEWS.md:275-279` states that the `_batch` siblings gained the corrected blame and that the other `hardware` verbs already had it; measured at review, every `_batch` verb still aborts naming `purrr::pmap(jobs, .f, ...)`, because threading `call =` into the pipeline builders reaches the scalar verbs only — the fan-out shape LESSONS M47/M48-F1 already records. (2) The roxygen sentence added to all 16 `@param hardware` blocks claims unconditionally that a `"nvenc"` call runs the binary under `run = FALSE`; it is false on `separate_audio_video` (at its DEFAULT `video_codec = "copy"`), on `segment_video(reencode = FALSE)`, and on both `_batch` siblings, all of which abort before resolving — 0 probes measured against 1 for the control. The plan-time criteria audit caught this case and the fix reached AC4's wording but never the sentence. Twelve further findings scored below 80 and are logged in the Review section, not actioned.

- 2026-08-06: return gate. F1: correct the NEWS claim rather than give the `_batch` verbs the blame fix, because LESSONS M47/M48-F1 records that a fan-out verb's `caller_env()` lands on `purrr::pmap()`'s anonymous closure whatever the pipeline threads, so the fix is a front-door guard in six verbs — new runtime behavior outside this milestone's stated Scope, and a guard the same lesson says no test can pin. F2: tailor the sentence per topic rather than hedge it uniformly, so a reader can tell which of their calls probe.

- 2026-08-06: T8 done (F2). Measured first: with `tidymedia.nvenc_encoders = character(0)` the four stream-copy topics do not skip the probe silently — `hardware = "nvenc"` alongside a stream copy is an error each topic already documents, and it fires before resolution (`separate_audio_video`/`_batch` at their default `video_codec = "copy"`: "`hardware` needs a re-encoding `video_codec`"; `segment_video`/`_batch` at `reencode = FALSE`: "`video_codec` and `hardware` need a re-encoding cut"). So the gate-approved wording was corrected to say the conflict aborts the call rather than that nothing is probed. All 16 blocks now condition the claim on re-encoding; the four gained one sentence pointing at the conflict they already state. `test-nvenc-docs.R` gained two guards — the exception clause on exactly those four topics, and a behavioral block counting `ffmpeg_encoders()` at 0 across all four aborts against 1 for the re-encoding control. Both proven to discriminate by mutation from a staged baseline (LESSONS M44): blanking the clause in `man/segment_video.Rd` reddened the topic-set guard, and forcing `segment_video_pipeline()`'s `!reencode` conflict branch dead reddened 3 expectations in the behavioral one. `R/ffmpeg.R` edited as bytes: CRLF 5700 -> 5708, 0 bare LF. `devtools::test()`: FAIL 0, PASS 3479, SKIP 5; the 4 warnings are the pre-existing M44 dropped-track diagnostic.

- 2026-08-06: T9 done (F1). Measured `conditionCall` on all sixteen `hardware`-bearing verbs under `tidymedia.nvenc_encoders = character(0)` rather than trusting the review's three: the fan-out blame reaches further than F1 said (the scalar `segment_video()` blames `purrr::pmap()` too, since it fans out over segments) and less far (`picture_in_picture_batch()` blames itself, validating before the fan-out). The NEWS entry now states exactly that, and `test-nvenc.R` gained a block pinning it — the two fan-out cases plus `picture_in_picture_batch()` as the control, so a blanket claim would fail there. The block is written to go red when the front-door guard lands, which is the signal to rewrite the note. ROADMAP candidate row added for that guard (search-first: no existing row or standing rejection covers batch blame attribution; M56 is codec tokens on scalar verbs). `cairn_validate` exit 0.
- 2026-08-06: T10 done; both returned findings closed, status back to `review`. `devtools::document()` no diff. `devtools::check()`: `Status: OK` at `00check.log:68`, 0 errors / 0 warnings / 0 notes. Test totals under check equal local (FAIL 0, PASS 3482, SKIP 5) and the skip count is unchanged from the pre-return run, so neither new guard is silently skipping (LESSONS M51). `pkgdown::check_pkgdown()` "No problems found". AC7 re-measured: 0 bare-LF endings, 5708 CRLF, `git diff --stat master -- R/ffmpeg.R` 58 insertions / 2 deletions.
- 2026-08-06: REVIEW ROUND 2 RETURNED to in-progress. Four findings at or above 80. D1 (94): `NEWS.md` excludes `picture_in_picture_batch()` from the fan-out blame limitation, but measured at HEAD with its real columns (`main`, `overlay`, `output`) it blames `purrr::pmap(jobs, .f, ...)` like every other `_batch` verb — T9's exclusion came from a measurement taken with the column name `inset`, whose jobs-schema abort fires before the fan-out and was misread as correct blame attribution. D2 (85): the control in `tests/testthat/test-nvenc.R` carries that same wrong column, so it passes for a schema reason and discriminates nothing. D3 (88): the ROADMAP candidate row repeats the exclusion and over-claims "measured across all sixteen verbs". D4 (88): `NEWS.md`'s Documentation entry still carries round 1's F2 proposition unconditionally — T8 fixed the roxygen and never touched NEWS. Eight further findings scored below 80 and are logged in the Review section. All seven acceptance criteria re-verified with fresh evidence and hold; the consistency gate is green. Defect return count for M54: 2.

- 2026-08-06: T11 done (D1/D2/D3). Re-measured all sixteen `hardware`-bearing verbs under `tidymedia.nvenc_encoders = character(0)`, asserting each abort's MESSAGE is the nvenc-unavailable one before reading its `conditionCall` — the step T9 skipped. That check caught four of my own first-pass cases aborting on malformed inputs instead (`anonymize_video` missing `regions`, `crop_video` on transposed positional args, and the `regions` / `videofile`-`audiofile` columns of two `_batch` tables). With well-formed inputs the result is uniform: all eight `_batch` verbs blame `purrr::pmap(jobs, .f, ...)`, `picture_in_picture_batch()` included, plus the scalar `segment_video()`; the other seven scalars blame themselves. NEWS and the ROADMAP row corrected to drop the exclusion; `test-nvenc.R`'s vacuous control replaced by `picture_in_picture_batch()` with its real `overlay` column plus `standardize_video()` as a self-blaming control, and `blamed()` now asserts the failure identity in every case. Both repairs proven by mutation: restoring the `inset` column reddens the identity assertion naming the schema error, and reverting `call = call` at `R/ffmpeg.R:1407` reddens the new control. `R/ffmpeg.R` restored — 5708 CRLF, 0 bare LF. nvenc suite: FAIL 0, PASS 82, SKIP 3.
- 2026-08-06: T12 done (D4). `NEWS.md`'s Documentation entry now carries T8's condition — the probe claim is limited to a call that re-encodes the video, and the four stream-copy topics are named with the conflict that aborts them first. T8's behavioral guard in `test-nvenc-docs.R` (0 probes across the four aborts against 1 for the re-encoding control) is the test behind the entry's claim.
- 2026-08-06: T13 done; all four round-2 findings closed, status back to `review`. `devtools::document()` no diff. `devtools::check()`: `Status: OK` at `00check.log:68`, 0 errors / 0 warnings / 0 notes. Test totals under check equal local (FAIL 0, PASS 3491, SKIP 5) and the skip count is unchanged, so the repaired blame test is not silently skipping (LESSONS M51). `pkgdown::check_pkgdown()` "No problems found". `cairn_validate` exit 0, all 16 checks PASS; its one advisory is the >10-task split tripwire, which two review returns produced and which splitting at this point would not serve. AC7 re-measured: 0 bare-LF endings, 5708 CRLF, `git diff --stat master -- R/ffmpeg.R` 58 insertions / 2 deletions.
- 2026-08-06: review round 3, F1 (86) fixed on the branch under the triage, not a floor return. The diff-bug lens found round 2's D2 defect class surviving one test block above the repaired one: `test-nvenc.R`'s first `blamed()` carried no message assertion and its `anonymize_video` control passed `regions = list(c(1, 1, 2, 2))`, which aborts at `check_regions()` — and because that check also threads `call = call`, the abort blamed `anonymize_video(...)` and satisfied the expectation for a reason unrelated to encoder resolution, under a comment claiming the controls could not pass wrongly. Reproduced, then fixed by giving the block the same failure-identity assertion and a well-formed `regions` data frame. Proven by mutation: restoring the malformed `regions` reddens the identity assertion naming the schema error, and stripping `call = call` at `R/ffmpeg.R:1617` reddens the `anonymize_video` control — which it did not do before the fix. `R/ffmpeg.R` restored, 5708 CRLF, 0 bare LF.

## Decisions

## Review

### Acceptance-criteria evidence (fresh, 2026-08-06)

- **AC1** `cairn/DECISIONS.md:1196` carries D034; its heading names "supersedes D024's
  `run = FALSE` bullet". D024's sentence AND its `two_pass` continuation are quoted
  verbatim — verified by whitespace-normalized comparison rather than grep, since the
  sentence wraps mid-phrase in both entries and a single-line grep returns 0 for text
  that is present. D034 states the rule under the heading "The rule, as a condition on
  probe shape" and names the failure mode ("it enumerated the shape's instances where it
  should have stated the shape"), not a verb list.
- **AC2** `grep -c "sole exception" cairn/DESIGN.md` → 0.
- **AC3** The two nvenc-excluding comments are gone, replaced by live blocks; the third
  file gained one. Fresh: passthrough 85 / crop-segment 86 / format-web 30 expectations,
  0 failures. Discrimination re-verified fresh at review (below).
- **AC4** `test-nvenc-docs.R` 3 expectations, 0 failures; 16 `hardware`-bearing Rd topics
  found, all carrying the sentence, plus the converse check. Confirmed to RUN under
  `R CMD check` rather than skip (LESSONS M51): check-run totals equal local totals
  (PASS 3472, SKIP 5) and all 5 skips are the GPU gate in `test-nvenc.R` /
  `test-video-codec.R`.
- **AC5** Four `resolve_hw_encoder()` call sites (`R/ffmpeg.R:1141,1405,1615,2494`), every
  one passing `call =`; the only other grep hit is a comment line.
- **AC6** `devtools::check()` → `Status: OK` at `00check.log:68`, 0 errors / 0 warnings /
  0 notes. `devtools::test()` → FAIL 0, PASS 3472, SKIP 5.
- **AC3 discrimination, re-run fresh at review:** forcing `resolve_hw_encoder()`'s
  `hardware == "none"` early return to fire unconditionally turned exactly the three new
  blocks red (3 / 3 / 2 failing expectations); `R/ffmpeg.R` restored, CRLF 5700, 0 bare LF.
- **AC7** 0 bare-LF line endings, 5700 CRLF; `git diff --stat master -- R/ffmpeg.R` →
  50 insertions / 2 deletions, far under the 100-line bound.

### Consistency gate

- `cairn_validate.py` exit 0 — all 10 PASS checks and 8 OK advisories green, including
  `coverage complete` and `binding criteria`.
- No `DESIGN.md` principle (IPn/GPn) changed, so `cairn_impact` is skipped; the header's
  Principles-touched slot is `—`.
- Toolchain slot (`r-package`): `devtools::document()` no diff · `pkgdown::check_pkgdown()`
  "No problems found" · README pair untouched and in sync · NEWS.md carries a Bug fixes and
  a Documentation entry, with no milestone numbers in user-facing text · no new root files
  needing `.Rbuildignore`.
- CI on PR #57: 9/9 green (macOS release, Ubuntu devel/release/oldrel-1, Windows release,
  pkgdown, test-coverage, codecov patch+project).

### Independent review — three lenses + scorer

- **[O] diff-bug (Opus):** 13 findings. Confirmed all seven ACs literally satisfied and
  independently reproduced the evidence, but found the milestone's Goal not fully met.
- **[S] blame-history (Sonnet):** 7 checks, no defects. The deleted nvenc-excluding
  comments are honored rather than undone (M47/M48 wrote them as a documented gap, and
  the ROADMAP records M54 as the milestone closing that candidate); `helper-rd.R` is a
  verbatim move preserving M51's `../../man`-only constraint; D034 narrows nothing
  M45/M47/M48/M49 rely on.
- **[S] prior-review (Sonnet):** no findings. Its PR-comment probe returned empty, so
  archived `## Review` sections were the whole surface. There it found M31 had logged this
  exact call-attribution gap (scored 74, not actioned then) — this diff closes it.
- **[S] scorer (Sonnet, fresh):** 2 findings at or above 80; 12 below, logged below.

### Actioned findings (>= 80)

- **F1 (92) — `NEWS.md:275-279` claims a `_batch` blame fix that did not happen.** Measured
  at review with `tidymedia.nvenc_encoders = character(0)`: `standardize_video_batch`,
  `format_for_web_batch` and `crop_video_batch` all abort with `conditionCall`
  `purrr::pmap(jobs, .f, ...)`. Threading `call =` at `R/ffmpeg.R:1141,1405` fixes the
  scalar verbs only; no `_batch` verb passes a front-door `call` into its pipeline, so
  `caller_env()` lands on the anonymous `.f`. LESSONS M47/M48-F1 records exactly this
  fan-out shape and it was not applied.
- **F2 (92) — the new roxygen sentence is false on 4 of the 16 topics.** Its second clause
  ("a `"nvenc"` call runs the binary while the command is built, even under
  `run = FALSE`") is unconditional. Measured at review: `separate_audio_video(hardware =
  "nvenc", run = FALSE)` on its DEFAULT `video_codec = "copy"` performs 0 probes and
  aborts; `segment_video(reencode = FALSE, ...)` likewise; both `_batch` siblings likewise;
  control `segment_video` default performs 1. Sites `R/ffmpeg.R:807, 2565, 2811, 4760`.
  The plan-time criteria audit flagged this exact case and the work log records it as
  "fixed before AC wording was written" — AC4's wording was made conditional, the roxygen
  sentence was not. The finding was raised and then lost between the two.

### Logged, not actioned (12 below the 80 threshold)

- F3 (78) D034 cites `R/ffmpeg.R:2283` / `:2388`; real lines are `:2301` / `:2403` after
  T5 inserted 48 lines, and `:2301` is inside `ffmpeg_encoders()`, not the resolver.
- F4 (72) `test-nvenc.R`'s blame test covers scalars only — the direct cause of F1.
- F9 (68) `ROADMAP.md:29` cites `has_nvenc()` at `:2385`; now `:2403`. Same class as F3.
- F5 (55) The purity tests count `ffmpeg_encoders()` calls, pinning the seam rather than a
  real process; a future cache would keep them green while the doc claim goes false.
- F13 (48) The sentence's "runs the binary" is absolute while `has_nvenc()` short-circuits
  on the option seam. Overlaps F2.
- F8 (45) D034's grep also hits dead code (`get_volume()`) and omits `system(`/`system2(`.
- F10 (38) D034 and the T4 amendment appear to disagree about what the 2026-07-30
  measurement observed.
- F6 (30) Vignettes still say `run = FALSE` runs nothing; out of stated Scope.
- F11 (30) `DESIGN.md`'s bullet reads self-contradictory without DECISIONS.md beside it.
- F7 (22) `@examples` comments say "without calling FFmpeg" on topics now carrying the
  new sentence.
- F12 (15) `arg_match(hardware)` lacks `error_call`; pre-existing and unreachable.
- F14 (12) `helper-rd.R` names are now visible to every test file.

### Disposition

**Returned to `in-progress`** under the return floor: F1 and F2 both score 92 on defects
in user-facing deliverables (release notes and reference documentation). Neither is an
amendment return — AC4's mandated claim is conditional ("resolving `hardware = "nvenc"`
probes...") and correct as written; the roxygen sentence overreached it. Defect return
count for M54: 1.

## Review — round 2 (2026-08-06)

### Acceptance-criteria evidence (fresh, round 2)

- **AC1** `cairn/DECISIONS.md:1196` carries D034; heading names "supersedes D024's
  `run = FALSE` bullet". D024's sentence and its `two_pass` continuation are quoted
  verbatim (byte-compared by the diff-bug lens). The rule is stated under "The rule, as a
  condition on probe shape", naming the failure mode rather than a verb list.
- **AC2** `grep -c "sole exception" cairn/DESIGN.md` → 0.
- **AC3** The two nvenc-excluding comments are replaced by live blocks and the third file
  has one; fresh counts 85 / 86 / 30 expectations, 0 failures. Discrimination re-run fresh:
  forcing `resolve_hw_encoder()`'s `hardware == "none"` early return to fire
  unconditionally turns the `audio-stream-*` suite red; baseline is 0 failures.
  `R/ffmpeg.R` restored afterward — 5708 CRLF, 0 bare LF.
- **AC4** 16 Rd topics document `hardware`; all 16 carry the probe sentence, measured by
  `grep -l` over `man/*.Rd`. Exactly four carry the stream-copy exception clause
  (`separate_audio_video`, `separate_audio_video_batch`, `segment_video`,
  `segment_video_batch`). `test-nvenc-docs.R` 10 expectations, 0 failures.
- **AC5** Four `resolve_hw_encoder()` call sites (`R/ffmpeg.R:1143,1407,1617,2496`), every
  one passing `call =`; the fifth grep hit is a comment.
- **AC6** `devtools::check()` → `Status: OK` at `00check.log:68`, 0 errors / 0 warnings /
  0 notes. `devtools::test()` → FAIL 0, PASS 3482, SKIP 5. Test totals under `R CMD check`
  equal local totals, so no new guard is silently skipping.
- **AC7** 0 bare-LF endings, 5708 CRLF; `git diff --stat master -- R/ffmpeg.R` → 58
  insertions / 2 deletions, under the 100-line bound.

### Consistency gate — round 2

- `cairn_validate.py` exit 0; all 16 PASS checks and 8 OK advisories green, including
  `coverage complete` and `binding criteria`.
- No `DESIGN.md` principle changed, so `cairn_impact` is skipped.
- Toolchain slot (`r-package`): `devtools::document()` no diff · `pkgdown::check_pkgdown()`
  "No problems found" · README pair untouched · NEWS.md carries entries with no milestone
  numbers · no new root files.

### Independent review — three lenses + scorer (round 2)

- **[O] diff-bug (Opus):** 12 findings, plus a verified-clean list (line-ending integrity,
  `document()` no-diff against a `git archive` copy, D034's verbatim quote, the Rd topic
  counts, and a check that `tool_versions()` is not a third build-time probe seam).
- **[S] blame-history (Sonnet):** 11 checks, 1 finding (D034's drifted line citations).
  No past intent undone: the deleted nvenc-excluding comments are honored, `helper-rd.R`
  is a verbatim lift, D034 narrows nothing D013/D024 rely on.
- **[S] prior-review (Sonnet):** no findings. Its PR-comment probe returned empty, so
  archived and live `## Review` sections were the whole surface; round 1's F1 and F2 are
  genuinely addressed rather than merely claimed.
- **[S] scorer (Sonnet, fresh):** 4 findings at or above 80; 8 below.

### Actioned findings (>= 80)

- **D1 (94) — `picture_in_picture_batch()` does NOT validate before fanning out; the NEWS
  claim is false.** `NEWS.md:278-281` says the fan-out blame applies to "the `_batch`
  verbs, bar `picture_in_picture_batch()`". Measured at HEAD with
  `tidymedia.nvenc_encoders = character(0)` and a well-formed jobs table (`main`,
  `overlay`, `output`): conditionCall is `purrr::pmap(jobs, .f, ...)`, the same as every
  other `_batch` verb. `compare_videos_batch` with its correct columns likewise.
- **D3 (88) — the ROADMAP candidate row repeats the same false exclusion** and over-claims
  its provenance: `cairn/ROADMAP.md` says "bar `picture_in_picture_batch()`, which
  validates before fanning out ... measured 2026-08-06 across all sixteen
  `hardware`-bearing verbs", while that measurement used a malformed jobs table.
- **D4 (88) — F2's fix never reached NEWS.md.** `NEWS.md:586-590` still states
  unconditionally that asking for `"nvenc"` means "such a call runs the binary even with
  `run = FALSE`" — the proposition round 1 scored 92 on — and so misdescribes the
  documentation change it announces, since four topics now say the opposite for their
  default arguments.
- **D2 (85) — the test written to pin D1 is vacuous.** `tests/testthat/test-nvenc.R:147`
  passes `inset` where `picture_in_picture_batch()` requires `overlay`, so the call aborts
  on a jobs-schema error before any fan-out and independently of `hardware`; the
  expectation passes for a reason unrelated to blame attribution.

### Logged, not actioned (8 below the 80 threshold)

- D6 (72) "as most other verbs taking `hardware` already did" is a minority, not a majority.
- D9 (68) the exception guard hand-lists its four topics where the probe guard derives its
  list from the Rd — the staleness shape D034 exists to retire.
- D8 (62) round 1's AC evidence block cites pre-T8 line numbers and totals; round 2's block
  above supersedes it.
- D5 (58) "that re-encodes the video" still overreaches where `codec_family()` cannot map
  the codec (e.g. `video_codec = "libvpx-vp9"`), which aborts with 0 probes.
- D11 (50) D034's "no runtime behavior changes here" sits in tension with T1's changed
  `conditionCall`, documented as a bug fix.
- D7 (50) D034's cited line numbers drifted further after T8 — round 1's F3, still open.
- D10 (38) five topics carry the probe claim with no probe-count assertion behind it.
- D12 (32) `standardize_video(video_codec = "copy")` aborts without probing and its topic
  does not say so.

### Disposition — round 2

**Returned to `in-progress`** under the return floor: D1 scores 94 on a defect in a
user-facing deliverable (the release notes). D2, D3 and D4 are actioned alongside it —
D1/D2/D3 share one root cause, a `picture_in_picture_batch()` measurement taken with a
wrong column name whose schema error was read as correct blame attribution, and D4 is
round 1's F2 surviving in a file T8 never touched. No finding is an amendment return: no
acceptance criterion is falsified, and all seven verified above. Defect return count for
M54: 2.
