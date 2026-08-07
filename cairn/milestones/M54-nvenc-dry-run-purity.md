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
