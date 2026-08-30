<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M091: The multi-track advice stops arriving when the caller is already following it

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m091-separation-diagnostic-container-gate` / https://github.com/jmgirard/tidymedia/pull/95

## Goal

`separate_audio_video()` and its batch sibling stop telling a caller to write a
multi-stream container when their output already is one.

## Scope

Surface tier: **user-facing** — the deliverable is a rendered error message and
a public condition class vector.

**In:** a package-internal list of output extensions that hold several audio
streams, and a gate on it at both diagnostic sites. `run_separation_audio()`
(`R/ffmpeg.R:643`) fails open to `ffm_run()`'s own condition when `audiofile`'s
extension is on the list; `warn_failed_separation_batch()` (`R/ffmpeg.R:807`)
drops such rows before `warn_failed_separation()` builds a bullet for them.
Both help pages state when the diagnostic fires, when it silently does not, and
that it reports what the call did rather than why FFmpeg refused.

**Out:**
- Classifying FFmpeg's failure from its stderr — `ffm_run()` spawns with
  `stderr = ""` (`R/program_management.R:105`), so nothing is captured, and
  capturing it would stop the live console output. → candidate row (M45 F1's
  successor).
- A differential re-run of the pipeline with `-map 0:a:0` into a temp path,
  which would be decisive across every cause rather than capacity alone. Needs
  its own D-entry: no probe in this package has executed FFmpeg and written a
  file. → candidate row.
- The remaining false-blame causes the gate cannot reach — `audio_codec =
  "copy"` into a container that refuses the source codec (the DEFAULT path;
  measured 2026-08-30, a 3-track AAC `.mkv` → `.mp3` exits 234 at `-map 0:a:0`
  too), an unknown encoder, a missing output directory. Disclosed in the docs
  and the D-entry, not fixed here. → same candidate row.

## Acceptance criteria

- [x] AC1: A `separate_audio_video()` audio command that FFmpeg ends at a non-zero
      exit status, with a >1-audio-track input, no `audio_stream`, and an
      `audiofile` extension held by the new list, re-raises the condition
      `ffm_run()` raised for that pipeline with no enrichment from the multi-track
      diagnostic — same class vector (no `tidymedia_multitrack_separation`), same
      `tm_status`, and a message equal to `ffm_run()`'s, save for the one
      video-written bullet `abort_after_video()` appends when the video half wrote
      a file and the audio condition is an rlang condition — for each extension the
      list holds.
- [x] AC2: In `separate_audio_video_batch()`, a failed audio row whose output
      extension is held by the list contributes no bullet to the post-fan-out
      warning, and a batch whose failed audio rows all have such outputs signals
      no warning at all.
- [x] AC3: On `mp3`, `wav`, `aac`, `flac`, `wv`, `caf`, `aiff`, `au` and `w64` —
      the extensions T1 measured at a non-zero exit status that this milestone
      leaves off the list — the scalar abort keeps the class vector
      `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")`, its
      `tm_status` field and its five bullets as currently worded, and the batch
      warning keeps its class and its per-row bullet form.
- [x] AC4: The list holds at least `mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm`,
      `ogg`, `opus` and `ts`, and every extension it holds names a container
      FFmpeg writes three mapped audio streams into at exit 0.
- [x] AC5: `?separate_audio_video` states that the diagnostic fires only when no
      `audio_stream` was named, FFmpeg returned a non-zero exit status, the input
      carries more than one audio track, and the output extension is not on the
      list. `?separate_audio_video_batch` states that a row reaches the warning
      only when it named no `audio_stream`, the row is recorded `success = FALSE`
      for any cause — a non-zero exit, a hard error and a reached limit among
      them — its input carries more than one audio track, and its output
      extension is not on the list; and no sentence on that page names an exit
      status among the conditions under which the warning fires, the section's
      opening sentence included. Both pages state that the diagnostic may
      silently not fire when the track count is unanswerable (D024's
      documentation requirement), and that it reports what the call did, never
      why FFmpeg refused.
- [x] AC6: `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T2, T3, T5, T8
- AC2 → T2, T4, T5, T8
- AC3 → T3, T4, T5, T8
- AC4 → T1, T2, T5, T8
- AC5 → T6, T8
- AC6 → T7, T8

## Tasks

- [x] T1: Rebuild the 2026-08-30 measurement inside the repo — a 3-audio-track
      fixture written to each candidate extension with `-map 0:a` — and record
      each exit status in the work log. Confirms the seven accepting extensions
      before any of them is written into source.
- [x] T2: Add the list and its predicate beside the other Layer-2 separation
      helpers in `R/ffmpeg.R` (IP1: this is Layer-2 knowledge, never the
      engine's), with the measurement cited in the comment above it.
- [x] T3: Gate `run_separation_audio()`'s enrichment on the predicate, failing
      open to `stop(cnd)` on the same branch the `is.na(status)` case uses
      (`R/ffmpeg.R:659`).
- [x] T4: Drop list-held rows in `warn_failed_separation_batch()` before it calls
      `warn_failed_separation()`, so the count in the headline matches the
      bullets shown.
- [x] T5: Tests in `tests/testthat/test-separate-av-multitrack.R` — the
      suppression case per extension, iterating the list itself rather than a
      hand-written copy; the batch row-drop and the no-warning-at-all case; the
      AC3 unchanged-behavior cases. AC4's capacity check runs `-c:a copy` on
      every listed extension except `webm`, `ogg` and `opus`, which hold no AAC
      and take `-c:a libopus` — the encoders measured for each, named here so
      the criterion is decidable without them. The suppression case runs twice per extension:
      once with the video half failing too, once with it left at its default so
      it succeeds and writes, which is the sub-case AC1's video-written
      exception covers.
- [x] T6: Roxygen on both verbs plus `R/audio-stream-doc.R` if its shared
      sentence needs it; `devtools::document()`; `NEWS.md` entry.
- [x] T7: D-entry recording the gate, its measured basis, the rejected
      alternatives and the causes left indistinguishable; `devtools::check()`.
- [x] T8: Re-measure the twelve refusing extensions under a codec each container
      takes, separating a codec refusal from a capacity one; add `ogg` and
      `opus` to the list on that measurement; correct the source comment, both
      help pages and `NEWS.md`, which enumerate the list by hand; record the
      measurement rule as a D-entry. Review's F2, F3 and F7 land in the same
      round.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (user-facing tier). Returned four findings. AC3 bound a test-suite property and quantified over every non-listed extension; AC4 bound the harness's construction; AC5 was unsatisfiable, naming a disjunction it never enumerated — all three fixed before the gate. The fourth (AC1/AC2/AC4 all satisfied by a one-element list) went to the gate as a question and became AC4's membership floor.
- 2026-08-30: plan gate chose a static measured container list over a differential one-track re-run into a temp path, because the re-run is decisive across every cause but costs an unbounded extra FFmpeg spawn on an already failed call and would be the first probe in this package to execute FFmpeg and write a file (D024 licenses the effect, not the shape); falsified by a report of the diagnostic blaming track count for a cause the container list cannot see, which the Out section already records as measured and expected.
- 2026-08-30: plan gate chose disclosing the remaining false-blame causes in the docs and the D-entry over also excluding a missing output directory with `dir.exists(dirname(outfile))`, because that check reaches one more cause while leaving the larger copy-into-incompatible-container case untouched, and the milestone's goal sentence stays single; falsified by a report of the missing- directory case specifically.
- 2026-08-30: T1 rebuilt the measurement in-repo (ffmpeg 9.0.1, 3-audio-track AAC `.mkv` from `make_multitrack_video()`'s recipe, `-map 0:a -c:a copy` per extension). Exit 0 with three audio streams in the output: `mka`, `m4a`, `mp4`, `mov`, `mkv`, `ts`. Exit 234: `webm`, `mp3`, `wav`, `aac`, `flac`, `ogg`, `opus`, `wv`, `caf`, `aiff`, `au`, `w64`. `webm`'s 234 is a CODEC refusal, not a capacity one ("Only VP8 or VP9 or AV1 video and Vorbis or Opus audio ... are supported for WebM"): `-c:a libopus` into `.webm` exits 0 carrying three opus streams. Baseline suite before any change: 0 failures, 8493 passing, 12 warnings, 5 skips.
- 2026-08-30: implement gate chose a case-insensitive extension match over an exact-lowercase one, because FFmpeg selects the output muxer from the extension without regard to case, so `OUT.MKA` is the same container as `out.mka` and an exact match would leave the false blame alive in an uppercase spelling; falsified by a report of a caller wanting the two spellings to behave differently.
- 2026-08-30: implement gate chose stating the quiet-diagnostic wording on the two separation help pages alone over also rewriting `audio_stream_extras$separation_container`, because that shared sentence is pasted into fourteen verbs' `@param audio_stream` text and only these two raise the diagnostic; falsified by a report of a caller reading a third verb's page and expecting the gate there.
- 2026-08-30: AMENDMENT (substantive, AC4). T1's measurement falsified AC4 as written: it required `webm` in the list AND required every list member to reach exit 0 from the suite's AAC fixture, which cannot both hold, since WebM holds no AAC and refuses the stream copy at exit 234 while taking three opus streams at exit 0. A first repair naming the codecs inside the criterion went to a fresh-context [O] FULL criteria audit, which returned three findings and a WIDENS verdict — it bound the harness's encoder construction, quantified over FFmpeg's encoder set with nothing enumerating it, and certified a codec-dependent property in place of the container-capacity one the gate needs. The user's mini gate answer was "decide for me", so the recommended narrowing was taken: AC4 now states the container property and the encoder choice moved to T5. The narrowed wording took its one re-entry with a second fresh [O] reader, which returned HOLDS (domain and certified property unchanged, promise bounded to the list's own membership, deliverable-bound) with two minor findings: F1, the criterion is not decidable for `webm` without naming an encoder somewhere — fixed in T5's text; F2, the trailing "never one it refuses for capacity" is entailed by the exit-0 clause and adds no verifiable binding — left standing rather than churned a third time, since the once-only re-entry was spent.
- 2026-08-30: implement gate chose a numbered `DECISIONS.md` entry over a milestone-local one, because two candidate ROADMAP rows cite this reasoning for future work and it narrows D024's diagnostic-probe licence, which needs a stable id readable from D024's own file; falsified by nothing outside this milestone ever citing it.
- 2026-08-30: T2 added `multi_audio_extensions` (the seven measured extensions) and the case-insensitive `holds_multiple_audio()` beside the separation helpers in `R/ffmpeg.R`, with T1's measurement and its per-container encoders cited in the comment above the vector. The list is an exclusion list, so an unmeasured container keeps the diagnostic it has today.
- 2026-08-30: T3 gated `run_separation_audio()`'s enrichment, and T4 dropped list-held rows in `warn_failed_separation_batch()` before the FFprobe sweep and before `warn_failed_separation()` builds a bullet, so the headline count matches the bullets shown. Both gates sit ahead of their probe: on a listed output the diagnostic cannot fire whatever the count is, so probing would spawn FFprobe for an answer nothing reads. Suite after T2-T4: 0 failures, 8493 passing, unchanged from baseline -- no existing test drove a failing audio command into a listed container, which is what T5 adds.
- 2026-08-30: T5 added seven tests iterating `multi_audio_extensions` itself. Check discrimination run both ways: with the predicate forced FALSE (the pre-gate behaviour) five of the seven go red, 29 assertions across them; with it forced TRUE 13 tests go red, including the new unlisted-container control and the existing unchanged-behaviour tests for the enriched abort, the batch warning and the both-fail path. Neither plant leaves the new tests green. `.ts` needed its own counting instrument: MPEG-TS lists its streams once per program, so `count_audio_streams()` reads 6 on a three-track `.ts` -- a property of ffprobe's listing, not the container -- and the test counts distinct stream indices instead.
- 2026-08-30: T6 corrected the scalar help page's claim that the report attaches to "any failing audio command on a multi-track input", which this branch falsifies, and gave both pages the four conditions, the silent-omission clause and the reports-what-the-call-did clause. `audio_stream_extras$separation_container` left untouched per the gate. `NEWS.md` entry added; `devtools::document()` rewrote the two `.Rd` files and nothing else.
- 2026-08-30: T7 recorded D069 in `cairn/DECISIONS.md`. Its heading cross-reference was corrected before commit: the multi-track report's own reasoning is milestone-local (M45-D1/M45-D2), not the D026 first drafted, which is about the pass-through verbs.
- 2026-08-30: all seven tasks done. `Rscript -e 'devtools::test()'` 0 failures / 8563 passing / 12 warnings / 5 skips (baseline before this branch: 0 / 8493 / 12 / 5). `Rscript -e 'devtools::check()'` Status: OK -- 0 errors, 0 warnings, 0 notes. `devtools::document()` rewrote only the two separation `.Rd` files. Status -> review. Open concern for the hygiene pass: the `Last hygiene check` stamp's ROADMAP figure (21,612) does not match the file, which measured 22,833 bytes on master before this branch touched it; the branch adds 10 bytes (a `D069` cross-reference on the M45-F1 candidate row) for 22,843 of 24,000 over 44 of 60 lines.
- 2026-08-30: amendment return: AC1 — "raises the condition `ffm_run()` raises for that pipeline — same message, same class vector (no `tidymedia_multitrack_separation`), same `tm_status`". Measured at review: with the video half succeeding, the condition carries one extra bullet that `abort_after_video()` appends (M090's contract, already on the two pre-existing fail-open branches), so "same message" is false across the ordinary half of AC1's domain while the code is correct. Review's other five criteria verified; consistency gate clean; PR #95 open as a draft.
- 2026-08-30: amendment return: AC1 — "A `separate_audio_video()` audio command that FFmpeg ends at a non-zero exit status, with a >1-audio-track input, no `audio_stream`, and an `audiofile` extension held by the new list, re-raises the condition `ffm_run()` raised for that pipeline with no enrichment from the multi-track diagnostic — same class vector (no `tidymedia_multitrack_separation`), same `tm_status`, and a message equal to `ffm_run()`'s, save for the one video-written bullet `abort_after_video()` appends when the video half wrote a file and the audio condition is an rlang condition — for each extension the list holds."
- 2026-08-30: the amended AC1 went to two fresh-context [O] FULL criteria audits before it was written. The first returned four findings: "nothing added by the diagnostic" was false of the `tm_video_error` field `abort_after_video()` also assigns; the bullet was demanded unconditionally though it is appended only to rlang conditions; no committed test drove the video-succeeds sub-case; and the falsified "unchanged" claim stands in prose elsewhere. The first two were fixed and the revised bytes went to a second reader, which returned NARROWS with five findings — three mechanical repairs taken verbatim ("object" dropped, since what reaches the caller is a modified copy; the rlang predicate stated as the code's `inherits(cnd, "rlang_error")`; the trigger narrowed from any failure to the non-zero exit the branch actually gates) and the two already routed to the gate.
- 2026-08-30: mini gate chose correcting the falsified "unchanged" prose without binding it in a criterion over also extending AC5 to cover the corrected sentence, because this milestone already carries one return and widening the criteria set after a return is how one return becomes three, while the correction ships either way; falsified by the same sentence drifting false again with no criterion holding it.
- 2026-08-30: the amendment landed. AC1 amended as above; `R/ffmpeg.R:689`, the two scalar help-page sentences and the `NEWS.md` entry corrected, all of which said the caller gets the run's own error "unchanged"; D070 recorded, superseding D069's rule paragraph alone. T5 gained a per-extension video-succeeds test asserting the message is `ffm_run()`'s plus exactly one trailing line. Check discrimination both ways: suppressing the video bullet turns that test red at 21 assertions, forcing the container gate FALSE turns it red at 42, and neither plant leaves it green.
- 2026-08-30: amendment complete. `Rscript -e 'devtools::test()'` 0 failures / 8633 passing / 12 warnings / 5 skips (before the amendment: 0 / 8563 / 12 / 5; the +70 is the seven-extension video-succeeds test). `Rscript -e 'devtools::check()'` Status: OK -- 0 errors, 0 warnings, 0 notes. `devtools::document()` rewrote `man/separate_audio_video.Rd` alone. `cairn_validate` exit 0. Status -> review.
- 2026-08-30: amendment return: AC5 — "each state that the diagnostic fires only when no `audio_stream` was named, FFmpeg returned a non-zero exit, the input carries more than one audio track, and the output extension is not on the list". Measured at review: `warn_failed_separation_batch()` selects on `!out$success` alone and `ffm_batch()`'s `run_one()` sets `success = FALSE` for a hard error or a reached limit too, so the exit-status condition AC5 requires the BATCH page to state is not one the batch verb enforces — a timed-out audio row on a multi-track input into an unlisted container does reach the warning. The scalar half of AC5 is sound; the batch half is the clause to narrow. Maintainer's decision at the gate, over accepting the mismatch or narrowing the batch code (not cheaply implementable: no per-row exit status survives `ffm_batch()`). Review's other findings F2, F3, F4 and F7 were directed fix-now into the same round. All six criteria verified as written; consistency gate clean; PR #95 open as a draft.
- 2026-08-30: amendment return: AC5 — "`?separate_audio_video` states that the diagnostic fires only when no `audio_stream` was named, FFmpeg returned a non-zero exit status, the input carries more than one audio track, and the output extension is not on the list. `?separate_audio_video_batch` states that a row reaches the warning only when it named no `audio_stream`, the row is recorded `success = FALSE` for any cause — a non-zero exit, a hard error and a reached limit among them — its input carries more than one audio track, and its output extension is not on the list; and no sentence on that page names an exit status among the conditions under which the warning fires, the section's opening sentence included. Both pages state that the diagnostic may silently not fire when the track count is unanswerable (D024's documentation requirement), and that it reports what the call did, never why FFmpeg refused."
- 2026-08-30: the amended AC5 went to two fresh-context [O] FULL criteria audits before it was written, both returning NARROWS. The first found the batch half self-contradictory — "the same four conditions ... rather than as a non-zero exit" withdraws mid-sentence one of the four it just named — and its repair, enumerating the batch conjunction outright, was taken. The revised bytes went to a second reader, which returned three findings, all taken verbatim: the negative clause was decided against paraphrase, so it now binds the page's condition list and names the section opener; the three causes read as an exhaustive enumeration of a domain `run_one()` leaves open, so they are stated as examples; and "alike" carried a sufficiency reading the necessary-condition frame does not support, so it is gone. Questions 2, 4, 5 and 6 returned nothing in both readings.
- 2026-08-30: mini gate chose narrowing AC5 to the condition the batch verb enforces over accepting the mismatch or narrowing the batch code, because the batch runner records whether a row succeeded and not how, so no per-row exit status survives `ffm_batch()` and making the page's claim true is an `ffm_batch()` contract change outside this milestone; falsified by a caller reading the two pages side by side and taking the difference for an oversight rather than the runner's own limit, which the amended page now states outright.
- 2026-08-30: the amendment landed, with review's F2, F3, F4 and F7 fix-now work in the same round. The batch help page dropped its exit-status condition and gained the runner's own; both pages now name the seven as an exclusion list rather than a survey (F2), citing `.avi` and `.nut`, re-measured here at exit 0 with three distinct audio streams on ffmpeg 9.0.1; the scalar page's video-bullet clause gained AC1's rlang qualifier (F4); `NEWS.md` gained `.webm`'s encoder caveat, the same rlang qualifier and the exclusion-list clause (F3); D069's falsifier was reworded, since "an unlisted one that accepts several" was satisfied on the day it was written — `.avi`, `.nut`, `.m4b`, `.3gp`, `.wma` and `.asf` all take three mapped AAC streams at exit 0, measured here — and now names the one direction the list can fail in that leaves a caller worse off. T5 gained an eleven-extension AC3 test (F7), where two of the eleven had suite coverage before. Check discrimination both ways: rewording one abort bullet turns 11 tests red, dropping `tidymedia_ffmpeg_exit` from the class vector turns 15 red, and neither plant leaves the new test green.

- 2026-08-30: amendment return: AC3 — "On the extensions measured 2026-08-30 as refusing three mapped audio streams (`mp3`, `wav`, `aac`, `flac`, `ogg`, `opus`, `wv`, `caf`, `aiff`, `au`, `w64`)"; AC4 — "The list holds at least `mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm` and `ts`, and every extension it holds names a container FFmpeg writes three mapped audio streams into at exit 0, never one it refuses for capacity." Measured at review on ffmpeg 9.0.1: `.ogg` and `.opus` refuse the three-AAC-track copy for a CODEC reason ("Unsupported codec id in stream 0"), not a capacity one, and take three distinct audio streams at exit 0 under `-c:a libopus` — the same shape as `.webm`, which the milestone measured, named a codec refusal, and listed. `separate_audio_video(3-track.mkv, "a.ogg", "v.mp4")` at the defaults still raises `tidymedia_multitrack_separation` advising a container that holds several, into one that does; the identical `.webm` call does not. Neither criterion is false as written — AC4 sets a membership floor and AC3 binds only that behaviour on the eleven is unchanged, both verified — but the repair the Goal demands puts `.ogg` and `.opus` in the list, which falsifies AC3's enumerated domain and AC4's seven. Maintainer's decision at the gate, over correcting the comment alone or filing the gap to the M45-F1 candidate row: return and gate them. Review's other four criteria verified as written; consistency gate clean; PR #95 open as a draft. Findings F2 (NEWS' "any failing audio command"), F3 (the false half of the batch-gate comment) and F7 (a missing `info = ext`) are directed fix-now into the same round; F4 and F5 are follow-ups; F6 rejected.
- 2026-08-30: T8 re-measured all twelve refusing extensions on ffmpeg 9.0.1 before touching source. `.ogg` and `.opus` refuse the three-AAC-track copy at exit 234 with "Unsupported codec id in stream 0" and take three distinct audio streams at exit 0 under `-c:a libopus` -- `.webm`'s shape, not the nine's. The other nine name capacity in their own words ("Exactly one MP3 audio stream is required", "wav muxer does not support more than one stream of type audio", "AIFF allows only one audio stream and a picture", and so on), and `.wv`, whose message names a codec, still exits 234 under `-c:a wavpack`, the one codec it holds.
- 2026-08-30: AMENDMENT (substantive, AC3 and AC4), executing the AC3/AC4 amendment return above. AC3's domain drops `ogg` and `opus`, leaving nine, and its gloss stops asserting WHY FFmpeg refused -- it now names the non-zero exit status T1 recorded, which is a fact of the record rather than a classification the milestone cannot make. AC4's membership floor grows from seven to nine. WIDENING under D-118, recorded per that rule: AC4 is the criterion widened, and because AC1 and AC2 quantify over "each extension the list holds", `ogg` and `opus` enter their domains too. The direction was the user's selection at the review gate, over filing the gap to the M45-F1 candidate row.
- 2026-08-30: the amended AC3 and AC4 went to two fresh-context [O] FULL criteria audits before they were written. The first returned six findings and a WIDENS verdict; four were taken -- T5's encoder sentence (AC4 is undecidable for two of nine without it), AC4's trailing "never one it refuses for capacity" dropped as a stderr-derived property the Scope's Out section excludes reading and the earlier round had already ruled non-binding, AC3's batch half given nine-extension coverage rather than the one it had, and a new T8 rather than a rewrite of executed T1. Its optional AC5 extension binding the help-page enumeration was declined at the mini gate. The revised bytes went to a second reader, which returned HOLDS (tilting narrow) with no criterion-wording finding and two repairs outside the criteria, both taken: T5's encoder sentence in its own words, and the source comment above `multi_audio_extensions`, false on two of the extensions it named.
- 2026-08-30: mini gate chose a numbered D071 over a milestone-local entry or a source comment alone, because D069 is what anyone growing `multi_audio_extensions` reads and the measurement procedure is the thing that failed; falsified by nothing outside this milestone ever growing the list.
- 2026-08-30: mini gate chose correcting the two help pages and `NEWS.md` to nine over interpolating the list into roxygen or binding the enumeration in a criterion, because this milestone already carries three defect returns and a criteria-set widening after a return is what D-118 exists to hold off, while the correction ships either way; falsified by the enumeration drifting a second time, which review's F4 follow-up now has a demonstrated trigger for.
- 2026-08-30: T8 landed. `ogg` and `opus` added to `multi_audio_extensions`; the comment above it corrected and given each absent extension's capacity wording; both roxygen blocks and `NEWS.md` corrected from seven to nine; D071 recorded. Review's fix-now items in the same round: F2 (`NEWS.md`'s "any failing audio command", false of the pre-change code, now the non-zero exit the branch gated), F3 (the batch-gate comment's false headline-count clause, dropped -- `warn_failed_separation()` applies its own `keep` filter and counts what survives it, so FFprobe cost carries the placement alone), F7 (the unlabelled `expect_no_error`, which takes no `info`, so the assertion became an explicit capture that labels the extension and shows FFmpeg's complaint). Tests: the codec-refuser defaults-path case generalized from `.webm` to all three, and a nine-extension batch per-row-bullet test added. Check discrimination three ways: reverting `ogg`/`opus` from the list turns 7 assertions red across the AC4 floor and the codec-refuser loop, forcing the gate TRUE turns 15 tests red including both new ones, and rewording the batch bullet turns the new batch test red at 9. None of the three leaves a new test green.
- 2026-08-30: amendment complete. `Rscript -e 'devtools::test()'` 0 failures / 8792 passing / 12 warnings / 5 skips (before this round: 0 / 8743 / 12 / 5; the +49 is the nine-extension batch bullet test, the codec-refuser loop's two new extensions and the two extensions AC4's loop gained, less the two AC3's loop shed). `Rscript -e 'devtools::check()'` Status: OK -- 0 errors, 0 warnings, 0 notes, 3m 36s. `devtools::document()` rewrote the two separation `.Rd` files alone. `cairn_validate` exit 0. Status -> review.

## Decisions

## Review

Re-reviewed 2026-08-30 against PR #95 (draft), after the AC3/AC4 amendment
(T8). This section replaces the previous round's, whose outcome was the
amendment return recorded in the work log. `master` had not moved since the
branch was cut (`git fetch`; `git rev-list --count origin/master ^HEAD` = 0),
the branch is pushed and its tip matches `origin`, and the working tree is
clean. All evidence below is fresh, by command, in this session.

### Acceptance-criterion evidence

- **AC1** — verified in both sub-cases across all nine listed extensions.
  Committed suite, run this session: "a listed container falls open to
  `ffm_run()`'s own condition" 45 assertions green (video half also failing, so
  no bullet is appended) and "a listed container falls open with the video half
  succeeding too" 90 assertions green. Each iterates `multi_audio_extensions`
  itself and asserts, against a reference condition from
  `ffm_run(separate_stream_pipeline(...))`: `tidymedia_multitrack_separation`
  absent, class vector identical, `tm_status` identical, and the message equal
  to the reference's plus exactly one trailing video-written bullet. Re-measured
  live at review on `.opus` with `audio_codec = "notanencoder"` and the video
  half at its default: video file written, class vector identical to the
  reference's, `tm_status` identical, and the only message difference the one
  "The video output was written to" bullet.
- **AC2** — two committed tests, 11 assertions green this session. "a failed
  batch row on a listed container contributes no bullet" (8): a two-row batch
  whose `.mka` and `.mp3` audio rows both fail warns naming only the `.mp3`
  output and "Input row 2", headline "1 audio output failed", with both rows
  confirmed `success = FALSE` so the silence is a drop and not a success. "a
  batch whose failed rows are all listed containers warns not at all" (3):
  `expect_no_warning` green with both rows failed.
- **AC3** — two committed tests over exactly the nine the criterion names
  (`mp3`, `wav`, `aac`, `flac`, `wv`, `caf`, `aiff`, `au`, `w64`), 117
  assertions green this session. The scalar half, "every measured refusing
  container keeps the enriched abort as worded" (90): per extension, the class
  vector `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")` at the
  head, `tm_status` equal to the status in the message, and the five bullets in
  order, each pinned by the phrase carrying its meaning. The batch half, "every
  unlisted container keeps the batch warning's per-row bullet" (27): the
  warning's class, its "1 audio output failed on a multi-track input" headline
  and the per-row bullet's exact sprintf form, per extension. The wording is
  also unchanged by construction: `git diff master..HEAD -- R/ffmpeg.R` adds
  only `multi_audio_extensions`, `holds_multiple_audio()`, the two gate lines
  and comments — no `cli_abort()` message body and no bullet form is touched.
- **AC4** — committed test "every listed container carries three mapped audio
  streams", 19 assertions green this session: the membership floor (all nine of
  `mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm`, `ogg`, `opus`, `ts` present in
  `multi_audio_extensions`), then, for each member of the vector itself,
  `separate_audio_video()` raising no error and the output carrying three
  distinct audio stream indices — `copy` for six, `libopus` for `webm`, `ogg`
  and `opus`. Re-measured independently at review outside the suite, on a
  three-AAC-track fixture with `-map 0:a` on ffmpeg 9.0.1: `mka`, `m4a`, `mp4`,
  `mov`, `mkv` and `ts` exit 0 under `-c:a copy`, `webm`, `ogg` and `opus` exit
  0 under `-c:a libopus`, each output carrying three distinct audio stream
  indices (`ts` lists each stream twice, once per program and once globally,
  and the three distinct indices are what survives deduplication).
- **AC5** — read from the generated `.Rd` on this branch.
  `man/separate_audio_video.Rd` states the four conditions as a conjunction —
  no `audio_stream` named, a non-zero exit status, more than one audio track,
  and an extension not among the nine it then lists. `man/separate_audio_video_batch.Rd`
  states its own four — named no `audio_stream`, recorded `success = FALSE`,
  more than one audio track, extension not among the nine — and then "No exit
  status is among those conditions", giving a non-zero exit, a hard error and a
  reached limit as causes recorded the same way. `grep -n -i exit
  man/separate_audio_video_batch.Rd` returns five lines: two are that denial and
  its explanation, three are about the condition object's own fields
  (`tm_status`, `tidymedia_ffmpeg_exit`, and where to catch an exit status
  instead), and none names an exit status among the conditions under which the
  warning fires. The section's opening sentence reads "A row whose audio command
  does not finish cleanly is recorded as `success = FALSE`" — no exit status.
  Both pages carry the silent-omission clause with its "never itself a second
  failure" half, and both state the report says what the call did, never why
  FFmpeg refused, naming the copy-into-incompatible-container, unknown-encoder
  and missing-directory causes.
- **AC6** — `Rscript -e 'devtools::test()'`: 0 failures, 8792 passing, 12
  warnings, 5 skips (baseline on `master` before the branch: 0 / 8493 / 12 / 5;
  previous review round: 8743). `Rscript -e 'devtools::check()'`: `Status: OK` —
  0 errors, 0 warnings, 0 notes, 4m 56s. Nothing to justify.

### Consistency gate

- `cairn_validate.py`: exit 0, all checks passed; the `release window` advisory
  did not fire.
- `cairn_impact.py`: skipped — `git diff master..HEAD -- cairn/DESIGN.md` is
  empty, so no principle changed.
- Toolchain slot (`r-package`): `devtools::document()` produced no diff
  (`git status --porcelain` empty after running it); `pkgdown::check_pkgdown()`
  "No problems found"; `README.md` is newer than `README.Rmd` and neither is
  touched by the branch; `NEWS.md` carries a user-facing entry with no milestone
  numbers; no files added at top level; `check()` reports 0 NOTEs.

### Independent review — three fresh-context lenses

Full three-lens fan-out (the diff touches executable surface and the declared
tier is user-facing). Findings ranked as reported, with dispositions.

**[O] diff-bug lens** — no correctness bug. It re-measured all nine listed
extensions at exit 0 with three mapped audio streams and all nine absent ones at
exit 234 under a codec each container natively holds, confirmed `.avi`, `.nut`,
`.m4b`, `.3gp`, `.wma` and `.asf` take three (so the "exclusion list, not a
survey" claim is true), confirmed `separate_stream_pipeline()` emits `-map 0:a`
so the measurement matches the verb's own command, and confirmed
`holds_multiple_audio()` is `NA`-, no-extension- and zero-length-safe while
`(bad + 1L) %/% 2L` still names the caller's row after the drop. Check
discrimination independently: with the predicate forced FALSE in an exported
copy of HEAD, 95 assertions fail across all six new gate tests. Also confirmed
`man/` regenerates byte-identically, no assertthat, nothing exported, and no
command-string gluing (IP1/D002). Five findings, all prose or coverage.

- **F1.** Two test comments still say *seven* where the list now holds nine:
  `tests/testthat/test-separate-av-multitrack.R:1055` ("reads the same 3 out of
  every one of the seven") and `:1094` ("the same failure reaches all seven").
  T8 corrected the source comment, both help pages and `NEWS.md` and missed
  these two — the same hand-enumeration drift class this milestone has now
  shipped twice. **Disposition: fix now.**
- **F2.** The source comment above `multi_audio_extensions` says the nine absent
  extensions were "Refused with exit 234 under every codec the container holds".
  T8's record measures one second run per container, not every codec; the lens
  spot-checked three more (`caf`/`alac`, `caf`/`pcm_s16be`, `w64`/`pcm_f32le`)
  and all still refuse, so the claim is likely true but is stronger than the
  evidence recorded. **Disposition: fix now** — narrow the comment to what was
  measured.
- **F3.** `?separate_audio_video`'s "Fail any of the four and the error you get
  is the one the run itself raised — same class, same exit status, same
  message…" is loose on one of the four legs it enumerates: when the failing leg
  is the non-zero exit itself (a missing binary), there is no exit status to be
  the same. The next paragraph states this correctly, so the page self-corrects,
  but the sentence is inaccurate for a case it names. **Disposition: fix now** —
  one clause.
- **F4.** Batch-side case-insensitivity is untested: `tolower()` is exercised
  only through the scalar path, and AC2's two batch tests use lowercase
  extensions. The predicate is shared, so the behaviour follows, but a
  batch-side regression on the extension read would not go red.
  **Disposition: follow-up** — a coverage gap, not a defect in shipped
  behaviour.
- **F5.** Milestone work-log lines still say "seven" (98, 141, 143, 146, 152,
  157). **Disposition: reject** — those are dated history entries recording what
  T1–T5 did before T8 added the two, and the work log is append-only.

**[S] blame-history lens** — no findings. It traced the M45 → M085 → M090 → M091
lineage against D024, D045/D046, D062, D065/D066, D068, D069, D070 and D071,
`LESSONS.md`'s M38 entry, the M45-F1 candidate row and M090's archive summary,
and confirmed in code that `stop(cnd)` re-raises the original condition so
`abort_after_video()`'s bullet and `tm_video_error` attach identically on all
three fail-open branches. It read the narrowing of M45's diagnostic as
documented and intentional and found no contradiction with D024's diagnostic
licence or D062's class-naming rule.

**[S] prior-review-record lens** — no findings. It verified every fix-now item
from the previous round landed in this diff: the `.ogg`/`.opus` gate (previous
F1), `NEWS.md`'s "any failing audio command" (F2), the false headline-count
clause in the batch-gate comment (F3), and the unlabelled `expect_no_error`
(F7). It confirmed the rejected F6 work-log line was correctly left untouched
and that F4 and F5, triaged follow-up last round, are still open by disposition
rather than newly reintroduced. The
`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` probe returned `[]`,
so no PR-thread walk ran, as on the previous three rounds.

### Outcome

All six criteria verified as written with fresh evidence; the consistency gate
is clean; the AC3/AC4 amendment holds, and the behaviour it was returned for is
fixed — `separate_audio_video()` on a three-track input into `.ogg` or `.opus`
at the defaults now raises FFmpeg's own condition rather than the multi-track
diagnostic, measured live at review, while `.mp3` keeps it. No driving RR, so no
projection to measure against. No finding demonstrates a criterion failing and
none is a load-bearing defect in shipped behaviour, so review does not return
the milestone on the floor. F1, F2 and F3 are mechanical prose corrections
proposed fix-now; F4 is a follow-up; F5 is rejected.
