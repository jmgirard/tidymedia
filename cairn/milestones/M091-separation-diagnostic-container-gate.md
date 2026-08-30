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

- [ ] AC1: A `separate_audio_video()` audio command that FFmpeg ends at a non-zero
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
- [x] AC3: On the extensions measured 2026-08-30 as refusing three mapped audio
      streams (`mp3`, `wav`, `aac`, `flac`, `ogg`, `opus`, `wv`, `caf`, `aiff`,
      `au`, `w64`), the scalar abort keeps the class vector
      `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")`, its
      `tm_status` field and its five bullets as currently worded, and the batch
      warning keeps its class and its per-row bullet form.
- [x] AC4: The list holds at least `mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm` and
      `ts`, and every extension it holds names a container FFmpeg writes three
      mapped audio streams into at exit 0, never one it refuses for capacity.
- [x] AC5: `?separate_audio_video` and `?separate_audio_video_batch` each state
      that the diagnostic fires only when no `audio_stream` was named, FFmpeg
      returned a non-zero exit, the input carries more than one audio track, and
      the output extension is not on the list; that it may silently not fire when
      the track count is unanswerable (D024's documentation requirement); and
      that it reports what the call did, never why FFmpeg refused.
- [x] AC6: `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T2, T3, T5
- AC2 → T2, T4, T5
- AC3 → T3, T4, T5
- AC4 → T1, T2, T5
- AC5 → T6
- AC6 → T7

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
      every listed extension except `webm`, which holds no AAC and takes
      `-c:a libopus` — the encoders T1 measured, named here so the criterion is
      decidable without them. The suppression case runs twice per extension:
      once with the video half failing too, once with it left at its default so
      it succeeds and writes, which is the sub-case AC1's video-written
      exception covers.
- [x] T6: Roxygen on both verbs plus `R/audio-stream-doc.R` if its shared
      sentence needs it; `devtools::document()`; `NEWS.md` entry.
- [x] T7: D-entry recording the gate, its measured basis, the rejected
      alternatives and the causes left indistinguishable; `devtools::check()`.

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

## Decisions

## Review

Reviewed 2026-08-30 against PR #95 (draft). `master` had not moved since the
branch was cut, so no merge-forward was needed. Evidence below is fresh, by
command, in this session.

### Acceptance-criterion evidence

- **AC1** — **NOT VERIFIED.** The per-extension test is green (35 assertions,
  five per extension over all seven of `multi_audio_extensions`): class vector
  identical to the reference `ffm_run()` condition and `tidymedia_multitrack_separation`
  absent, `tm_status` identical, `conditionMessage()` identical, the reference
  itself asserted `tidymedia_ffmpeg_exit`. But that test gives BOTH halves the
  missing encoder, so the video half fails too. Measured at review with the video
  half succeeding — the ordinary case — into `.mka` with
  `audio_codec = "notanencoder"`: class vector identical, `tm_status` identical,
  message NOT identical. The verb's condition carries one extra bullet,
  "The video output was written to '.../v.mp4'", which `abort_after_video()`
  appends whenever the video half wrote a file. So AC1's "same message" clause is
  false across the ordinary half of its own domain.

  The code is right and the criterion is wrong. That bullet is M090's deliberate
  contract: `abort_after_video()` appends it to whatever condition the audio half
  raised, agnostic to which branch produced it, and it already rides the two
  pre-existing fail-open branches (`is.na(status)`, `n <= 1L`) the same way.
  Stripping it on the new container branch would silently undo M090. AC1 was
  written without accounting for a note a different function adds downstream of
  the branch AC1 is about. This routes as an amendment return under the
  never-reinterpret rule, not as a defect return.

- **AC2** — same file, two tests, 11 assertions green. "a failed batch row on a
  listed container contributes no bullet": a two-row batch whose `.mka` row and
  `.mp3` row both fail warns naming only `blamed.mp3` and "Input row 2", with
  the headline reading "1 audio output failed", and both rows confirmed
  `success = FALSE` so the silence is a drop and not a success. "a batch whose
  failed rows are all listed containers warns not at all": `expect_no_warning`
  green with both rows failed.
- **AC3** — verified at review across all eleven refusing extensions, not only
  the two the committed tests exercise. A review-side script ran
  `separate_audio_video()` with a missing encoder on a three-track input into
  `mp3`, `wav`, `aac`, `flac`, `ogg`, `opus`, `wv`, `caf`, `aiff`, `au` and
  `w64`: every one kept the class vector
  `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")`, an integer
  `tm_status` of 8, and all five bullets, at 18 message lines each. The batch
  form over the same eleven rows warned with class
  `tidymedia_multitrack_separation`, headline "11 audio outputs failed", every
  output named and "Input row 1".."Input row 11" present. The wording is
  unchanged by construction: `git diff master..HEAD -- R/ffmpeg.R` shows the
  `cli_abort()` message body untouched, the only code changes being the new
  vector, the predicate, and three gate lines.
- **AC4** — same test file, test "every listed container carries three mapped
  audio streams": 15 assertions green. It asserts the membership floor
  (`mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm`, `ts` all present) and then, for
  each member of `multi_audio_extensions` itself, that
  `separate_audio_video()` raises no error and the written output carries three
  distinct audio stream indices — `copy` for six, `libopus` for `webm`, the
  encoders T1 measured.
- **AC5** — read from the generated `.Rd` on this branch. Both
  `man/separate_audio_video.Rd` and `man/separate_audio_video_batch.Rd` state
  the four conditions as a conjunction (no `audio_stream`, non-zero exit, more
  than one audio track, extension not among the seven, which each page lists);
  both state the silent-omission clause ("the report may simply not appear, and
  its absence is never itself a second failure" / the batch equivalent); and
  both state that the report says what the call did, never why FFmpeg refused,
  naming the copy-into-incompatible-container, unknown-encoder and
  missing-directory causes that look alike from there. The scalar page's prior
  claim that the report attaches to "any failing audio command on a multi-track
  input" is gone, which this branch falsifies.
- **AC6** — `Rscript -e 'devtools::test()'`: 0 failures, 8563 passing, 12
  warnings, 5 skips (baseline on `master` before the branch: 0 / 8493 / 12 / 5;
  the +70 is exactly the seven new tests' assertion counts).
  `Rscript -e 'devtools::check()'`: `Status: OK` — 0 errors, 0 warnings, 0
  notes, 6m 23s. Nothing to justify.

### Consistency gate

- `cairn_validate.py`: exit 0, all checks passed; the `release window` advisory
  did not fire.
- `cairn_impact.py`: skipped — no `DESIGN.md` principle changed on this branch.
- Toolchain slot (`r-package`): `devtools::document()` produced no diff;
  `pkgdown::check_pkgdown()` "No problems found"; `README.md` is newer than
  `README.Rmd` and neither was touched; `NEWS.md` carries a user-facing entry
  with no milestone numbers; no new top-level files, and `check()` reports no
  NOTEs.

### Independent review — three fresh-context lenses

Full three-lens fan-out (the diff touches executable surface and the declared
tier is user-facing). Findings ranked as reported, with dispositions.

**[O] diff-bug lens** — seven findings.

- **O1 (routes the amendment return).** "the error you get is the one the run
  itself raised, unchanged" is false on the common sub-case, and the AC1 test is
  built to avoid it. `abort_after_video()` still appends the video-written
  bullet on the fail-open path; the AC1 test gives both halves the bad encoder
  specifically so that bullet is absent. Reproduced at review — see the AC1
  evidence above. **Disposition: routes the amendment return on AC1.** The code
  stands; the criterion is amended.
- **O2.** The goal is not met for unlisted containers that do hold several audio
  streams, and D069 presents that as a hypothesis rather than a measured fact.
  Verified at review on ffmpeg 9.0.1 with the suite's three-AAC fixture and
  `-map 0:a -c:a copy`: `avi`, `nut`, `m4b`, `3gp`, `wma` and `asf` all exit 0
  carrying three distinct audio streams, and none is in
  `multi_audio_extensions`; `.m4b` is `.m4a`'s own sibling. AC4 sets a
  membership floor, so this is not a criterion failure, but D069's falsifier is
  written as a future possibility while it is true today for six measured
  extensions, and neither help page tells a caller the list is not exhaustive.
  **Disposition: to the maintainer at the gate.**
- **O3.** The batch help page states a condition the batch code does not
  implement: it promises a row reaches the warning "only when ... FFmpeg
  returned a non-zero exit status", but `warn_failed_separation_batch()` selects
  on `!out$success` alone and `ffm_batch()`'s `run_one()` sets
  `success = FALSE` for any row error — a missing binary, a reached limit —
  via `!inherits(res, "error") && is.null(attr(res, "status"))`
  (`R/ffm_batch.R:144`). Verified at review by reading that function. The
  ROADMAP already records the underlying fact ("the warning fires for any
  failure cause"). The claim is NEW in this diff — the pre-change page made no
  exit-status claim. The scalar page's identical claim is backed by code.
  **Disposition: to the maintainer at the gate.**
- **O4.** NEWS overstates `.webm`: it calls the seven "every one of which takes
  three audio tracks without complaint", but `.webm` refuses the fixture's three
  AAC tracks at exit 234 and takes three only under opus or vorbis — which the
  code comment, the work log and the test helper all state and NEWS does not. A
  reader of NEWS alone would conclude a default
  `separate_audio_video(x, "a.webm", ...)` succeeds; it does not.
  **Disposition: to the maintainer at the gate.**
- **O5.** AC3 quantifies over eleven extensions and the committed suite
  exercises two (`.mp3` and `.aac`). Confirmed. Review verified all eleven
  directly (see AC3 above), so the criterion holds, but the durable suite does
  not carry that coverage. **Disposition: to the maintainer at the gate.**
- **O6.** The FFprobe-timeout warning becomes unreachable from these two sites
  for listed outputs, since both gates were deliberately moved ahead of
  `count_audio_streams_all()`, the function that emits it. The reviewer notes it
  does not break D049 — no probe starts, so no limit is reached. **Disposition:
  reject.** The gate ordering is what T3 and T4 planned, and their comments state
  the reason; a real defect inside an intentional change would still count, and
  this is the intended consequence rather than a flaw in carrying it out.
- **O7.** A missing `output` column would now silently suppress the warning
  instead of erroring: `out$output[bad]` is `NULL` when the column is absent, so
  the filter yields `integer(0)` and the next line returns early. Only reachable
  if `ffm_batch()`'s contract changes. **Disposition: reject** — latent
  robustness note on a contract the package controls, not a live defect.
- Nits, no action implied: `holds_multiple_audio()` reads as a capacity claim
  only conditionally true for `.webm`; the scalar page's opening sentence drops
  the "and no `audio_stream` was named" qualifier the following paragraph
  reinstates.

**[S] blame-history lens** — no defects. It traced the touched regions through
M45, M085-M088 and M090 and D024/D029/D030/D069 and found nothing that undoes a
past milestone's guard, resurrects a fixed bug, or contradicts a decision entry.
One low-severity pattern note: `multi_audio_extensions` is the hand-enumerated
extension-keyed container list D030 retired for `normalize_audio()` after two
misses — but here it is an exclusion list that fails toward the safe direction on
an omission, which D069 states outright, so it is not a silent repeat. **Reject
(no action).** It also verified that the reordering of the status check, the
container gate and the track probe is logically equivalent to the old single
`if`, and that M090's contract is untouched.

**[S] prior-review-record lens** — one finding. The test "a stream copy into
.webm no longer blames the track count" triggers FFmpeg's failure through the
default codec behaviour with no version-conditional skip, which the archived M45
lesson (now in `cairn/references/false-greens.md`) warns against where the
refusal itself is the subject; CI runs macOS and several Ubuntu builds, the same
split that produced M45's original failure. The rest of the new suite avoids this
by using an encoder no build has. **Disposition: to the maintainer at the gate,
noting that the failure mode here is a red rather than a false green** — if a
build accepted AAC into WebM the test's first assertion would fail loudly, which
is the opposite of the silent pass M45's lesson guards against. The
`gh api .../pulls/comments` probe returned empty, so no PR-thread walk ran.

### Outcome

Review returns M091 to `in-progress` for a criterion amendment on AC1 alone.
Every other criterion is verified with fresh evidence, the consistency gate is
clean, and no reported finding demonstrates a defect in what the code does.

