<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M091: The multi-track advice stops arriving when the caller is already following it

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m091-separation-diagnostic-container-gate`

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

- [ ] AC1: A failing `separate_audio_video()` audio command with a >1-audio-track
      input, no `audio_stream`, and an `audiofile` extension held by the new list
      raises the condition `ffm_run()` raises for that pipeline — same message,
      same class vector (no `tidymedia_multitrack_separation`), same `tm_status`
      — for each extension the list holds.
- [ ] AC2: In `separate_audio_video_batch()`, a failed audio row whose output
      extension is held by the list contributes no bullet to the post-fan-out
      warning, and a batch whose failed audio rows all have such outputs signals
      no warning at all.
- [ ] AC3: On the extensions measured 2026-08-30 as refusing three mapped audio
      streams (`mp3`, `wav`, `aac`, `flac`, `ogg`, `opus`, `wv`, `caf`, `aiff`,
      `au`, `w64`), the scalar abort keeps the class vector
      `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")`, its
      `tm_status` field and its five bullets as currently worded, and the batch
      warning keeps its class and its per-row bullet form.
- [ ] AC4: The list holds at least `mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm` and
      `ts`, and every extension it holds names a container FFmpeg writes three
      mapped audio streams into at exit 0, never one it refuses for capacity.
- [ ] AC5: `?separate_audio_video` and `?separate_audio_video_batch` each state
      that the diagnostic fires only when no `audio_stream` was named, FFmpeg
      returned a non-zero exit, the input carries more than one audio track, and
      the output extension is not on the list; that it may silently not fire when
      the track count is unanswerable (D024's documentation requirement); and
      that it reports what the call did, never why FFmpeg refused.
- [ ] AC6: `Rscript -e 'devtools::test()'` clean and `Rscript -e
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
      decidable without them.
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

## Decisions

## Review
