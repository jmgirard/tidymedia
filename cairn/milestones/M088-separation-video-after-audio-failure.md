# M088: A failed audio half no longer costs the caller the video

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m088-separation-video-after-audio-failure`

## Goal

A `separate_audio_video()` run whose audio command fails still writes the video
file, and still reports the audio failure.

## Scope

Surface tier: **user-facing** — it changes what an exported verb leaves on disk
and what its error says.

**In:** the `run = TRUE` sequencing in `separate_audio_video()`
(`R/ffmpeg.R:938-945`): the audio failure is captured, the video command runs,
and the audio condition is then raised, carrying one added bullet naming the
video file when that command succeeded. The both-fail case. The roxygen
"When the audio output fails" section, `@return`, and the `NEWS.md` entry.

**Out:** a symmetric `video_stream` selector, and the note that `ffm_concat()` /
`concatenate_videos()` map all streams unconditionally → both stay on their
ROADMAP candidate row, narrowed by this milestone. Any change to
`separate_audio_video_batch()` or to `ffm_batch()`'s per-row contract → the
batch already runs both rows; its own leftovers keep their existing rows.
Layer 1's failed-output removal → unchanged, relied on here (D046).

## Acceptance criteria

- [ ] AC1: With `run = TRUE`, a call whose audio command exits non-zero and
      whose video command succeeds creates `videofile` at a path that did not
      exist before the call, and the call still aborts.
- [ ] AC2: On the AC1 path the raised condition's class vector and its
      `tm_status` field hold the values the same failure raises on `master`, in
      each of two branches of `run_separation_audio()` (`R/ffmpeg.R:645-661`):
      the enriched multi-track branch (`tidymedia_multitrack_separation`,
      `tidymedia_ffmpeg_exit`) and the `n <= 1L` fall-open branch, which
      re-raises the original non-zero-exit condition. The `is.na(status)` and
      `is.na(n)` fall-open branches are out of this criterion.
- [ ] AC3: When the audio command and the video command both exit non-zero, the
      call aborts with the audio command's condition, and the video output that
      failed video run created or changed is absent from disk after the call.
- [ ] AC4: On the AC1 path the abort's rendered message contains the string
      `The video output was written to` followed by `videofile`'s basename; on
      the AC3 path that string does not appear in the rendered message.
- [ ] AC5: `?separate_audio_video`'s "When the audio output fails" section, its
      `@return` text, and the `NEWS.md` entry each state the run order and what
      each of the two failure paths leaves on disk.
- [ ] AC6: The batch run path — `separate_audio_video_batch()`
      (`R/ffmpeg.R:5850`) and `warn_failed_separation_batch()`
      (`R/ffmpeg.R:757`) — is untouched by the branch diff, and
      `tests/testthat/test-separate-audio-video-batch.R` and
      `test-separate-av-multitrack.R` pass with no edits to their pre-existing
      expectations.
- [ ] AC7: `devtools::test()` clean and `devtools::check()` at 0 errors /
      0 warnings (the profile's verify slot); every new execution test
      `skip_if` the ffmpeg binary is absent.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T4
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T6
- AC7 → T6

## Tasks

- [ ] T1: Tests first, in `tests/testthat/test-separate-av-multitrack.R`: the
      AC1 path (multi-track input into a single-stream audio container, valid
      `videofile` at a fresh path), the AC3 path (same, plus an unknown
      `video_codec`), and the AC4 message assertions. Record `master`'s class
      vector and `tm_status` for AC2's two branches before touching `R/`.
- [ ] T2: Reorder `separate_audio_video()`'s `run = TRUE` block
      (`R/ffmpeg.R:938-945`): capture the condition `run_separation_audio()`
      raises, run `ffm_run(video)`, then raise.
- [ ] T3: The both-fail branch — the video run's own condition is discarded
      after Layer 1 has removed what that run wrote (D046), and the captured
      audio condition is the one raised.
- [ ] T4: Raise the captured condition with the AC4 bullet appended, preserving
      the class vector and `tm_status` exactly (AC2); route `videofile` through
      a cli field so a brace-bearing path cannot be interpolated (M44's lesson).
- [ ] T5: Roxygen "When the audio output fails" section and `@return`;
      `devtools::document()`; `NEWS.md` entry. Prose derived from an executed
      call, never composed.
- [ ] T6: `devtools::test()` and `devtools::check()`; confirm the AC6 batch
      sites are absent from the diff.
- [ ] T7: Append the D-entry recording the run-order choice and its falsifier;
      narrow the ROADMAP candidate row to its two remaining halves.

## Work log

- 2026-08-29: created by /milestone-plan.
- 2026-08-29: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader; returned six findings across AC1/AC2/AC4/AC5/AC6, all with one clear answer and all fixed before the criteria were written — AC1 satisfiable by a pre-existing video file, AC2's "fall-open case" covering three branches with differing class vectors, AC4's naming clause having no textual test, AC5 binding how the prose was authored rather than what it states, AC6 promising "unchanged" over all batch inputs where only two test files were named. AC3 clean.
- 2026-08-29: plan gate chose running the video command after the audio command fails over keeping today's early abort; the batch sibling already runs both rows and the divergence has no stated rationale. Falsified by a report of a caller who needed the split to be all-or-nothing.
- 2026-08-29: plan gate chose adding a bullet naming the written video over leaving the message byte-identical; the message otherwise describes a situation it no longer covers. Falsified by a report of a caller matching on that message's text.
- 2026-08-29: plan gate chose raising the audio condition alone in the both-fail case over naming both failures; one message correct across every combination of two failures is more surface than the case earns. Falsified by a report of a caller who could not tell the video command had also failed.
- 2026-08-29: plan gate chose keeping audio-first ordering over swapping to video-first; swapping changes which command runs first on every successful call too. Falsified by a failure whose diagnosis depends on the video command having already run.
- 2026-08-29: implement gate chose appending one formatted bullet to the audio condition's own body over rebuilding the condition (the rebuild re-runs cli's formatter over already-formatted text, M44's brace trap, and copies fields by hand); and chose letting ANY audio-run failure fall through to the video command over only a non-zero FFmpeg exit (one rule to document, and the excluded causes are ones the video command fails on too).
- 2026-08-29: T1 tests written first and confirmed red against unchanged `R/` — six new tests in `test-separate-av-multitrack.R`; `master`'s class vectors and `tm_status` for AC2's two branches recorded in the file's own comment (ffmpeg 9.0.1, macOS arm64, status 234).

## Decisions

## Review
