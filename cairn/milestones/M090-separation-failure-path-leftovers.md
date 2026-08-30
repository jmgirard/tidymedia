# M090: The both-fail path stops throwing away what it knows

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m090-separation-failure-path-leftovers`

## Goal

A `separate_audio_video()` call whose video half also failed hands the caller
that failure programmatically, and the video-written line is gated on the file
actually being written.

## Scope

Surface tier: **user-facing** — every deliverable here is the shipped condition
object, its rendered message, or `?separate_audio_video`'s prose.

**In:** four of the five findings the M088 review passes deferred.
(a) The video run's condition is stashed on the raised audio condition at
`tm_video_error` (D062's field prefix); the rendered message is unchanged, so
D065's one-message reasoning survives and only its "Why the both-fail case names
one failure" section is superseded. (b) `wrote` is decided by a pre/post
`output_snapshot()` comparison over `videofile` rather than by the video run's
exit status alone — a Layer 2 filesystem read on the executing path, licensed by
D024's diagnostic clause and D037 (IP1 governs command assembly, not
validation), not a new licence. (c) `abort_after_video()`'s bare-condition
fallback goes, and the note's append is explicitly guarded so a bare condition
loses the note visibly in the source rather than through a `body` assignment
`stop()` ignores. (d) The roxygen states the second-spawn cost D066 measured,
and drops the sentence (a) falsifies.

**Out:** the SIGINT finding — a Ctrl-C reaching only the child surfacing as a
non-zero exit and spawning a second run. Unreproduced and with no bounded
investigation (M078's precedent), it stays on the candidate row it shares with
these four, promoted on the first report of Ctrl-C failing to return to the
prompt. Any change to what the *message* says about the video failure → not
here; D065's one-message reasoning stands.

## Acceptance criteria

- [ ] AC1 On the both-fail path of `separate_audio_video(run = TRUE)`,
      `cnd$tm_video_error` returns the video run's own condition object; when
      the video command succeeded, `cnd$tm_video_error` returns `NULL`. On both
      of the audio run's two branches — the enriched multi-track diagnostic and
      the `n <= 1L` fall-open — the condition's class vector and `tm_status` are
      the ones the audio run itself raised, and the rendered message names no
      video failure.
- [ ] AC2 `separate_audio_video()` decides the "The video output was written to"
      line by comparing an `output_snapshot()` of `videofile` taken before the
      video run against one taken after, not by the run's exit status alone.
      Three tests that mock `ffm_run()` per call, dispatching on `object$output`:
      the audio call fails and the video call returns successfully without
      touching `videofile`, on a fresh path and on a path already holding a
      file — no line in either; and the audio call fails while the video call
      rewrites a `videofile` that already existed — the line is there. The two
      M088 tests that expect the line on a fresh path
      (`test-separate-av-multitrack.R:620` and `:645`) still pass.
- [ ] AC3 The video-written note reaches the caller as an `i` bullet on every
      path that renders it, there being one such path; a condition that is not
      an `rlang_error` reaches `stop()` unchanged and without the note, refused
      by an explicit guard rather than by an append `stop()` would discard.
- [ ] AC4 `?separate_audio_video`'s "When the audio output fails" section states
      that a reached wall-clock limit on the audio command still lets the video
      command run on its own fresh budget, so such a call can wait up to two
      limits rather than one — the behavior D066 measured — and no longer
      contains the sentence "nothing reports `videofile`'s fate on the both-fail
      path, because the video command's error is not the one you get"
      (`R/ffmpeg.R:909-913`), which AC1 falsifies.
- [ ] AC5 `devtools::test()` and `devtools::check()` are clean (0 errors,
      0 warnings; NOTEs justified) and `devtools::document()` produces no diff.

## Coverage

- AC1 → T2, T3
- AC2 → T4, T5
- AC3 → T6, T7
- AC4 → T8
- AC5 → T9

## Tasks

- [x] T1 Append the D-entry superseding **only** D065's "Why the both-fail case
      names one failure" section: the video condition is stashed, not reported;
      the rendered message is unchanged; the field is `tm_video_error` per D062.
      Record D065's met falsifier as the reason.
- [x] T2 Tests first: on the both-fail path assert `cnd$tm_video_error` is the
      video run's condition, and on the video-succeeded path assert it is
      `NULL`; assert the class vector, `tm_status` and message text are
      unchanged on both of the audio branches
      (`tests/testthat/test-separate-av-multitrack.R`, beside the existing
      both-fail test at `:660`).
- [x] T3 Carry the video condition out of the `tryCatch()` at
      `R/ffmpeg.R:1019-1023` and attach it in `abort_after_video()`
      (`R/ffmpeg.R:708`).
- [ ] T4 Tests first for the `wrote` gate: three mocked-`ffm_run()` cases per
      AC2, dispatching on `object$output`. The mocked audio error must be
      status-free (so `run_separation_audio()` takes the fall-open at
      `R/ffmpeg.R:660`) or the call must name `audio_stream`, otherwise the verb
      shells out to real FFprobe at `R/ffmpeg.R:658`; `infile` must exist on
      disk for `check_file_readable()` at `R/ffmpeg.R:968`.
- [ ] T5 Snapshot `videofile` before `ffm_run(video)` and again after, and gate
      `wrote` on the comparison (`output_snapshot()`, `R/ffm.R:1421`).
- [ ] T6 Test that a bare `simpleError` passed to `abort_after_video()` with a
      written video reaches the caller unchanged and without the note.
- [ ] T7 Delete the `else` branch at `R/ffmpeg.R:722-723` and guard the `body`
      append on `inherits(cnd, "rlang_error")`; replace the source comment with
      one recording that a bare condition now loses the note.
- [ ] T8 Roxygen: add the second-spawn sentence to "When the audio output
      fails", remove the sentence AC4 names, and state that the video failure is
      on `tm_video_error`. `devtools::document()`.
- [ ] T9 `NEWS.md` entry; full `devtools::test()` and `devtools::check()`.

## Work log

- 2026-08-29: created by /milestone-plan. Promoted from the candidate row added 2026-08-29 (M088 review passes 1 F3/F6/F8 and 2 F2/F3); finding (e) stays on that row.
- 2026-08-29: criteria audit ran in FULL mode (surface tier user-facing), two rounds, fresh-context [O] reader. Round 1 returned nine findings: three went to the question gate (D065 collision on AC1; existence-vs-authorship on AC2; the unreachable non-`rlang` branch on AC3), five were fixed at the gate, one (AC5's "NOTEs justified") was noted and left as repo-standard wording. Round 2 on the revised text returned eight, all fixed at the gate and reported in chat; AC4 came back CLEAN in both rounds.
- 2026-08-29: plan gate chose stashing the video condition on a `tm_` field over reporting it in the message, because D065's "one message correct across every combination of two failures is more surface than the case earns" survives a field that changes no rendered text; falsified by a report of a caller who needed the video failure in the text a human reads.
- 2026-08-29: plan gate chose an authorship predicate for `wrote` — D046's pre/post size-and-mtime comparison — over `file.exists(videofile)`, because existence is satisfied by a pre-existing file the run never opened, the case `test-separate-av-multitrack.R:718-740` already pins; falsified by a measured cost for the extra `file.info()` pair, or by a snapshot that cannot tell a rewrite from an untouched file.
- 2026-08-29: plan gate chose deleting `abort_after_video()`'s bare-condition fallback over testing it, because the source records that nothing can reach it and a test of an unobservable path sits below this milestone's surface tier; falsified by a bare condition reaching the note with a written video, which would arrive with the note dropped.
- 2026-08-29: plan gate chose leaving the SIGINT finding on the candidate row over a timeboxed reproduce-attempt task, because an unreproduced Ctrl-C interaction has no bounded investigation and M078 is the precedent for what that costs; falsified by a report of Ctrl-C failing to return to the prompt.
- 2026-08-29: implement gate — chose the snapshot comparison alone on both video-run outcomes over an exit-status short-circuit, and attaching `tm_video_error` unconditionally over guarding it with the note's `rlang_error` check. Both recommendations; both taken.
- 2026-08-29: T1 — D068 appended, superseding only D065's "Why the both-fail case names one failure" section. Reason recorded: D065's own falsifier — a caller who could not tell from the condition alone that the video command had also failed — is met.
- 2026-08-30: T2/T3 — the video run's condition is held beside the audio one and attached at `tm_video_error`; NULL when the video command succeeded. Two tests over both audio branches (both-fail and video-succeeded); the field tests were red on the pre-change code at the attachment, and the whole suite is 8,460 pass / 0 fail.

## Decisions

## Review
