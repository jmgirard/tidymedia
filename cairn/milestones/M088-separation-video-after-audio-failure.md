# M088: A failed audio half no longer costs the caller the video

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m088-separation-video-after-audio-failure` / https://github.com/jmgirard/tidymedia/pull/92

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

- [x] AC1: With `run = TRUE`, a call whose audio command exits non-zero and
      whose video command succeeds creates `videofile` at a path that did not
      exist before the call, and the call still aborts.
- [x] AC2: On the AC1 path the raised condition's class vector and its
      `tm_status` field hold the values the same failure raises on `master`, in
      each of two branches of `run_separation_audio()` (`R/ffmpeg.R:645-661`):
      the enriched multi-track branch (`tidymedia_multitrack_separation`,
      `tidymedia_ffmpeg_exit`) and the `n <= 1L` fall-open branch, which
      re-raises the original non-zero-exit condition. The `is.na(status)` and
      `is.na(n)` fall-open branches are out of this criterion.
- [x] AC3: When the audio command and the video command both exit non-zero, the
      call aborts with the audio command's condition, and the video output that
      failed video run created or changed is absent from disk after the call.
- [x] AC4: On the AC1 path the abort's rendered message contains the string
      `The video output was written to` followed by `videofile`'s basename; on
      the AC3 path that string does not appear in the rendered message.
- [x] AC5: `?separate_audio_video`'s "When the audio output fails" section, its
      `@return` text, and the `NEWS.md` entry each state the run order and what
      each of the two failure paths leaves on disk.
- [x] AC6: The batch run path — `separate_audio_video_batch()`
      (`R/ffmpeg.R:5850`) and `warn_failed_separation_batch()`
      (`R/ffmpeg.R:757`) — is untouched by the branch diff, and
      `tests/testthat/test-separate-audio-video-batch.R` and
      `test-separate-av-multitrack.R` pass with no edits to their pre-existing
      expectations.
- [x] AC7: `devtools::test()` clean and `devtools::check()` at 0 errors /
      0 warnings (the profile's verify slot); every new execution test
      `skip_if` the ffmpeg binary is absent.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T4
- AC3 → T1, T3
- AC4 → T1, T4, T9
- AC5 → T5, T9, T10
- AC6 → T6
- AC7 → T6

## Tasks

- [x] T1: Tests first, in `tests/testthat/test-separate-av-multitrack.R`: the
      AC1 path (multi-track input into a single-stream audio container, valid
      `videofile` at a fresh path), the AC3 path (same, plus an unknown
      `video_codec`), and the AC4 message assertions. Record `master`'s class
      vector and `tm_status` for AC2's two branches before touching `R/`.
- [x] T2: Reorder `separate_audio_video()`'s `run = TRUE` block
      (`R/ffmpeg.R:938-945`): capture the condition `run_separation_audio()`
      raises, run `ffm_run(video)`, then raise.
- [x] T3: The both-fail branch — the video run's own condition is discarded
      after Layer 1 has removed what that run wrote (D046), and the captured
      audio condition is the one raised.
- [x] T4: Raise the captured condition with the AC4 bullet appended, preserving
      the class vector and `tm_status` exactly (AC2); route `videofile` through
      a cli field so a brace-bearing path cannot be interpolated (M44's lesson).
- [x] T5: Roxygen "When the audio output fails" section and `@return`;
      `devtools::document()`; `NEWS.md` entry. Prose derived from an executed
      call, never composed.
- [x] T6: `devtools::test()` and `devtools::check()`; confirm the AC6 batch
      sites are absent from the diff.
- [x] T7: Append the D-entry recording the run-order choice and its falsifier;
      narrow the ROADMAP candidate row to its two remaining halves.
- [x] T8 (discovered): the new handler makes `separate_audio_video()` a member
      of the derived timeout-absorber partition, so
      `tests/testthat/test-timeout-silence.R` records it there and adds it to
      that file's abort half, which requires its forced timeout to reach the
      caller still carrying `tidymedia_timeout`.
- [x] T9 (return fix, F7): the pre-existing-output guards behind AC5's
      corrected prose — an `audiofile` an unknown audio codec never wrote to,
      and a `videofile` the both-fail path never wrote to — plus the missing
      `audiofile` assertion and per-branch labels in AC4's loop.
- [x] T10 (return fix, F2): the roxygen "When the audio output fails" section,
      `@return` and the `NEWS.md` entry corrected to what D046 actually
      promises, derived from executed calls that include a pre-existing output
      file; `devtools::document()`.
- [x] T11 (return fix, F1): the `run = TRUE` comment's timeout claim corrected
      in place against a measured run; D066 appended superseding D065's
      "Why any audio failure falls through" section.
- [x] T12 (return fix, F5): the `.gitignore` line named in the work log.

## Work log

- 2026-08-29: created by /milestone-plan.
- 2026-08-29: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader; returned six findings across AC1/AC2/AC4/AC5/AC6, all with one clear answer and all fixed before the criteria were written — AC1 satisfiable by a pre-existing video file, AC2's "fall-open case" covering three branches with differing class vectors, AC4's naming clause having no textual test, AC5 binding how the prose was authored rather than what it states, AC6 promising "unchanged" over all batch inputs where only two test files were named. AC3 clean.
- 2026-08-29: plan gate chose running the video command after the audio command fails over keeping today's early abort; the batch sibling already runs both rows and the divergence has no stated rationale. Falsified by a report of a caller who needed the split to be all-or-nothing.
- 2026-08-29: plan gate chose adding a bullet naming the written video over leaving the message byte-identical; the message otherwise describes a situation it no longer covers. Falsified by a report of a caller matching on that message's text.
- 2026-08-29: plan gate chose raising the audio condition alone in the both-fail case over naming both failures; one message correct across every combination of two failures is more surface than the case earns. Falsified by a report of a caller who could not tell the video command had also failed.
- 2026-08-29: plan gate chose keeping audio-first ordering over swapping to video-first; swapping changes which command runs first on every successful call too. Falsified by a failure whose diagnosis depends on the video command having already run.
- 2026-08-29: implement gate chose appending one formatted bullet to the audio condition's own body over rebuilding the condition (the rebuild re-runs cli's formatter over already-formatted text, M44's brace trap, and copies fields by hand); and chose letting ANY audio-run failure fall through to the video command over only a non-zero FFmpeg exit (one rule to document, and the excluded causes are ones the video command fails on too).
- 2026-08-29: T2-T4 landed — the audio run's condition is held, `ffm_run(video)` runs either way, and the held condition is re-raised with one formatted bullet appended to its body when the video was written; the both-fail branch discards the video condition. All six T1 tests green.
- 2026-08-29: T8 (discovered sub-task, minor amendment): `devtools::test()` reddened the derived timeout-absorber pin — the new handler makes `separate_audio_video()` an absorber. Recorded there with why a held timeout is not a swallowed one, and added to the same file's abort half so its forced timeout must still carry `tidymedia_timeout`.
- 2026-08-29: T5 roxygen `@return` and "When the audio output fails" section rewritten from two executed calls (the AC1 and AC3 messages, both read off a real run); `devtools::document()`; NEWS.md entry under Bug fixes.
- 2026-08-29: T7 D065 appended; the M45-leftovers ROADMAP row narrowed to its two remaining halves.
- 2026-08-29: T6 `devtools::test()` clean (0 failures) and `devtools::check()` at 0 errors / 0 warnings / 0 notes; the branch diff touches `R/ffmpeg.R` in three hunks, none of them reaching `warn_failed_separation_batch()` or `separate_audio_video_batch()`. Status to review.
- 2026-08-29: T1 tests written first and confirmed red against unchanged `R/` — six new tests in `test-separate-av-multitrack.R`; `master`'s class vectors and `tm_status` for AC2's two branches recorded in the file's own comment (ffmpeg 9.0.1, macOS arm64, status 234).
- 2026-08-29: review returned M088 to in-progress — AC5 fails. The roxygen "When the audio output fails" section and the `NEWS.md` entry state that a failed run leaves its output absent, which is false when the output path already held a file: reproduced on this branch, a pre-existing `videofile` survives the both-fail path byte-for-byte while the prose says both outputs are absent, and a pre-existing `audiofile` survives an `audio_codec = "nosuchcodec"` failure while the prose says it is absent (D046 removes what a run wrote, not what it found). AC1-AC4, AC6, AC7 pass with fresh evidence; consistency gate clean; the three review lenses and their dispositions are in the Review section. First defect return.

- 2026-08-29: T12 — the `.gitignore` line `tests/testthat/testthat-problems.rds`, added in the T6 commit and untraced until review F5, is recorded here: it is a local testthat artifact and its sibling `tests/testthat/_problems/` was already ignored. Kept, not reverted; the AC6-style "the diff touches only X" claims now account for it.
- 2026-08-29: T11 — the fall-through comment's timeout claim was measured and is false: a 7,200 s input under `options(tidymedia.timeout = 2)` with `audio_codec = "libopus"` times out on the audio half, then the video half runs on a fresh 2 s budget, succeeds, and the caller gets `tidymedia_timeout` carrying the video-written line (call took 2.5 s). Implement gate chose keeping the behavior and fixing the record over special-casing `tidymedia_timeout`; `?with_timeout` already documents the limit as per spawned program, not per call. Comment corrected in place, D066 appended superseding D065's last section.
- 2026-08-29: T10 — AC5's prose corrected in all three places. The old wording said a failed path leaves its output "absent"; D046 removes what a run *wrote*, so a file the run never wrote to is left as it was. Derived from executed calls: `audio_codec = "nosuchcodec"` over a pre-existing `audiofile` leaves it byte-for-byte with the error saying "was left as it was: FFmpeg never wrote to it", and the both-fail path leaves a pre-existing `videofile` likewise. (The stream-copy audio failure, which the tests use, *does* remove a pre-existing `audiofile` — FFmpeg opened it — so the honest rule is per run, not per path, and that is what the docs now state.)
- 2026-08-29: T9 — two tests added for the pre-existing-output cases the milestone's fresh-path tests could not reach; AC4's loop now names its two branches and asserts the audio half's own output is gone on the stream-copy failure.

- 2026-08-29: return fix verified — `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 8434 (was 8422 at the first review), `devtools::check()` 0 errors / 0 warnings / 0 notes (3m 30s). AC5 re-checked and now passing; status back to review.
- 2026-08-29: second review pass — all seven criteria re-executed with fresh evidence and all pass, AC5 included; consistency gate clean (one sizing advisory at 12 tasks). Three lenses returned six findings, five of them from [O]: F1 (the corrected AC5 sentence's "its own error says which" clause is false for the video half on the both-fail path) triaged fix-now and put to the maintainer at the gate; F2 and F3 to the deferred-findings candidate row; F4 and F5 rejected. Both [S] lenses reported no findings.

## Decisions

## Review

### First pass — returned

Reviewed 2026-08-29 against PR #92. Environment: ffmpeg 9.0.1, macOS arm64,
R CMD check on tidymedia 0.1.0.9000.

**Outcome: returned to `in-progress`. AC5 fails.** Every other criterion passes
with fresh evidence.

### Acceptance-criteria evidence

- **AC1 — pass.** `testthat::test_local(filter = "separate-av-multitrack")`:
  "a failed audio command still leaves the video file behind" and "the
  fall-open re-raise keeps its class vector and status" both assert the video
  path did not exist before the call (`sep_fresh_video()`'s `expect_false`),
  then that it exists, is non-empty, and probes as one video stream, while the
  call still aborts. 157 pass / 0 fail in that file.
- **AC2 — pass.** Same run: the enriched branch pins
  `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit",
  "rlang_error", "error", "condition")` and the `n <= 1L` fall-open branch pins
  `c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")`; both
  require `tm_status` to equal the status read out of the rendered message.
  These are the vectors the file's own header comment records from `master`.
- **AC3 — pass.** "when both commands fail the audio failure is what aborts"
  passes: the audio command's class vector is the one raised, and the
  fresh-path video output is absent afterwards. Independently reproduced
  outside the suite (`video_codec = "nosuchcodec"`, fresh path): file absent,
  audio condition raised.
- **AC4 — pass.** "the abort names the video file it wrote" matches `The video
  output was written to` plus `basename(videofile)` on both branches; the
  both-fail test's `expect_no_match` covers the silent half.
- **AC5 — FAIL.** The run order is stated correctly in all three places, but
  the disk-state claims are false on inputs the milestone's own tests never
  reach. Reproduced on this branch: with a pre-existing `videofile`, the AC3
  both-fail path leaves that file on disk byte-for-byte unchanged, while the
  roxygen section and `NEWS.md` both say "both outputs are absent"; with
  `audio_codec = "nosuchcodec"` and a pre-existing `audiofile`, that file
  likewise survives unchanged, while the roxygen section says `audiofile` is
  absent. D046 removes what a run *wrote*, not what it *found*, so the prose
  over-generalizes the two fresh-path calls it was derived from. AC3 itself is
  unaffected — it says "created or changed", which is exact.
- **AC6 — pass.** `git diff master...HEAD -U0 -- R/ffmpeg.R` yields four hunks
  (at 690, 887, 897, 991); `warn_failed_separation_batch()` begins at
  `R/ffmpeg.R:798` and `separate_audio_video_batch()` at `R/ffmpeg.R:5921`, and
  no hunk falls inside either. `test-separate-audio-video-batch.R` is absent
  from the diff; `test-separate-av-multitrack.R` is +146/-0, append-only. Both
  files run 157 pass / 0 fail.
- **AC7 — pass.** `devtools::test()`: FAIL 0 | WARN 12 | SKIP 5 | PASS 8422.
  `devtools::check()`: 0 errors / 0 warnings / 0 notes (3m 24s). Each of the
  six new execution tests calls `make_test_video()` or
  `make_multitrack_video()`, whose first statement is `skip_if_no_ffmpeg()`.

No Driving RR, so no projection-vs-outcome pairs.

### Consistency gate

`cairn_validate.py` exit 0, all checks passed, no advisories fired (the
`release window` advisory did not fire). No DESIGN.md principle changed, so
`cairn_impact.py` was not run. Toolchain slot: `devtools::document()` produces
no diff; `NAMESPACE`/`man/` regenerate clean; `README.Rmd` untouched;
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries the entry; no
new top-level files; `devtools::check()` clean.

### Independent review — three lenses, ranked findings and disposition

**[O] diff-bug (Opus).**

- F1 — *The stated rule for the timeout case is false.* The `run = TRUE`
  comment and D065 both claim the non-exit failure causes "are ones the video
  command fails on too, so nothing is written and no bullet is added."
  Reproduced independently: with `tidymedia.timeout = 2` on a 600 s input and
  `audio_codec = "libmp3lame"`, the audio half times out, the video half then
  runs on a *fresh* budget, succeeds, and the caller gets a `tidymedia_timeout`
  condition carrying the video-written line. The behavior is what the milestone
  wants; the recorded rationale is wrong, and a caller who set a wall-clock
  limit pays a second spawn past that limit. No test covers a timeout that
  yields the bullet (`tm_force_timeout()` injects at both wrappers, so both
  halves always time out there). **Disposition: fix now** — the code comment is
  current knowledge and is corrected in place; D065 is history and is
  superseded, not edited.
- F2 — *"Both outputs are absent" / "`audiofile` is absent" contradicts D046.*
  See AC5 above; independently reproduced. **Disposition: fix now** (this is
  the AC5 failure).
- F3 — *The video half's condition is discarded unconditionally,* including a
  video-half timeout, a missing binary, or an internal R error, with no
  `parent`, field, or trace. D065 records this as the deliberate choice with a
  stated falsifier, so it is a caveat rather than a defect; the reviewer notes
  it is cheap to pre-empt by stashing the discarded condition in a field.
  **Disposition: follow-up** — a candidate row, sweep-first, at the next
  hygiene pass.
- F4 — *The added bullet renders after `ffm_run()`'s closing "The failing
  command was:" line,* so it reads as pinned past the end. Cosmetic.
  **Disposition: reject** — style nitpick, out-of-scope taxonomy.
- F5 — *The `.gitignore` line for `tests/testthat/testthat-problems.rds` is out
  of this milestone's Scope,* untraced in the work log and D065. Confirmed: it
  rode along in the T6 commit. It is a correct ignore (the sibling
  `tests/testthat/_problems/` is already ignored). **Disposition: fix now** —
  keep the line, name it in the work log so the AC6-style "the diff touches
  only X" claims stay honest.
- F6 — *The non-rlang branch of `abort_after_video()` is untested and renders
  without an `i` marker,* and `c()` on a lazy or unnamed `body` would behave
  inconsistently. The code comment concedes the branch is a floor, not a live
  path. **Disposition: follow-up** — folded into F3's candidate row.
- F7 — *Test discrimination gaps:* the AC4 loop over `c(multi, single)` has no
  per-iteration label, no new test asserts anything about `audiofile` on any
  path, and `expect_match(msg, basename(video))` is width-dependent.
  **Disposition: fix now** for the `audiofile` half (it is the missing guard
  behind F2) and the loop label; the width point is **rejected** as a nitpick.
- F8 — *Interrupt handling,* flagged at low confidence and not reproduced: a
  SIGINT reaching only the child could surface as a non-zero exit, after which
  the branch spawns a second FFmpeg run rather than returning to the prompt.
  **Disposition: follow-up** — folded into F3's candidate row, promoted on a
  report of Ctrl-C failing to return.

What the [O] lens confirmed clean: class-vector and `tm_status` preservation
(the object is re-raised, not rebuilt); M44's brace trap correctly handled
(`cli_abort()` stores an already-formatted `body`, so `format_inline()`'s
result adds no second glue pass and `v{n}.mp4` survives literally); the success
path byte-identical to `master`; AC6's batch sites absent from the diff; Layer
boundaries respected — `abort_after_video()` assembles no commands.

**[S] blame-history (Sonnet).** No problems found. The removed "unchanged
behavior … M45 Out" comment is the planned resolution of an item M45 deferred
on purpose, not a silent reversal; `abort_after_video()`'s `stop(cnd)` matches
the pre-existing idiom in `run_separation_audio()`'s fall-open branch; D046 is
invoked unchanged; the `test-timeout-silence.R` additions are consistent with
the derived absorber sweep rather than a hand-maintained list. One non-finding
noted: `tests/testthat/_problems/` holds a stale local testthat artifact — it
is already gitignored and outside the diff. **Disposition: no action.**

**[S] prior-PR-comments (Sonnet).** No prior-review regression found; zero
findings. Primary surface: `## Review` sections in `cairn/milestones/archive/`
touching these files (M44, M45, M85, M86, M87, M70, M38, M58) — M44's brace
trap, M85/M86's class-vector and `tm_status` preservation rules, M87/D063's
shared-event naming rule, D046, and M70/D049's absorber-partition guard all
clear. Secondary surface probed once
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) and returned
empty, so the per-PR walk was skipped. **Disposition: no action.**

### Return

AC5 fails on evidence recorded above, so the milestone returns to
`in-progress` under the return floor. First defect return on this milestone —
no thrash trigger. The fix-now list for the next implement pass: F2 (the AC5
prose, corrected against executed calls that include a pre-existing output
file), F7's `audiofile` guard and loop label, F1 (the code comment corrected,
D065 superseded), and F5's work-log line. F3/F6/F8 become one candidate row at
the hygiene pass that follows a passing review; F4 and F7's width point are
rejected.

### Second pass — after the return fix

Reviewed 2026-08-29 against PR #92 at `b800124`. Environment: ffmpeg 9.0.1,
macOS arm64, R CMD check on tidymedia 0.1.0.9000. Branch in sync with
`origin/master` (0 commits either way). All seven criteria re-executed from
scratch; the first-pass evidence above is superseded by what follows.

#### Acceptance-criteria evidence

- **AC1 — pass.** `testthat::test_local(filter = "separate-av-multitrack")`:
  "a failed audio command still leaves the video file behind" (5 assertions)
  and "the fall-open re-raise keeps its class vector and status" (5) each call
  `sep_fresh_video()`, whose `expect_false(file.exists(path))` proves the path
  was absent before the call, then assert the call aborted, `videofile` exists,
  is non-empty, and probes as one video stream. File total: 116 pass / 0 fail /
  0 skip across 36 tests.
- **AC2 — pass.** Same run. The enriched branch pins
  `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit",
  "rlang_error", "error", "condition")`; the `n <= 1L` fall-open branch pins
  `c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")`. Both
  require `tm_status` to equal the status parsed out of the rendered message —
  a fact stated independently of the field under test. These are the vectors
  the test file's header records from `master`.
- **AC3 — pass.** "when both commands fail the audio failure is what aborts"
  (6 assertions) passes. Reproduced independently outside the suite on a fresh
  `videofile` with `video_codec = "nosuchcodec"`: the raised condition is the
  audio command's `tidymedia_multitrack_separation` vector, and `videofile` is
  absent afterwards.
- **AC4 — pass.** "the abort names the video file it wrote" (8 assertions)
  matches `The video output was written to` and `basename(videofile)` on both
  branches, each iteration labelled by branch; the both-fail test's
  `expect_no_match` covers the silent half. Reproduced independently: the
  both-fail message contains no reference to the video basename.
- **AC5 — pass.** All three surfaces state the run order — roxygen "The two
  commands run in order — audio first, video second"; `@return` "the audio
  command runs first and the video command runs second"; `NEWS.md` "runs the
  audio one first … The video command now runs either way". All three now state
  the disk rule correctly for both failure paths: a partial file the failed run
  wrote is removed, a file it never wrote to is left as it was. Verified by
  execution, not by reading: with a pre-existing `audiofile` and
  `audio_codec = "nosuchcodec"`, the file survives byte-identical (md5 match)
  and the error says "was left as it was"; with a pre-existing `videofile` on
  the both-fail path, that file likewise survives byte-identical; the
  stream-copy audio failure, where FFmpeg does open the output, reports "The
  incomplete … was removed." The first pass's failure — prose promising the
  output is "absent" — is gone from all three surfaces. One clause in this same
  sentence is separately false and is finding F1 below; it is not part of what
  AC5 requires the surfaces to state.
- **AC6 — pass.** `git diff master...HEAD -U0 -- R/ffmpeg.R` yields four hunks
  at 690, 887, 897 and 995; `warn_failed_separation_batch()` begins at
  `R/ffmpeg.R:798` and `separate_audio_video_batch()` at `R/ffmpeg.R:5929`, so
  no hunk falls inside either. `test-separate-audio-video-batch.R` is absent
  from the diff and runs 53 pass / 0 fail; `test-separate-av-multitrack.R` is
  +204/-0, append-only, and runs 116 pass / 0 fail.
- **AC7 — pass.** `devtools::test()`: FAIL 0 | WARN 12 | SKIP 5 | PASS 8434.
  `devtools::check()`: 0 errors / 0 warnings / 0 notes (3m 21.6s). Every new
  execution test opens with `skip_if_no_ffprobe()` and builds its input through
  `make_test_video()` or `make_multitrack_video()`, whose first statement is
  `skip_if_no_ffmpeg()`.

No Driving RR, so no projection-vs-outcome pairs.

#### Consistency gate

`cairn_validate.py` exit 0, all checks passed; one advisory — M088 at 12 tasks
against the >10 split tripwire, which is the four return-fix tasks and not a
gate failure. The `release window` advisory did not fire. No `DESIGN.md`
principle changed on this branch, so `cairn_impact.py` was not run. Toolchain
slot (`r-package`): `devtools::document()` leaves a clean tree;
`NAMESPACE`/`man/` regenerate with no diff; `README.Rmd` untouched;
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries the entry; no
new top-level files (0 check notes); `devtools::check()` clean.

#### Independent review — three lenses, ranked findings and disposition

**[O] diff-bug (Opus).**

- F1 — *The corrected AC5 sentence carries a new false clause: "its own error
  says which".* The roxygen section says the disk-state rule is "the same on
  either path, and its own error says which", and `NEWS.md` says it "is stated
  in its own error". True for the audio half. False for the video half on the
  both-fail path, where the video run's condition is discarded
  (`R/ffmpeg.R:1015-1018`), so nothing in the error the caller receives names
  `videofile` at all. Reproduced independently at review: on the both-fail path
  with a pre-existing `videofile`, the file survives byte-identical and the
  rendered message never mentions its basename — no "was removed", no "was left
  as it was". FFmpeg's own stderr for the failed video command is printed, but
  it reports the encoder failure, not the file's fate. **Disposition: fix now**
  — a one-clause correction on the branch, put to the maintainer at the gate.
- F2 — *The video-written bullet is inferred from exit status, never from the
  file.* `abort_after_video(held, videofile, wrote)` sets `wrote = TRUE` on a
  zero exit alone; nothing stats `videofile`. A video command exiting 0 without
  producing the named path would make the error assert the file was written
  when it is not there. Not demonstrated against the current `-map 0:v`
  pipeline. **Disposition: follow-up** — folded into the first pass's F3/F6/F8
  candidate row at the hygiene pass.
- F3 — *The doubled timeout budget is recorded in D066 but not in user-facing
  docs.* Reproduced: a 3,600 s input under `tidymedia.timeout = 2` with
  `audio_codec = "libopus"` times out on the audio half, the video half runs on
  a fresh 2 s budget and succeeds, and the caller gets `tidymedia_timeout`
  carrying the video-written bullet at 2.31 s wall clock. The code comment and
  D066 are true; neither `?separate_audio_video` nor `NEWS.md` says a limit can
  now cost a second spawn. **Disposition: follow-up** — same candidate row.
- F4 — *`options(warn = 2)` widens the fall-through:* `tryCatch(error = )`
  holds any error, so a promoted dropped-track warning from the audio half also
  reaches the video command. Untested, very low impact, and the "hold any audio
  failure" rule is the implement gate's recorded choice. **Disposition: reject**
  — an intentional change the plan called for.
- F5 — *The added bullet renders after `ffm_run()`'s closing "The failing
  command was:" line on the fall-open branch,* while landing before the
  `Caused by` chain on the enriched branch. Cosmetic. **Disposition: reject** —
  style nitpick, and the same point the first pass rejected as F4.
- F6 — *The out-of-scope `.gitignore` line is now logged rather than removed.*
  Not a finding; confirms T12's disposition landed. **Disposition: no action.**

What the [O] lens confirmed clean: the corrected disk-state rule is true on
both surfaces it was executed against; the condition is re-raised rather than
rebuilt, so class vector, `tm_status` and the `Caused by` chain survive
intact; M44's brace trap is handled and its `v{n}.mp4` test is a real guard;
AC6's batch sites are absent from the diff; the success and `run = FALSE` paths
are byte-identical to `master`; IP1–IP3 hold — `abort_after_video()` assembles
no command strings; `man/separate_audio_video.Rd` matches the roxygen exactly.

**[S] blame-history (Sonnet).** No findings. M45's sequencing comment was the
planned resolution of an item M45 deferred on purpose, and the ROADMAP row was
narrowed in the same diff; D066 supersedes exactly the one D065 clause a
measurement falsified, and the code matches D066's rule; `abort_after_video()`
always ends in `stop(cnd)`, so a held timeout is re-raised rather than absorbed
(D047–D049); `tm_timeout_absorbers()` is computed structurally, so
`separate_audio_video()` joining the partition is the drift detection working,
not a hand-edited list; D007's batch contract and D062's naming convention are
untouched. **Disposition: no action.**

**[S] prior-PR-comments (Sonnet).** No prior-review regression; zero findings.
Primary surface: `## Review` sections in `cairn/milestones/archive/` touching
these files (M44, M45, M85, M86, M87, M70, M38, M58) — M44's brace trap,
M85/M86's class-vector and `tm_status` rules, M87/D063's class-vector rule,
D046, and M70/D049's absorber guard all clear. Secondary surface probed once
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) and returned
empty, so the per-PR walk was skipped. **Disposition: no action.**

#### Return floor

No finding demonstrates an acceptance criterion failing. F1 is a false
user-facing claim this branch adds, in the same sentence AC5 governs but
outside what AC5 requires that sentence to state; whether it is a load-bearing
defect is the maintainer's judgment at the gate, and the proportionate
disposition is the one-clause fix recorded above.
