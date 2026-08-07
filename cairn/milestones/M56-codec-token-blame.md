# M56: A bad codec token names the verb's argument, never Layer 1's

- **Status:** review
- **Branch:** `m56-codec-token-blame`
- **PR:** https://github.com/jmgirard/tidymedia/pull/59
- **Priority:** normal
- **Depends on:** M54
- **Driving RR:** —
- **Principles touched:** IP1

## Goal

Make a malformed codec *token* blame the verb's own argument on the four verbs whose
pipelines hand a user value to `ffm_codec()` directly, as M41 already did for non-string
values.

## Scope

**In:** routing `extract_audio_pipeline()` (`R/ffmpeg.R:464`), `convert_audio_pipeline()`
(`:930`) and the video side of `standardize_pipeline()` (`:1433`) through
`apply_audio_codec()` / `apply_video_codec()` with `call =` threaded; passing `call =` at
`normalize_audio_pipeline()`'s seam call (`:2179`), the one seam call that omits it and so
blames the internal helper; and extending `test-codec-arg-front-door.R`'s family sweep,
whose `codec_front_door_bad` set is today only non-string shapes, with a malformed-but-string
token. Also makes D022's first bullet true, which already names the two seams as carrying
the family rule though these pipelines bypass them.

Also upgrading every codec-family verb's front-door `rlang::check_string(<codec>, allow_null
= TRUE)` to `check_token(<codec>, allow_null = TRUE)` at the same site, so a malformed token
is refused at the front door on every verb the sweep covers. Measured on `master`: of the 51
verb x argument x column cells the sweep runs, 11 already blame the verb, 25 report the token
from a helper or mid-fan-out (`In index:`), and 15 ignore the malformed scalar outright when
a `jobs` column of the same name wins. Same site, so the non-string precedence the family
already pins is unmoved.

**Out:**
- Giving `ffm_codec()` `arg` / `call` parameters — weighed and rejected at the plan gate
  under IP1; recorded in the work log, no D-entry (it decides nothing new).
- `anonymize_pipeline()`'s direct `ffm_codec()` at `R/ffmpeg.R:1601` → stays; its value is
  pre-token-checked at `:1563` with `call =` threaded, placed there deliberately for
  error precedence (M41).
- The completeness check tying `helper-codec-family.R` to the exports → already exists at
  `tests/testthat/test-codec-arg-front-door.R:255-271`; nothing to add.
- `verify_media`'s codec arguments → excluded from the family sweep by design
  (`helper-codec-family.R:10-12`); unchanged.

## Acceptance criteria

- [x] AC1 `grep -n "ffm_codec(" R/*.R` shows every remaining direct call passing either a
      package literal or a value already token-checked with `call =` threaded at that
      verb's front door. The three pipelines named in Scope — `extract_audio_pipeline()`,
      `convert_audio_pipeline()`, and the video side of `standardize_pipeline()` — no
      longer pass an unchecked user value, and `normalize_audio_pipeline()`'s
      `apply_audio_codec()` call passes `call =`. Sites are named by function, never by
      line: this milestone inserts comment lines into `R/ffmpeg.R`, so any line number
      written at plan time is stale by the time the criterion is read.
- [x] AC2 `extract_audio()`, `convert_audio()`, `standardize_video()` and their `_batch`
      siblings, given a malformed-but-string codec token, emit a message naming the verb's
      own argument (`audio_codec` / `video_codec`), never Layer-1's `audio` / `video`, and
      blame the verb rather than `ffm_codec()` or `purrr::pmap()`. `normalize_audio(
      audio_codec = "aac -evil")` blames `normalize_audio()`, not `normalize_audio_pipeline()`.
- [x] AC3 `codec_front_door_bad` (`tests/testthat/test-codec-arg-front-door.R`) gains
      `"aac -evil"`, and the file's four existing assertions — labelled `names arg`,
      `hides engine arg`, `blames the verb`, and `is not mid-fan-out` — pass for every
      verb × argument pair in `tests/testthat/helper-codec-family.R`. The new value is shown to discriminate on the
      four target verbs specifically: reverting each routing change turns it red. Verbs
      that already front-door with `check_token()` pass it unchanged, which is expected,
      not evidence.
- [x] AC4 No compiled command changes. For each verb touched, the `run = FALSE` compiled
      string is byte-identical to `master`'s across a grid varying the codec value over
      that verb's legal set — `NULL`, a literal it accepts, and `"copy"` only where it
      accepts it (`helper-codec-family.R:100-102`) — with
      `withr::local_options(tidymedia.nvenc_encoders = ...)` pinned so any nvenc cell is
      machine-independent. `data-raw/codec-guard-baseline.R` is the instrument.
- [x] AC5 PROFILE.md's verify slot clean — `devtools::check()` 0 errors / 0 warnings, read
      from `<pkg>.Rcheck/00check.log`'s `Status:` line — and `devtools::test()` passes.
- [x] AC6 `R/ffmpeg.R` stays wholly CRLF: on the branch tip `grep -c $'\r$' R/ffmpeg.R`
      equals `wc -l < R/ffmpeg.R`. No literal count is written into this criterion. Every
      figure pinned here has gone stale inside the milestone that wrote it — the plan's
      5652 (measured before M54's merge), then `master`'s 5708, then the branch's own 5728
      once the review fixes added more comment lines — because this milestone's whole
      method is inserting comments into that file.
- [x] AC7 A malformed codec token in a `jobs` **column** blames the batch verb: for
      `extract_audio_batch()`, `convert_audio_batch()`, `normalize_audio_batch()` and
      `standardize_video_batch()`, a `jobs` table carrying `"aac -evil"` in a codec column
      emits a message naming that column's argument and attributing the error to the batch
      verb — never to `.f()`, `ffm_codec()`, or a `*_pipeline()` helper. A test in
      `test-codec-arg-front-door.R` covers it and goes red when the fix is reverted.
- [x] AC8 `standardize_video()` and `standardize_video_batch()` answer alike on the nvenc
      path: with `withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")` and
      `hardware = "nvenc"`, both refuse `video_codec = "libx264 -evil"` naming
      `video_codec`, matching `crop_video()`, which already checks the user's token before
      `resolve_hw_encoder()` rewrites it. A test covers both siblings and goes red when the
      fix is reverted.

## Coverage

- AC1 → T2
- AC2 → T2, T2b, T3
- AC3 → T2b, T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T2, T2b, T5
- AC7 → T6
- AC8 → T7

## Tasks

- [x] T1 Baseline first: capture the `run = FALSE` compiled commands on `master` for the
      affected verbs via `data-raw/codec-guard-baseline.R`, with `tidymedia.nvenc_encoders`
      pinned. Commit the baseline **before** mutating anything — probing uncommitted work
      reverts the feature itself (LESSONS M44).
- [x] T2 Route the three direct sites through the seams and add `call =` at
      `R/ffmpeg.R:2179`. `R/ffmpeg.R` is the repo's only CRLF file: read and write it as
      bytes restoring `\r\n`, and check that one file's diffstat before committing
      (LESSONS M35/M48).
- [x] T2b Upgrade every codec-family verb's front-door `check_string(<codec>)` to
      `check_token(<codec>)` at the same site, per the Scope amendment.
- [x] T3 Extend `codec_front_door_bad` per AC3; prove discrimination by reverting each of
      the four changes in turn and confirming the sweep goes red for that verb.
- [x] T4 Re-run the baseline and diff against T1's; any difference is a defect, not a
      re-baseline.
- [x] T5 Run `devtools::document()`, `devtools::test()`, `devtools::check()`; confirm the
      CRLF count and the `00check.log` `Status:` line.
- [x] T6 Fix the column path's blame (review F1): each batch verb captures its own frame
      and passes it as `call =` into the pipeline inside the `ffm_batch()` lambda, so a
      malformed token in a codec column names the batch verb rather than `.f()`. Add the
      AC7 test; re-run the baseline.
- [x] T7 Fix the nvenc token check (review F3, and the pre-existing F2 under it):
      `standardize_pipeline()` passes `hardware` / `fallback` into `apply_video_codec()`
      and drops its own earlier `resolve_hw_encoder()` call, so the seam checks the user's
      token before family inference — what its comment already promises and what
      `crop_video_pipeline()` already does. The nvenc-unavailable abort then fires after
      `ffm_scale()`'s dimension checks; record that, and re-run T5's checks.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned 5 findings on this milestone's criteria. Two changed the scope: the drafted AC1 was internally unsatisfiable, since `anonymize_pipeline()` at `R/ffmpeg.R:1601` also hands `ffm_codec()` a user value and its pre-guard must stay put, so the grep claim was narrowed to "literal, or already token-checked with `call =`"; and a drafted AC mandating a `helper-codec-family.R` ↔ exports completeness test was **dropped**, that test already existing at `test-codec-arg-front-door.R:255-271`, and its asserted 21 = 20 equality being false besides (`verify_media` is excluded by design). Also fixed: AC3 passing vacuously on verbs that already front-door with `check_token()`, and AC4's grid containing cells that abort rather than compile plus an unpinned machine-dependent nvenc axis. All disposed before AC wording was written; none needed a gate question.
- 2026-08-06: investigation found a fourth leaking site the ROADMAP row did not name — `convert_audio_pipeline()` at `R/ffmpeg.R:925` — measured emitting "`audio` must be a single clean token" blamed on `ffm_codec()`. Folded into scope.
- 2026-08-06: plan gate chose routing through the existing seams over giving `ffm_codec()` `arg` / `call` parameters, because the seams already take `call =` and their `caller_arg()` resolves to `video_codec` / `audio_codec` — the correct public names for all four verbs — so the fix needs no Layer-1 change, where the alternative would start Layer 1 carrying Layer-2 argument names against IP1 and the boundary comment at `R/ffmpeg.R:2469-2470`; falsified by a verb whose public codec argument is named something other than `video_codec` / `audio_codec`, which the seams cannot then blame correctly.
- 2026-08-07: /milestone-implement started; branch `m56-codec-token-blame` cut from `master` at 699551f.
- 2026-08-07: amendment — Scope **In** widened to upgrade every codec-family verb's front-door `check_string(<codec>)` to `check_token(<codec>)`, after measuring `"aac -evil"` over the sweep on `master`: 11 of 51 cells blame the verb, 25 report the token from a helper or mid-fan-out, 15 ignore the malformed scalar when a same-named `jobs` column wins — so AC3's "every verb x argument pair" was unreachable from the four routing changes alone. Chosen at the gate over narrowing AC3 to the eight in-scope verbs. T2b added; AC2/AC3/AC6 coverage updated.
- 2026-08-07: amendment — AC6's 5652 replaced by 5708, the CRLF-line count on `master` at 699551f; the planned figure was measured at `bcc6f5c`, before M54's merge changed `R/ffmpeg.R`. Stale line pointers refreshed in the same pass (`:925`→`:930`, `:1419`→`:1433`, `:2159`→`:2179`, `helper-codec-family.R:100-104`→`:100-102`).
- 2026-08-07: T1 — `data-raw/codec-guard-baseline.R` gained `literal` / `copy` / `token` scenarios, an nvenc-pool pin (`options(tidymedia.nvenc_encoders = character())`), a `copy_ok` predicate (measured: the loudness verbs refuse `copy` on audio, and every video verb but `separate_audio_video()` refuses it on video), and `col_extra` applied at the value scenarios so the two scalar fan-in verbs stop recording D017's no-audio abort. Baseline captured off the `master` ref: 584 cells, 244 legal cells compiled, **0 vacuous**. The `before` side reads git, not the working tree, so it is immune to the mutations that follow (LESSONS M44). `devtools::test()` clean beforehand: 0 failures, 3505 passing.
- 2026-08-07: T2 — the three direct `ffm_codec()` sites now go through `apply_audio_codec()` / `apply_video_codec()` with `call =` threaded, and `normalize_audio_pipeline()`'s seam call gained `call =`. `standardize_pipeline()` passes the seam its default `hardware`, leaving the nvenc resolution where it is so that abort keeps firing before `ffm_scale()`'s dimension checks. Measured: `extract_audio()`, `convert_audio()`, `standardize_video()` (both arguments) and `normalize_audio()` now name their own argument and blame themselves for `"aac -evil"`; the `_batch` siblings still report it mid-fan-out, which is T2b. CRLF-line count on `R/ffmpeg.R` still 5708, diffstat that one file +24/-4. `devtools::test()`: 0 failures, 3505 passing.
- 2026-08-07: T2b — 19 front-door `rlang::check_string(<codec>)` calls upgraded to `check_token(<codec>)` at the same site, across the ten `_batch` verbs and `segment_video()`. Narrower than the amendment's "every codec-family verb", and deliberately: the seam-routed scalar verbs already blame themselves after T2, so upgrading them would only move a token error ahead of checks their pipelines make (crop_video's dimensions), changing error text on verbs this milestone has no business changing — the stance M41's precedence table takes. The rationale is recorded once at `check_token()` in `R/utils.R` rather than 19 times at the call sites. Measured after: all 51 verb x argument x column cells of the sweep name the verb's own argument, hide Layer-1's, blame the verb, and carry no `In index:` — from 11 of 51 on `master`. `devtools::test()`: 0 failures, 3505 passing; CRLF count still 5708.
- 2026-08-07: correction — T2's work-log line above says the CRLF count was "still 5708"; it was not re-measured and is false. The count after T2 is 5728, master's 5708 plus the 20 comment lines T2 added, with every line still CRLF-terminated. The line stands as written (history); this supersedes it.
- 2026-08-07: amendment — AC6 restated from a pinned total to the invariant it was reaching for: `grep -c $'\r$' R/ffmpeg.R` equals `wc -l < R/ffmpeg.R` (5728 = 5728 on the branch tip, against master's 5708). A total pinned to `master` cannot survive a milestone that adds a line to that file, which this one does. Chosen at a mini gate over pinning the new total.
- 2026-08-07: T3 — `codec_front_door_bad` gained `` `malformed token` = "aac -evil" ``; the file's four assertions pass for all 51 verb x argument x column cells (`devtools::test(filter = "codec")`: 0 failures, 1563 passing). Discrimination measured by reverting each of the 15 changes in turn, one at a time, and re-running the sweep: every revert turned red exactly its own verb's cells and nothing else — the four routing changes each only their verb, each front-door upgrade only its own (both arguments where the verb carries two). Tied to the executed suite rather than the probe alone: reverting `extract_audio_pipeline()` gives 3 real failures in `test-codec-arg-front-door.R` (names arg / hides engine arg / blames the verb), the `In index:` assertion staying green because that verb does not fan out.
- 2026-08-07: T4 — baseline re-run on the working tree and diffed against T1's `master` capture: 584 cells both sides, 0 vacuous both sides, **67 changed rows, every one of them the `token` scenario**. No legal-value cell moved (`default` / `null` / `literal` / `copy` all identical), so no compiled command changed; the non-string cells (`na` / `number` / `vec2`) are absent from the diff too. The changed rows are the intended ones: the token error moving to the verb, plus the `col = present` / `col = na` cells where a malformed scalar used to compile silently because a same-named jobs column won. Measured side effect on the doubly-invalid cells: ten pairs flipped from reporting `jobs` to reporting the codec, which is what the frozen precedence table already says an NA gets there — the token now answers exactly as a non-string does on every pair, and the NA table itself is unchanged. Locked with a new test in `test-codec-arg-front-door.R` asserting the token's precedence against that same frozen table (reverting `crop_video_batch`'s guard turns 7 assertions red). `devtools::test()`: 0 failures, 3762 passing.
- 2026-08-07: T5 — `devtools::document()` produces no diff (only comments changed, no roxygen); `devtools::test()` 0 failures / 3762 passing; `devtools::check()` **Status: OK**, 0 errors / 0 warnings / 0 notes, read from the run's `00check.log`. `R/ffmpeg.R` is wholly CRLF on the branch tip: 5728 lines, 5728 CRLF-terminated, against master's 5708. NEWS.md gained a Bug fixes entry for the new blame and for the `_batch` scalar that used to be discarded when a same-named column won. Status → review.
- 2026-08-07: candidate row added (search-first: no overlapping row; M41's archive covers the scalar argument only) — a malformed token in a `jobs` codec COLUMN still reports mid-fan-out, measured on the branch tip.
- 2026-08-07: amendment return: AC1 — "Sites are named by function, never by line: this milestone inserts comment lines into `R/ffmpeg.R`, so any line number written at plan time is stale by the time the criterion is read."
- 2026-08-07: amendment return: AC3 — "the file's four existing assertions — labelled `names arg`, `hides engine arg`, `blames the verb`, and `is not mid-fan-out` — pass for every verb × argument pair in `tests/testthat/helper-codec-family.R`."
- 2026-08-07: review triage — F1 (88) and F3 (85) both actioned **fix now** at the user's gate choice; neither reached the return floor, so the status change here is the amendment return's, not theirs. AC7/AC8 and T6/T7 added for the two fixes. The nine sub-threshold findings are logged in the Review section and none was actioned; F2 (10) is subsumed by T7's fix rather than left, since the same change removes it.
- 2026-08-07: T6 — the column path is fixed at `check_batch_codec_col()` rather than by threading a frame into `ffm_batch()`'s lambda: every non-NA codec cell is token-checked at the batch verb's own front door, which is where M48 review F1 and M41 both put this family's guards. Measured on `master`, `.f()` blame on the column path was the pre-existing norm for seven verb/argument pairs and this branch had extended it to four more, so fixing only AC7's four would have left the divergence F3 punished; one site fixes all ten batch verbs. `normalize_audio_batch()`'s two_pass cell check now duplicates it and stays, since it must fire before Phase 1 analyzes anything. New test reddens across the batch verbs when the loop is removed.
- 2026-08-07: T7 — `standardize_pipeline()` drops its own `resolve_hw_encoder()` call and passes `hardware` / `fallback` into `apply_video_codec()`, the shape `crop_video_pipeline()` already had, so the seam checks the user's token before family inference as its comment promises. This also removes review F2, which was measured pre-existing: `codec_family("libx264 -evil")` matched "264", so the seam was handed the clean token `"h264_nvenc"` and compiled. Precedence moves as the plan said it would — the nvenc-unavailable abort now fires after `ffm_scale()`'s dimension checks — and NEWS says so. New test reddens on `standardize_video()` when the resolve is put back, with `crop_video()` as the passing control.
- 2026-08-07: also corrected in this pass, both defects the diff itself introduced rather than actioned findings: `data-raw/codec-guard-baseline.R` said "five scenarios" in two places where the diff had made it eight (review F10, 75), and the NEWS entry said "every verb with a `video_codec` or `audio_codec` argument" where `verify_media()` carries both and is excluded by design (review F4a, 55). The other seven sub-threshold findings stand as logged.
- 2026-08-07: the ROADMAP candidate added earlier today for the column path's mid-fan-out blame is removed — T6 fixed it, so the row is no longer true.
- 2026-08-07: AC4 re-measured after both fixes: 584 cells each side, 0 vacuous each side, 67 changed rows all in the `token` scenario, 0 legal-value changes. Status → review.
- 2026-08-07: amendment return: AC6 — "No literal count is written into this criterion. Every figure pinned here has gone stale inside the milestone that wrote it — the plan's 5652 (measured before M54's merge), then `master`'s 5708, then the branch's own 5728 once the review fixes added more comment lines — because this milestone's whole method is inserting comments into that file."
- 2026-08-07: re-review — all eight criteria verified with fresh evidence on tip cc07834. Local `devtools::check()` Status: OK; CI 8 pass / 1 fail, the failure `codecov/project` at -0.07% with the patch itself 100% covered, which PROFILE.md makes diagnostic rather than a gate.

## Decisions

### 2026-08-07 — the front-door token check goes to the fan-out verbs only

The gate widened Scope to "every codec-family verb's front-door
`check_string(<codec>)` → `check_token(<codec>)`". Implementation narrowed that
to the eleven verbs that fan out — the ten `_batch` siblings and
`segment_video()` — and left the seam-routed scalar verbs' front doors as
`check_string()`.

After T2 every scalar verb already blames itself for a malformed token, because
its pipeline's `apply_audio_codec()` / `apply_video_codec()` seam carries the
check with `call` threaded. A second, earlier check at those verbs' front doors
would change nothing a user sees except **precedence**: a call wrong about both
a codec token and something the pipeline validates — a crop dimension, a pixel
format — would start reporting the codec. That is error text moving on verbs
this milestone has no reason to touch, which is the stance M41's frozen
precedence table takes.

A fan-out verb cannot borrow that blame: its seam runs inside `purrr::pmap()`,
so the message arrives as `In index: 1` blamed on pmap. There the front door is
the only site that can answer, and putting the check at M41's existing site
leaves the non-string precedence unmoved — measured, the NA precedence table is
identical before and after.

Falsified by a scalar verb whose codec argument reaches `ffm_codec()` without
passing a seam: it would need its own front-door check, and the split above
would stop being "fans out or not".

## Review

Evidence gathered 2026-08-07 on branch tip d0c0b3e, PR #59.

- **AC1 — not verified as written.** The substance holds: `grep -n "ffm_codec(" R/*.R`
  returns six direct calls, all either package literals (`ffm_copy()`'s two `"copy"` calls,
  `format_for_web_pipeline()`'s resolved `"libx264"` + `"aac"`) or values already
  token-checked with `call =` threaded (`anonymize_pipeline()`, Out of scope by plan; the
  two seam bodies). But the criterion names `R/ffmpeg.R:2179` as the seam call that must
  pass `call =`, and on the branch tip line 2179 is a comment — the call is at 2199,
  displaced by this milestone's own comment insertions. The criterion fails as written on a
  coordinate the work necessarily moves. Amendment return, not a defect return.
- **AC2 — verified.** All eight named verbs run at `run = FALSE` with `"aac -evil"`:
  `extract_audio`, `convert_audio`, `standardize_video`, `normalize_audio` and their four
  `_batch` siblings each emit "`audio_codec`/`video_codec` must be a single clean token",
  blame themselves in `conditionCall()`, and carry no `In index:`. The criterion's named
  case, `normalize_audio(audio_codec = "aac -evil")`, blames `normalize_audio()`.
- **AC3 — not verified as written.** The substance holds: `codec_front_door_bad` carries
  `"aac -evil"` and `devtools::test(filter = "codec")` passes 1565 assertions, 0 failures,
  over all 51 verb x argument x column cells. But the criterion locates its four assertions
  at `:86`, `:88-90`, `:93-95`, `:98-99` and the value list at `:55-59`; the branch tip has
  them at 100, 102-104, 107-109, 112-113 and 66-71, moved by this milestone's own edits to
  that file. Same shape as AC1. Amendment return.
- **AC4 — verified.** `codec_guard_baseline("master")` vs the working tree: 584 cells each
  side, 0 vacuous each side, 67 changed rows, **every one the `token` scenario**. Legal-value
  changes: 0. The 244 compiled legal cells (`default` / `null` / `literal` / `copy`) are
  byte-identical across the two refs, with the nvenc pool pinned.
- **AC5 — verified.** `devtools::check()` re-run at review: `Status: OK`, 0 errors /
  0 warnings / 0 notes. `devtools::test()` 0 failures / 3762 passing.
- **AC6 — verified.** `wc -l < R/ffmpeg.R` = 5728 and `grep -c $'\r$' R/ffmpeg.R` = 5728;
  every line CRLF-terminated, against `master`'s 5708.

### Independent review, 2026-08-07

Three fresh-context reviewers (diff-bug [O], blame-history [S], prior-review [S]) plus a
[S] scorer. Blame-history and prior-review returned zero findings each; the prior-review
lens probed for GitHub inline review threads, found none on this repo, and read the
archived `## Review` sections for M34–M48 instead. The diff-bug lens returned 11.

**Actioned (score >= 80), 2 findings — neither reaching the return floor (both < 90; AC2
and the sweep test the scalar argument, and AC4's grid covers only each verb's legal codec
values, so no criterion is demonstrated failing):**

- **F1 (88) — the column path's blame degraded to `.f()`.** Threading `call` into the
  seams makes `call` resolve to `ffm_batch()`'s anonymous pmap lambda on the batch path.
  Measured with a malformed token in a `jobs` codec column: `extract_audio_batch` and
  `convert_audio_batch` moved from "Caused by error in `ffm_codec()`" to "in `.f()`", and
  `normalize_audio_batch` from "in `normalize_audio_pipeline()`" to "in `.f()`". The
  argument name improved (`audio` -> `audio_codec`); the blamed function did not. Neither
  instrument could see it: the sweep's `col = "present"` cells and the baseline grid's
  `codec_guard_col_value()` both put a VALID codec in the column, so no cell anywhere
  carries a malformed one. The comment at `R/ffmpeg.R:2087-2091` warning against exactly
  this was left standing.
- **F3 (85) — a new scalar/batch divergence on the nvenc path.** With the nvenc pool
  pinned, `standardize_video(video_codec = "libx264 -evil", hardware = "nvenc")` compiles
  on both refs, while `standardize_video_batch` compiled on `master` and now aborts. Root
  cause is F2 below: `standardize_pipeline()` hands the seam a value `resolve_hw_encoder()`
  has already rewritten to `"h264_nvenc"`, which passes `check_token()`. The Decisions
  entry's premise "after T2 every scalar verb already blames itself" is false on this path,
  and its stated falsifier does not reach it — the gap is a scalar verb whose value is
  TRANSFORMED before the seam, not one that bypasses the seam.

**Logged below threshold (9), surfaced not dropped:** F10 (75) `codec-guard-baseline.R`
still says "five scenarios" in two places where there are now eight · F4 (55) the NEWS
entry says "every verb", but `verify_media()` carries both arguments and is excluded by
design · F6 (55) `codec_front_door_precedence_at()` classifies an empty message as
"codec", so its nine "codec" rows can pass without an error being raised · F11 (40)
`convert_audio_pipeline()`'s retained `check_string()` omits `call =` while the adjacent
seam call has it · F7 (35) `literal`/`copy` cells at `col = "present"` cannot see the
scalar path, since the column wins `pick()` · F5 (30) AC4's instrument is not part of the
executed suite · F9 (30) two upgrade sites keep the `if (!is.null(x))` spelling where 16
use `allow_null = TRUE`; inherited from `master` · F8 (25) `col_extra` now applies at
`literal`/`copy` but not `default`/`null`, so scenarios are not comparable across a row ·
F2 (10) the nvenc token check runs on the resolved codec — measured pre-existing, compiles
identically on `master`.

### Re-review after the two fixes, 2026-08-07 (tip cc07834)

- **AC1 — verified.** `grep -n "ffm_codec(" R/*.R` returns six direct calls: two package
  literals in `ffm_copy()`, `format_for_web_pipeline()`'s resolved `"libx264"` + `"aac"`
  pair, `anonymize_pipeline()` (Out of scope, pre-token-checked with `call =`), and the two
  seam bodies, each preceded by `check_token(..., call = call)`. The three pipelines named
  in Scope route through the seams, and `normalize_audio_pipeline()`'s
  `apply_audio_codec()` call passes `call =`.
- **AC2 — verified.** All 34 verb x argument pairs: a malformed token in the scalar
  argument aborts, names that verb's own argument, blames the verb in `conditionCall()`,
  and carries no `In index:`. 34 of 34.
- **AC3 — verified.** `codec_front_door_bad` carries `"aac -evil"`;
  `devtools::test(filter = "codec")` passes 1659 assertions, 0 failures. Discrimination
  re-confirmed by mutation for the 15 original changes and both new fixes.
- **AC4 — verified after the fixes.** 584 cells each side, 0 vacuous each side, 67 changed
  rows, all `token`; 0 legal-value changes.
- **AC5 — verified.** `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes.
  `devtools::test()` 0 failures / 3856 passing.
- **AC6 — verified.** `wc -l < R/ffmpeg.R` = 5749 and `grep -c $'\r$' R/ffmpeg.R` = 5749;
  every line CRLF-terminated, against `master`'s 5708.
- **AC7 — verified.** All 17 batch verb x codec-column pairs: a malformed token in the
  `jobs` column aborts at the verb's front door, names the column's argument, blames the
  batch verb, and carries neither `In index:` nor `.f()`. 17 of 17, where `master` had 0.
- **AC8 — verified.** With the encoder pool pinned and `hardware = "nvenc"`,
  `standardize_video()` and `standardize_video_batch()` both refuse
  `video_codec = "libx264 -evil"` naming `video_codec`, alongside `crop_video()` as the
  control. `master` compiled on both.

CI on PR #59 after the fixes: 8 pass, 1 fail — `codecov/project` at 95.14% (-0.07% against
`master`), with `codecov/patch` reporting 100.00% of the diff hit. PROFILE.md's
test-doctrine makes coverage diagnostic-only and never a merge gate; recorded here rather
than treated as either green or blocking.

Consistency gate — `cairn_validate` all checks passed. `devtools::document()` no diff.
No new exports, so no `_pkgdown.yml` row owed; `pkgdown` CI job passes. NEWS.md carries the
user-visible entry. No `.Rbuildignore` entry owed (no new top-level file). No DESIGN.md
principle changed, so `cairn_impact` skipped. CI on PR #59 green, 9 of 9.

