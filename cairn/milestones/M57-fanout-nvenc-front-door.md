# M57: A missing nvenc encoder is refused at the front door, on every verb that fans out

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M54, M56
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m57-fanout-nvenc-front-door · https://github.com/jmgirard/tidymedia/pull/60

## Goal

Make an unavailable nvenc encoder abort at the fan-out verb the user called, not
inside `purrr::pmap()`.

## Scope

**In:** the nine verbs that take `hardware = c("none", "nvenc")` *and* fan out
through `ffm_batch()` — `segment_video()` plus `anonymize_video_batch()`,
`segment_video_batch()`, `standardize_video_batch()`, `crop_video_batch()`,
`format_for_web_batch()`, `separate_audio_video_batch()`,
`compare_videos_batch()`, `picture_in_picture_batch()`. Each gains a front-door
availability check, placed last in its front-door block (M41), reaching the
abort through one shared helper that `resolve_hw_encoder()` also calls. A new
`cairn/DECISIONS.md` entry licenses the construction-time abort gate, which
D024 places outside its diagnostic licence and requires be recorded before it
is built.

**Out:** the other seven `hardware`-bearing verbs — they call their pipelines
directly, so `call =` already lands on the verb (M47 F8) and a second guard
would only move error text. Every *other* pipeline-level validation on these
nine verbs → AC6 enumerates them into ROADMAP candidate rows. Memoizing
`ffmpeg_encoders()` → stays the standing candidate row; its open question is
cache lifetime, not where the guard fires. Hoisting resolution to the front
door → rejected at the plan gate (work log).

## Acceptance criteria

- [ ] AC1: On each of `segment_video()`, `anonymize_video_batch()`,
      `segment_video_batch()`, `standardize_video_batch()`, `crop_video_batch()`,
      `format_for_web_batch()`, `separate_audio_video_batch()`,
      `compare_videos_batch()` and `picture_in_picture_batch()`, a call with
      `hardware = "nvenc"`, `fallback = FALSE` and a `tidymedia.nvenc_encoders`
      seam lacking the required encoder aborts before `ffm_batch()` is reached,
      with a message naming the unavailable encoder and `conditionCall()` naming
      that verb. Evidence: a sweep test, one cell per verb, each matching the
      message *before* reading `conditionCall()` (M54); nine of nine green, each
      cell recorded naming `purrr::pmap` on master, plus one cell at
      `parallel = TRUE`, whose master reading is furrr's internal `...furrr_fn`
      closure rather than the `furrr::future_pmap` this criterion first
      predicted (D033).
- [x] AC2: The unavailable-nvenc abort text is emitted from exactly one
      function, and `resolve_hw_encoder()` reaches it by calling that function
      rather than by carrying its own copy. Evidence: reading the two functions,
      plus a test asserting the front-door and pipeline messages are
      string-identical for one `(video_codec, hardware, fallback)` triple.
- [x] AC3: On a `_batch` verb carrying a `video_codec` column, the guard checks
      every distinct family the column spells, with an `NA` cell and an absent
      column both spelling the h264 family that `resolve_hw_encoder()`'s `NULL`
      sentinel resolves to (`R/ffmpeg.R:2475-2480`, D022); a verb with no such
      column checks the argument alone. Evidence: a two-row table (H.264 + AV1)
      under a seam listing only `h264_nvenc` aborts naming the AV1 encoder, and
      compiles under a seam listing both; an all-`NA` column behaves as h264.
- [x] AC4: `fallback = TRUE` reaches no front-door guard at all — including the
      column sweep, whose `codec_family()` call aborts on an unmappable codec
      regardless of `fallback` (`R/ffmpeg.R:2440-2452`). Evidence: a test on one
      fan-out verb asserting no abort and the *same count* of fallback
      `cli_inform()` messages as before the change; the pre-existing fallback
      tests green.
- [x] AC5: No existing test is re-baselined and no compiled command the suite
      exercises changes, with one named exception: `test-nvenc.R`'s M54 blame
      test pins the misblame this milestone removes and its own comment
      anticipates going red here. Its three fan-out assertions flip from
      `purrr::pmap` to the verb named; its scalar control is untouched.
      Evidence: `git diff tests/` shows additions only outside that one test;
      `devtools::test()` green; `devtools::check()` reports `Status: OK`.
- [x] AC6: Each `cli::cli_abort()` site that `grep -n "cli_abort(" R/` returns
      inside the nine verbs' `*_pipeline()` functions carries a recorded
      disposition — guarded here, ROADMAP candidate row, or left with a stated
      reason. Evidence: the grep output and its dispositions in the work log.
- [x] AC7: `devtools::document()` produces no diff; `NEWS.md` carries the
      user-visible change; `R/ffmpeg.R`'s CRLF line endings survive every commit
      (M35/M48).

## Coverage

- AC1 → T2, T3, T4
- AC2 → T1
- AC3 → T4, T5
- AC4 → T6
- AC5 → T8
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1: Write the D-entry licensing a construction-time abort gate (D024's
      third excluded shape) — before any code, as D024 requires. Then extract
      the abort from `resolve_hw_encoder()` (`R/ffmpeg.R:2498-2506`) into a
      shared `check_nvenc_available()`; `resolve_hw_encoder()` calls it. No
      behavior change; suite green.
- [x] T2: Write the nine-cell sweep test (message first, then
      `conditionCall()`), plus the `parallel = TRUE` cell. Record each cell's
      master reading. Red on master.
- [x] T3: Add the front-door guard to `segment_video()`, last in its front-door
      block beside the M48 guards (`R/ffmpeg.R:2650-2662`).
- [x] T4: Add it to the eight `_batch` verbs, reading the `hardware` argument
      and any `video_codec` column; place it after `check_batch_codec_col()` so
      a malformed codec still reports first (M41 precedence).
- [x] T5: Column-spanning tests (H.264 + AV1; all-`NA`; no column), verified
      against each verb's real column names — `picture_in_picture_batch()` takes
      `overlay`, `compare_videos_batch()` an `inputs` list-column (M54).
- [x] T6: `fallback = TRUE` test asserting no abort and the message count.
- [x] T7: Run AC6's grep, record the dispositions in the work log, add the
      ROADMAP candidate rows it produces.
- [x] T8: `@param hardware` wording on the nine verbs, `NEWS.md`,
      `devtools::document()`, `devtools::test()`, `devtools::check()`; check
      `grep -c $'\r' R/ffmpeg.R` against the default branch before every commit
      touching it (M35/M48).

## Work log

- 2026-08-07: created by /milestone-plan; promotes the M54 review F1/F4 candidate row.
- 2026-08-07: criteria audit ([O] reader) returned findings on all six drafted criteria — AC1 scoped itself to an absent section and ignored the `furrr::future_pmap` path; AC2 claimed a firing-condition invariance AC3 falsifies, over a grep matching two `cli_inform()` lines; AC3 was undefined for D022's `NA` cells; AC3/AC4 collided on `codec_family()` aborting regardless of `fallback`; AC4 asserted presence where count was meant; AC5's `.new`-file check was satisfied by the state it excluded; AC6 quantified over a call graph no grep computes. Six fixed at the gate, the hoist-vs-duplicate finding raised as a gate question.
- 2026-08-07: plan gate chose duplicating the check at the front door over hoisting resolution there, because hoisting re-forks the resolver seam for per-row `video_codec` columns and undoes M56's fix that made `standardize_pipeline()` hand `hardware` to the seam unresolved; falsified by a front-door guard and a pipeline guard observed firing on different inputs.
- 2026-08-07: plan gate chose nvenc availability alone over every pipeline-level validation on the nine verbs, because the wider cut trips the sizing tripwires; falsified by AC6's enumeration returning few enough sites to have been folded in.
- 2026-08-07: implement gate skipped — the plan gate settled hoist-vs-duplicate, scope, AC6 and the probe cache, and nothing left open was more than a helper signature.
- 2026-08-07 (T8): the availability note added to the nine fan-out verbs' `@param hardware` blocks; `devtools::document()` rewrote exactly those nine `.Rd` files and a second run produced no further change. The M54 NEWS paragraph that stated the fan-out limitation is rewritten, since M57 removes what it described. `devtools::test()` FAIL 0 | PASS 3918 with the same 4 warnings and 5 skips as at T1; `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes. `git diff master -- tests/` outside `test-nvenc.R` has 0 deleted lines, so AC5's exception is the only re-baseline. `R/ffmpeg.R` CRLF 5749 on master -> 5922 here, matching a numstat of 186 added / 13 deleted, net +173 (M35/M48).
- 2026-08-07 (T7): AC6 enumeration run. `grep -n "cli_abort(" R/` attributed to the enclosing function returns six sites inside the nine verbs' `*_pipeline()` functions, and each was measured on the branch rather than read off the source: `separate_stream_pipeline():592` (copy video codec against `hardware`), `segment_pipeline():2810` and `:2826` (the two `reencode = FALSE` contradictions), `compare_videos_pipeline():5251` (audio codec, no mapped audio) and `:5263` (the two-input `resize` limit), `picture_in_picture_pipeline():5396` (audio codec, no mapped audio). Disposition: all six blame `purrr::pmap` today and all six are out of M57's scope by the plan gate's nvenc-only cut, so they take one grouped ROADMAP candidate row. None is guarded here, and none is left without a row.
- 2026-08-07 (T7): the first `:5263` measurement recorded a `jobs` schema error, not the resize limit — `compare_videos_batch()` takes `output` as a column, never an argument. Re-measured against the verb's real column names before the disposition was written (M54).
- 2026-08-07 (T5/T6): column-spanning and fallback tests added. A two-row table spelling h264 and av1 under a seam holding only `h264_nvenc` aborts naming `av1_nvenc`, and compiles under a seam holding both; an `NA` cell and an absent column both read as h264; `format_for_web_batch()` checks h264. `fallback = TRUE` emits 2 fallback messages for a 2-row table on the branch and 2 on master, and an unmappable `prores` cell under `fallback = TRUE` still fails inside the fan-out rather than at the front door. Suite FAIL 0 | PASS 3918.
- 2026-08-07 (T5): the sweep helper used `utils::modifyList()` to apply per-test overrides, which merged a replacement `jobs` tibble column-wise into the template's (a tibble is a list) and would have deleted any `NULL`-valued override instead of setting it. Replaced with direct element assignment; two tests were erroring on it.
- 2026-08-07 (T4): eight `_batch` guards added immediately before each `ffm_batch()` call, which is where M41 puts a guard added for blame, rather than after `check_batch_codec_col()` as the plan said — on several verbs that anchor sits mid-block, so output derivation and duplicate-path checks would have started reporting after it. `separate_audio_video_batch()` takes its guard before the N->2N reshape, while `jobs` still carries the caller's `video_codec` column. New helper `batch_video_codecs()` yields the column's distinct cells, or the argument where the verb honours no column. `format_for_web_batch()` passes `"libx264"`: its recipe fixes the codec by identity. Sweep 46/46 green; full suite FAIL 0 | PASS 3902, the same 4 warnings and 5 skips as at T1. `R/ffmpeg.R` +90 lines, 0 deletions, CRLF 5805 -> 5895 (M35/M48).
- 2026-08-07 (T4): the M54 blame test flipped under AC5's amended exception — three fan-out assertions now name their verb, the scalar control untouched, and the `test_that()` title corrected, since it read "still blames the fan-out".
- 2026-08-07 (T3): `segment_video()` guarded; its sweep cell is green and the other eight plus the parallel cell stay red until T4. Two pipelines abort BEFORE reaching `resolve_hw_encoder()` — `segment_pipeline()` on a non-re-encoding cut naming an encoder, and the shared separation recipe on `video_codec = "copy"` — so each front door mirrors that precondition, with two tests asserting the pipeline's own message still reports there.
- 2026-08-07 (T3): amendment — AC5 forbade re-baselining any existing test, but `test-nvenc.R`'s M54 blame test asserts the misblame M57 removes and its own comment says it goes red when this lands. AC5 amended at a mini gate to name that one exception; the flip itself lands with T4.
- 2026-08-07 (T2): master readings recorded on a worktree at master — nine of nine fan-out verbs blame `purrr::pmap` with the nvenc-unavailable message, `separate_audio_video_batch` reporting "In index: 2" for a 1-row table because it reshapes N->2N (M45). Sweep red on the branch: nine blame cells plus the parallel cell; every message assertion already passes, which is what confirms these are the nvenc failure and not a schema error (M54).
- 2026-08-07 (T2): amendment — AC1 predicted a `furrr::future_pmap` master reading at `parallel = TRUE`; measured, it is furrr's internal `...furrr_fn` closure. Criterion amended at a mini gate to record the measurement and that the prediction was wrong.
- 2026-08-07 (T1): D035 written before any code, as D024 requires of a shape its third exclusion reserved. Abort extracted from `resolve_hw_encoder()` into `check_nvenc_available()`; the resolver now reaches it by calling it. `devtools::test()` FAIL 0 | PASS 3856, the same 4 warnings and 5 skips as before, all in test files this milestone does not touch. `R/ffmpeg.R` CRLF count 5749 -> 5791 for 42 net added lines, diffstat 55/13 (M35/M48).

- 2026-08-07 (review): returned to in-progress. AC1 fails on a legal call the sweep never ran: `segment_video_batch()` with a mixed `reencode = c(TRUE, FALSE)` column skips the guard entirely and still blames `purrr::pmap` (F4, scored 90). Two more actioned: `separate_audio_video_batch()`'s guard preempts `reject_duplicate_outputs()` and hides M26's within-row collision catch (F3, 85), and `check_nvenc_available()`'s `isTRUE(fallback)` swallows a malformed `fallback` that `resolve_hw_encoder()`'s `check_bool()` would have caught, machine-dependently (F1, 82). Six logged below threshold. Three lenses plus a scorer; blame-history and prior-review zero.

## Decisions

- 2026-08-07 (T1): the shared guard takes `video_codec` as either one value or a LIST of values, so one function serves the scalar resolver and a `_batch` verb whose `video_codec` column spells several families in one call. `NULL` and its column form `NA` (D022) both resolve to the h264 family, matching `resolve_hw_encoder()`'s sentinel branch — the two readings must agree, or the front door would refuse a call the pipeline compiles, which is D035's second condition.
- 2026-08-07 (T1): `check_nvenc_available()` returns early on `fallback = TRUE` rather than sweeping and then suppressing. Sweeping a column would reach `codec_family()`, which aborts on an unmappable codec regardless of `fallback` (`R/ffmpeg.R:2440-2452`), so a `fallback = TRUE` call that falls back happily today would start being refused.

## Review

Fresh evidence, gathered 2026-08-07 on `m57-fanout-nvenc-front-door` at PR #60.
Every figure below was re-measured at review, not carried from implement.

**AC1 — the abort names the fan-out verb.** `testthat::test_file()` per-test:
"an unavailable nvenc encoder blames the fan-out verb, not purrr::pmap()" runs
27 assertions, 0 failed — three per verb (error class, message, blamed
function) across all nine; "the abort names the verb at parallel = TRUE too"
3 assertions, 0 failed. Each cell matches the nvenc message before reading
`conditionCall()`, so a schema error cannot satisfy it (M54). Master readings
were recorded at T2 from a worktree at master: nine of nine `purrr::pmap`, and
`...furrr_fn` for the parallel cell. A completeness test pins the nine fan-out
verbs plus the seven direct ones to exactly the exported functions carrying a
`hardware` formal, so a tenth verb cannot join the family unnoticed.

**AC2 — one abort site.** `check_nvenc_available()` (`R/ffmpeg.R:2517-2555`)
holds the only `cli_abort()` for the unavailable-encoder condition;
`resolve_hw_encoder()` (`:2489-2515`) reaches it by calling that function.
Verified by reading both, and by two tests: the front-door and pipeline
messages are string-identical for `("libx264", "nvenc", FALSE)` (4 assertions),
and the deparsed body of `resolve_hw_encoder()` contains the call and no
`cli_abort` of its own (2 assertions). 0 failed.

**AC3 — every family the column spells.** Four tests, 11 assertions, 0 failed:
a two-row H.264+AV1 table under a seam holding only `h264_nvenc` aborts naming
`av1_nvenc` and blames `standardize_video_batch()`; the same table compiles
under a seam holding both; an `NA` cell and an absent column both read as h264
on `segment_video_batch()`; `format_for_web_batch()`, which honours no codec
column, checks h264 — the codec its recipe fixes.

**AC4 — fallback untouched.** Two tests, 5 assertions, 0 failed. A 2-row table
at `fallback = TRUE` with nothing available emits 2 fallback messages, the
count measured on a master worktree for the same table. An unmappable `prores`
cell at `fallback = TRUE` still fails inside the fan-out, not at the front
door — the early return is what keeps `codec_family()`, which aborts
regardless of `fallback`, from refusing a call master accepts.

**AC5 — no other re-baseline.** `git diff master..HEAD -- tests/` excluding
`test-nvenc.R` has 0 deleted lines; the sole exception is the named M54 blame
test. `devtools::test()` FAIL 0 | WARN 4 | SKIP 5 | PASS 3918 — the same 4
warnings and 5 skips as the pre-change baseline, all in files this milestone
does not touch. `devtools::check()` `Status: OK`, 0 errors / 0 warnings /
0 notes.

**AC6 — the remaining aborts enumerated.** The stated grep returns six
`cli_abort()` sites inside the nine verbs' `*_pipeline()` functions; each was
measured on the branch rather than read off the source, and all six blame
`purrr::pmap` today. Disposition recorded in the work log and carried into one
grouped ROADMAP candidate row. None guarded here, none left without a row.

**AC7 — docs and line endings.** `devtools::document()` rewrote exactly the
nine fan-out verbs' `.Rd` files; a re-run at review left 0 files changed.
`NEWS.md` carries the user-visible change, with no milestone or decision
identifiers in user-facing text (checked by grep over the added lines).
`R/ffmpeg.R` numstat 186 added / 13 deleted, net +173, against a CRLF count
rising 5749 -> 5922 — a real edit, not a full-file re-encode (M35/M48).

**Discrimination probe.** M56's lesson says a family sweep pins these guards
only if reverting one reddens exactly that verb's cells. Verified in a detached
worktree at HEAD: disabling `crop_video_batch()`'s guard alone failed exactly
one assertion in the whole file, and that assertion was `crop_video_batch`'s
blame cell. Every other verb stayed green, so the sweep is not passing for a
shared reason.

**Independent review — three lenses plus a scorer.** The [S] blame-history
lens traced every modified line through M31/M38/M41/M47/M48/M54/M56 and
returned zero findings. The [S] prior-review lens ran the inline-comment
existence probe, got an empty result, correctly skipped the thread walk, and
returned zero regressions. The [O] diff-bug lens returned eight findings; a
ninth came from blame-history as a wording nit. All nine went to a fresh [S]
scorer holding the diff and this milestone file, which reproduced every
measured claim on this machine.

**Actioned (>=80), and the milestone returns.**

- F4 (90) — `segment_video_batch`'s guard is skipped whenever ANY row has
  `reencode = FALSE`, so a legal mixed `reencode = c(TRUE, FALSE)` column with
  `hardware = "nvenc"` and no encoder still aborts `In index: 1 ... purrr::pmap`
  — the exact misblame M57 exists to remove. **This is AC1 failing inside its
  own domain**: AC1 promises the abort for "a call with `hardware = "nvenc"`,
  `fallback = FALSE` and a seam lacking the required encoder", and this is such
  a call. The `@param hardware` sentence this milestone added
  ("checked once at this verb's own front door, before any row runs") is
  unqualified and false for it. No test covers a mixed `reencode` column.
- F3 (85) — `separate_audio_video_batch`'s guard sits at `R/ffmpeg.R:5107`
  while `reject_duplicate_outputs(long)` runs below it at `:5162`, so the guard
  preempts the duplicate-output check, including M26's within-row
  `audiofile == videofile` catch. A row with `audiofile == videofile` reports
  the collision on master and nvenc-unavailable on the branch. Every other
  guarded `_batch` verb places its guard after its duplicate check, and this
  guard's own comment claims it is "last in the front-door block".
- F1 (82) — `check_nvenc_available()` gates on `isTRUE(fallback)` where
  `resolve_hw_encoder()` validates with `rlang::check_bool(fallback)`. Eight of
  the nine guarded verbs never `check_bool(fallback)` at their front door, so a
  malformed `fallback` (`NA`, `"yes"`, `c(TRUE, TRUE)`) now receives the
  nvenc-unavailable message instead of its own type error — and only on a
  machine lacking nvenc, so one wrong call is diagnosed two ways depending on
  the machine. That is D035's own stated falsifier.

**Logged below threshold (6).**

- F2 (78) — the `"copy"` exemption in `separate_audio_video_batch` filters copy
  CELLS but a mixed column still has copy ROWS the pipeline aborts on first, so
  the branch reports availability where master reported the copy conflict. Same
  verb and same shape as F3; it travels with F3's fix.
- F6 (72) — D035's second bullet says the precedence reassignment "is tested
  for, not assumed away". No test pins any reassignment; three were measured
  flipping (`compare_videos_batch`'s resize limit and its audio-codec case,
  `picture_in_picture_batch`'s audio-codec case). The claim is unsupported as
  written.
- F7 (55) — the front door adds an unmemoized `ffmpeg -encoders` subprocess per
  distinct family, so a 1-row nvenc batch shells out twice instead of once,
  undocumented. The memoization candidate row already exists.
- F8 (35) — the deparsed-body test pins source text rather than behavior; the
  scorer rejected it as evidence AC2 explicitly called for.
- F9 (20) — "checked once" undersells a mechanism that re-checks per row; no
  observable behavior is misstated.
- F5 (20) — `%in% TRUE` coerces, but the `reencode` column type guard makes it
  unreachable.

**Disposition: back to `in-progress`.** F4 demonstrates AC1 failing, which is
the return floor. AC1 is unticked; the evidence recorded for AC2-AC7 stands.
This is the first defect return on this milestone.

**Consistency gate.** `cairn_validate` exit 0, all checks passed, no advisory
warnings. `cairn_impact` skipped: `cairn/DESIGN.md` is unchanged, so no
principle moved. Toolchain slot: `document()` no diff; `NAMESPACE`, `man/` and
`data/` regenerate clean; README.Rmd untouched so README.md is in sync;
`pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` carries the entry;
no new top-level files, so no `.Rbuildignore` entry is owed; `devtools::check()`
clean.

**AC fencing note.** The seven acceptance boxes had been ticked during
implement, which fencing forbids — they are review-owned. All seven were
unticked at the start of this review and re-ticked one at a time as the
evidence above was recorded.

