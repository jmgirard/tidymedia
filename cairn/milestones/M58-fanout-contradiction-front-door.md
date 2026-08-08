# M58: Six argument contradictions are refused at the fan-out verb's front door

- **Status:** review
- **Priority:** normal
- **Depends on:** M57
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m58-fanout-contradiction-front-door` · PR #61 https://github.com/jmgirard/tidymedia/pull/61

## Goal

Make each of the six argument-contradiction aborts inside a fan-out verb's
pipeline report from the verb the user called, not from `purrr::pmap()`.

## Scope

**In:** the six conditions below, each extracted into a shared checker that
both the `*_pipeline()` function and the named front doors call. All six are
contradictions between values the front door already holds, so none probes
anything — D024/D034 are not engaged, and D035's licence is not needed to
decide *whether* to check, only reused as the shape to check *with*. All six
were measured reporting `purrr::pmap(jobs, .f, ...)` on merged master
2026-08-07; the eight (condition, verb) pairs are:

1. `separate_stream_pipeline()`: `video_codec = "copy"` with `hardware != "none"`
   → `separate_audio_video_batch()`
2. `segment_pipeline()`: `reencode = FALSE` with `video_codec` or `hardware` set
   → `segment_video()`, `segment_video_batch()`
3. `segment_pipeline()`: `reencode = FALSE` with `audio_codec != "copy"`
   → `segment_video()`, `segment_video_batch()`
4. `compare_videos_pipeline()`: `audio_codec` set with `audio = NULL`
   → `compare_videos_batch()`
5. `compare_videos_pipeline()`: `resize = TRUE` with `length(inputs) != 2`
   → `compare_videos_batch()`
6. `picture_in_picture_pipeline()`: `audio_codec` set with `audio = NULL`
   → `picture_in_picture_batch()`

Also in: row-sweeping wherever a value can arrive as a `jobs` column; the
contradiction-before-availability precedence the gate chose (2026-08-07),
which supersedes the order M57 pinned; condition 5's missing `call = call`;
a D-entry; NEWS.

**Out:** the four per-row value validations (crop `width`/`height` range, PiP
`margin` range, anonymize `regions` shape, compare per-row `audio` index) →
M59, which depends on this milestone for the checker shape. An unreadable
input file, which also blames `purrr::pmap` (measured 2026-08-07), → ROADMAP
candidate row: it is not an argument contradiction, and checking it at the
front door puts filesystem access before the fan-out, a D024-shaped question
owing its own entry. Line-ending governance → M60.

## Acceptance criteria

- [x] AC1 — Each of the six conditions is authored in exactly one place:
      five shared checkers, conditions 4 and 6 sharing one parameterized
      checker because their headline is byte-identical (`R/ffmpeg.R:5301`,
      `:5443`) and only the hint differs. Verified two ways: a test asserting
      exactly one `cli_abort()` site per distinct headline across `R/*.R`, and
      mutation — deleting any one front-door call turns that pair's AC2 test
      red.
- [x] AC2 — For each of the eight (condition, verb) pairs enumerated in Scope
      In, a call violating that condition aborts with `conditionCall()` naming
      the verb the user called and a message containing neither `purrr::pmap`
      nor `In index:`. One test per pair.
- [x] AC3 — For each of the six conditions the front-door guard refuses
      exactly the calls its pipeline counterpart refuses, over a committed
      before/after grid varying: each argument the condition names, at a
      violating and a non-violating value; the scalar-versus-column form of
      each of those that can be a `jobs` column; one mixed column; and, for
      condition 5, input count (2 and 3) crossed with `resize`. Each cell's
      non-violating baseline is asserted to succeed on both refs, so no cell
      compares equal by both sides failing.
- [x] AC4 — Where a contradiction's values can arrive as `jobs` columns, the
      guard sweeps rows rather than gating all-or-nothing: a column with at
      least one violating row aborts naming the verb, and a column all of
      whose rows are non-violating does not abort and compiles.
- [x] AC5 — Contradiction reports before nvenc availability on each of the
      five verbs carrying both guards, measured only on calls where both
      errors are live: a uniform call for `compare_videos_batch` and
      `picture_in_picture_batch`, whose contradiction is encoder-independent;
      a mixed column for `segment_video`, `segment_video_batch` and
      `separate_audio_video_batch`, whose contradictions each imply no encoder
      to check. The two committed tests pinning the old order
      (`test-nvenc-front-door.R:320`, `:369`) are rewritten to it. Every test
      runs under a seam holding no encoder.
- [x] AC6 — For the four conditions whose scalar sibling keeps no front-door
      guard (1, 4, 5, 6 → `separate_audio_video()`, `compare_videos()`,
      `picture_in_picture()`), the pipeline's own abort still reports with
      `conditionCall()` naming the scalar verb. Condition 5's abort threads
      `call = call` like the other five, so none of the six displays a
      `*_pipeline()` name to the user.
- [x] AC7 — The r-package profile's verify slot is clean:
      `devtools::document()` produces no diff, `devtools::test()` passes, and
      `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T3, T4, T5, T6
- AC3 → T8
- AC4 → T2, T3, T4, T5, T6
- AC5 → T7
- AC6 → T1, T6
- AC7 → T9

## Tasks

- [x] T1 — Extract the six conditions into five shared checkers; the four
      `*_pipeline()` functions call them rather than carrying the abort; give
      condition 5's abort (`R/ffmpeg.R:5312`) the `call = call` its siblings
      already thread.
- [x] T2 — `segment_video()` and `segment_video_batch()` front doors
      (conditions 2, 3), row-swept on `reencode`/`video_codec`/`audio_codec`
      columns, placed before M57's nvenc guard; retire that guard's now-dead
      `reencode` gating (`R/ffmpeg.R:2744`, `:3047`).
- [x] T3 — `separate_audio_video_batch()` front door (condition 1), row-swept
      on `video_codec`, before the nvenc guard; reconcile with the existing
      `Filter("copy")` in that guard's call (`R/ffmpeg.R:5186`).
- [x] T4 — `compare_videos_batch()` front door (conditions 4, 5), row-swept on
      `audio`/`audio_codec`/`resize` columns and per-row `inputs` lengths.
- [x] T5 — `picture_in_picture_batch()` front door (condition 6), row-swept on
      `audio`/`audio_codec`.
- [x] T6 — Tests: one blame test per (condition, verb) pair; mixed-column
      tests; scalar-sibling tests; mutation-verify each front-door call by
      deleting it and requiring the paired test red.
- [x] T7 — Rewrite the two committed precedence tests to contradiction-first
      and add the two uniform-call precedence tests.
- [x] T8 — Build and commit the before/after grid as the evidence ledger.
- [x] T9 — D-entry superseding D035's precedence example; NEWS; roxygen where
      precedence changed; run the verify slot clean.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: plan gate chose contradiction-before-availability precedence over keeping M57's availability-first order because a contradiction is machine-independent while availability is not, so the same wrong call is diagnosed identically everywhere (the M54 lesson); falsified by a contradiction whose detection turns out to need the encoder list, or by a user report preferring the availability error on a mixed column.
- 2026-08-07: plan gate chose five shared checkers over six because conditions 4 and 6 have a byte-identical headline and differ only in hint text, so six sites would fail AC1's own uniqueness test; falsified by the two hints diverging enough that one parameter cannot carry both.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on AC1 (two ways), AC3, AC4, AC6, and a coverage gap on AC7; all fixed before writing. AC2 and AC7 returned clean. Its AC5 finding — that three of five verbs are vacuous on uniform calls and two committed tests pin the opposite precedence — went to the question gate.
- 2026-08-07: T1 — five checkers extracted; the four pipelines call them. Measured against `origin/master`: five of the six messages and their `conditionCall()` are byte-identical, and condition 5's call target moves from the pipeline to its caller, which is the `call = call` AC6 asks for.

- 2026-08-07: T2 — both segment front doors check conditions 2 and 3 (the _batch one row-swept via a new `batch_arg_rows()`); M57's `reencode` gate and row-scoping on the nvenc guards retired as dead, since `hardware = "nvenc"` now contradicts every copying row before that guard runs.
- 2026-08-07: minor amendment (task reorder) — T7's rewrite of the two committed precedence tests was pulled forward into T2's checkpoint, because the precedence flip turns them red the moment T2's code lands and the verify slot must be clean per task. A third test (`test-nvenc-front-door.R:299`) kept passing but for a new reason, so its comment was corrected and a blame assertion added. T7 keeps the two uniform-call tests, which need T4/T5.
- 2026-08-07: T3 — `separate_audio_video_batch()` sweeps condition 1 over its `video_codec` column, below the reshape so a within-row output collision still reports first (M57 review F3). The nvenc guard's `Filter("copy")` retired as dead for the same reason T2's gating was. Second committed precedence test rewritten.
- 2026-08-07: T4, T5 — the two fan-in composites sweep conditions 4/5 and 6 over their `audio`/`audio_codec`/`resize` columns and per-row input counts.
- 2026-08-07: T7 — all five verbs carrying both guards now pin contradiction-first. The two mixed-column cases were rewritten under T2/T3; the three encoder-independent ones (conditions 4, 5, 6) are a new uniform-call test asserting the two seams give an identical message, and condition 5 left the availability-first sweep it used to sit in.
- 2026-08-07: T6 — `test-contradiction-front-door.R`: eight blame tests, nine mixed-versus-clean column pairs, four scalar-sibling tests, and the one-site headline check (five headlines, five sites). Mutation-verified all eight front-door call sites by deleting each and re-running the file: 3, 3, 5, 4, 2, 5, 5 and 4 assertions red respectively, every site restored after.
- 2026-08-07: T8 — `data-raw/contradiction-guard-baseline.R` (112 cells: six conditions x scalar/column/mixed forms x each named argument at a violating and a non-violating value, condition 5 crossed with input counts 2 and 3). Measured `origin/master` against the working tree: 0 vacuous cells on either ref, 33 aborting cells on both and **0 whose refusal changed**, 30 whose blame moved (29 `purrr::pmap` -> the verb, 1 `compare_videos_pipeline` -> `compare_videos`), `In index:` in 29 cells before and 0 after.
- 2026-08-07: T9 — D036 (contradiction before availability; supersedes D035's precedence example and states the checker shape M59 inherits); NEWS entry plus a narrowing of M57's precedence sentence, which this reverses for the six; the availability roxygen on the five verbs carrying both guards gained the precedence sentence.
- 2026-08-07: verify slot clean — `devtools::document()` no diff, `devtools::test()` 4104 pass / 0 fail / 5 skip, `devtools::check()` 0 errors / 0 warnings / 0 notes. Status -> review.
- 2026-08-07: restored `R/ffmpeg.R`'s CRLF endings, which scripted edits during T1-T9 had stripped: master's copy is uniformly CRLF, so the branch diff had grown to 12072 changed lines and would have swept M60's scope into this PR. Real diff on that file is 307 insertions / 135 deletions; verify slot re-run clean afterwards.

## Decisions

## Review

Reviewed 2026-08-07 on branch `m58-fanout-contradiction-front-door`, PR #61.
`origin/master` unmoved since the branch was cut (ffcb6d5), so no merge-forward
was needed. All evidence below is fresh, gathered by command in this session.

**AC1** — Each of the five distinct headlines the six conditions carry occurs at
exactly one `cli_abort()` site across `R/*.R` (grep count 1 for each of the
five; five checker definitions). Conditions 4 and 6 share one checker, and the
two verbs' hints are asserted to differ. Mutation, re-run fresh over all eight
front-door call sites: deleting each turned 3, 3, 5, 4, 2, 5, 5 and 4
assertions red respectively (segment_video c2/c3, segment_video_batch c2/c3,
separate_av_batch c1, compare_batch c5/c4, pip_batch c6); `R/ffmpeg.R` restored
clean after each.

**AC2** — `test-contradiction-front-door.R` "every (condition, verb) pair blames
the verb the user called": 48 assertions pass over the eight pairs (six per
pair — the condition's own message, `conditionCall()` naming the verb, and the
absence of `purrr::pmap` / `In index:` from both message and call).

**AC3** — `data-raw/contradiction-guard-baseline.R`, `origin/master` against the
branch: 112 cells (condition 1: 8 scalar / 4 column / 2 mixed; 2: 16/8/4;
3: 8/4/2; 4: 12/6/2; 5: 8/4/4; 6: 12/6/2). Vacuity screen empty on BOTH refs,
so no cell compares equal by both sides failing. 33 cells abort on each ref and
**0 cells changed which refusal they get**. Blame moved on 30 cells (29
`purrr::pmap` -> the verb, 1 `compare_videos_pipeline` -> `compare_videos`);
`In index:` present in 29 cells before and 0 after.

**AC4** — "one violating row is refused and a clean column compiles": 54
assertions pass over nine mixed-versus-clean pairs covering every column a
condition's values can arrive in (`video_codec`, `reencode`, `audio_codec`,
`audio`, `resize`, and per-row `inputs` counts). Each pair asserts the violating
table aborts naming the verb AND the clean table compiles the expected number of
commands, so no case passes by refusing everything.

**AC5** — Contradiction-before-availability pinned on all five verbs carrying
both guards, every test under a seam holding no encoder. Mixed-column cases:
"a MIXED reencode column is refused at the front door" (3), "on a mixed column
the cut contradiction reports before availability" (6), "a mixed copy column
reports the copy conflict before availability" (6) — the last two being the
committed tests that pinned the old order, rewritten. Uniform-call cases: "a
contradiction reports before availability on the fan-in verbs" (15) covers
conditions 4, 5 and 6, asserting the present-encoder and absent-encoder seams
yield an identical message.

**AC6** — "the scalar siblings still blame themselves": 16 assertions pass over
`separate_audio_video`, `compare_videos` (conditions 4 and 5) and
`picture_in_picture`, each asserting `conditionCall()` names the verb and
contains no `_pipeline`. Condition 5's `call = call` is what makes its case
pass; on master the same call named `compare_videos_pipeline`.

**AC7** — `devtools::document()` produced no diff; `devtools::test()` 4104 pass
/ 0 fail / 0 error / 5 skip; `devtools::check()` Status OK, 0 errors / 0
warnings / 0 notes.

**Consistency gate** — `cairn_validate` exit 0, all 16 checks PASS and all 8
advisories OK. No `DESIGN.md` principle changed, so `cairn_impact` did not
apply. Toolchain slot: `document()` no diff (generated files not hand-edited);
`README.Rmd` untouched so README is in sync; `pkgdown::check_pkgdown()` reports
no problems; NEWS.md carries the user-visible entry with no milestone numbers;
no new top-level files (both new files sit under already-ignored `data-raw/`
and `tests/`); `check()` clean.

