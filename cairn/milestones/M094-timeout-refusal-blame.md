# M094: An invalid `tidymedia.timeout` is refused by the function the caller typed

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m094-timeout-refusal-blame`

## Goal

`options(tidymedia.timeout = <invalid>)` makes 47 of the 53 exports in
`tm_timeout_domain()` abort naming a function the caller never typed; the
refusal fires instead from the frame that can name the caller.

## Scope

Surface tier: **user-facing** — the deliverable is the condition every exported
timeout-domain verb raises for an invalid option value.

**In:** siting the refusal per D042 (the export re-calls `resolve_timeout()` at
its own front door) across the six wrong-blame classes measured on master
2026-08-30 — `ffm_run(object)` ×15, `ffm_batch(jobs, <deparsed builder>)` ×17,
`ffmpeg(...)` ×4, `mediainfo_parameter(...)` ×6, `mediainfo_read(file, inform)`
×3, `purrr::map(infile, probe_one)` ×6. A sweep test over the computed domain.
The D-entry, NEWS.

**Out:** the blame of any condition other than this refusal — a reached limit's
own condition is D049's and is unchanged (AC6). `resolve_timeout()`'s message
wording, which must not fork (AC4). A `call` argument on the exported builders,
which D042 rejected → stays rejected; superseding it is its own milestone.

## Acceptance criteria

- [ ] AC1. With an invalid `tidymedia.timeout` set, every member of
      `tm_timeout_domain()` (computed at `tests/testthat/helper-timeout-sweep.R:104`),
      invoked through its own `tm_timeout_call_specs()` cell, raises a condition
      whose `conditionCall()` head is that member's own name. One carve-out: a
      call that never reads the limit — `has_nvenc()` under a set
      `tidymedia.nvenc_encoders`, where D044's memo sits above the read — raises
      nothing and is not required to. Master baseline (2026-08-30): 6 of 53.
- [ ] AC2. AC1 holds for each invalid form `resolve_timeout()`'s own comment
      names (`R/timeout.R:19-26`) — `-1`, `0.5`, `NA`, `"2"`, `c(1, 2)` — not for
      one form alone.
- [ ] AC3. AC1 holds at `run = FALSE` as well as `run = TRUE`, and at
      `parallel = TRUE` as well as the default, at every domain member carrying
      that argument. This closes the asymmetry measured on master, where
      `extract_audio(v, o, run = FALSE)` raises nothing and
      `extract_audio_batch(jobs, run = FALSE)` aborts.
- [ ] AC4. The wording does not fork: for each AC2 form, `conditionMessage()`
      under a pinned `cli.width` is identical across all 53 members and equals
      what `resolve_timeout()`'s single `rlang::check_number_whole()` site
      (`R/timeout.R:31`) produces for that form. At the six members that blame
      `purrr::map(infile, probe_one)` today, the `purrr_error_indexed` class and
      its `In index: 1. / Caused by error in .f()` prefix are gone.
- [ ] AC5. Blame changes and nothing else (D042's siting rule): with the option
      unset or set to a valid whole number, every domain member's return value
      and its `system`/`system2` count are unchanged from the T1 baseline; and
      under an invalid limit no domain member reaches `system()`/`system2()`.
- [ ] AC6. D049's rule is unchanged: with the limit forced to be reached, every
      member of `tm_timeout_domain()` still either aborts or warns, and which of
      the two each member does matches T1's per-member table.
- [ ] AC7. `devtools::test()` passes and `devtools::check()` reports 0 errors and
      0 warnings.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T1, T2
- AC3 → T2, T3, T4
- AC4 → T4, T5
- AC5 → T1, T3, T4, T5
- AC6 → T1, T7
- AC7 → T7

## Tasks

- [x] T1. Capture two master baselines into `tests/testthat/helper-timeout-sweep.R`
      as recorded tables, so AC1/AC5/AC6 have a referent the repo does not hold
      today: per-member blame under each AC2 form, and per-member abort-vs-warn
      under a reached limit (the grid at `test-timeout-silence.R:342` records
      only the disjunction).
- [x] T2. Write the failing sweep over `tm_timeout_domain()` × AC2's forms for
      AC1, including the `has_nvenc()` carve-out and AC3's `run = FALSE` /
      `parallel = TRUE` cells. Expect red at 47 of 53.
- [x] T3. Site the re-call per D042 at the Layer-2 callers of the two exported
      builders: the 15 verbs blaming `ffm_run(object)` and the 17 blaming
      `ffm_batch(...)` (`R/ffmpeg.R`), above each verb's `run` gate so AC3 holds.
- [x] T4. Site the remaining four classes: the 4 blaming `ffmpeg(...)`
      (`ffmpeg_codecs`, `ffmpeg_encoders`, `has_nvenc` — below D044's memo), the
      6 `get_*` blaming `mediainfo_parameter(...)`, the 3 `mediainfo_*` blaming
      `mediainfo_read(file, inform)`, and the 6 `probe_*`/`verify_media` blaming
      `purrr::map(infile, probe_one)`. The last two classes may instead thread
      `call` through the internal helper, which D042's carve-out allows.
- [x] T5. Assert AC4 (one wording, pinned `cli.width`, the `purrr` wrapper gone)
      and AC5 (valid/unset path and spawn counts unchanged against T1).
- [x] T6. Append the D-entry: the refusal is sited at the verb the caller typed
      per D042; it fires at `run = FALSE` too, and why that is not a D024 breach;
      the `has_nvenc()` carve-out.
- [ ] T7. `NEWS.md`, `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (declared tier user-facing); a fresh-context reader that authored none of the criteria returned 8 findings. Six fixed here and reported in chat: AC4's master-baseline referent contradicted AC1 (rewritten to state the property directly); AC3 was instrument-bound on a spawn interceptor that does not exist (narrowed to the deliverable, folded into AC5); AC1's member-vs-cell quantifier was ambiguous and false at `has_nvenc()` (quantifier stated, carve-out added); no criterion pinned the valid-limit path unchanged (AC5); AC6 compared against a per-member abort/warn table the repo does not hold (T1 captures it); AC2's form set omitted the length-2 form its own checker comment names (added). The seventh — whether `run = FALSE` should start refusing — went to the gate and is AC3. The eighth (one spec cell per member) is accepted: the cross-product has no enumerating table, so AC3 varies the axes that matter instead of widening AC1.
- 2026-08-30: plan gate chose D042's front-door re-call over a `call` argument on the exported builders (`ffm_run()`, `ffm_batch()`, `ffmpeg()`, `mediainfo_parameter()`) because D042 already rejected that shape as API surface with an audience of one, and its carve-out still allows threading `call` through the internal `mediainfo_read()`/`probe_one()`; falsified by a shared checker whose abort cannot be aimed at a Layer-2 caller from the verb's own frame.
- 2026-08-30: plan gate chose refusing at `run = FALSE` on both forms over keeping `run = FALSE` a pure compile, because `R/ffm_batch.R:96-99` already made that choice for the batch form and the scalar/batch split is itself the defect; falsified by a report of a dry-run compile refused on a limit that run would never have read.
- 2026-08-30: plan gate chose this scope over the M071 F9 option-rollback fix because the rollback was measured to be `future`'s own (it restores `options()` at every future boundary, sequential plan included), leaving nothing for the package to fix; falsified by a `future` release that stops restoring options across the boundary. Recorded as D073.
- 2026-08-30: implement started on `m094-timeout-refusal-blame`; master blame baseline re-measured at 53 domain members, 6 already correct (`ffm_batch`, `ffm_run`, `ffmpeg`, `ffprobe`, `mediainfo`, `mediainfo_parameter`).
- 2026-08-30: gate chose the same front-door re-call at the six metadata readers over threading `call` into `mediainfo_read()`/`probe_one()`, because that plumbing also renames the blame on a REACHED limit, which Scope puts out of bounds; and chose the timeout check last among each front door's guards, so every refusal that fires today still fires first.
- 2026-08-30: T1 recorded both master baselines in `helper-timeout-sweep.R` (`tm_timeout_blame_master()`, `tm_timeout_reached_master()`), each verified cell-by-cell against a live measurement at ae5ff1c: 6 of 53 members already named themselves, and the blame head is identical across all five invalid forms at every member.
- 2026-08-30: measured per-class wrong-blame counts are `ffm_run` 14, `ffm_batch` 16, `ffmpeg` 3, `mediainfo_parameter` 5, `mediainfo_read` 3, `purrr::map` 6 (47 total). Scope's `×15/×17/×4/×6/×3/×6` counts each class INCLUDING its own leader, which already blames itself correctly; the six classes and the 47-of-53 total are unchanged.

- 2026-08-30: T2 added `test-timeout-refusal-blame.R` — the domain x invalid-form sweep plus the `run = FALSE` and `parallel = TRUE` axes and the `has_nvenc()` carve-out. Red on master at 47 of 53 members (235 of 265 blame cells), green on the 6 that already named themselves.
- 2026-08-30: T3 sited `resolve_timeout()` at the front door of the 30 verbs in the `ffm_run` and `ffm_batch` classes, last among each front door's guards and above the `run` gate; the siting rule and its three properties are stated once above `resolve_timeout()` in `R/timeout.R` and cross-referenced at each site. Delegated to one [S] subagent; diff verified site by site. Sweep now leaves exactly the 17 members T4 owns, and the rest of the suite is clean (105 failures, all in the new file; 9271 pass).
- 2026-08-30: T4 closed the last four classes. Front-door calls at `ffmpeg_codecs()`, `ffmpeg_encoders()` and the five `get_*` scalars; at `mediainfo_query()` and `mediainfo_template()` (whose alias `mediainfo_summary()` inherits the right name, since the refusal is built from the frame's own call). `has_nvenc()` takes its call INSIDE the `is.null(pool)` branch, so a call answered by the override refuses nothing. The six FFprobe readers take two internal sites rather than six front doors — `probe_all_impl()` (covering `probe_all()` and `verify_media()`) and `resolve_probe()`'s infile branch (covering the four `probe_*` shortcuts) — both of which already thread `call`, neither of which builds the reached-limit condition, so no reached-limit blame moves; the `probe = ` path, which reprobes nothing, still reads no limit. Sweep now reports 0 of 53 members wrong; full suite 0 failures, 9376 pass.
- 2026-08-30: T5 asserted AC4 and AC5. Every member's message is compared to what `resolve_timeout()`'s own site writes for that form under a pinned output context, not to the other members, so 53 copies of a drifted wording could not pass; the `purrr_error_indexed` class and its `In index:` prefix are gone at the six readers named by the recorded master table rather than by a retyped list.
- 2026-08-30: T5's AC5 half added a third master baseline the plan had not named — per-member return value and spawn count under an unset and a valid limit — recorded as `tests/testthat/fixtures/timeout-valid-baseline.rds` from ae5ff1c by `data-raw/timeout-valid-baseline.R`, which runs the suite's own `tm_spawn_trace()` against a worktree of that ref rather than a second copy of the reading. 53 members x 2 limit states compare identical, over 61 exercised spawns; under all five invalid forms every member's spawn count is 0. Spawn interception is at `guard_timeout()`, which `tm_spawn_interception_complete()` proves sufficient from the computed spawn-site set, and which is itself shown able to return FALSE on a planted unguarded spawn.
- 2026-08-30: T6 appended D074 — the refusal is sited at the verb the caller typed, it fires at `run = FALSE`, why that leaves D024 untouched, and the `has_nvenc()` carve-out.
## Decisions

## Review
