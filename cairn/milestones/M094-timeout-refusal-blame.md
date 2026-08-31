# M094: An invalid `tidymedia.timeout` is refused by the function the caller typed

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m094-timeout-refusal-blame` / [#98](https://github.com/jmgirard/tidymedia/pull/98)

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
- AC7 → T7, T13
- F-returns → T8 (F1, F2, F3, F4, F7), T9 (F8), T10, T11 (F9, F10), T12 (F5, F6)

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
- [x] T7. `NEWS.md`, `devtools::document()`, `devtools::test()`, `devtools::check()`.
- [x] T8. Re-site the calls per the amended siting rule: after every check that
      decides identically on every machine (front-door guards AND the pipeline
      builder's argument validation), immediately before the first probe or
      spawn on each path, above the `run` gate. Closes F1 (four verbs whose
      pipeline checks are masked), F2 (`hardware = "nvenc"` blaming
      `has_nvenc(family)` at eight batch verbs), F4 (`extract_frame(frame =)`
      blaming `get_frame_rate`), F7 (the ordering was per-verb) and F3
      (`normalize_audio_batch(two_pass = TRUE)` returning above its only site —
      the two-pass path takes its own call).
- [x] T9. Give `ffm_run()` and `mediainfo_parameter()` front-door calls, matching
      `ffmpeg()`/`ffprobe()`/`mediainfo()`, which already resolve before
      `find_*()`. This is F8 and the CI blocker: it puts the machine-independent
      refusal ahead of `run_program()`'s `Could not locate` check
      (`R/program_management.R:111`) per D036, and stops
      `mediainfo_parameter(<nonexistent>)` raising nothing.
- [x] T10. Widen `tm_timeout_call_specs()` with the cells the sweep could not
      see — `hardware = "nvenc"`, `extract_frame(frame =)`,
      `normalize_audio_batch(two_pass = TRUE)` — and add a binary-less-PATH leg
      so the runner-dependent failure the review found on CI is reproducible
      locally. Test coverage under AC1/AC4; no criterion widens.
- [x] T11. Instrument (F9, F10): make the AC1 test discriminate on the
      condition's identity, not only `conditionCall()`'s head; assert the
      `.rds` fixture's recorded `source`/`generator`/`recorded` provenance in
      `tm_timeout_valid_baseline()`, pinning the sha to `ae5ff1c`.
- [x] T12. Amend D074's first and third properties and the matching comment
      block above `resolve_timeout()` (`R/timeout.R:29-45`); correct
      `R/tidymedia-package.R:91-97` and the `NEWS.md` entry, which overclaim
      (F6).
- [x] T13. `devtools::document()`, `devtools::test()`, `devtools::check()`, then
      push and confirm CI green on all five platforms — the leg AC7 failed on.

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
- 2026-08-30: T7 added the NEWS entry and a paragraph to `tidymedia-package`'s "Bounding a run that hangs" section saying the refusal names the function you called, arrives on a `run = FALSE` call, and that `has_nvenc()` under a set encoder override refuses nothing. `devtools::document()`, `devtools::test()` (0 failures, 10,379 pass) and `devtools::check()` (0 errors, 0 warnings, 0 notes) all clean.
- 2026-08-30: all tasks done; status set to review.
- 2026-08-30: review returned M094 to in-progress. AC7 fails: CI on PR #98 is red on macos-latest and windows-latest with 10 failures at `test-timeout-refusal-blame.R:133`, `devtools::check()` 1 ERROR on both — on a runner with no media binaries `ffm_run` and `mediainfo_parameter` hit `run_program()`'s `Could not locate` check (`R/program_management.R:111`) before `resolve_timeout()` at `:122`, so the AC4 wording sweep measures the runner's PATH. AC1-AC6 verified locally and their evidence is recorded in the Review section; every checkbox is unticked again because the gate did not pass. Nine further findings (F1-F7, F9, F10) are recorded there, F1-F5 being defects in the deliverable.
- 2026-08-30: return gate chose one siting rule over four patches — the refusal
  fires after every check that decides identically on every machine (front-door
  guards and the pipeline builder's argument validation alike) and immediately
  before the first probe or spawn on each path, still above the `run` gate, with
  a call per spawn-bearing path where a verb has several. This is D036's
  machine-independent-first ordering applied to the seam, and it replaces D074's
  first property ("last among the front door's guards"), which F1 falsified.
  Decides F1, F2, F3, F4 and F7 together; falsified by a verb whose pipeline
  builder probes before it validates, which would leave nowhere to put the call.
- 2026-08-30: return gate chose front-door calls at `ffm_run()` and
  `mediainfo_parameter()` over hoisting the resolve inside `run_program()`,
  because `mediainfo_parameter()`'s loop `next`s past `run_program()` on a
  missing file and so would stay silent (F8's second half); and over relaxing
  the sweep, which would green CI while leaving both gaps in the deliverable.
- 2026-08-30: return gate chose keeping `has_nvenc()`'s call above D044's memo
  and amending D074's third property to carve out only a caller-set
  `tidymedia.nvenc_encoders` override, over moving it below the memo — which
  would make the refusal depend on whether the memo happened to be warm, so the
  first `has_nvenc()` of a session refuses and the second does not (F5).
- 2026-08-30: return gate chose fixing F9 and F10 here over deferring them to
  candidate rows; both guard work this milestone just did.
- 2026-08-30: no acceptance criterion is amended. AC1-AC7 stand as written and
  the cells T10 adds (`hardware = "nvenc"`, `frame =`, `two_pass = TRUE`, a
  binary-less PATH) enter as test coverage under AC1/AC4, not as new promises
  (D-118's direction rule: the criteria set neither widens nor narrows).
- 2026-08-30: T10 added `tm_timeout_variant_specs()` (axes computed from
  `formals()`, not listed) and two sweep legs — the variant cells and a
  `PATH = ""` leg. Red by design, like T2: 70 of 99 variant assertions and 10 of
  267 binary-less assertions fail. The 14 wrong cells are 12 `hardware = "nvenc"`
  paths blaming `has_nvenc`, `normalize_audio_batch(two_pass = TRUE)` blaming
  `purrr::pmap` under `purrr_error_indexed`, and `extract_frame(frame = )`
  blaming `get_frame_rate` — F2, F3 and F4 reproduced. The binary-less 10 are
  `ffm_run` and `mediainfo_parameter` x 5 forms, which is F8 and exactly the two
  members CI failed on. The nvenc cells name `video_codec = "libx264"` where the
  verb has one, because the `"copy"` default plus `hardware` is a contradiction
  the verb correctly refuses first (D036), which would have left those cells
  testing something else.
- 2026-08-30: T9 gave `ffm_run()` and `mediainfo_parameter()` front-door calls.
  The binary-less leg is green — the 10 assertions that stood for CI's macOS and
  Windows failures now pass under `PATH = ""` — and
  `mediainfo_parameter("/nope/x.mp4", "Video", "Width")` under an invalid limit
  now names itself where it raised nothing before, because its loop `next`s past
  a missing file without reaching `run_program()`. Full suite: 10,675 pass, 70
  failures, all of them T10's variant cells, which T8 owns.
- 2026-08-30: T8 re-sited per the amended rule. Ten scalar verbs moved their
  call BELOW pipeline construction (`p <- <verb>_pipeline(...)`, then the call,
  then `ffm_finish(p, run)`), so the builder's argument checks report first
  again: measured at all four verbs F1 named, a bad `regions` / `pixel_format` /
  `video_codec` now gives the same error under an unset limit and under `0.5`,
  and a clean call under `0.5` names the verb. Three verbs already built `p`
  first and only gained the comment. `nvenc_available()` was split out of
  `has_nvenc()` with `call` threaded, so the one probe that runs while a command
  is BUILT — reached from `resolve_hw_encoder()` inside the pipeline and from
  `check_nvenc_available()` at eight fan-out front doors — refuses in the verb's
  name instead of `has_nvenc`'s (F2); the site stays above D044's memo, so the
  refusal does not depend on whether this session already asked (F5).
  `extract_frame` takes its call above the `frame / get_frame_rate()` conversion,
  the one place a probe precedes the pipeline (F4), and
  `normalize_audio_batch`'s two-pass branch takes its own call before Phase 1's
  analysis, having `return()`ed above the verb's only site (F3). All 19 variant
  cells now blame the verb the caller typed; full suite 0 failures, 10,745 pass.
- 2026-08-30: T11 closed both instrument findings. AC1's sweep now calls
  `tm_refusal_head()`, which compares the message to what the one checker site
  writes before reporting the call head, so a member aborting on something else
  in its own frame names what it raised instead of passing (F9). The fixture's
  provenance is read rather than carried: `tm_provenance_ok()` requires the four
  recorded fields, the generator path, and the `ae5ff1c` sha in `source`, and is
  shown to say no on a stripped blob, another ref and another generator (F10).
- 2026-08-30: T12 amended D074. Property 1 is rewritten from "last among the
  front door's guards" to "as late as the verb allows, but never after a probe or
  a spawn", with the four verbs that falsified the old wording named; property 3
  is narrowed to a caller-set override, with the memo explicitly outside the
  carve-out and the reason stated (an error whose identity depends on session
  history is D036's failure mode moved from the machine to the session). A new
  paragraph records the build-time nvenc probe and `nvenc_available()`, and the
  falsifier list gains a verb whose pipeline builder probes before it validates.
  The same three properties are restated once above `resolve_timeout()`. The help
  page and `NEWS.md` gain the sentence F6 said they were missing — an argument
  your call got wrong is still reported first — and their `has_nvenc()` exception
  is now the only one, which the code makes true.
- 2026-08-30: T13's first CI run was red on macOS and Windows on one cell —
  `normalize_audio [two_pass = TRUE]` reporting `Could not locate FFmpeg.`,
  confirmed by reading the failing job's log rather than inferred. Same shape as
  F3 on the scalar half: the two-pass block probes (D024's drop check) and then
  spawns the analysis pass, both above the verb's site, so the limit was read
  inside `run_program()`, which refuses a missing binary first. Reproduced
  locally by running the suite with the media binaries off `PATH`, fixed with the
  branch's own call above the probe, and the binary-less leg widened to run the
  variant cells too — the base cell passed while the two-pass cell failed, which
  is exactly the gap that let this reach CI. Green both ways afterwards: 0
  failures with the binaries present (10,844 pass) and 0 without (2,453 pass over
  the timeout files).
- 2026-08-30: T13's second CI run turned macOS green and left Windows red on a
  DIFFERENT failure, read from the job log: the AC5 baseline comparison, not the
  blame sweep. `tm_scrub_paths()` redacted the fixture directory with
  `fixed = TRUE`, and on Windows that directory reaches the digest with a mix of
  separators (R's `tempdir()` gives forward slashes, the verbs concatenate
  backslashes, and `str()` prints each backslash doubled) — so the substitution
  matched nothing and every member's digest carried the runner's absolute path.
  A pre-existing instrument defect, invisible until now because the earlier
  failure in the same file stopped testthat before this comparison ran.
  `tm_dir_pattern()` makes only the SEPARATORS flexible, leaving the digest's own
  `\"` escapes alone: a Windows-shaped and a POSIX-shaped digest redact to the
  identical string, and a reading where the separator is already `/` is
  byte-for-byte what it was, so the fixture recorded on macOS stays valid.
- 2026-08-30: T13's third CI run took Windows from 106 failures to 4, all
  `concatenate_videos`/`_batch`: the separator JOINING `<tmp>` to
  `<concat-list>` survives both substitutions and is the platform's, since that
  path is built with `file.path()`. Normalized like the separators inside each
  path. Timeout files green locally either way (2,511 pass).
- 2026-08-30: T13 green everywhere. `devtools::document()` no diff,
  `devtools::test()` 0 failures / 10,844 pass / 5 skips (absent nvenc encoder) /
  12 warnings all pre-existing in unrelated files, `devtools::check()`
  **Status: OK** (0 errors, 0 warnings, 0 notes), and CI on PR #98 all ten checks
  pass — including `macos-latest (release)` and `windows-latest (release)`, the
  two legs AC7 failed on at review. The suite also runs green with the media
  binaries off `PATH`, which is what those runners are.
- 2026-08-30: all tasks done; status set to review. Defect returns on M094: 1.
- 2026-08-30: review round 2 returned M094 to in-progress by maintainer judgment at the gate, not on a criterion failure — AC1-AC7 all verified with fresh evidence and CI green on all ten checks. G1: at nine exports (the five `get_*` scalars and the four `probe_*` shortcuts) an invalid limit displaces the caller's own argument error, which is round-1 F1's class at verbs the return gate never touched, and which falsifies D074's amended property 1 and the shipped `NEWS.md`/help sentence. G2: the same paragraph's "one exception" claim is false again, since `probe_*(probe = )` reads no limit either. G3: AC3's two sweeps still use the head-only comparator. G4-G8 carried in for triage. Every criterion checkbox is unticked. Defect returns on M094: 2.

## Decisions

## Review

### Round 1 — returned 2026-08-30

PR [#98](https://github.com/jmgirard/tidymedia/pull/98). Reviewed 2026-08-30 on
`m094-timeout-refusal-blame`; `origin/master` had not moved since the branch was
cut (`ae5ff1c`), so no merge was needed before measuring.

#### Acceptance criteria — fresh evidence

- **AC1 ✓** Measured at review by a script written here, not by the suite's own
  `tm_blame_head()`: for each of the 53 members of `tm_timeout_domain()`,
  invoked through its `tm_timeout_call_specs()` cell, `conditionCall()`'s head
  was compared to the member's name. 265 cells (53 x 5 forms), 0 wrong-blame
  against the master baseline's 47-of-53. The `has_nvenc()` carve-out is
  exercised separately and raises nothing under a set
  `tidymedia.nvenc_encoders`, while still answering `TRUE` from the override.
- **AC2 ✓** The same 265 cells span all five forms `resolve_timeout()`'s comment
  names (`-1`, `0.5`, `NA`, `"2"`, `c(1, 2)`); each form is 53 of the 265.
- **AC3 ✓** `test-timeout-refusal-blame.R`'s two axis sweeps ran clean: every
  domain member carrying `run` at `run = FALSE`, and every member carrying
  `parallel` at `parallel = TRUE`. Both filters are computed from `formals()`,
  and the file asserts `extract_audio` and `extract_audio_batch` are both in
  the `run` set, so the asymmetry master had cannot vanish silently.
- **AC4 ✓** Measured at review under a pinned `cli.width = 80`: for each of the
  five forms, the 53 members produce exactly **1** distinct `conditionMessage()`,
  and that string is `identical()` to what `resolve_timeout()` itself writes for
  that form — e.g. ```tidymedia.timeout` must be a whole number, not the number
  0.5.`` At the six `purrr::map` members named by the recorded master table
  (`probe_all`, `probe_audio`, `probe_container`, `probe_streams`,
  `probe_video`, `verify_media`), neither `purrr_error_indexed` nor an
  `In index:` prefix survives.
- **AC5 ✓** The suite's baseline comparison ran clean: 53 members x 2 limit
  states (unset, 30) identical to `fixtures/timeout-valid-baseline.rds`, over 61
  exercised spawns, with `tm_spawn_interception_complete()` asserted first and
  shown falsifiable on a planted unguarded spawn. Under all five invalid forms
  every member's spawn count is 0.
- **AC6 ✓** Measured at review rather than by the suite (no committed test
  compares against `tm_timeout_reached_master()`): a forced timeout was driven
  through all 53 members and each classified abort/warn. 0 mismatches against
  T1's recorded master table, 0 silent members.
- **AC7 ✓** `devtools::test()`: 0 failures, 10,379 passing, 5 skips (absent
  nvenc encoder), 12 warnings all pre-existing in unrelated files.
  `devtools::check()`: **Status: OK** — 0 errors, 0 warnings, 0 notes.

#### Consistency gate

`cairn_validate.py` exit 0, all checks pass (`release window` advisory did not
fire). No DESIGN principle changed, so `cairn_impact.py` was skipped.
`r-package` profile slot: `devtools::document()` produces no diff in `man/` or
`NAMESPACE`; `pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries
a Bug-fixes entry with no milestone numbers; no new top-level files and no new
exports; `devtools::check()` clean.

#### Independent review

Three fresh-context lenses. The prior-review lens found no regression of any
past finding (the GitHub inline-comment probe returned empty, so the archived
`## Review` sections were the surface). The diff-bug and blame-history lenses
returned overlapping findings; every one below was re-measured against the
implementation before being recorded.

##### Findings, ranked (each re-measured at review; disposition at the gate)

- **F1. The new front-door call masks argument errors that used to fire first,
  at the four verbs whose own comments forbid exactly that.** `crop_video`
  (`R/ffmpeg.R:1483`), `format_for_web` (`:1565`), `standardize_video`
  (`:1772`), `anonymize_video` (`:1956`) deliberately carry no front-door guard
  for `video_codec` / `pixel_format` / `regions`; those checks live in the
  pipeline builder, which `ffm_finish()` evaluated lazily — i.e. after
  `resolve_timeout()` used to be reached. Siting the call on the line above
  moves it ahead of them. Measured on this branch with `tidymedia.timeout =
  0.5`: `anonymize_video(v, o, regions = "nope", run = FALSE)` reports
  ```tidymedia.timeout` must be a whole number` where the same call with the
  option unset reports `` `regions` must be a data frame with one row per box``;
  same shape at `standardize_video(pixel_format = "bad fmt!")` and
  `crop_video(video_codec = "bad codec!")`. This falsifies D074's first stated
  property ("every refusal that fired before it still fires first and only the
  blame for this one moves") and the M47-F8 comment three lines above the new
  call at `anonymize_video`. No AC fails: AC5 pins the unset/valid paths and the
  spawn count, not which condition an *invalid* limit displaces.
- **F2. Every `hardware = "nvenc"` batch call still blames `has_nvenc(family)`.**
  `check_nvenc_available()` (`R/ffmpeg.R:3093`) runs at each batch verb's front
  door *before* that verb's new `resolve_timeout()`, and `has_nvenc()` now
  raises the refusal from its own frame. Measured with the
  `tidymedia.nvenc_encoders` override unset:
  `crop_video_batch(jobs, video_codec = "h264_nvenc", hardware = "nvenc")` and
  `standardize_video_batch(...)` both report `conditionCall() =
  has_nvenc(family)`. Eight batch verbs reach `check_nvenc_available()`. Not a
  regression (master blamed `ffmpeg(...)`), but the Goal is unmet on that path;
  the sweep cannot see it because every spec cell uses `hardware = "none"`.
- **F3. `normalize_audio_batch(jobs, two_pass = TRUE)` blames
  `purrr::pmap(args, analyze_one)`, wrapped in `purrr_error_indexed`.** The
  verb's `resolve_timeout()` sits at the bottom, below the `if (two_pass)`
  branch's own `return()` (`R/ffmpeg.R:4903-4988`), so the two-pass path never
  reaches it. Measured: `In index: 1. / Caused by error in `.f()`: /
  `tidymedia.timeout` must be a whole number, not the number 0.5.` — the exact
  wrapper shape AC4's second sentence removes at the six FFprobe readers and
  `NEWS.md` says is gone. The scalar `normalize_audio(two_pass = TRUE)` is
  correct; only the batch form regressed past its own site.
- **F4. `extract_frame(infile, outfile, frame = N)` blames
  `get_frame_rate(infile)`.** `timestamp <- frame / get_frame_rate(infile)`
  (`R/ffmpeg.R:77`) runs before the new `resolve_timeout()`, and
  `get_frame_rate()` now has a front door of its own. Measured:
  `conditionCall()` is `get_frame_rate(infile)`. `frame =` is half of a
  documented "provide exactly one of" pair; `tm_timeout_call_specs()` pins
  `extract_frame` to `timestamp = 1`, so the sweep never sees it.
- **F5. `has_nvenc()` now refuses on a path that reads no limit and spawns
  nothing.** The call sits inside the `is.null(pool)` branch but above
  `cached_encoder_names()` (`R/ffmpeg.R:2968-2974`), so once D044's session memo
  is warm `has_nvenc()` aborts on an invalid limit although it asks FFmpeg
  nothing. Measured: a second `has_nvenc("h264")` in the same session, memo
  populated, aborts. This is the same situation D074's third property carves out
  for the `tidymedia.nvenc_encoders` override, handled the opposite way.
- **F6. The help text and `NEWS.md` overclaim.** `R/tidymedia-package.R:91-97`
  and the `NEWS.md` entry both say a refused value "is refused by the function
  you called" and name `has_nvenc()`-under-override as *the one* exception. F2,
  F3 and F4 are further exceptions, and F5 makes the named exception itself
  conditional on a cold memo.
- **F7. The siting is not uniform relative to pipeline construction, and no
  rule says which is intended.** `strip_metadata` (`R/ffmpeg.R:1640`),
  `separate_audio_video` (`:1170`), `compare_videos` (`:6639`) and
  `picture_in_picture` (`:6796`) place the call *after* the pipeline is built,
  so their pipeline-level checks keep precedence — which is why they are not in
  F1. The other eleven place it before. `R/timeout.R:29-45` and D074 state
  three properties, none of which decides this, so the ordering a caller gets
  depends on the verb.
- **F8. `mediainfo_parameter()` and the three Layer 0 hatches refuse only when
  they reach a spawn.** `mediainfo_parameter()`'s loop `next`s on a missing file
  (`R/mediainfo.R:82-102`), so `mediainfo_parameter(<nonexistent>, ...)` under
  an invalid limit raises nothing at all (measured: `<none>`). It was counted
  among the six "already correct" members and got no front-door call. At
  `ffmpeg()`/`ffprobe()`/`mediainfo()`, `run_program()`'s `Could not locate`
  check (`R/program_management.R:111`) precedes `resolve_timeout()` at `:122` —
  a machine-dependent refusal ahead of a machine-independent one, the inverse of
  the D036 ordering the new `probe_all_impl()` comment invokes.
- **F9. The AC1 test discriminates on the call head alone.**
  `test-timeout-refusal-blame.R:24-40` compares only `conditionCall()`'s head to
  the member name, so any error raised from that member's own frame passes,
  timeout-related or not. It is rescued by the AC4 test at `:117-133`, which
  compares `conditionMessage()` over the same domain x forms against
  `resolve_timeout()`'s own output — the file as a whole is sound, but that one
  test alone would go green on the wrong condition.
- **F10. The `.rds` fixture's provenance is recorded but never asserted.**
  `data-raw/timeout-valid-baseline.R:79-88` attaches `source` / `generator` /
  `recorded` / `r_version`; `tm_timeout_valid_baseline()`
  (`helper-timeout-sweep.R:685`) reads the blob without asserting any of them,
  and no test pins the recorded sha to `ae5ff1c`. The blob is also coupled to
  `tm_spawn_trace()`'s current digest format, so a later edit to that helper
  invalidates it with no signal beyond a wall of mismatches.

##### Disposition

**Returned to `in-progress` under the return floor.** CI on PR #98 is red on
`macos-latest (release)` and `windows-latest (release)` — 10 failures, all at
`test-timeout-refusal-blame.R:133` (the AC4 wording sweep) — so **AC7 fails**
inside its own named procedure's domain: `devtools::check()` reports 1 ERROR on
two of the five check platforms. It passes locally (Status: OK) only because
this machine has FFmpeg and MediaInfo installed. The three Ubuntu legs,
`pkgdown` and `test-coverage` are green.

The mechanism is **F8**, made fatal: on a runner with no media binaries, the
members with no front-door site — `ffm_run` and `mediainfo_parameter`, two of
the six counted "already correct" — reach `run_program()`'s `Could not locate
FFmpeg.` / `Could not locate MediaInfo.` check (`R/program_management.R:111`),
which precedes `resolve_timeout()` at `:122`. So the sweep measures the
runner's PATH rather than the package — the exact failure mode
`helper-timeout-sweep.R`'s own comments say the forcing design exists to avoid
("CI's macOS and Windows runners install no media binaries at all"), reproduced
in the new file, which spawns for real instead of injecting.

Findings carried into the return, all re-measured against the implementation:
**F1** (masked argument errors at four verbs, falsifying D074's first property),
**F2** (`hardware = "nvenc"` blames `has_nvenc(family)` at eight batch verbs),
**F3** (`normalize_audio_batch(two_pass = TRUE)` blames `purrr::pmap`, with the
`purrr_error_indexed` wrapper `NEWS.md` says is gone), **F4**
(`extract_frame(frame =)` blames `get_frame_rate`), **F5** (`has_nvenc()`
refuses on a warm memo that spawns nothing), **F6** (help text and `NEWS.md`
overclaim), **F7** (siting not uniform relative to pipeline construction, and no
rule decides it), **F8** (the CI cause above), **F9** and **F10** (instrument:
the AC1 test discriminates on the call head alone; the fixture's provenance is
recorded but never asserted).

F1–F5 and F8 are defects in the deliverable. F6 follows from them. F7 is the
design call F1 exposes — whether the site goes before or after pipeline
construction — and settling it is what makes F1's fix a rule rather than four
patches. F9/F10 are instrument work and can ride along or become candidate rows.

Nothing was fixed at the gate; no merge approval was requested and no
`cairn/.merge-approved` marker was written. Defect returns on M094: 1.

### Round 2 — 2026-08-30

Same PR [#98](https://github.com/jmgirard/tidymedia/pull/98). Re-reviewed
2026-08-30 after the round-1 return. `origin/master` is still `ae5ff1c`, the
commit the branch was cut from, so no merge was needed before measuring. CI on
PR #98 is green on all ten checks, including `macos-latest (release)` and
`windows-latest (release)`, the two legs AC7 failed on in round 1.

#### Acceptance criteria — fresh evidence

- **AC1 ✓** Measured at review by a script written here, not by the suite: each
  of the 53 members of `tm_timeout_domain()` was driven through its own
  `tm_timeout_call_specs()` cell under each invalid form, and `conditionCall()`'s
  head compared to the member's name. 265 cells, **0 wrong-blame**, against the
  recorded master baseline's 47 of 53. The `has_nvenc()` carve-out was exercised
  separately: under a set `tidymedia.nvenc_encoders` it raises nothing and still
  answers `TRUE` from the override.
- **AC2 ✓** The same 265 cells span all five forms `resolve_timeout()`'s comment
  names (`-1`, `0.5`, `NA`, `"2"`, `c(1, 2)`), 53 members each.
- **AC3 ✓** Measured at review over the axes computed from `formals()`: 31
  members carry `run` and 22 carry `parallel`; every one of the 31 at
  `run = FALSE` and every one of the 22 at `parallel = TRUE` blames itself — 0
  wrong. `extract_audio` and `extract_audio_batch` are both in the `run` set, so
  master's asymmetry cannot vanish silently.
- **AC4 ✓** Measured at review under a pinned `cli.width = 80`: for each of the
  five forms the 53 members produce exactly **1** distinct `conditionMessage()`,
  and that string is `identical()` to what `resolve_timeout()`'s own site writes
  for that form. Across the six `purrr::map` members the recorded master table
  names, 0 of 30 cells carry `purrr_error_indexed` and 0 carry an `In index:`
  prefix.
- **AC5 ✓** The suite's baseline comparison ran clean: 53 members x 2 limit
  states (unset, 30) identical to `fixtures/timeout-valid-baseline.rds`, over 61
  exercised spawns in each state, with `tm_spawn_interception_complete()`
  asserted true first and shown able to return FALSE on a planted unguarded
  spawn. Under all five invalid forms every member's spawn count is 0. The
  fixture's provenance now reads back as recorded: source `ae5ff1c`, generator
  `data-raw/timeout-valid-baseline.R`, recorded 2026-08-30.
- **AC6 ✓** Measured at review (no committed test compares against
  `tm_timeout_reached_master()`): a forced timeout was driven through all 53
  members and each classified abort/warn — 22 abort, 31 warn, **0 mismatches**
  against T1's recorded master table and 0 silent members.
- **AC7 ✓** `devtools::test()`: 0 failures, 10,844 passing, 5 skips (absent nvenc
  encoder), 12 warnings all pre-existing in unrelated files.
  `devtools::check()`: **Status: OK** — 0 errors, 0 warnings, 0 notes. CI on PR
  #98: all ten checks pass, the two round-1 failing legs among them.

#### Round-1 findings, re-measured

- **F1 fixed.** At all four verbs the finding named, the argument error reports
  first again: with `tidymedia.timeout = 0.5` and with it unset,
  `anonymize_video(regions = "nope")`, `standardize_video(pixel_format = "bad
  fmt!")`, `crop_video(video_codec = "bad codec!")` and
  `format_for_web(audio_stream = "bad stream!")` each raise the identical
  condition, and a clean call under `0.5` names the verb.
- **F2, F3, F4 fixed.** The 19 cells of `tm_timeout_variant_specs()` —
  `hardware = "nvenc"`, `two_pass = TRUE`, `extract_frame(frame = )` — all blame
  the verb the caller typed; 0 wrong.
- **F5 fixed.** A warm-memo `has_nvenc("h264")` under an invalid limit and no
  override still blames `has_nvenc`, so the refusal does not depend on session
  history; only a caller-set `tidymedia.nvenc_encoders` silences it.
- **F8 fixed.** Verified by the suite's `PATH = ""` leg, which asserts the
  binaries are actually hidden before measuring, and now runs the variant cells
  as well as the base ones.
- **F6, F7, F9, F10 fixed.** D074's first and third properties are rewritten,
  the help page and `NEWS.md` sentence added; `tm_refusal_head()` checks the
  condition's identity before reporting its head; `tm_provenance_ok()` is
  asserted and shown to say no on a stripped blob, another ref and another
  generator.

#### Consistency gate

`cairn_validate.py` — all checks pass; two advisory WARNs (a `sizing` tripwire at
13 tasks, and `work-log format` on the multi-line entries), neither a gate
failure, and the `release window` advisory did not fire. No DESIGN principle
changed, so `cairn_impact.py` was skipped. `r-package` profile slot:
`devtools::document()` produces no diff in `man/` or `NAMESPACE`;
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries a Bug-fixes
entry with no milestone numbers; `README.md`/`README.Rmd` untouched; the one new
top-level file is under `data-raw/`, already in `.Rbuildignore`;
`devtools::check()` clean.

#### Independent review

Three fresh-context lenses, distinct evidence bases. The blame-history lens
returned no findings: it re-verified each round-1 fix against the current source
and found no contradiction with D024, D036, D042 or D044. The prior-review lens
found no regression of any past finding — the GitHub inline-comment probe
returned `[]`, so the archived `## Review` sections were the surface, and it
cleared M64/M65's blame rule, M78's wording rule and D043's ordering by name.
The diff-bug lens returned eight findings. Every one below was re-measured
against the implementation at review before being recorded.

##### Findings, ranked (each re-measured at review; disposition at the gate)

- **G1. Nine exported verbs now report the limit where the caller's own argument
  error used to fire — round-1 F1's class, at verbs the return gate never
  touched.** The five `get_*` scalars (`R/mediainfo.R:359, 393, 420, 447, 474`)
  place `resolve_timeout()` above the `file` validation, which lives inside
  `mediainfo_parameter()`; `resolve_probe()`'s infile branch (`R/ffprobe.R:452`)
  places it above the `infile` validation, which lives inside
  `probe_all_impl()`, covering `probe_video`, `probe_container`, `probe_streams`
  and `probe_audio`. Measured: with the option unset, `get_width(123)` reports
  ``` `file` must be a character vector of one or more file paths.``` from
  `mediainfo_parameter`; under `tidymedia.timeout = 0.5` the same call reports
  ``` `tidymedia.timeout` must be a whole number, not the number 0.5.``` from
  `get_width`. Same substitution at all five `get_*` and at
  `probe_video(infile = 123)`, whose unset form names `probe_all`. `probe_all()`
  itself is correct — `probe_all_impl()` resolves below its own validation. This
  falsifies D074's amended property 1 ("every refusal that fired before it still
  fires first"), the `NEWS.md` sentence "An argument your call got wrong is still
  reported first", and `R/tidymedia-package.R:93-95`. No acceptance criterion
  fails: every `tm_timeout_call_specs()` and `tm_timeout_variant_specs()` cell
  carries VALID arguments, so no sweep leg can see a masked argument error —
  which is the instrument gap that let F1's class recur.
- **G2. The docs' "one exception" is still false in a second way: the four
  `probe = ` shortcuts read no limit and refuse nothing.** Measured under
  `tidymedia.timeout = 0.5`: `probe_video(probe = info)`,
  `probe_container(probe = info)`, `probe_streams(probe = info)` and
  `probe_audio(probe = info)` all return normally. That is correct behaviour by
  `resolve_probe()`'s own comment — a `probe =` call reprobes nothing — but
  `R/tidymedia-package.R:95-97`, the `NEWS.md` entry and D074's "The one
  carve-out" all say `has_nvenc()` under a set `tidymedia.nvenc_encoders` is the
  only call that refuses nothing.
- **G3. AC3's two sweeps still use the head-only comparator F9 replaced.**
  `test-timeout-refusal-blame.R:86` (`run = FALSE`) and `:116`
  (`parallel = TRUE`) call `tm_blame_head()` where the AC1 and variant sweeps
  call `tm_refusal_head()`, so any error raised from the member's own frame
  passes those two legs. The reviewer measured it: a planted unrelated abort in
  `strip_metadata()` on the `run = FALSE` path leaves the whole file green.
- **G4. AC6 has no committed test and its recorded referent is dead code.**
  `tm_timeout_reached_master()` (`helper-timeout-sweep.R:488`) is documented as
  AC6's per-member abort/warn table; nothing in `tests/`, `data-raw/` or `R/`
  calls it. AC6 held when measured at review (0 mismatches, above), but nothing
  in the repo guards it.
- **G5. `tm_provenance_ok()` checks the attribute, not the blob.**
  `helper-timeout-sweep.R:733` verifies four field names, the `ae5ff1c`
  substring and the generator path; the three `expect_false` cases all tamper
  with the attribute. A hand-edited `.rds` that keeps the attribute passes, and
  the fixture's coupling to `tm_spawn_trace()`'s digest format — round-1 F10's
  second half — is still unguarded.
- **G6. The same decision is cited two ways at the new sites.** 28 added comment
  lines cite `D042` and 18 cite `D074`; D074 is the entry that states the siting
  rule, so a reader following a `D042` comment lands on the rule's premise.
- **G7. `nvenc_available()`'s `call` default is never exercised.**
  `R/ffmpeg.R:3000` defaults `call = rlang::caller_env()` and all three call
  sites pass `call` explicitly, so a future site that forgets gets internal
  blame silently rather than an error.
- **G8. `data-raw/timeout-valid-baseline.R` is unexercised and mutates the
  shared clone.** It runs `git worktree add` / `worktree remove --force` against
  the package root (`:41-51`); nothing runs it in CI or tests, so "regenerate
  with this script" is an untested claim, and running it beside another agent or
  worktree on the same clone is not safe.

##### Disposition

**Returned to `in-progress` under the return floor, by maintainer judgment at
the gate.** No acceptance criterion fails — AC1–AC7 were all verified with fresh
evidence above and CI is green on all ten checks — but **G1** is a load-bearing
defect in what the deliverable does for its users: at nine exports (`get_duration`,
`get_frame_rate`, `get_width`, `get_height`, `get_sample_rate`, `probe_video`,
`probe_container`, `probe_streams`, `probe_audio`) an invalid limit now displaces
the caller's own argument error, which is round-1 F1's class at verbs the return
gate never touched, and which makes the shipped `NEWS.md` and help-page sentence
"An argument your call got wrong is still reported first" false. **G2** makes the
same paragraph's "one exception" claim false a second way. Every criterion
checkbox is unticked again because the gate did not pass.

Actioned: **G1** and **G2** as defects in the deliverable; **G3** as the
instrument that would have caught G1 had any sweep leg driven an INVALID
argument under an invalid limit — no cell in `tm_timeout_call_specs()` or
`tm_timeout_variant_specs()` does, which is why the class recurred silently.
**G4**–**G8** carried into the return for triage at the next gate: an
uncommitted AC6 comparison against dead recorded code (G4), a provenance check
that reads the attribute rather than the blob (G5), two decision ids cited for
one rule (G6), an unexercised `call` default (G7), and a baseline generator that
mutates the shared clone and nothing runs (G8).

Nothing was fixed at the gate; no merge approval was given and no
`cairn/.merge-approved` marker was written. PR #98 stays a draft. Defect returns
on M094: 2.
