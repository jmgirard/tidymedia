# M62: A missing input file is refused at the front door, in both forms

- **Status:** review
- **Priority:** normal
- **Depends on:** M61
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m62-fanout-input-path-front-door` / [#65](https://github.com/jmgirard/tidymedia/pull/65)

## Goal

Make a call naming an input file that does not exist report against the verb the
user called, in both the table-driven and the scalar form.

## Scope

**In:** One shared, vector-capable missing-input checker, arity-templated so a
one-path call renders byte-identically to today's `check_file_exists()` message.
Every exported verb that fans out through `ffm_batch()` sweeps its resolved
input paths at the front door — the `input` column, the `inputs` list-column,
and `picture_in_picture_batch()`'s `main`/`overlay` pair. The two scalar fan-in
verbs `concatenate_videos()` and `compare_videos()`, which have no front-door
input guard at all today, gain one. A `cairn/DECISIONS.md` entry licensing a
filesystem read at a verb's front door under D024's third exclusion. The sweep's
precedence fixed and measured against every front-door abort it now precedes.

**Out:** The existing-but-unreadable input, which `check_file_exists()`'s
existence predicate does not catch and `ffm_files()`'s readability predicate
does → M63 (planned, depends on this). Output paths and `outdir` creation →
unchanged, no row. Any verb outside the derived fan-out and `ffm_files`-reaching
sets — the metadata side (`probe_all()`, `mediainfo()`) — → no row; those never
reach `ffm_files()`.

## Acceptance criteria

- [x] AC1 — The front door's missing-input abort is written at exactly one
      site. A test walks every function body in `asNamespace("tidymedia")` and
      asserts that abort's wording appears in exactly one, which
      `check_file_exists()` and every front-door sweep reach. `ffm_files()`
      keeps its own separate readability abort, which M63 unifies with this
      site; the same test asserts `ffm_files()` and its `ffm` alias are the only
      places that second wording appears, so the residual is pinned by a test
      rather than assumed.
- [ ] AC2 — For a one-path call the shared checker's rendering is byte-identical
      to the string `check_file_exists()` emits on merged master, asserted by a
      snapshot recorded against the pre-change ref; for a multi-path call it
      names every missing path, not the first.
- [x] AC3 — Every exported verb whose parsed call graph transitively reaches
      `ffm_batch` refuses a missing input at its front door, with
      `conditionCall()` naming that verb. The domain is derived by that walk and
      never listed: the test fails when a verb the walk returns has no
      call-shape spec, so the walk fixes membership and the spec supplies only
      the shape of a legal call.
- [x] AC4 — Every exported verb whose parsed call graph transitively reaches
      `ffm_files` but not `ffm_batch` refuses a missing input at its own front
      door, with `conditionCall()` naming that verb — the same walk-derived,
      spec-required construction. This is what makes `concatenate_videos()` and
      `compare_videos()` stop reporting `Error in ffm_files(infiles, outfile)`.
- [x] AC5 — `data-raw/input-guard-baseline.R` generates its cells from declared
      axes rather than hand-written rows, crossing each verb and form with each
      front-door abort named in its declaration: the M58 contradiction sweep,
      `check_nvenc_available()`, `ffm_batch()`'s `run` guard, and the four value
      guards D039 moved (`direction`, `position`, `margin`, the per-row `audio`
      bound). A companion `input_guard_uncovered()` re-derives the same product
      and reports any declared combination with no cell. Measured over both
      refs: no call's refused-or-accepted status changes, no message regresses,
      no abort loses its `call`, and every cell's crossed error is shown live by
      a paired control.
- [x] AC6 — The sweep runs after each fan-out verb's jobs-shape and column-type
      guards and before its M58 contradiction sweep, pinned by the AC5 cells
      that cross the two.
- [x] AC7 — `input_guard_uncovered()`, the control validator, and the domain
      walk are each verified by mutation: deleting one verb's spec makes the
      reader report every combination that verb owed, re-pointing a control at a
      different error makes it fail, and deleting a call edge changes the
      derived verb set.
- [x] AC8 — A `cairn/DECISIONS.md` entry licenses a filesystem read at a verb's
      front door, quoting D024's third exclusion verbatim, taking D035's *shape*
      and not its licence (D035's rule is conditioned on a probe whose result
      enters the compiled command; this one's does not), stating its conditions
      and what it does not license, and recording the M63 residual.
- [x] AC9 — `NEWS.md` records the blame move in user-facing terms with no
      milestone number, and a named test fails without the behavior it asserts.
- [x] AC10 — `devtools::document()` produces no diff, `devtools::test()` and
      `devtools::check()` are clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T7
- AC2 → T1, T7, T9
- AC3 → T2, T3, T7
- AC4 → T4, T7
- AC5 → T5
- AC6 → T2, T3, T5
- AC7 → T6
- AC8 → T8
- AC9 → T8
- AC10 → T8

## Tasks

- [x] T1 — Add the shared vector-capable checker beside `check_file_exists()`
      (`R/utils.R:26-37`); make `check_file_exists()` delegate to it so the
      one-path rendering is unchanged. Snapshot the pre-change message first.
- [x] T2 — Wire the per-row sweep into the two shared jobs validators —
      `check_batch_jobs()` (`R/ffmpeg.R:4310`) and `check_fanin_jobs()`
      (`R/ffmpeg.R:4563`) — and into `picture_in_picture_batch()`'s inline
      `main`/`overlay` block (`R/ffmpeg.R:6176`).
- [x] T3 — Wire it into the fan-out verbs that validate their jobs table inline
      rather than through those two helpers: the sites at `R/ffmpeg.R:1822`,
      `:3155`, `:3319`, `:3478`, `:3705`, `:3903`, `:4102`, plus `segment_video()`
      and `normalize_audio(two_pass = TRUE)`, which reaches `ffm_batch` through
      `run_normalize_correction()`.
- [x] T4 — Add the front-door guard to `concatenate_videos()` and
      `compare_videos()`.
- [x] T5 — Author `data-raw/input-guard-baseline.R` and
      `input_guard_uncovered()` on the D039 pattern
      (`data-raw/value-guard-baseline.R` is the model); run it against both refs.
- [x] T6 — Mutation-verify the reader, the control validator, and the domain
      walk (`data-raw/value-guard-mutations.py` is the model).
- [x] T7 — Write the walk-derived completeness tests for AC1–AC4, reading the
      namespace rather than the source tree (the M51/M59 lesson).
- [x] T8 — D-entry, `NEWS.md`, roxygen for the two newly-guarded verbs; then
      `document()` / `test()` / `check()`.
- [x] T9 — review return 1: fix F1 (factor carrier), F2 (two columns, two
      aborts) and F3 (duplicates counted twice) at the shared site; widen the
      grid on the two axes the review's AC5 caveat named so the fix ships
      measured; regression tests; `NEWS.md` correction (F11).

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned 7 findings. Six had one right answer and were fixed before the gate: vector-capable single abort site (AC1/AC2), crossing axes narrowed to what the grid declares and widened to D039's four value guards (AC5), domain derived by parsed call-node walk rather than deparsed substring — fixing a measured false positive (`ffm_manifest`, matched inside a cli string) and a measured false negative (`normalize_audio(two_pass = TRUE)`) (AC3/AC4), aggregate-vs-first-path message made a measured property (AC2), D035 cited for shape not licence (AC8), third mutation over the domain walk (AC7). The seventh became gate question 2.
- 2026-08-08: plan gate chose the existence predicate over `ffm_files()`'s readability predicate because it reuses the wording thirteen scalar verbs already emit and keeps both forms uniform, at the cost of an existing-but-unreadable residual; falsified by a report of an unreadable-but-present input reporting differently by form before M63 ships.
- 2026-08-08: plan gate chose placing the sweep above the M58 contradiction sweep over placing it below, because a caller who mistyped a path should hear about the path and this matches where the scalar verbs' guard already sits; falsified by a report preferring the contradiction on a table that is both wrong about a path and self-contradictory. The alternative carried D036's machine-independent-first reasoning, which the M62 D-entry must therefore address rather than ignore.
- 2026-08-08: T2-T4 - the sweep wired into 15 `_batch` verbs plus the 2 scalar fan-in verbs, delegated to an [S] agent against an objective checker; diff reviewed site by site. 30/30 verbs now refuse at their own front door; suite unchanged at 4658 pass / 0 fail. Two spec bugs found and fixed first (anonymize's `regions` cell shape, extract_frame's `timestamp` column) - both were aborting for their own reason and reading as passes, the M54 vacuity trap.
- 2026-08-08: T7 (part) - walk-derived completeness tests added, including one pinning that `ffm_manifest` is excluded because its `ffm_batch(` occurrence is inside a cli string.
- 2026-08-08: plan chose a generated cross-product grid over hand-written per-verb tests because M61's three review returns were each a combination nobody typed; falsified by the grid's declaration itself dropping a crossing, which its reader cannot catch.
- 2026-08-08: implement gate chose the count-first plural rendering over one always-pluralized sentence, and `jobs$input` over the package's "the `input` column of `jobs`" phrasing, so both forms share one sentence shape; both prototyped against cli before the chip.
- 2026-08-08: T1 — `check_paths_exist()` added at `R/utils.R:26`; `check_file_exists()` delegates its existence half. One-path rendering pinned byte-for-byte against strings captured from master before the change. Suite 4658 pass / 0 fail; the 4 warnings and 5 skips are pre-existing (M44 dropped-track warnings, nvenc-absent skips).
- 2026-08-08: amendment (substantive, user-approved) - AC1 rewritten. It promised `ffm_files()` reaches the shared abort site, which M62 deliberately does not do: `ffm_files()`'s predicate is readability and unifying the two is M63's scope. The replacement promises one site for the FRONT DOOR and pins the residual with a test asserting `ffm_files()`/`ffm` are the only other place an input refusal is worded.
- 2026-08-08: T7 - walk-derived tests complete; 138 pass. Verified by mutation, not by eye: deleting crop_video_batch's sweep -> 3 red; deleting compare_videos' sweep -> 3 red; duplicating the abort wording into another body -> 1 red; deleting one verb's call-shape spec -> 3 red; degrading the walk to a deparsed-substring match -> `ffm_manifest` re-enters the fan-out set, which its pinning test refuses.
- 2026-08-08: session end at a task boundary; T1-T4 and T7 done, branch pushed, tree clean, suite 4785 pass / 0 fail. Resume at T5 (`data-raw/input-guard-baseline.R`, modelled on `data-raw/value-guard-baseline.R`), then T6 and T8. `data-raw/input-guard-progress.R` is the working checker, not evidence, and is deleted at T8.
- 2026-08-08: T5 — `data-raw/input-guard-baseline.R` generates its cells from three declarations (the walk-derived verb domain, a per-verb crossing list, a per-verb call shape) crossed with the two forms. Measured at this commit by sourcing that file and running its nine readers over `origin/master` and the working tree: 404 cells; vacuous 0 and 0, refusals 0, message regressions 0, blame regressions 0, lost `call` 0, dead controls 0, uncovered 0, misordered 0, with 66 cells' blame moved to the verb the user called. Three declarations were corrected by the grid rather than by eye: six fan-out verbs validate their table inline and reject no NA in the input carrier, so `jobs_na` is declared only where that guard exists and those six pin AC6's upper half through `column_type` instead; `sample_frames_batch` needs a per-row `outdir` or every multi-row cell reports the frame-pattern collision guard; and the `nvenc` crossing must not name `video_codec` on a verb that has none, which raises "unused argument" in place of the crossed error.
- 2026-08-08: T5 (minor amendment) — the crossing declaration names two aborts beyond AC5's colon-list, `jobs_na` and `column_type`, because AC6's first half ("after each fan-out verb's jobs-shape and column-type guards") has no cell without them. AC5's four remain as written; this widens the declaration the reader re-derives from, never narrows it.
- 2026-08-08: T6 — `data-raw/input-guard-mutations.py`; all three AC7 mutations caught, tree restored by the harness after each. Deleting `crop_video_batch`'s call shape made `input_guard_uncovered()` report exactly the 10 combinations it owed (5 crossings x 2 forms); re-pointing the `audio_codec` contradiction's control at `ffm_batch()`'s `run` guard made `input_guard_dead_controls()` report 8 controls, each `reported run_guard`; deleting `strip_metadata_batch`'s `ffm_batch()` call edge moved it out of the walk's fan-out set and into its scalar set. `input_guard_domain()` was narrowed so the missing-shape case reaches the reader instead of a hard error that would have shadowed it.
- 2026-08-08: T8 — D040 appended (licenses the front-door filesystem read; quotes D024's third exclusion verbatim, takes D035's shape and not its licence, discloses the existence-vs-readability residual M63 closes, and answers D036's machine-independence argument rather than ignoring it). `NEWS.md` bug-fix entry, no milestone number. `@param infiles` on `concatenate_videos()` and `compare_videos()` records the new front door. `data-raw/input-guard-progress.R` deleted as planned — it was the working checker, never evidence.
- 2026-08-08: T8 — measured at this commit: `devtools::document()` no diff after regenerating the two `.Rd` files, `devtools::test()` 4785 pass / 0 fail (4 warnings and 5 skips pre-existing: M44 dropped-track warnings, nvenc-absent skips), `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes. `spelling::spell_check_package()` clean after two NEWS words were reworded rather than added to the wordlist.
- 2026-08-08: review return 1 (defect) — AC2 fails and two message/blame regressions against `origin/master` were measured. F1: `segment_video_batch()` never coerces `jobs$input` to character, so a factor column reaches `file.exists()` and raises the base error `invalid 'file' argument` blamed on `file.exists(x)`, where master raised a cli abort blamed on `purrr::pmap()`. F2: `picture_in_picture_batch()` calls the sweep once per column, so a row missing both `main` and `overlay` names only `main`, where master named both — AC2's "names every missing path, not the first" is false for that verb. F3 (folded in, scored 82): `check_paths_exist()` has no `unique()`, so one bad path shared by N rows reports as N files. Status back to `in-progress`; the 16 sub-threshold findings are logged in the Review section.

- 2026-08-08: T9 (minor amendment) — T9 added as a discovered task carrying review return 1; AC2's Coverage row gains it. No criterion or scope text changed: the three findings are defects against AC2 and AC5 as written.
- 2026-08-08: T9 — F1/F2/F3 fixed at the shared site, not at the three verbs. `check_paths_exist()` coerces its carrier with `as.character()` before `file.exists()` and deduplicates `missing`; `check_batch_inputs()` accepts several columns and `picture_in_picture_batch()` sweeps `main`/`overlay` in one call, the message's verb agreeing with `length(arg)`. All three reproduced against `origin/master` first and re-measured after: F1 `invalid 'file' argument` blamed on `file.exists(x)` → the verb's own abort (and a factor column of PRESENT paths falls through to exactly master's error, unmoved); F2 `jobs$main` alone → `` `jobs$main` and `jobs$overlay` name 2 files that do not exist ``; F3 two rows one bad path "2 files" → "1 file".
- 2026-08-08: T9 — grid widened on the two axes the review's AC5 caveat named, so the fix ships measured rather than asserted: `all`'s absent paths are now distinct, and two declared forms were added at the `none` crossing — `dup` (2+ slots, one repeated absent path) and `factor` (the path column re-typed). 524 cells, 424 live, up from 404/362.
- 2026-08-08: T9 — two readers added, because the widened cells had nothing holding them: `input_guard_unreported()` (every uncrossed cell must report the missing path, closing the review's F7 gap that the new forms would otherwise sit in) and `input_guard_unnamed()` (every distinct absent path named, counted once — AC2's second clause as a query over the domain instead of at hand-typed shapes).
- 2026-08-08: T9 — measured over three refs at this commit. Working tree: all eleven readers empty (vacuous both refs, refusals, message regressions, blame regressions, lost `call`, dead controls, uncovered, misordered, unreported, unnamed), 96 cells' blame moved. Pre-fix branch tip `70dc722` under the SAME widened grid: unnamed 19, blame_regressions 1 (`segment_video_batch`/`factor` blaming `file.exists`), unreported 1 — so the widening is falsifiable, not decorative. `origin/master`: unnamed 17.
- 2026-08-08: T9 — the three regression tests fail against `70dc722` and pass here, verified by running the new test file in a worktree at that ref: 7 assertion failures across the three (20-vs-1 duplicate count, `jobs$main` alone twice, `invalid 'file' argument` / `file.exists` three times).
- 2026-08-08: T9 gate chose to leave scalar `picture_in_picture(main, overlay)` naming only `main` when both are missing, at parity with master, over sweeping its two arguments jointly: they are two single-file arguments and a single-file argument reporting on its own is AC2's first clause. Recorded as a declared `separate_args` exclusion in the grid rather than an undisclosed one; falsified by a report reading two missing arguments as one multi-path call.
- 2026-08-08: T9 — two below-threshold findings folded in where the same prose was being rewritten anyway: F11 (`NEWS.md` said `segment_video()` took a `jobs` table, and "first" meant the first row) and F5 (`R/utils.R`'s header and D040's first condition still claimed `ffm_files()` reaches the shared site — the claim AC1 was amended to retract). The other 14 stand as logged.
- 2026-08-08: T9 — measured at this commit: `devtools::document()` no diff, `devtools::test()` 4793 pass / 0 fail (4 warnings and 5 skips pre-existing), `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes, `spelling::spell_check_package()` clean after one NEWS word was reworded. `python3 data-raw/input-guard-mutations.py` all three CAUGHT, mutation 1 now reporting the widened 12 combinations rather than 10; tree restored.

## Decisions

- **M62-D1: the grid's ordering claim is stated over the after ref alone.**
  `input_guard_misordered()` asserts only where each crossed cell's error
  reports NOW — the crossing for the two guards above the sweep, `input` for
  the four below. It states no expected BEFORE value, because there is no
  single one: a verb that already guarded its input reported `input` before
  too, and the two scalar fan-in verbs reported `ffm_files`'s readability
  refusal for some crossings and the crossing itself for others, depending on
  where their pipeline happened to reach `ffm_files()`. Declaring a
  per-(verb, crossing) `want_before` would be fitting the expectation to the
  measurement. The before ref carries AC5's claims instead — fate, message,
  blame, lost `call` — and `input_guard_ordering()` shows the move as a table
  without asserting one shape for it.

- **M62-D2: "names every missing path" is a claim about a CARRIER, not about a
  call.** `input_guard_unnamed()` holds every uncrossed cell to naming each
  distinct absent path once, and excludes the verbs whose shape declares
  `separate_args` — today only scalar `picture_in_picture(main, overlay)`,
  whose two paths arrive as two single-file arguments rather than in one
  carrier. A single-file argument reporting on its own IS AC2's first clause,
  the rendering pinned byte-for-byte against master; requiring those two
  arguments to report jointly would change a message on a verb master already
  guarded, with no blame moving, which is the one thing AC5 forbids. The
  exclusion is declared on the shape rather than applied in the reader, so a
  verb acquiring a second input carrier is held to the claim by default and
  has to be opted out on the record.

## Review

Reviewed 2026-08-08 against PR #65, branch `m62-fanout-input-path-front-door`
at `16f8231`, base `origin/master` at `33b064c` (unmoved since the branch was
cut, so no merge was needed). Every figure below was measured this session.

### Acceptance criteria

- **AC1** — `testthat::test_local(filter = "input-path-front-door")`: 13 tests,
  138 assertions, 0 failures. Two of them walk `tm_namespace_bodies()` and
  assert the front-door wording appears in exactly one body and the
  `ffm_files()` readability wording in exactly two (`ffm_files` and its `ffm`
  alias), so the M63 residual is pinned rather than assumed.
- **AC2** — the one-path rendering was compared byte-for-byte against
  `origin/master` this session by sourcing that ref's `check_file_exists()`
  into an environment (`codec_guard_env("origin/master")`) and diffing the
  message: `identical()` TRUE at `arg = "infile"` and at `arg = "file"`. The
  multi-path branch names every missing path and leads with the count, at two
  missing of two and at one missing of two (tests 3 and 4). Mechanism note: the
  pre-change strings are asserted as literals, not as a `testthat` snapshot —
  a snapshot records itself on first run and so cannot witness a pre-change
  string. The property the criterion states is met and was re-measured against
  the ref; only the word "snapshot" reads narrower than what is in the file.
  **FAILS on its second clause.** `picture_in_picture_batch()` sweeps `main`
  and `overlay` as two independent aborts, so a row missing both names only
  `main` — measured this session against `origin/master`, which named both
  ("Can't find or read 2 input files. Not readable: 'm.mp4' and 'ov.mp4'").
  "names every missing path, not the first" is false for that verb, and was
  true before. Finding F2 below; unticked.
- **AC3** — 16 fan-out verbs derived by the parsed call-node walk; each refuses
  a missing input with `conditionCall()` naming it. Falsifiability re-measured
  this session, not cited: deleting `crop_video_batch`'s
  `check_batch_inputs(jobs)` line turned 2 named tests red ("every fan-out verb
  refuses a missing input at its own front door", 2 failures; "no verb reports
  the missing input from inside the fan-out", 1). Working tree restored.
- **AC4** — 14 scalar verbs derived by the same walk, `concatenate_videos()`
  and `compare_videos()` among them; the grid confirms both now blame
  themselves where `origin/master` blamed `ffm_files()`.
- **AC5** — `data-raw/input-guard-baseline.R` run over `origin/master` and the
  working tree: 404 cells, 362 live. All eight comparison readers empty —
  vacuous 0 on each ref, refusals 0, message regressions 0, blame regressions
  0, lost `call` 0, dead controls 0, uncovered 0. 66 cells' blame moved to the
  verb the user called. **Caveat recorded rather than left implicit:** the
  criterion is a claim about what that procedure measures, and it holds — but
  the cell set does not reach two defects found by inspection. Every cell's
  input column is character, so no cell exercises F1's factor column; and every
  missing path in every cell is the same constant string, so no cell can
  distinguish "names every missing path" from "names the first", nor surface
  F3's duplicate miscount. Widening the cell set on those two axes belongs with
  the fix, or the fix ships unmeasured.
- **AC6** — `input_guard_misordered()` 0 rows over 133 crossed cells: all 42
  cells crossed with a guard above the sweep report that guard, all 91 crossed
  with a guard below it report the missing input. Each is paired with a control
  proving the crossed error live, and `input_guard_dead_controls()` is empty.
- **AC7** — `python3 data-raw/input-guard-mutations.py`: all three CAUGHT.
  Deleting `crop_video_batch`'s call shape made `input_guard_uncovered()`
  report exactly the 10 combinations it owed; re-pointing the `audio_codec`
  contradiction's control at `ffm_batch()`'s `run` guard made
  `input_guard_dead_controls()` report 8 controls, each `reported run_guard`;
  deleting `strip_metadata_batch`'s `ffm_batch()` call edge moved it from the
  walk's fan-out set to its scalar set. Tree restored by the harness.
- **AC8** — D040 appended. It quotes D024's third exclusion verbatim, states
  why D035's licence does not carry and only its shape does (D035 is
  conditioned on a probe whose result enters the compiled command; a file's
  existence never does), gives three conditions and a "what this does not
  license" clause, discloses the existence-vs-readability residual as M63's
  scope, and answers D036's machine-independence argument rather than ignoring
  it.
- **AC9** — `NEWS.md` "Bug fixes" entry, user-facing terms, no milestone
  number; it also discloses the unreadable-input residual. The named test
  behind it is falsifiable — see AC3's re-measured mutation.
- **AC10** — `devtools::document()` leaves `man/` and `NAMESPACE` with no diff;
  `devtools::test()` 4785 pass / 0 fail (4 warnings and 5 skips pre-existing:
  M44 dropped-track warnings, nvenc-absent skips); `devtools::check()` Status
  OK — 0 errors, 0 warnings, 0 notes.

### Consistency gate

`cairn_validate` exit 0, all checks PASS. One advisory: `sizing (split
tripwires)` warns that M62 carries 10 acceptance criteria against a 7
tripwire — noted, not a gate failure, and terminal for this milestone.
Toolchain slot: `document()` no diff; `man/`, `NAMESPACE` and `.Rd` files
regenerate; no README.Rmd change; no `_pkgdown.yml` change needed (no new
exports); `NEWS.md` carries this milestone's user-visible change with no
milestone number; no new top-level files (`data-raw/` is already
`.Rbuildignore`d, confirmed by check() reporting 0 notes); full `check()` clean.
No `DESIGN.md` principle changed, so `cairn_impact` was not run.

### Independent review

Three fresh-context lenses, spawned in parallel, then a separate scorer that
did not generate the findings.

- **[S] prior-review record** — archived `## Review` sections for M57–M61 plus
  a GitHub inline-comment probe (empty, so no thread walk). **Zero findings.**
  Checked specifically for all-or-nothing gating, "some failure" assertions,
  vacuous grid cells, controls that establish nothing, messages moving while
  blame stays put, aborts losing `conditionCall()`, and source-tree-instead-of-
  namespace reads.
- **[S] blame history** — `git log -L` / `git blame` over every touched hunk
  against M41, M48, M54, M57, M58, M59, M61 and D024/D034/D035/D036/D039.
  **Zero findings.** Confirmed the sweep sits above each verb's nvenc check
  without displacing M41's "immediately before `ffm_batch()`" placement, that
  the M58 → M61 tier order is intact beneath it, and that the
  `check_file_exists()` refactor is behaviour-preserving across all 15 existing
  callers.
- **[O] diff bug** — 19 candidate findings, unfiltered as instructed.

**Actioned (scored 80+), all three reproduced independently before recording:**

- **F1 (92) — `segment_video_batch()`'s sweep degrades to an unattributed base
  error.** It is the only fan-out verb whose inline block never coerces
  `jobs$input` to character, so a factor column reaches `file.exists()` raw.
  Measured: branch raises `invalid 'file' argument` with `conditionCall()`
  naming `file.exists(x)`; `origin/master` raised a cli abort, ``  `input` must
  be a character vector naming at least one input file. ``, naming
  `purrr::pmap(jobs, .f, ...)`. Both the message and the blame are worse than
  before. Violates CLAUDE.md's cli-abort rule and D040's own first condition.
  → **fix now**, in the return below.
- **F2 (88) — `picture_in_picture_batch()` hides the missing `overlay`.**
  `R/ffmpeg.R:6328-6329` calls the sweep twice, once per column, so the first
  abort wins. Measured above; falsifies AC2's second clause and the `NEWS.md`
  promise. The fix is one `check_paths_exist()` over both columns with a
  combined `arg`, not two calls. → **fix now**.
- **F3 (82) — duplicated missing paths are counted once per occurrence.**
  `check_paths_exist()` has no `unique()`. Measured: a two-row table sharing
  one bad path reports "names 2 files that do not exist ... 'gone.mp4' and
  'gone.mp4'"; `origin/master` reported "1 input file". On the `inputs`
  list-column `unlist()` flattens rows, so one typo shared by twenty rows reads
  as twenty files. Every sibling guard in the package (`reject_duplicate_
  outputs`, `standardize_video_batch`'s duplicate-input guard) uses `unique()`.
  → **fix now** (below the return floor on its own, folded into this return).

**Logged, below the 80 threshold (16), surfaced not dropped:**

- F11 (78) — `NEWS.md` says `segment_video()` "used to accept a `jobs` table
  naming a missing path"; it takes no `jobs` table and already had
  `check_file_exists()`. And "stopping at the first" describes the first *row*,
  not the first path. Both confirmed. Worth folding into F2's NEWS rewrite.
- F5 (72) — `R/utils.R:27-31`'s header comment still says `ffm_files()` reaches
  the shared abort site, the claim AC1 was amended to retract; D040's "one
  abort site" bullet omits `concatenate_videos()`/`compare_videos()`, the two
  verbs it exists for.
- F4 (68) — cli's default `vec_trunc` elides paths past ~20, so "lists every
  missing path" overclaims in NEWS, D040 and both new roxygen blocks.
- F7 (66) — no reader asserts a `none` cell reports the missing-input error;
  the ordering readers filter `crossing != "none"` and the message reader
  excludes moved-blame cells.
- F8 (65) — two ordering claims written into `crop_video_batch` /
  `anonymize_video_batch` comments (M59 sites 1 and 3) have no declared
  crossing and so no cell.
- F6 (62) — the grid's single constant `absent` path (see the AC5 caveat).
- F9 (58) — `tryCatch(condition =)` records a warning as `kind = "condition"`,
  which the vacuity screen accepts as a refusal; latent, zero today.
- F13 (45) — `tm_callees()` reads only bare-name call heads, so a
  namespace-qualified `ffm_batch()` call would silently shrink the domain.
- F17 (42) — the classifier's `"not exist"` branch wins first in the cascade.
- F14 (40) — `check_batch_inputs()` errors rather than no-ops on an absent
  column; unreachable at all ten current call sites.
- F12 (32) — T3 and the criteria-audit log describe
  `normalize_audio(two_pass = TRUE)` as reaching `ffm_batch`; it does not, and
  there is no coverage gap because the scalar verb guards its input already.
- F18 (32) — `check_paths_exist()`'s `multiple` default branches on received
  length, which its own comment says it must not.
- F16 (28) — the mutation harness edits the shared tree under `try/finally`;
  inherited from `value-guard-mutations.py`.
- F10 (22) — AC2's "snapshot" instrument, already surfaced above.
- F19 (22) — a test name says "readable" where the checker tests existence.
- F15 (15) — `jobs$input` vs the package's "the `input` column of `jobs`"
  spelling; recorded at the implement gate as deliberate.

**Disposition — return floor tripped.** F1 scores 92 on a defect in what the
package does for its users, and F2 demonstrates AC2 failing. Status returns to
`in-progress`; F3 is folded into the same return. Review stops here.

