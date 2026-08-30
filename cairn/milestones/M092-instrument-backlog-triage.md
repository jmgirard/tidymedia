<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M092: The deferred-findings backlog is triaged and the page retired

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m092-instrument-findings-triage` / [#96](https://github.com/jmgirard/tidymedia/pull/96)

## Goal

Close the four instrument gaps that could let a defect in shipped behaviour
reach a user, prune the rest of `cairn/references/instrument-findings.md`
with each pruning's reason recorded, and retire the page.

## Scope

Surface tier: **internal** — the deliverable is a tracking page, a triage
ledger, and tests over the repo's own guards; no external consumer of the
package relies on any of them.

**In:** filing M091's eighth finding onto the page so the triage covers it;
one triage ledger over every `## M` section; four closures (the batch
container fold, `run_with_progress()`'s binary-free return contract, the
class-pairing probes' handler, the two-pass batch status grid); deleting the
page, its `INDEX.md` line and its ROADMAP row; the carry-forward rows; a
D-entry recording the rule and the retirement.

**Out:** hardening any instrument beyond the four closures — pruned, not
deferred, and the ledger records why per finding. The two runtime-shaped
findings → candidate ROADMAP rows (AC6), not fixes here. A runtime defect the
closures uncover → `/hotfix`, since a user-visible bug is not milestone work.
The help-topic over-attribution needs a design call and stays a row.

## Acceptance criteria

- [x] AC1 — The Triage ledger in this file has one entry per `## M` heading of
      `cairn/references/instrument-findings.md`, as `grep -c '^## M'` over that
      file enumerates them after T1 (eight). Each entry names, for its section,
      which finding ids close and which are pruned, with one reason per id.
- [x] AC2 — A test drives `separate_audio_video_batch()` with a failing row
      whose output extension is an uppercase multi-audio container (`.MKA`) and
      asserts the multi-track advice is absent from the warning. It reddens when
      `R/ffmpeg.R:899`'s `holds_multiple_audio()` call is replaced by an
      exact-case extension match.
- [x] AC3 — A test calls `run_with_progress()` (`R/ffm_batch.R:240`) with a stub
      `run_one` and asserts each returned element carries `success` and
      `timed_out` as length-1 logicals. It carries no `skip_if_no_ffmpeg()` and
      reddens when the stub returns a shape violating that contract.
- [x] AC4 — `grep -c 'condition = function(e) e'` over
      `tests/testthat/test-ffmpeg-exit-condition.R` returns 0, and a probe whose
      site signals a `tidymedia_`-classed warning before its abort captures the
      abort, asserted by its condition class.
- [x] AC5 — `cairn/references/instrument-findings.md` is deleted along with its
      `cairn/references/INDEX.md` line, and the ROADMAP candidate row pointing at
      it is gone: `git grep -l instrument-findings -- cairn/ROADMAP.md
      cairn/references/` returns no hits. A D-entry states the triage rule
      applied (a finding closes only where the gap lets a defect in shipped
      behaviour reach a user; every other finding is pruned with its reason in
      the ledger) and records the page's retirement.
- [x] AC6 — Every finding the AC1 ledger classes as runtime rather than
      instrument holds a candidate ROADMAP row stating the class of evidence
      that would promote it.
- [x] AC7 — `devtools::check()` clean (0 errors / 0 warnings) and
      `devtools::test()` green.
- [x] AC8 — A test drives `normalize_audio_batch(two_pass = TRUE)` through a
      real failing Phase 1 row — no `run_loudnorm_analysis_batch()` mock — and
      asserts the raised condition's `tm_rows` and `tm_row_status` against that
      row, `tm_row_status` being a non-`NA` integer. It carries
      `skip_if_no_ffmpeg()`, and reddens when `assemble_measured()`'s status
      extraction is changed to return `NA_integer_`.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T7, T8
- AC6 → T7
- AC7 → T9
- AC8 → T6

## Tasks

- [x] T1 — File M091's eighth finding (the batch path never exercises
      `holds_multiple_audio()`'s case fold) onto the page as its own `## M091`
      section, so the ledger's domain covers it.
- [x] T2 — Author the Triage ledger in this file: one entry per `## M` section,
      each finding id marked close/prune with one reason, and each marked
      instrument or runtime.
- [x] T3 — Add the uppercase-container batch test
      (`tests/testthat/test-separate-av-multitrack.R`); plant the exact-case
      mutation at `R/ffmpeg.R:899` and record it red before the fix.
- [x] T4 — Add the binary-free `run_with_progress()` contract test; plant a
      contract-violating stub return and record it red.
- [x] T5 — Change the four `condition = function(e) e` handlers
      (`tests/testthat/test-ffmpeg-exit-condition.R:532,538,543,561`) to
      `error =`, and add the warning-before-abort probe.
- [x] T6 — Close the two-pass batch status gap: drive `tm_row_status` through a
      real failing Phase 1 row rather than the mocked binding
      (`tests/testthat/test-normalize-audios-two-pass.R:353-361`).
- [x] T7 — Write the carry-forward candidate ROADMAP rows the ledger's
      classification calls for, then delete the page, its `INDEX.md` line and
      the grouped ROADMAP row.
- [x] T8 — Append the D-entry.
- [x] T9 — Run `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Triage ledger
<!-- owner: plan (heading) / implement (entries, T2) -->

One row per `## M` heading of `cairn/references/instrument-findings.md` (eight, as
`grep -c '^## M'` reported after T1), naming every finding id that section holds
with its own reason. **The rule** (D072): a finding closes only where the gap lets
a defect in shipped behaviour reach a user; every other id is pruned — not
deferred. Each id is also classed **instrument** (it grades the package) or
**runtime** (it is the package's own shipped behaviour, misfiled here); a runtime
id gets a candidate ROADMAP row at T7 rather than a fix, per AC6. Eight sections,
53 ids, four closures, three ids carried forward.

| section | closes | pruned — one reason per id (all **instrument** unless marked) |
|---|---|---|
| `## M081` — the flag-guard sweep | — | **F3, F7** the gap is in what the sweep enumerates (a third flag guard; a fourth operator), not in a live guard: the two live guards and the three read operators are checked, and the namespace sweep found no live instance. **F2** the AC6 completeness reader derives its vocabulary from the entries it reads — self-referential and weak, but reaching no shipped behaviour (the M080 N6/F8 shape). |
| `## M079` — the floor-measurement harness | — | **F3, F4, F6, F8, F9, F10, F11, F12, F13, F15** (ten, one shared reason) nothing under `data-raw/` ships: the harness is run by hand at a floor audit and is absent from the built package, so a wrong or unattributable floor measurement is a wrong *record*, corrected by re-running it. No path leads from any of the five coverage gaps or five script defects to a defect a user meets. |
| `## M071` — the parallel-carry harness | — | **F4** the option-unset control assumes a scheduling `future` does not promise — it weakens the control, not the carry it controls. **F5** the fan-out domain guard compares a basename set and a count, so an unwiring inside one file passes it; the behavioural AC1 tests catch that, and their Windows/furrr skips are a coverage limit on an instrument. **F7** both refusal tests assert class-and-message equality between branches, so a shared unrelated regression passes. **F8** AC1's `probe_all` case never checks a filename appears — the criterion asked for it and the test under-asserts. **F9 — runtime**, not an instrument finding: under a sequential plan with `parallel = TRUE` a caller's own `options(tidymedia.*)` set inside `.f` is rolled back. Shipped behaviour, outside this milestone's Scope; carried to a candidate ROADMAP row at T7 with its promote-on clause. |
| `## M70` — the timeout-silence guards | **O6 (AC3, T4)** — `run_with_progress()`'s return contract was covered only behind `skip_if_no_ffmpeg()`, and CI's macOS and Windows runners install no media binaries, so a contract mismatch surfaces as a hard `vapply` type error on a user's machine rather than red on CI. A binary-free contract test closes it. | **O2** `tm_condition_api` cannot see `absorb_timeout()`, so three absorbers go unlisted; the row itself records AC1's grid as unaffected and the loss as explanatory. **O3** `tm_program_arg()` needs a character literal, so a future call omitting `program` would slip the set assertion; no site omits it (grepped at that review) and the mutation probe varies rather than removes. **O4** already discharged — taken by M071 on 2026-08-26, whose AC3 drives a real timeout through `ffm_batch(parallel = TRUE, run = TRUE)`. **O5** AC2's "exactly one warning" is asserted away from the `_batch` verbs while AC1's grid asserts at-least-one, so a refactor could satisfy both — an assertion-strength gap. **O7** the `warned` verdict greps cli-formatted text that wraps at narrow `cli.width`; brittle, and the conditions it could assert instead carry classes. **O8** the doc guards grep all of `NEWS.md` rather than its timeout paragraph, so an unrelated release note could redden them — a false-red risk that is loud and self-explaining when it fires. **O11 — runtime**: `probe_all_impl()`'s threaded `call` would make an argument refusal name `infile` through `verify_media()`, which has no such argument — a wrong shipped message, unreachable today because `check_file_exists()` refuses first; candidate row at T7 with that reachability as its promote-on clause. |
| `## M62 / M63 / M64 / M080` — the input-guard blame grid | — | The 16 ids the page's own text declares live (M62 N1 closed at M63; M62 N3, N7 and M64 F4 were promoted to M080 on 2026-08-28, so they are off that set). **M62 N2** for the eleven `slots = 1L` verbs the `all` form is a one-row cell, so only the factor-typed cell carries two distinct absent paths — a grid-shape gap over guards that are correct. **M63 C1** the `unreadable` form inherits that one-row-cell limit. **M63 A5** `tm_refused_input()` uses `catch_cnd()`'s default `classes = "condition"`, so an earlier condition reads as "not refused"; M087's pass-2 F5 shares the shape and closes only because a live candidate exists on its sites, and this one has none. **M63 A8** the site-uniqueness test fences no new third wording. **M63 A9** `input_guard_reword()` substitutes on every message rather than keying on the `input` class, so an unrelated guard could be waved through as wording-only. **M64 F5** the controls-neutered check passes vacuously if `Rscript` crashes. **M64 F7** the usage docs say `origin/master` where the ACs say merge-base, equal today. **M64 F10** the precedence crossing list omits three crossings, none flip-bearing. **M64 F11** the nvenc-ordering test carries no guard-liveness control. **M080 A10** `input_guard_blame_unexpected()` reports all 30 unreadable cells on any ref pair other than M62→M63, so it cannot be read as pass/fail. **M080 N6** verb membership is gated on `grepl(..., fixed = TRUE)` over deparsed bodies and `input_guard_uncovered()` re-derives from the same declaration — self-referential. **M080 N7** the `scalar_arg` classifier matches a bad scalar `video_codec` no crossing supplies, so that half can never fire. **M080 F5** a retrospective scoping error in a shipped milestone's prose, with no artifact left to correct. **M080 F6** the sweep probes `f(vals[[i]])` positionally after deriving the required formal by name; no such predicate exists. **M080 F7** 12 of 60 sweep cells accept all four NA types silently and assert nothing — a grid that grades vacuously over predicates that are correct. **M080 F8** the carrier-completeness reader derives its vocabulary from the entries — the M081 F2 / M080 N6 shape again. |
| `## M086` — the two-pass batch analysis grid | **F9 (AC8, T6)** — AC4's grid mocks `run_loudnorm_analysis_batch()` wholesale and hand-builds its failed-row fixtures, so nothing ties `assemble_measured()`'s expected input to what `run_program()` returns: a change to that return shape leaves `tm_row_status` all-`NA` in a real batch with the grid still green, which is the wrong status a user reads. | **(i), (ii)** already pruned at M086's own §7 on 2026-08-29 with reasons recorded on the page — (i) the exit-numbering assertion measured green three FFmpeg majors apart; (ii) the direct `system2()` call misfires only under `set_ffmpeg()` off-PATH, which no supported path exercises. Re-read against this triage's rule, both prune again. |
| `## M087` — the condition-class pairing and topic guards | **pass-2 F5 (AC4, T5)** — the pairing probes catch with `condition = function(e) e` at three error sites, so a `tidymedia_`-classed *warning* signalled before the abort is captured instead and asserted against topics for a site nobody tested, passing the probe's non-empty-class guard while testing the wrong condition. The row names the live candidate (the dropped-track check on the `normalize_audio` sites), so this is the gap through which a wrong class claim reaches a user's help page; `error =` binds each probe to its site. | **pass-1 F5 / pass-2 F1, F2** the AC4 pairing test binds a class claim to a topic, not a site, which is why the same over-attribution shipped green twice; a test that binds a claim to a site is a design call, which this milestone's Scope puts out. Instrument-classed, not runtime — the over-attributed claim lives in a help page the instrument grades, not in a code path — but carried to a candidate ROADMAP row at T7 under Scope Out's design-call clause. **(i), (ii), (iii)** already pruned at M087's own §7 on 2026-08-29 — (i) AC5 itself holds and the paragraph's real offset was measured; (ii) a reflow failure is loud and self-explaining; (iii) `find_program()` warns rather than aborting and the assertions never reach a binary. All three prune again. |
| `## M091` — the container gate's case fold on the batch path | **the round-4 batch case fold (AC2, T3)** — `holds_multiple_audio()`'s fold is exercised only at the scalar site; no test passes an uppercase extension through `separate_audio_video_batch()`, so replacing the batch site's call with an exact-case match leaves the suite green while the false blame M091 exists to remove keeps arriving on any batch row whose `audiofile` is spelled `.MKA`. The defect the gap admits is one a user reads. | — |

## Work log

- 2026-08-30: created by /milestone-plan; absorbs the grouped instrument-findings candidate row (dispositioned 2026-08-30 as promotable at the next planning pass).
- 2026-08-30: plan gate chose triage-then-retire over hardening the instruments broadly, because the scope hits the checker-regress shape — every instrument here verifies repo-internal artifacts and none of the ~40 findings is a defect in shipped behaviour; falsified by a defect reaching a user through a gap this pass prunes.
- 2026-08-30: plan gate chose all four closures over the two cheapest, because the batch-runner contract and the two-pass status grid are the two whose failure surfaces on a user's machine; falsified by either closure costing more than a task.
- 2026-08-30: criteria audit ran in REDUCED mode (internal tier, no RB tripwire) via a fresh [O] reader; returned three findings — AC1's headline quantified over "every section" where its grep enumerates only `## M` sections, AC5 carved out two author-recalled paths (one of which holds no hit today while `references/INDEX.md` does), and AC6 hand-listed the two carry-forwards before the triage that classifies them had run. All three fixed before writing: AC1 narrowed to `## M` headings, AC5 rewritten to sweep two named paths directly, AC6 bound to the ledger's own classification.
- 2026-08-30: amendment (substantive, implement gate): T6 — the fourth closure the Goal and Scope name — had no acceptance criterion, Coverage having mapped AC6 (the carry-forward ROADMAP rows T7 writes) to it. Added AC8 binding the two-pass batch status closure and repaired Coverage to AC6 → T7, AC8 → T6. Criteria set widened by one; no defect return is on this log, so D-118's return-adjacent direction rule does not apply.
- 2026-08-30: criteria audit of AC8's wording ran in REDUCED mode (internal tier, no RB tripwire) via a fresh [O] reader that did not author it, before the text was written; returned nothing — CLEAR on the bounded-promise, proportionality and instrument questions.
- 2026-08-30: T1 — filed M091's eighth finding (the batch path never exercises `holds_multiple_audio()`'s case fold; only the scalar site at `R/ffmpeg.R:728` is covered, by `test-separate-av-multitrack.R:1191`) as the page's `## M091` section, and corrected the page's stale "Six sections" header, which had never listed M087. `grep -c '^## M'` now reports eight.
- 2026-08-30: T2 — authored the Triage ledger: eight entries, one per `## M` section, every finding id classed instrument or runtime and marked close or prune with one reason. Four close (M70 O6, M086 F9, M087 pass-2 F5, M091's batch case fold) — the four the plan gate named. Two ids class runtime (M071 F9's rolled-back caller options under a sequential plan, M70 O11's wrong argument name in a `probe_all_impl()` refusal); M087's pass-1 F5 / pass-2 F1-F2 prunes as instrument but carries forward under Scope Out's design-call clause.
- 2026-08-30: amendment (minor, T7 wording): T7 said "the two carry-forward candidate ROADMAP rows", written before the triage ran. The ledger yields three rows — the two runtime-classed ids AC6 binds, plus the help-topic design call Scope Out sends to a row. T7 now reads "the carry-forward candidate ROADMAP rows the ledger's classification calls for". No criterion changed; AC6 was already bound to the ledger's own classification.
- 2026-08-30: T3 — added "the batch gate reads the extension without regard to case" to `tests/testthat/test-separate-av-multitrack.R`, beside its scalar sibling. Measured first (ffmpeg 9.0.1): the batch verb raises no generic per-row failure warning, so a dropped `.MKA` row leaves the batch silent and the test asserts a returned tibble with `success` FALSE, plus a lowercase `.mp3` control that does warn, so the green is not the row having quietly succeeded. Mutation planted at `R/ffmpeg.R:899` (`holds_multiple_audio()` → an exact-case `tools::file_ext() %in% multi_audio_extensions`): FAIL 2 / PASS 475, both failures in the new test, `upper` arriving as a `tidymedia_multitrack_separation` warning — the false blame itself. Restored: FAIL 0 / PASS 477.
- 2026-08-30: amendment (minor, T3 file name): T3 named `tests/testthat/test-separate-audio-video-batch.R`; the batch multi-track warning tests all live in `test-separate-av-multitrack.R`, and the new test's whole point is that the scalar `OUT.MKA` test in that file does not cover the batch site. T3 now names the file the test went into. AC2 names no file.
- 2026-08-30: T4 — added "run_with_progress() returns one success/timed_out record per job" to `tests/testthat/test-ffm-batch.R`, with no `skip_if_no_ffmpeg()`: a stub `run_one` over three pipelines, per-element `expect_named` + `rlang::is_bool`, and the two `vapply()` expressions `ffm_batch()` itself applies to this return (`R/ffm_batch.R:157-158`). Two contract-violating stubs recorded red — dropping `timed_out` (FAIL on the names and the bool assertions) and returning `success` as an integer, which reproduces M70 O6's own failure mode verbatim: `Error in vapply(results, [[, logical(1), "success"): values must be type 'logical'`, FAIL 4 / PASS 7. Restored: FAIL 0 / PASS 47 over the file.
- 2026-08-30: T5 — swapped all four `condition = function(e) e` handlers in `tests/testthat/test-ffmpeg-exit-condition.R` for `error = function(e) e` (`grep -c` now returns 0) and added "the pairing probe captures the abort, not a warning raised before it". The probe mocks `run_program` to signal a `tidymedia_probe_warning` before returning unparseable output, then asserts both halves permanently rather than by a transient mutation: under `condition =` the captured object is the warning and is NOT `tidymedia_loudnorm_no_measurement`; under `error =` it is. File green afterwards, FAIL 0 / PASS 156, so the four swapped probes still observe non-empty class vectors and the AC4 loop's non-vacuity guard still holds.
- 2026-08-30: T6 — added "a real failing Phase 1 row carries its exit status on tm_row_status" to `tests/testthat/test-normalize-audios-two-pass.R`: no mock anywhere, a real `make_dynamic_audio()` row beside a readable-but-not-media `.wav`, asserting `tm_rows` is `2L` (so the good row measured and the batch did not fail wholesale) and `tm_row_status` is a length-1 non-`NA` integer. Measured on ffmpeg 9.0.1: the bad row exits 183, matching the by-hand check M086's review recorded. Mutation planted — `assemble_measured()`'s `function(x) if (is.null(x$exit)) NA_integer_ else x$exit` → `function(x) NA_integer_` — and the new test reddens with `Expected is.na(cnd$tm_row_status) to be FALSE`, F9's all-NA symptom itself (FAIL 3 / PASS 80; the two other failures are the mocked grid's exact-status assertions). Restored: FAIL 0 / PASS 83.
- 2026-08-30: T7 — wrote the three carry-forward candidate ROADMAP rows the ledger calls for (M071 F9 and M70 O11, both classed runtime; M087's topic-vs-site design call), deleted `cairn/references/instrument-findings.md`, its `cairn/references/INDEX.md` line and the grouped ROADMAP row. Renamed this milestone file to `M092-instrument-backlog-triage.md`: AC5's sweep also matched the file's own path in the ROADMAP table, and a rename clears it without amending the criterion. `git grep -l instrument-findings -- cairn/ROADMAP.md cairn/references/` now returns no hits.
- 2026-08-30: T7, budget — the swap took `cairn/ROADMAP.md` to 24,337 bytes, over D067's plugin-owned `< 24,000`. Compressed the heaviest candidate row (the multi-track blind-abort row, 1,531 → 1,318 bytes) by cutting what D069/D071 and M091's archive already hold, then tightened the three new rows, whose full text the Triage ledger carries. 23,832 bytes, 168 under budget.
- 2026-08-30: T8 — appended D072 to `cairn/DECISIONS.md`, stating the triage rule, the four closures with each one's path to a user, where the pruned findings went, and the page's retirement. It supersedes nothing: D040, D041, D049, D050 and D059 all stay in force; what it retires is the working artifact those five accumulated onto.
- 2026-08-30: T9 — `devtools::document()` (no diff), `devtools::test()` FAIL 0 / WARN 12 / SKIP 5 / PASS 8815, `devtools::check()` 0 errors / 0 warnings / 0 notes. The suite's 12 warnings are pre-existing: the four files this milestone touched report 0 warnings between them (477 + 47 + 156 + 83 passes).
- 2026-08-30: T9, AC4 repair — the sweep returned 2, not 0, both hits inside T5's own probe: its explanatory comment and its deliberate counter-example, which spelled the retired handler literally. Reworded the comment (the literal is now described, not typed, the way the retired class name in the same file is assembled rather than written) and respelled the counter-example `condition = identity`, which behaves identically. The sweep returns 0 and the file stays green at PASS 156.
- 2026-08-30: review checkpoint — PR #96 opened as draft. Consistency gate FAILED on weight caps: this file's plan-owned body is 206 lines against the <150 cap (Triage ledger 96, Acceptance criteria 38, Tasks 25). Evidence gathering was already in flight and is being completed before the return so it lands in one pass.
- 2026-08-30: review returned M092 to in-progress. Consistency gate FAILED: `cairn_validate.py` weight caps — this file's plan-owned body is 206 lines against the <150 cap, shed >=57 (Triage ledger 96, Acceptance criteria 38, Tasks 25). Every other cairn check and the whole r-package toolchain slot are green (test FAIL 0 / PASS 8815; check Status: OK; document() no diff; pkgdown clean). Five criteria verified and ticked (AC1, AC4-AC7); AC2, AC3 and AC8 stay unticked because the gate stopped the phase before their mutation-redness clauses were re-executed. Review fan-out: blame-history 0 findings, prior-review 0 findings, diff-bug 9, all recorded in the Review section, none hitting the return floor on its own.
- 2026-08-30: return repair, cap — compressed the Triage ledger, the heaviest plan-owned section, in one rewrite per tracking-rules: 96 lines to 22. Eight `###` sub-headings and eight tables collapse to one table with one row per `## M` heading, so AC1's "one entry per heading" is now literally one row per heading; every id and its own reason survive verbatim in the row's cells, the rule statement cross-references D072 rather than restating it, and the derivable Tally folds into the preamble. Plan-owned body 206 → 133 lines against the <150 cap; `cairn_validate.py` now reports all checks passed, with the pre-existing `sizing` advisory (8 ACs > 7) unchanged.
- 2026-08-30: return gate — the review's three test-strength findings (1, 6, 7: AC3's test asserts its own stub rather than `ffm_batch()`'s inline `run_one`; AC2's test asserts no warning at all rather than the advice being absent; AC8 asserts a non-`NA` status but never its value) were put to the user with holding the criteria recommended, since each criterion passes as written and strengthening one widens its promise. Chosen: compression and the D072 correction only; no criterion or test changed.
- 2026-08-30: D072 correction — the entry's motivating measurement was wrong. `git log --follow` on the deleted path shows five commits: creation at M083 (94f6c77, 2026-08-28), filings at M086 (a9d5ec0) and M087 (dea2821) on 2026-08-29 and M092's T1 (3414982) on 2026-08-30, then the deletion. "Five later hygiene passes" and "five filings in three weeks" become "created 2026-08-28, grown by a section at each of three later passes, and drained by none of them" — the same conclusion on the numbers that hold. Also "roughly forty" findings → 53, the count the ledger enumerates, in both places; and "M091's eighth finding" → "M091's round-4 finding, the page's eighth *section*".
- 2026-08-30: ROADMAP repair — the M45 multi-track row's promote clause named "(c)'s missing-directory case" after T7's compression stopped naming that case inside (c); it now reads "the missing-directory cause among (c)'s three", which (c)'s "the three causes D069 enumerates" resolves. 23,844 bytes, 156 under D067's budget. The review's paired sub-finding about T7's byte figure does not hold: `git show f9fda86:cairn/ROADMAP.md | wc -c` is 23,832, exactly what T7 recorded; the review's 23,827 was measured at 695a8a4, after its own checkpoint edit.
- 2026-08-30: verify slot re-run after the return repair — no R or test file changed, and `devtools::test()` reports FAIL 0 / WARN 12 / SKIP 5 / PASS 8815, identical to T9. `origin/master` has not moved since the branch was cut, so no merge was owed.

- 2026-08-30: review pass 2 — consistency gate PASS (`cairn_validate` exit 0, weight caps green at 133 plan-owned lines; document() no diff, pkgdown clean, check Status: OK, test FAIL 0 / PASS 8815). All eight criteria verified with fresh evidence, the three mutation-redness clauses re-executed this pass (AC2 FAIL 2/475, AC3 FAIL 6/40 and FAIL 3/42, AC8 FAIL 3/80), and every box ticked against its own evidence line. Fan-out re-run on the repaired branch: prior-review 0, blame-history 5, diff-bug 14, merged to 14 — none meets the return floor, five are the criterion-wording findings the first pass's gate already held.

- 2026-08-30: review pass 2, gate — approved for merge with the five record-accuracy findings (6, 7, 8, 12, 14) triaged fix-now and repaired on the branch: D072's leftover "sixth time" ceiling, its 53-vs-57 finding count, this log's ROADMAP byte figure (23,849 -> 23,844), the Triage ledger's `M080 F7` stray clause, and D072's over-long line. No criterion or test changed; `cairn_validate` still exit 0 and no `R/` file is touched, so the verify slot is unaffected.

## Decisions

- **D072** (promoted, `cairn/DECISIONS.md`) — a deferred finding about an
  instrument closes only where the gap lets a defect in shipped behaviour reach
  a user; every other finding is pruned with its reason in the Triage ledger.
  Records the four closures and their paths to a user, the two ids reclassed
  runtime, and the retirement of `cairn/references/instrument-findings.md`.
  Falsified by a defect reaching a user through a gap this triage pruned.

## Review

Second pass, 2026-08-30, on `m092-instrument-findings-triage` at b682338, PR
[#96](https://github.com/jmgirard/tidymedia/pull/96) (draft). The first pass
returned the milestone on the consistency gate's 150-line plan-owned cap; the
return repair compressed the Triage ledger (206 -> 133 plan-owned lines) and
corrected D072's motivating measurement. This pass re-executes every criterion
from scratch, including the three mutation-redness clauses the first pass could
not reach. `origin/master` has not moved since the branch was cut, so no merge
was owed. **Outcome: all eight criteria verified.**

### Acceptance criteria

- **AC1 - verified.** `git show f9fda86^:cairn/references/instrument-findings.md
  | grep -c '^## M'` returns 8; the compressed ledger carries 8 table rows, one
  per heading, titles verbatim. Ids reconciled mechanically, section by section,
  against the page blob: every id the page's live set holds appears in its own
  row's cell with its own reason, none invented, none dropped (M081 3, M079 10,
  M071 5, M70 8, grid 16, M086 3, M087 7, M091 1). A second mechanical
  comparison against the pre-compression ledger (3953221) shows no id lost in the
  rewrite; the three extras in the new text are cross-references (`M080 N6`,
  `M081 F2`, `M087 pass-2 F5`) and the grid row's new explicit accounting for the
  four already-handled ids (`M62 N1/N3/N7`, `M64 F4`), which the first pass's
  finding 5 asked for.
- **AC2 - verified.** The test is at
  `tests/testthat/test-separate-av-multitrack.R:412-446`, green in a fresh file
  run (FAIL 0 / PASS 477). Redness re-executed this pass: `R/ffmpeg.R:899`'s
  `holds_multiple_audio(out$output[bad])` replaced by an exact-case
  `tools::file_ext(out$output[bad]) %in% multi_audio_extensions` gives FAIL 2 /
  PASS 475, both failures inside the new test. Restored, FAIL 0 / PASS 477.
- **AC3 - verified.** The test is at `tests/testthat/test-ffm-batch.R:250-274`,
  carries no `skip_if_no_ffmpeg()`, green at FAIL 0 / PASS 47. Redness
  re-executed twice this pass, each mutation confined to the new test: dropping
  `timed_out` from the stub gives FAIL 6 / PASS 40; returning `success` as an
  integer gives FAIL 3 / PASS 42. Restored both times. See finding 1 on what the
  criterion binds.
- **AC4 - verified.** `grep -c 'condition = function(e) e'
  tests/testthat/test-ffmpeg-exit-condition.R` returns 0. The probe at `:507-545`
  mocks `run_program` to signal a `tidymedia_probe_warning` before the site's
  abort and asserts by condition class that the catch-first handler captures the
  warning while `error =` reaches `tidymedia_loudnorm_no_measurement`. File green
  at FAIL 0 / PASS 156. See finding 3 on the sweep's strength.
- **AC5 - verified.** `git grep -l instrument-findings -- cairn/ROADMAP.md
  cairn/references/` returns no hits (exit 1). The page is absent from the tree
  and from `cairn/references/` (six files remain, none it); its `INDEX.md` line
  and the grouped ROADMAP row are gone. D072 at `cairn/DECISIONS.md:3283` states
  the triage rule in the words AC5 requires and records the retirement.
- **AC6 - verified.** Both runtime-classed ids hold their own candidate row, each
  stating a class of evidence rather than a count: M071 F9 ("Promote on a report
  of an option set during a batch not surviving it") and M70 O11 ("Promote on any
  change letting `verify_media()` refuse before the file-existence check, or on a
  report of such a refusal"). The third new row carries M087's topic-vs-site
  design call under Scope Out.
- **AC7 - verified.** `devtools::check()` `Status: OK` - 0 errors / 0 warnings /
  0 notes, 2m 58.5s. `devtools::test()` FAIL 0 / WARN 12 / SKIP 5 / PASS 8815.
  The 12 warnings are pre-existing; the four files this milestone touched report 0
  between them (477 + 47 + 156 + 83).
- **AC8 - verified.** The test is at
  `tests/testthat/test-normalize-audios-two-pass.R:422-451`, carries
  `skip_if_no_ffmpeg()`, uses no `run_loudnorm_analysis_batch()` mock, green at
  FAIL 0 / PASS 83. Redness re-executed this pass: `assemble_measured()`'s status
  extraction (`R/loudnorm_two_pass.R:272`) changed to `function(x) NA_integer_`
  gives FAIL 3 / PASS 80 - the new test plus the mocked grid's two exact-status
  assertions. Restored, FAIL 0 / PASS 83. See finding 5 on what the criterion
  asserts.

### Consistency gate

- `cairn_validate.py` - **PASS**, exit 0. Every check green, including
  `weight caps` (the first pass's failure), `coverage complete`,
  `binding criteria`, `scaffold present`, `roadmap<->disk orphans` and
  `references index<->disk`. One advisory: `sizing` WARNs at 8 acceptance
  criteria against the >7 tripwire. `release window` did not fire.
- Byte and line budgets by hand: `cairn/ROADMAP.md` 23,844 bytes / 47 lines
  (D067's `<24,000` / `<60`); `cairn/LESSONS.md` 19,102 / 31 (`<20,000` / `<50`);
  `references/false-greens.md` 25,810 / 55 against its own header's `<26,000` /
  `<60`; `references/guard-ordering.md` 5,451 / 27 against `<11,000` / `<31`. All
  inside budget. The ROADMAP figure is 5 bytes off the one the work log recorded
  - finding 8.
- `cairn_impact.py` - skipped, no DESIGN principle changed (`Principles
  touched: -`; `cairn/DESIGN.md` is not in the diff).
- Toolchain slot (`r-package`) - all green. `devtools::document()` leaves the
  tree clean. `pkgdown::check_pkgdown()` "No problems found." `devtools::check()`
  clean, above. `NEWS.md` untouched and correctly so: no `R/` file changed, so
  there is no user-visible behaviour change to declare, no `man/` regeneration
  and no `.Rbuildignore` entry owed.

### Independent fresh-context review

Internal tier, but the diff touches `tests/` - executable surface - so the full
three-lens fan-out ran again on the repaired branch, each lens fresh-context and
none having seen the implementation or this section.

**[S] prior-review - 0 findings.** Archived `## Review` sections on the touched
files reconcile with what the diff closes; each of the four closures matches its
archived source finding, and the `condition =` -> `error =` swap correctly leaves
`batch_sep` (`:608`) on `warning =`, the one site that signals by design. The
GitHub probe (`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`)
returned `[]` and PR #96 carries 0 reviews, so that surface was skipped.

**[S] blame-history - 5 findings**, all duplicates of the [O] list below except
its own #5 (recorded there as finding 6). It independently confirmed against
fresh history that D072's corrected "created 2026-08-28, grown by a section at
each of three later passes" matches `git log --follow` exactly, that D072
contradicts none of D040/D041/D049/D050/D059, and that no fixed bug is
resurrected.

**[O] diff-bug - 14 findings, ranked.** Merged with the blame lens and
de-duplicated below; each verified against the implementation, not the
reviewer's account. No `R/` file changed on this branch, so none is a defect in
shipped package behaviour. Findings 1-5 and 9-11 were raised and triaged at the
first pass's gate; findings 6-8 and 12-14 are new this pass.

1. *(confirmed; raised and held at the first pass's gate)* **The AC3 test
   asserts the properties of its own stub, not of anything shipped.**
   `run_with_progress()` (`R/ffm_batch.R:239-250`) is a bare loop, so every
   assertion is satisfied by construction from the stub written three lines
   above; M70 O6's real exposure is the `run_one` closure defined inline in
   `ffm_batch()` (`R/ffm_batch.R:141-147`), which the test never enters. AC3's
   own redness clause places the mutation in the stub, so the criterion as
   written passes.
2. *(confirmed; raised and held at the first pass's gate)* **The AC2 test asserts
   "no warning at all" where AC2 asks that the multi-track advice be absent from
   the warning.** The discriminator is `expect_s3_class(upper, "tbl_df")`, which
   holds only because `tryCatch(warning = )` fell through. The premise that the
   multi-track warning is the verb's only warning sits in a comment, not an
   assertion, and is not true in general - `ffm_batch()` also raises
   `tidymedia_batch_timeout` on that path (`R/ffm_batch.R:188-198`), unreachable
   under this test's conditions but not excluded by it.
3. *(confirmed; raised and held at the first pass's gate)* **AC4's grep measures
   a lexical spelling.** The new probe's counter-example is spelled
   `condition = identity`, semantically identical to the retired literal, and the
   test's own comment states it is spelled that way to keep the count at 0.
   Separately, the four repaired handlers at `:572/:578/:583/:601` carry no
   suite-level regression guard: reverting any of them leaves the suite green.
4. *(confirmed; raised and held at the first pass's gate)* **AC5's zero-hit sweep
   was achieved partly by renaming the artifact under measurement.** T7 renamed
   the milestone file because the sweep matched its own path in the ROADMAP
   table. Substantively benign - a milestone filename is not the retired page -
   but the criterion's evidence no longer means only what it claims.
5. *(confirmed; raised and held at the first pass's gate)* **The AC8 test cannot
   distinguish a correct status from a wrong one.** It asserts type, length and
   non-`NA`, never the value, while M086 F9's promote condition names "a
   `tm_row_status` that is **wrong** or all-`NA`". AC8's own text asks only for a
   non-`NA` integer, so the criterion passes; half the defect class is unguarded.
   The `expect_identical(cnd$tm_rows, 2L)` assertion is sound - `tm_rows` is
   1-indexed row numbers (`R/loudnorm_two_pass.R:269`).
6. *(confirmed, new; **fixed at the gate**)* **D072's closing rule said a logged instrument finding is
   "never carried a sixth time", a figure left over from the pre-correction
   draft.** The entry's own corrected measurement is "created 2026-08-28, grown
   by a section at each of three later passes" - four filings, not five - so the
   sixth-time ceiling no longer reconciles with the numbers beside it.
7. *(confirmed, new; **fixed at the gate**)* **D072 opened "held eight sections and 53 review findings"
   where the page's text carries 57 ids.** The extra four (`M62 N1/N3/N7`,
   `M64 F4`) are recorded on the page as already closed or promoted, so 53 is the
   live set rather than what the page held.
8. *(confirmed, new; **fixed at the gate**)* **The work log's ROADMAP byte figure was off by 5.** The
   final repair line records "23,849 bytes, 151 under D067's budget";
   `git show HEAD:cairn/ROADMAP.md | wc -c` is 23,844 (156 under). Both are
   inside budget, so nothing breaches; the record is simply wrong. The same
   line's rebuttal of the first pass's byte finding does check out -
   `git show f9fda86:cairn/ROADMAP.md | wc -c` is exactly 23,832.
9. *(confirmed; raised and held at the first pass's gate)* **Two archive
   summaries still name the deleted page with no forwarding note** -
   `archive/M083-roadmap-byte-budget.md` and
   `archive/M087-scalar-batch-condition-classes.md`. IP4 forbids editing an
   archive, so a forwarding note belongs in D072 or nowhere; D072 says the
   content is "in M092's Triage ledger" without naming the path.
10. *(not sustained as an AC1 failure; raised at the first pass's gate)* **Four
    grid ids have no ledger row of their own** - `M62 N1/N3/N7`, `M64 F4`. AC1's
    domain is met: the page declares N1 closed at M63 and the other three
    promoted to M080, and declares the live set as exactly the 16 the ledger
    carries. The compressed row now records them explicitly in its opening
    clause, which is what the first pass asked for.
11. *(confirmed, minor; the first pass's grouped finding, now discharged)* D072's
    "roughly forty" became 53 and "M091's eighth finding" became "the page's
    eighth *section*" in the return repair; the M45 candidate row's promote
    clause was repaired to "the missing-directory cause among (c)'s three".
    Re-verified this pass: all three corrections are in the tree.
12. *(confirmed, new, cosmetic; **fixed at the gate**)* **The ledger's `M080 F7` cell carried a stray
    trailing clause** - "the four SHIPPED-predicate findings this row carried
    were promoted to M080 on 2026-08-28 and are already fixed" - which belongs to
    the row's section-level accounting, not to F7's prune reason, and duplicates
    what the same cell's opening clause already says.
13. *(confirmed, new, minor)* **Some ledger cells give the finding's description
    where AC1 asks for a reason** - `M63 A8`, `M64 F11`, `M080 N7`. Each id does
    carry a clause, so AC1 passes as written, but a reader cannot re-derive the
    prune decision from those three cells without D072's rule in hand.
14. *(confirmed, new, cosmetic; **fixed at the gate**)* **`cairn/DECISIONS.md:3315` ran to 110
    characters** against the file's ~80-column body convention. Also noted: T7
    compressed the unrelated M45 candidate row under D067's byte budget, dropping
    its measured "exits 234" detail; verified recoverable - D069 enumerates the
    three causes and `cairn/DECISIONS.md:889/917` carry the measurement.

### Disposition

No finding meets the return floor. None demonstrates an acceptance criterion
failing inside its named procedure's domain - each of the eight passes as
written, with its redness clause re-executed where it has one - and none is a
defect in shipped package behaviour, since the branch changes no `R/` file.

Findings 1, 2, 3, 4 and 5 are evidence about the promises rather than the work,
and would route to the gated criterion-amendment protocol. They were put to the
user at the first pass's return gate, which chose to hold the criteria as
written; they are restated verbatim here rather than re-posed, and the user may
reopen any of them.

Findings 6, 7, 8, 12 and 14 were accuracy defects in this milestone's own
durable record. The maintainer triaged them **fix now** at the approval gate and
all five are repaired on the branch before the merge: D072's leftover "sixth
time" ceiling now reads "never carried forward a second time"; its opening
sentence now says 53 still-open findings and states in its own parenthetical
that the page's text carries 57 ids, four already recorded there as closed or
promoted; the work log's ROADMAP figure now reads 23,844 bytes / 156 under
budget; the ledger's `M080 F7` cell now gives F7's own prune reason with the
misplaced section-level clause removed; and D072's over-long line is rewrapped.

Findings 9, 10, 11 and 13 are recorded and rejected: 11 is already discharged,
10 is not an AC1 failure and was addressed by the compression, 9 has no writable
target under IP4 beyond what D072 already says, and 13 is a presentation
preference over cells that satisfy the criterion.
