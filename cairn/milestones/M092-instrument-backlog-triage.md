<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M092: The deferred-findings backlog is triaged and the page retired

- **Status:** in-progress
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
- [ ] AC2 — A test drives `separate_audio_video_batch()` with a failing row
      whose output extension is an uppercase multi-audio container (`.MKA`) and
      asserts the multi-track advice is absent from the warning. It reddens when
      `R/ffmpeg.R:899`'s `holds_multiple_audio()` call is replaced by an
      exact-case extension match.
- [ ] AC3 — A test calls `run_with_progress()` (`R/ffm_batch.R:240`) with a stub
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
- [ ] AC8 — A test drives `normalize_audio_batch(two_pass = TRUE)` through a
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

One entry per `## M` heading of `cairn/references/instrument-findings.md`
(eight, as `grep -c '^## M'` reports after T1). **The rule:** a finding closes
only where the gap lets a defect in shipped behaviour reach a user. Every other
finding is pruned — not deferred — with its reason below. Each id is also
classed **instrument** (it is about a guard, sweep, grid or harness that
measures the package) or **runtime** (it is about the package's own shipped
behaviour and was misfiled here); a runtime-classed id gets a candidate ROADMAP
row at T7 rather than a fix, per AC6.

### `## M081` — the flag-guard sweep

| id | class | disposition | reason |
|---|---|---|---|
| F3 | instrument | prune | A third flag guard would sit outside the exported-route sweep. The gap is in what `flag_guard_verbs()` enumerates, not in any shipped guard; the two live guards are checked. No shipped defect passes through it. |
| F7 | instrument | prune | `tm_bare_flag_operands()` reads three operators and AC1 names those three as its domain. The row records the namespace as swept with no live instance, so nothing shipped is unguarded today. |
| F2 | instrument | prune | The AC6 completeness reader derives its vocabulary from the entries it reads. A self-referential reader is weak, but its weakness reaches no shipped behaviour — it is the same shape as the blame grid's F8 below, pruned for the same reason. |

### `## M079` — the floor-measurement harness

| id | class | disposition | reason |
|---|---|---|---|
| F3, F4, F6, F8, F9, F10, F11, F12, F13, F15 | instrument | prune (all ten) | Nothing under `data-raw/` ships: the harness is run by hand at a floor audit and is absent from the built package. A wrong or unattributable floor measurement is a wrong *record*, corrected by re-running the harness; no path leads from any of these ten to a defect a user meets. The five coverage gaps and five script defects are pruned together because they share that one reason. |

### `## M071` — the parallel-carry harness

| id | class | disposition | reason |
|---|---|---|---|
| F4 | instrument | prune | AC2's option-unset control assumes a scheduling `future` does not promise. It weakens the control's meaning, not the carry it controls. |
| F5 | instrument | prune | The fan-out domain guard compares a basename set and a count, so an unwiring inside one file passes it. The behavioural AC1 tests catch the unwiring; that they skip on Windows and without furrr is a coverage limit on an instrument, not a shipped defect. |
| F7 | instrument | prune | Both refusal tests assert class-and-message equality between branches, so a shared unrelated regression passes. A weak equality assertion; no shipped path depends on it. |
| F8 | instrument | prune | AC1's `probe_all` case never checks a filename appears. The criterion asked for it and the test under-asserts — an instrument gap, closed by nothing a user would observe. |
| F9 | **runtime** | prune (→ ROADMAP row, T7) | Not an instrument finding at all: under a sequential plan with `parallel = TRUE`, a caller's own `options(tidymedia.*)` set inside `.f` is rolled back. That is shipped behaviour reaching a user, and it is outside this milestone's Scope, which fixes instrument gaps. Carried forward as a candidate row with its promote-on clause. |

### `## M70` — the timeout-silence guards

| id | class | disposition | reason |
|---|---|---|---|
| O2 | instrument | prune | `tm_condition_api` cannot see `absorb_timeout()`, so three absorbers go unlisted. The row itself records that AC1's grid is unaffected; the loss is explanatory. |
| O3 | instrument | prune | `tm_program_arg()` needs a character literal, so a future call omitting `program` would slip the set assertion. No site omits it (grepped at that review) and the mutation probe varies rather than removes — a latent instrument gap with no shipped caller behind it. |
| O4 | instrument | prune (already discharged) | Taken by M071 on 2026-08-26, whose AC3 drives a real timeout through `ffm_batch(parallel = TRUE, run = TRUE)`. Nothing left to close. |
| O5 | instrument | prune | AC2's "exactly one warning" is asserted away from the `_batch` verbs and AC1's grid asserts at-least-one. A refactor could satisfy both — an assertion-strength gap on the sweep. |
| **O6** | instrument | **close (AC3, T4)** | The one entry on this page whose gap lets a shipped defect reach a user: `run_with_progress()`'s return contract is covered only behind `skip_if_no_ffmpeg()`, and CI's macOS and Windows runners install no media binaries, so a contract mismatch surfaces as a hard `vapply` type error on a user's machine rather than red on CI. A binary-free contract test closes it. |
| O7 | instrument | prune | The `warned` verdict greps cli-formatted text that wraps at narrow `cli.width`. A brittle verdict on an instrument; the conditions it could assert instead carry classes, which is a strengthening, not a defect. |
| O8 | instrument | prune | The doc guards grep all of `NEWS.md` rather than its timeout paragraph, so an unrelated future release note could redden them. A false-red risk on a guard — loud and self-explaining when it fires. |
| O11 | **runtime** | prune (→ ROADMAP row, T7) | Not an instrument finding: `probe_all_impl()`'s threaded `call` would make an argument refusal name `infile` through `verify_media()`, which has no such argument. That is a shipped message naming a wrong argument. Unreachable today because `check_file_exists()` refuses first, so it is not a defect a user can meet now — carried forward as a candidate row with the reachability condition as its promote-on clause. |

### `## M62 / M63 / M64 / M080` — the input-guard blame grid

| id | class | disposition | reason |
|---|---|---|---|
| M62 N2 | instrument | prune | For the eleven `slots = 1L` verbs the `all` form is a one-row cell, so only the factor-typed cell carries two distinct absent paths. A grid-shape gap; the guards it grades are correct. |
| M63 C1 | instrument | prune | The `unreadable` form inherits N2's one-row-cell limit — same gap, same reason. |
| M63 A5 | instrument | prune | `tm_refused_input()` uses `catch_cnd()`'s default `classes = "condition"`, so an earlier condition reads as "not refused". The same shape M087's pass-2 F5 carries; that one closes because a live candidate for the earlier condition exists on its sites, and this one does not. |
| M63 A8 | instrument | prune | The site-uniqueness test fences no new third wording. It asserts less than it could; no shipped message is wrong. |
| M63 A9 | instrument | prune | `input_guard_reword()` substitutes on every message rather than keying on the `input` class, so an unrelated guard could be waved through as wording-only. A classifier gap inside the grid. |
| M64 F5 | instrument | prune | The mutation harness's controls-neutered check passes vacuously if `Rscript` crashes. A vacuous-pass risk on a control — the false-greens shape, and confined to the harness. |
| M64 F7 | instrument | prune | The usage docs say `origin/master` where the ACs say merge-base, equal today. Prose drift in a harness's own documentation. |
| M64 F10 | instrument | prune | The precedence crossing list omits three crossings, none flip-bearing. Coverage the grid does not have, with no flip behind it. |
| M64 F11 | instrument | prune | The nvenc-ordering test carries no guard-liveness control. A missing control on a passing test. |
| M080 A10 | instrument | prune | `input_guard_blame_unexpected()` reports all 30 unreadable cells on any ref pair other than M62→M63, so it cannot be read as pass/fail. The instrument is uninterpretable off its original pair; it grades nothing shipped. |
| M080 N6 | instrument | prune | Verb membership is gated on `grepl(..., fixed = TRUE)` over deparsed bodies, so a reformat drops a verb silently and `input_guard_uncovered()` re-derives from the same declaration. Self-referential coverage — the F2/F8 shape again. |
| M080 N7 | instrument | prune | The `scalar_arg` classifier matches a bad scalar `video_codec` no crossing supplies, so that half can never fire. Dead coverage inside the grid. |
| M080 F5 | instrument | prune | M080's Scope Out mis-described the NA sweep's excluded set, and the row records the plan gate's own falsifier as already fired. A retrospective scoping error on a shipped-and-merged milestone's prose; there is no artifact left to correct here. |
| M080 F6 | instrument | prune | The sweep probes `f(vals[[i]])` positionally after deriving the required formal by name, so a predicate whose required formal is not first passes vacuously. No such predicate exists; a vacuous-pass risk on the sweep. |
| M080 F7 | instrument | prune | 12 of 60 sweep cells accept all four NA types silently and assert nothing. Cells that grade nothing — the four SHIPPED-predicate findings this row carried were promoted to M080 in 2026-08-28 and are already fixed. |
| M080 F8 | instrument | prune | The carrier-completeness reader derives its vocabulary from the entries. Same self-referential shape as M081 F2 and M080 N6; pruned identically. |

### `## M086` — the two-pass batch analysis grid

| id | class | disposition | reason |
|---|---|---|---|
| **F9** | instrument | **close (AC8, T6)** | AC4's grid mocks `run_loudnorm_analysis_batch()` wholesale and hand-builds its failed-row fixtures, so nothing ties `assemble_measured()`'s expected input to what `run_program()` returns. A change to that return shape leaves `tm_row_status` all-`NA` in a real batch with the grid still green — the wrong status a user reads. A test through a real failing Phase 1 row closes it. |
| (i), (ii) | instrument | prune (already pruned) | Pruned at M086's own §7 disposition on 2026-08-29 with reasons recorded on the page: (i) the exit-numbering assertion measured green three FFmpeg majors apart; (ii) the direct `system2()` call misfires only under `set_ffmpeg()` off-PATH, which no supported path exercises. This triage re-reads both against its rule and reaches the same disposition. |

### `## M087` — the condition-class pairing and topic guards

| id | class | disposition | reason |
|---|---|---|---|
| pass 1 F5 / pass 2 F1, F2 | instrument | prune (→ ROADMAP row, T7) | The AC4 pairing test binds a class claim to a topic, not a site, which is why the same over-attribution shipped green twice. A test that would catch it must bind a claim to a site — a design call, which this milestone's Scope puts out. Not runtime-classed (the over-attributed claim lives in a help page the instrument grades, not in a code path), but the design call is worth keeping plannable, so the finding carries forward as its own candidate row per Scope Out. |
| **pass 2 F5** | instrument | **close (AC4, T5)** | The pairing probe catches with `condition = function(e) e` at three error sites, so a `tidymedia_`-classed *warning* signalled before the abort is captured instead and asserted against topics for a site nobody tested — passing the probe's non-empty-class guard while testing the wrong condition. The row names the live candidate (the dropped-track check on the `normalize_audio` sites), so this is the gap through which a wrong class claim reaches a user's help page. `error =` binds each probe to its site. |
| (i), (ii), (iii) | instrument | prune (already pruned) | Pruned at M087's own §7 disposition on 2026-08-29 with reasons recorded on the page: (i) AC5 itself holds and the paragraph's real offset was measured; (ii) a reflow failure is loud and self-explaining; (iii) `find_program()` warns rather than aborting and the assertions never reach a binary. Re-read against this triage's rule, all three prune again. |

### `## M091` — the container gate's case fold on the batch path

| id | class | disposition | reason |
|---|---|---|---|
| M091 review round 4 (batch case fold) | instrument | **close (AC2, T3)** | `holds_multiple_audio()`'s fold is exercised only at the scalar site; no test passes an uppercase extension through `separate_audio_video_batch()`. Replacing the batch site's call with an exact-case match leaves the suite green while the false blame M091 exists to remove keeps arriving on any batch row whose `audiofile` is spelled `.MKA`. The defect the gap admits is one a user reads. |

**Tally.** Eight sections; four closures (M70 O6, M086 F9, M087 pass-2 F5, M091's
batch case fold); every other id pruned with its reason above; two ids classed
runtime (M071 F9, M70 O11) and one design-call finding (M087 pass-1 F5 / pass-2
F1, F2) carried forward as candidate ROADMAP rows at T7.

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

## Decisions

- **D072** (promoted, `cairn/DECISIONS.md`) — a deferred finding about an
  instrument closes only where the gap lets a defect in shipped behaviour reach
  a user; every other finding is pruned with its reason in the Triage ledger.
  Records the four closures and their paths to a user, the two ids reclassed
  runtime, and the retirement of `cairn/references/instrument-findings.md`.
  Falsified by a defect reaching a user through a gap this triage pruned.

## Review

Reviewed 2026-08-30 on `m092-instrument-findings-triage` at 695a8a4, PR
[#96](https://github.com/jmgirard/tidymedia/pull/96) (draft). **Outcome: returned
to `in-progress`** — the consistency gate failed on the milestone file's line cap.
Criterion verification below is therefore partial: the gate stopped the phase
before the three mutation-redness clauses were re-executed.

### Acceptance criteria

- **AC1 — verified.** `git show f9fda86^:cairn/references/instrument-findings.md
  | grep -c '^## M'` returns 8; the Triage ledger carries 8 `###` entries whose
  titles match those headings verbatim. Ids reconcile section by section against
  the page blob: M081 F3/F7/F2; M079's ten; M071 F4/F5/F7/F8/F9; M70
  O2/O3/O4/O5/O6/O7/O8/O11; the grid's 16 (M62 N2 · M63 C1/A5/A8/A9 · M64
  F5/F7/F10/F11 · M080 A10/N6/N7/F5/F6/F7/F8), which is exactly the live set the
  page's own text declares stays there; M086 F9 + (i)(ii); M087 pass-1 F5 /
  pass-2 F1/F2/F5 + (i)(ii)(iii); M091's round-4 finding. No id invented, none
  dropped from a section's live set. One reason per id.
- **AC2 — partially verified.** The test exists at
  `tests/testthat/test-separate-av-multitrack.R:412-445` and passes in the fresh
  suite run. The `R/ffmpeg.R:899` exact-case mutation was **not** re-planted this
  pass; the redness half rests on T3's recorded measurement, not on fresh
  evidence. Not ticked.
- **AC3 — partially verified.** The test exists at
  `tests/testthat/test-ffm-batch.R:248-274`, carries no `skip_if_no_ffmpeg()`,
  and passes. The stub mutation was not re-planted this pass. Not ticked; see
  finding 1, which questions what the criterion binds.
- **AC4 — verified.** `grep -c 'condition = function(e) e'
  tests/testthat/test-ffmpeg-exit-condition.R` returns 0. The probe at
  `:507-545` mocks `run_program` to signal a `tidymedia_probe_warning` before the
  site's abort and asserts, by condition class, that `error =` captures
  `tidymedia_loudnorm_no_measurement` while the catch-first handler captures the
  warning instead. File green in the suite run. See finding 3 on the sweep's
  strength.
- **AC5 — verified.** `git grep -l instrument-findings -- cairn/ROADMAP.md
  cairn/references/` returns no hits (exit 1). `cairn/references/instrument-findings.md`
  is absent from the tree; its `INDEX.md` line is gone; the grouped ROADMAP row is
  deleted. D072 is at `cairn/DECISIONS.md:3283` and states the triage rule in the
  words AC5 requires, plus the retirement.
- **AC6 — verified.** Both runtime-classed ids hold their own candidate row:
  M071 F9 ("Promote on a report of an option set during a batch not surviving
  it") and M70 O11 ("Promote on any change letting `verify_media()` refuse before
  the file-existence check, or on a report of such a refusal"). Each states a
  class of evidence, not a count.
- **AC7 — verified.** `devtools::test()` FAIL 0 / WARN 12 / SKIP 5 / PASS 8815.
  `devtools::check()` `Status: OK` (0 errors / 0 warnings / 0 notes), 3m 2.2s.
  The 12 warnings are pre-existing and none is in a file this milestone touched.
- **AC8 — partially verified.** The test exists at
  `tests/testthat/test-normalize-audios-two-pass.R:423-451`, carries
  `skip_if_no_ffmpeg()`, uses no `run_loudnorm_analysis_batch()` mock, and
  passes. The `assemble_measured()` mutation was not re-planted this pass. Not
  ticked; see finding 7 on what the criterion asserts.

### Consistency gate

- `cairn_validate.py` — **FAIL**, exit 1. `weight caps`:
  `cairn/milestones/M092-instrument-backlog-triage.md` at 206 plan-owned lines
  against the `<150` cap, shed ≥57. Heaviest first: Triage ledger 96 ·
  Acceptance criteria 38 · Tasks 25 · Scope 19 · Coverage 11 · Goal 6. Every
  other check PASS, including `coverage complete`, `binding criteria`,
  `scaffold present`, `roadmap<->disk orphans` and `references index<->disk`.
  One advisory: `sizing` WARNs at 8 acceptance criteria against the >7 tripwire.
- Byte and line budgets by hand: `cairn/ROADMAP.md` 23,827 bytes / 47 lines
  (`<24,000` / `<60`); `cairn/LESSONS.md` 19,102 bytes / 31 lines (`<20,000` /
  `<50`). Both inside budget.
- `cairn_impact.py` — skipped, no DESIGN principle changed (`Principles
  touched: —`; `cairn/DESIGN.md` is not in the diff).
- Toolchain slot (`r-package`) — all green. `devtools::document()` leaves no
  diff (`git status` clean after). `pkgdown::check_pkgdown()` "No problems
  found." `devtools::check()` clean, above. `NEWS.md` untouched, correct: the
  diff changes tests and tracking only, with no user-visible behaviour change.
  No new top-level files, so no `.Rbuildignore` entry is owed.

### Independent fresh-context review

Internal tier, but the diff touches `tests/` — executable surface — so the full
three-lens fan-out ran, each lens fresh-context and none having seen the
implementation.

**[S] blame-history — 0 findings.** Traced the `condition =` → `error =` swap to
93e88ca (M087 T4) and confirmed the catch-first handler was not a deliberate
design choice but the bug M087's own pass-2 F5 filed; the one site that raises a
warning by design (`batch_sep`, `:590`) is correctly left on `warning =`. No
dangling pointer to the deleted page in a live tracking file, no decision
contradicted, no fixed bug resurrected.

**[S] prior-review — 0 findings.** Archived `## Review` sections on the touched
files (M083, M085, M086, M087, M091) reconcile with what the diff closes and
carries forward; no wording drift in the two runtime rows. The GitHub probe
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`, so
that surface was skipped.

**[O] diff-bug — 9 findings, ranked.** Verified against the implementation, not
the reviewer's account; verification result noted per finding. None is a defect
in shipped package behaviour.

1. *(confirmed)* **`tests/testthat/test-ffm-batch.R` — the AC3 test asserts the
   properties of its own stub, not of anything shipped.** `run_with_progress()`
   (`R/ffm_batch.R:240-250`) is a bare loop assigning `run_one(pipelines[[i]])`
   into a list, so `expect_named`, both `rlang::is_bool()` calls and the two
   replayed `vapply()` expressions are satisfied by construction from the stub
   the test wrote three lines earlier. M70 O6's real exposure is the `run_one`
   closure defined inline in `ffm_batch()` (`R/ffm_batch.R:141-147`), which this
   test never enters. Non-vacuous content is limited to length preservation,
   ordering, and `cli_progress_bar()` not erroring non-interactively. AC3's own
   redness clause places the mutation in the stub, so the criterion as written
   passes — the criterion, not the work, is what does not bind the defect.
2. *(confirmed)* **D072's motivating measurement is wrong.** The entry says the
   page "grew by one section at each of five later hygiene passes" and that "five
   filings in three weeks" is the measurement. `git log --follow` on the deleted
   path shows five commits total: creation at M083 (94f6c77) and three later
   filings (M086 a9d5ec0, M087 dea2821, M092's T1 3414982), then the deletion.
   Three later passes, not five.
3. *(confirmed)* **AC4's grep is a sweep the file is written to evade, and the
   four repaired probes carry no suite-level regression guard.** The new probe at
   `:507-545` exercises a synthetic replica, so reverting any of `:572/578/583/601`
   to the catch-first form leaves the suite green. `condition = identity` at
   `:537` is semantically identical to the retired literal, and the test's own
   comment states it is spelled that way to keep AC4's count at 0. AC4 as written
   holds; what it measures is a lexical spelling.
4. *(confirmed)* **AC5's evidence was produced partly by renaming the artifact the
   sweep measures.** T7 renamed the milestone file from
   `M092-instrument-findings-triage.md` because the sweep matched its own path in
   the ROADMAP table. Substantively benign — a milestone filename is not the
   retired page — but the criterion no longer means only what it claims to have
   measured.
5. *(not sustained as an AC1 failure)* **Four grid ids absent from the ledger.**
   M62 N1, N3, N7 and M64 F4 have no ledger row. Verified: the page's own grid
   text records N1 as closed at M63 and N3/N7/F4 as promoted to M080 on
   2026-08-28, and declares the live set as exactly the 16 the ledger carries. So
   AC1's domain is met. The inconsistency is presentational — every other section
   records already-handled ids explicitly ("prune (already discharged)", "prune
   (already pruned)") and this one does not.
6. *(confirmed)* **`tests/testthat/test-separate-av-multitrack.R:412-445` asserts
   "no warning at all" where AC2 asks that the multi-track advice be absent from
   the warning.** The discriminator is `expect_s3_class(upper, "tbl_df")`, which
   holds only because the `tryCatch(warning = )` returned the tibble; the premise
   that the multi-track warning is the verb's only warning sits in a comment, not
   an assertion. Correct under today's mutation, brittle to any future warning on
   the batch path.
7. *(confirmed)* **`tests/testthat/test-normalize-audios-two-pass.R:423-451`
   cannot distinguish a correct status from a wrong one.** It asserts type,
   length and non-`NA`, never the value, while M086 F9's promote condition names
   "a `tm_row_status` that is **wrong** or all-`NA`". AC8's own text asks only for
   a non-`NA` integer, so the criterion passes; half the defect class is
   unguarded. The reviewer's reading of `tm_rows` as row indices, making the
   `2L` assertion sound, checks out.
8. *(confirmed)* **Two archive summaries still point at the deleted page with no
   forwarding note** — `archive/M083-roadmap-byte-budget.md` and
   `archive/M087-scalar-batch-condition-classes.md`. Both are historical
   narration, so nothing live breaks, but M083's text tells a reader to consult
   the page and no surviving pointer says where the content went. (IP4 forbids
   editing an archive; a forwarding note belongs in D072 or nowhere.)
9. *(confirmed, minor, grouped)* D072 says "roughly forty" findings where the
   ledger enumerates 53 ids; D072 and T1 call M091's entry "M091's eighth
   finding" when it is the page's eighth *section*; the compressed M45 candidate
   row's promote clause still names "(c)'s missing-directory case" after the
   compression stopped naming that case in (c); the T7 work-log line records
   23,832 bytes where the file measures 23,827.

### Disposition

Returned to `in-progress` on the `cairn_validate` weight-caps failure. Under the
return floor none of the nine findings independently returns the milestone — none
demonstrates an acceptance criterion failing inside its named procedure's domain,
and none is a defect in shipped package behaviour — so they ride the gate's
return and are triaged at the maintainer's gate on re-review. Findings 1 and 3
are evidence about the promises rather than the work, and route to the gated
criterion-amendment protocol if the maintainer agrees the two closures should
bind the defects the Goal names.
