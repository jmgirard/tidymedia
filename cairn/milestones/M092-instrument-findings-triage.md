<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M092: The deferred-findings backlog is triaged and the page retired

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m092-instrument-findings-triage`

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

- [ ] AC1 — The Triage ledger in this file has one entry per `## M` heading of
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
- [ ] AC4 — `grep -c 'condition = function(e) e'` over
      `tests/testthat/test-ffmpeg-exit-condition.R` returns 0, and a probe whose
      site signals a `tidymedia_`-classed warning before its abort captures the
      abort, asserted by its condition class.
- [ ] AC5 — `cairn/references/instrument-findings.md` is deleted along with its
      `cairn/references/INDEX.md` line, and the ROADMAP candidate row pointing at
      it is gone: `git grep -l instrument-findings -- cairn/ROADMAP.md
      cairn/references/` returns no hits. A D-entry states the triage rule
      applied (a finding closes only where the gap lets a defect in shipped
      behaviour reach a user; every other finding is pruned with its reason in
      the ledger) and records the page's retirement.
- [ ] AC6 — Every finding the AC1 ledger classes as runtime rather than
      instrument holds a candidate ROADMAP row stating the class of evidence
      that would promote it.
- [ ] AC7 — `devtools::check()` clean (0 errors / 0 warnings) and
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

- [ ] T1 — File M091's eighth finding (the batch path never exercises
      `holds_multiple_audio()`'s case fold) onto the page as its own `## M091`
      section, so the ledger's domain covers it.
- [ ] T2 — Author the Triage ledger in this file: one entry per `## M` section,
      each finding id marked close/prune with one reason, and each marked
      instrument or runtime.
- [ ] T3 — Add the uppercase-container batch test
      (`tests/testthat/test-separate-audio-video-batch.R`); plant the exact-case
      mutation at `R/ffmpeg.R:899` and record it red before the fix.
- [ ] T4 — Add the binary-free `run_with_progress()` contract test; plant a
      contract-violating stub return and record it red.
- [ ] T5 — Change the four `condition = function(e) e` handlers
      (`tests/testthat/test-ffmpeg-exit-condition.R:532,538,543,561`) to
      `error =`, and add the warning-before-abort probe.
- [ ] T6 — Close the two-pass batch status gap: drive `tm_row_status` through a
      real failing Phase 1 row rather than the mocked binding
      (`tests/testthat/test-normalize-audios-two-pass.R:353-361`).
- [ ] T7 — Write the two carry-forward candidate ROADMAP rows, then delete the
      page, its `INDEX.md` line and the grouped ROADMAP row.
- [ ] T8 — Append the D-entry.
- [ ] T9 — Run `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Triage ledger
<!-- owner: plan (heading) / implement (entries, T2) -->

_Authored at T2._

## Work log

- 2026-08-30: created by /milestone-plan; absorbs the grouped instrument-findings candidate row (dispositioned 2026-08-30 as promotable at the next planning pass).
- 2026-08-30: plan gate chose triage-then-retire over hardening the instruments broadly, because the scope hits the checker-regress shape — every instrument here verifies repo-internal artifacts and none of the ~40 findings is a defect in shipped behaviour; falsified by a defect reaching a user through a gap this pass prunes.
- 2026-08-30: plan gate chose all four closures over the two cheapest, because the batch-runner contract and the two-pass status grid are the two whose failure surfaces on a user's machine; falsified by either closure costing more than a task.
- 2026-08-30: criteria audit ran in REDUCED mode (internal tier, no RB tripwire) via a fresh [O] reader; returned three findings — AC1's headline quantified over "every section" where its grep enumerates only `## M` sections, AC5 carved out two author-recalled paths (one of which holds no hit today while `references/INDEX.md` does), and AC6 hand-listed the two carry-forwards before the triage that classifies them had run. All three fixed before writing: AC1 narrowed to `## M` headings, AC5 rewritten to sweep two named paths directly, AC6 bound to the ledger's own classification.
- 2026-08-30: amendment (substantive, implement gate): T6 — the fourth closure the Goal and Scope name — had no acceptance criterion, Coverage having mapped AC6 (the carry-forward ROADMAP rows T7 writes) to it. Added AC8 binding the two-pass batch status closure and repaired Coverage to AC6 → T7, AC8 → T6. Criteria set widened by one; no defect return is on this log, so D-118's return-adjacent direction rule does not apply.
- 2026-08-30: criteria audit of AC8's wording ran in REDUCED mode (internal tier, no RB tripwire) via a fresh [O] reader that did not author it, before the text was written; returned nothing — CLEAR on the bounded-promise, proportionality and instrument questions.

## Decisions

## Review
