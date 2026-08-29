# M087: A diagnostic answers to the same class from the scalar verb and its batch sibling

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** — (RR05 is advisory; no binding criteria)
- **Principles touched:** —
- **Branch/PR:** `m087-scalar-batch-condition-classes`

## Goal

A caller who writes a condition handler from a documented class name gets that
class from a verb's scalar form and from its `_batch` form, or the documentation
states where the two differ and why.

## Scope

Surface tier: **user-facing** — exported condition classes and the help topics
naming them are what callers write `tryCatch()` and
`suppressWarnings(classes = )` against.

**In:** the two diagnostics M086's review left divergent, at five signalling
sites. The loudnorm analysis pass: the scalar abort (`R/loudnorm_two_pass.R:151`)
raises `tidymedia_ffmpeg_exit` alone, the scalar zero-exit unparseable abort
(`:112`) raises nothing at all, and the batch abort (`:253`) raises
`tidymedia_loudnorm_analysis` alone, so a handler on the last fires only for
the batch and part of the event is uncatchable by any name (RR05 B1). The
multi-track separation diagnostic: the scalar abort (`R/ffmpeg.R:681`) carries
two classes and the batch warning (`R/ffmpeg.R:742`) one — which RR05 Q6 finds
is NOT D062's falsifier (one event keeps one name at both severities; the error
site adds a second class for a second fact) but does narrow D062 in a way the
record does not yet state. Also in: one sentence on the silence asymmetry, which
aborts scalar-side and is marked batch-side (RR05 B2); and the
help topics stating class names for those sites (`?normalize_audio`,
`?normalize_audio_batch`, `?separate_audio_video`, `?separate_audio_video_batch`,
`?ffm_run`, `?tidymedia`); a `DECISIONS.md` entry annotating D062 with the
divergence this milestone records and leaves standing; and `?tidymedia`'s closing
`See vignette(…)` navigation paragraph, misfiled inside `@section Session
options:` (`R/tidymedia-package.R:159-161`).

**Out:** `ffm_batch()`'s per-row result contract (D007 surface), which is why the
batch warning can carry no exit status → stays on its own candidate row. The
package's other unclassed `cli_abort()` sites → stays on its own candidate row.
Adopting the ecosystem's `pkg_error_*` shape across every class → D062's
"What this does not decide", unchanged here.

## Acceptance criteria

- [ ] **AC1.** The three loudnorm analysis-pass abort sites raise the shared
      event class `tidymedia_loudnorm_no_measurement`: the scalar non-zero-exit
      abort (`R/loudnorm_two_pass.R:151`) as
      `c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit")` carrying
      `tm_status`; the scalar zero-exit unparseable abort (`:112`) as that class
      alone with no `tm_status`; the batch abort (`:253`) as that class alone
      carrying `tm_rows` and `tm_row_status`. A repo-wide
      `grep -r tidymedia_loudnorm_analysis` over tracked files returns no hit
      outside `cairn/`. (Settled by RR05 Q1/Q3/Q4/Q5 and B1.)
- [ ] **AC2.** A `normalize_audio_batch(two_pass = TRUE)` call carrying at least
      one row that failed to run or printed no parseable measurement block raises
      a condition inheriting AC1's shared class and not inheriting
      `tidymedia_ffmpeg_exit`, carrying `tm_rows` and `tm_row_status`; and
      `?normalize_audio_batch` states that this abort carries no single exit
      status, because it also fires for rows that exited zero. (The class and
      field half is a regression lock on today's behaviour; the reason is new.)
- [ ] **AC3.** `separate_audio_video_batch()`'s post-fan-out warning
      (`R/ffmpeg.R:742`) inherits `tidymedia_multitrack_separation` and not
      `tidymedia_ffmpeg_exit`, and `?separate_audio_video_batch` states that this
      warning carries no exit status because the batch runner's per-row result
      records whether the row succeeded, not how FFmpeg exited. (First clause is
      a regression lock; the reason is new.)
- [ ] **AC4.** Each site's class names, as stated in the topics paired with it
      here, are the class names an executed call observes at that site:
      `R/loudnorm_two_pass.R:151` and `:112` → `?normalize_audio`, `?ffm_run`,
      `?tidymedia`;
      `R/loudnorm_two_pass.R:253` → `?normalize_audio_batch`, `?tidymedia`;
      `R/ffmpeg.R:681` → `?separate_audio_video`, `?ffm_run`, `?tidymedia`;
      `R/ffmpeg.R:742` → `?separate_audio_video_batch`. `?tidymedia`'s names are
      those in its `Bounding a run that hangs` section.
- [ ] **AC5.** `?tidymedia`'s closing `See vignette(…)` navigation paragraph
      renders outside every `\section{}` block in the generated
      `man/tidymedia-package.Rd` (it opens inside `\section{Session options}`
      today).
- [ ] **AC6.** `Rscript -e 'devtools::check()'` reports 0 errors and 0 warnings,
      and `Rscript -e 'devtools::test()'` passes with the FFmpeg-dependent tests
      covering AC1–AC4 running rather than skipping.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T2, T3, T6
- AC3 → T3, T6
- AC4 → T4
- AC5 → T5
- AC6 → T7

## Tasks

- [x] **T1.** Settle the shared event class's name through the escalated review
      (`/milestone-brief`). Done 2026-08-29 by RR05; the answer and its reasoning
      are in `## Decisions` below. (RB tripwire: irreversible-api — discharged)
- [x] **T2.** Apply `tidymedia_loudnorm_no_measurement` at all three sites per
      AC1 (`R/loudnorm_two_pass.R:151`, `:112`, `:253`), retiring
      `tidymedia_loudnorm_analysis`; edit the two unreleased `NEWS.md` entries
      (lines 41, 63) in place so users meet one name. Gate against M075's
      fall-through — the `if (two_pass)` block in `normalize_audio()` does not
      return — so nothing signals twice. Update every test asserting either name.
- [x] **T3.** Add the stated reasons AC2 and AC3 require to
      `?normalize_audio_batch` and `?separate_audio_video_batch`, the latter
      naming what the per-row result does record. While in the two normalize
      topics, add one sentence on the silence asymmetry (RR05 B2).
- [x] **T4.** Re-derive the class enumerations in the six topics AC4 pairs by
      *running* each of the four sites and reading the observed class vector —
      never by reading `class =` off the source (M041/M080: such prose is
      derived, never composed). Record the four observed vectors in the work
      log. Correct each topic to match, including `?ffm_run`'s "two other paths
      raise the same class" sentence (`R/ffm.R:1553-1556`), which AC1 falsifies.
      The probe input must EXIST and be unreadable as media — `nonexistent.wav`
      dies unclassed in `check_file_readable()` (`R/ffmpeg.R:2240`) and never
      reaches FFmpeg (RR05 B3).
- [ ] **T5.** Move `?tidymedia`'s closing `See vignette(…)` paragraph
      (`R/tidymedia-package.R:159-161`) above the `@section` tags so it renders
      outside every section.
- [x] **T6.** Tests: assert the full class vector by identity at each of the four
      sites, including AC2's and AC3's absence assertions; each new assertion
      red against pre-milestone code before it is trusted.
- [ ] **T7.** Append the `DECISIONS.md` entry annotating D062, carrying the five
      points RR05 §6 requires (verbatim in `## Decisions` below): the rule held;
      a site's vector asserts every fact established there; "same event, same
      class vector" is NOT the convention; D007 is what forces it; the sharpened
      falsifier. `NEWS.md` entry.
      Then `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-29: created by /milestone-plan. Absorbs the M086-review-F5 half of the unclassed-aborts candidate row, the M086-review-F6 half of the batch-warning row, and the `?tidymedia` navigation row; the D007 half of the batch-warning row and the wider unclassed-abort sweep stay on their rows.
- 2026-08-29: criteria audit ran in FULL mode (declared tier user-facing), fresh [O] reader, two passes. Pass 1 over the pre-gate draft returned six findings (F1 overstated "cannot" for the batch warning; F2 missing `?ffm_run`; F3 indefinite site→topic referents; F4 antecedent admitting silent rows; F5 no stated reason for the loudnorm pair; F6 unlabelled regression locks) — all six fixed. Pass 2 over the post-gate rewrite returned six more (R1 AC4's "derived by executing" bound an instrument, not the deliverable; R2 F3's site→topic half still all-to-all; R3 AC1's deferred name lacked a remit; R4 AC6 satisfiable with every relevant test skipped; R5 AC3's mandated reason false about the batch result; R6 probe adequacy vacuous) — all six fixed, none deferred.
- 2026-08-29: plan gate chose making both loudnorm sites share one event class over leaving the divergence documented-only, because a handler written from `?normalize_audio_batch` silently catches nothing on the scalar form; falsified by the escalated review returning that the two sites report distinct events that must keep distinct names with no shared parent.
- 2026-08-29: plan gate chose documenting the batch warning's missing exit status over changing `ffm_batch()`'s per-row result contract, because that contract is relied on by every `_batch` verb and is its own milestone; falsified by a batch caller needing a failed row's exit status, the condition already on that row.
- 2026-08-29: plan gate chose escalating the class NAME rather than settling it here, because renaming a class callers match on cannot be undone quietly after release; falsified by the review declining to rule, which returns the naming to T1 as an in-milestone call.
- 2026-08-29: blocked on RB05 — AC1's shared class name and T1 escalated to independent review; every other task is unblocked, but T2 must not start before the RR lands.

- 2026-08-29: RB05 drafted and M087 blocked on it; the escalated question was the shared class's NAME, not whether a shared class exists. RB05 carried the second-escalation removal option, this being the second brief on the package's condition-class naming after RB04.
- 2026-08-29: RR05 ingested (advisory, no binding criteria). T1 discharged. Amendments this ingestion made to plan-owned sections: Scope In corrected — the multi-track asymmetry is NOT D062's falsifier, which RR05 Q6 shows requires two names for one event where the code has one name at both severities; Scope In and AC1 gained a fifth site, `R/loudnorm_two_pass.R:112`, whose abort carries no class at all and sits inside the shared event (RR05 B1); AC1 fixed the name and dropped its deferral clause; AC4 paired the new site; T2/T3/T4/T7 extended.
- 2026-08-29: ingest chose to leave the D062 annotation to T7 rather than append it now, because the entry states what the code establishes and would otherwise be written twice; RR05 §6's five required points are recorded in `## Decisions` below so the content survives this milestone being abandoned. Falsified by M087 being dropped with D062 still unannotated.

- 2026-08-29: T2 + T6 done in ONE commit, a minor task-ordering amendment: T6's "red before trusted" evidence has to be taken against pre-milestone code, so the new assertions were written and run first (three red: the scalar exit site, the scalar unparseable site, the batch site each escaped a `tidymedia_loudnorm_no_measurement` handler), then T2's code landed and turned them green.
- 2026-08-29: T2 — `tidymedia_loudnorm_no_measurement` applied at `R/loudnorm_two_pass.R:112` (alone, no `tm_status`), `:151` (with `tidymedia_ffmpeg_exit`, keeping `tm_status`) and `:253` (alone, keeping `tm_rows`/`tm_row_status`); `tidymedia_loudnorm_analysis` renamed out of NEWS.md, three roxygen sites, their `man/` counterparts and eight test lines. M075's fall-through checked: `run_loudnorm_analysis()` aborts, so the non-returning `if (two_pass)` block cannot reach a second signalling site.
- 2026-08-29: T6 — full class-vector identity assertions added at all four sites plus the silence boundary; M085's flat-vector assertion at `test-ffmpeg-exit-condition.R:52` corrected, since M087 deliberately makes that site carry two classes. `devtools::test()`: 0 failures, 8353 passing, 5 skips (all nvenc-absent).

- 2026-08-29: T3 — `?normalize_audio_batch` now states the abort carries no `tm_status` and is not `tidymedia_ffmpeg_exit` because a batch mixes causes (rows that exited zero included); `?separate_audio_video_batch` now states the warning carries no exit status because `ffm_batch()`'s `run_one()` returns `list(success =, timed_out =)` and discards the condition (verified at `R/ffm_batch.R:141-147`). The silence asymmetry is stated from both sides in `?normalize_audio` and `?normalize_audio_batch`. Four Rd-text assertions added, each confirmed absent from the pre-T3 `man/` files.

- 2026-08-29: T4 — the five class vectors, read off EXECUTED calls (garbage `.wav` probe, FFmpeg 9.0.1, macOS): `:151` `c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")` with `tm_status = 183L`; `:112` `c("tidymedia_loudnorm_no_measurement", "rlang_error", "error", "condition")` with no fields; `:253` the same four with `tm_rows = 1L`, `tm_row_status = 183L`; `R/ffmpeg.R:681` `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")` with `tm_status = 234L`; `R/ffmpeg.R:742` `c("tidymedia_multitrack_separation", "rlang_warning", "warning", "condition")` with no fields.
- 2026-08-29: T4 — `?ffm_run` and `?tidymedia` corrected (the "two other paths raise the same class" sentence now says both also carry a narrower class of their own, and both topics now state which paths raise the shared loudnorm class WITHOUT the exit class); `?normalize_audio` gained the class names for its two sites; `?separate_audio_video` and `?separate_audio_video_batch` already matched. A pairing test executes all five sites and requires every observed `tidymedia_*` class to appear in each paired topic; planting `tidymedia_PLANTED_DRIFT` into `man/ffm_run.Rd` turned it red at the two expected pairings.

## Decisions

- 2026-08-29 (RR05 Q1/Q2): the scalar and batch analysis-pass aborts report ONE event — the loudnorm analysis pass yielded no usable measurement, so no correction could be built — and share one class. The scalar site establishes a second, narrower fact (a known non-zero exit) and carries it as a second class. The docs-only alternative was rejected: it leaves the moved-handler trap armed and cannot reach `R/loudnorm_two_pass.R:112`, which has no class to document. Would change on evidence that callers of the two forms need different recoveries; none exists, the correction pass being unreachable either way.
- 2026-08-29 (RR05 Q3/Q5): the shared class is `tidymedia_loudnorm_no_measurement`; `tidymedia_loudnorm_analysis` is retired. The incumbent names a PHASE and so overpromises over three neighbors that escape it — a timeout aborts `tidymedia_timeout`, a missing binary aborts unclassed, and a silent input aborts unclassed at `:103-110` — which is RR04 §1b's rejected shape in a second dress. `no_measurement` truthfully excludes silence, since a silent input WAS measured at `-inf` (`:79-85`) and the batch marks rather than aborts it (`:262`). `tidymedia_loudnorm_unmeasured` was set aside (what is unmeasured is the input, not the pass); dropping the `loudnorm_` scope was rejected on RR04's narrow-name precedent. The rename is taken now because the package is unreleased and pre-0.2.0 (D014), the only handlers on the incumbent are this repo's own tests, and after first release the calculus inverts permanently.
- 2026-08-29 (RR05 Q4): no class beyond AC1's at either site. `tidymedia_ffmpeg_exit` on the batch abort would assert a fact false for any zero-exit row and could not carry the scalar `tm_status` that class has carried at every site since M085.
- 2026-08-29 (RR05 Q6): the multi-track asymmetry is recorded, not fixed; `R/ffmpeg.R:681` and `:742` are left alone. Restoring symmetry would mean either asserting a mechanism the warning site cannot evidence, or undoing M086. The five points T7's D062 annotation must carry: (1) the rule held — the shared event carries one name at both severities and the falsifier's shape did not occur; (2) what the code has newly established, which D062 did not state — a site's class vector asserts every event established AT THAT SITE, so vectors at two severities of one event may differ by additional classes, never by the shared event's own name; (3) the concession — "same event, same class vector" is therefore NOT the convention, and a handler on a mechanism class does not see batch-severity signals of events whose scalar form carries it, bridging which is documentation's job; (4) the constraint forcing it — D007 discards the per-row condition, so the warning site can evidence neither a non-zero exit nor a `tm_status`; (5) the sharpened falsifier — falsified if the shared event's own name ever differs across severities, or if a class is ever attached at a site that cannot carry that class's contractual fields.
- 2026-08-29 (RR05 recommendations triage): 1-5 applied (shared class; the name; the vector shapes; classing `:112`; recording via T7). 6 applied rather than merely considered — the silence sentence is one line inside a topic T3 already edits; its second half, a distinct class for the silent abort, stays on the unclassed-aborts candidate row. 7 and 8 are RR05's own rejections of alternatives this plan had already declined. 9 accepted, and it is what AC2 and AC3 already lock.

## Review
