# M087: A diagnostic answers to the same class from the scalar verb and its batch sibling

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** — (RR05 is advisory; no binding criteria)
- **Principles touched:** —
- **Branch/PR:** `m087-scalar-batch-condition-classes` / https://github.com/jmgirard/tidymedia/pull/91

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

- [x] **AC1.** The three loudnorm analysis-pass abort sites raise the shared
      event class `tidymedia_loudnorm_no_measurement`: the scalar non-zero-exit
      abort (`R/loudnorm_two_pass.R:151`) as
      `c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit")` carrying
      `tm_status`; the scalar zero-exit unparseable abort (`:112`) as that class
      alone with no `tm_status`; the batch abort (`:253`) as that class alone
      carrying `tm_rows` and `tm_row_status`. A repo-wide
      `grep -r tidymedia_loudnorm_analysis` over tracked files returns no hit
      outside `cairn/`. (Settled by RR05 Q1/Q3/Q4/Q5 and B1.)
- [x] **AC2.** A `normalize_audio_batch(two_pass = TRUE)` call carrying at least
      one row that failed to run or printed no parseable measurement block raises
      a condition inheriting AC1's shared class and not inheriting
      `tidymedia_ffmpeg_exit`, carrying `tm_rows` and `tm_row_status`; and
      `?normalize_audio_batch` states that this abort carries no single exit
      status, because it also fires for rows that exited zero. (The class and
      field half is a regression lock on today's behaviour; the reason is new.)
- [x] **AC3.** `separate_audio_video_batch()`'s post-fan-out warning
      (`R/ffmpeg.R:742`) inherits `tidymedia_multitrack_separation` and not
      `tidymedia_ffmpeg_exit`, and `?separate_audio_video_batch` states that this
      warning carries no exit status because the batch runner's per-row result
      records whether the row succeeded, not how FFmpeg exited. (First clause is
      a regression lock; the reason is new.)
- [x] **AC4.** Each site's class names, as stated in the topics paired with it
      here, are the class names an executed call observes at that site:
      `R/loudnorm_two_pass.R:151` and `:112` → `?normalize_audio`, `?ffm_run`,
      `?tidymedia`;
      `R/loudnorm_two_pass.R:253` → `?normalize_audio_batch`, `?tidymedia`;
      `R/ffmpeg.R:681` → `?separate_audio_video`, `?ffm_run`, `?tidymedia`;
      `R/ffmpeg.R:742` → `?separate_audio_video_batch`. `?tidymedia`'s names are
      those in its `Bounding a run that hangs` section.
- [x] **AC5.** `?tidymedia`'s closing `See vignette(…)` navigation paragraph
      renders outside every `\section{}` block in the generated
      `man/tidymedia-package.Rd` (it opens inside `\section{Session options}`
      today).
- [x] **AC6.** `Rscript -e 'devtools::check()'` reports 0 errors and 0 warnings,
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
- [x] **T5.** Move `?tidymedia`'s closing `See vignette(…)` paragraph
      (`R/tidymedia-package.R:159-161`) above the `@section` tags so it renders
      outside every section.
- [x] **T6.** Tests: assert the full class vector by identity at each of the four
      sites, including AC2's and AC3's absence assertions; each new assertion
      red against pre-milestone code before it is trusted.
- [x] **T7.** Append the `DECISIONS.md` entry annotating D062, carrying the five
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

- 2026-08-29: T5 — the `See vignette(…)` navigation paragraph moved from the end of `@section Session options:` to the end of `@details`, so it now renders inside `\details{}` and before the first `\section{}` in `man/tidymedia-package.Rd`. Guarded by a test that locates it against the first `\section{` offset; restoring the old placement in the generated Rd turned it red.

- 2026-08-29: T7 — D063 appended, annotating D062 with RR05 §6's five points and sharpening its falsifier; `R/ffmpeg.R:681` and `:742` left as they are. `NEWS.md` gained the M087 entry and the unreleased M086 paragraph lost its now-false "a class of its own" phrasing. `devtools::document()` no diff; `devtools::test()` 0 failures / 8388 passing / 5 skips (all nvenc-absent); `devtools::check()` 0 errors, 0 warnings, 0 notes. `git grep tidymedia_loudnorm_analysis -- ':!cairn/'` returns nothing.
- 2026-08-29: review returned M087 to `in-progress` on the return floor. AC4 failed: `?tidymedia`'s `Bounding a run that hangs` section states the loudnorm analysis abort is classed `tidymedia_ffmpeg_exit` with no "when FFmpeg exits non-zero" qualifier (`man/tidymedia-package.Rd:103-107`), so the topic AC4 pairs with `R/loudnorm_two_pass.R:112` names a class an executed call at that site does not raise. AC1, AC2, AC3, AC5 and AC6 verified with fresh evidence; consistency gate clean. Fix-now findings riding the return: F2 (NEWS.md's "raises the same class ... one handler covers both" and "Two paths still do not signal it", both false of the zero-exit path), F3 (`?tidymedia`'s "so does" attaching `tm_rows`/`tm_row_status` to the fieldless scalar abort), F8 (no `DECISIONS.md` record of the class rename or the chosen name), F9/F10 (NEWS.md redundancy and an 83-character line). First defect return.

- 2026-08-29: return fix F1 (AC4) — `?tidymedia`'s exit-class sentence now qualifies the loudnorm analysis pass with "when FFmpeg exits non-zero", matching `?ffm_run`'s wording, so no topic paired with `R/loudnorm_two_pass.R:112` names a class an executed call there does not raise. Guarded by a new assertion in `test-package-topic.R` that reads the attribution window between the loudnorm mention and the class name and requires the qualifier; run against the pre-fix `man/tidymedia-package.Rd` it fails on the old "all classed" sentence, so the guard is red before it is trusted. This binds an attribution rather than an enumeration, which is why the T4 pairing test could not see the defect — F5's point, still carried as a follow-up.
- 2026-08-29: return fix F3 — `?tidymedia`'s "so does the scalar abort" replaced; the scalar zero-exit abort now says it raises the shared class alone "carrying no fields at all", which is the observed vector T4 recorded (no `tm_status`, no `tm_rows`, no `tm_row_status`).
- 2026-08-29: return fixes F2/F9/F10 — `NEWS.md`'s first condition bullet now qualifies "raises the same class ... when FFmpeg exits non-zero" and counts three non-signalling paths rather than two, naming the scalar zero-exit path; the second bullet's duplicate account of why the batch phase answers to the shared class is cut to a forward pointer, leaving the reason stated once in the third bullet (F9); line 41's 83-character line is gone with that rewrite (F10).
- 2026-08-29: return fix F8 — D064 appended, recording the retirement of `tidymedia_loudnorm_analysis` and the choice of `tidymedia_loudnorm_no_measurement`. Appended rather than folded into D063, which states the class-vector rule; the rename is a naming decision under D062 and would be lost when this milestone file compresses to an archive summary. The entry is shown verbatim in the close block.
- 2026-08-29: return verified — `devtools::document()` no diff after the run; `devtools::test()` 0 failures / 8391 passing / 5 skips (all nvenc-absent); `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes (3m 22.6s).

## Decisions

- 2026-08-29 (RR05 Q1/Q2): the scalar and batch analysis-pass aborts report ONE event — the loudnorm analysis pass yielded no usable measurement, so no correction could be built — and share one class. The scalar site establishes a second, narrower fact (a known non-zero exit) and carries it as a second class. The docs-only alternative was rejected: it leaves the moved-handler trap armed and cannot reach `R/loudnorm_two_pass.R:112`, which has no class to document. Would change on evidence that callers of the two forms need different recoveries; none exists, the correction pass being unreachable either way.
- 2026-08-29 (RR05 Q3/Q5): the shared class is `tidymedia_loudnorm_no_measurement`; `tidymedia_loudnorm_analysis` is retired. The incumbent names a PHASE and so overpromises over three neighbors that escape it — a timeout aborts `tidymedia_timeout`, a missing binary aborts unclassed, and a silent input aborts unclassed at `:103-110` — which is RR04 §1b's rejected shape in a second dress. `no_measurement` truthfully excludes silence, since a silent input WAS measured at `-inf` (`:79-85`) and the batch marks rather than aborts it (`:262`). `tidymedia_loudnorm_unmeasured` was set aside (what is unmeasured is the input, not the pass); dropping the `loudnorm_` scope was rejected on RR04's narrow-name precedent. The rename is taken now because the package is unreleased and pre-0.2.0 (D014), the only handlers on the incumbent are this repo's own tests, and after first release the calculus inverts permanently.
- 2026-08-29 (RR05 Q4): no class beyond AC1's at either site. `tidymedia_ffmpeg_exit` on the batch abort would assert a fact false for any zero-exit row and could not carry the scalar `tm_status` that class has carried at every site since M085.
- 2026-08-29 (RR05 Q6): the multi-track asymmetry is recorded, not fixed; `R/ffmpeg.R:681` and `:742` are left alone. Restoring symmetry would mean either asserting a mechanism the warning site cannot evidence, or undoing M086. The five points T7's D062 annotation must carry: (1) the rule held — the shared event carries one name at both severities and the falsifier's shape did not occur; (2) what the code has newly established, which D062 did not state — a site's class vector asserts every event established AT THAT SITE, so vectors at two severities of one event may differ by additional classes, never by the shared event's own name; (3) the concession — "same event, same class vector" is therefore NOT the convention, and a handler on a mechanism class does not see batch-severity signals of events whose scalar form carries it, bridging which is documentation's job; (4) the constraint forcing it — D007 discards the per-row condition, so the warning site can evidence neither a non-zero exit nor a `tm_status`; (5) the sharpened falsifier — falsified if the shared event's own name ever differs across severities, or if a class is ever attached at a site that cannot carry that class's contractual fields.
- 2026-08-29 (RR05 recommendations triage): 1-5 applied (shared class; the name; the vector shapes; classing `:112`; recording via T7). 6 applied rather than merely considered — the silence sentence is one line inside a topic T3 already edits; its second half, a distinct class for the silent abort, stays on the unclassed-aborts candidate row. 7 and 8 are RR05's own rejections of alternatives this plan had already declined. 9 accepted, and it is what AC2 and AC3 already lock.

## Review

_Reviewed 2026-08-29 on `m087-scalar-batch-condition-classes` at 4bdebd9, PR #91.
Evidence taken fresh: every class vector below was read off an EXECUTED call in
a review-side probe script, independent of the branch's own tests (FFmpeg 9.0.1,
macOS 25.6.0)._

### Acceptance criteria

- **AC1 — verified.** A review-side probe executed all three loudnorm
  analysis-pass abort sites and printed the observed `class()` vector and
  fields. `R/loudnorm_two_pass.R:151` (scalar, unreadable `.wav`):
  `c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit",
  "rlang_error", "error", "condition")` with `tm_status = 183`. `:112` (scalar
  zero-exit, reachable only with `run_program()` substituted, since a real
  FFmpeg exiting zero always prints the block): the shared class alone, no
  `tm_status`, message "Could not parse the `loudnorm` measurement". `:253`
  (batch): the shared class alone, `tm_status` NULL, `tm_rows = 1`,
  `tm_row_status = 183`. `git grep -n tidymedia_loudnorm_analysis -- ':!cairn/'`
  exits 1 with no hit; the five remaining hits are all under `cairn/`.
- **AC2 — verified.** The batch probe above raised the shared class, did not
  inherit `tidymedia_ffmpeg_exit`, and carried `tm_rows`/`tm_row_status`.
  `man/normalize_audio_batch.Rd:86-88` states the abort "carries no single exit
  status on `tm_status`, and is not classed `tidymedia_ffmpeg_exit`, because it
  also fires for rows that exited zero".
- **AC3 — verified.** The probe ran `separate_audio_video_batch()` over a
  3-audio-track `.mkv` written to `.mp3`; the post-fan-out warning's observed
  vector is `c("tidymedia_multitrack_separation", "rlang_warning", "warning",
  "condition")`, `tm_status` NULL. `man/separate_audio_video_batch.Rd:118-124`
  states the warning carries no exit status because the batch runner records
  whether a row succeeded, not how FFmpeg exited. That reason checks out against
  the code: `run_one()` reduces a row to `list(success =, timed_out =)`
  (`R/ffm_batch.R:143-147`) and `out$success` is the logical column
  (`R/ffm_batch.R:157`).
- **AC4 — FAILED.** Five sites executed; in the direction the branch's own
  test checks — every observed class appears in each paired topic — the pairings
  hold: `:151`/`:112` → `man/normalize_audio.Rd`, `man/ffm_run.Rd`,
  `man/tidymedia-package.Rd`; `:253` → `man/normalize_audio_batch.Rd`,
  `man/tidymedia-package.Rd`; `R/ffmpeg.R:681` → `man/separate_audio_video.Rd`,
  `man/ffm_run.Rd`, `man/tidymedia-package.Rd`; `R/ffmpeg.R:742` →
  `man/separate_audio_video_batch.Rd`. The criterion is a biconditional, and the
  other direction fails at one pairing. `man/tidymedia-package.Rd:106-107`, in
  the `Bounding a run that hangs` section the criterion names, states that the
  aborts "from the `loudnorm` analysis pass behind
  `normalize_audio(two_pass = TRUE)`" are "all classed `tidymedia_ffmpeg_exit`",
  with no qualifier. An executed call at `R/loudnorm_two_pass.R:112` observes
  `c("tidymedia_loudnorm_no_measurement", "rlang_error", "error", "condition")`
  and no `tidymedia_ffmpeg_exit`. `?ffm_run`'s parallel sentence
  (`R/ffm.R:1554-1556`) carries the qualifier "when FFmpeg exits non-zero";
  `?tidymedia`'s does not. A later paragraph in the same section states the
  zero-exit case correctly, so the section contradicts itself rather than
  omitting the fact — but the class name stated for that site is not the class
  name the site raises, which is what AC4 requires. Reported as finding F1.
- **AC5 — verified.** In the generated `man/tidymedia-package.Rd` the
  `See \code{vignette("tidymedia")}` paragraph begins at byte offset 1372 and
  the first `\section{` opens at 1677, so it renders inside `\details{}` and
  outside every section block.
- **AC6 — verified.** `Rscript -e 'devtools::check()'`: `Status: OK`, 0 errors,
  0 warnings, 0 notes (3m 17.2s). `Rscript -e 'devtools::test()'`:
  FAIL 0 | WARN 12 | SKIP 5 | PASS 8388, and all five skips report
  "nvenc encoder not listed" — none is an absent FFmpeg or FFprobe. Re-run over
  the two files carrying the AC1-AC4 assertions
  (`test-ffmpeg-exit-condition.R`, `test-package-topic.R`):
  FAIL 0 | WARN 0 | SKIP 0 | PASS 158, so those tests ran rather than skipped.

### Consistency gate

Universal cairn-file checks: `cairn_validate.py` exits 0 — 16 PASS, 7 advisory
OK, none firing. No `DESIGN.md` principle changed, so `cairn_impact.py` is not
run.

Toolchain checks (the `r-package` profile's `consistency-gate` slot):
`devtools::document()` produces no diff (`git status` clean apart from this
milestone file). `NAMESPACE`, `man/` and `data/` are generated, and the no-diff
`document()` run covers them. `README.Rmd`/`README.md` are untouched by the
branch, so no re-knit is due. `pkgdown::check_pkgdown()` — "No problems found."
`NEWS.md` carries this milestone's user-visible changes and names no milestone
numbers. No new top-level files, so no `.Rbuildignore` entry is due.
`devtools::check()` — 0 errors, 0 warnings, 0 notes, 3m 17s.

### Independent review

Three fresh-context reviewers ran in parallel on distinct evidence bases. The
[S] blame-history lens and the [S] prior-PR-comments lens each returned zero
findings: history shows the one history-sensitive move — breaking M085's flat
single-class shape at `R/loudnorm_two_pass.R:151` — is the exact question RB05
escalated and D063 records, and the GitHub inline-comment probe
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]`, so
that lens's per-PR walk was correctly skipped. The [O] diff-bug lens found no
functional bug and ten prose, record and test-strength findings, ranked below
with their dispositions. Each was verified against the implementation, not
against the reviewer's account of it.

- **F1 — return.** `?tidymedia` still states the loudnorm analysis abort is
  classed `tidymedia_ffmpeg_exit` without the "when FFmpeg exits non-zero"
  qualifier `?ffm_run` carries, so the topic AC4 pairs with
  `R/loudnorm_two_pass.R:112` names a class that site does not raise. Confirmed
  in the generated Rd at `man/tidymedia-package.Rd:103-107`. This is the silent
  miss the milestone exists to remove, on the landing topic a caller reads
  first. Fails AC4 — see the criterion above.
- **F2 — fix.** `NEWS.md:34-36` states without qualification that the analysis
  pass "raises the same class and carries the same field, so one handler covers
  both", which is false of the zero-exit path this milestone added a class to;
  and `NEWS.md:38-42`'s "Two paths still do not signal it" is now three, the
  scalar zero-exit path being the third. Same defect shape as F1, in unreleased
  user-facing text.
- **F3 — fix.** `R/tidymedia-package.R:130-132`: "so it raises … alone, carrying
  `tm_rows` and `tm_row_status` and no exit status; so does the scalar abort for
  an analysis pass that exited zero". Read strictly, "so does" carries the whole
  predicate, but the scalar `:112` abort carries no fields at all — the probe
  above observed none.
- **F8 — fix.** The retirement of `tidymedia_loudnorm_analysis` and the choice
  of `no_measurement` are recorded only in this milestone file's `## Decisions`.
  D062 owns class naming and is unamended on the rename; D063 states only the
  class-vector rule. The milestone file is replaced by a ≤25-line summary at
  archive, so a reader of `DECISIONS.md` alone could not reconstruct why a
  public class name changed.
- **F9/F10 — fix, folded into F2.** The two `NEWS.md` bullets both explain that
  the batch analysis phase raises the shared class rather than the exit class,
  for the same stated reason (`NEWS.md:62-69` and `71-97`); and `NEWS.md:41`,
  the line T2 edited, runs to 83 characters against the file's ~80-column wrap.
- **F5 — follow-up.** The AC4 pairing test
  (`tests/testthat/test-ffmpeg-exit-condition.R:564-587`) asserts only that
  every observed class appears in each paired topic, never that a topic omits a
  class its paired sites do not raise — which is why F1 shipped green, and why
  the planted-drift check only exercised the covered direction. Not fixed here:
  the obvious strengthening (over the union of a topic's paired sites) would
  still not catch F1, since `?tidymedia` is also paired with `:151`, which does
  raise the exit class. A test that would catch it needs to bind a claim to a
  site rather than to a topic, which is a design call of its own. Carried to
  the post-merge hygiene pass of the re-review, where the instrument-findings
  page takes it under its own disposition rule.
- **F6 — follow-up.** The AC5 guard
  (`tests/testthat/test-package-topic.R:52-58`) asserts the paragraph precedes
  the first `\section{`, which `\description{}` would also satisfy; AC5 says
  outside every section. AC5 itself holds — the paragraph sits at offset 1405,
  inside `\details{` (420) and before the first `\section{` (1677) — so this is
  test strength, not behaviour. Carried alongside F5.
- **F4 — rejected.** `?ffm_run` calling the event classes "narrower" is read by
  the reviewer as set inclusion, which D063 denies. In context the sentence is
  "a second, narrower class ahead of this one … which is what to catch when it
  is that failure in particular you want" — narrower in what it names, which is
  the same "most specific context first" ordering the code comments state. A
  reading nitpick, not a false claim.
- **F7 — rejected.** AC1's `git grep` sweep skipping outside a git checkout is
  correct guarding, not a gap: the criterion's evidence is the review-side grep
  recorded under AC1 above, and a test that shelled out to `git` from an
  `R CMD check` tarball would be the defect.

### Disposition

Returned to `in-progress` on the return floor: F1 demonstrates AC4 failing.
F2, F3, F8, F9 and F10 ride the same return as fix-now work; F5 and F6 are
follow-ups; F4 and F7 are rejected with the reasons above. First defect return
for this milestone.

