# M087: A diagnostic answers to the same class from the scalar verb and its batch sibling

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

A caller who writes a condition handler from a documented class name gets that
class from a verb's scalar form and from its `_batch` form, or the documentation
states where the two differ and why.

## Scope

Surface tier: **user-facing** — exported condition classes and the help topics
naming them are what callers write `tryCatch()` and
`suppressWarnings(classes = )` against.

**In:** the two diagnostics M086's review left divergent, at four signalling
sites. The loudnorm analysis pass: the scalar abort (`R/loudnorm_two_pass.R:151`)
raises `tidymedia_ffmpeg_exit` alone while the batch abort (`:253`) raises
`tidymedia_loudnorm_analysis` alone, so a handler on the latter fires only for
the batch. The multi-track separation diagnostic: the scalar abort
(`R/ffmpeg.R:681`) carries two classes and the batch warning (`R/ffmpeg.R:742`)
one, which is D062's own stated falsifier, real and unrecorded. Also in: the
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

- [ ] **AC1.** The scalar `normalize_audio(two_pass = TRUE)` analysis-pass abort
      (`R/loudnorm_two_pass.R:151`) and the batch analysis-pass abort (`:253`)
      both inherit one shared `tidymedia_*` event class other than
      `tidymedia_ffmpeg_exit`; the scalar abort additionally inherits
      `tidymedia_ffmpeg_exit`, carrying that run's exit status on `tm_status`.
      The shared class's name — and whether either site keeps a narrower class
      beside it — is settled by the escalated review before T2. That review's
      remit is the naming; that a shared class exists is settled here.
      (RB tripwire: irreversible-api)
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
      `R/loudnorm_two_pass.R:151` → `?normalize_audio`, `?ffm_run`, `?tidymedia`;
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

- [ ] **T1.** Settle the shared event class's name through the escalated review
      (`/milestone-brief`); record the answer and its reasoning in this file's
      `## Decisions` section before touching T2. (RB tripwire: irreversible-api)
- [ ] **T2.** Give the scalar analysis-pass abort (`R/loudnorm_two_pass.R:151`)
      the settled shared class alongside `tidymedia_ffmpeg_exit`, and apply the
      settled name at `:253`. Gate the edit against M075's fall-through — the
      `if (two_pass)` block in `normalize_audio()` does not return — so nothing
      signals twice. Update every test asserting either name.
- [ ] **T3.** Add the stated reasons AC2 and AC3 require to
      `?normalize_audio_batch` and `?separate_audio_video_batch`, the latter
      naming what the per-row result does record.
- [ ] **T4.** Re-derive the class enumerations in the six topics AC4 pairs by
      *running* each of the four sites and reading the observed class vector —
      never by reading `class =` off the source (M041/M080: such prose is
      derived, never composed). Record the four observed vectors in the work
      log. Correct each topic to match, including `?ffm_run`'s "two other paths
      raise the same class" sentence (`R/ffm.R:1553-1556`), which AC1 falsifies.
- [ ] **T5.** Move `?tidymedia`'s closing `See vignette(…)` paragraph
      (`R/tidymedia-package.R:159-161`) above the `@section` tags so it renders
      outside every section.
- [ ] **T6.** Tests: assert the full class vector by identity at each of the four
      sites, including AC2's and AC3's absence assertions; each new assertion
      red against pre-milestone code before it is trusted.
- [ ] **T7.** Append the `DECISIONS.md` entry annotating D062 — the divergence
      recorded and left standing, and what would falsify it. `NEWS.md` entry.
      Then `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-29: created by /milestone-plan. Absorbs the M086-review-F5 half of the unclassed-aborts candidate row, the M086-review-F6 half of the batch-warning row, and the `?tidymedia` navigation row; the D007 half of the batch-warning row and the wider unclassed-abort sweep stay on their rows.
- 2026-08-29: criteria audit ran in FULL mode (declared tier user-facing), fresh [O] reader, two passes. Pass 1 over the pre-gate draft returned six findings (F1 overstated "cannot" for the batch warning; F2 missing `?ffm_run`; F3 indefinite site→topic referents; F4 antecedent admitting silent rows; F5 no stated reason for the loudnorm pair; F6 unlabelled regression locks) — all six fixed. Pass 2 over the post-gate rewrite returned six more (R1 AC4's "derived by executing" bound an instrument, not the deliverable; R2 F3's site→topic half still all-to-all; R3 AC1's deferred name lacked a remit; R4 AC6 satisfiable with every relevant test skipped; R5 AC3's mandated reason false about the batch result; R6 probe adequacy vacuous) — all six fixed, none deferred.
- 2026-08-29: plan gate chose making both loudnorm sites share one event class over leaving the divergence documented-only, because a handler written from `?normalize_audio_batch` silently catches nothing on the scalar form; falsified by the escalated review returning that the two sites report distinct events that must keep distinct names with no shared parent.
- 2026-08-29: plan gate chose documenting the batch warning's missing exit status over changing `ffm_batch()`'s per-row result contract, because that contract is relied on by every `_batch` verb and is its own milestone; falsified by a batch caller needing a failed row's exit status, the condition already on that row.
- 2026-08-29: plan gate chose escalating the class NAME rather than settling it here, because renaming a class callers match on cannot be undone quietly after release; falsified by the review declining to rule, which returns the naming to T1 as an in-milestone call.

## Decisions

## Review
