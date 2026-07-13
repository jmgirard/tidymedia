<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M25: Verb coverage survey (research-domain gap analysis)

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** GP1   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Survey the Layer 2 task verbs a behavioral/affective-science media-preprocessing
pipeline demands, diff them against what tidymedia already exports, and give
every gap a scoped keep/refuse/defer verdict — producing a reference doc and
prioritized candidate rows, not new code.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- A written survey/gap-analysis reference doc anchored on the behavioral/
  affective-science video-preprocessing workflow (facial/interview video,
  anonymization, segmentation, frame/audio extraction, comparison).
- A "have" inventory: every current Layer 2 task verb classified by pipeline
  stage, cross-checked against `NAMESPACE`.
- A "want" model: the verbs each pipeline stage needs from a researcher's view.
- A scoped gap list: each absent verb gets an explicit **keep / refuse / defer**
  disposition with one-line rationale (refusals cite GP1 / D001).
- ROADMAP candidate rows for the keep/defer gaps; reconciliation of the existing
  region-blur candidate so it is not duplicated.

**Out:**
- Building, renaming, or modifying any verb — this is analysis only. Implementing
  any surfaced gap → its own future milestone, planned from the candidate row it
  produces here.
- Promoting gaps into fully *planned* milestone files → deferred by user choice;
  candidate rows only (a routing chip offers to plan the top gap afterward).
- Layer 0 / Layer 1 surface, metadata families, and program-management verbs —
  this survey is scoped to Layer 2 task verbs.
- Analog-tool (av/magick/tuneR/moviepy) feature parity as a goal — such tools may
  be cited as evidence of a workflow need, but demand is anchored on the research
  domain, not on matching another package's surface.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1: A survey doc exists at `cairn/references/verb-coverage-survey-M25.md`
      and is listed under "Working artifacts" in `cairn/references/INDEX.md`.
- [ ] AC2: The doc's "have" inventory lists every exported Layer 2 task verb, and
      that set matches the task verbs in `NAMESPACE` exactly (no verb missing, none
      invented) — verifiable by diffing the doc's list against `grep '^export'
      NAMESPACE` filtered to task verbs.
- [ ] AC3: The doc names the stages of a behavioral/affective-science media-
      preprocessing pipeline and, for each stage, lists the verbs a researcher
      needs, marking each as covered or absent.
- [ ] AC4: Every absent verb carries an explicit `keep | refuse | defer`
      disposition with a one-line rationale; each `refuse` cites GP1 or D001.
- [ ] AC5: Every `keep`/`defer` gap appears as a prioritized candidate row in
      `cairn/ROADMAP.md`; no `refuse` verb is added as a candidate.
- [ ] AC6: The existing region-blur candidate row is reconciled (absorbed into or
      cross-referenced from this survey), not duplicated — at most one region-blur
      candidate exists after this milestone.

(No `devtools::check()` criterion: docs/tracking-only milestone, no runtime
surface touched. No code tests — the deliverable is a reference doc; evidence is
its content and the ROADMAP diff.)

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T4
- AC2 → T1, T4
- AC3 → T2, T4
- AC4 → T3, T4
- AC5 → T5
- AC6 → T3, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate -->

- [ ] T1: Enumerate the current Layer 2 task-verb surface from `NAMESPACE`
      (excluding `ffm_*`, metadata, program-management, and reexports) and classify
      each verb by pipeline stage into a "have" table. Cross-check the set against
      `grep '^export' NAMESPACE`.
- [ ] T2: Model the behavioral/affective-science media-preprocessing pipeline as
      ordered stages (e.g. ingest/standardize → trim/segment → anonymize →
      extract frames/audio → normalize → compare/assemble) and, per stage, list the
      verbs a researcher needs — the "want" model. Analog tools may be cited as
      evidence of a need but the framing stays domain-anchored (Scope: Out).
- [ ] T3: Diff want vs have → gap list. Assign each gap a `keep | refuse | defer`
      verdict with a one-line rationale; refusals cite GP1 / D001. Reconcile the
      existing region-blur candidate into this list.
- [ ] T4: Write `cairn/references/verb-coverage-survey-M25.md` (have table, pipeline
      stages, gap list with dispositions) and add it to `cairn/references/INDEX.md`
      under "Working artifacts".
- [ ] T5: Add prioritized candidate rows to `cairn/ROADMAP.md` for the keep/defer
      gaps; edit/replace the region-blur row so it is not duplicated; commit the doc
      + INDEX + ROADMAP together.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-13: created by /milestone-plan.

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- owner: review · exclusive; evidence per criterion; consistency-gate
     results; independent-review findings and their triage -->
