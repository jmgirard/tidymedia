<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: Docs gap-fill (@seealso web, metadata-boundary prose, batch disambiguation)

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M23   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Fill the targeted documentation gaps from the M22 audit — `@seealso`
cross-linking, metadata-family boundary prose, and batch disambiguation — using
the final (M23) names.

## Scope

**In:** the additive, non-API doc work from the M22 audit
(`cairn/references/naming-docs-audit-M22.md §4`):

- **`@seealso` cross-link web** bridging *across* families (`@family` already
  groups within): each Layer-2 verb ↔ the `ffm_*` verb(s) it wraps; the
  metadata triad `probe_*` ↔ `get_*` ↔ `mediainfo_*` so users discover the
  alternative backend; plus the Layer-1 `ffm_*` filter/IO pages and program
  management, per the §4 enumeration.
- **Metadata-boundary prose** (N5): a `@description`/`@details` sentence on each
  metadata family page stating backend (ffprobe vs MediaInfo) and return shape
  (tibble vs scalar), plus a comparison table in `vignettes/metadata.Rmd`.
- **Batch disambiguation prose** (N8): each `*_batch` page says "batch" in its
  title/first line.

**Out:**
- Any renames or signature changes → done in **M23** (this milestone only
  touches roxygen prose and one vignette; no `NAMESPACE` change expected).
- CRAN release mechanics → the "CRAN readiness" candidate.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1** `@seealso` cross-links added on the pages enumerated in audit §4:
      every Layer-2 verb links to the `ffm_*` verb(s) it wraps, and each of
      `probe_*` / `get_*` / `mediainfo_*` links to the other two backends.
      Verified by grepping the rendered `man/*.Rd` for `\seealso` on the named
      pages (bridges exist, not just `@family`).
- [ ] **AC2** Each metadata family doc page states its backend (ffprobe /
      MediaInfo) and return shape (tibble / scalar) in `@description`/`@details`,
      and `vignettes/metadata.Rmd` gains a family-vs-backend comparison table.
- [ ] **AC3** Each `*_batch` doc page (`segment_video_batch`,
      `standardize_video_batch`, `normalize_audio_batch`, `anonymize_video_batch`,
      `extract_frame_batch`) says "batch" in its title or first sentence.
- [ ] **AC4** `devtools::document()` + `devtools::build_readme()` run;
      `devtools::check()` clean — confirm `Status: OK` in `00check.log`
      (`spelling::update_wordlist()` run first — LESSONS M17).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [ ] **T1** Add `@seealso` to the Layer-1 `ffm_*` filter/IO pages and each
      Layer-2 verb (bridge each verb to the `ffm_*` verb[s] it wraps), per §4.
- [ ] **T2** Add `@seealso` across the metadata triad (`probe_*` ↔ `get_*` ↔
      `mediainfo_*`) and program-management pages.
- [ ] **T3** Add backend + return-shape prose to each metadata family
      `@description`/`@details`; add the comparison table to
      `vignettes/metadata.Rmd`.
- [ ] **T4** Add "batch" to the title/first line of each `*_batch` doc page.
- [ ] **T5** `devtools::document()`; `spelling::update_wordlist()`;
      `devtools::build_readme()`; `devtools::check()` → confirm `Status: OK`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (executes M22 audit §4; depends on M23).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
