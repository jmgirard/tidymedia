<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M24: Docs gap-fill (@seealso web, metadata-boundary prose, batch disambiguation)

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M23   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m24-docs-gap-fill · https://github.com/jmgirard/tidymedia/pull/26   <!-- owner: implement (branch) / review (PR URL) · create -->

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

- [x] **AC1** `@seealso` cross-links added on the pages enumerated in audit §4:
      every Layer-2 verb links to the `ffm_*` verb(s) it wraps, and each of
      `probe_*` / `get_*` / `mediainfo_*` links to the other two backends.
      Verified by grepping the rendered `man/*.Rd` for `\seealso` on the named
      pages (bridges exist, not just `@family`).
- [x] **AC2** Each metadata family doc page states its backend (ffprobe /
      MediaInfo) and return shape (tibble / scalar) in `@description`/`@details`,
      and `vignettes/metadata.Rmd` gains a family-vs-backend comparison table.
- [x] **AC3** Each `*_batch` doc page (`segment_video_batch`,
      `standardize_video_batch`, `normalize_audio_batch`, `anonymize_video_batch`,
      `extract_frame_batch`) says "batch" in its title or first sentence.
- [x] **AC4** `devtools::document()` + `devtools::build_readme()` run;
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

- [x] **T1** Add `@seealso` to the Layer-1 `ffm_*` filter/IO pages and each
      Layer-2 verb (bridge each verb to the `ffm_*` verb[s] it wraps), per §4.
- [x] **T2** Add `@seealso` across the metadata triad (`probe_*` ↔ `get_*` ↔
      `mediainfo_*`) and program-management pages.
- [x] **T3** Add backend + return-shape prose to each metadata family
      `@description`/`@details`; add the comparison table to
      `vignettes/metadata.Rmd`.
- [x] **T4** Add "batch" to the title/first line of each `*_batch` doc page.
- [x] **T5** `devtools::document()`; `spelling::update_wordlist()`;
      `devtools::build_readme()`; `devtools::check()` → confirm `Status: OK`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (executes M22 audit §4; depends on M23).
- 2026-07-13: implement started; branch m24-docs-gap-fill cut from master.
- 2026-07-13: gate — new @seealso use roxygen markdown `[fn()]`; normalize the 7
  legacy `\code{\link{}}` @seealso in ffmpeg.R to markdown too; @seealso on
  high-fan-out `ffm_*` pages stays curated (representative consumer + related
  builders), not exhaustive.
- 2026-07-13: T1 — added curated cross-family `@seealso` to all 20 Layer-1
  `ffm_*` builder/IO/compile/run pages (+ print method) and every Layer-2 verb;
  normalized the 8 legacy `\code{\link{}}` @seealso (7 in ffmpeg.R + ffm_batch)
  to markdown; fixed a copy-paste in `ffm_scale`'s `@return` ("crop"→"resize").
  `document()` clean, 873 tests pass.
- 2026-07-13: T2 — cross-backend `@seealso` on the metadata triad (`probe_all`,
  `probe_container` page, `mediainfo_*`, all five `get_*`) so each backend links
  to the other two; plus program-management (`find_*`/`set_*`/`install_on_win`),
  capability (`ffmpeg_codecs`/`ffmpeg_encoders`), and the three Layer-0 escape
  hatches. `document()` clean, 873 tests pass.
- 2026-07-13: T3 — backend + return-shape boundary sentence on every metadata
  family page (`probe_all`, `probe_container`, `mediainfo_parameter/query/
  template`, all five `get_*`); added a "Which reader?" family-vs-backend
  comparison table to `vignettes/metadata.Rmd`. `document()` clean, 873 pass.
- 2026-07-13: T4 — first sentence of all five `*_batch` pages now reads "the
  **batch** (table-driven) sibling of ...", so each says "batch" in its opening
  line (verified in the rendered `\description`); normalized the inline
  scalar-sibling `\code{\link{}}` in those clauses to markdown. 873 pass.
- 2026-07-13: T5 — `document()` + `spelling::update_wordlist()` (added Codec /
  getters / pre — pre-existing NEWS.md words, no M24 typos) +
  `build_readme()` (no content diff; reverted ephemeral temp-path churn) +
  `devtools::check()` → **Status: OK** (0 errors / 0 warnings / 0 notes).
  Status → review.
- 2026-07-13: post-verify polish — `mediainfo_query`/`mediainfo_template`
  `@seealso` now also bridge to the `get_*` scalar family, so every metadata
  family's annotated `@seealso` reaches the other two (AC1 triad symmetric).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-13: `@seealso` link syntax = roxygen markdown `[fn()]` across the whole
  package (existing legacy `\code{\link{}}` blocks in `ffmpeg.R` normalized in
  passing). Cross-family `@seealso` on `ffm_*` builder pages is **curated**
  (bridge to the representative wrapping verb + closely-related builders), not an
  exhaustive consumer list, to keep pages readable. Docs-only; no D-entry.

## Review
<!-- owner: review · exclusive -->

**Reviewed 2026-07-13 · PR #26 · branch m24-docs-gap-fill (8 commits, +NEWS/PR
review commit) vs master.**

### Acceptance-criteria evidence (fresh)

- **AC1 — PASS.** Grepped rendered `man/*.Rd`. All 18 Layer-2 verbs carry a
  manual `\seealso` bridge to an `ffm_*` builder (e.g. `extract_audio`→`ffm_drop`,
  `crop_video`→`ffm_crop`, `normalize_audio`→`ffm_loudnorm`, batch verbs→`ffm_batch`
  + scalar sibling). Metadata triad: each family's manual `@seealso` (above the
  `@family` auto-list) reaches the other two — `probe_all`→`mediainfo_*`+`get_*`;
  `mediainfo_query`/`template`→`probe_all`+`get_duration`; each `get_*`→
  `mediainfo_parameter`+`probe_all`. Bridges are the annotated manual block, not
  just `@family`.
- **AC2 — PASS.** Backend + return-shape statement present on every metadata
  page: `probe_all`/`probe_container` ("**FFprobe** … **tibbles**"),
  `mediainfo_parameter` ("**MediaInfo** … a **value**"), `mediainfo_query`/
  `template` ("**MediaInfo** … a **tibble**"), all five `get_*` ("**MediaInfo** …
  a **single value per file** (a numeric scalar)", rendered into `\details`).
  `vignettes/metadata.Rmd` has the "Which reader?" section with a 4-row
  family-vs-backend comparison table.
- **AC3 — PASS.** First `\description` sentence of all five `*_batch` pages reads
  "the **batch** (table-driven) sibling of …" — "batch" confirmed in each
  rendered description.
- **AC4 — PASS.** `devtools::document()` produces no `man/`/`NAMESPACE` diff;
  fresh `devtools::check(--no-manual)` → **Status: OK, 0 errors / 0 warnings /
  0 notes** (873 tests pass; spelling test OK). `build_readme()` yields no content
  change (only ephemeral temp-path churn, reverted).

### Consistency gate

- `cairn_validate.py` → all checks PASS (incl. mirror, single in-progress, caps,
  coverage complete, ISO dates, scaffold). `document()` no diff. README content
  in sync. `pkgdown::check_pkgdown()` → no problems. No `DESIGN.md` principle
  changed → impact scan skipped. NEWS.md: added a "Documentation" entry (no
  milestone numbers). No new top-level files.

### Independent fresh-context review
