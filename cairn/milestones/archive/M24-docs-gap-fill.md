# M24: Docs gap-fill (@seealso web, metadata-boundary prose, batch disambiguation) — done

- **Depends on:** M23 · **PR:** #26 (squash-merged 2026-07-13) · docs-only, no API/NAMESPACE change.
- Executed the additive doc work from the M22 audit §4.

## Outcome

- **@seealso cross-link web:** every Layer-2 verb bridges to the `ffm_*` builder(s) it
  wraps (and each builder back to a representative wrapping verb); the metadata triad
  `probe_*` ↔ `mediainfo_*` ↔ `get_*` cross-links so users find the alternative backend;
  program-management, capability, and Layer-0 escape-hatch pages bridged. All legacy
  `\code{\link{}}` @seealso normalized to markdown `[fn()]`.
- **Metadata boundary:** each metadata family page states its backend (FFprobe vs
  MediaInfo) and return shape (tibble / value / scalar); `vignettes/metadata.Rmd` gained a
  "Which reader?" comparison table.
- **Batch:** each `*_batch` page says "batch" in its first sentence. Fixed a 2020
  copy-paste in `ffm_scale()`'s `@return` ("crop"→"resize"); added a NEWS Documentation
  entry. `devtools::check()` OK (0/0/0); 873 tests.

## Decision / review

- Link syntax = markdown `[fn()]` package-wide; cross-family @seealso on `ffm_*` pages kept
  **curated** (not exhaustive). Docs-only; no D-entry.
- AC1–AC4 verified fresh. Review: 1 confirmed diff-bug (get_* over-generalized
  `mediainfo_*()` as tibble-returning) fixed in review; blame-history clean.
