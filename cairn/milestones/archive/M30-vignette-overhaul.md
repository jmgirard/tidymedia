# M30: Vignette overhaul — quality/clarity/realism + fuller verb coverage — done

**Goal:** Raise the vignettes' quality, clarity, and realism — lead with the
task-verb front door and demonstrate every Layer-2 task verb in a concrete
research pipeline. Docs-only, no `R/` change.

**Outcome:**
- Reframed `tidymedia.Rmd` front-door-first (opens on `extract_audio`/`crop_video`,
  IP1/D002) before descending to the `ffm_*` builder.
- New `workflow.Rmd`: an end-to-end dyadic-interaction preprocessing pipeline
  (standardize → normalize/convert audio → sample/extract frames → de-identify →
  concatenate → format_for_web) — homes the 9 task verbs no vignette showed,
  plus `standardize_video_batch`.
- All 15 task-verb families now appear in ≥1 vignette (up from 4); all four
  vignettes cross-link; `workflow` registered in `_pkgdown.yml`; NEWS entry.
- `anonymize_video` described honestly as fixed-region box-fill, not face
  tracking (GP1).

**Key points:**
- Fulfilled the "examples/vignette pass" portion of the CRAN candidate (release
  mechanics + a roxygen `@examples` pass remain on that row).
- Review: all 6 ACs fresh-verified (grep-audit 15/15; masked-binary render clean;
  `check()` 0/0/0; `pkgdown` clean); independent 3-lens review returned 0 findings.

**PR:** https://github.com/jmgirard/tidymedia/pull/32
