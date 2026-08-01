# M51: Make the package's two 0-based audio indices legible

**Status:** done (2026-07-31, PR #54 https://github.com/jmgirard/tidymedia/pull/54)

**Goal:** Give a reader who meets both `audio_stream` and `audio` one documented
place saying what each counts, and cross-link the two disjoint families.

**Outcome:** New `?audio_stream` topic (aliases `audio-tracks`, `audio_indices`;
`_pkgdown.yml` "Concepts") covering both counting bases, both `NULL` readings,
the `NA`-cell rule, and `audio`'s three meanings. `R/audio-stream-doc.R` holds
the two family vectors and generates all eighteen `@param audio_stream` blocks
plus the four fan-in `@param audio` ones via inline `` `r ` `` roxygen calls, so
a stale enumeration is unrepresentable (four were already stale). All 22 verbs
share `@family audio selection functions`. `test-audio-index-docs.R` enumerates
the parameters across `man/*.Rd` (or `tools::Rd_db()`) and reddens on a missing
link.

**Decisions:** M51-D1 — the shared `@param` source is an internal R function
evaluated by roxygen at `document()` time, over an `@inheritParams` donor
(duplicates the lists) and a `man-roxygen/` template. See D032.

**Review:** Blame-history and prior-PR-comments returned nothing; diff-bug
returned 23. F1 (90), F3 (82), F5 (85) actioned; 14 sub-threshold also fixed as
verified-false prose this milestone authored — notably the guard skipping
entirely under `R CMD check`; 6 logged.
