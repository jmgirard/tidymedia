# M22: Naming & docs audit + target-scheme decisions

- **Status:** done · **PR:** https://github.com/jmgirard/tidymedia/pull/24 · 2026-07-12

## Goal
Assess the public API (79 exports) for naming + docs quality and decide target
conventions — no `R/` changes — for a later execution milestone.

## Outcome
Shipped `cairn/references/naming-docs-audit-M22.md`: a family-grouped inventory,
10 ranked function-name findings (headliner — `get_*` is overloaded across
file-metadata getters and ffmpeg capability queries), an arg-vocabulary deviation
table, and a `@seealso` gap checklist. Cairn-only diff; no package code changed.

## Key decisions
- **D014 ratified** — target naming scheme + **clean-break** rename policy (no
  `lifecycle` shims, API pre-0.2.0). Batch siblings adopt a `<scalar_verb>_batch`
  suffix (retires `*_videos`/`_audios`, disambiguates `extract_frames`). `.data`
  reexport stays (used in `filter_streams()`); the 4 quoting helpers get dropped.
- CRAN candidate trimmed to release mechanics; its API-cleanup + examples/vignette
  portions moved under the execution follow-up.

## Follow-up
Candidate "Apply M22 naming/docs recommendations" — D014 renames + arg
harmonization + docs gap-fill (RB tripwire: irreversible-api).
