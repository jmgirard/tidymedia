# M20: Fixed-region box-fill anonymization verb — done

- **Status:** done · **PR:** https://github.com/jmgirard/tidymedia/pull/22
- **Principles:** IP1, IP2 (works under; unchanged)

**Goal.** Add a Layer-2 verb to cover fixed rectangular regions of a video with
opaque filled boxes for de-identification (no tracking).

**Outcome.** `anonymize_video(infile, outfile, regions, color = "black",
vcodec = "libx264", pixel_format = "yuv420p", run = TRUE)` ships: `regions` is a
data frame (one row per box: `x`/`y`/`width`/`height`, optional per-row
`color`); one filled `ffm_drawbox` per region chains into a single `-vf`
(IP2-clean, no `-filter_complex`); video re-encoded reproducibly with an
even-dimension floor guard, audio stream-copied — the `standardize_video()`
encode profile. Shared `anonymize_pipeline()` + `check_regions()` hold all
validation so the M21 batch sibling inherits it (M13 extract-first). 25 new
tests; full suite 810 pass; `devtools::check()` 0/0/0. Independent review (3
lenses) found zero actionable findings.

**Key decisions.** Box-fill only; region blur deferred to a candidate (needs an
IP2 filtergraph design call + a new `ffm_boxblur`). Batch sibling split to M21.
Integer coordinate columns coerced to double (ffm_drawbox's `check_dim` rejects
bare integers). No explicit `ffm_map` — default stream selection, matching
`standardize_pipeline()`. Incidental: normalized DESIGN.md principle lines to
cairn's canonical `- IPn:` format (substance unchanged, no D-entry).
