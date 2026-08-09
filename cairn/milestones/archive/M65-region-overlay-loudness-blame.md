# M65: A region, overlay or loudness mistake names the verb the user called, in both forms

**Status:** done (2026-08-08, PR #68 https://github.com/jmgirard/tidymedia/pull/68)

**Goal:** `anonymize_video`, `picture_in_picture` and `normalize_audio` refuse
their own region, scale and loudness values at their front doors, naming the verb.

**Outcome:** Shared checkers re-called per D042: `check_region_values()` in
`anonymize_pipeline()` (call threaded) + per batch cell; `check_overlay_scale()`
in the pip pipeline below the contradiction (M61 ordering) + per batch row above
nvenc; `check_loudnorm_targets()` above BOTH `two_pass` blocks, so a bad target
precedes the analysis spawn (D043). One binding per range (`overlay_scale_range`,
`loudnorm_range_*`), read by both layers and doc-rendered. Instruments: the m65
spec list (30 cells) + grid blocks, test-shared-range-bindings.R,
blame-precedence-m65.R (121 crossings, 30 flips), blame-guard-mutations-m65.py.

**Decisions:** D043 promoted (cheap value refusal precedes the analysis probe).
Milestone-local: M65-D1 reordering table (partly superseded by M65-D3: corrected
batch rows + full divergence disclosure); M65-D2 NEWS citations. Gated
amendment: spec list to tests/ (M51/M59 lesson).

**Review:** One floor return (F1, 92): batch copy/audio_stream divergence
undisclosed + undercounted grid — fixed via S6 extension and M65-D3. F3/F4
(90/82) stale comments fixed; F10 (80) row-locator loss → candidate row (with
the divergence residual). Ten logged 25–78; nothing retired.
