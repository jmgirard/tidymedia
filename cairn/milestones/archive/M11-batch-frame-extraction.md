# M11: Tibble-driven batch frame extraction — done 2026-07-12

**Goal:** add `extract_frames()`, a jobs-table sibling of `extract_frame()`
that grabs one still image per row across many inputs.

**Outcome:** a new exported Layer-2 verb (PR #13), a thin `ffm_batch()` fan-out
(D007) mirroring `segment_videos()`:
- Required `input` column + exactly one of `timestamp`/`frame`; optional
  `output` (auto-derived per input via a new `derive_frame_names()` —
  `<basename>_<n>.<format>`, default `png`, numbering restarting per input).
- `...` forwards batch options (verify/manifest/checksums/progress) to the
  runner, never into the per-row `.f` (M09 lesson respected).
- Refactored `extract_frame()`'s single-frame body into a shared internal
  `frame_pipeline()` so both verbs build byte-identical commands.

29 tests (pure parity/validation/edge + binary-gated execution);
`devtools::check()` zero errors/warnings/notes; all CI green.

**Key decisions:** table-level validation matches `extract_frame()`'s scalar
contract — `frame` must be whole, numeric `timestamp` must be finite (not
deliberately coarser).

**Review:** no blame-history findings; diff-bug F1 (82) — `frame` accepted
non-whole values vs. its "whole frame numbers" roxygen — fixed with a
regression test; F2 (66, non-finite `timestamp`) logged + fixed alongside.
