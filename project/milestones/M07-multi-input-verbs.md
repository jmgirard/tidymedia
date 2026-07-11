# M07: Complete the blessed multi-input verbs

- **Status:** idea <!-- stub; full plan written when this milestone is reached -->
- **Created:** 2026-07-10

## Idea

Finish the D003 blessed set: `ffm_hstack()` and `ffm_concat()` exist;
`ffm_vstack()` and `ffm_overlay()` (named in D003) do not. Decide whether a
grid verb (`xstack`) and audio mixing (`amix`) join the blessed set or stay
Layer 0. Add Layer 2 verbs on top where research use is obvious (e.g.
side-by-side comparison video, picture-in-picture). All verbs manage their own
stream labels internally per D003/D006 — still no filtergraph DAGs.
