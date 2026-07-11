# M07: Complete the blessed multi-input verbs (done 2026-07-11)

**Goal:** finish the D003-named blessed multi-input set and give the engine
verbs research-facing Layer-2 front doors.

**Outcome (PR #7, squash-merged as 0239182; CI 7/7 green):**
- Layer 1: `ffm_vstack()` (vertical companion to hstack, width-matching
  `resize`) and `ffm_overlay()` (composite input 2 over input 1 at raw `x`/`y`
  number-or-expression; optional `scale` resizes the overlay via a `scale2ref`
  graph kept in Layer 1). Shared `check_multi_input_ordering()` guard now backs
  hstack/vstack/overlay.
- Layer 2: `compare_videos()` (side-by-side/stacked over hstack/vstack;
  `resize` default for two inputs, `audio` index) and `picture_in_picture()`
  (corner/center `position`, `scale`, `margin`, `audio`). Both assemble via
  Layer 1 only; audio dropped unless an input index is named.
- All four blessed verbs stay single video output `[vout]` (D006); audio stays
  explicit-map-only (D-M06-1). Evidence: test 339 pass / 0 fail; check 0/0/0;
  new snapshots + `ffm_args()` parity + binary-gated E2E; NEWS + vignette
  updated. Independent Opus review: 0 blocker / 0 important.

**Key decisions:** D009 (promoted) — blessed set completed video-only;
`xstack`/`amix` deferred (`amix` needs an `[aout]` engine generalization);
filtergraph assembly stays in Layer 1 (D002).

**Follow-up:** `check_dim()` accepts any length-1 expression for `x`/`y`
(consistent across dim-taking verbs, not a regression) — candidate hardening.
