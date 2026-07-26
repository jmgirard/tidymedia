# M33: Wire `hardware=` nvenc into `anonymize_video`

**Status:** done (2026-07-26, PR #35 https://github.com/jmgirard/tidymedia/pull/35)

**Goal:** Give `anonymize_video()` and `anonymize_video_batch()` the same opt-in
`hardware = "nvenc"` GPU-encoding toggle M31 shipped on `standardize_video`.

**Outcome:** Both verbs gained `hardware = c("none","nvenc")` + `fallback = FALSE`.
`anonymize_pipeline()` calls `resolve_hw_encoder(video_codec, hardware, fallback,
call = call)` before its `ffm_codec()`, rewriting the user's `video_codec` to its
nvenc encoder; `hardware = "none"` compiles byte-identical commands (verified by
compiling master and HEAD side by side). The batch sibling threads both as
captured scalars, batch-wide — a `hardware` jobs column is inert, matching
`standardize_video_batch`. Layer 1 untouched (IP1); nvenc machinery reused.

**Decisions:** none milestone-local. Passing `call = call` to
`resolve_hw_encoder()` (stricter error attribution than the M31 reference, which
omits it) was a deviation from the bare mirror, accepted at review.

**Review:** 3-lens fan-out — blame-history and prior-review clean (M31's CI trap
not repeated; its logged F1 is orthogonal). Diff-bug found 3 docs-staleness
items. F2 (90) actioned: `NEWS.md` nvenc bullet extended to name
`anonymize_video`. F1 (70) actioned at the user's direction at the merge gate:
`has_nvenc()`/`nvenc_encoder()` now point back at all three verbs. F3 (45)
logged, not actioned — vignette prose, out of scope. CI green (9 checks).
