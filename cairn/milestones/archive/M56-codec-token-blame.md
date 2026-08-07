# M56: A bad codec token names the verb's argument, never Layer 1's

**Status:** done (2026-08-07, PR #59 https://github.com/jmgirard/tidymedia/pull/59)

**Goal:** Make a malformed codec token blame the verb's own argument on the four verbs
whose pipelines hand a user value to `ffm_codec()` directly.

**Outcome:** Widened at the implement gate to the whole codec family, then at review to all
three paths a batch verb reads a codec by. Four pipelines route through `apply_audio_codec()`
/ `apply_video_codec()` with `call =` threaded; nineteen front-door `check_string(<codec>)`
became `check_token()` at the same site, on the ten `_batch` verbs and `segment_video()`;
`check_batch_codec_col()` token-checks every codec cell, so a bad column stops at the verb,
not inside `purrr::pmap()`; and `standardize_pipeline()` hands `hardware` to the seam instead
of pre-resolving, so the check sees the user's token, not the nvenc name
`resolve_hw_encoder()` rewrote it to. Scalar 34/34, column 17/17, nvenc parity with
`crop_video()`, against 11/51 and 0/17 on master. No compiled command changed.

**Decisions:** Milestone-local — the front-door token check goes to the fan-out verbs only,
the seam-routed scalar verbs already blaming themselves, so a second guard would only move
error text on verbs out of scope.

**Review:** Three reviewers + scorer; blame-history and prior-review zero, diff-bug 11.
Actioned: F1 (88) column blame degraded to `.f()`, fixed at the shared column guard for all ten
batch verbs; F3 (85) nvenc scalar/batch divergence, fixed by checking before resolution. Nine
logged below threshold. AC1/AC3/AC6 amended, each having pinned a coordinate this work moved.
