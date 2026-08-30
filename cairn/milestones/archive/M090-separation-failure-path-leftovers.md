# M090: The both-fail path stops throwing away what it knows

**Status:** done (2026-08-30, PR #94 https://github.com/jmgirard/tidymedia/pull/94)

**Goal:** A `separate_audio_video()` call whose video half also failed hands the caller that
failure programmatically, and the video-written line is gated on the file actually being written.

**Outcome:** The video run's condition is carried out of the `tryCatch()` and attached
to the raised audio condition at `tm_video_error`, `NULL` when that command succeeded, so
a handler reads the second failure rather than only a human reading FFmpeg's console
output; no rendered text changes. The "The video output was written to" line is decided by
`is.null(video_error) && !identical(output_snapshot(videofile), before_video)` across
pre/post the video run, replacing a bare exit-status read. `abort_after_video()` gained a
`video_error` argument, lost its bare-condition `else` fallback, and guards the `body` note on
`inherits(cnd, "rlang_error")`. Roxygen and the unreleased `NEWS.md` entry state the second-spawn cost.

**Decisions:** D068. Milestone-local: the `wrote` gate takes the video run's outcome AND
the snapshot comparison together, superseding the implement gate's snapshot-alone choice;
the `NEWS.md` mechanism sentence corrected with it.

**Review:** Three lenses; blame-history and prior-review returned nothing. Diff-bug
returned six ranked. F1 (the gate claiming a written video after a failed run
destroyed a pre-existing `videofile`, reproduced with real FFmpeg), F2 (same gate, read-only
removal shape) and F3 (the missing test) fixed on the branch, pinned by a test red on the
branch's own gate; F4-F6 rejected. LESSONS' M088 line corrected.
