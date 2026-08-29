# M088: A failed audio half no longer costs the caller the video

**Status:** done (2026-08-29, PR #92 https://github.com/jmgirard/tidymedia/pull/92)

**Goal:** A `separate_audio_video()` run whose audio command fails still writes the video file, and still reports the audio failure.

**Outcome:** `separate_audio_video(run = TRUE)` holds the audio run's condition in a `tryCatch()`,
runs `ffm_run(video)` either way, then re-raises the held object through the new
`abort_after_video()` — the original condition, never a rebuild, so its class vector, `tm_status`
and the `Caused by` chain survive. On a written video it appends one bullet, `The video output was
written to <file>`, formatted once via `cli::format_inline()` so a brace-bearing path cannot
re-interpolate (M44); when the video command failed too that run's condition is discarded and the
audio failure aborts. Roxygen's "When the audio output fails", `@return` and `NEWS.md` state the run
order and the per-run disk rule — what a failed run wrote is removed, what it never wrote to is left
as it was. Both batch sites untouched; the verb joins the derived timeout-absorber partition.

**Decisions:** D065 (run order, and which condition is raised); D066 (a held audio failure
of any kind lets the video command run, a reached limit included).

**Review:** Two passes, three lenses each. First returned the milestone: AC5 failed on prose
promising a failed run leaves its output "absent", false over a pre-existing file (D046 removes
what a run wrote, not what it found); F1/F2/F5/F7 fixed as T9–T12, F3/F6/F8 deferred. Second: all
seven criteria pass on fresh evidence; F1 — the replacement sentence's "its own error says which",
false for the discarded video condition — fixed at the gate; F2/F3 to a new candidate row; F4/F5
rejected. Both [S] lenses clean on both passes.
