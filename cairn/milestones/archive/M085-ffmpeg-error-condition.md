# M085: A failed FFmpeg run is a condition you can catch

**Status:** done (2026-08-29, PR #89 https://github.com/jmgirard/tidymedia/pull/89)

**Goal:** A non-zero FFmpeg exit raises a classed condition carrying the exit status as a field, so a caller can catch a failed run programmatically and the package reads the status from that field instead of scanning the abort's formatted message.

**Outcome:** `ffm_run()`'s non-zero-exit `cli_abort()` and the `loudnorm` analysis pass's abort both gained `class = "tidymedia_ffmpeg_exit"` and `tm_status = as.integer(status)`, message text unchanged.
`ffmpeg_exit_status()` (`R/ffmpeg.R`, unexported) is now `inherits()` + `cnd$tm_status` + a `NULL` guard — no `conditionMessage()`, no regex — and the wording-coupling test that pinned the old parse is replaced by `tests/testthat/test-ffmpeg-exit-condition.R`.
Documented in `ffm_run()`'s roxygen, `?tidymedia`'s timeout section, and NEWS. Two paths deliberately do not signal the class: `separate_audio_video()`'s multi-track diagnostic re-signals with the exit condition as `parent`, and `ffm_batch()` records `success = FALSE` (D007).

**Decisions:** M085-D1 promoted to D062. M085-D2 one flat class, no parent; M085-D3 both abort sites share it; M085-D4 `ffmpeg_exit_status()` retained rather than collapsed into a handler. Class name settled by RB04/RR04.

**Review:** Three-lens fan-out; blame-history and prior-review lenses found nothing. The diff-bug lens returned nine ranked findings: five fixed on the branch (NEWS overclaiming which verbs raise the class; the helper's class guard unfalsifiable because every negative case also lacked the field; the loudnorm site's class vector unpinned; the 128-plus-signal encoding attributed to R, not the shell; the AC1 oracle's timeout option unpinned), two rejected, F4 extended onto the standing `separate_audio_video()` row at the user's disposition, one noted. No returns.
One line appended to `cairn/references/false-greens.md`; nothing retired.
