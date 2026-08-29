# M086: The catchable failure reaches the two paths M085 left behind

**Status:** done (2026-08-29, PR #90 https://github.com/jmgirard/tidymedia/pull/90)

**Goal:** `?tidymedia` promised a failed FFmpeg run raises `tidymedia_ffmpeg_exit` carrying `tm_status`; two paths broke that, and both now honour it.

**Outcome:** `run_separation_audio()`'s multi-track diagnostic (`R/ffmpeg.R:681`)
carries `c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit")` and
`tm_status = as.integer(status)`, keeping its message, enrichment and `parent`
chain; its `is.na(status)`, `is.na(n)` and `n <= 1L` fail-open branches still
re-raise the original condition unchanged. `assemble_measured()` (`R/loudnorm_two_pass.R:227,253`) stops collapsing a failed row to
`list(status = "error")` and raises `tidymedia_loudnorm_analysis` with `tm_rows`
plus `tm_row_status`, aligned to it, `NA_integer_` where the row exited zero but
printed no parseable block. `?separate_audio_video`, `?ffm_run`,
`?normalize_audio_batch`, `?tidymedia` and `NEWS.md` say which class catches what.

**Decisions:** none cross-cutting; two plan-gate choices are in the work log —
the exit class over documenting the `cnd$parent` chain, and a new event class for
the batch abort over narrowing `tidymedia_ffmpeg_exit`.

**Review:** three-lens fan-out; blame-history and prior-PR lenses found nothing,
`[O]` diff-bug returned ten. Fixed at the gate: a self-contradicting `NEWS.md`
bullet, two stale comments, a help page omitting `tidymedia_timeout`. Ubuntu CI on
ffmpeg 6.1.1 settled the AC1 status-variation check; four deferred into existing
rows, two rejected. One amendment return (AC2); no defect returns.
