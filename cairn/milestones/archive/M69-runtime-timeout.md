# M69: A hung media program stops the call, not the session

**Status:** done (2026-08-26, PR #72 https://github.com/jmgirard/tidymedia/pull/72)

**Goal:** Give callers a wall-clock limit on every process tidymedia spawns, so a hung
FFmpeg aborts the call instead of blocking the R session forever.

**Outcome:** Option seam `tidymedia.timeout` (whole seconds; `0`, the default, means no
limit), read by `resolve_timeout()` and passed to the `timeout=` argument of all four spawn
sites — `ffmpeg()`, `ffprobe()`, `mediainfo()`, `run_program()`. New `R/timeout.R` holds it
plus `is_timeout()` (keyed on `status == 124L`, never on R's translated warning text),
`guard_timeout()`, and `abort_timeout()`, which names program and limit only, keeping R's
command line and `input=` temp path from the caller. A limit does three things: the task
verbs, `ffm_run()`, `verify_media()` and the Layer 0 hatches abort with class
`tidymedia_timeout`; `probe_one()` returns a classed sentinel so `probe_all()` yields an NA
row plus one warning counting timeouts apart from unreadable files; `count_audio_streams()`
and `tool_versions()` absorb silently, disclosed in the docs. D046 applies unchanged.

**Decisions:** D047 (the seam, off by default, abort-not-warn, the rejected per-verb argument, the disclosed `parallel = TRUE` worker gap); D048 supersedes D047's readers bullet.

**Review:** Five passes; three defect returns fired thrash trigger (a) and one (b) on AC3,
and the re-cut split uniform absorption out as M70. 56 findings over three three-lens
fan-outs, all triaged and logged. Pass 4 blocked on a pre-existing FFmpeg 9
`normalize_audio()` regression, cleared by PR #73; pass 5 ran no fan-out at the maintainer's
gate call — ten criteria fresh, nothing actioned, CI green on nine.
