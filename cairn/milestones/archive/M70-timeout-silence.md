# M70: No timeout is silent

**Status:** done (2026-08-26, PR #74 https://github.com/jmgirard/tidymedia/pull/74)

**Goal:** Close the gap M69 disclosed rather than fixed — some call paths absorbed a
wall-clock timeout with no warning at all, so a bounded hang was invisible.

**Outcome:** `count_audio_streams()` and `capture_version()` return an absorbed-timeout sentinel
instead of a silent `NA`; `count_audio_streams_all()` and `tool_versions()` warn once per call
(`tidymedia_probe_timeout`), counts and manifest value unchanged so D024's licence holds.
`ffm_batch()` warns once per run (`tidymedia_batch_timeout`) for timed-out jobs and verifications,
reaching the 15 `_batch` verbs and `segment_video()`; other failures keep their silent
`success = FALSE`. `probe_all()`'s body became `probe_all_impl(absorb =, call =)`, so
`tm_timed_out` is gone and `verify_media()` refuses via the shared body re-raising. One display
literal per program, asserted by `tm_program_literals()` over every spawn-wrapper call.
`?tidymedia` and `NEWS.md` state the rule and name the three classes, retiring M69's disclosure
with its guard. The domain is derived: `helper-timeout-sweep.R` closes the symbol-mention graph
over `system`/`system2` to 53 exports and forces a timeout through each at the two spawn wrappers.

**Decisions:** D049 (supersedes D048's fourth and fifth bullets).

**Review:** Three lenses, 13 findings, no correctness bug. Three fixed on the branch (the new
warning classes undocumented against the package's own `suppressWarnings(classes =)` recipe; the
batch promise unqualified at `parallel = TRUE`; one cli `.arg` token); two rejected; eight
guard-strength items to a candidate row. Return floor not reached.
