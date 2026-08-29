# M082: The track check has an off switch, and says what it costs

**Status:** done (2026-08-28, PR #86 https://github.com/jmgirard/tidymedia/pull/86)

**Goal:** Give the dropped-audio-track diagnostic a session-wide off switch, and make every verb that runs it say what it costs.

**Outcome:** `tidymedia.check_tracks` is the third option seam, default `TRUE`,
read by `resolve_check_tracks()` (`R/timeout.R`) through `rlang::check_bool()`,
which aborts naming the option on a malformed value. All seven probe sites gate
on it: the four scalar sites as the last conjunct of
`isTRUE(run) && is.null(audio_stream)`, so a track-naming or `run = FALSE` call never reads it; the three `_batch` verbs share one early return inside `warn_dropped_audio_batch()`. `carried_option_values()` carries it raw into
`parallel = TRUE` workers, like the encoder override rather than the timeout.
`count_audio_streams_all()` gained a `progress` argument, `TRUE` only at the
batch site, driving one `cli` bar over the inputs the sweep visits. The six
verbs' help states the cost, the per-row exemption, both switch-off forms and
the serial front-door sweep; `?tidymedia` gained a *Session options* section.

**Decisions:** D060 (seam and bar inside D024's licence) and D061 (the bar does reach D024's outcome clause on the ran-vs-skipped axis, superseding one sentence of D060).

**Review:** Two rounds, three lenses each. Round one, nine findings: F4 an
amendment return on AC4 (the bar counts the inputs the sweep visits, which
"N distinct inputs" did not bound); F1/F3/F5 fixed; F9/F6 to candidate rows;
F2/F7/F8 rejected. Round two, four: F10 fixed at the gate (the `_batch` topics
still overstated the cost on a mixed table); F11-F13 rejected. Nothing retired.
