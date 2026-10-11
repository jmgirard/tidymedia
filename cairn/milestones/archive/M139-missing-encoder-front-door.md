# M139: A failed FFmpeg run names the encoder this build lacks

**Status:** done (2026-10-11, PR #143 https://github.com/jmgirard/tidymedia/pull/143)

**Goal:** A failed FFmpeg run whose codec this build cannot encode says so, naming the encoder and the function the caller typed, under a catchable class.

**Outcome:** `diagnose_missing_encoders()` (`R/ffmpeg.R`) runs only in `ffm_run()`'s non-zero-exit branch. It reads
`cached_encoder_names()` and the new `cached_encodable_codecs()` (`R/cache.R`), skips `"copy"`, unset codecs and
`hardware_encoder_names()`, and fails open. A diagnosed exit carries `tidymedia_encoder_unavailable` before
`tidymedia_ffmpeg_exit`, with `tm_encoder` and `tm_stream`. `ffm_finish()` takes `call` and sets it as the
pipeline's `tm_call` attribute. `ffm_batch()` warns once with `tm_row`. A listing read that reaches the limit gives
one `tidymedia_probe_timeout` warning (`tm_check = "encoders"`) per call, and `tm_no_diagnosis` skips later runs.
`check_hardware_available()`'s abort takes the class. `ffmpeg_encoders()` and `ffmpeg_codecs()` read with
`-hide_banner`. Help on `?ffm_run`, `?ffm_batch`, `?refresh_ffmpeg_capabilities`, `?tidymedia` and the
`tidymedia.timeout` page. Six accepted limits in `cairn/DESIGN.md` Known issues.

**Decisions:** none promoted. Re-cut at plan: two gate rounds chose a diagnosis after failure over a check before
the start, which refuses calls that run today (FFmpeg uses no encoder for an absent stream kind).

**Review:** Two three-lens passes with one defect return (the probe-timeout warning came once per row, against
D049) and one amendment return (AC2 narrowed to "compiles the same command and, from the same remembered encoder
listing, starts the same programs as many times", accepted by the user after two re-audits). 41 findings: 26
fixed, 1 amendment, 3 follow-ups absorbed into candidate rows (the unquoted path in `ffmpeg()`, the separation
track diagnostic over a diagnosed encoder, repeated failed listing reads), 8 rejected. Two claim audits corrected
13 lines. All 11 CI checks green. Merged before 0.2.2 was cut, at the user's choice.
