# M49: Finish D026 on `format_for_web()` and `normalize_audio()`

**Status:** done (2026-08-01, PR #52 https://github.com/jmgirard/tidymedia/pull/52)

**Goal:** Close D026's last gap by making `format_for_web()` and
`normalize_audio()` state which audio tracks they carry.

**Outcome:** Both emitted no `-map` and took whichever track carried the
container's DEFAULT disposition (the third, on a 3-track fixture).
`format_for_web()`/`_batch` take `audio_stream` under D026's every-track `NULL`.
`normalize_audio()`/`_batch` produce **one audio stream and no video** — a single
`-map 0:a:0` or `0:a:<n>`, no `-codec:v copy`, identical across every output
container — and the two-pass analysis maps the track the correction normalizes.
Breaking: normalize-and-keep-picture is gone (candidate row); a no-audio input
is now an FFmpeg error.

**Decisions:** D028 (first-track `NULL`: `0:a?` prints one JSON block per mapped
track while the parser reads `hit[[1]]`), D029 (container predicate, superseded),
D030 (audio-only; `?` dropped because FFmpeg reverts to default selection when
every optional map matches nothing).

**Review:** Three rounds; the first two sent it back, an unconditional
`-map 0:v?` then an enumeration missing six containers. Round 3 fixed 11
findings, top a scripted CRLF→LF rewrite that blinded `git blame` (H1, 90).
