# M67: The encoder probe answers once per session, not once per row

**Status:** done (2026-08-09, PR #70 https://github.com/jmgirard/tidymedia/pull/70)

**Goal:** A `hardware = "nvenc"` call asks FFmpeg which encoders it has once per
session instead of once per row, without pinning a caller to a stale answer.

**Outcome:** A package-local capability memo (`R/cache.R`, the package's first
mutable session state), read inside `has_nvenc()` strictly BELOW its
`getOption("tidymedia.nvenc_encoders")` seam, so the option still wins
mid-session and never populates the memo. An N-row nvenc batch spawns one
`ffmpeg -encoders` instead of N; compiled commands unchanged. New export
`refresh_ffmpeg_capabilities()` discards it, as does `set_program()`;
`ffmpeg_encoders()`/`ffmpeg_codecs()` stay uncached. Tested by a
namespace-derived grid over all 16 exported verbs taking `hardware`, each cell
built from its own `formals()`, and a `setup-` file discarding per test.

**Decisions:** D044 (session lifetime, the two discard routes, per-process under
`parallel = TRUE`, why D034's falsifier is untripped, the GP1 trade the new
export takes). Plan gate rejected keying the memo on the resolved binary path,
and rejected seeding parallel workers through the option seam.

**Review:** Blame-history and prior-review found nothing; diff-bug returned 13,
scored by a fourth agent. F1 (85) fixed — the memo had made a stream-copy probe
guard vacuous; F2 (55) too. Eleven logged, F11 (78) and F5 (75) highest.
