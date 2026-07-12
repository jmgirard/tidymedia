# M12: Video standardization verb — done 2026-07-12

**Goal:** add `standardize_video()`, a Layer-2 verb re-encoding a video to a
reproducible format, plus the `ffm_fps()` Layer-1 filter it needs.

**Outcome:** two new exported functions (PR #14):
- `ffm_fps(object, fps)` — appends an `fps=<n>` video filter, validated via
  `check_dim` (positive number or FFmpeg framerate expression).
- `standardize_video(infile, outfile, width, height, fps, vcodec,
  pixel_format, run)` — a thin composition (IP1) of scale/fps/codec/pix_fmt +
  `+faststart`. Explicit-parameter defaults (`libx264`/`yuv420p`); single
  dimension preserves aspect via `-2`; audio stream-copied (`-c:a copy`); the
  default path floor-crops to even dimensions so odd-sized sources encode.

Full suite 513 pass; `devtools::check()` zero errors/warnings/notes; CI green.

**Key decisions:** the "standard" is an explicit-parameter default set, not a
named-`preset` bundle (T4 irreversible-api tripwire) — additive-forward and
keeps every knob visible in the command (D001); escalation declined.

**Review:** two findings, both reproduced with ffmpeg and fixed (user chose
"fix both now", via a gated AC2/AC3/AC4 amendment): audio doc mismatch (score
90) — bare re-encode transcoded audio, fixed with `-c:a copy`; odd-dimension
default crash (score 76) — 0-byte output on odd input, fixed with a
floor-to-even crop safeguard (format_for_web parity). Both regression-tested.
