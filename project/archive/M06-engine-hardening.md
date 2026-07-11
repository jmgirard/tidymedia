# M06: Engine hardening & safe execution (done 2026-07-10)

**Goal:** retire the M02–M04 engine debts, chiefly shell-string execution.

**Outcome (PR #6, squash-merged as 1a7dcdf; CI 7/7 green):**
- `ffm_compile()` refactored onto shared `ffm_groups()`: one assembly renders
  the display string (byte-identical, snapshot-guarded) and internal
  `ffm_args()`. `ffm_run()`/`ffm_batch()`/all task verbs execute arg vectors
  via `run_program()` (no shell) — hostile paths (spaces, `'`, `$`, backticks)
  E2E-tested. `ffm_run()` now aborts with FFmpeg's exit status on failure.
- `ffm_map()` combines with auto `-map "[vout]"` in complex mode (M02 F3);
  `separate_audio_video()` stream-copies by default (`reencode=` opt-out);
  `check_token()` validation for codec/pixel-format; quoted
  `ffm_output_options()` groups rejected loudly.
- covr 0% root-caused: empty last-sorting `R/zzz.R` breaks covr's
  `split_on_line_directives` (reversed range → NA chunk). Deleted → coverage
  87.67% on CI. Upstream covr bug, not yet reported.
- Evidence: test 286 pass / 0 fail; check 0/0/0; NEWS → 0.1.0.9000.

**Key decisions:** D-M06-1 map combine; D-M06-2 args internal-only;
D-M06-3 cheap token sanity; D-M06-4 copy default. None promoted (layer-local).

**Deferred:** Windows quoting fix (`shQuote` cmd-style) is code-correct but
E2E-unverified — Windows CI lacks ffmpeg; consider installing it there.
Pre-existing `#TODO` at R/ffmpeg.R:556 (raw output cleanup) out of scope.
