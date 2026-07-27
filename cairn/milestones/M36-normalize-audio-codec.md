# M36: `audio_codec` for `normalize_audio` (+ batch)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** —

## Goal

Give `normalize_audio()` and `normalize_audio_batch()` an `audio_codec` argument
so the loudness-normalized output's encoder is named explicitly instead of
falling to the local FFmpeg build's container default.

## Scope

**In:** An `audio_codec = NULL` sentinel arg on `normalize_audio()`
(`R/ffmpeg.R:1197`) and `normalize_audio_batch()` (`R/ffmpeg.R:2698`), threaded
through the single shared `normalize_audio_pipeline()` seam (`R/ffmpeg.R:1238`)
so single-pass, two-pass, scalar and batch all inherit one implementation.
`NULL` emits no `-codec:a`, preserving today's exact command; a named encoder
pins it. `audio_codec = "copy"` aborts with a `cli` error — this verb filters
the audio, so a stream copy is impossible and D017's `"copy"` default
deliberately does not transfer (RB tripwire: irreversible-api — settled at the
2026-07-26 plan gate, D-entry to be appended at implementation). Batch gains a
per-row `audio_codec` column (`NA` → unset) reusing `check_batch_codec_col(col =)`
(`R/ffmpeg.R:2916`) and `batch_codec_cell()` (`R/ffmpeg.R:2935`). Docs + NEWS.

**Out:** `separate_audio_video`'s codec args → M37. A `video_codec` arg here →
no row: the verb's documented contract is video-copy (`-codec:v copy`,
`R/ffmpeg.R:1264`), so changing it needs its own D-entry first. The two-pass
*analysis* pass → untouched; it measures and writes no output. Quality /
bitrate / rate-control knobs → the existing "Video quality / rate-control"
candidate row.

## Acceptance criteria

- [ ] AC1: `normalize_audio(f, out, audio_codec = "aac", run = FALSE)` compiles a
      command containing `-codec:a aac`; with the default the compiled command is
      byte-identical to the same call on the default branch.
- [ ] AC2: `audio_codec = "copy"` aborts from both the scalar verb and a batch
      column cell, with an error naming why a filtered stream cannot be copied.
- [ ] AC3: under `two_pass = TRUE` the returned correction command carries the
      same `-codec:a`, proving the shared-seam threading reaches both phases.
- [ ] AC4: `normalize_audio_batch()` honors a per-row `audio_codec` column with
      `NA` → unset, and rejects a wrong-typed column at both boundaries — a
      numeric column and an all-`NA` non-character column (M34 lesson).
- [ ] AC5: an execution test confirms the named encoder reaches the output
      (`probe_audio()` `codec_name`), `skip_if` the binaries are absent.
- [ ] AC6: profile `verify` clean — `devtools::test()` passes,
      `devtools::document()` produces no diff, `devtools::check()` reports 0
      errors / 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T4
- AC4 → T3
- AC5 → T5
- AC6 → T6

## Tasks

- [ ] T1: Thread an `audio_codec` parameter through
      `normalize_audio_pipeline()` (`R/ffmpeg.R:1238`) via `apply_audio_codec()`
      (`R/ffmpeg.R:1594`), adding the `"copy"` abort ahead of it. Tests first.
      Edit `R/ffmpeg.R` as bytes — it is the repo's only CRLF file (M35 lesson).
- [ ] T2: Add the arg + roxygen to `normalize_audio()` (`R/ffmpeg.R:1197`);
      assert default-command parity against the default branch.
- [ ] T3: Add the arg and per-row column to `normalize_audio_batch()`
      (`R/ffmpeg.R:2698`), reusing `check_batch_codec_col(col = "audio_codec")`
      and `batch_codec_cell()`; test both column-type boundaries.
- [ ] T4: Cover the two-pass correction path (`R/loudnorm_two_pass.R:318`) —
      confirm it inherits the codec through the shared pipeline, no second seam.
- [ ] T5: Execution test verifying the output's actual audio codec; `skip_if`
      binaries absent.
- [ ] T6: Roxygen `@examples`, `NEWS.md` entry, `devtools::document()`; append
      the `DECISIONS.md` entry extending D016/D017 to a verb where the filter
      forces a re-encode.

## Work log

- 2026-07-26: created by /milestone-plan.

## Decisions

## Review
