# M03: Task verbs rebuilt on the builder + batch support

- **Status:** review <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

Make Layer 2 real: every task verb becomes a thin wrapper over the Layer 1
`ffm_*` builder (D002) instead of gluing its own command string, and add the
D001 headline feature — batch processing over many files. Delivers the
package's reason to exist: reproducible, inspectable compiled commands run
tidily across a whole folder of media.

## Scope

**In:**

- Small, reused engine primitives that unblock the migration:
  - `ffm_concat()` — blessed concat-demuxer verb (D006 anticipated it).
  - Copy-safe **seek** (`-ss`/`-to` as options, distinct from the trim
    *filter*) so trims/segments can stream-copy without re-encoding.
  - Controlled output-options passthrough so a verb can add specific flags
    (e.g. `extract_frame` quality flags) while `ffm_compile()` still owns
    positioning/quoting — not full-command gluing.
- `ffm_batch(jobs, .f, ..., run, parallel)`: tibble-in/tibble-out runner;
  `.f` builds an ffm pipeline per row; returns the original columns plus the
  compiled `command` and (when run) a status column; opt-in `furrr`
  parallelism; dry-run returns commands without executing.
- Migrate single-output verbs onto the builder with a consistent `run`
  (dry-run) arg returning the compiled command: `extract_audio`,
  `audio_as_mp3`, `crop_video`, `format_for_web`, `extract_frame`.
- Rebuild fan-out verbs as batch/multi-pipeline wrappers (single-output per
  job, D003 intact): `segment_video` (jobs tibble -> `ffm_batch` + seek/copy),
  `separate_audio_video` (two single-output pipelines).
- Rebuild `concatenate_videos()` on `ffm_concat()`.

**Out:**

- overlay / vstack and any other new engine verbs — later milestone.
- `get_volume()` and all audio-analysis / metadata work — M04.
- `get_codecs()` / `get_encoders()` introspection — stay Layer 0 helpers,
  unchanged.
- Multi-output *engine* model — D003 stands; fan-out lives in Layer 2.
- New user-facing filters beyond those above.

## Acceptance criteria

Each verifiable with evidence at review time.

- [x] No task verb in `R/ffmpeg.R` assembles an ffmpeg command by string glue:
      each delegates to the builder / `ffm_compile()`. Only residual `glue` is
      the Layer 0 `ffmpeg()` executor (line 15) and the out-of-scope unexported
      `get_volume()` (M04).
- [x] `ffm_concat()` and seek/passthrough primitives compile to correct
      commands — pure snapshot tests (CI-safe, no binaries).
- [x] `ffm_batch()` maps a pipeline over a jobs tibble and returns a tibble
      with `command` (+ `success` when run); dry-run returns commands without
      executing — covered by CI-safe tests.
- [x] `segment_video`, `separate_audio_video`, `concatenate_videos` produce
      runnable commands — compile snapshots + binary-gated E2E.
- [x] `devtools::test()` clean (190 pass, 4 mediainfo skips); local
      `devtools::check()` 0/0/0. CI green on all platforms verified at review.
- [x] `NEWS.md` updated (0.0.0.9002 development version).

## Plan

Tasks sized to one working session or less, ordered by dependency.

- [x] T1: Engine — controlled output-options passthrough slot + `ffm_compile()`
      positioning + tests.
- [x] T2: Engine — copy-safe seek (`-ss`/`-to` options) + tests.
- [x] T3: Engine — `ffm_concat()` (concat demuxer) + compile support + tests.
- [x] T4: `ffm_batch()` runner (tibble in/out, `furrr` opt-in, dry-run) + tests.
- [x] T5: Migrate single-output verbs (`extract_audio`, `audio_as_mp3`,
      `crop_video`, `format_for_web`, `extract_frame`) with consistent `run`
      + tests.
- [x] T6: Rebuild fan-out/concat verbs (`segment_video` via batch+seek,
      `separate_audio_video` as two pipelines, `concatenate_videos` via
      `ffm_concat`) + binary-gated E2E.
- [x] T7: `devtools::document()`, `NEWS.md`, README check, final
      `devtools::check()`.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (batch = tibble runner; segment_video via
  batch+seek; ffm_concat added; breaking changes allowed).
- 2026-07-10: Implementation started; branched milestone/M03-task-verbs. Open
  gate item raised by user: copy-cut keyframe accuracy must not corrupt output.
- 2026-07-10: T1-T3 engine primitives done (ffm_output_options, ffm_seek
  accurate/copy modes, ffm_concat demuxer). Tests + snapshots green; E2E
  confirms accurate seek hits 4.0s and copy seek starts at pts 0. Next: T4 batch.
- 2026-07-10: T4-T7 done. ffm_batch runner; all task verbs migrated onto the
  builder (no command gluing); segment_video via batch+seek, separate/concat
  rebuilt. NEWS + version bump to 9002. test 190 pass; check 0/0/0. Status
  -> review (branch not yet pushed).

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- D-M03-1: Batch is a tibble-in/tibble-out runner (`ffm_batch`), not verb
  vectorization; scalar verbs stay scalar (user pick).
- D-M03-2: Fan-out (segment/separate) is a Layer 2 concern implemented via
  multiple single-output pipelines; the engine stays single-output (D003).
- D-M03-3: Seek (`-ss`/`-to` options) is separate from the trim *filter* so
  copy-based cuts avoid the D-M02-5 copy+filter guard (no forced re-encode).
- D-M03-4: Seek is a new `ffm_seek(start, end, reencode)` verb, not a mode on
  `ffm_trim` (implement-gate pick).
- D-M03-5: Cutting defaults to `reencode = TRUE` (frame-exact). `reencode =
  FALSE` is a documented, correctly-built fast/lossless copy path (input-seek
  `-ss` before `-i` + `-avoid_negative_ts make_zero`), which snaps to
  keyframes. Empirically the old output-seek-copy path returned a 3.08s clip
  for a requested 4.00s and shifted pts to 1.0 — that broken path is removed.
- D-M03-6: `ffm_batch(.f=)` receives each job row pmap-style (columns as named
  args) and returns an ffm pipeline. Task verbs gain `run = TRUE` and return
  the compiled command (invisibly, after executing, when run).

## Review

Filled in by `/milestone review`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
