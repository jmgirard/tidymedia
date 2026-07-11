# M08: Verification & provenance

- **Status:** planned <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

Make tidymedia's "reproducible pipelines" (D001) auditable. Sitting on the safe
execution path (M06) and the metadata tibbles (M04), add probe-backed output
assertions (did the encode actually produce the intended duration, size, codec,
sample rate?), a batch provenance manifest (commands, tool versions, timestamps,
optional file checksums), and progress reporting for long batch runs. This turns
"the command is reproducible" into "the *result* is verified and recorded."

## Scope

**In:**
- `verify_media()` — a standalone, probe-backed checker returning a tidy
  pass/fail tibble (one row per checked property).
- Verification wired into execution: `ffm_run(verify =)` aborts on a failed
  assertion; `ffm_batch(verify =)` records outcomes in a `verified` column
  without aborting.
- A batch provenance manifest: per-job command, ffmpeg/ffprobe versions,
  timestamps, output size, and (opt-in) md5 checksums of inputs+outputs.
  Delivered via `ffm_manifest()` (tibble; optional `path =` writes CSV).
- Progress reporting in `ffm_batch(progress =)`.

**Out:**
- Content/perceptual quality checks (SSIM/PSNR, visual diffs) — structural
  metadata only.
- Any engine/object-model change or filtergraph work — D003 stays intact.
- Provenance for single `ffm_run()` beyond the `verify` hook (manifest is
  batch-only); no per-run progress bar.
- JSON manifest / any new hard dependency — CSV writer only (base `utils`).
- Hashes beyond md5, signing, and any automatic re-run/repair on failed
  verification.

## Acceptance criteria

- [ ] `verify_media(file, duration=, width=, height=, video_codec=,
  audio_codec=, sample_rate=, ...)` returns a tidy tibble
  (`file`, `check`, `expected`, `actual`, `pass`); numeric checks honor a
  tolerance. Evidence: `test-verify.R` passing.
- [ ] The comparison core is a **pure** function (no binaries) with CI-safe
  unit tests covering pass, fail, tolerance edges, and missing-stream cases.
  Evidence: those tests pass with ffprobe absent.
- [ ] `ffm_run(object, verify = <spec>)` aborts with a `cli` error listing the
  failed checks when an assertion fails, and returns normally when all pass.
  Evidence: test (execution `skip_if_no_ffmpeg`; error message via snapshot on
  the pure path where feasible).
- [ ] `ffm_batch(jobs, .f, verify = <spec|fn>)` adds a logical `verified`
  column (NA when not run/verified) without aborting on failure. Evidence:
  `test-ffm-batch.R` additions.
- [ ] `ffm_manifest(batch_result)` returns a manifest tibble with command,
  ffmpeg/ffprobe versions, timestamp, output size; `checksums = TRUE` adds
  input/output md5 columns; `path =` writes a CSV. Evidence: tests (assembly +
  checksum logic CI-safe; version capture `skip_if`).
- [ ] `ffm_batch(progress = TRUE)` drives a `cli` progress bar and does not
  error non-interactively. Evidence: test that a progress run completes.
- [ ] `devtools::check()` is 0 errors / 0 warnings; `devtools::test()` clean;
  `man/` regenerated, NEWS.md updated, `_pkgdown.yml` lists the new exports.

## Plan

- [ ] T1: Pure comparison core + `verify_media()`. Factor a binary-free
  `compare_expectations(expected, actual)` (CI-safe tests) under `verify_media()`
  which probes the output via the M04 layer (`probe_container`/`probe_video`/
  `probe_audio`) and assembles the tibble. New `@family verification functions`.
- [ ] T2: Wire verification in. `ffm_run(verify = NULL)`: after a successful
  run, resolve the spec against `object$output` and abort on any failed check.
  `ffm_batch(verify = NULL)`: `verify` is a named list (applied to all jobs) or
  a function of the job columns (pmap-style, mirroring `.f`); collapse each job's
  checks to a `verified` logical, recorded like `success`.
- [ ] T3: Provenance manifest. Internal `tool_versions()` (parse `-version`),
  per-job capture from the pipeline objects (input/output paths from `$input`/
  `$output`), optional `tools::md5sum()` checksums. `ffm_batch(manifest = FALSE,
  checksums = FALSE)` attaches the manifest; `ffm_manifest(x, path = NULL)`
  reads it (helpful error if absent) and optionally writes CSV via `utils`.
- [ ] T4: Progress reporting. `ffm_batch(progress = FALSE)`: `cli` progress bar
  on the sequential path; `.progress` for the furrr parallel path.
- [ ] T5: Docs & polish. `devtools::document()`, NEWS.md under the dev heading,
  add a "Verification & provenance" section to `_pkgdown.yml`, `devtools::check()`
  to green.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (API + failure/manifest/progress decisions
  settled at the plan question gate); ready to implement.

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- Verify API is a standalone `verify_media()` primitive wired into `ffm_run`/
  `ffm_batch` via `verify =`; no `ffm_expect()` verb, so the engine object stays
  command-only (D003). (plan gate, 2026-07-10)
- Failure policy: `ffm_run()` aborts (mirrors its FFmpeg-exit abort, M06);
  `ffm_batch()` records in `verified`, consistent with `success`. (plan gate)
- Manifest is batch-only, checksums opt-in (`checksums = FALSE`), CSV output —
  no new JSON dependency. (plan gate, 2026-07-10)
- Probing is impure; the comparison core is pure and CI-safe (D004). (plan gate)

## Review

Filled in by `/milestone review`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
