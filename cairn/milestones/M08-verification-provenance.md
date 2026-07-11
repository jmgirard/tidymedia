# M08: Verification & provenance

- **Status:** review   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M04, M06   <!-- both done -->
- **Branch/PR:** m08-verification-provenance · https://github.com/jmgirard/tidymedia/pull/9

## Goal

Make tidymedia's reproducible pipelines (D001) auditable: probe-backed output
assertions, a batch provenance manifest, and progress reporting for long runs —
turning "the command is reproducible" into "the *result* is verified and recorded."

## Scope

**In:**
- `verify_media()` — a standalone, probe-backed checker returning a tidy
  pass/fail tibble (one row per checked property).
- Verification wired into execution: `ffm_run(verify =)` aborts on a failed
  assertion; `ffm_batch(verify =)` records outcomes in a `verified` column
  without aborting.
- A batch provenance manifest via `ffm_manifest()`: per-job command,
  ffmpeg/ffprobe versions, timestamps, output size, and opt-in md5 checksums
  of inputs+outputs (tibble; optional `path =` writes CSV).
- Progress reporting in `ffm_batch(progress =)`.

**Out:**
- Content/perceptual quality checks (SSIM/PSNR, visual diffs) — structural
  metadata only.
- Any engine/object-model change or filtergraph work — D003 stays intact.
- Provenance for single `ffm_run()` beyond the `verify` hook (manifest is
  batch-only); no per-run progress bar.
- JSON manifest / any new hard dependency — CSV writer only (base `utils`).
- Hashes beyond md5, signing, and any automatic re-run/repair on failure.

## Acceptance criteria

- [x] `verify_media(file, duration=, width=, height=, video_codec=,
      audio_codec=, sample_rate=, ...)` returns a tidy tibble
      (`file`, `check`, `expected`, `actual`, `pass`); numeric checks honor a
      tolerance. Evidence: `test-verify.R` passing.
- [x] The comparison core is a **pure** function (no binaries) with CI-safe
      unit tests covering pass, fail, tolerance edges, and missing-stream
      cases. Evidence: those tests pass with ffprobe absent.
- [x] `ffm_run(object, verify = <spec>)` aborts with a `cli` error listing the
      failed checks when an assertion fails, and returns normally when all
      pass. Evidence: test (execution `skip_if_no_ffmpeg`; abort asserted via
      `expect_error` — a snapshot wasn't feasible since the check runs only
      after a real encode).
- [x] `ffm_batch(jobs, .f, verify = <spec|fn>)` adds a logical `verified`
      column (NA when not run/verified) without aborting on failure. Evidence:
      `test-ffm-batch.R` additions.
- [x] `ffm_manifest(batch_result)` returns a manifest tibble with command,
      ffmpeg/ffprobe versions, timestamp, output size; `checksums = TRUE` adds
      input/output md5 columns; `path =` writes a CSV. Evidence: tests
      (assembly + checksum logic CI-safe; version capture `skip_if`).
- [x] `ffm_batch(progress = TRUE)` drives a `cli` progress bar and does not
      error non-interactively. Evidence: test that a progress run completes.
- [x] `devtools::check()` is 0 errors / 0 warnings; `devtools::test()` clean;
      `man/` regenerated, NEWS.md updated, `_pkgdown.yml` lists new exports.

## Tasks

- [x] T1: Pure comparison core + `verify_media()`. Factor a binary-free
      `compare_expectations(expected, actual)` (CI-safe tests) under
      `verify_media()`, which probes the output via the M04 layer
      (`probe_container`/`probe_video`/`probe_audio`) and assembles the tibble.
      New `@family verification functions`.
- [x] T2: Wire verification in. `ffm_run(verify = NULL)`: after a successful
      run, resolve the spec against `object$output` and abort on any failed
      check. `ffm_batch(verify = NULL)`: `verify` is a named list (all jobs) or
      a function of the job columns (pmap-style, mirroring `.f`); collapse each
      job's checks to a `verified` logical, recorded like `success`.
- [x] T3: Provenance manifest. Internal `tool_versions()` (parse `-version`),
      per-job capture from the pipeline objects (paths from `$input`/`$output`),
      optional `tools::md5sum()` checksums. `ffm_batch(manifest = FALSE,
      checksums = FALSE)` attaches the manifest; `ffm_manifest(x, path = NULL)`
      reads it (helpful error if absent) and optionally writes CSV via `utils`.
- [x] T4: Progress reporting. `ffm_batch(progress = FALSE)`: `cli` progress bar
      on the sequential path; `.progress` for the furrr parallel path.
- [x] T5: Docs & polish. `devtools::document()`, NEWS.md under the dev heading,
      a "Verification & provenance" section in `_pkgdown.yml`, `check()` green.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-10: Milestone planned (API + failure/manifest/progress decisions
  settled at the plan question gate); ready to implement.
- 2026-07-11: Migrated from `project/milestones/` into cairn (adopt-in-place,
  reformatted to cairn template); status and content unchanged.
- 2026-07-11: Implement started; branch `m08-verification-provenance` cut from
  pushed master.
- 2026-07-11: Pre-impl gate settled — tolerance is uniform absolute (default
  `0.1`, `abs(actual-expected) <= tolerance`, so integer dims match exactly and
  duration gets slack); `...` extras resolve container→video→audio, first match
  wins; manifest opt-in (`manifest = FALSE`).
- 2026-07-11: T1 done — `R/verify.R` with pure `compare_expectations()` core +
  `verify_media()`; `test-verify.R` (28 tests, pure ones CI-safe); pkgdown
  "Verification & provenance" section added.
- 2026-07-11: T2 done — `ffm_run(verify=)` aborts on a failed check via shared
  `verify_output()`; `ffm_batch(verify=)` records a `verified` column (list or
  job-column function spec) without aborting. Tests green (verify 31, batch 22).
- 2026-07-11: T3 done — `R/ffm_manifest.R` (`ffm_manifest()` + internal
  `build_manifest`/`tool_versions`/`parse_version_line`); `ffm_batch(manifest=,
  checksums=)` attaches an opt-in manifest. Assembly/checksum/parse tests
  CI-safe, version capture gated. `test-ffm-manifest.R` (24 tests).
- 2026-07-11: T4 done — `ffm_batch(progress=)` drives a `cli` progress bar
  (sequential `run_with_progress()` helper; `.progress` on the furrr path);
  no-op non-interactively. Batch tests green (25).
- 2026-07-11: T5 done — NEWS "Verification & provenance (M08)" section, pkgdown
  section (`verify_media`, `ffm_manifest`), `document()`; added auditable/
  checksums/keyframe/md to WORDLIST. Full suite 405 pass / 1 skip;
  `devtools::check()` 0 errors / 0 warnings / 0 notes. Status → review.
- 2026-07-11: Review — draft PR #9; fresh evidence gathered (suite green,
  check 0/0/0, consistency gate clean). Independent Opus review: no blockers;
  1 should-fix + 3 nits all fixed on branch (empty-spec batch abort, throwing
  verify → NA, sci-notation display, resolve_batch_verify CI-safe tests).

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

- Verify API is a standalone `verify_media()` primitive wired into `ffm_run`/
  `ffm_batch` via `verify =`; no `ffm_expect()` verb, so the engine object
  stays command-only (D003). (plan gate, 2026-07-10)
- Failure policy: `ffm_run()` aborts (mirrors its FFmpeg-exit abort, M06);
  `ffm_batch()` records in `verified`, consistent with `success`. (plan gate)
- Manifest is batch-only, checksums opt-in (`checksums = FALSE`), CSV output —
  no new JSON dependency. (plan gate, 2026-07-10)
- Probing is impure; the comparison core is pure and CI-safe (D004). (plan gate)

## Review

_Reviewed 2026-07-11; PR [#9](https://github.com/jmgirard/tidymedia/pull/9)._

**Acceptance criteria (fresh evidence).** All 7 met.
1. `verify_media()` tidy tibble + tolerance — `test-verify.R` green.
2. Pure `compare_expectations()` core, CI-safe (pass/fail/tolerance/missing) —
   green with ffprobe not required.
3. `ffm_run(verify=)` aborts / returns normally — green (execution gated).
4. `ffm_batch(verify=)` `verified` column, never aborts — green.
5. `ffm_manifest()` (+ checksums, CSV) — green (assembly CI-safe, capture gated).
6. `ffm_batch(progress=)` completes non-interactively — green.
7. Full suite 405+ pass / 1 pre-existing skip; `devtools::check()` 0/0/0;
   `man/` regenerated; NEWS + `_pkgdown.yml` updated.

**Consistency gate.** `document()` no diff; README.Rmd untouched (in sync);
`pkgdown::check_pkgdown()` clean; NEWS entry present; no new top-level files
(check() 0 notes). NEWS heading keeps the `(M08)` tag to match the existing
dev-NEWS sections (M01–M07) — user-facing milestone tags are consolidated out
at release (M10), per repo precedent.

**Independent review (fresh-context Opus).** No blockers; all criteria met.
Findings triaged — all fixed on the branch:
- (should-fix) empty `verify` spec from a *function* aborted the batch,
  breaking the never-abort contract → specs now resolved before running and an
  empty/unnamed spec is rejected fast with a clear message.
- (nit) a throwing per-job verify aborted the batch → wrapped in `tryCatch`,
  recorded as `NA`.
- (nit) large numbers rendered as `1e+06` in the report → `format_check_value()`
  now formats numerics without scientific notation.
- (nit) `resolve_batch_verify` lacked direct CI-safe tests → added.
