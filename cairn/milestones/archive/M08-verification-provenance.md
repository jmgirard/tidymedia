# M08: Verification & provenance (done 2026-07-11)

**Goal.** Make reproducible pipelines auditable: probe-backed output
assertions, a batch provenance manifest, progress reporting.

**Outcome.** Shipped:
- `verify_media(file, duration=, width=, height=, video_codec=, audio_codec=,
  sample_rate=, ..., tolerance=0.1)` → tidy `file/check/expected/actual/pass`
  tibble; pure binary-free `compare_expectations()` core.
- `ffm_run(verify=<named list>)` aborts on a failed check; `ffm_batch(verify=
  <list|fn>)` records a `verified` column, never aborts.
- `ffm_manifest()` reads an opt-in per-job manifest (command, ffmpeg/ffprobe
  versions, timestamp, output size; opt-in md5) attached by
  `ffm_batch(manifest=, checksums=)`; `path=` writes CSV.
- `ffm_batch(progress=)` cli progress bar (`.progress` on the furrr path).

**Key decisions.** Uniform absolute tolerance (0.1); `...` extras resolve
container→video→audio; manifest opt-in; verification lives outside the engine
object — no `ffm_expect` verb (D011, reaffirms D003).

**Evidence.** Suite 405+ pass; `check()` 0/0/0; CI green all platforms.
Independent Opus review clean (empty-spec abort, sci-notation, tryCatch fixes
applied on branch).

**PR.** https://github.com/jmgirard/tidymedia/pull/9
