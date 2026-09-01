# Design

<!-- Drafted by /cairn-init on 2026-07-11 from CLAUDE.md, DECISIONS.md, and a
     read of R/. Reviewed lightly — refine the prose, principles, and known
     issues as you see fit. Principles cite the legacy decision IDs (D001–D009)
     now living in cairn/DECISIONS.md. -->

## Purpose & scope

tidymedia is an R interface to FFmpeg and MediaInfo for **reproducible media
preprocessing in research / data-science pipelines** — batch trimming,
cropping, format standardization, and metadata extraction as tibbles. It is
deliberately *not* "all of ffmpeg in R" (D001): differentiators are batch
processing over many files, metadata as tibbles, and reproducible compiled
commands. Out of scope forever: chasing full ffmpeg option coverage,
realtime/streaming, and full filtergraph DAGs.

## Function families

- **Layer 0 — escape hatch:** `ffmpeg()`, `ffprobe()`, `mediainfo()` — raw
  argument passthrough to the CLIs. The only "faithful wrapper" (D002).
- **Layer 1 — engine (`ffm_*`):** the pipe builder (`R/ffm.R`, `R/ffm_oop.R`)
  and batch runner (`R/ffm_batch.R`). Construction/compilation (`ffm`,
  `ffm_compile`, `ffm_run`, `ffm_batch`), input/output (`ffm_copy`, `ffm_seek`,
  `ffm_map`, `ffm_drop`, `ffm_files`, `ffm_codec`, `ffm_pixel_format`,
  `ffm_output_options`), filters (`ffm_trim`, `ffm_crop`, `ffm_scale`,
  `ffm_fps`, `ffm_drawbox`, and the first audio filter `ffm_loudnorm`), and
  blessed multi-input verbs (`ffm_hstack`, `ffm_vstack`, `ffm_overlay`,
  `ffm_concat`).
- **Layer 2 — task verbs:** `extract_audio`, `extract_frame`, `crop_video`,
  `segment_video`, `separate_audio_video`, `concatenate_videos`,
  `format_for_web`, `standardize_video`, `normalize_audio`, `audio_as_mp3`,
  `compare_videos`, `picture_in_picture` — thin wrappers over Layer 1.
- **Metadata:** `probe_all/container/streams/video/audio` (ffprobe → tibbles),
  the `mediainfo_*` family, and `get_duration/framerate/height/width/samplingrate`.
- **Program management (`R/program_management.R`):** `find_*`/`set_*` locators
  for ffmpeg/ffprobe/ffplay/mediainfo, `set_program`, `get_codecs`,
  `get_encoders`, `install_on_win`.
- **Tidy-eval reexports & utils:** `enquo`/`enquos`/`as_label`/`as_name`/`:=`/
  `.data`; `pad_integers`, `convert_fractions` (flagged for cleanup by the M22
  naming/docs audit — the four quoting reexports + `:=` have no internal use;
  `.data` is used internally and stays).

## Conventions

- User-facing conditions use `cli::cli_abort()` / rlang checks; assertthat is
  being retired — never add new assertthat calls (D004).
- Command **compilation** is pure and CI-safe (no binaries). `run = FALSE`
  promises a *command*, not a binary-free call: a probe whose result enters the
  compiled command runs when the pipeline is built, `run` notwithstanding —
  today D013's two-pass analysis pass and the nvenc encoder resolver, the two
  the D034 grep finds. The `run = TRUE` path may additionally run D024's
  diagnostic probes, which never run under `run = FALSE`. Command **execution**
  tests `skip_if` the ffmpeg/mediainfo binaries are absent (D004, D024, D034).
- Batch is one tibble-in/tibble-out runner, `ffm_batch(jobs, .f, …)`; `.f`
  builds one pipeline per row (pmap-style), one reproducible command per job (D007).
- testthat 3e; usethis scaffolding; GitHub Actions CI. `man/` and `README.md`
  are generated (roxygen → `document()`; `README.Rmd` → `build_readme()`).

## Design principles

- IP1: **Three-layer separation (D002).** Layer 2 task verbs are thin
  wrappers that never glue their own command strings; all assembly, quoting,
  and copy-vs-re-encode logic lives once in Layer 1.
- IP2: **The builder stays linear (D003).** Layer 1 models a single input
  chain with sequential filters and one output, plus only the blessed
  multi-input verbs (hstack, vstack, overlay, concat). No filtergraph DAGs;
  those users get the Layer 0 escape hatch.
- IP3: **Blessed multi-input set is single-video-output (D006, D009).**
  Multi-input verbs ride the `-filter_complex … [vout]` path and manage their
  own stream labels; audio in stacked/overlaid output is explicit-map-only.
  `xstack` and `amix` stay Layer 0 (`amix` would require an `[aout]` engine
  generalization — a future milestone, not a bolt-on).
- GP1: **Scope discipline (D001).** Prefer refusing an ffmpeg feature over
  growing toward full coverage; tradeable only with an explicit decision.
- GP2: **Frame-accurate by default (D008).** Cutting re-encodes for
  frame accuracy by default; the lossless stream-copy fast path snaps to
  keyframes and is opt-in.

<!-- IP = inviolable (changing one needs a D-entry); GP = guiding (tradeable
     with stated justification). Promote/adjust as the design settles. -->

## Architecture (as it is)

One input chain → sequential filters → one output, compiled to a single
reproducible FFmpeg invocation. `ffm_compile()` selects the filter flag by
input arity: single-input chains emit `-vf`/`-af`; any blessed multi-input verb
sets the pipeline `complex` and emits `-filter_complex` with explicit
`[0:v][1:v]…` labels and an automatic `-map "[vout]"` (D006). Batch fans out
over a jobs tibble; scalar verbs stay scalar and fan-out verbs (e.g.
`segment_video`) emit multiple single-output pipelines (D007).

## Known issues

- Eleven arguments are refused below the verb the caller typed, and that position is
  accepted rather than pending a fix. `fallback` at the eight fan-out verbs is checked
  where the encoder question is asked, which D075 records as an accepted cost;
  `anonymize_video_batch()`'s `pixel_format` and `color`, and
  `normalize_audio_batch()`'s `channels` and `sample_rate`, are validated inside the
  per-row fan-out, so `purrr::pmap()` is blamed and a set `tidymedia.timeout` displaces
  them; `has_hardware_encoder()`'s `codec` reads no limit and refuses nothing; and six
  `verify_media()` arguments are refused by an assignment rather than a guard. Measured
  cell by cell by M096's member x formal x wrong-form census, each named with the frame
  that refused it; accepted at M096's post-merge hygiene pass rather than carried as
  planned work. `?tidymedia` discloses the fan-out members it names. The gate booleans
  and `ffm_batch()`'s `output` column are NOT here — they stay a ROADMAP candidate row.

- `check_dim()` accepts any `x`/`y` expression string across all dim verbs
  (noted as future hardening in the M07 review) — not a regression.
- API naming & docs assessed by M22 (`cairn/references/naming-docs-audit-M22.md`):
  overloaded `get_*` prefix, `acodec`/`vcodec` and `ts_start`/`ts_stop` arg drift,
  unused tidy-eval reexports, and thin `@seealso` cross-linking. The target scheme
  is proposed as draft D014; renames + gap-fill land in the M22 execution
  follow-up under a clean-break policy. Naming conventions will be added here when
  D014 is ratified.
- Four of M092's instrument tests bind less than the gap they close. `test-ffm-batch.R`'s
  `run_with_progress()` contract test asserts the properties of its own stub, never the
  `run_one` closure defined inline in `ffm_batch()`; the batch case-fold test in
  `test-separate-av-multitrack.R` asserts that no warning arrives rather than that the
  multi-track advice is absent from one; the two-pass status test in
  `test-normalize-audios-two-pass.R` asserts `tm_row_status` is a non-`NA` integer but
  never its value, so a wrong-but-non-`NA` status passes; and AC4's sweep over
  `test-ffmpeg-exit-condition.R` counts a lexical spelling, with the four repaired
  handlers carrying no suite-level regression guard. Each satisfies its acceptance
  criterion as written and each was raised at both of M092's review gates; the
  maintainer chose to hold the criteria rather than widen them. Accepted, not deferred
  — there is no candidate row and no fix planned (M092 review, findings 1-5; D072).
