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
  `ffm_drawbox`), and blessed multi-input verbs (`ffm_hstack`, `ffm_vstack`,
  `ffm_overlay`, `ffm_concat`).
- **Layer 2 — task verbs:** `extract_audio`, `extract_frame`, `crop_video`,
  `segment_video`, `separate_audio_video`, `concatenate_videos`,
  `format_for_web`, `audio_as_mp3`, `compare_videos`, `picture_in_picture` —
  thin wrappers over Layer 1.
- **Metadata:** `probe_all/container/streams/video/audio` (ffprobe → tibbles),
  the `mediainfo_*` family, and `get_duration/framerate/height/width/samplingrate`.
- **Program management (`R/program_management.R`):** `find_*`/`set_*` locators
  for ffmpeg/ffprobe/ffplay/mediainfo, `set_program`, `get_codecs`,
  `get_encoders`, `install_on_win`.
- **Tidy-eval reexports & utils:** `enquo`/`enquos`/`as_label`/`as_name`/`:=`/
  `.data`; `pad_integers`, `convert_fractions` (candidates for cleanup in M10).

## Conventions

- User-facing conditions use `cli::cli_abort()` / rlang checks; assertthat is
  being retired — never add new assertthat calls (D004).
- Command **compilation** is pure and CI-safe (no binaries); command
  **execution** tests `skip_if` the ffmpeg/mediainfo binaries are absent (D004).
- Batch is one tibble-in/tibble-out runner, `ffm_batch(jobs, .f, …)`; `.f`
  builds one pipeline per row (pmap-style), one reproducible command per job (D007).
- testthat 3e; usethis scaffolding; GitHub Actions CI. `man/` and `README.md`
  are generated (roxygen → `document()`; `README.Rmd` → `build_readme()`).

## Design principles

- **IP1 — Three-layer separation (D002).** Layer 2 task verbs are thin
  wrappers that never glue their own command strings; all assembly, quoting,
  and copy-vs-re-encode logic lives once in Layer 1.
- **IP2 — The builder stays linear (D003).** Layer 1 models a single input
  chain with sequential filters and one output, plus only the blessed
  multi-input verbs (hstack, vstack, overlay, concat). No filtergraph DAGs;
  those users get the Layer 0 escape hatch.
- **IP3 — Blessed multi-input set is single-video-output (D006, D009).**
  Multi-input verbs ride the `-filter_complex … [vout]` path and manage their
  own stream labels; audio in stacked/overlaid output is explicit-map-only.
  `xstack` and `amix` stay Layer 0 (`amix` would require an `[aout]` engine
  generalization — a future milestone, not a bolt-on).
- **GP1 — Scope discipline (D001).** Prefer refusing an ffmpeg feature over
  growing toward full coverage; tradeable only with an explicit decision.
- **GP2 — Frame-accurate by default (D008).** Cutting re-encodes for
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

- `check_dim()` accepts any `x`/`y` expression string across all dim verbs
  (noted as future hardening in the M07 review) — not a regression.
- API-surface cleanup pending (tidy-eval reexports, `pad_integers`,
  `convert_fractions`) — tracked as candidate M10.
