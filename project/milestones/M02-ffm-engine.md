# M02: ffm builder engine rework (real command model)

- **Status:** review <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

The Layer 1 `ffm` object is a bag of pre-rendered string fragments with
baked-in flags and spaces (`overwrite = "-y "`, `codec_video =
"-codec:v libx264 "`); `ffm_compile()` just concatenates them in a fixed
order. Every known builder bug is a symptom of that model, and one output
(`-filter_complex:v`) is invalid syntax that cannot run. This milestone
replaces the fragment model with a **structured argument model** — the engine
becomes the single correct place command assembly, quoting, and
simple-vs-complex filter selection live (per D002/D003). It is the foundation
M03 (task verbs on the builder) depends on, so it must be correct and
executable, not merely plausible.

## Scope

**In:**
- Rework the `ffm` S3 object and `ffm_compile()` to a structured model:
  global options, per-input options, an ordered filter chain (with stream
  labels), and output options — no pre-baked flags/spaces in stored fields.
- Correct argument positioning: global opts before inputs; stream-drop and
  codec/pixel/map/filter opts as **output** options after inputs, before the
  output file.
- Filter emission split: single-input sequential chains → `-vf` / `-af`;
  multi-input blessed verbs (hstack, and the label plumbing that concat/
  overlay will reuse) → `-filter_complex` with explicit `[0:v][1:v]` labels
  and an auto `-map` of the final output label.
- Fix the four deferred bugs: (1) `ffm_trim(setpts = FALSE)` must not append
  setpts; (2) `ffm_drop()` flags positioned as output options, not before
  `-i`; (3) `ffm_pixel_format()` spacing; (4) valid filter flags (no
  `-filter_complex:v`).
- Remove dead fields (`trim_start`/`trim_end` in `new_ffm()`).
- Rewrite `tests/testthat/test-ffm.R` to assert **correct** output (replacing
  the KNOWN-BUG characterizations) + full-command snapshots; add one
  binary-gated end-to-end execution test.

**Out:**
- Layer 2 task verbs (`extract_audio()` etc. in `R/ffmpeg.R`) — they still
  glue their own strings and are migrated onto the builder in **M03**. Not
  touched here.
- New user-facing verbs (concat, overlay, vstack, audio filters, drawtext,
  fade, etc.). Only the internal label machinery that makes multi-input
  correct is in scope; new verbs are later work.
- Migration to S7 or any other OOP system (D-M02-1: stay S3).
- Automatic copy-vs-re-encode inference (e.g. auto `-c copy` when no filters).
  Copy stays explicit via `ffm_copy()`; smart inference is out of scope.
- Layer 0 wrappers (`ffmpeg()`, `ffprobe()`, `mediainfo()`) and metadata
  tibbles (M04).

## Acceptance criteria

Each criterion must be verifiable with evidence at review time.

- [ ] `ffm_trim(setpts = FALSE)` produces a filter chain with **no** setpts
      filter; `setpts = TRUE` still adds it (test asserts both).
- [ ] `ffm_drop()` flags compile as output options positioned after `-i` and
      before the output file (not prepended before `-i`); test asserts order.
- [ ] `ffm_pixel_format()` compiles with correct spacing — no token abuts the
      output filename; snapshot test confirms.
- [ ] No compiled command emits `-filter_complex:v` or `-filter_complex:a`;
      single-input chains use `-vf`/`-af`, multi-input uses `-filter_complex`
      with `[..]` labels + `-map`. Tests assert each path.
- [ ] `new_ffm()` no longer declares `trim_start`/`trim_end`; no field is
      write-once-never-read (verified by grep + passing suite).
- [ ] Snapshot tests cover the full compiled command for every `ffm_*` verb
      and representative chains; `devtools::test()` clean.
- [ ] A binary-gated (`skip_if` no ffmpeg) end-to-end test runs ffmpeg on a
      real trim+crop and an hstack, and asserts the output file exists and is
      non-empty.
- [ ] `devtools::check()` = 0 errors / 0 warnings / 0 notes.

## Plan

Tasks sized to one working session or less, ordered by dependency.

- [x] T1: Design + implement the structured `ffm` S3 model in `R/ffm_oop.R`
      (fields for global/input/filter-chain/output opts; drop dead
      `trim_start`/`trim_end`; keep `new_ffm()` `stopifnot` invariants).
- [x] T2: Rewrite `ffm_compile()` in `R/ffm.R` to assemble from the structured
      model with correct option positioning and quoting; single source of the
      `-vf`/`-af` vs `-filter_complex` decision + label/map plumbing.
- [x] T3: Update the verbs to write structured fields instead of pre-baked
      strings — `ffm_trim` (honour `setpts`), `ffm_drop` (output-opt
      position), `ffm_pixel_format` (spacing), `ffm_codec`, `ffm_map`,
      `ffm_copy`, `ffm_crop`, `ffm_scale`, `ffm_drawbox`.
- [x] T4: Fix `ffm_hstack()` onto the `-filter_complex` + labels + map path so
      multi-input actually compiles to a runnable command.
- [x] T5: Rewrite `test-ffm.R` — replace KNOWN-BUG assertions with correct
      expectations; add full-command snapshot tests for every verb.
- [x] T6: Add binary-gated end-to-end execution test (real trim+crop, hstack).
- [x] T7: `devtools::document()` (no roxygen change); `devtools::test()` +
      `devtools::check()` clean; NEWS entry at review.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (structured model, vf/af split, keep S3,
  gated E2E acceptance). Next: `/milestone implement M02`.
- 2026-07-10: Implemented T1–T7 on branch. Structured S3 model + rewritten
  `ffm_compile()`; all 4 bugs fixed; hstack on filter_complex path; copy+filter
  guard; `-y` global. test 88 pass/0 fail (E2E ran, ffmpeg local); check clean
  (0/0/0). Next: review recap gate.
- 2026-07-10: Review — pushed branch, draft PR #2. Opus review: no blockers;
  applied F1/F2/F4/F5, deferred F3/F6 with reason. test 87 pass/0 fail/0 skip;
  check 0/0/0; NEWS updated. Awaiting final merge approval.

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- D-M02-1 (2026-07-10): Keep the `ffm` object as plain S3; no S7 migration in
  this milestone (kept low-risk, dependency-free).
- D-M02-2 (2026-07-10): Filter emission is split — single-input sequential
  chains compile to `-vf`/`-af`; any multi-input (blessed verb) compiles to
  `-filter_complex` with explicit `[..]` labels + auto `-map`. Candidate for
  promotion to DECISIONS.md at review if it holds up in implementation.
- D-M02-3 (2026-07-10): Acceptance requires a binary-gated end-to-end ffmpeg
  execution test, not string assertions alone — the command must run.
- D-M02-4 (2026-07-10): `ffm_hstack()` output is video-only for now — auto-map
  the stacked `[vout]` only, no audio. Audio passthrough deferred as a feature.
- D-M02-5 (2026-07-10): `ffm_compile()` aborts (cli) when a stream is set to
  codec `copy` while a filter targets that same stream — validation, not
  inference.
- D-M02-6 (2026-07-10): `-y`/`-n` emitted as a global option before `-i`.

## Review

- Criteria verification: all 8 acceptance criteria met with fresh evidence —
  setpts omitted when FALSE; drop flags after `-i`; pixel_format spacing; no
  `-filter_complex:v` (vf/af vs labelled filter_complex); dead fields gone;
  full-command snapshots; gated E2E ran real ffmpeg (trim+crop, hstack,
  hstack-resize); check 0/0/0.
- check()/test()/coverage results: `devtools::test()` 87 pass / 0 fail / 0 skip
  (ffmpeg present locally, all E2E ran); `devtools::check()` 0 errors / 0
  warnings / 0 notes (24.2s). PR #2 (draft).
- Opus review: no blockers; fixed F1 (resize graph newlines → single line,
  now snapshot + gated E2E), F2 (guard: stacking before other video filters),
  F4 (test exercises `-af` + audio copy guard), F5 (check_ffm before field
  reads). Deferred with reason: F3 (complex path overrides a user `ffm_map()`
  — documented limitation, hstack is video-only/terminal), F6 (shQuote paths —
  platform-dependent, breaks snapshot determinism; low risk).
- Known limitation: an explicit `ffm_map()` set before a stacking verb is
  superseded by the auto `-map "[vout]"`. Follow-up candidate: robust path
  quoting (F6) if untrusted filenames become a concern.
- Follow-ups spawned: none (all in-scope fixes applied on branch).
