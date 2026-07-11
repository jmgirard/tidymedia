# M07: Complete the blessed multi-input verbs

- **Status:** in-progress <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

Finish the D003-named blessed multi-input set. `ffm_hstack()` and `ffm_concat()`
exist; `ffm_vstack()` and `ffm_overlay()` are named in D003 but missing. Both are
video→`[vout]` verbs that fit the existing `-filter_complex` compile path with no
structural change, so this milestone closes the gap cheaply and gives the engine
verbs their first research-facing Layer-2 front doors (side-by-side comparison,
picture-in-picture) — common needs in behavioral/annotation workflows.

## Scope

**In:**
- `ffm_vstack()` — vertical mirror of `ffm_hstack()` (`shortest`, `resize` to
  match widths for the two-input case). Same self-labelled/label-free token model.
- `ffm_overlay()` — composite input 1 over input 0 at `x`/`y` (numbers or FFmpeg
  expressions), with `shortest`. Two inputs → one `[vout]`.
- Refactor the arity + "must precede other video filters" guards into one internal
  helper shared by hstack/vstack/overlay (Layer-1 "once", D002).
- Layer 2 `compare_videos()` (built on hstack/vstack; `direction`, `audio`
  passthrough, `run`) and `picture_in_picture()` (built on overlay; corner
  `position`, `scale`, `audio` passthrough, `run`).
- Audio stays **explicit-map-only** at Layer 1 (video-only `[vout]`); Layer-2
  verbs expose an `audio =` arg that resolves to `ffm_map()`.

**Out:**
- `xstack` (grid) and `amix` (audio mix) — remain Layer-0 escape hatch. `amix`
  outputs audio, which would force generalizing the `[vout]`-only complex path to
  an `[aout]` output; that engine change is a separate future milestone.
- Any multi-/audio-output generalization of the complex-compile path.
- The concat *filter* (re-encode concat); filtergraph DAGs (D003, forever out).

## Acceptance criteria

- [ ] `ffm_vstack()` and `ffm_overlay()` exported + documented; `document()` clean,
      `man/` regenerated with no diff drift.
- [ ] Compile snapshots (`_snaps/ffm.md`) show both verbs emitting
      `-filter_complex "…[vout]" -map "[vout]"`; no `-filter_complex:` invalid form.
- [ ] Unit tests cover: >1-input arity guard, "before other video filters" guard,
      and `ffm_args()`/`ffm_compile()` parity for both new complex pipelines.
- [ ] Execution tests (`skip_if` binaries absent) run vstack, overlay,
      `compare_videos()`, and `picture_in_picture()` through ffmpeg to nonempty output.
- [ ] Layer-2 verbs assemble via Layer 1 only (no hand-glued command strings);
      `audio =` passthrough verified in the compiled command.
- [ ] `devtools::test()` clean; `devtools::check()` 0 errors / 0 warnings.
- [ ] DECISIONS.md records the M07 scope choice (vstack+overlay blessed;
      xstack/amix deferred; amix's `[aout]` implication).

## Plan

- [x] T1: `ffm_vstack()` + tests (compile, guards, args parity) + snapshot.
- [x] T2: `ffm_overlay()` + tests (uses shared ordering guard) + snapshot.
- [x] T3: Extract shared blessed-video-verb ordering guard
      (`check_multi_input_ordering()`); hstack/vstack use it (overlay in T2).
- [x] T4: `compare_videos()` Layer-2 verb + tests (incl. execution skip_if).
- [x] T5: `picture_in_picture()` Layer-2 verb (+ `ffm_overlay(scale=)`) + tests.
- [ ] T6: `document()`; update multi-input vignette section if present; add
      DECISIONS.md entry; (NEWS.md at review).
- [ ] T7: Full `devtools::test()` + `devtools::check()` clean; finalize.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (vstack+overlay blessed; xstack/amix deferred;
  audio explicit-map-only; two Layer-2 verbs). Next: `/milestone implement M07`.
- 2026-07-10: T1/T3 — `ffm_vstack()` (width-matching resize) + shared
  `check_multi_input_ordering()` guard (hstack routed through it); tests green.
  Next: T2 `ffm_overlay()`.
- 2026-07-10: T2 — `ffm_overlay()` (raw x/y expr, exactly-two-input guard);
  tests green (150 pass). Next: T4 `compare_videos()`.
- 2026-07-10: T4 — `compare_videos()` (horizontal/vertical, resize default for
  2 inputs, `audio` index drops by default); tests green. Next: T5 PiP.
- 2026-07-10: T5 — `picture_in_picture()` + `ffm_overlay(scale=)` (scale2ref
  inset kept in Layer 1); corner/center positions, margin; execution tests pass.
  Next: T6 snapshots/docs/DECISIONS.

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- Blessed set for M07 = `vstack` + `overlay` only (both video→`[vout]`). `xstack`
  and `amix` stay Layer 0; `amix` deferred because audio output needs an `[aout]`
  engine generalization — promote this rationale to DECISIONS.md at implementation.
- Audio in stacked/overlaid output stays explicit-map-only at Layer 1 (consistent
  with D-M06-1); convenience lives in Layer-2 `audio =` args over `ffm_map()`.

## Review

Filled in by `/milestone review`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
