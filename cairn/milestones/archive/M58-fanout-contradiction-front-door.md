# M58: Six argument contradictions are refused at the fan-out verb's front door

**Status:** done (2026-08-07, PR #61 https://github.com/jmgirard/tidymedia/pull/61)

**Goal:** Make each of the six argument-contradiction aborts inside a fan-out
verb's pipeline report from the verb the user called, not from `purrr::pmap()`.

**Outcome:** Five shared checkers — `check_hardware_needs_encode()`,
`check_codec_needs_reencode()`, `check_audio_codec_needs_reencode()`,
`check_audio_codec_needs_audio()` (conditions 4 and 6, parameterized on `hint`)
and `check_resize_needs_two_inputs()` — each the one site its abort is worded.
The four `*_pipeline()` functions call them; five fan-out verbs also call them
at their front doors, row-swept via a new `batch_arg_rows()`. M57's `reencode`
gating, its `encoding` row-scoping and `separate_audio_video_batch`'s
`Filter("copy")` retire as dead; `compare_videos_pipeline()`'s resize abort
gained `call = call`. Evidence: `data-raw/contradiction-guard-baseline.R`.

**Decisions:** D036 (cross-cutting). Milestone-local: checkers take one row's
resolved values and the front door sweeps rows, over a table-aware checker
needing a one-row stand-in in the single-call pipelines.

**Review:** Three lenses; blame and prior-review clean, diff-bug returned 13.
Actioned F10 (80, roxygen naming a contradiction two verbs lack), F6 (85) and
F3 (80) — guards preempting `ffm_batch()`'s checks and a per-row range check,
not reorderable, so disclosed and pinned. Fixed F1 (72) and F9 (50); 9 logged.
