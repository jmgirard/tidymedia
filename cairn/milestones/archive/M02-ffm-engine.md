# M02: ffm builder engine rework (real command model) (done 2026-07-10)

**Goal:** replace the Layer 1 `ffm` string-fragment model (pre-baked flags +
spaces, concatenated in fixed order) with a structured command model so
`ffm_compile()` is the single place assembly, quoting, and simple-vs-complex
filter selection live. Foundation for M03.

**Outcome (PR #2, squash-merged as f1cd262; CI green ×6):**
- `new_ffm()` stores unformatted values (logical `overwrite`, `drop` names,
  bare codec/pixfmt, filter chains, `complex` flag); dead `trim_start`/
  `trim_end` removed. `ffm_compile()` rewritten to position/quote everything.
- Fixed 4 deferred M01 bugs: `setpts=FALSE` honoured; `ffm_drop()` flags after
  `-i`; `ffm_pixel_format()` spacing; invalid `-filter_complex:v` gone
  (single→`-vf`/`-af`, multi-input→`-filter_complex` `[0:v][1:v]` + auto `-map`).
- `ffm_hstack()` runnable E2E, must precede other video filters; compile guard
  aborts on codec `copy` + a filter on that stream. Tests: correct expectations
  + full-command snapshots + gated E2E (trim+crop, hstack, resize). 87 pass;
  check 0/0/0.

**Key decisions:** D-M02-1 stay S3; D-M02-2 vf/af-vs-filter_complex split →
**promoted to DECISIONS D006**; D-M02-3 gated E2E; D-M02-4 hstack video-only;
D-M02-5 copy+filter guard; D-M02-6 `-y` global.
**Deferred (Opus review):** F3 explicit `ffm_map()` overridden by auto-map in
complex mode (documented); F6 `shQuote` path quoting (platform-dependent, breaks
snapshot determinism). Possible follow-up: assert output dims via ffprobe (M04).
