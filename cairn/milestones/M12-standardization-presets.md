<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M12: Video standardization verb

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m12-standardization-presets · https://github.com/jmgirard/tidymedia/pull/14   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add `standardize_video()`, a Layer-2 verb that re-encodes a video to a
reproducible, consistent format (resolution, framerate, codec, pixel format),
introducing the `ffm_fps()` Layer-1 filter it needs.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** a new Layer-1 `ffm_fps(object, fps)` verb (appends the `fps=<n>`
video filter to `filter_video`, IP2-compliant single-input sequential
filter), and an exported `standardize_video(infile, outfile, ...)` that
composes `ffm_scale()` ([R/ffm.R:308](../../R/ffm.R)), `ffm_fps()`,
`ffm_codec()`, `ffm_pixel_format()`, and `+faststart` into one reproducible
command with documented, analysis-friendly defaults — a parameterized
generalization of `format_for_web()` ([R/ffmpeg.R:236](../../R/ffmpeg.R)).
Optional `width`/`height` (aspect preserved via scale's `-2` even-dimension
expression when only one is given), optional `fps`, overridable `vcodec` /
`pixel_format`. Composes existing Layer-1 verbs only (IP1) — no hand-glued
strings.

**Out:** all audio work — loudnorm, downmix, resample, sample-rate
standardization → the audio-normalization milestone (candidate; owns the new
Layer-1 audio-filter primitive). A batch sibling `standardize_videos()` →
candidate row (composable meanwhile via `ffm_batch()` directly). Named-preset
*bundles* (e.g. `preset = "archive"`) are not built here; the default
argument set is the one documented standard (see the API tripwire on T4).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1: `ffm_fps(object, fps)` appends exactly `fps=<fps>` to the video
      filter chain, compiling into the `-vf` list ([R/ffm.R:1006](../../R/ffm.R));
      a non-positive or non-numeric/non-string `fps` aborts with `cli::cli_abort`.
      Oracle: the compiled `-vf` string.
- [ ] AC2: `standardize_video()` with explicit `width`, `height`, `fps`,
      `vcodec`, `pixel_format` compiles to one command carrying `scale`,
      `fps`, `-c:v`, `-c:a copy`, `-pix_fmt`, and `-movflags +faststart` in the
      engine's order. Oracle: the full compiled command string against a
      hand-written expected value.
- [ ] AC3: Aspect handling & codec-valid dimensions — only `width` (or only
      `height`) supplied scales preserving aspect with even output dimensions
      (the other dimension emits `-2`); both supplied forces exact dimensions;
      neither supplied keeps the source resolution but applies an
      even-dimension safeguard (`crop` to the nearest even width/height, a
      no-op for already-even input, mirroring `format_for_web()`) so
      `yuv420p`/`libx264` always encodes. Oracle: compiled `scale=`/`crop=`
      expression.
- [ ] AC4: Defaults alone (`standardize_video(infile, outfile)`) produce a
      documented, deterministic standard — the same input yields a
      byte-identical command across runs, the output encodes successfully even
      for odd-dimensioned input, and the roxygen states what that standard is.
- [ ] AC5: Input validation — bad `fps`/`width`/`height` and a missing input
      file each abort with a `cli::cli_abort` message; `run = FALSE` returns
      the command without invoking FFmpeg.
- [ ] AC6 (execution; `skip_if` no ffmpeg): a standardized output file exists
      and, probed with the metadata verbs, has the requested framerate and
      width.
- [ ] `devtools::check()` clean (zero errors/warnings/notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T3, T4
- AC6 → T6
- check clean → T5, T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Tests first — `ffm_fps()` compilation/validation in test-ffm.R (emits
      `fps=<n>` in `-vf`; bad `fps` aborts) (AC1).
- [x] T2: Implement `ffm_fps()` in [R/ffm.R](../../R/ffm.R) (mirror `ffm_scale`;
      write `filter_video`); roxygen; `_pkgdown.yml` Layer-1 row; `document()`.
- [x] T3: Tests first — `standardize_video()` compilation in test-ffmpeg.R:
      full-arg parity, aspect cases, defaults determinism, validation/`run=FALSE`
      (AC2–AC5).
- [x] T4: Implement `standardize_video()` in [R/ffmpeg.R](../../R/ffmpeg.R) as a
      thin composition (IP1). **(RB tripwire: irreversible-api)** — settled:
      explicit-parameter default set (see Decisions); escalation declined.
- [x] T5: Roxygen documenting the standard; `_pkgdown.yml` Layer-2 row;
      `document()`; `test()`.
- [x] T6: Execution test (`skip_if` no ffmpeg) proving output fps/width via probe
      verbs (AC6); `check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (split from the research-verbs candidate; standardization family, video-only).
- 2026-07-12: T1+T2 — `ffm_fps()` Layer-1 verb (`check_dim`: number or framerate string), pkgdown row, docs; 4 tests.
- 2026-07-12: T3–T6 — `standardize_video()` Layer-2 verb (scale/fps/codec/pix_fmt/+faststart; explicit defaults libx264+yuv420p); compile/validation + execution tests; check clean.
- 2026-07-12: review send-back ("fix both now") — gated amendment (AC2 gains `-c:a copy`; AC3/AC4 add even-dim safeguard) + fixes for both findings; roxygen/NEWS/oracles updated; +2 regression tests.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (T4 tripwire, irreversible-api): the "standard" is an
  explicit-parameter default set, not a named-`preset` bundle — additive-forward
  (a `preset=` arg can be added later without breaking callers), keeps every
  knob visible in the command (D001), matches Scope. Defaults `libx264`/`yuv420p`
  (mirror `format_for_web()`), `+faststart` always on. User delegated the call.
- 2026-07-12 (review): audio is stream-copied (`-c:a copy`), not transcoded, and
  the default path floor-crops to even dimensions — both to match the documented
  intent and `format_for_web()` behavior (see review Findings 1–2).

## Review
<!-- owner: review · exclusive -->

- 2026-07-12 (/milestone-review M12, PR #14). AC evidence (post-amendment),
  oracles = compiled command strings:
  - AC1 ✓ `ffm_fps(p,30)`→`-vf "fps=30"`; `ffm_fps(p,0)` aborts.
  - AC2 ✓ full-args carries `scale,fps,-c:v libx264,-c:a copy,-pix_fmt yuv420p,
    -movflags +faststart` in order (matches hand-written oracle).
  - AC3 ✓ width-only `scale=w=640:h=-2`; height-only `scale=w=-2:h=480`; both
    `scale=w=640:h=480`; neither → even-dim `crop=floor(in_w/2)*2:...`.
  - AC4 ✓ two default calls byte-identical; default carries crop safeguard +
    `-c:a copy`; roxygen states the standard.
  - AC5 ✓ `width=0`/`fps=-1`/missing input each abort; `run=FALSE` returns the
    command without invoking FFmpeg.
  - AC6 ✓ 48×32@5 probes width 48/fps 5; odd-dim 65×49 default → 3057 bytes
    (was 0); audio mp3→mp3 (was mp3→aac). `test()` 513/0/0; `check()` clean.
- Consistency gate: coverage complete; pkgdown clean; `document()` no-diff;
  README in sync; NEWS updated; no new files; no DESIGN change.
  `cairn_validate.py`'s sole FAIL is a pre-existing false-positive (date regex
  flags the repo's N/N/N check-results shorthand in 7 already-merged archived
  files, outside M12's diff); M12's file is validator-clean, archives untouched.
- Fresh-context review (2 lenses + scorer): two findings, both reproduced and
  **fixed** ("fix both now"). Finding 2 (audio mismatch, 90) — transcoded
  audio; fixed via `ffm_codec(audio="copy")` + regression test. Finding 1
  (odd-dim default crash, 76) — 0-byte output; fixed via floor-to-even
  `ffm_crop` safeguard + regression test; needed the gated AC2/AC3/AC4 amendment.
