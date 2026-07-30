# RB03: Restoring `ffm_copy()` / `ffm_concat()` idempotence (M48)

- **Date:** 2026-07-30
- **Output required:** write findings to `cairn/reviews/RR03-ffm-copy-idempotence.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `tidymedia` is an R interface to FFmpeg for reproducible media
preprocessing in research pipelines. It has three layers:

- **Layer 0** — `ffmpeg()`, `ffprobe()`: raw CLI passthrough.
- **Layer 1** — the `ffm_*` pipe builder (`R/ffm.R`). An `ffm` object
  accumulates state through piped calls (`ffm_files()` → `ffm_crop()` →
  `ffm_map()` → …) and `ffm_compile()` renders it to one FFmpeg command
  string. **All** command assembly and quoting lives here, once.
- **Layer 2** — task verbs (`crop_video()`, `segment_video()`, …) in
  `R/ffmpeg.R`: thin wrappers that compute arguments and call Layer 1.

The package is pre-0.2.0 and not on CRAN. Its clean-break rename policy (no
`lifecycle` shims) is D014 in `cairn/DECISIONS.md`.

**What changed, and the bug it introduced.** `ffm_map()` used to *overwrite* the
pipeline's map. Milestone M43 needed a map that could sit beside another (keep
the video, name one audio track), so it changed `ffm_map()` to **append** and
added `replace = TRUE` as the escape hatch. That change is recorded in
`cairn/DECISIONS.md` D023, fourth bullet.

`ffm_copy()` — the stream-copy shortcut — calls `ffm_map(object, mapping = "0")`
to map every stream. Since it now appends, **`ffm_copy()` is no longer
idempotent**. Measured in this repo on 2026-07-30 (compile only, no binary):

```
ffm(video, "out.mkv") |> ffm_copy() |> ffm_copy() |> ffm_compile()
#> ... -codec:v copy -codec:a copy -map 0 -map 0 "out.mkv"
```

Two `-map 0` arguments duplicate **every** output stream (a 1-video/1-audio
input yields a 4-stream output). `ffm_concat()` calls `ffm_copy()` internally
(`R/ffm.R:943`), so `ffm_concat() |> ffm_copy()` doubles too — and the suite
already contains that exact composition at `tests/testthat/test-ffm.R:968`,
where it passes because the test asserts only the leading `-f concat` arguments
and never the map count.

A related composition, also measured:

```
ffm(video, "out.mkv") |> ffm_map("0:v") |> ffm_copy() |> ffm_compile()
#> ... -codec:v copy -codec:a copy -map 0:v -map 0 "out.mkv"
```

which duplicates the video stream. Before M43 this composition compiled to a
single `-map 0` (the overwrite).

No in-package pipeline performs any of these compositions today: the two
in-package `ffm_copy()` callers each call it once
(`strip_metadata_pipeline()`, `R/ffmpeg.R:1189`, and `segment_pipeline()`,
`R/ffmpeg.R:2659`). The exposure is **user-composed Layer-1 chains**, which the
package's vignette actively teaches (`vignettes/tidymedia.Rmd:117-123` pipes
`ffm_seek() |> ffm_copy()`).

**Why this is being decided now.** Milestone M48 carries an `audio_stream`
track selector to `crop_video()` and `segment_video()` under the rule set by
D026: every call states its stream selection, compiling `-map 0:v? -map 0:a?`
when no track is named and `-map 0:v? -map 0:a:<n>` when one is. On
`segment_video(reencode = FALSE)` that map must **replace** `ffm_copy()`'s
`-map 0` rather than append beside it. So M48 re-enters and re-reads this
contract anyway, and the tracking system's candidate row for the idempotence
bug named exactly that as its promotion condition.

**Why this needs independent review.** The fix changes an exported Layer-1
contract on a pre-1.0 package whose builder semantics several decisions now
rest on. The implementing session tagged it `irreversible-api`.

## Materials

Read these, in this order:

1. `cairn/DECISIONS.md` — **only** these entries, by heading:
   - **D002** (three-layer architecture), **D003** (the builder stays linear)
   - **D014** (naming scheme + clean-break rename policy) — the pre-0.2.0,
     no-`lifecycle`-shim policy
   - **D023** — read the whole entry; its **fourth bullet** ("`ffm_map()`
     appends; `replace = TRUE` narrows") is the contract in question
   - **D026** — the map rule M48 is applying; its Scope bullet names M48
2. `cairn/DESIGN.md` §"Design principles" — IP1, IP2, IP3, GP1, GP2.
3. `R/ffm.R`:
   - `ffm_map()` — definition at **line 574**, with the M43 rationale comment
     at lines 585–590
   - `ffm_copy()` — roxygen at lines **598–625** (note `@param streams` at
     **610–613**, which documents the appending behavior as the contract),
     definition at **line 626**
   - `ffm_concat()` — definition at **line 921**; its `ffm_copy()` call at
     **line 943**
   - `ffm_compile()`'s map emission — lines **1200–1234**. Note that in
     `complex` mode the automatic `-map "[vout]"` is synthesized at compile
     time and is **not** an element of `object$map`.
4. `R/ffmpeg.R`:
   - `pass_through_maps()` — lines **281–329**, the M47 resolver M48 reuses
   - `strip_metadata_pipeline()` — line **1186**, an `ffm_copy()` caller
   - `segment_pipeline()` — line **2620**, the other `ffm_copy()` caller and
     the one M48 modifies
5. `tests/testthat/test-ffm.R`:
   - `ffm_copy()` tests at **377**
   - the `replace = TRUE` test at **417**
   - the per-verb map-count invariant at **438**
   - the concat + copy composition at **965**
   - the complex-mode explicit-map test at **980**
6. `vignettes/tidymedia.Rmd` lines **110–145** — what the package teaches
   users to compose at Layer 1.

**To reproduce any claim**, from the repo root:

```
Rscript -e 'devtools::load_all(); f <- system.file("extdata","sample.mp4",package="tidymedia"); cat(ffm_compile(ffm_copy(ffm_copy(ffm(f,"out.mkv")))))'
```

`ffm_compile()` runs no binary, so every compile-level claim in this brief is
checkable without FFmpeg installed. The suite runs with
`Rscript -e 'devtools::test()'`; execution tests skip when FFmpeg is absent.

## Questions

1. **Which spelling should restore idempotence?** Three candidates are on the
   table. Pick one, or propose a fourth, and say why it is right rather than
   merely workable.

   - **(A)** `ffm_copy(streams = TRUE)` calls
     `ffm_map(object, mapping = "0", replace = TRUE)`. Leaves `ffm_map()`'s
     appending contract untouched. Mental model: `ffm_copy()` *assigns*,
     `ffm_map()` *appends*, `replace = TRUE` *narrows*. Cost: a map set
     **before** `ffm_copy()` is silently discarded (this restores the exact
     pre-M43 behavior of that composition).
   - **(B)** `ffm_map()` de-duplicates: `object$map <- unique(c(object$map, mapping))`.
     Fixes every repeated specifier at once. Cost: changes the Layer-1
     contract D023's fourth bullet states, and removes the ability to map one
     specifier twice deliberately (an FFmpeg technique for duplicating a
     stream into two output streams). It does **not** fix
     `ffm_map("0:v") |> ffm_copy()`, which still emits `-map 0:v -map 0`.
   - **(C)** `ffm_copy()` appends `"0"` only when `"0"` is not already
     present. Smallest possible change; no contract change anywhere; nothing
     is ever discarded. Cost: leaves `ffm_map("0:v") |> ffm_copy()` still
     duplicating the video stream, so it fixes the reported instance rather
     than the class.

2. **Is "idempotence" even the right frame?** The candidate row and M48's
   acceptance criteria state the goal as "`ffm_copy()` applied twice compiles
   exactly one `-map 0`". Is the underlying defect narrower or wider than
   that — e.g. is the real invariant "the compiled command never maps the same
   stream twice unless the caller asked for it", and if so does any of A/B/C
   actually establish it?

3. **Does deliberately mapping one specifier twice have a legitimate use in
   this package's scope?** Option B removes it. D001/GP1 is scope discipline —
   tidymedia deliberately refuses ffmpeg features rather than growing toward
   full coverage — so "FFmpeg permits it" is not by itself an argument for
   keeping it. Should stream duplication be reachable at Layer 1 at all, and
   does the answer change if the only way to reach it becomes the Layer 0
   escape hatch (`ffmpeg()`)?

4. **Silent discard versus silent duplication.** Option A makes
   `ffm_map(…) |> ffm_copy()` drop the earlier map with no warning; the status
   quo makes it duplicate a stream with no warning. Both are silent and
   order-dependent. Should the chosen fix *also* signal — a `cli` warning, or
   an abort — when `ffm_copy()` meets a non-empty map, or is silence correct
   here? Note the package's stated purpose is removing invisible,
   environment-dependent variation from compiled commands (D001), and that
   several D-entries (D023, D026) rest on selection being *stated* rather than
   inherited.

5. **Complex mode.** In `complex` mode `ffm_compile()` synthesizes
   `-map "[vout]"` outside `object$map` (`R/ffm.R:1216-1219`), and
   `ffm_concat()` sets `concat`/`concat_list` rather than `complex`. Does any
   of A/B/C interact badly with either path — in particular, can any of them
   make a blessed multi-input verb (hstack/vstack/overlay/concat) emit a map
   set it did not emit before?

6. **Blast radius and the deprecation question.** D014 puts the package on a
   pre-0.2.0 clean break with no `lifecycle` shims. Given that, and given that
   no in-package pipeline performs any doubling composition today: does the
   chosen fix need any deprecation or migration affordance at all, or is a
   NEWS entry plus a rewritten `@param streams` sufficient? Name anything in
   the repo you found that the fix would break.

7. **What must the tests assert?** M48's criterion AC4 currently reads:
   "`ffm_copy()` applied twice compiles exactly one `-map 0`, and
   `ffm_concat() |> ffm_copy()` likewise; with ffmpeg present a doubled
   `ffm_copy()` over a 5-stream `.mkv` writes 5 streams, not the 10 master
   writes." Is that sufficient to pin the contract you recommend, and what
   additional assertions (compile-level or execution-level) would you require?
   Note `tests/testthat/test-ffm.R:438` is a per-verb map-count invariant that
   M48 is already rewriting.

## Constraints

Fixed; do not relitigate. Flag disagreement explicitly rather than working
around it silently.

- **IP1 / D002 — three-layer separation.** The fix lives in Layer 1
  (`R/ffm.R`). No Layer-2 verb may compensate by gluing its own map string.
- **IP2 / D003 — the builder stays linear.** One input chain, sequential
  filters, one output, plus the blessed multi-input verbs. No answer may
  require a filtergraph DAG or a multi-output engine model.
- **D023's first three bullets stand.** `audio_stream` counts one input's
  audio streams; selection is stated, never inherited from the file's
  disposition flags. Only the **fourth** bullet (the `ffm_map()` append
  contract) is open.
- **D026 stands.** The pass-through verbs emit `-map 0:v? -map 0:a?` when no
  track is named and `-map 0:v? -map 0:a:<n>` when one is; the trailing `?` on
  unselected specifiers is load-bearing and the named specifier deliberately
  carries none. M48 must be able to narrow `ffm_copy()`'s map to that pair on
  `segment_video(reencode = FALSE)` — whichever option you pick must leave
  that reachable.
- **D014 — pre-0.2.0 clean break, no `lifecycle` shims.** A deprecation cycle
  is not the default answer here; if you think this case earns one, argue it.
- **Out of scope for this brief:** carrying `audio_stream` to
  `format_for_web()` / `normalize_audio()`; quoting map specifiers in the
  compiled command; a `subtitle_stream` / `video_stream` selector. Each is a
  standing ROADMAP candidate row and must not be folded in.

## Output format

In `cairn/reviews/RR03-ffm-copy-idempotence.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason. Where findings bind implementation, also emit a
`## Binding criteria` section: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its tolerance.
These are ingested VERBATIM into M48's acceptance criteria and mechanically
diffed against this file; departures are legal only through M48's shown
"Deviations from RR03" table.
