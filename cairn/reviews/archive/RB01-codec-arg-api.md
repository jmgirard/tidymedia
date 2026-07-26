# RB01: `video_codec` API shape for the four codec-less re-encode verbs (M34)

- **Date:** 2026-07-26
- **Output required:** write findings to `cairn/reviews/RR01-codec-arg-api.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**tidymedia** is an R interface to FFmpeg/MediaInfo for *reproducible media
preprocessing in research/data-science pipelines* — deliberately not "all of
ffmpeg in R" (D001). It has three layers (D002):

- **Layer 0** — raw CLI passthrough (`ffmpeg()`/`ffprobe()`/`mediainfo()`).
- **Layer 1** — the `ffm_*` pipe builder (`R/ffm.R`, `R/ffm_oop.R`): all command
  assembly, quoting, and copy-vs-re-encode logic lives here once. Deliberately
  linear (IP2): one input chain, sequential filters, one output, plus blessed
  multi-input verbs (hstack/vstack/overlay/concat), no filtergraph DAGs.
- **Layer 2** — task verbs (`crop_video`, etc.) as thin wrappers that compute
  arguments and never glue their own command strings (IP1).

A prior milestone (M31) added an **opt-in GPU-encoding toggle** to two verbs:
`hardware = c("none","nvenc")` + `fallback = FALSE`. The mechanism (in
`R/ffmpeg.R`): a Layer-2 verb calls internal `resolve_hw_encoder(video_codec,
hardware, fallback)`, which returns `video_codec` unchanged when
`hardware="none"`, or the nvenc encoder name (e.g. `h264_nvenc`) when
`hardware="nvenc"` and it is available, else aborts (or, with `fallback=TRUE`,
re-encodes in software with a message). Crucially, **`resolve_hw_encoder`
requires a `video_codec` string it can rewrite** — it works only on verbs that
thread a codec into `ffm_codec(video = ...)`.

**The milestone this brief serves (M34, not yet planned):** extend the
`hardware=` toggle to the four re-encode verbs M31 skipped: `crop_video`,
`segment_video`, `compare_videos`, `picture_in_picture`. Investigation found
that, unlike `standardize_video`/`anonymize_video` (which already expose a
user-controlled `video_codec` feeding `ffm_codec`), **these four set no video
codec at all** — when a filter forces a re-encode, FFmpeg uses the *output
container's default encoder* (e.g. libx264 for `.mp4`, VP9 for `.webm`). So
`hardware=` has nothing to rewrite until a codec is threaded through first.

**Why this needs independent review:** introducing a `video_codec` argument to
four exported verbs is an irreversible exported-API commitment (pre-0.2.0 but
still a public surface, and D014 forbids `lifecycle` shims — old shapes are
removed, not deprecated). The default value in particular has a subtle
correctness trap around non-mp4 containers (see Q2). We want a durable verdict
on the API shape *before* M34 is planned, so the review's binding criteria can
drive the milestone's acceptance criteria.

## Materials

Read these (all paths repo-relative):

- `R/ffmpeg.R`, specifically:
  - `crop_video` + `crop_video_pipeline` — approx lines 451–465 and 424–450.
  - `standardize_video` + `standardize_pipeline` (the M31 reference pattern) —
    approx 650–720.
  - `anonymize_video` + `anonymize_pipeline` (also already has `video_codec`) —
    approx 759–835.
  - `format_for_web` + `format_for_web_pipeline` (the *alternative* pattern:
    `hardware=` with an internally-fixed libx264 family, **no** user-facing
    `video_codec`) — approx 468–514.
  - the nvenc machinery `nvenc_encoder`/`has_nvenc`/`codec_family`/
    `resolve_hw_encoder` — approx 1360–1470.
  - `segment_video` + `segment_pipeline` — approx 1508–1520 and 1596–1605
    (note the `reencode` arg: `TRUE` re-encodes, `FALSE` stream-copies).
  - `compare_videos` + `compare_videos_pipeline` (blessed multi-input hstack/
    vstack) — approx 3216–3271.
  - `picture_in_picture` + `picture_in_picture_pipeline` (blessed multi-input
    overlay) — approx 3295–3360.
  - the `*_batch` siblings of all four verbs (grep `crop_video_batch`,
    `segment_video_batch`, `compare_videos_batch`, `picture_in_picture_batch`)
    and how they thread per-verb args (note: some use a `pick()` closure over
    `...` for per-row column overrides; M31's `hardware`/`fallback` are instead
    captured batch-wide scalars).
- `R/ffm.R`: `ffm_codec` (stores `codec_video`; approx line 523) and
  `ffm_compile` (emits `-codec:v <x>` only when `codec_video` is populated;
  approx 1200–1210, and the seek/`copy` interaction approx 1122–1123).
- `cairn/DESIGN.md` — principles IP1, IP2, IP3, GP1, GP2 and the architecture
  section.
- `cairn/DECISIONS.md` — entries **D006** (filter emission simple vs complex),
  **D008** (cutting/seeking; frame-accurate re-encode default vs opt-in
  stream-copy), **D009** (blessed multi-input set is single-video-output;
  "Layer-2 verbs only compute arguments"), **D014** (API naming scheme +
  clean-break rename policy; ratifies `video_codec`/`pixel_format` as the
  canonical arg vocabulary), and the M31 nvenc decision in
  `cairn/milestones/archive/M31-nvenc-encoding.md`.

You do not need to run anything; command **compilation** is pure and binary-free
(the tests assert on compiled command strings). If you wish to sanity-check a
compiled command mentally, note that a filter in the chain forces a re-encode.

## Questions

1. **Codec exposure.** Should all four verbs gain a user-facing `video_codec`
   argument (defaulting to some codec) flowing into `ffm_codec(video=...)` — the
   `standardize_video`/`anonymize_video` pattern (call it **Option B**) — or
   should they gain only `hardware=`/`fallback` with an internally-fixed H.264
   family and no user-facing codec — the `format_for_web` pattern (**Option
   A**)? Weigh GP1 (scope discipline — refuse features over growing toward full
   coverage) against consistency with the two verbs that already expose
   `video_codec`, real user value (choosing libx265/av1), and D014's ratified
   `video_codec` vocabulary. A split verdict (Option B for some verbs, Option A
   for others) is acceptable if you justify the boundary.

2. **Default value and the container-default trap (most important).** These
   verbs currently emit *no* `-c:v`, so FFmpeg picks the encoder from the output
   container: libx264 for `.mp4`/`.mkv`, but VP9/VP8 for `.webm`, etc. If
   `video_codec` defaults to a literal `"libx264"` (as `standardize_video`
   does), then `crop_video("in.webm","out.webm", ...)` would now force libx264
   into a WebM container — a behavior change and likely a broken/mismatched
   output for any non-H.264 container that worked before. Options include: (a)
   default `"libx264"` and document the change; (b) a sentinel default (e.g.
   `NULL`/`"auto"`) that emits **no** `-c:v` and preserves today's
   container-default behavior exactly, with `resolve_hw_encoder` only engaging
   when the user opts into `hardware="nvenc"` or names a codec; (c) something
   else. Which default is correct, and what exactly must `resolve_hw_encoder`
   and `codec_family` do when `hardware="nvenc"` is requested but `video_codec`
   is the sentinel (no software codec to derive a family from)?

3. **`pixel_format` pairing.** `standardize_video`/`anonymize_video` pair
   `video_codec` with a `pixel_format` argument. Should M34 also add
   `pixel_format` to these four verbs for consistency, or is `video_codec`
   (plus `hardware`/`fallback`) the right minimal surface, deferring
   `pixel_format`? Consider that `anonymize_video` needs `pixel_format=yuv420p`
   for odd-dimension safety; do any of these four have a comparable need?

4. **Blessed multi-input verbs (IP3/D009).** `compare_videos` and
   `picture_in_picture` ride the `-filter_complex … [vout]` path (D006). D009
   says "Layer-2 verbs only compute arguments" and the engine stays
   single-video-output. (a) Does adding `ffm_codec(video=...)` to these compose
   correctly with the complex/`[vout]` compile path, or is there an ordering or
   mapping hazard? (b) Is exposing a `video_codec` knob on these composite verbs
   consistent with IP3/D009, or does it stretch the "thin wrapper" boundary?

5. **`segment_video` and the stream-copy path (D008/GP2).** `segment_video`
   defaults to `reencode=TRUE` (frame-accurate re-encode) but supports
   `reencode=FALSE` (lossless stream-copy, no encoder runs). The plan is to
   **error** when `hardware="nvenc"` is combined with `reencode=FALSE`. (a) Is
   erroring correct, or should it warn/ignore? (b) What should happen if a user
   passes a non-default `video_codec` together with `reencode=FALSE` — error,
   ignore, or is that combination meaningful? (c) Any interaction with
   `ffm_compile`'s seek/`copy` handling to watch for?

6. **Batch siblings.** M31 made `hardware`/`fallback` **batch-wide captured
   scalars** (not per-row job columns). For M34's new `video_codec` (and
   `pixel_format` if Q3 says yes): should it be a **per-row override column**
   (the batch convention for most verb args, via the `pick()` closure) or a
   **batch-wide scalar**? Note a known repo hazard (from a prior milestone): a
   per-row override *column* bypasses the scalar verb's argument guards, and an
   all-NA override column is *logical* type, not numeric — so any per-row codec
   column needs per-row re-validation. Which choice is right, and what
   validation must accompany it?

## Constraints

Fixed; do not relitigate (flag disagreement explicitly rather than working
around it):

- **IP1 (D002):** Layer 2 verbs stay thin wrappers — no hand-glued command
  strings; the codec name is *computed* in Layer 2 and *assembled* in Layer 1
  (`ffm_codec`/`ffm_compile`). Any proposal must keep assembly in Layer 1.
- **IP2 (D003):** the builder stays linear; no filtergraph DAGs. M34 adds no new
  engine capability — it only threads a codec into existing `ffm_codec`.
- **IP3 (D006/D009):** the blessed multi-input set is single-video-output and
  audio is explicit-map-only; do not propose an engine generalization.
- **D008/GP2:** cutting is frame-accurate re-encode by default; the stream-copy
  path is opt-in and lossless — do not change these defaults.
- **D014:** `video_codec`/`pixel_format` are the ratified arg names (do not
  propose `vcodec`/`codec`); rename policy is a **clean break** (pre-0.2.0, no
  `lifecycle` shims). New args are additive and fine.
- **D-M31:** reuse the existing `resolve_hw_encoder`/`codec_family`/`has_nvenc`
  machinery and the `tidymedia.nvenc_encoders` option seam for tests; do not
  reimplement nvenc detection. nvenc supports only the h264/hevc/av1 families.
- The **milestone split is fixed:** `anonymize_video` is already handled
  separately (M33); this brief is only about the four codec-less verbs.

## Output format

In `RR01-codec-arg-api.md`: answer each question by number with your reasoning
and evidence (cite file:line where it matters); list any additional findings
separately under "Beyond the brief"; end with concrete recommendations, each
marked apply / consider / reject-with-reason. Where findings bind
implementation, also emit a `## Binding criteria` section: numbered `BC1…`, each
a measurable assertion checkable against evidence (a passing test, a compiled
command string, a file that exists), with any numeric projection stating its
tolerance. These are ingested VERBATIM into M34's acceptance criteria and
mechanically diffed against this file; departures are legal only through M34's
shown "Deviations from RR01" table. Keep binding criteria to what genuinely
must hold — over-constraining the plan is itself a cost.
