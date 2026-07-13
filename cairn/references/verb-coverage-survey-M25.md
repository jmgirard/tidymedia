# Verb coverage survey — research-domain gap analysis (M25)

_Working artifact produced by M25. Surveys the Layer 2 **task verbs** a
behavioral / affective-science media-preprocessing pipeline needs, diffs them
against what tidymedia exports today, and gives every gap a scoped
**keep / refuse / defer** verdict. Anchored on the research domain, not on
matching another package's surface (D001, GP1). Scope is Layer 2 only —
Layer 0/1 (`ffm_*`, escape hatches), metadata families, and program management
are out._

_Demand source: the behavioral/affective-science video workflow tidymedia
already leans toward — facial/interview/dyadic recordings prepared for manual
coding, reliability rating, and per-frame CV feature extraction (e.g. OpenFace
action units). Analog tools (av, magick, tuneR/seewave, moviepy) are cited only
as evidence that a need is real, never as a parity target._

## 1. The "have" — current Layer 2 task verbs

18 exported task verbs today (13 scalar + 5 `_batch` siblings), classified by
pipeline stage. Cross-checked against `NAMESPACE`
(`grep '^export' NAMESPACE`).

| Verb (`_batch` sibling) | One-line role | Stage |
|---|---|---|
| `standardize_video` (`_batch`) | Re-encode to a consistent reproducible format (codec/res/fps/pixfmt) | B — standardize |
| `format_for_web` | Re-encode to widely-compatible H.264/web-friendly form | B — standardize |
| `convert_audio` | Extract or transcode a file's audio track to a chosen format | B / F — audio |
| `segment_video` (`_batch`) | Cut one video into many clips by `start`/`end` times | C — trim/segment |
| `crop_video` | Crop to a rectangular pixel region | D — spatial |
| `anonymize_video` (`_batch`) | Cover fixed rectangular regions with opaque boxes | E — de-identify |
| `normalize_audio` (`_batch`) | Loudness-normalize audio to an EBU R128 target (opt. two-pass) | F — audio |
| `extract_audio` | Map/stream-copy the audio stream out to a file | F — audio |
| `extract_frame` (`_batch`) | Save one frame (by timestamp or frame number) to an image | G — frames |
| `separate_audio_video` | Split a file into separate audio and video files | H — split |
| `concatenate_videos` | Join videos end-to-end via the concat demuxer | H — assemble |
| `compare_videos` | Stack ≥2 videos into one side-by-side comparison | H — assemble |
| `picture_in_picture` | Overlay one video inset on another | H — assemble |

Adjacent, non-transform surface (not task verbs, listed for the stage map):
- **Stage A — ingest/inspect:** metadata families (`probe_*`, `mediainfo_*`,
  `get_*`) — out of this survey's scope but the stage is covered.
- **Stage I — verify/provenance:** `verify_media` + `ffm_manifest` — the
  verification layer over the engine (D011), not a preprocessing transform.

## 2. The "want" — the research pipeline, stage by stage

An ordered model of how a behavioral/affective researcher takes raw recordings
to analysis-ready assets. `✓` covered · `△` partial/composable · `✗` absent.

- **A. Ingest & inspect** — what codecs/streams/duration/resolution do I have?
  `✓` metadata families.
- **B. Standardize / transcode** — bring heterogeneous captures to one
  reproducible codec/resolution/fps/pixel-format.
  `✓` `standardize_video`, `format_for_web`, `convert_audio`.
- **C. Trim & segment** — cut to task windows; split into trials by time.
  `✓` `segment_video` (single window = one-row call). `✗` content-based
  auto-segmentation (scene/silence/speech) — see §3 refusals.
- **D. Spatial / ROI** — crop to a face, remove letterbox, isolate one person
  from a side-by-side capture. `✓` `crop_video`. `△` splitting a
  multi-view/side-by-side recording into per-person files (composable from
  repeated `crop_video`, no dedicated verb).
- **E. De-identify** — cover or obscure faces/regions; strip identifying
  metadata. `✓` `anonymize_video` (box fill). `✗` region blur (candidate);
  `✗` metadata scrubbing; face-tracking → refused (§3).
- **F. Audio conditioning** — loudness normalize, resample, downmix, extract.
  `✓` `normalize_audio` (`sample_rate`/`channels` pin resampling/downmix),
  `extract_audio`, `convert_audio`. `△` per-speaker multitrack split/mix
  (`amix` is Layer 0 by D009).
- **G. Frame / still extraction** — pull frames for image-based coding and
  per-frame CV features (AUs, landmarks). `✓` `extract_frame` (one),
  `extract_frame_batch` (an enumerated set). `✗` **fixed-rate sampling** — "give
  me a frame every N seconds / at R fps as a numbered image sequence" — the
  single most common frame need for per-frame analysis, and distinct from both
  existing verbs.
- **H. Assemble / compose** — concatenate, side-by-side, PiP, split.
  `✓` `concatenate_videos`, `compare_videos`, `picture_in_picture`,
  `separate_audio_video`. `✗` grid/mosaic (`xstack` → Layer 0, D009).
- **I. Verify & provenance** — `✓` `verify_media`, `ffm_manifest` (D011).

Cross-cutting need surfaced across stages: **burn-in of visible
timecode/frame-number or text labels** — routinely used so human coders and
reliability raters can reference exact frames. `✗` absent (no `drawtext`
surface yet).

## 3. The gap list — scoped dispositions

Every absent/partial item, with a **keep / refuse / defer** verdict. Refusals
cite GP1 (scope discipline) / D001 (not "all of ffmpeg"; no realtime/detection).
The bar for a task verb: it must encode a *research task* with non-obvious
defaults or guards (as `standardize_video`'s even-dimension + audio-copy guards,
`normalize_audio`'s two-pass, `anonymize_video`'s region handling do) — a bare
one-filter passthrough does not clear it and belongs to the `ffm_*` builder or
the Layer 0 escape hatch.

### keep — in scope, high value, buildable within IP1/IP2

- **K1 · Fixed-rate frame sampling** (`sample_frames` working name) — extract
  frames at a fixed rate/interval to a numbered image sequence (Stage G `✗`).
  Single-input, deterministic, linear: an `fps`/`select` filter into the
  `image2` muxer. Distinct from `extract_frame` (one frame) and
  `extract_frame_batch` (a caller-enumerated set). Earns its place: sampling
  rate ↔ output-numbering ↔ frame-count guards are non-obvious. **Highest
  value** — it is the front door to per-frame coding and CV feature pipelines.
- **K2 · Metadata scrubbing** (`strip_metadata` working name) — remove container
  and per-stream metadata tags (device, GPS, creation identity) for
  IRB/de-identification (Stage E `✗`). Single-input, stream-copy
  (`-map_metadata -1`, `-map_chapters -1`, and the codec-private tag cases).
  Earns its place: "which tags actually clear, and does stream-copy preserve
  A/V" is a real guard story, and de-identification is core to the domain.

### defer — in scope but needs a design call before planning

- **D1 · Region blur anonymization** — *(existing candidate; reconciled here, not
  duplicated)*. split→crop→boxblur→overlay needs an IP2 filtergraph design call
  (new blessed composite verb vs. Layer 0) plus an `ffm_boxblur` filter. Confirmed
  in-scope and wanted by this survey; still gated on that call.
- **D2 · Timecode / text burn-in** (`burn_timecode` / label overlay) — visible
  frame/timecode or text for coders & reliability raters (§2 cross-cutting).
  In scope (single-input `drawtext`) but needs a new `ffm_drawtext` Layer-1
  filter and a decision on how much of `drawtext`'s surface to expose — a design
  call, hence defer not keep.
- **D3 · Minor in-scope convenience verbs** *(grouped)* — each buildable but each
  needs a small design call on its argument shape; grouped to keep the ROADMAP
  signal-dense:
  - *Split multi-view into per-person clips* — the inverse of `compare_videos`;
    composable from repeated `crop_video` today (Stage D `△`).
  - *Orientation fix* (`rotate`/`flip`/transpose) — genuine guard story
    (display-matrix rotation metadata vs. a real transpose re-encode) for
    phone/webcam captures; otherwise close to a passthrough.
  - *Contact-sheet / thumbnail montage* — tile sampled frames into one QC image
    for fast visual review of many recordings (builds on K1 + a `tile` filter).

### refuse — out of scope; escape hatch / external tools

- **R1 · Face-tracking / moving-ROI anonymization** — requires CV face
  detection+tracking. **D001** (not "all of ffmpeg"; no detection/analysis
  engine) / **GP1**. Fixed-region box fill (`anonymize_video`) and, later, region
  blur (D1) are the in-scope half; moving ROIs stay external (detect elsewhere,
  feed regions per frame via Layer 0).
- **R2 · Content-based auto-segmentation** (scene-cut / silence / speech split) —
  requires content detection. **D001** / **GP1**. Time-based cutting
  (`segment_video`) is the in-scope half.
- **R3 · Grid/mosaic (`xstack`) and audio mixing (`amix`)** — **D009** already
  keeps these at Layer 0 (`amix` needs an `[aout]` engine generalization). Not
  reopened here.
- **R4 · Bare one-filter passthrough verbs** (speed/retime `setpts`, deinterlace,
  arbitrary single filters) — **GP1**: wrapping each thin filter as a task verb
  chases full coverage. These belong to the `ffm_*` builder / Layer 0 escape
  hatch, which already serve them.
- **R5 · Subtitle/caption extract & burn, chapter editing** — outside the
  media-preprocessing core for this domain. **GP1** / **D001**.

## 4. Outcome → ROADMAP

Registered as candidate rows (keep + defer only; no refuse item becomes a
candidate, per M25 AC5):

- **K1** `sample_frames` — fixed-rate frame sampling → candidate (high).
- **K2** `strip_metadata` — de-identification metadata scrub → candidate (high).
- **D1** region blur — existing candidate, reconciled (this survey confirms scope).
- **D2** `burn_timecode` / drawtext overlay → candidate (needs `ffm_drawtext`).
- **D3** grouped minor convenience verbs (split multi-view, orientation fix,
  contact sheet) → one grouped candidate pointing back at this survey.

Refusals R1–R5 are recorded here as the standing rationale so the ideas are not
re-litigated each time they recur (search-first candidate discipline).
