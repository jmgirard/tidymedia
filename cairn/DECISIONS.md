# Design Decisions

Append-only log of cross-cutting decisions. One short entry each: what was
decided, why, and what it rules out. Milestone-local decisions stay in the
milestone file; promote them here only if they constrain future work.
Newest entries at the bottom. Never edit old entries; supersede them with a
new entry that references the old ID.

_D001–D009 predate the cairn migration (2026-07-11) and keep their original
IDs; existing citations across the codebase and CLAUDE.md stay valid. New
decisions continue at D010._

---

## D001 — Package scope (2026-07-10)

tidymedia is **reproducible media preprocessing for research/data-science
pipelines**, not "all of ffmpeg in R." Differentiators: batch processing over
many files, metadata as tibbles, reproducible compiled commands. Rules out:
chasing full ffmpeg option coverage, realtime/streaming use cases.

## D002 — Three-layer architecture (2026-07-10)

- **Layer 0 (escape hatch):** `ffmpeg()`, `ffprobe()`, `mediainfo()` raw CLI
  passthrough. This IS the "faithful wrapper" — no further faithful mapping
  will be built.
- **Layer 1 (engine):** the `ffm_*` pipe builder. All command assembly,
  quoting, option ordering, and copy-vs-re-encode logic lives here, once.
- **Layer 2 (front door):** task verbs (`extract_audio()`, `segment_video()`,
  …) implemented as thin wrappers over Layer 1. Most users only touch this.

Rules out: task functions that glue their own command strings (the current
`ffmpeg.R` functions must migrate onto the builder).

## D003 — The builder stays linear (2026-07-10)

Layer 1 models a **single input chain with sequential filters and one
output**, plus a small set of blessed multi-input verbs (stack, concat,
overlay) that manage their own stream labels internally. Full filtergraph
DAGs are out of scope forever; those users get the Layer 0 escape hatch.

## D004 — Tooling (2026-07-10)

usethis for scaffolding; testthat (3rd edition); cli/rlang for all
user-facing messages and errors (assertthat to be retired); GitHub Actions
for CI. Command *compilation* is tested purely (no binaries, CI-safe);
command *execution* tests are skipped when ffmpeg is unavailable.

## D005 — Development workflow (2026-07-10)

Milestone-driven development tracked in `project/`, orchestrated by Opus via
the `/milestone` skill. Subagents: Sonnet for well-specified parallel work,
Opus for design-sensitive work, never Haiku. Fable is consulted only through
the brief protocol (user-run clean session), never as a subagent.

## D006 — Filter emission: simple vs complex (2026-07-10, from M02)

`ffm_compile()` chooses the filter flag by input arity. Single-input
sequential chains compile to `-vf`/`-af`. Any blessed multi-input verb (hstack;
concat/overlay to come) sets the pipeline `complex` and compiles to
`-filter_complex` with explicit `[0:v][1:v]…` input labels and an automatic
`-map "[vout]"`; such verbs manage their own labels (D003) and must precede
other video filters. Rules out emitting the invalid `-filter_complex:v`, and
rules out `-filter_complex` for the single-input common case.

## D007 — Batch model (2026-07-10, from M03)

Batch processing is a single tibble-in/tibble-out runner, `ffm_batch(jobs, .f,
…)`: `.f` builds one `ffm` pipeline per row (job-table columns passed by name,
pmap-style); the runner compiles one reproducible command per job and returns
the jobs tibble plus `command` (and `success` when run). Scalar task verbs
stay scalar; fan-out task verbs (one input → many outputs, e.g.
`segment_video`, `separate_audio_video`) are Layer 2 wrappers that emit
multiple single-output pipelines. Rules out vectorizing individual verbs and
reaffirms D003 — the engine never grows a multi-output model.

## D008 — Cutting and seeking (2026-07-10, from M03)

Seeking (`ffm_seek()`, `-ss`/`-to` options) is distinct from the `trim`
*filter* (`ffm_trim()`), because only seeking can stream-copy. Cutting is
frame-accurate by default (`reencode = TRUE`: output-seek + re-encode). The
fast path (`reencode = FALSE`) input-seeks (`-ss` before `-i`) with
`-avoid_negative_ts make_zero` and is lossless but snaps cuts to keyframes, so
the output duration is approximate. Rules out the old output-seek-copy path,
which produced wrong-duration, timestamp-shifted output.

## D009 — Blessed multi-input set completed, video-only (2026-07-10, from M07)

The D003 blessed set is now `hstack`, `vstack`, `overlay`, `concat` — all
**single video output** verbs riding the existing `-filter_complex … [vout]`
path (D006). `xstack` (grid) and `amix` (audio mix) stay Layer 0: `amix`
specifically is deferred because an audio output would require generalizing the
`[vout]`-only complex-compile path to an `[aout]` output — a distinct future
milestone, not a bolt-on. Audio in stacked/overlaid output stays
explicit-map-only (D-M06-1); Layer-2 verbs (`compare_videos`,
`picture_in_picture`) expose an `audio =` index that resolves to `ffm_map()`.
Filtergraph assembly (including `ffm_overlay(scale=)`'s scale2ref inset) stays
in Layer 1 (D002); Layer-2 verbs only compute arguments. Rules out per-verb
hand-glued filtergraphs at Layer 2 and any multi-/audio-output engine model
for now.

## D010 — Tracking moved to cairn (2026-07-11)

Project tracking migrated from the bespoke `project/` layout to the **cairn**
plugin, adopt-in-place: `project/` content moved into canonical `cairn/` files
(ROADMAP regrouped by status; DESIGN.md added; live/done milestones relocated),
the repo-local `.claude/skills/milestone` skill retired to `cairn/legacy/`, and
the eight cairn skills (`/milestone*`, `/hotfix`, `/cairn-release`,
`/cairn-init`) are now the sanctioned way to change project state. Supersedes
D005's `project/`-path reference (the milestone-driven workflow itself stands);
architecture rationale now lives in `cairn/DESIGN.md`.

## D011 — Verification & provenance layer outside the engine (2026-07-11, from M08)

Output verification and batch provenance are a layer *over* the engine, never a
change to the `ffm` object (D003). `verify_media()` is a standalone probe-backed
primitive wired into execution via `ffm_run(verify=)` (aborts on failure, like
its FFmpeg-exit abort) and `ffm_batch(verify=)` (records a `verified` column,
never aborts) — there is no `ffm_expect()` verb, so the pipeline object stays
command-only. The batch provenance manifest (`ffm_manifest()` /
`ffm_batch(manifest=, checksums=)`) is opt-in and attached as an attribute; md5
checksums opt-in; CSV output only (no JSON/hash dependency). Rules out an
assertion-carrying engine object and any always-on provenance overhead.

## D012 — `future` declared in Suggests (2026-07-12, from hotfix)

`future` is now an explicit `Suggests` dependency. The parallel batch path
(`ffm_batch(parallel = TRUE)`) already relied on it transitively via `furrr`;
the sequential-plan guard added in this hotfix references `future::plan()`
directly, so the dependency is declared to satisfy `R CMD check`'s undeclared-
`::` check. No new install footprint (`furrr`, itself in `Suggests`, imports
`future`). Rules out reaching into `future` internals; only `plan()` is used.

## D013 — Analyze-then-build execution pattern (2026-07-12, from M16)

`normalize_audio(two_pass = TRUE)` is tidymedia's first verb that must **run a
binary to build a later command**: an analysis pass
(`loudnorm=…:print_format=json -f null -`) is executed, its stderr parsed (a
small regex, no JSON dependency — D011 spirit), and the measured values drive a
linear correction pass built on the shared `normalize_audio_pipeline()`. The
orchestrator lives beside `ffm_run()`; `ffm_compile()` stays pure (D002). New
consequence: `run = FALSE` **no longer guarantees a binary-free call** — under
`two_pass = TRUE` the analysis pass always runs (it needs the binary and a
readable input) and `run` gates only the correction pass. Single-pass behavior
is byte-for-byte unchanged and stays binary-free under `run = FALSE`. Rules out
folding analysis into `ffm_compile()` and rules out a two-pass path that skips
the binary under `run = FALSE`.

## D014 — API naming scheme + clean-break rename policy (2026-07-12, from M22)

Canonical naming conventions for the public surface, from the M22 audit
(`cairn/references/naming-docs-audit-M22.md §5–6`), to be applied by the M22
execution follow-up:

- **Task verbs** are `verb_object`; no verb hard-codes a fixed format/codec in
  its name (retires `audio_as_mp3`).
- **`ffm_*`** marks Layer-1 engine surface only; nothing outside Layer 1 uses it
  (references IP1's three-layer separation; does not change IP1).
- **`get_*` is reserved for per-file metadata scalars.** ffmpeg **capability**
  queries are not `get_*` (retires the `get_codecs`/`get_encoders` overload).
- **Metadata prefixes carry backend meaning:** `probe_*` = ffprobe→tibble,
  `mediainfo_*` = MediaInfo→tibble/value, file-metadata scalars a distinct
  getter prefix; the boundary is documented, not merged.
- **Batch siblings use a `<scalar_verb>_batch` suffix**, not a plural noun:
  `segment_video_batch`, `standardize_video_batch`, `normalize_audio_batch`,
  `anonymize_video_batch`, `extract_frame_batch` (retires `*_videos`/`_audios`;
  also disambiguates the old `extract_frames`).
- **Argument vocabulary:** `infile`/`outfile`/`infiles` for transforms, `file`
  for read-only metadata; full-word compound args (`audio_codec`, `video_codec`,
  `pixel_format`, `sample_rate`) — retires `acodec`/`vcodec`; time bounds
  `start`/`end` (+ `duration`, `timestamp` where distinct) — retires
  `ts_start`/`ts_stop`; `run`/`reencode`/`parallel` keep current spellings.
- **Reexports:** drop the unused tidy-eval quoting helpers
  (`enquo`/`enquos`/`as_label`/`as_name`, and `:=` unless a documented pattern
  needs it); **keep `.data`** (used internally in `filter_streams()`).
- **Rename policy: clean break** — no `lifecycle` shims; the API is pre-0.2.0
  and still soaking (D001). Old names are removed, not deprecated.

Rules out silent per-verb naming drift and `lifecycle`-shim compatibility for
this cleanup. The renames themselves are an irreversible-API change carried by
the execution follow-up, not by M22 (which is audit-only).

## D015 — Fan-in batch input-shape (2026-07-26, from M32)

The `_batch` siblings for the fan-in (many-inputs → one-output) verbs carry
their per-row inputs by **shape**, extending D007's single-`input`-column
model (which covers only scalar-input jobs): the variable-arity verbs
(`concatenate_videos_batch`, `compare_videos_batch`) take an **`inputs`
list-column** — each row's cell a character vector — while the fixed-arity,
distinct-role `picture_in_picture_batch` takes named **`main`/`overlay`**
columns. `purrr::pmap` passes both shapes to `.f` row-wise, so `ffm_batch`
needs no change, and the provenance manifest already joins multi-input with
`";"`. These verbs stay single-output, so D007's ban on a multi-output engine
model (and IP2) is untouched — this is an input-side extension only. Rules out
a uniform positional list-column for PiP (roles would become order-dependent)
and any per-verb hand-glued batch runner outside `ffm_batch`.

## D016 — Codec-arg API shape for re-encode verbs (2026-07-26, from M34/RR01)

Resolves how the four codec-less re-encode verbs (`crop_video`,
`segment_video`, `compare_videos`, `picture_in_picture`) expose an output
codec, from Fable review RR01 (`cairn/reviews/archive/RR01-codec-arg-api.md`).
Extends D014's arg vocabulary; reuses the D-M31 nvenc machinery; sits under
IP1/IP3/D009.

- **Expose a user-facing `video_codec` arg on configurable transforms.** The
  boundary rule: a *fixed-recipe* verb (`format_for_web` = H.264/yuv420p/AAC by
  identity) hides the codec; a *configurable transform* (crop, cut, stack,
  overlay) exposes it — matching `standardize_video`/`anonymize_video`. Rules
  out the `format_for_web` hidden-codec pattern for general transforms and rules
  out a per-verb split.
- **Default `video_codec = NULL` is a "leave it alone" sentinel** — emit no
  `-codec:v`, preserving the output container's default encoder byte-for-byte.
  Rules out a literal `"libx264"` default (silently forces H.264 into non-H.264
  containers like `.webm` — the container trap) and an `"auto"` string (collides
  with `check_token`-valid encoder names).
- **The sentinel is handled inside `resolve_hw_encoder()`, before family
  inference:** `hardware="none"`→`NULL`; `hardware="nvenc"`+`NULL`→the h264
  family; nvenc-unavailable + `fallback=TRUE` + `NULL`→`NULL` (container
  default), never an injected libx264 — the fallback never silently changes the
  codec. One resolver seam, not a fork (D-M31).
- **Hardware acceleration and a real codec are meaningless on a stream-copy
  path:** `segment_video(reencode=FALSE)` combined with `hardware!="none"` or a
  non-NULL `video_codec` aborts, enforced per-row in the shared pipeline (D008
  keeps stream-copy lossless and opt-in).
- **Batch: `video_codec` is a per-row column** (a per-file property; NA→sentinel,
  all-NA-logical accepted), while `hardware`/`fallback` stay batch-wide (a
  machine property). Rules out a per-row `hardware`/`fallback` column.
- **`pixel_format` is deferred** on these verbs (no imposed-standard need;
  additive later under D014). Does not rule out a future addition.

## D017 — Audio-codec API shape for the re-encode verbs (2026-07-26, from M35, extends D016)

Carries D016's boundary rule across to the *audio* stream on the same four
verbs (`crop_video`, `segment_video`, `compare_videos`, `picture_in_picture`).
Uses D014's `audio_codec` spelling; sits under IP1/IP3/D009 and GP1.

- **Default `audio_codec = "copy"`.** These verbs never need to touch audio, so
  re-encoding it is pure loss — and until now they re-encoded it to whatever
  the local FFmpeg build's container default was, the environment-dependence
  D001 exists to remove. Copy is already the norm the package documents
  (`standardize_video`, `anonymize_video`, `strip_metadata`,
  `concatenate_videos`). Rules out a `NULL` default: byte-identical, but it
  would leave the surprising behavior standing as the default.
- **This is a deliberate change to existing default output** — every default
  command gains `-codec:a copy` — taken under D014's pre-0.2.0 clean-break
  policy, no `lifecycle` shim.
- **The accepted trap:** copying a source codec the output container cannot
  hold (FLAC into `.mp4`) now fails loudly where it previously re-encoded
  silently. The remedy is naming an encoder (`audio_codec = "aac"`), and it is
  documented. Rules out reverting to a silent re-encode, which trades a loud,
  fixable failure for an invisible quality loss.
- **Asymmetric with D016 on purpose.** `video_codec` defaults to the `NULL`
  sentinel because a literal default would force a codec into an incompatible
  container; `audio_codec` cannot hit that trap in its default state, because
  copy preserves whatever the source already had. `NULL` is retained on
  `audio_codec` as the escape hatch meaning "emit no `-codec:a`".
- **Contradiction guards, per D016's precedent:** on a stream-copy path
  (`segment_video(reencode = FALSE)`) only `"copy"` is legal, enforced per row
  in the shared pipeline; on the composites a named encoder with no audio
  mapped (`audio = NULL`) aborts.
- **Batch: `audio_codec` is a per-row column** (`NA` → unset), reusing
  `check_batch_codec_col(col =)` and `batch_codec_cell()`. Rules out reusing
  `check_batch_string_col()`, which rejects `NA` and so cannot spell "unset".

## D018 — GP2 is traded on `segment_video`'s audio stream (2026-07-26, from M35 review, narrows D017)

D017's rationale reads "these verbs never need to touch audio, so re-encoding it
is pure loss." That holds for three of the four verbs, and **not literally for
`segment_video`**, which must cut the audio as well as the video. Surfaced by the
M35 review's diff-bug lens (scored 55 — bookkeeping, not a behavioral defect);
recorded here because GP2 is tradeable only *with stated justification*, and
until now the trade was unstated.

- **The trade.** GP2 is "Cutting re-encodes for frame accuracy by default"
  (D008). With `reencode = TRUE` the video is still cut at the exact timestamp,
  but a stream-copied audio track now cuts at the nearest packet boundary.
  Measured on a 1 s output-seek cut of a 25 fps / AAC fixture: audio
  `start_time = 0.007007`, `duration = 0.998458`, against the previous
  re-encode's `0.000000` / `1.000000`; video identical at `0.040000` / `0.960000`
  in both.
- **Why the trade is accepted.** The error is bounded by one audio frame (~7 ms
  here) and is plausibly smaller than the encoder delay the old re-encode path
  introduced on its own. Against it: that path silently re-encoded to a
  build-dependent codec on every cut. GP2 guards frame accuracy on the *video*
  cut, which is what "frame" means and what is unchanged.
- **The escape hatch is exact, not approximate.** A caller who needs the audio
  cut sample-accurately passes `audio_codec` a real encoder, which restores the
  pre-M35 behavior deliberately instead of by default.
- **Scope.** Narrows D017's rationale only. D017's default, its guards, and its
  batch column all stand unchanged, and no code changes.

## D019 — `audio_codec` where the filter forces a re-encode (2026-07-26, from M36, extends D016/D017)

Carries the codec-arg boundary rule to `normalize_audio()` and
`normalize_audio_batch()` — the first verbs to take an `audio_codec` where a
stream copy is *impossible* rather than merely undesirable. Sits under IP1/GP1.

- **Default `audio_codec = NULL`, D016's sentinel — not D017's `"copy"`.** D017's
  default rests on "these verbs never need to touch audio, so re-encoding it is
  pure loss." Loudness normalization is the opposite case: it filters the audio,
  so the stream is re-encoded no matter what and copy is not an available
  behavior. The sentinel preserves every pre-existing command byte-for-byte, so
  this milestone changes no default output. Rules out transferring D017's
  `"copy"` default by analogy to any verb that merely handles audio.
- **`"copy"` is refused at the verb, not only at the engine.** Layer 1 already
  aborts a filtered stream carrying `codec_audio = "copy"` (`ffm_groups()`, M02
  D-M02-5) and remains the enforcement point, so IP1 holds. The Layer-2 helper
  `check_audio_codec_not_copy()` adds what Layer 1 cannot: it names
  `audio_codec` rather than `ffm_codec()`, and it runs *before*
  `run_loudnorm_analysis()`, so a two-pass call fails without first burning an
  analysis pass per row. Rules out relying on the engine's message alone.
- **Batch: `audio_codec` is a per-row column** (`NA` → sentinel), reusing
  `check_batch_codec_col(col =)` and `batch_codec_cell()` — never the numeric
  knob-column guard beside it, which rejects `NA` and so cannot spell "unset".
- **The two-pass correction is a second seam, and it is threaded.**
  `normalize_audio_batch(two_pass = TRUE)` bypasses its own `ffm_batch()` call
  and fans out through `run_normalize_correction()`; the codec reaches both.
  Rules out treating `normalize_audio_pipeline()` as the single seam it appears
  to be when read from the scalar verb alone.
- **`video_codec` stays out.** This verb's contract is "touch audio only",
  pinned by `-codec:v copy`; changing that needs its own decision.

## D020 — Codec args subsume `reencode` on the demux verb (2026-07-26, from M37, extends D016/D017)

Carries the codec-arg boundary rule to `separate_audio_video()` and
`separate_audio_video_batch()` — the first verbs where a codec argument
*replaces* an existing boolean rather than joining one. Sits under IP1/GP1.

- **`audio_codec` and `video_codec` default to `"copy"`, and `reencode` is
  removed.** `reencode` was a single boolean over two output files, so it could
  say "copy both" or "leave both to the container" and nothing else — including
  nothing about *which* encoder. Two per-stream args say all three: `"copy"`
  compiles what `reencode = FALSE` compiled byte-for-byte, `NULL` (D016's
  sentinel) compiles what `reencode = TRUE` compiled, and a name pins the
  encoder. The default output is therefore unchanged. Rules out keeping
  `reencode` beside the codec args, which would need a contradiction guard for
  every combination the two spellings can disagree on.
- **`"copy"`, not D016's `NULL` sentinel, because this verb demuxes.** D019
  chose `NULL` for `normalize_audio` on the ground that a filtered stream is
  re-encoded no matter what. Demuxing is the opposite case: copy is not only
  available but is what the verb has defaulted to since D-M06-4, so `"copy"`
  keeps the default and D017's reasoning transfers intact.
- **A clean break with no `lifecycle` shim**, under D014's pre-0.2.0 policy, at
  the user's explicit waiver of the deprecation cycle (2026-07-26 plan gate).
- **The batch verb aborts on a stale `reencode`; the scalar does not need to.**
  The scalar has no `...`, so R rejects the retired argument itself. The batch's
  `...` forwards `ffm_batch` options and would swallow `reencode` in silence —
  stream-copying output the caller asked to have re-encoded, a wrong result with
  no signal. A guard naming the replacement is a diagnostic, not a shim: it
  never makes the old spelling work. Rules out relying on `...`'s tolerance,
  and rules out closing `...` to unknown names, which would break the
  forwarding slot every `_batch` verb depends on.
- **The reshape collapses the two codec columns into one.** An input row fans
  out into an audio row and a video row (D003/D007), so the per-row
  `audio_codec` / `video_codec` columns resolve to a single `codec` column on
  the 2N table, routed by the existing `stream` marker — which is also what
  makes it structurally impossible for one stream's choice to reach the other's
  command. The column is carried only when `jobs` supplied one. Rules out
  carrying both columns on every row, where half of each is dead weight.

## D021 — The codec-argument sweep closes (2026-07-26, from M40, extends D016/D017/D019/D020)

Renames `convert_audio()`/`convert_audio_batch()`'s `format` argument to
`audio_codec` and declares the codec-argument sweep (M34–M40) complete. Uses
D014's `audio_codec` spelling and its pre-0.2.0 clean-break policy; sits under
IP1/GP1.

- **`format` was an audio codec in all but name.** Its own documentation said
  "naming the output audio codec … passed to FFmpeg's `-c:a`", so this is a
  spelling correction, not a behavior change — M22's naming audit simply missed
  it while retiring `acodec`/`vcodec`. A clean break with no `lifecycle` shim,
  under D014. Rules out keeping a second spelling for the one argument in the
  package that named a codec something other than `audio_codec`.

- **`NULL` keeps meaning `-q:a 0`, and that departs from D016's sentinel on
  purpose.** Everywhere else `audio_codec = NULL` emits nothing and defers to
  the container; here it selects highest-VBR-quality encoding, which is what
  `format = NULL` has always compiled. Transferring D016's sentinel would have
  changed every existing default call's output to win a consistency the rename
  does not need — the argument's *name* is what was wrong, not its default.
  Settled at the 2026-07-26 plan gate. The asymmetry is documented on the
  `@param` rather than left for a reader to discover. Rules out both a silent
  behavior change and an `"auto"`-style third spelling.

- **The per-row column gains what the argument already had.** `format` was
  guarded by `check_batch_string_col()`, which rejects `NA`, so a jobs table
  could not say "leave this row on the default" — the one thing the scalar
  could say. `check_batch_codec_col()` + `batch_codec_cell()` fix that, matching
  every other codec column (D016/D017/D019).

- **Both retired spellings abort naming the replacement.** The batch verb's
  `...` would swallow a stale `format` argument, and a stale `format` column
  would fall through as one of the ignored columns — either way silently
  ignoring the codec the caller named. The scalar sibling needs no guard: with
  no `...`, R's own `unused argument` covers it (M37's precedent, same
  reasoning).

- **Three verbs stay deliberately codec-less, all on D016's hidden-codec side.**
  `format_for_web()` is D016's own exemplar: a fixed recipe (H.264 / `yuv420p` /
  `+faststart` / AAC) whose identity *is* the codec choice, so exposing one
  would let a caller contradict the verb's name. `strip_metadata()` copies every
  stream by identity — it edits the container, never the picture or the sound,
  and a codec argument would turn a metadata edit into a transcode. And
  `concatenate_videos()` uses the concat *demuxer*, which requires that the
  inputs already share a codec and joins them without decoding; a codec argument
  there would be inert at best and would misrepresent the verb at worst (the
  re-encoding route is the concat *filter*, a separate design call under IP2's
  linear-builder limits). Rules out a blanket "every verb gets a codec arg"
  reading of the sweep: D016's boundary rule is configurable-transform vs.
  fixed-recipe, and these three are the fixed-recipe side.

- **`extract_audio()`'s asymmetry is left standing, recorded not fixed.** It
  takes `audio_codec = "copy"` but validates with `check_string()` scalar-side
  and `check_batch_string_col()` column-side, so unlike every other
  `audio_codec` it accepts neither `NULL` nor `NA` — it cannot spell "unset".
  That is defensible (a `NULL` codec on a pure extraction hands the container's
  default encoder a stream the verb exists to copy) but it is undocumented as a
  choice, and it is the last inconsistency the sweep leaves behind. Noted here
  so a later milestone finds a decision rather than an oversight.

The sweep is closed on **spelling and shape**: every configurable transform now
exposes the codec argument its stream needs, spelled `audio_codec` /
`video_codec`, carried as a per-row column guarded the same way. It is
deliberately **not** closed on semantics, and the two bullets above are what a
later milestone should read rather than this line: `convert_audio`'s `NULL`/`NA`
means `-q:a 0` rather than "unset", and `extract_audio` accepts neither. Anyone
treating the family as uniform in what `NA` *means* will be wrong on two of the
verbs.

## D022 — What `NULL` and a column `NA` mean across the codec family (2026-07-29, from M42, closes the semantics D021 left open; supersedes D021's `extract_audio` bullet; extends D016/D017/D019/D020)

D021 closed the codec-argument sweep on **spelling and shape** and left semantics
open, warning that "anyone treating the family as uniform in what `NA` *means*
will be wrong on two of the verbs." Measured over all 34 codec verb × argument
pairs (`data-raw/codec-guard-baseline.R`, `codec_guard_semantics()`), the count
was seven, not two. This entry sets the rule and names the one departure that
survives it. Sits under IP1/GP1; uses D014's pre-0.2.0 clean-break policy.

- **The rule: `NULL` means "emit no `-codec:v` / `-codec:a` at all", and a
  column `NA` is the column form of `NULL`.** D016 introduced the sentinel for
  one verb family; it is now the family-wide spelling of "unset", carried by
  `apply_video_codec()` / `apply_audio_codec()` scalar-side and by
  `check_batch_codec_col()` + `batch_codec_cell()` column-side. Rules out a
  per-verb answer to what "unset" means — the reading D021 warns against.

- **The three aborts were accidents of placement, not choices, and are
  removed.** `anonymize_video()` aborted only because `anonymize_pipeline()`
  called `check_token()` unconditionally, while its twin
  `standardize_pipeline()` routes through the sentinel-aware path and compiles
  the identical call; `extract_audio()` aborted while `extract_audio_batch()`
  has always compiled `audio_codec = NULL`. D021 read the `extract_audio` case
  as "defensible … a `NULL` codec on a pure extraction hands the container's
  default encoder a stream the verb exists to copy" and recorded it as an open
  question rather than a settled choice. The measured disagreement with its own
  batch sibling settles it the other way, and **supersedes that bullet of
  D021** — including its claim that `extract_audio` "accepts neither `NULL` nor
  `NA`" read as a statement about the pair, which was never true of the batch
  verb.

- **`NULL` is the only exit from the container trap on the two verbs shipping a
  literal codec default.** `standardize_video()` and `anonymize_video()` default
  to `video_codec = "libx264"` — precisely what D016 rejected as a *general*
  default because it forces H.264 into a `.webm`. Those two keep the literal
  default, since a documented standard profile is their contract, so `NULL` is
  how a caller opts out of it. Refusing `NULL` there would leave D016's named
  trap with no exit at all.

- **Every codec column takes `check_batch_codec_col()` + `batch_codec_cell()`.**
  Three did not: `standardize_video_batch` and `anonymize_video_batch`'s
  `video_codec` (an inline `str_cols` no-`NA` loop) and `extract_audio_batch`'s
  `audio_codec` (`check_batch_string_col()`). Both `str_cols` comments justified
  the guard by calling `video_codec` "a literal `libx264` default with no
  sentinel"; the argument accepts `NULL`, so the premise was false when written.
  Rules out `check_batch_string_col()` on any codec column — it cannot spell
  "unset", which is this entry's whole content. `pixel_format` and `color` stay
  in `str_cols`: not codec arguments, no sentinel.

- **A *scalar* `NA` still aborts, and that is not an exception.** `NA` spells
  "unset" only as a column cell, where a per-row table has no other way to leave
  one row alone; passed as the scalar argument it is a type error, refused at
  the front door by M41's guards before `batch_codec_cell()` is ever reached
  (M41-D2). Rules out reading "a column `NA` means `NULL`" as licence to accept
  `NA` wherever the family expects a string.

- **`convert_audio()` / `convert_audio_batch()` are the one surviving
  departure: `NULL` and a column `NA` select `-q:a 0`.** D021's reasoning stands
  unchanged — the rename corrected the argument's *name*, and transferring the
  sentinel would silently change the output of every existing default call.
  Reaffirmed at M42's 2026-07-29 implement gate. It is documented on the
  `@param` and asserted in the family test table as an expected departure rather
  than skipped, so the exception cannot decay back into an accident.

- **No existing command changes.** Every call that passes neither `NULL` nor a
  column `NA` compiles byte-for-byte what it compiled before; the change is
  strictly widening — calls that aborted now compile. Rules out reading this
  entry as a behavior change to existing pipelines, and is why it ships without
  a deprecation cycle beyond D014's standing policy.

## D023 — Audio-track selection: `audio_stream` indexes streams, `audio` indexes inputs (2026-07-30, from M43, narrows D009; extends D003)

The audio verbs now name the track they take instead of leaving it to FFmpeg.
This entry fixes what the selector means, why it is a second argument rather
than a widening of D009's `audio =`, and the Layer-1 contract change that
carries it. Sits under IP1/D002; uses D014's pre-0.2.0 clean-break policy.

- **Two arguments, two bases, and the difference is which thing is being
  counted.** `audio_stream` on `extract_audio()` / `convert_audio()` (+ `_batch`)
  is a 0-based index **among one input's audio streams** — `1` is that file's
  second audio track. D009's `audio =` on `compare_videos()` /
  `picture_in_picture()` is a 0-based index **among the verb's inputs** — `1` is
  the second *file*. Both read as "0-based audio index" and neither can be
  computed from the other, so they stay separate names. Rules out reusing
  `audio` for track selection, which would make one argument mean two things
  depending on the verb's arity.

- **Selection is stated, never inherited from the file's flags.** `extract_audio()`
  emitted no `-map` at all, so FFmpeg applied its default-stream heuristic,
  which prefers whichever track carries the container's DEFAULT disposition.
  That made the extracted track a property of the input's flags and of the
  FFmpeg build, not of anything the caller wrote — the invisible variation the
  package exists to remove. Both verbs now compile an explicit `0:a:<n>` on
  every call. Rules out respecting the DEFAULT disposition, including as a
  fallback: a heuristic consulted only sometimes is still a heuristic.

- **`NULL` means "no selection", which resolves to the first audio track — it is
  not D016's emit-nothing sentinel.** The codec family's `NULL` removes an
  option from the command; here there is no such reading, because the map is
  always emitted. A column `NA` is the column form of that same `NULL` and so
  keeps its row on the first track, overriding the argument rather than
  deferring to it — the family's rule from D022, applied to a numeric column.
  Rules out reading D022's `NULL`/`NA` equivalence as also transferring the
  codec family's *meaning* to every new argument.

- **`ffm_map()` appends; `replace = TRUE` narrows.** Selecting a track needs a
  map that can sit beside another (keep the video, name one audio track), which
  overwriting made impossible — a second call silently discarded the first. The
  builder now appends and renders one `-map` per element, and `mapping` takes a
  character vector. `replace = TRUE` is the escape hatch appending needs:
  `ffm_copy()` sets the all-streams map `0`, and appending to that duplicates a
  stream rather than narrowing to it. No verb's compiled command changes — each
  sets its map once, pinned by a test. Rules out append-only, which strands
  `ffm_copy()`'s map with no way to narrow it. `replace` has no in-package
  caller today and ships anyway, because the alternative removes an ability
  `ffm_map()` has now.

- **What this does not settle.** The pass-through verbs
  (`separate_audio_video`, `standardize_video`, `crop_video`, `segment_video`,
  `anonymize_video`) still take whatever their `-map 0` carries; extending the
  selector to them stays a ROADMAP candidate, now unblocked because this entry
  fixes the argument's shape. `separate_audio_video()`'s multi-track abort is a
  separate candidate — that one is about how many tracks an output holds, not
  which one it takes.
