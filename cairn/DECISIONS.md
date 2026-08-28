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

## D024 — The pure surface is compilation and `run = FALSE`; diagnostics may probe the executing path (2026-07-30, from M44/RR02, clarifies D013)

DESIGN.md's Conventions section says: "Command **compilation** is pure and
CI-safe (no binaries); command **execution** tests `skip_if` the
ffmpeg/mediainfo binaries are absent (D004)." M44 needed to count an input's
audio streams — an FFprobe call — so it could warn a caller whose extra audio
tracks the output silently drops, and read that line as forbidding it. RR02
found the reading wrong: the convention constrains *compilation*, and the
executing path has never been binary-pure. D011's `verify=` has run FFprobe on
these very verbs' executing path since it shipped
(`extract_audio_batch(..., verify = ...)` reaches it today) and nobody wrote a
carve-out for it. So this entry is a **clarification** of a line that was read
too broadly, not a second exception beside D013. Writing it as an exception
would ratify the wrong reading and guarantee a third entry the next time
anything probes anything.

**The pure surface, stated exactly.**

- `ffm_compile()` and every builder it walks run no binary from any path. That
  is a property of those functions, not of a window of time during a verb call.
  (`ffm_run()` and `ffm_batch()` carry the same `@family builder functions` doc
  tag and do run binaries; the purity claim is about the pipeline builders
  `ffm_compile()` walks, never about that documentation family.)
- Every verb's `run = FALSE` call runs no binary — with **the two-pass
  normalization path the sole exception**: `normalize_audio(two_pass = TRUE)`
  and `normalize_audio_batch(two_pass = TRUE)` both run D013's analysis pass
  before `run` is consulted. D013 recorded that consequence and it stands; this
  entry adds no second exception to `run = FALSE` and narrows none.
- A `run = TRUE` call may run a binary before or after compilation, provided
  the conditions below hold. "Before or after" is deliberate: compilation is
  pure, so nothing can observe the ordering, and a rule hanging on it would
  protect nothing. **"Executing path" means the call has `run = TRUE`** — it is
  not a claim about what has already run.

**The operative rule is about effect, not about where the bytes go.** A probe
may run on a `run = TRUE` path when its outcome — ran, skipped, succeeded,
failed — changes nothing observable except whether a diagnostic condition is
signalled. The compiled command, every resolved default, whether execution
proceeds, and which pipeline executes must be identical under all four
outcomes.

Four things are therefore outside the licence, and each needs its own decision
entry before it is built:

- a probe whose result enters the compiled command — this is D013's shape;
- a probe that resolves a default the caller did not set, which D023
  independently forbids ("a heuristic consulted only sometimes is still a
  heuristic");
- a probe that decides whether execution proceeds — an abort gate is not a
  diagnostic, and an abort gate that fails open silently stops gating;
- a probe that selects between pipelines, which also breaks `run = FALSE`,
  there being no single command a dry run could return.

The narrower test "its result is not in the compiled command" is a corollary,
not the rule: an abort gate passes that test and is still outside the licence.

**Fail-open is a consequence, not a policy.** A probe whose only permitted
effect is a diagnostic must fail open, because failing closed — aborting on an
absent binary or an unreadable input — would give it a second effect and put it
outside the licence by its own terms. Two implementation consequences follow,
neither obvious: `find_program()` warns and `run_program()` aborts on a missing
binary, so silence has to be built rather than inherited; and a diagnostic that
can silently not run must say so in its documentation, because a contract
promising more than the code delivers is exactly the reliance a warning invites.

**Scope: conditions, not a verb list.** A verb may run a binary on its
`run = TRUE` path when (i) the outcome affects nothing but a diagnostic
condition, (ii) it fails open, (iii) it never runs on the `run = FALSE` path,
and (iv) it never runs from `ffm_compile()` or any builder it walks. First
instances: the dropped-audio-track warning on `extract_audio()`,
`convert_audio()`, and their `_batch` siblings. A verb adopting a probe under
these same conditions records the adoption in its own milestone's decision log;
a probe that stretches any condition needs a new D-entry. Rules out both a verb
enumeration — which would force a content-free entry the moment the warning
reaches another verb — and a predicate about narrowing a multi-track input,
which states this warning's *occasion* and would be misread as the licence
*condition*: M45, where `NULL` means every track and nothing narrows by
default, would wrongly read itself as excluded from diagnostics entirely.

## D025 — `audio_stream = NULL` means every track on `separate_audio_video()`, the first track on the extraction verbs (2026-07-30, from M45, extends D023; annotates D024)

M43 gave `extract_audio()` / `convert_audio()` an `audio_stream` whose `NULL`
resolves to the first audio track. M45 gives `separate_audio_video()` the same
argument name and the same counting base with the opposite `NULL`: every audio
track. This entry records the split, why the two are different questions, and
what it costs a caller who uses both families.

- **The D023 bullet this departs from**, verbatim:

  > **`NULL` means "no selection", which resolves to the first audio track — it
  > is not D016's emit-nothing sentinel.** The codec family's `NULL` removes an
  > option from the command; here there is no such reading, because the map is
  > always emitted.

  Its reasoning is untouched on the verbs it was written for, and half-untouched
  here: the map is always emitted on this verb too, so `NULL` is "no selection"
  and not "emit nothing". What differs is what no selection resolves *to*, which
  D023 fixed only for the two verbs in front of it.

- **`0:a` and `0:a:<n>` answer different questions, and D023 said so.** Its
  closing bullet already separated them: "`separate_audio_video()`'s multi-track
  abort is a separate candidate — that one is about how many tracks an output
  holds, not which one it takes." An extraction verb writes one audio stream by
  construction, so its unselected case must pick one, and "the first" is the only
  non-heuristic answer available. A separation verb writes whatever the caller's
  container holds, so its unselected case has a second answer — all of them — and
  that is the answer it has given since it shipped.

- **What made the divergence the cheaper option.** Uniformity here is not free,
  it is measured breakage: `-map 0:a` into `.mka` / `.m4a` carries all three
  tracks of a three-track input today (measured 2026-07-30, ffmpeg 8.1.2), and
  M44's dropped-track warning does not cover this verb, so a first-track default
  would have narrowed those callers to one track in silence. Rules out the
  uniform default, and rules out a second argument name too: the base being
  counted is identical (0-based among one input's audio streams), so a second
  name would make a caller learn one concept twice.

- **The cost, stated.** One argument name now carries two defaults across two
  verb families, so a caller using both must read which. Both `@param` blocks say
  so and name the other family. Falsified by a report of a caller confused by the
  split, which reopens the choice under D014's pre-0.2.0 clean break.

- **The milestone absorbed one verb from the pass-through candidate.** The ROADMAP
  row carrying `audio_stream` to the five pass-through verbs no longer covers
  `separate_audio_video`; M45 took that one alone, because the row's promotion
  condition had not fired and the full five-verb carry trips the sizing tripwire.
  That row must now settle whether the four remaining verbs follow this entry's
  every-track `NULL` or D023's first-track one — `standardize_video`,
  `crop_video`, `segment_video` and `anonymize_video` all pass audio *through*,
  so on their face they are the separation shape rather than the extraction one.

- **The abort is Layer 2's, and its probe adopts D024 rather than extending it.**
  The enriched multi-track abort names `audio_stream`, so it is raised in the verb
  and never in `ffm_run()` (IP1/D002). The FFprobe call behind it runs only after
  FFmpeg has already failed and decides only which abort is signalled, never
  whether execution proceeds — D024's licence, on the four conditions recorded in
  M45's own decision log (M45-D1 for the scalar verb, M45-D2 for the batch).

## D026 — The pass-through verbs state their stream selection; `NULL` keeps every audio track (2026-07-30, from M47, extends D023/D025; answers the question D025's fifth bullet left open)

D025 closed by naming a question and handing it to the ROADMAP row this
milestone promotes:

> That row must now settle whether the four remaining verbs follow this entry's
> every-track `NULL` or D023's first-track one — `standardize_video`,
> `crop_video`, `segment_video` and `anonymize_video` all pass audio *through*,
> so on their face they are the separation shape rather than the extraction one.

The answer is M45's every-track reading, and the reason is not the one D025
guessed at. "They pass audio through" turned out to be true of only half of
them, and the half where it was false was failing in a way nobody had measured.

- **What these verbs actually did, measured.** On a 3-audio-track + 1-subtitle
  `.mkv` (ffmpeg 8.1.2, macOS), `standardize_video()` and `anonymize_video()`
  emitted **no `-map` at all**, so FFmpeg's implicit selection applied: one
  stream of each type, preferring whichever audio track carries the container's
  DEFAULT disposition. With DEFAULT moved to track 1 the output carried the
  **second** track and dropped the other two, silently. `crop_video()` and
  `segment_video(reencode = FALSE)` emit `-map 0` and did carry all three. So
  the four were never one family: two of them were narrowing three tracks to
  one by a rule the caller never wrote and could not see, which is precisely
  D023's second bullet — "a heuristic consulted only sometimes is still a
  heuristic" — stated there in terms that were never verb-scoped.

- **The map is emitted on every call, and `NULL` means every audio track.**
  `NULL` → `-map 0:v? -map 0:a?`; a named track → `-map 0:v? -map 0:a:<n>`.
  This keeps `crop_video()` and the `segment_video()` copy path compiling what
  they compile today and stops the other two consulting the heuristic. Rules
  out D023's first-track `NULL`, which would have narrowed the `-map 0` verbs
  from every track to one — a data loss, to buy uniformity with verbs whose
  output *is* an audio stream. An extraction verb's unselected case must pick
  one track because it writes one; a pass-through verb's need not, and D025
  already established that the two questions are different.

- **The trailing `?` on the unselected specifiers is load-bearing.** A bare
  `-map 0:a` aborts FFmpeg outright on an input with no audio (exit 234,
  "Stream map '' matches no streams"), and a bare `-map 0:v` does the same on
  an audio-only input — both ordinary research inputs, and both cases where
  the pre-M47 code exited 0 and passed the stream through. The **named**
  specifier deliberately carries no `?`, so naming a track the input lacks
  stays an FFmpeg error, which is what every `@param audio_stream` in the
  package promises (D023). Rules out a uniform `?`, which would turn a
  mistyped index into a silently audio-less output, and rules out an FFprobe
  guard, whose result would enter the compiled command and so sit outside
  D024's licence.

- **Subtitle and data streams are not carried, and that is a change.** A
  uniform `-map 0` would carry them and was rejected on measurement: `-map 0`
  into `.mp4` on a subtitle-bearing input fails outright (exit 8, no default
  mp4 subtitle encoder). That failure is one `crop_video()` already has today
  and M48 removes. Into `.mkv` these two verbs previously carried one subtitle
  and now carry none. A `subtitle_stream`/`video_stream` selector is the
  standing ROADMAP candidate row; `0:v` rather than `0:v:0` leaves video
  unnarrowed until that row is promoted.

- **The cost, stated.** `audio_stream` now carries two defaults across three
  verb families: the first track on the four extraction entry points, every
  track on the six others. All ten `@param` blocks name the families that read
  it the other way. Falsified by a report of a caller confused by the split,
  which reopens the choice under D014's pre-0.2.0 clean break.

- **Scope.** M47 applies this to `standardize_video()` and `anonymize_video()`
  (+ `_batch`); M48 applies the identical rule to `crop_video()` and
  `segment_video()`, where it additionally has to narrow `ffm_copy()`'s
  `-map 0` rather than append beside it.

## D027 — `ffm_copy()` assigns its map; a conflicting prior map aborts (2026-07-30, from M48/RR03, adds to D023's fourth bullet without narrowing it)

M43 changed `ffm_map()` from overwrite to append (D023, fourth bullet) so a
pipeline could keep the video and name one audio track. `ffm_copy()` sets its
all-streams map through `ffm_map()`, so it inherited the append and stopped
being idempotent: `ffm_copy() |> ffm_copy()` compiled `-map 0 -map 0` and
duplicated every output stream, and `ffm_concat()` calls `ffm_copy()`
internally so `ffm_concat() |> ffm_copy()` doubled too (measured 2026-07-30,
compile-level). Settled by Fable review RR03
(`cairn/reviews/archive/RR03-ffm-copy-idempotence.md`). Sits under IP1/D002 and
IP2/D003; uses D014's pre-0.2.0 clean-break policy.

- **`ffm_copy(streams = TRUE)` assigns**, setting the map through
  `ffm_map(object, "0", replace = TRUE)`. The reason is the specifier, not
  idempotence: `"0"` subsumes every other specifier the linear builder can
  address (one input chain, IP2/D003), so `c(existing, "0")` has no composition
  in which it is what the caller wanted — if the existing map is `"0"` it
  duplicates every stream, and if it is narrower (`"0:v"`) it duplicates that
  selection's streams. An operation whose right-hand side subsumes any possible
  left-hand side is an assignment. Rules out de-duplicating inside `ffm_map()`,
  which changes D023's contract yet leaves the subsumption half
  (`ffm_map("0:v") |> ffm_copy()`) standing, and rules out appending `"0"` only
  when absent, which fixes the literal instance and reopens on the next
  subsuming pair.

- **`ffm_map()`'s append contract is untouched.** D023's fourth bullet stands
  verbatim. `object$map` remains the builder's only accumulating field — every
  other `ffm_*` setter assigns — and that exception is earned by `ffm_map()`'s
  arguments being partial selections that genuinely compose. `ffm_copy()`'s
  `"0"` is not one.

- **A conflicting prior map aborts rather than being discarded.**
  `ffm_copy(streams = TRUE)` on a pipeline whose map is non-empty and not
  identical to `"0"` raises a classed `tidymedia_*` condition naming both legal
  spellings: `ffm_copy(streams = FALSE)` to keep the existing map, or
  `ffm_copy()` first and `ffm_map(replace = TRUE)` after. Without the guard,
  assignment would silently discard a stated selection — the precise flaw D023
  was written to remove ("a second call silently discarded the first"). The
  package already decided this shape one field over: `segment_pipeline()`
  aborts on an `audio_codec` that `ffm_copy()` would silently overwrite
  (M35/D017). The message is worded around the *pipeline's* map, never the
  caller's frame, because `ffm_concat()` calls `ffm_copy()` internally and a
  user chain `ffm_map(…) |> ffm_concat()` trips the guard from a frame the user
  never called. Rules out a warning: the compiled command is the product
  (D001), and an abort relaxed to a warning later is backward-compatible where
  the reverse breaks running code.

- **The `identical(map, "0")` carve-out is load-bearing.** It is what keeps
  `ffm_copy() |> ffm_copy()` and `ffm_concat() |> ffm_copy()` silent no-op
  restatements rather than aborts, and it is literal: `c("0", "0")` is not
  identical to `"0"`, so `ffm_map(c("0", "0")) |> ffm_copy()` aborts.

- **What this does not establish.** "The compiled command never maps the same
  stream twice" is not reachable at Layer 1 — deciding whether `"0:a:1"`
  overlaps `"0:a"` means implementing FFmpeg's stream-specifier algebra in R,
  the full-coverage growth GP1/D001 refuses. The guard is one-directional:
  `ffm_copy() |> ffm_map("0:a:1")` still compiles `-map 0 -map 0:a:1` silently,
  and under D023 that is the user's stated selection. Two compositions change
  behavior with no in-package or test caller today:
  `ffm_hstack() |> ffm_map("0:a") |> ffm_copy()` now aborts, while
  `ffm_hstack() |> ffm_copy()` still compiles `-map "[vout]" -map 0` and
  duplicates the video stream, which no criterion in M48 addresses.

- **No deprecation.** The appending behavior removed here shipped in M43 and
  has never been in a release; no in-package pipeline performs any doubling
  composition, and the two in-package `ffm_copy()` callers
  (`strip_metadata_pipeline()`, `segment_pipeline()`) plus `ffm_concat()` all
  call it on an empty map, so every existing compiled command is byte-identical.
  NEWS plus the rewritten `@param streams` is the whole migration (D014).

## D028 — `normalize_audio()`'s unselected `audio_stream` is the first track, not every track (2026-07-31, from M49, narrows D026; extends D023/D025)

D026 gave the pass-through verbs one rule for the unselected case — `NULL` →
`-map 0:v? -map 0:a?`, every audio track — and named `standardize_video`,
`anonymize_video`, `crop_video` and `segment_video` in its Scope bullet. Two
verbs were outside that bullet and still emitted no `-map` at all:
`format_for_web()` and `normalize_audio()`. M49 closes both. One of them takes
D026's rule unchanged; the other cannot, and this entry records why.

- **What the two verbs actually did, measured.** On a 3-audio-track `.mkv`
  (ffmpeg 8.1.2, macOS; languages eng/spa/fra, DEFAULT disposition moved to
  track 2), both verbs emitted **zero** `-map` arguments, so FFmpeg's implicit
  selection applied and each output carried only `fra` — the **third** track,
  chosen by the container's DEFAULT flag rather than by anything the caller
  wrote. The two-pass analysis pass did the same thing independently: with no
  map of its own it sent stream `#0:3` to `loudnorm`, so measurement and
  correction agreed only by both consulting the same heuristic.

- **`format_for_web()` takes D026's rule unchanged.** `NULL` →
  `-map 0:v? -map 0:a?`, a named track → `-map 0:v? -map 0:a:<n>`. Nothing
  about this verb argues for narrowing: it re-encodes audio to AAC into `.mp4`,
  a container that holds many audio tracks, and its output is a video file
  rather than an audio stream. This is the case D026 already decided.

- **`normalize_audio()` takes a first-track `NULL`, spelled `0:a:0?`.** Under
  `-map 0:a?` the two-pass analysis pass prints one JSON measurement block **per
  mapped audio track** — three, measured on the fixture above — while
  `classify_loudnorm_output()` reads `hit[[1]]` (`R/loudnorm_two_pass.R`). So an
  every-track `NULL` would correct every mapped track with **track 0's**
  measurements, and would do it silently: the command succeeds, the file is
  written, and the only symptom is that two of the three tracks are normalized
  to the wrong loudness. This is a defect the every-track rule would *introduce*,
  which is what distinguishes it from D026's cases.

- **This is a determinism change, not a cardinality one.** The verb already
  carried exactly one audio track; it just could not say which. Ruling out
  D026's uniform every-track `NULL` therefore costs no data, unlike the
  symmetric case D026 itself ruled out — narrowing `crop_video()`'s `-map 0`
  from three tracks to one, which would have.

- **Ruled out: making the analysis pass per-track so uniformity is affordable.**
  One measured set per mapped track needs per-stream filter options, and the
  linear builder has no slot for them (IP2/D003 keeps it one filter chain). That
  is a real feature and now its own ROADMAP candidate row — it is the falsifier
  for this entry, not an objection to it. Also ruled out: leaving
  `normalize_audio()` unmapped, which is the status quo this milestone exists to
  remove, and which D023's second bullet already refuses in terms that were
  never verb-scoped.

- **The analysis pass carries the audio half only.** `-map 0:a:0?` or
  `-map 0:a:<n>`, never `0:v?`: that pass writes to `-f null` and has no output
  for a video selection to describe. Measured indistinguishable from the
  `0:v?`-carrying pair in exit code and block count on a 3-track `.mkv` and a
  video-only `.mp4` alike. The invariant that matters is not that the analysis
  and correction commands *look* alike but that they name the same **audio**
  track, which is asserted directly (M49-D1 records this).

- **The `?` is load-bearing on the unselected spelling and absent from the named
  one**, exactly as D026 established: a bare `-map 0:a:0` exits 234 on a
  video-only input where the unmapped code exited 0, while a named `0:a:9` on a
  3-track input must stay an FFmpeg error rather than compile to a silently
  audio-less output (D023).

- **The cost, stated.** `audio_stream` now carries two defaults across
  **eighteen** exported entry points: the first track on `extract_audio`,
  `convert_audio` and `normalize_audio` (+ `_batch`), every track on the six
  pass-through verbs (+ `_batch`). D026 stated this cost for a ten/four split
  and accepted it; M49 moves `normalize_audio` from the every-track side of the
  ledger to the first-track side, so the split is now three families deep rather
  than two, and the reason for the third is a property of the two-pass path
  rather than of the argument. Falsified by a report of a caller confused by the
  split, which reopens the choice under D014's pre-0.2.0 clean break — the same
  falsifier D026 named, now carrying one more case.

## D029 — `normalize_audio()`'s video map follows the output container (2026-07-31, from M49's review send-back, narrows D028's video half; extends D026)

D028 gave `normalize_audio()` the map pair `-map 0:v? -map 0:a:0?` and stated
its change as "determinism, not cardinality". That is true of the **audio**
half and false of the **video** half, and the video half broke real calls. This
entry narrows it. D028's first-track audio rule, and its measured reason, stand
unchanged.

- **What broke, measured.** `-map 0:v?` forces the input's video stream into the
  output muxer. The `?` makes a specifier optional when the **stream** is
  absent; it does nothing when the stream is present and the **muxer refuses
  it**. On `inst/extdata/sample.mp4` (ffmpeg 8.1.2, macOS), against master:
  `.wav`, `.mp3`, `.aac` and `.opus` went from exit 0 to **exit 234** ("wav
  muxer does not support any stream of type video", zero bytes written), and
  `.mka` silently **gained** a video stream where master wrote audio alone.
  `normalize_audio("interview.mp4", "interview.wav")` is an ordinary research
  call and D028 broke it.

- **Why D028 did not see it.** Master delegated the whole selection to FFmpeg's
  implicit rule, which is **muxer-aware**: it dropped video for a container that
  could not hold it. That same rule picked the audio track by DEFAULT
  disposition, which is the defect M49 exists to remove — so the mechanism was
  right about video and wrong about audio, and D028 replaced both halves when
  only the audio half was at fault. `0:v?` came from D026, written for verbs
  whose product **is** a video file; `normalize_audio()`'s product is whatever
  container the caller named.

- **The rule.** `normalize_audio()` (and `_batch`) emit `-map 0:v?` unless the
  **output path** names an audio-only container, in which case the audio map
  stands alone. Keyed on the output extension the caller wrote — not on the
  input, and not on a probe — so the compile stays binary-free under
  `run = FALSE` (D024). The list lives in `AUDIO_ONLY_CONTAINERS`
  (`R/ffmpeg.R`), and it is deliberately one-directional: an extension absent
  from it keeps the pass-through shape, so a missing entry costs an unusual
  audio container its pre-M49 behavior and can never cost a video caller their
  video.

- **This is a stated rule, not a restored heuristic.** D023's objection is to a
  selection the caller never wrote and cannot see. Here the caller writes the
  extension, the rule is documented on `@param audio_stream` and in NEWS, and
  the compiled command shows the result. What is *not* reinstated is FFmpeg
  choosing which audio track to normalize.

- **Two deliberate divergences from master, both toward the caller's intent.**
  `.m4a` carried a video stream on master and carries none here, because a
  caller writing `.m4a` means audio. `.ogg` exited 234 on master (H.264 cannot
  be copied into Ogg) and now succeeds, because no video is mapped into it.

- **Ruled out.** Dropping the video map unconditionally — it would strip video
  from `.mp4`/`.mkv` outputs and contradict the documented `-codec:v copy`
  pass-through, a worse regression than the one being fixed. An opt-out
  argument — it leaves the default broken and makes the caller discover the
  switch after hitting exit 234. Probing the input to decide — forbidden on the
  compile path by D024, and unnecessary, since the output path already carries
  the caller's intent.

- **The coverage gap that let this ship.** No test in the package normalized to
  an audio container, so a fully green suite sat over the regression; review
  caught it, not the suite. `test-audio-stream-normalize.R` now walks the
  container matrix under execution, and the walk was verified to fail against
  the pre-fix code. Falsified by an audio container that this list does not
  name and that a caller reports failing — which is an addition to the list,
  not a reopening of the rule.

## D030 — `normalize_audio()` produces audio and no video (2026-07-31, from M49's second review send-back, supersedes D029 and narrows D028's video half)

D029 made `normalize_audio()`'s video map conditional on the output container,
via an enumerated `AUDIO_ONLY_CONTAINERS` list. Review measured six audio-only
containers the list did not name — `.w64`, `.mpa`, `.voc`, `.sbc`, `.latm`,
`.adts` — each going from exit 0 on master to exit 234 (or 176) on the branch.
That was the second attempt at the same question and the second miss, so the
question is removed rather than answered again. D029's list and predicate are
deleted; this entry replaces them.

- **The rule.** `normalize_audio()` and `normalize_audio_batch()` compile
  exactly one map — `-map 0:a:0` unselected, `-map 0:a:<n>` when a track is
  named — and never a video map. The compiled command does not depend on the
  output container at all, which is what makes "did we enumerate every
  audio-only container?" unanswerable by construction rather than answered
  again. `-codec:v copy` is gone with it: it named a stream that is never
  mapped, and the compiled command is the product (D001).

- **What this costs, stated plainly.** Normalizing a recording's soundtrack
  *while keeping its picture* was possible on master in one call and is not
  possible with this verb any more. That is a real capability removed, not a
  clarification, and it is the reason this entry exists rather than a doc fix.
  The replacement path is to normalize to an audio file and mux it back with
  the `ffmpeg()` escape hatch; a first-class verb for it is a ROADMAP candidate
  row created by this entry.

- **Why an audio verb, and not a third try at the predicate.** `normalize_audio`
  re-encodes audio by construction and its product is an audio stream — the
  shape `extract_audio()` and `convert_audio()` already have, and the reason
  D023 gives those verbs a first-track `NULL`. The pass-through family
  (`crop_video`, `standardize_video`, …) keeps D026 unchanged; nothing here
  touches it. Ruled out: extending the list, which the thrash rule identifies as
  buying the next missing extension rather than a fix; an opt-out argument,
  which leaves the broken default in place until a caller discovers the switch;
  and probing the output muxer, which D024 forbids on the compile path and which
  would make `run = FALSE` and `run = TRUE` compile different commands.

- **The unselected map carries no trailing `?`, and that is measured.** When
  EVERY map specifier is optional and matches nothing, FFmpeg discards the maps
  and reverts to default stream selection: `-map 0:a:5?` on a video+audio file
  writes video AND audio, the map ignored entirely. This verb emits exactly one
  map, so "all maps matched nothing" is reachable by an ordinary input — a
  silent screen recording — and with a `?` that call would exit 0 while writing
  the video through, by way of the very DEFAULT-disposition heuristic M49
  removes. Without it the input fails at exit 234, "Stream map '' matches no
  streams". An input with no audio is therefore an error, which is the honest
  answer for a verb whose output is audio. This also supplies the measured
  reason behind D026's rule that named specifiers carry no `?`; that was
  reasoning when written and is now evidence.

- **Scope.** `normalize_audio()` and `normalize_audio_batch()` only. D028's
  first-track `NULL` and its measured reason stand. D026 and the pass-through
  verbs are untouched — `format_for_web()` keeps `-map 0:v? -map 0:a?`, which is
  right for a verb whose product is a web video file, though writing it to an
  audio container fails as it does for every pass-through verb (recorded as a
  ROADMAP candidate row, not fixed here).

- **Falsified by** a caller who needs normalized audio muxed back over the
  original video often enough that the escape hatch is not an answer — which
  promotes the candidate row into a verb, and does not reopen this rule.

## D031 — What the compiled command string quotes, and why quoting it cannot reach FFmpeg (2026-07-31, from M50; states a convention that was unowned prose)

Nothing in DESIGN.md or DECISIONS.md said which tokens the compiled command
string wraps in double quotes. It had accumulated one file at a time — paths,
filter graphs, the automatic `[vout]` map — and M47's `-map 0:v?` was the first
metacharacter to reach a token nobody had quoted, so the string the vignette
sells as the thing you inspect, log, and paste stopped surviving a paste: zsh
answers `no matches found: 0:v?`.

- **The rule.** The display string quotes, at minimum: every input and output
  **path**, every **filter graph** (`-vf`, `-af`, `-filter_complex`), and every
  **map specifier** (`-map`), including the automatic `[vout]` one. Map
  specifiers join that list here; the rest were already there. Adding a token
  class to the list is additive and needs no entry; removing one is a change to
  what a reader can paste, and needs one.

- **Left bare, deliberately:** codec names (`-codec:v libx264`), pixel formats,
  seek values, and the raw output-option passthrough (`-movflags +faststart`,
  `-q:a 0`, `-f null`). The first three are single clean tokens today by their
  own validators, so quoting buys nothing; the passthrough cannot express it at
  all — it hands `ffm_group()` a
  finished `display` (`R/ffm.R:1331`) and so bypasses the quoting mechanism
  without a signature change. A ROADMAP candidate row carries that work,
  promoted by the first report of one of those classes breaking a pasted
  command.

- **`quote` is an INDEX, not a level.** `ffm_group(args, quote = 2L)` means
  "quote `args[[2]]`" — a positional index into the group's own argument vector,
  and the display renderer's sole quoting style is `paste0('"', x, '"')`
  (`R/ffm.R:1175-1183`). `quote = 2L` recurs across the file because the value
  is the second element of a two-element option group, not because there is a
  level 2. Note this is the DISPLAY renderer's style, not the package's only
  one: execution quotes separately and differently, per the next bullet.

- **Why this cannot reach FFmpeg.** `ffm_groups()` returns both renderings of
  each group, and quoting exists only in `display`. `ffm_compile()` pastes the
  `display` fields (`R/ffm.R:1149-1152`); `ffm_run()` executes `ffm_args()`, the
  `args` fields, and never the display string (`R/ffm.R:1161-1164`,
  `:1383-1389`). Execution then does its own quoting, which is why the display
  style cannot leak into it: `run_program()` hands the vector to
  `system2(location, args = shQuote(args, type = quote_type))`
  (`R/program_management.R:113-121`), one element per argument, `shQuote`d for
  the platform's shell — `"sh"` elsewhere, `"cmd"` on Windows. The safety of a
  path carrying spaces or `$` comes from that `shQuote`, not from bypassing a
  shell. The two renderings coming from one
  structure is M06's reason for the split, and it is what makes a display-only
  change provable rather than argued: M50 pins `ffm_args()` for all fourteen
  in-package pipelines in a snapshot recorded before the change, and the snapshot
  did not move.

- **Falsified by** any token FFmpeg receives whose VALUE differs from the value
  the display string shows — path, filter graph, or map specifier alike; the two
  renderings quote differently by design, so only the values are comparable.
  That would mean the split had drifted, and is a bug in the split rather than
  in this rule. Note the display string is a
  reproducibility artifact, not a shell-escaping library: a path containing a
  double quote still renders unescaped, which predates this entry and is
  untouched by it.

## D032 — `audio_stream` and `audio` stay two names; the docs, not the API, carry the disambiguation (2026-07-31, from M51, re-confirms D023's first bullet at eighteen verbs; extends D025/D026/D028)

D023 fixed the two counting bases when the selector reached two verbs. It now
reaches eighteen, reads `NULL` two ways across two families, and compiles in
four spellings: the named `0:a:<n>`, and three unselected ones — `0:a:0` on the
first-track family (`R/ffmpeg.R:273`, `:2142`, `R/loudnorm_two_pass.R:45`), a
bare `0:a` on `separate_audio_video()` (`R/ffmpeg.R:569`), and the optional
`0:a?` on the verbs that pass video through (`R/ffmpeg.R:335`). There is no
`0:a:0?`: a *named* track deliberately carries no `?`, so a mistyped index stays
an FFmpeg error rather than a silently audio-less output (`R/ffmpeg.R:322-327`).
This entry re-confirms the two-name call at that scale and records what M51
shipped in place of a rename.

- **The D023 bullet re-confirmed**, verbatim:

  > **Two arguments, two bases, and the difference is which thing is being
  > counted.** `audio_stream` on `extract_audio()` / `convert_audio()`
  > (+ `_batch`) is a 0-based index **among one input's audio streams** — `1`
  > is that file's second audio track. D009's `audio =` on `compare_videos()` /
  > `picture_in_picture()` is a 0-based index **among the verb's inputs** — `1`
  > is the second *file*. Both read as "0-based audio index" and neither can be
  > computed from the other, so they stay separate names. Rules out reusing
  > `audio` for track selection, which would make one argument mean two things
  > depending on the verb's arity.

- **Nothing that grew since D023 touches its reason.** The two `NULL` readings
  (D025/D026/D028) and the `?` suffix (M47) are both about *how much* audio a
  verb takes and *whether a missing track aborts*; neither changes *what is
  being counted*, which is the only thing the bullet fixes. Scale is not an
  argument against it either: eighteen verbs sharing one well-documented base
  is the bullet working, not failing.

- **What shipped instead of a rename.** A user-facing `audio_stream` topic
  (`?audio_stream`, aliased `audio-tracks` / `audio_indices`) covering both
  bases, both `NULL` readings, the `NA`-cell-versus-absent-column rule, and the
  three unrelated things `audio` names (input index; a codec string on
  `ffm_codec()`; a logical on `ffm_copy()`). All twenty-two verbs carrying
  either argument link to it, asserted by a test that enumerates the parameters
  across `man/*.Rd`, and share a `@family audio selection functions`.

- **The family enumerations are now generated, not written.** `R/audio-stream-doc.R`
  holds the two verb vectors and the `@param` text built from them; the
  eighteen blocks call it through an inline `` `r ` `` roxygen expression. This
  is why the entry can be a documentation answer rather than an API one: the
  failure a rename would have pre-empted — a `@param` block naming the wrong
  siblings — was live in four blocks when M51 started, and is now
  unrepresentable rather than merely detected.

- **Falsified by** a report of a caller confused by the two names or the two
  `NULL` readings, which is D025's and D026's stated falsifier and reopens the
  choice under D014's pre-0.2.0 clean break. Ruled out as a trigger: the count
  alone. Eighteen verbs is not eighteen confused callers, and a rename paid for
  by a headcount would be paid by every existing caller.

## D033 — `furrr` fan-out crosses to the metadata side (2026-08-06, from M53, extends D007/D012)

`probe_all(parallel = TRUE)` fans its per-file FFprobe calls out with
`furrr::future_map()`, honoring the active `future::plan()`. The default stays
`FALSE`, and the four `probe_*()` shortcuts pass `parallel` through exactly
where they already pass `typed` — on the `infile` branch, ignored when handed a
`probe` object.

- **What is new is the side, not the site count.** Before this milestone
  `grep -rn "furrr::" R/` returned three call sites in two files
  (`R/ffm_batch.R:102`, `:132`, `R/loudnorm_two_pass.R:197`), and all three sit
  on the **execution** side — running FFmpeg jobs. This is the first fan-out on
  the **metadata-reading** side. M53's plan originally framed the entry as
  recording "a second `furrr` fan-out"; that count was wrong when it was
  written, and the framing is corrected here rather than carried forward.

- **D007 is not violated, and this entry is what stops that reading eroding.**
  D007 fixes batch processing as "a single tibble-in/tibble-out runner" and
  rules out "vectorizing individual verbs". `probe_all()` is neither: it is a
  metadata reader that has always been vectorized over `infile`, so giving it a
  worker pool adds no second runner and vectorizes no verb. The line D007 draws
  is around the **engine's execution model** — one input chain, one output, one
  runner (D003) — not around every use of `future`.

- **What may NOT follow from this.** A `parallel` argument on a *scalar* verb,
  a second runner beside `ffm_batch()`, or a parallel mechanism other than
  `furrr`/`future`. Each of those is the thing D007 and D003 rule out, and this
  entry licenses none of them. Parallelizing the `mediainfo_*()` readers or the
  `get_*()` helpers is not ruled out — it is simply unplanned (a ROADMAP
  candidate, not a decision).

- **The guard fires here, unlike loudnorm's Phase 1.**
  `run_loudnorm_analysis_batch()` fans out silently and leaves
  `warn_if_sequential_plan()` to the `ffm_batch()` call that follows it, "so it
  fires exactly once" (`R/loudnorm_two_pass.R:162-171`). That rationale needs a
  downstream call that warns. `probe_all()` is a terminal entry point with
  none, so it emits the guard itself — otherwise `parallel = TRUE` under the
  default sequential plan is a silent no-op, which is the case D012 added the
  guard for. Callers therefore see two warnings from one call when the plan is
  sequential and a file is unprobeable; the file warning stays a single
  end-of-call report naming every failure, which is what the fan-out had to
  preserve.

- **Only `probe_one()` is fanned out.** The failure accumulator and the
  end-of-call warning stay in the parent process; moving them into workers
  would make that one report one-per-worker or none. Only `probe_one()` shells
  out, so the parallelism given up is free.

- **Falsified by** a profile showing the parent-side assembly, rather than the
  FFprobe spawns, dominating `probe_all()` on a large corpus — which would mean
  the fan-out is drawn around the wrong part — or by a caller needing a
  parallel mechanism `future` cannot express, which reopens D012's choice.

## D034 — A probe whose result enters the compiled command runs when the pipeline is built, `run` notwithstanding (2026-08-06, from M54, supersedes D024's `run = FALSE` bullet; extends D013)

D024 stated the pure surface with a bullet that hand-listed its exceptions:

> Every verb's `run = FALSE` call runs no binary — with **the two-pass
> normalization path the sole exception**: `normalize_audio(two_pass = TRUE)`
> and `normalize_audio_batch(two_pass = TRUE)` both run D013's analysis pass
> before `run` is consulted.

That sentence was false when it was written. `resolve_hw_encoder()` reaches
`has_nvenc()` → `ffmpeg_encoders()` → `ffmpeg("-encoders")` while the pipeline is
being built, so `standardize_video(hardware = "nvenc", run = FALSE)` shells out —
measured 2026-07-30 under a counting mock, and again at M54. nvenc shipped at M31
on 2026-07-26, four days *before* D024; the list was falsified by code already in
the package, not by anything that came later. This entry **supersedes that
bullet**, and `cairn/DESIGN.md`'s Conventions bullet, which carried the same
claim, is corrected with it.

**The rule, as a condition on probe shape.** A probe whose result enters the
compiled command runs when the pipeline is built. `run` gates execution, never
construction — so `run = FALSE` promises a *command*, not a binary-free call.
This is D013's analyze-then-build shape, which D024's own taxonomy already names
("a probe whose result enters the compiled command — this is D013's shape") and
already treats as licensed. D024's error was not the licence but the bookkeeping:
it enumerated the shape's instances where it should have stated the shape.

**What the condition covers today, by procedure rather than by recall.** Grep
`R/` for the execution seams — `run_program(`, `ffmpeg(`, `ffprobe(`,
`mediainfo(` — and keep the call sites reachable while a pipeline is being built.
Two survive: D013's loudnorm analysis pass (`R/loudnorm_two_pass.R:140,182`) and
the nvenc resolver (`R/ffmpeg.R:2283`, whose sole internal caller is `has_nvenc()`
at `:2388`). `ffmpeg_codecs()` has no internal caller at all. Re-run the grep
rather than trusting this pair — a reader who trusts the pair has reproduced the
exact mistake this entry exists to correct.

**What it does not change.** D024's diagnostic licence stands unnarrowed,
including its condition (iii) that a diagnostic probe never runs on the
`run = FALSE` path — that is a different shape (its result reaches a condition,
never the command) and its tests still pin it. No runtime behavior changes here:
every call compiles and executes exactly what it did before.

**Rules out** making the probe lazy. Deferring resolution to `ffm_finish()` /
`ffm_batch()` — the only readers of `run` — needs the pipeline-object hook
D024/RR02 Q3 rejected, and would force a `run = FALSE` call to name an encoder it
had not verified, so a dry run on a GPU-less machine would print a command that
aborts on execution. Weighed and rejected at M54's plan gate. Also rules out
reading `run = FALSE` as a sandbox: it is a command-preview switch, and a caller
who needs a binary-free call needs the option seam or a machine without the verb.

- **Falsified by** a report of a dry run's compiled command differing from what a
  subsequent `run = TRUE` call executes — which would mean construction-time
  probing had begun deciding more than the encoder name — or by a third
  build-time probe appearing that the stated grep does not find, which would mean
  the procedure, not the list, is the thing that is wrong.

## D035 — A probe already licensed under D034 may also gate at the front door (2026-08-07, from M57, licenses one instance of the shape D024's third exclusion reserved; extends D034)

D024 lists four probe shapes outside its diagnostic licence, "each need[ing]
its own decision entry before it is built". The third, verbatim:

> - a probe that decides whether execution proceeds — an abort gate is not a
>   diagnostic, and an abort gate that fails open silently stops gating;

This entry licenses one instance of that shape: the nvenc availability check,
run a second time at the front door of the nine verbs that fan out through
`ffm_batch()`, so an unavailable encoder blames the verb the user called
instead of `purrr::pmap()`.

**Why the exclusion does not settle it either way.** The abort gate D024
refused was a *new* effect — a probe that would stop a call the package would
otherwise have run. This one stops nothing that was not already stopped:
`resolve_hw_encoder()` has aborted on an unavailable encoder since nvenc
shipped at M31, and D034 already licenses that probe running while the pipeline
is built. M57 changes *where* the identical abort is raised, never *whether*
one is. D024's clause is about a gate's existence; it does not reach a gate's
position, and reading it as forbidding this would forbid moving any existing
abort earlier in any verb.

**The rule.** A probe already licensed under D034 — its result entering the
compiled command — may also run at a verb's front door, before any fan-out,
when three conditions hold:

- **One abort site.** The front door and the pipeline reach the abort through
  one shared function, so no wording and no firing condition exists in two
  places to drift apart. Here that function is `check_nvenc_available()`, which
  `resolve_hw_encoder()` calls rather than carrying its own copy.
- **No new refusal.** Every call the front-door guard aborts is a call the
  pipeline would have aborted. The guard changes the `conditionCall()` and the
  moment of failure, never the set of calls that fail. It does reassign
  precedence *within* a failing call — an unavailable encoder now reports
  before validations that live in the pipeline — which is M41's known cost and
  is tested for, not assumed away.
- **It fails closed, and must.** D024's fail-open requirement is a consequence
  of the diagnostic licence, whose probes may have no effect but a message.
  This probe is not a diagnostic and never was: an unavailable encoder has to
  stop the call, or `hardware = "nvenc"` would silently encode in software.

**What it does not license.** A front-door probe with no pipeline counterpart —
one refusing a call nothing downstream would refuse — is a new abort gate and
still needs its own entry. So does a probe adopted under D024's diagnostic
licence that later grows an abort; that is the fail-open clause, and it stands
unnarrowed.

**Rules out** hoisting resolution to the front door — resolving the encoder
once there and handing the name down. Weighed at M57's plan gate and rejected:
it re-forks the resolver seam for the per-row `video_codec` column that seven
of the eight `_batch` verbs honour, and it undoes M56's fix that made
`standardize_pipeline()` hand `hardware` to the seam *unresolved*, so the token
check sees the user's value rather than `resolve_hw_encoder()`'s rewrite of it.

- **Falsified by** the front-door guard and the pipeline guard observed firing
  on different inputs — which would mean the one shared function had stopped
  being shared — or by any call the front door refuses that a `run = FALSE`
  pipeline still compiles.

## D036 — An argument contradiction reports before an availability probe (2026-08-07, from M58, supersedes D035's precedence example; states the front-door checker shape M59 inherits)

M57 moved the nvenc availability abort to the front door of every verb that
fans out, and D035 licensed that move. M58 does the same for six *argument
contradictions* — a stream copy that also names GPU hardware, an audio encoder
with no audio mapped, a resize across other than two inputs. Both now sit at
the same five front doors, so which of them reports has to be decided rather
than left to whichever line happens to come first.

**D035's licence question does not arise here.** D024's exclusions are about
*probes*: things that consult FFmpeg, the filesystem, or anything outside the
call. None of the six contradictions consults anything — each compares two
values the verb already holds — so D024 and D034 are not engaged and no licence
is needed. What M58 takes from D035 is its **shape**, not its permission: one
shared abort site, and no call refused that the pipeline would not have refused.

**The rule.** Where a verb carries both, **the contradiction reports first.**

A contradiction is decided identically on every machine; availability is not.
Under M57's order the same wrong call was diagnosed two ways depending on the
local FFmpeg build — a `video_codec = "copy"` batch naming `hardware = "nvenc"`
was told about the copy on a GPU machine and about the missing encoder
everywhere else. That is the failure mode M54 named: an error whose identity
depends on the machine cannot be reasoned about from a bug report. Ordering the
machine-independent answer first removes the dependence.

**What this supersedes.** D035's second condition ("No new refusal") carries a
worked example: "an unavailable encoder now reports before validations that live
in the pipeline". For these six that example is now false — the six no longer
live only in the pipeline, and they report first. **The condition itself stands
unchanged**, and M58 satisfies it: measured over a 112-cell grid across both
refs, the same 33 cells are refused before and after, and only the blame moves
(`data-raw/contradiction-guard-baseline.R`). What is superseded is the example,
not the rule it illustrates.

**The checker shape.** A shared front-door checker takes **one row's already
resolved values** and answers for that row; the fan-out verb resolves its
override columns to per-row values and calls the checker once per row. The
alternative — a checker taking the whole jobs table and sweeping it itself —
was rejected at M58's question gate because the single-call pipelines would then
have to hand it a one-row stand-in table, putting table shapes into code paths
that have never carried one. Row-by-row is also what makes a mixed column
answerable at all: a table with one violating row is refused for that row while
a table with none compiles, where an all-or-nothing gate does neither (the shape
M57's review caught on `segment_video_batch`).

- **Falsified by** a contradiction whose detection turns out to need the encoder
  list — which would make it machine-dependent and put it back behind
  availability — or by a user report preferring the availability error on a
  mixed column.

---

## D037 — IP1 governs command assembly, not validation (2026-08-07, from M59's plan amendment; states the scope of an existing principle rather than changing it)

M59 was planned on the reading that IP1 "puts validation logic in Layer 1
once", making a Layer-2 front-door check a principle violation needing an
exception. That reading is wrong, and it survived a plan, a criteria-audit
line and a routing decision before a fresh-context reader caught it.

**What IP1 actually says** (`cairn/DESIGN.md:62-64`, verbatim): "Layer 2 task
verbs are thin wrappers that never glue their own command strings; all
assembly, quoting, and copy-vs-re-encode logic lives once in Layer 1." Every
clause is about building the command. Validation is not mentioned.

**The standing counterexamples.** Layer 2 front doors validate pervasively and
always have: `rlang::arg_match()`, `check_number_whole()`, `check_token()`, and
the whole `check_batch_*_col()` family. Two D-entries put checks there on
purpose — D035 licensed hoisting the nvenc availability probe to nine front
doors, and D036 did the same for six argument contradictions and fixed their
precedence. If IP1 governed validation, both would be exceptions to it, and
neither records one, because none is needed.

**The rule.** A Layer-2 verb may validate its own arguments at its front door
without engaging IP1. What IP1 still forbids is unchanged: a Layer-2 verb
assembling, quoting, or copy-vs-re-encode-branching its own command string.

**What this does not license.** Duplicating a check's *wording* or its
vocabulary across layers is still a defect — the M40 stale-hint lesson and
M59's AC2 both bite on it — but it is a drift-and-maintenance problem, not an
IP1 violation, and the remedy is one shared checker, never an IP exception.

- **Falsified by** a DESIGN.md amendment extending IP1 to validation locality,
  which would retroactively make D035 and D036 exceptions and so needs to say
  what happens to them; or by a Layer-1 checker whose abort cannot be aimed at
  a Layer-2 caller at all, which would force validation back down a layer for
  mechanical reasons rather than principled ones.

---

## D038 — D036's contradiction-first rule is scoped to the swept form; the scalar-argument form is a disclosed gap (2026-08-07, from M59's review, narrows D036)

D036 states its rule without qualification: "Where a verb carries both, **the
contradiction reports first.**" M59's review measured that this is false for one
form of one class of call, and M59 recorded the exception in its own acceptance
criterion, in NEWS, on two help pages and in a ROADMAP candidate row — but not
here, which is the only file a later contributor is obliged to read. That is the
gap this entry closes; the behavior it describes is unchanged.

**What is actually true.** On `compare_videos_batch()` and
`picture_in_picture_batch()`, a value violation arriving in a `jobs` column is
swept at the front door *after* the M58 contradiction sweep, so the
contradiction reports — D036 as written. The same violation passed as a scalar
*argument* is caught by the verb's own `direction`/`position`/`margin` guard,
which sits at the top of the function, above the contradiction sweep, and
reports the value instead:

    compare_videos_batch(jobs, direction = "sideways", audio_codec = "aac")
    #> `direction` must be one of "horizontal" or "vertical", not "sideways".

    compare_videos_batch(jobs_with_direction_column, audio_codec = "aac")
    #> `audio_codec` needs an audio stream to encode.

**This ordering predates both M58 and M59.** Those scalar guards have sat at the
top of their verbs since M32; M58 added the contradiction sweep below them and
M59 placed its column sweep below that. Neither milestone moved a scalar guard,
so no branch introduced the disagreement — what M59 introduced is a second form
of the same mistake that answers differently.

**Why it is recorded rather than fixed.** Making the two agree means moving each
scalar guard below the contradiction sweep, which also reorders it against every
other front-door check above it — the jobs-shape guards, `check_token()` on both
codecs, `arg_match(hardware)` — none of which has a test or a changelog line
pinning its position today. The work is that disclosure and its tests, not the
move, and it is the reordering-unremarked failure M41's review caught twice. It
carries a ROADMAP candidate row.

**D036's own reasoning says the gap should close.** A contradiction is decided
identically on every machine and that is why D036 puts it first; nothing in that
argument distinguishes a column from an argument. So this entry scopes D036's
rule to what the code does, and does not defend the difference.

- **Falsified by** any report of a caller confused by the two forms answering
  differently, or by a milestone that pins these verbs' front-door ordering —
  at which point the exception should close rather than be re-recorded.

---

## D039 — A value error and a contradiction resolve the same way in both forms (2026-08-08, from M61, supersedes D038 and restores D036 unconditionally)

D036 stated its rule without qualification: "Where a verb carries both, **the
contradiction reports first.**" D038 measured that this was false for one form
of one class of call on `compare_videos_batch()` and `picture_in_picture_batch()`
— a value violation arriving in a `jobs` column obeyed the rule, while the same
violation passed as a scalar argument was caught by a guard at the top of the
verb and reported instead — and recorded the difference as a disclosed gap
rather than defending it. This entry closes the gap. D036 is again true as
written, in both forms, and D038's exception is retired.

**What moved.** Four front-door value guards, and only four: `direction`
(compare), `position` and `margin` (pip), and the per-row `audio` bound (both).
Each now runs *below* its verb's M58 contradiction sweep, where its column
counterpart already sat. The two vocabulary guards live in the shared
`*_pipeline()` functions, which the SCALAR `compare_videos()` and
`picture_in_picture()` also call and which is their only vocabulary check — so
those two verbs answer the new way as well, on the same reasoning and with
their own cells in the grid. `picture_in_picture_batch()`'s `audio` index gained a
front-door sweep it never had — before, it was re-checked only inside the
fan-out closure, so an out-of-range column cell was reported against
`purrr::pmap()` naming the closure's local `aud` (M59 review F7). The set was
closed by inspection, not by a procedure, and the milestone-local decision entry
(M61-D1) names the commit it was closed at.

**The guards moved rather than being deleted.** Three of the four are also
covered by a per-row sweep that resolves a column over the argument, so deleting
the scalar guard looked equivalent. It is not: a sweep never sees a bad argument
that a column overrides, and all three scalar guards refuse such a call today
(measured — `compare_videos_batch(jobs_with_audio_column, audio = -1)` aborts).
Deleting one would have lost a refusal, which D035's "no new refusal" condition
governs in the other direction and this milestone's scope forbids outright.

**What was measured.** `data-raw/value-guard-baseline.R` now crosses each guard,
in each form, with each front-door error that could report instead of it — the
contradiction, `check_nvenc_available()`, and `ffm_batch()`'s own `run` guard —
each paired with a control asserting the crossed error is live on that call.
Where a verb carries two contradictions, each guard is crossed with both:
`compare_videos_batch()` carries an `audio_codec` one and a `resize` one, and
they are separate members of the crossing list rather than one standing for the
pair. Over 128 cells against both refs: no refusal changed, no message
regressed, no blame regressed, no abort lost its `call`, no control was dead,
and no combination went uncovered. Fourteen cells change which error they
report — nine scalar-argument cells on the `_batch` verbs crossed with a
contradiction, three on the scalar verbs, and pip's `audio` column crossed with
the availability and `run` guards, whose front-door guard is new.

**The crossings are generated, not listed.** Three review rounds each returned
this milestone on a different combination of that cross-product being absent
from the grid, every one a cell nobody had typed out. So the grid declares the
(verb, value) pairs, the two forms and the per-verb crossings once and builds
every combination from them; each guard supplies only what cannot be derived —
the shape of a call to its verb, and which value violates it. A companion
reader, `value_guard_uncovered()`, re-derives the same product and reports any
combination with no cell, so completeness is checkable from the grid's output
rather than by eye. What the reader cannot catch is a crossing dropped from the
shared declaration; what it does catch is the failure that actually recurred.

Both the reader and the control validator were then verified by mutation rather
than by eye, and both were wrong on their first pass. Deleting a whole guard
from the specs reported one missing combination where eight were owed, because
a variant cell (`audio(low)`) was read back to its base value and stood in for
the guard it merely supplements; the reader now matches the bare value name, so
a variant is extra coverage and never the coverage the criterion asks for
(1 → 8 rows on the mutation). And a control was validated against the error
*class* rather than the crossing it names, so a control for one of compare's
contradictions passed when the other fired instead — the same conflation an
earlier round returned on, relocated into the validator; it now compares at
crossing grain (4 → 7 dead controls on the mutation, the three newly caught
being exactly the `direction` controls that had fallen through).

**The four guards also report after every check that stays above them.** D038
named this consequence and called its disclosure "the work": a call wrong in
both one of these four values and in an earlier argument check — a malformed
codec token, an unrecognized `hardware`, a `resize` that is not `TRUE` or
`FALSE` (`compare_videos_batch()` only), a non-numeric `scale`
(`picture_in_picture_batch()` only), a `jobs` table of the wrong shape — is now
told about the earlier check. No refusal changes;
only which error is shown. NEWS states it, and the grid pins the three crossings
that the ordering rule itself is about.

**Which bound, not only which value.** D038 noted that for `audio` the answer
varied "even by which bound was crossed", and that is why the grid probes
compare's `audio` at both. Its upper bound already sat below the sweep; only the
lower bound moved. A grid probing one bound would have measured no change and
reported the milestone complete.

**And which value.** `audio` is the one guard here whose value decides whether
the contradiction exists at all, because supplying an index is what gives the
encoder something to encode. An in-range index removes the contradiction and so
does an out-of-range one; what does not is an **NA-ish** value, which
`batch_stream_cell()` resolves to `NULL` — dropping the audio while still being
a value the argument guard refuses. That helper tests `is.na()`, and
`is.na(NaN)` is `TRUE`, so the reachable set is every length-1 NA-ish value,
`NA` and `NaN` alike. Both are probed, with `audio = NULL` as the control — an
in-range control would remove the very error it exists to prove live.

This took two rounds to state correctly, and the two errors have one shape:
reasoning from the values in hand to a universal. The milestone first recorded
the cell as one that could not exist, on the reasoning that "supplying `audio`
at all removes the contradiction" — false at `NA`. Its replacement said the
pairing was reachable "at exactly one value" — false at `NaN`. What survives
both is the mechanism rather than the enumeration: the pairing is reachable
exactly where `audio` is non-`NULL` and `batch_stream_cell()` resolves it to
`NULL`. The non-`NULL` half is not pedantry — that helper returns `NULL` for
input `NULL` too, which is why `audio = NULL` is the control here, and a
biconditional omitting it would admit a call carrying no value error at all.

**What this does not change.** `rlang::check_bool(resize)`, the jobs-shape
guards and every column *type* guard stay above the contradiction sweep, for the
reasons M61-D1 records — the first because the contradiction checker consumes
`resize` and degrades to unattributed base-R errors without its type guard, the
rest because the row-sweep reads the table's shape. `check_token()` on both
codecs and `arg_match(hardware)` also stay: their column counterparts already
sit above the sweep, so moving them would create the disagreement this entry
removes.

- **Falsified by** a caller who needs the value error first on a call that also
  contradicts itself — the case D036's machine-independence argument does not
  reach — or by a fifth guard turning up non-uniform, which would mean the set
  was closed by inspection over the wrong surface rather than that the rule is
  wrong.

## D040 — A verb's front door may read the filesystem to refuse a missing input (2026-08-08, from M62, licenses a second instance of the shape D024's third exclusion reserved; takes D035's shape, not its licence; narrows D036's ordering)

D024 lists four probe shapes outside its diagnostic licence, "each need[ing]
its own decision entry before it is built". The third, verbatim:

> - a probe that decides whether execution proceeds — an abort gate is not a
>   diagnostic, and an abort gate that fails open silently stops gating;

This entry licenses a second instance of that shape: `file.exists()`, run at
the front door of every verb that fans out through `ffm_batch()` and of the two
scalar fan-in verbs, so a call naming a file that is not there blames the verb
the user called instead of `purrr::pmap()` or `ffm_files()`.

**Why D035's licence does not carry, only its shape.** D035's rule opens on a
condition this probe fails: "A probe already licensed under D034 — its result
entering the compiled command — may also run at a verb's front door". A file's
existence never enters the compiled command. The *path* does, and it is written
into the command whether or not anything is at the end of it; that is precisely
why an absent input is a runtime failure today rather than a compile-time one.
So D034 does not reach this probe, D035's rule cannot be invoked for it, and
what M62 takes from D035 is its **shape** — one shared abort site, no new
refusal, fails closed — under an entry of its own.

**Purity is untouched, which is not the same as unengaged.** DESIGN.md's
Conventions and D024 protect a *binary*-pure compilation surface; `file.exists()`
runs no binary, so nothing here costs the CI-safety claim, and unlike D035's
nvenc probe this guard needs no FFmpeg build to be decided. What it does engage
is D024's reach past the call — the filesystem is outside the call in the sense
D024 means — and that is why an entry is needed at all rather than nothing.
That this guard runs on the `run = FALSE` path is D035's precedent, not a new
departure: the front-door nvenc gate has run there since M57.

**The rule.** A verb's front door may test its resolved input paths for
existence and abort, when three conditions hold:

- **One abort site.** `check_paths_exist()` (`R/utils.R`) is where the
  package's missing-input abort is written, and every front door reaches it —
  the single-input verbs through `check_file_exists()`, the fan-out verbs and
  the two scalar fan-in verbs this entry exists for through
  `check_batch_inputs()`. No wording and no firing condition exists in two
  places to drift apart. Reaching it is not enough on its own: a carrier handed
  to the predicate untyped raises base R's error before the site is reached, so
  the site coerces (M62 review F1).
- **No new refusal, with one disclosed asymmetry.** Every call the front door
  refuses is a call `ffm_files()` would have refused inside the fan-out. The
  converse does not hold, and the reason is that the two predicates differ:
  `check_paths_exist()` tests existence, `ffm_files()` tests readability. An
  input that exists but cannot be read is therefore still refused only by
  `ffm_files()`, still inside the fan-out, still blaming `purrr::pmap()`. That
  residual is M63's scope, is pinned by a test asserting `ffm_files()` and its
  `ffm` alias are the only other place an input refusal is worded, and is
  disclosed here rather than assumed away.
- **It fails closed, and must.** D024's fail-open requirement is a consequence
  of the diagnostic licence, whose probes may have no effect but a message.
  This is not a diagnostic: a missing input has to stop the call, or FFmpeg
  would be handed a path to nothing.

**Ordering, and what D036 does and does not decide.** The sweep reports *before*
the M58 contradiction sweep, which is the opposite direction to D036's rule that
the machine-independent answer comes first — and a file's existence is
machine-dependent in the strongest available sense. D036's argument is not
reached here, because its subject is an error whose *identity* changes with the
machine while the call stays the same: "a `video_codec = "copy"` batch naming
`hardware = "nvenc"` was told about the copy on a GPU machine and about the
missing encoder everywhere else", which "cannot be reasoned about from a bug
report". A missing input does not vary that way. It varies with the caller's own
data, which the caller has and the report names — `` `jobs$input` names 1 file
that does not exist: 'clip3.mp4' `` is fully actionable by the person who typed
the path, and reproducing it elsewhere is not what anyone needs to do with it.
Ordering it first also keeps both forms uniform: thirteen scalar verbs already
put this exact refusal above everything else, and leaving the table-driven form
alone would reinstate the by-form disagreement D039 closed.

**What this does not license.** Reading anything *about* an input but its
existence — its size, its container, its streams — is a probe of a different
kind whose result would shape the command, and stays under D013/D034. A
front-door abort with no pipeline counterpart, refusing a call nothing
downstream would refuse, remains a new abort gate needing its own entry; the
asymmetry disclosed above is the reverse of that shape, a pipeline refusal the
front door does not yet reach. Output paths and `outdir` creation are untouched:
this entry is about inputs, and an output that does not exist yet is the normal
case.

- **Falsified by** a report preferring the contradiction on a table that is both
  wrong about a path and self-contradictory — the case D036's argument does not
  reach either way — or by a report of an existing-but-unreadable input still
  reporting differently by form after M63 ships, which would mean the residual
  above was a permanent split rather than a staged one.

## D041 — One predicate refuses an input, and both ends reach it (2026-08-08, from M63, closes the residual D040 disclosed; narrows D040's "One abort site" and "No new refusal" conditions)

D040 licensed a front-door existence test and disclosed, in the same entry, the
one thing that test could not reach:

> An input that exists but cannot be read is therefore still refused only by
> `ffm_files()`, still inside the fan-out, still blaming `purrr::pmap()`. That
> residual is M63's scope [...] and is disclosed here rather than assumed away.

This entry closes it, and the way it closes matters. The front door did not
acquire a second, wider test of its own; the two tests became one. The site
D040 named holds `ffm_files()`' predicate — `file.access(mode = 4)` — and
`ffm_files()` now reaches that site instead of writing a refusal of its own.

**Why unification rather than a second test.** Copying readability up to the
front door would have satisfied the same acceptance criterion and left the
defect intact: two predicates spelled the same way today drift apart at the
first edit, and drift is precisely what D040's one-abort-site condition exists
to prevent. With one predicate there is nothing left to disagree — the front
door and the pipeline refuse the same paths because the same code decides.
Measured over the M62 grid across both refs, the cells whose blame moved are
exactly the unreadable ones, no call's fate changes, and no cell reports the
pipeline's retired wording at all.

**The rule.** The package tests a pipeline input for READABILITY, at one site,
and every front door and the pipeline builder reach it. `check_file_exists()`
remains for the two arguments that are not pipeline inputs — `verify_media()`'s
`file` and `write_mediainfo_template()`'s `templatefile` — and keeps existence
semantics for D040's own reason: neither has a downstream counterpart that
would refuse an unreadable file, so widening them here would be a front-door
abort refusing a call nothing else refuses, the shape D040 says needs its own
entry rather than a sweep.

**The wording moved, and had to.** "Does not exist" is false of a file that is
there and unopenable, so one message now covers both conditions: a single-file
argument renders `` `infile` can't be found or read: 'clip.mp4'. `` and a
carrier renders the count form. This is a user-visible change to thirteen verbs
that were already refusing missing inputs correctly, taken because the
alternative — branching the wording on which condition each path failed —
needs three renderings per arity and a fourth for a call carrying both, all at
the site whose single-wording property is the thing being protected.

**What this does not license.** Reading anything *about* an input but whether
it can be opened — its size, its container, its streams — is unchanged and
stays under D013/D034. A *readable* directory passes both the old predicate and
the new one (measured 2026-08-08), so nothing here starts refusing one; an
unreadable directory is refused, as any unreadable path now is, and its fate is
unchanged for the same reason every unreadable path's is — `ffm_files()` applied
this predicate already. Whether a directory in an input slot should be refused
*as a directory* is a separate question this entry does not answer.

- **Falsified by** a report of a call the front door refuses and the pipeline
  would have accepted, or the reverse — which under one predicate can only mean
  a caller reaching the pipeline by a path that skips the site — or by a report
  of the new wording sending someone looking for a permissions problem where
  the file was simply not there.

## D042 — A builder-bound value moves to the verb by re-calling the shared checker, never by threading `call` through an exported builder (2026-08-08, from M64; generalizes the shape M59-D1 chose for one verb and D037 licenses; extends D036's ordering to the crop/scale/rate sweeps)

A value that a Layer-2 verb hands to a validating `ffm_*` builder used to
abort naming the builder (scalar form) or `purrr::pmap()` (batch form) —
a function the caller never typed. Two fixes were available, and this entry
records why the package now has a rule rather than a per-verb choice.

**Rejected: giving the exported builders a `call` argument.** The builders
are Layer-1 API with direct callers of their own, for whom the builder IS the
right blame — that behavior is correct and keeps working. A `call` parameter
on an exported signature is API surface with an audience of one (the package
itself), it would have to land on every validating builder to close the class,
and M59-D1 already declined it for `ffm_crop()`. Threading `call` through an
INTERNAL `*_pipeline()` helper remains fine — `standardize_pipeline()`'s
pixel-format check does exactly that — because an internal signature is not
surface.

**Chosen: the verb calls the same shared checker at its own front door.**
`check_dim()`, `check_token()` and `resolve_sample_fps()` already exist apart
from the builders and write each message at one site, so the sweep adds no
second wording anything could drift from — measured by M64's baseline: across
30 cells at both refs, 27 messages are byte-identical and the 3 that differ
are the `format` → `pixel_format` argument-name correction (M64-D1), the same
sentence from the same site.

**The siting rule.** A sweep sits where the value was effectively read
before, so it changes blame and nothing else: at the end of the scalar
front door when the builder ran first in the pipeline, in the pipeline with
`call` threaded when the value is read mid-pipeline (`pixel_format`, which a
front-door check would have hoisted past both codec seams), and on a `_batch`
verb last among the value guards, immediately above `check_nvenc_available()`
— a machine-independent refusal reports before a machine-dependent one
(D036), which is the one reporting order M64 reassigns (M64-D2).

- **Falsified by** a shared checker whose abort cannot be aimed at a Layer-2
  caller from the verb's own frame, or a builder value whose refusal wording
  must diverge between the verb and the builder — either breaks the
  one-site-one-wording premise the sweep rests on.

## D043 — A cheap value refusal precedes the analysis probe (2026-08-08, from M65; extends D042's siting rule and D036's ordering to the probe-bearing path)

D042 sites a blame sweep where the value was effectively read before, so it
changes blame and nothing else. `normalize_audio(two_pass = TRUE)` is the case
where that rule conflicts with cost: the loudness targets were effectively
read only inside the analysis pipeline, AFTER `run_loudnorm_analysis()` had
spawned FFmpeg and measured the input — so the change-nothing-else placement
would preserve a wasted measurement per bad call (and per row on the batch
sibling's Phase 1).

**Chosen: the loudness sweep sits ABOVE the `two_pass` block, on both forms.**
A pure-R range comparison that refuses identically on every machine precedes a
probe that costs an FFmpeg execution. This is D036's
machine-independent-before-machine-dependent ordering read as cost: a free
refusal precedes an expensive probe, even where getting there moves the sweep
up the front door — the one deliberate exception to D042's siting rule in the
M64/M65 family. The reorderings it makes (10 scalar, 4 batch; every one a
shaping-knob complaint yielding to a target complaint) are M65-D1's table rows
9–22, each with a live control at both refs.

**Rejected: refusing only on the single-pass path and disclosing the two-pass
gap.** A disclosed ordering gap between two forms of the same verb is the
shape D038 recorded and D039 had to undo; the two-pass path — the one that
pays for the mistake — is exactly the path the refusal must reach first.

- **Falsified by** a builder-bound value whose validity is knowable only from
  the analysis result itself (its refusal then cannot precede the probe), or a
  measured caller for whom a shaping-knob complaint preceding a target
  complaint is load-bearing.

## D044 — A capability probe's answer is remembered for the R session, and the escape from it is exported (2026-08-09, from M67; narrows D034's per-build-frequency, leaves its licence untouched; trades GP1)

D034 licensed a probe whose result enters the compiled command to run while the
pipeline is built. It said nothing about how OFTEN, and the answer was: every
time. `has_nvenc()` reached `ffmpeg_encoders()` → `ffmpeg("-encoders")` on each
call, so an N-row `hardware = "nvenc"` batch spawned N FFmpeg processes to
re-learn a fact about the binary that cannot change while the binary does not.

**The rule.** The encoder-name pool is asked of FFmpeg at most once per R
session and remembered in a package-local environment (`R/cache.R`). The memo
sits strictly BELOW `has_nvenc()`'s `getOption("tidymedia.nvenc_encoders")`
seam: the option is read first on every call, so setting it mid-session takes
effect at once and never reads or populates the memo. `ffmpeg_encoders()` and
`ffmpeg_codecs()` stay uncached, so a caller always keeps a route to a fresh
answer.

**Lifetime, stated as a contract.** The memo lives for the R session. It is
discarded on exactly two routes: the exported `refresh_ffmpeg_capabilities()`,
and `set_program()`, which is the one package call that can repoint tidymedia at
a different binary. Nothing else discards it — not a new FFmpeg install, not a
driver change, not time. A caller who changes the machine under a running
session and uses neither route is pinned to the old answer, by design and by
documentation.

**Per-process, so `parallel = TRUE` workers each keep their own.**
`furrr::future_pmap()` runs `.f` in workers that `loadNamespace()` fresh, and
`future` exports the closure's globals rather than the package's internal
environment bindings, so a W-worker nvenc batch asks FFmpeg W times where the
sequential one asks once, and discarding in the parent does not reach them. W is
bounded by the worker count, not the row count, so this is a completeness gap
rather than a stall. Seeding workers was weighed and rejected: it means the
package *writing* `tidymedia.nvenc_encoders`, an option that is read-only from
the package's side today, which would change what that seam means. Disclosed in
the docs and carried as a ROADMAP candidate row.

**Why this does not trip D034's falsifier.** D034 is falsified by a dry run's
compiled command differing from what a subsequent `run = TRUE` call executes, or
by a third build-time probe its stated grep does not find. The memo changes how
often the probe runs, never what it answers within a session, so both calls
still compile the same encoder name; and it adds no execution seam, so the grep
finds exactly what it found before. D034's licence is untouched — the probe
still runs at construction, `run` notwithstanding. What is narrowed is only its
unstated frequency.

**The GP1 trade.** GP1 prefers refusing surface over growing it, and this adds a
permanent exported function under D014's clean-break policy. The trade is taken
because the alternative is worse than the surface: a session-scoped memo with no
user-facing escape pins a caller who installs FFmpeg or a GPU driver mid-session
to a stale answer, with no route back short of restarting R. The option seam is
not that route — it overrides the answer rather than refreshing it, and requires
the caller to already know the encoder names. The name is deliberately broader
than the memo it discards today (`refresh_ffmpeg_capabilities()`, not
`refresh_ffmpeg_encoders()`), so the already-planned `find_ffmpeg()` memo joins
it without a second permanent export.

- **Falsified by** a report of a stale pool surviving a mid-session FFmpeg
  change that neither `refresh_ffmpeg_capabilities()` nor `set_program()` was
  used for — which would mean the two discard routes do not cover how binaries
  actually change under a session — or by a measured parallel batch whose
  W-worker probe count is itself the reported problem, which would mean the
  per-process disclosure should have been a fix.

## D045 — Removing a failed run's output is not a probe; the executing path may delete what it wrote (2026-08-09, from M68; takes D040's filesystem premise and adds the write half; bounded by, not licensed by, D024)

`ffm_run()` now deletes the pipeline's output when FFmpeg exits non-zero, and
names it in the abort. Two questions had to be answered before it could.

**Is this a D024 probe?** No. D024 governs running a *binary* on a
`run = TRUE` path and confines one to a diagnostic-only effect. Nothing here
runs a binary: `ffm_run()` stats the output with `file.exists()` before the run
and `unlink()`s it after a non-zero exit. D040 already settled that reading the
filesystem is not a probe in D024's sense — a verb's front door stats its input
to refuse a missing one — and this entry takes that premise and adds the write
half.

**Then what bounds it?** Not D024's licence, which it would fail: removing a
file is observable and is not a diagnostic condition. What bounds it is *when*
it runs. It fires only after FFmpeg has already failed, so it changes no
compiled command, no resolved default, no choice of pipeline, and never whether
execution proceeds — the four things D024 puts outside its own licence. The call
aborts under every outcome; the removal decides only what is left on disk when
it does.

**Why at all.** FFmpeg creates its output before it knows the command will work
and truncates an existing one to zero on the way, so a failed run left a
zero-byte file looking like a result (measured 2026-08-09, ffmpeg 8.1.2 macOS:
an AAC-to-MP3 stream copy exits 234 with a zero-byte output, whatever the path
held beforehand).

**Scope.** `ffm_run()` alone, which every Layer 1 and Layer 2 execution path
reaches, `ffm_batch()` included. Two paths are not covered and cannot be: Layer
0's `ffmpeg()` runs a verbatim command string through `system()` and cannot tell
which token is an output; the two-pass loudnorm analysis calls `run_program()`
directly and writes to `-f null`, so it has no output to remove.

**The one exception** is `overwrite = FALSE` against a path that already
existed: FFmpeg was told not to replace that file, so the package does not
either. Narrowed to pre-existence deliberately — a non-overwriting run that
*created* its output still gets it cleaned up, so the exception protects a
caller's file without stranding a broken one.

Rules out a `cleanup =` argument or an option seam (there is one behavior, so
there is nothing to configure), and per-verb removal (IP1 keeps execution in
Layer 1 once). Falsified by a report of a caller who needed the failed output
kept: today's measured failures leave zero bytes, so there is nothing to
inspect, and a failure mode leaving a usefully partial file would reopen this.

## D046 — A failed run removes what it wrote, not what it found (2026-08-09, from M68's review return; supersedes D045's one-unconditional-rule half and its "one behavior" premise, keeps D045's not-a-probe reasoning intact)

D045 chose one unconditional removal on the premise that FFmpeg always truncates
a pre-existing output before failing, and the plan recorded its own falsifier: a
measured failure mode leaving a pre-existing output's bytes intact. That mode is
now measured, twice and independently. `ffmpeg -y -i in.mp4 -c:v nosuchcodec
out.mp4` exits 8 with a 13-byte pre-existing `out.mp4` byte-for-byte intact and
its mtime unmoved (2026-08-09, ffmpeg 8.1.2 macOS), because an unknown encoder —
like an unknown filter or a bad option value — is refused before the output is
opened. The unconditional rule deletes a caller's file that the run never
touched, and reports it as incomplete.

**The rule now.** `ffm_run()` stats the files its output designates before
running, and again after a non-zero exit, and removes only those this run
created or changed — by size or by modification time. The zero-byte truncation
D045 was written for is still removed; a file FFmpeg never opened is left
exactly as it was, and the abort says so.

**The degenerate case** is a pre-existing zero-byte output that FFmpeg opens and
leaves zero bytes: size cannot tell it apart, and mtime can — measured at
11:00:44.112779 before and 11:00:45.515060 after an AAC-to-MP3 copy into an
empty file (2026-08-09, same build). On a filesystem whose timestamp resolution
hides even that, the run leaves a zero-byte file that was already zero bytes.

**Frame sequences.** `sample_frames()`'s output is an image2 `%0Nd` pattern
rather than a path, so `file.exists()` was false of it and a failed sampling run
left every frame it had written. The rule applies set-wise: the files the
pattern matches in its own directory are snapshotted before the run, and those
the run created or changed are removed — an earlier run's frames survive.

**Deleting exactly the named path.** R's `unlink()` expands wildcards by
default: `unlink("a*.mp4")` emptied a directory of `aQQQ.mp4` and `aXYZ.mp4`,
and `unlink("out[1].mp4")` deleted `out1.mp4` and left `out[1].mp4` (measured at
M68's review). The removal passes `expand = FALSE`, so an output whose name
contains `*`, `?` or `[` costs no neighbour.

**`overwrite = FALSE` keeps the guard D045 gave it**, even though the general
rule now subsumes it: FFmpeg leaving the file alone is something the general
rule *observes*, while the guard is a promise the package *made*, and a build
that touched the file anyway must not cost the caller it.

Rejected: never removing a pre-existing output (strands the zero-byte
truncation, the common case this milestone exists for); keeping D045's
unconditional rule (refuted above). Falsified by a failure mode that writes a
usefully partial output a caller would want kept — today's failures leave zero
bytes or nothing at all.

## D047 — A wall-clock limit on every spawned program, off by default, carried by an option (2026-08-09, from M69; extends D044's seam precedent, bounded by D024, applies D046 unchanged)

The package's own runtime path had no timeout: every call blocked for as long
as FFmpeg, FFprobe or MediaInfo took, so a hung program blocked the R session
with it. M46 fixed this for the test suite only, deliberately.

- **The limit lives in an option, `tidymedia.timeout`, not in an argument.**
  Whole seconds; `0` means no limit. Read by `resolve_timeout()` and passed to
  base R's `timeout=` at all four spawn sites — `ffmpeg()`, `ffprobe()`,
  `mediainfo()` and `run_program()`, through which every task verb, `ffm_run()`,
  both loudnorm analysis passes and all metadata readers funnel. This is the
  package's second option seam, after `tidymedia.nvenc_encoders` (D044), and the
  first that changes what happens rather than what is reported.
- **Rejected: a `timeout =` argument on the run-capable verbs.** Sixty-odd new
  arguments is the largest irreversible-API commitment the package could make,
  and the seam forecloses none of it: the argument stays available under D014's
  pre-0.2.0 clean break, and is a ROADMAP candidate. The seam's grain is the
  session, which is the wrong grain for a script wanting different limits per
  call — that is this half's falsifier.
- **The default is `0`, no limit.** A ceiling would abort a legitimate
  multi-hour transcode that finishes today, changing the default behavior of
  every existing pipeline. Rejected accordingly; the cost is that a user who
  never reads the docs still hangs, which is the falsifier for this half.
- **Fractional values are refused, not rounded.** Measured on R 4.6.1: base R
  truncates `timeout=` toward zero, so a value below 1 becomes `0` — its own
  "no limit" sentinel — and a 6-second child ran to completion under a
  0.5-second limit. Rounding up would instead substitute a limit the caller
  never asked for. Nothing downstream catches a bad value either: `system2()`
  accepts `"2"` and `c(1, 2)` without complaint.
- **A timeout is identified by the `status` attribute being 124, never by
  matching R's timeout warning**, whose wording is translated under a
  non-English locale and which embeds the full command line and the `input=`
  temp path. The warning is held and dropped; the package's own message,
  naming only the program and the limit, replaces it. This is M46's lesson
  applied to the runtime path.
- **`limit > 0` is part of that test.** 124 is an ordinary exit status a
  program may return for its own reasons, so it means "killed by the limit"
  only when a limit was in force.
- **It aborts rather than warns, with class `tidymedia_timeout`.** A killed run
  leaves a truncated output that looks finished, and `ffm_batch()` records
  per-row errors, so a warning would make a timed-out row indistinguishable
  from a successful one in the results tibble.
- **D046 is applied unchanged on the timeout path.** `ffm_run()` catches the
  condition, calls `remove_failed_output()` with the same pre-run snapshot, and
  re-raises with the disposition appended, so which of D046's outcomes applied
  is stated here too. The rule itself is untouched: an output the killed run
  merely found still survives.
- **The readers absorb it exactly as they absorb any other error.**
  `count_audio_streams()` returns `NA` and a `probe_all()` row reads
  "unreadable", as both already do for every other failure. Making them
  re-raise would change `probe_all()`'s error contract, and D024 licenses the
  dropped-track probe only while its outcome changes nothing but whether a
  warning fires. The distinct class is what leaves that available later without
  re-deciding anything now.
- **Disclosed, not fixed: `parallel = TRUE` workers do not see the option.**
  Measured 2026-08-09 on future 1.70.0 — a `multisession` worker reading an
  option set to `42` in the parent got `UNSET` — so a parallel batch runs
  unbounded while the sequential one is bounded. Seeding workers means the
  package writing options into them, a seam question of its own; this takes
  D044's shape, which disclosed the same per-process gap for the capability
  memo. ROADMAP candidate.

Falsified by a report that the session-wide grain is itself the problem, or by
a hang from a caller who had read the docs and still expected a bound.

## D048 — What a reached timeout does to a reader is three rules, not one (2026-08-26, from M69's third review return, supersedes D047's readers bullet; the rest of D047 stands)

D047's readers bullet said the readers "absorb it exactly as they absorb any
other error", naming `count_audio_streams()` returning `NA` and a `probe_all()`
row reading "unreadable". That was true when it was written and false by the
end of the same milestone: T12 replaced it, and three review passes each found
one more call the uniform rule did not describe.

- **A timed-out probe is not an unreadable file, and `probe_one()` stops
  pretending it is.** It returns a classed sentinel carrying the program and
  the limit, not the bare `NULL` it returns for a corrupt file. The two
  outcomes were indistinguishable, which is how `ffm_run(verify = )` came to
  report a hung FFprobe as `width: expected 1920, got NA` — blaming a
  successful encode.
- **`probe_all()` still keeps the `NA` row and still warns once at the end**,
  so the documented return shape is unchanged and one hung file does not
  discard a corpus. What changed is that the warning counts timeouts apart from
  unreadable files and says so.
- **`verify_media()` re-raises rather than absorbing.** It asks whether a file
  HAS given properties, and a probe that never answered is not an answer of
  "no". It holds the probe's warnings and replays them only when it does not
  re-raise, so a caller is told once rather than twice.
- **Two paths absorb a timeout with no warning at all, and this milestone
  discloses them rather than fixing them.** `count_audio_streams()`
  (`R/ffprobe.R:199`), reached from `extract_audio()`, `convert_audio()`,
  `separate_audio_video()` and their `_batch` siblings, and `tool_versions()`
  (`R/ffm_manifest.R:127`), reached from `ffm_batch()`. Both return `NA` and
  say nothing, so a bounded hang on those calls is invisible. Fixing them is
  M70; disclosing them here is the honest reading, since a bounded silent hang
  is still strictly better than the unbounded one that preceded it.
- **Rejected: a fourth attempt at an exhaustive two-way partition of the
  package.** Each of the three returns beat a hand-written list with a member
  it omitted — and the third return's own finding named `remove_audio()`, a
  function this package does not export. A promise whose domain is fixed by
  what the author recalled is not repaired by recalling harder, so the docs now
  state the calls they name and say they are not a complete partition. M70
  replaces that with a promise bounded by a call-graph sweep over the package
  namespace.

Falsified by a caller reading the three-way description as exhaustive and being
surprised by a fourth behavior, which is the risk the "not a complete
partition" sentence is there to carry until M70 closes it.

## D049 — A reached limit is never silent, over a domain the package derives rather than recalls (2026-08-26, from M70, supersedes D048's fourth bullet and its fifth; the rest of D048 stands)

D048's fourth bullet disclosed two paths that absorbed a timeout with no warning
at all and named fixing them M70. Its fifth rejected a fourth hand-written
partition and promised M70 "a promise bounded by a call-graph sweep over the
package namespace." Both are discharged here.

- **The rule is uniform: every call that can start FFmpeg, FFprobe or MediaInfo
  either aborts or warns when the limit is reached.** Which of the two is
  unchanged from D047/D048 — an abort where the call's whole job is the run or
  the assertion, a warning where one hung file must not discard the rest of the
  work. What is new is that there is no third answer, so the docs state a rule
  instead of describing lists and then disclaiming them.

- **The domain is derived, not recalled.** A test closes the package's own call
  graph over `system()`/`system2()` and takes the exported functions that
  reach one — 53 today — then drives a forced timeout through each. M69 wrote
  that domain by hand three times and each review pass found one more member it
  omitted, the third naming `remove_audio()`, a function this package does not
  export. The sweep walks symbol MENTIONS rather than M62's call heads,
  because `probe_all_impl()` reaches FFprobe only through
  `purrr::map(infile, probe_one)`: a head-only walk drops the package's main
  metadata reader out of the domain entirely. The two guards err in opposite
  directions on purpose — a spurious member here costs one test cell, a missing
  one costs the promise.

- **The sweep found a third silent path M69's list never reached: the batch
  fan-out.** `ffm_batch()` recorded every job failure as `success = FALSE` and
  signalled nothing, so a bounded hang was invisible through it and through the
  15 `_batch` verbs and `segment_video()`. It now warns once per run. Only the
  limit speaks: every other failure keeps its silent `success = FALSE`, which is
  the contract each `_batch` verb's `@return` documents. That this path was
  found by the procedure and not by rereading the list is the evidence the
  procedure was the right instrument.

- **The warning grain is per CALL, not per file or per job.** `probe_all()`
  already warned once at the end; `count_audio_streams_all()` and
  `tool_versions()` now do the same, and `ffm_batch()` warns once for the whole
  run. Per-file would be worse than silence on a large jobs table: R collapses
  at "There were 50 or more warnings" and the message a caller needs is the one
  that gets swallowed (M44's gate, and the reason `warn_dropped_audio()` has
  been one-warning-whatever-the-length since it shipped).

- **A warning is inside D024's licence where a changed return is not.** D024
  licenses the dropped-track probe only while its outcome changes nothing
  observable but whether a diagnostic condition is signalled. A warning IS that
  diagnostic, so it is available; a changed count would be a second effect and
  would put the probe outside the licence by its own terms. Hence
  `count_audio_streams_all()` still returns `NA` for a killed probe — exactly
  what the silent version returned — and only the warning is new. The same
  reading governs the version probe: the manifest still records `NA`.

- **The absorbed-timeout sentinel stays off every public return.** It travelled
  out of `probe_all()` on a `tm_timed_out` attribute so `verify_media()` could
  read it, which broke `@param parallel`'s promise that both paths return
  identical output and would have survived every documented operation on the
  list. `probe_all()`'s body is now `probe_all_impl(absorb = )`, and
  `verify_media()` calls it with `absorb = FALSE` so the shared body re-raises.
  One assembly path, so the `file` column, the NA-row shape and
  `type_columns()` cannot drift between the two callers.

Falsified by a silent timeout on a call the sweep's domain excludes — a spawn
reached through a route no symbol-mention edge records (an `eval()` of a
constructed call, a function retrieved by `get()` from a string), or a
non-exported entry point a user reaches some other way. The sweep's seeds and
its recorded membership are both asserted, so the drift it cannot see is the
kind that never appears in a function body at all.

## D050 — A parallel worker runs under the settings the caller set, and hands them back (2026-08-27, from M071; supersedes D047's "Disclosed, not fixed" bullet and D044's seeding rejection; leaves the rest of both standing)

D047 disclosed that `parallel = TRUE` workers never see `tidymedia.timeout`, so
a parallel batch ran unbounded while the sequential one was bounded. D044 had
already disclosed the same shape for `tidymedia.nvenc_encoders` and rejected
seeding workers, on the ground that it would mean the package *writing* an
option it only ever reads. Both are now fixed, on one mechanism.

**The rule.** Every fan-out in the package captures, in the parent, the caller's
resolved `tidymedia.timeout` and their `tidymedia.nvenc_encoders`, installs both
inside the worker for the duration of the mapped call, and puts the worker's own
prior values back — on the returning path and on the erroring one alike. The
carrier is one internal wrapper (`R/timeout.R`), applied at the `furrr::future_*`
call and nowhere else; the sequential branches are untouched.

**Why this is not the seeding D044 rejected.** D044's objection was to the
package authoring a value for a seam it only reads. Nothing here originates with
the package: the value installed in the worker is the caller's own, and it is
withdrawn again when the call ends. A seam that is read-only from the package's
side stays read-only — what changed is *where* the caller's value is legible,
not who chooses it.

**An unset name is carried as unset.** The alternative — copy only what the
parent has set, leave the rest of the worker alone — makes clearing an option in
the parent stop meaning anything for the workers, and gives the same batch two
behaviors depending on where a row lands. One rule instead: for the duration of
the call the worker sees exactly what the parent sees. The cost is a caller who
configures workers separately through a `future` plan hook, whose value is
displaced for that call and returned afterwards; that is this half's falsifier.

**One value here is the package's, and it is the no-limit sentinel.** The limit
is carried *resolved* (see below), and `resolve_timeout()` answers `0` for an
unset option — so a parent with no limit installs `tidymedia.timeout = 0` in the
worker rather than leaving the name absent. The two seams are therefore
asymmetric: an unset encoder override is carried as genuinely unset, an unset
limit as the sentinel that means the same thing. For a worker with no limit of
its own the effect is identical; for one that had a limit set through a plan
hook it is the displacement the falsifier above already names, in its sharpest
form — that worker's limit is removed for the duration of the call, not merely
changed. Stated here rather than left to be inferred from "nothing here
originates with the package", which is true of every value the caller chose and
not of this one.

**The capability memo is still not carried, and stays disclosed.** D044's
per-process record of what the FFmpeg build reported is not an option and is not
shipped anywhere: a worker with no override still asks its own binary once.
Carrying a *memo* would mean the parent answering a question about a binary the
worker may not even be running. That gap keeps its ROADMAP candidate row.

**The limit is resolved in the parent, once.** Read at the spawn site it lands
below the per-job `tryCatch` that turns an error into a bare `success = FALSE`,
and is never read at all on a compile-only path — so a value base R would
mishandle was silent where it was worst. `ffm_batch()` therefore refuses it in
its validation block, before either branch dispatches, and the carrier refuses it
again at capture time for the fan-outs that have no validation block of their
own. One condition, in the process that can name the caller, on both branches.

Falsified by a report of a worker-side option write colliding with a caller's own
worker configuration, or by a caller who wanted the parallel path to diverge from
the sequential one and now cannot have it.

## D051 — A limit may be set for one call, by wrapping it (2026-08-27, from M072; supersedes D047's session-grain falsifier clause on its per-verb-argument bullet, leaving that bullet's rejection standing)

D047 put the wall-clock limit in an option and named what that cost: the seam's
grain is the session, which is the wrong grain for a script that wants a
different limit for a different call. That sentence was written as the falsifier
for rejecting `timeout =` arguments on the verbs. The grain is now available
without adding one.

**The rule.** `with_timeout(expr, seconds)` establishes `tidymedia.timeout` for
the dynamic extent of `expr` and puts the caller's prior state back on every
exit — the returning path, an ordinary abort and a reached limit alike, an unset
option restored as unset. Nothing is threaded and no signature changes: the
option is process-global, so every spawn site, `ffm_batch()`'s up-front refusal
and the parallel carrier (D050) read the per-call value exactly where they
already read the session's.

**Why a wrapper rather than the argument D047 rejected.** An argument can only
reach a function that has one, and most of the exports a timeout can be seen
through take no `run =` at all — `ffm_run()`, the Layer 0 hatches, the probe and
MediaInfo readers, `verify_media()`. A wrapper reaches every one of them, and
costs one irreversible export against a signature change on each verb. The
counts are in `cairn/milestones/M072-per-call-timeout.md`.

**The rejection of per-verb `timeout =` arguments stands.** Only D047's
falsifier clause is discharged: the grain that clause said was missing is here,
so the clause no longer names a way the rejection could be wrong. The argument
itself stays a ROADMAP candidate under D014's pre-0.2.0 clean break, promotable
on its own evidence — a caller needing a limit that varies per row inside one
batch, which a wrapper around the batch cannot express.

**The name is outside D014's families, deliberately.** `with_timeout` is
neither a task verb (`verb_object`), an `ffm_*` builder, nor a
`get_*`/`probe_*`/`mediainfo_*` reader, and `expr`/`seconds` are in none of
D014's argument vocabulary. That is the point: this is a control-flow wrapper,
not a media operation, and the name it wants is the one R users already have
for the shape — `withr`'s `with_*(...)` family and `R.utils::withTimeout()`.
Coining a tidymedia-shaped name for it would hide the idiom rather than record
it. D014 governs the media surface; this sits beside it.

**The expression comes first, the limit second.** `with_timeout(expr, seconds)`
rather than withr's value-first `with_*(new, code)` order, matching
`R.utils::withTimeout()`, the function an R user reaching for this already
knows. Chosen at the implementation gate; it is the half of this decision that
cannot be revisited without breaking callers.

**`seconds` is refused by the same rule the option is.** The wrapper applies
`resolve_timeout()`'s own check, so a value base R would mishandle is refused
identically whether it was written as an argument or set as an option — and it
is refused before `expr` is evaluated, so a caller who mistyped a limit does not
watch the call run unbounded. The message names `seconds`, since that is what
they wrote.

Falsified by a report that wrapping an expression is the wrong shape for a
script — that the natural place to say "bound the rest of this function" is a
statement, not a wrapper — or by a caller needing a limit that varies per row
within one batch.

## D052 — The limit may also be set as a statement, and `withr` becomes a hard dependency (2026-08-27, from M073; extends D051, leaving all of it standing)

D051 named its own falsifier: a report that wrapping an expression is the wrong
shape for a script — that the natural place to say "bound the rest of this
function" is a statement, not a wrapper. The statement form now exists, so that
clause no longer names a way D051 could be wrong. Nothing in D051 is superseded:
`with_timeout()` keeps its argument order, its refusal rule and its reach.

**The rule.** `local_timeout(seconds, .local_envir = parent.frame())`
establishes `tidymedia.timeout` from the call to the end of the frame it is
bound to, and puts the caller's prior state back on every exit — the returning
path, an abort and a reached limit alike, an unset option restored as unset. It
is the `local_*` half of the pair whose `with_*` half D051 shipped: one seam
written twice, not two seams. Nothing is threaded and no signature changes, for
D051's reason unchanged — the option is process-global, so every spawn site,
`ffm_batch()`'s up-front refusal and the parallel carrier (D050) read it exactly
where they already read the session's.

**Shipped ahead of its own trigger, on the user's call.** The candidate row's
stated trigger was a report that the wrapper is the wrong shape, and no such
report arrived. The trade taken is D014's pre-0.2.0 clean break, which keeps the
export withdrawable. Falsified by 0.2.0 arriving with no caller, at which point
it becomes permanent unused surface.

**The name sits outside D014's families, for D051's reason unchanged.**
`local_timeout` is neither a task verb, an `ffm_*` builder, nor a
`get_*`/`probe_*`/`mediainfo_*` reader, and it takes none of D014's argument
vocabulary. It is a control-flow statement, and the name it wants is the one R
users already have for the shape — withr's `local_*()` family, whose
`.local_envir` spelling it also takes, dot-prefixed so it cannot collide with a
caller's own argument. D014 governs the media surface; this sits beside it, as
`with_timeout()` does.

**The `NULL` asymmetry with the option seam is stated, not removed.**
`options(tidymedia.timeout = NULL)` removes the name, leaving the session unset
and unlimited; `local_timeout(NULL)` is a caller naming no limit at all, and is
refused. Both wrappers behave this way and both say so in their documentation.
`local_timeout(0)` is how a caller lifts a limit.

**`withr` moves from Suggests to Imports.** The undo has to run
last-in-first-out: two `local_timeout()` calls in one frame must restore to the
CALLER's state, not to the first call's, and `withr::defer()` prepends its
handler (`after = FALSE`) where a plain `on.exit(add = TRUE)` appends. It also
handles a global or knitr target environment, which a hand-rolled call does not.

It does NOT make the restore unclobberable, and an earlier draft of this entry
said it did. `withr::defer()` ends in
`do.call(base::on.exit, list(thunk, TRUE, after), envir = envir)`, so a calling
frame that writes `on.exit()` without `add = TRUE` discards the restore silently
— measured on withr 3.0.3, the option left at the wrapper's value where the
caller had `99`. `withr::local_options()` loses it identically. That hole is
stated in `local_timeout()`'s documentation rather than papered over.

The cost is one more package on every install, against a package already
required to run the tests and whose own Imports are `graphics` and `grDevices`.
Falsified by `withr` acquiring dependencies of its own that a media-tools
package should not carry.

## D053 — The `withr` floor stays 2.5.0, and now says so because it was measured (2026-08-27, from M074; extends D052's dependency bullet, leaving all of D052 standing)

DESCRIPTION declared `withr (>= 2.5.0)` while every claim `local_timeout()`
makes had only ever been measured on 3.0.3, so the floor stated a version
nobody had run. It is measured now, and it stays.

**What was measured.** `data-raw/withr-floor.R` installs a given `withr` from
CRAN into its own library — it tries the Archive URL first for every version and
falls back to the current `src/contrib` directory, and records nothing about
which of the two answered, so where 3.0.3 was fetched from is not in the log —
then
runs, in a fresh `Rscript` session with that library first, the two
timeout-wrapper test files, the two top-level forms, where each form's undo
actually registered, `source(local = TRUE)`, the four documented claims about
`local_timeout()`, and the `withr::` calls the documentation compares it to.
Each child session asserts that the `withr` it loaded came from the library it
was handed, not merely that the version string matches: the user library holds
the current release, so a version check alone could not catch a failed install
of the 3.0.3 arm. A failing `test_that()` block stops the run. On
2.5.0 and on 3.0.3: all 35 `test_that()` blocks of `test-local-timeout.R` and
`test-with-timeout.R` pass, 0 failures and 0 skips on either; the four
documented claims read exactly as written on both; `withr::defer()` and
`withr::local_options()` lose their undo the same two ways on both, and
`withr::with_options()` + `withr::local_options()` nest the way
`with_timeout()` + `local_timeout()` do on both; and the two top-level forms
agree on both — the `Rscript` form leaves the limit set at `.Last` and at a
finalizer registered after withr's own, the `source()` form has the caller's
value back when `source()` returns.

**The mechanism changed; what these forms observe did not.** withr 3.0.0
rewrote `defer()`'s `globalenv()` branch. `local_timeout()` hands that branch
`globalenv()` from both top-level forms — `parent.frame()`, the default
`.local_envir`, is `globalenv()` at the top level of an `Rscript` file and of a
`source()`d file alike, measured `TRUE` on both versions — but only the
`Rscript` form's undo ends up there. `withr::deferred_run(globalenv())` restores
the caller's value at an `Rscript` top level and finds nothing to run inside a
`source()`d file, on 2.5.0 and on 3.0.3 both: withr redirects the handler to
`source()`'s own frame first, and both versions do it, by different routes
(3.0.3 consults `source_exit_frame_option()` before reaching `global_defer()`;
2.5.0 runs `exit_frame()`/`source_frame()` before `setup_handlers()` is reached
at all). That last clause is read from withr's own sources, not from the
harness, which reads no version's internals and measures only the outcome, 30
against 99. The reading was made at M074's review round 2 and is recorded in
that section of the milestone file, three ways: `deparse(withr::defer)` on
3.0.3; the 2.5.0 tarball from the CRAN archive (`compat-defer.R:35-49` and
`:172-180`); and `length(withr:::the$global_exits)` on 3.0.3, which is `0`
inside and after a `source()`d file and `1` after the same call at an `Rscript`
top level. So the rewritten branch is reached from one of the two forms rather
than both, and the `source()` form's agreement across versions is caused by a
redirect both versions have — not by the rewritten branch behaving the same.
Only `global_defer()` is new in 3.x; `is_top_level_global_env()` is already in
2.5.0 (`compat-defer.R:174`, called at `:65`). D052's reason for choosing
`defer()` over `on.exit()` — that it also handles a global or knitr target
environment — therefore stands unqualified.

Nor is the `Rscript` form's outcome the absence of an undo. Both versions
schedule one — `reg.finalizer(globalenv(), function(env) deferred_run(env),
onexit = TRUE)`, from 2.5.0's `setup_handlers()` and 3.0.3's `global_defer()` —
and `withr::deferred_run(globalenv())` puts the caller's value back on both.
What `.Last` and a later finalizer report is that they run BEFORE withr's
finalizer. That is hook ordering, and it is not this package's to promise: "the
limit is still set when the script's own exit hooks look" is what was measured,
"there is nothing left to unwind" is not.

**The one difference found.** `source(file, local = TRUE)` from inside a
function frame — the form withr 3.0.0 made need
`options(withr.hook_source = TRUE)`, and which 2.5.0 redirected by default —
differs: the line after `local_timeout(30)` still reads the limit on 2.5.0 and
already reads the caller's value on 3.0.3. That line is the harness's only
observation point inside the sourced file, so what is measured is the direction
of the split, not how long 2.5.0 holds on. Both have the caller's value back
once the enclosing frame returns, and no `@details` claim is about this form, so
it does not move the floor — though `?local_timeout`'s description, "for the
remainder of the function you call this from", sits in tension with the 3.0.3
reading, and that is a reading of the page rather than a measurement. It is recorded because it is the one place the two versions were
seen to part.

**What was not measured.** The nine other `Imports` floors and every `withr`
between 2.5.0 and 3.0.3 — the walk
was to be run only if a block failed, and none did. The absent
`Depends: R (>= )` line was also unmeasured here; **M076 closed that** on
2026-08-27, declaring `R (>= 4.1.0)` as the measured maximum of the shipped
surface's R-version-gated syntax and the ten `Imports` floor versions' own
`Depends: R` fields, and checking the package at exactly that version in CI. The `knitr` target
environment. Neither of the two `@details` claims that are not about
frames was run under the floor as the page states it. The `parallel = TRUE`
fan-out is mentioned in neither measured file. The per-spawned-program claim is
stated of a `local_timeout()` above a batch, and no test writes that:
`grep -n local_timeout tests/testthat/test-with-timeout.R` returns one hit, a
comment. What DID run on the floor is the same per-spawn machinery driven
through `with_timeout()` — `test-with-timeout.R:255` (each spawn site is handed
the per-call limit), `:279` (`ffm_batch()`'s up-front limit check reads the
per-call value) and `:487` (a per-call limit kills a hung program) — all passing
under the pinned 2.5.0 library with 0 skips. Two earlier revisions of this
paragraph were wrong here in opposite directions: the first said the claim was
unrun and located its tests outside the two measured files; the second called it
run and counted `:432` among its tests, which is a process-lifetime block
asserting nothing about a limit. Test-side `withr` use is Suggests-side and says nothing about what a
user installing tidymedia gets. Nothing verifies this floor on a schedule: CI
installs the latest dependencies on all five jobs.

Falsified by a caller on 2.5.x observing a `local_timeout()` behavior these
forms do not reach — `knitr` is the untested one — or by `withr` 2.5.0 failing
to install on a supported R.

## D054 — The discarded VIDEO stays silent, on all six audio-producing verbs (2026-08-27, from M075; extends D024's diagnostic-probe licence and D030's audio-only contract, leaving both standing)

`normalize_audio()` and `normalize_audio_batch()` now warn about the audio
tracks their output does not receive, the way `extract_audio()` and
`convert_audio()` and their `_batch` siblings already did. All six also discard
the input's **video**, and about that they stay silent. That silence is the
rule, not an omission M075 ran out of room for.

**Why.** The parity M075 restores is with two verbs that discard video silently
themselves, so a video signal here would create a new divergence in the act of
closing an old one — and it would fire on the common, correct case: an
audio-producing verb given a video file is doing exactly what it says. D030
already states the discard in the first sentence of `?normalize_audio` ("The
output holds **one audio stream and no video**"), which is the disclosure
channel this package uses for a contract, where the warning channel is for the
case where the caller had a choice and did not know they were making it. A
caller who names no `audio_stream` had that choice; a caller who calls an
audio-producing verb did not.

**What this rules out.** A second condition class for the dropped picture; a
`video` field on `tidymedia_dropped_audio`; and adding the video note to the
existing warning's bullets, which would put text about a contract into a
message the caller can suppress by naming a track.

**Falsified by** a report of a caller surprised by the lost picture — that is
the observation that would show the docs channel is not reaching them, and it
would apply to all six verbs at once, not to the normalize pair alone.

## D055 — The nine `Imports` floors say what was measured, and one of them was wrong (2026-08-28, from M077; extends D053's "what was not measured" clause, leaving all of D053 standing)

D053 closed the `withr` floor and named the nine others as unmeasured. They are
measured now. Eight stand at the version they declared. One did not work at all.

**`rlang (>= 1.1.0)` was a floor the package could not run on, and is now
1.2.0.** `R/` calls `rlang::check_string()` 46 times, `check_bool()` 36,
`check_number_whole()` 38 and `check_number_decimal()` 12 — the front-door
checks every exported verb runs before it builds a command. rlang exports all
four for the first time in **1.2.0**: a NAMESPACE walk over 1.0.0, 1.0.1, 1.0.2,
1.0.3, 1.0.4, 1.0.5, 1.0.6, 1.1.0 through 1.1.7 and 1.2.0 — every release the
Archive holds between 1.0.0 and 1.2.0 — finds none of them exported before that
release. Pinned at the declared 1.1.0 — directly, ahead of the environment
reconciliation the harness now runs first, which would itself have raised 1.1.0
to the 1.1.7 `vctrs` requires, a version equally short of all four exports —
1528 tests failed, each reading `'check_string' is not an exported object from
'namespace:rlang'`. A user who
resolved the floor got a package whose every verb aborted on its own first
line. Nothing caught it because nothing had ever run it: CI installs the latest
dependencies on all five jobs. The same direction is forced independently by
the environment — current `vctrs` requires `rlang (>= 1.1.7)`.

**What was run against every floor.** `data-raw/imports-floors.R` installs each
versioned `Imports` entry at the version DESCRIPTION declares into one library,
ordered by the `LinkingTo`/`Imports` edges among the pinned set itself so
`archive` and `purrr` compile against the pinned `cli` headers, and runs the
package's `testthat` suite in a fresh `Rscript` whose first `.libPaths()` entry
is that library. Per pinned package the child asserts both the version resolved
and the DIRECTORY it resolved from, before anything loads and again for every
pinned namespace loaded after the suite — the user library holds current
releases, so a version check alone cannot catch a failed pin. Both binaries are
asserted on `PATH` first, because most execution tests `skip_if` they are
absent and "0 failures" is also true of a run where every one of them skipped.

Runner: `rocker/r-ver:4.4.3` — R 4.4.3, Ubuntu noble, aarch64, `ffmpeg` 6.1.1 —
under colima on macOS 26.5. One compiler flag was changed for the measurement
and nothing else: Debian and Ubuntu build R with `-Werror=format-security`, and
`rlang` 1.1.0 and `archive` 1.1.1 both call `Rf_error()` with a non-literal
format, so on this runner those two floors are a compile ERROR rather than a
warning. The harness appends `-Wno-error=format-security` so it measures the
floor rather than the distribution's hardening policy. The consequence for a
user is real and is not measured away: compiling those versions from source on
a distro that hardens this way does hit those errors.

Result, with `archive` 1.1.1, `cli` 3.4.0, `dplyr` 1.1.0, `glue` 1.6.2, `purrr` 1.0.0, `rappdirs` 0.3.3, `rlang` 1.2.0, `tibble`
3.1.4 and `withr` 2.5.0 all resolving from the pinned library: **6120 passing,
0 failing, 22 skipped over 66 files**, identical file for file to the same
suite's run on current dependencies in the same container.

**Why not the host's R.** R 4.5 hid `Rf_findVar` and `ATTRIB` behind
`ENABLE_LEGACY_NONAPI_FUNS` and dropped `SET_FORMALS`, `SET_CLOENV` and
`PRVALUE` outright, so on the host's R 4.6.1 six of the nine floors do not
compile at all and `archive` finds no `libarchive`. Walking each forward
reached today's release every time. Those failures say what a 2026 toolchain
will build, not what these floors do, and moving six floors to 2026 releases on
that evidence would have raised what users must install for a reason no user
has. The floors were measured on the newest R that still declares what they
call, which is inside the package's own `R (>= 4.1.0)`.

**These four things were not measured.**

1. *The pinned set is the direct `Imports` only.* Every sibling and transitive
   dependency sat at its current CRAN version, so a joint pass says the
   declared floors work together against current everything-else, and nothing
   more.
2. *Two packages were held back, and neither is a floor.* Current `testthat`
   requires `cli (>= 3.6.5)` and `withr (>= 3.0.2)`, and current `furrr`
   requires `purrr (>= 1.2.1)`; R enforces those at load time, so with the
   floors pinned neither would load. `testthat` was held at 3.1.10 and `furrr`
   at 0.3.1 — the newest releases the floors permit — rather than moving three
   runtime floors to satisfy the test harness. `withr` 2.5.0, which D053
   measured, is one of the three that would have moved.
3. *Three test files did not run, in either the baseline or the pinned run.*
   `test-with-timeout.R`, `test-runtime-timeout.R` and `test-timeout-silence.R`
   block a spawned program on a named pipe and expect the package's own limit
   to kill it. On this runner it does not: a blocked `ffmpeg` survives
   `SIGTERM` and dies only on `SIGKILL`, and `system2(stdout = TRUE, input = ,
   timeout = )` — the call `R/program_management.R:125` makes — did not
   escalate; one isolated run took 191.8 s against a 2 s limit and six
   full-suite runs never returned. The baseline wedges identically, so no floor
   is implicated, but the consequence stands: nothing the timeout surface does
   was exercised under the pinned floors. That behaviour is a ROADMAP candidate
   row of its own, not a finding about a floor.
4. *No floor was run alone, and the run was on one operating system.* A joint
   pass attributes nothing to any single floor; the harness's `--only` mode is
   the attribution tool and was not needed, because the one failure named its
   own package. macOS and Windows were not run at all.

Falsified by a user on a declared floor hitting a failure this joint run does
not reach — a floor that works alongside its eight siblings and breaks against
current ones is the shape this configuration cannot see — or by any of the
eight standing floors failing once a sibling moves.

## D056 — The limit bounds the wait, measured: limit + 40 s with a pipe, limit + 20 s and a surviving program without (2026-08-28, from M078; measures what D047 asserted and D055 item 3 reported second-hand, leaving both standing)

`options(tidymedia.timeout = )` was documented as bounding the wait rather than
the program, with a lag of "up to 40 seconds" carried in one topic only. That
number was inherited from `?system`'s contract and one 2026-08-09 CI
observation, never measured here. D055 item 3 then reported, as an aside to a
floors measurement, that a 2 s limit had produced a 191.8 s run and that six
full-suite runs never returned. This entry measures both.

**Runner.** `tidymedia-floors:r443` — Ubuntu noble, aarch64, R 4.4.3, ffmpeg
6.1.1-3ubuntu5 — rebuilt from `data-raw/Dockerfile.floors`, committed by this
milestone so the runner D055 names can be rebuilt. Measured by
`data-raw/timeout-bound.R`, which prints the numbers quoted below. Host figures
are context, not a second platform under test: macOS 26.6.2, aarch64,
R 4.6.1, ffmpeg 9.0.1.

**Set limit, 2 s in every case. Observed elapsed, and whether the spawned
program was still running when R returned.**

| case | spawn form | container | host |
|---|---|---|---|
| A1 | signal-ignoring child, `system2(stdout = TRUE)` | 42.00 s, dead | 42.03 s, dead |
| A2 | signal-ignoring child, `system2(stdout = "")` | 22.01 s, **alive** | 22.02 s, **alive** |
| A3 | as A1, `input = ""` | 42.02 s, dead | 42.03 s, dead |
| A4 | signal-ignoring child, `system(intern = TRUE)` — the Layer 0 call | 42.02 s, dead | 42.01 s, dead |
| B1 | FFmpeg blocked on a writer-less FIFO, `system2(stdout = TRUE)` | 42.03 s, dead | 2.01 s, dead |
| B2 | as B1, `system2(stdout = "")` | 22.01 s, **alive** | 2.01 s, dead |
| C1 | `with_timeout(ffmpeg(<blocked FIFO>), 2)` | 42.41 s, dead | 2.37 s, dead |

**What the numbers say.** R's escalation is SIGINT at the limit, SIGTERM at
+20 s, SIGKILL at +40 s. When R reads the child's stdout PIPE it stays until
the pipe closes, which is when SIGKILL lands: limit + 40. When it does not read
a pipe it stops waiting at the SIGTERM step, limit + 20 — **and the program is
still running**, verified by pid and command line, with a control that spawns a
known-live process, asserts the probe finds it, kills it and asserts the probe
clears. The package only ever uses the reading form (`run_program()` passes
`stdout = TRUE`; the Layer 0 hatches pass `intern = TRUE`), so no tidymedia call
takes the surviving-program path today. The macOS/Linux split at B1 is the
FFmpeg build, not R: 9.0.1 answers the first signal, 6.1.1 blocked on a FIFO
does not.

**M69 return 2's premise is confirmed, not falsified.** That gate rejected
`processx` because the Goal — a hung program stops the call rather than the
session — "is met by a bounded 42 s exactly as by a bounded 2 s". The largest
overrun in 14 cases across two platforms is 40.41 s, and every overrunning case
lands on limit + 40 or limit + 20. The ROADMAP row that carried this premise as
contradicted said so on the strength of D055's 191.8 s; it now carries this
measurement instead. Nothing here decides a replacement mechanism, and the
rejection is untouched — the milestone that swaps the mechanism is the one with
standing to overturn it.

**D055 item 3 does not reproduce in a rebuild of the runner it names.** Its
191.8 s under a 2 s limit was not observed in any case. Its three excluded
fixture files — `test-with-timeout.R`, `test-runtime-timeout.R` and
`test-timeout-silence.R`, which block a spawned FFmpeg on a `mkfifo` named pipe
nobody writes to (`local_blocking_input()` in `helper-timeout-sweep.R`) and
expect the package's limit to stop it — each ran to completion with
`NOT_CRAN=true`: 45.61 s, 267.30 s and 52.11 s, exit 0, no failures and no
skips. The full suite ran **pass=6477 fail=0 skip=22 in 445.9 s**. 267 s is
about six bounded calls at ~42 s each, which is the shape a "191.8 s isolated
run" most plausibly had: a file's duration, not a call's.

This is recorded as **unreproduced, not disproven**, and D055 stands unamended.
That run had the nine `Imports` floors pinned and `testthat` and `furrr` held
back, a configuration this measurement did not reproduce; D055 also reports its
baseline wedging identically, which this run — effectively that baseline — did
not. What can be said is that a rebuild of the named runner, on current
harness packages, does not wedge.

Falsified by a spawn returning materially later than limit + 40 s on any
platform -- a different escalation, not the sub-second package frame around the
wait that puts C1 at 42.41 s -- by a tidymedia call returning with its spawned
program still alive, or by the wedge reproducing on the pinned-floor
configuration D055 measured.

## D057 — A derived output's duplicate-input refusal reports after the path sweep (2026-08-28, from M080; narrows D040's ordering paragraph, leaving the rest of D040 and all of D041 standing)

D040 put the input sweep above the M58 contradiction sweep, and said why:

> A missing input does not vary that way. It varies with the caller's own
> data, which the caller has and the report names — `` `jobs$input` names 1
> file that does not exist: 'clip3.mp4' `` is fully actionable by the person
> who typed the path.

That paragraph settled the sweep's position against the guards D040's own grid
reached. It did not reach one, because every cell in that grid supplied an
explicit `output`: the refusal three verbs make when the caller supplies no
`output` column and the verb has to derive one name per input. Two rows naming
the same input would derive the same output, so those verbs refuse the
duplication — and they refused it ABOVE the sweep, so a `jobs` table whose rows
all carried one mistyped path was told its inputs were duplicated and never
told which file was not there. That message names no path the caller can act
on, and D040's own argument applies to it unchanged.

**The rule.** A verb that derives its outputs refuses duplicated inputs BELOW
the input sweep, never above it. The refusal is written at one site,
`reject_duplicate_inputs()`, so a verb that derives outputs later inherits the
order rather than restating it — the same reason D040 gave for one abort site.

**What does not move, and why.** `reject_duplicate_outputs()` stays where it
is. It runs on outputs already derived or supplied, where two rows really do
collide on a destination and the collision is the message the caller needs. It
would take a different argument to move, and D040's paragraph does not reach it
either.

- **Falsified by** a report preferring the duplication on a table that is both
  wrong about a path and duplicated, or by a report of an explicit-output table
  whose output collision hid a missing path — the case this entry leaves alone.

## D058 — The input sweep is never lifted past a check that already sat above it (2026-08-28, from M080; narrows D057 by fixing the sweep's upper bound, leaving D040 and D041 standing; the rule below was narrowed on 2026-08-28 after M080's second review found its first wording claimed an invariant the package does not have, and again after its third review found the narrowed wording forbade the one move D057 licenses)

D040 fixed what the input sweep reports BEFORE, and D057 added the
derived-output duplication refusal to that list. Neither fixed what the sweep
reports AFTER, and M080's first attempt at D057 read the gap the permissive
way: it lifted `check_batch_inputs()` to sit directly above the derived-output
block, which on two verbs sat above their remaining front-door checks, so the
sweep passed those too and a wrong column type or a bad scalar argument began
reporting after the missing path. M080's review returned the milestone on it;
the calls and messages are recorded in that milestone file's Review section.

**The rule.** The input sweep is never lifted past a check that already sat
above it, with one carve-out: the derived-output duplication refusal, the one
guard D057 deliberately puts below the sweep. Where the sweep must move down to
reach some OTHER check, the sweep and that check move down together as a unit,
so no check that reported before the sweep begins reporting after it.

The carve-out is not a loophole, it is D057 restated: `normalize_audio_batch()`
already had every other front-door check above its sweep on `origin/master`,
with only the derived-output block above it, so M080 lifted the sweep past that
block and nothing else. Without the carve-out this rule forbids the very move
D057 exists to license, and `normalize_audio_batch(tibble(input =
c("gone.mp4", "gone.mp4")), run = FALSE)` — which reports the missing path here
and reported the duplication on `master` — fires the falsifier below (M080
review round 3, N1).

This fixes the sweep's position RELATIVE to the checks above it. It does not
say every check a verb makes on its own arguments is one of them, and the
package is not shaped that way, and it splits BOTH ways on the same verb.
Measured on 2026-08-28 by reading each sweep verb's body around its
`check_batch_inputs()` call and measuring each cell against a readable-path
control: `picture_in_picture_batch()` checks its codec tokens above the sweep
but `margin` and `position` below it, and SPLITS `scale` across the sweep —
`scale = "x"` is refused above it by the type check, `scale = 5` below it by
`check_overlay_scale()`'s range check, so the grain is per CHECK and not even
per argument (M080 review round 3, N2);
`compare_videos_batch()` checks `resize` and its codec tokens above but
`direction` below; `standardize_video_batch()` checks `video_codec` and
`audio_stream` above but `width` below; `normalize_audio_batch()` checks
`two_pass` above but `target_loudness` below. What sits above the sweep is per
CHECK — not per verb, not per category, and not even reliably per argument —
and NEWS.md accordingly promises no ordering for a verb's own arguments.

What IS uniform, and what the first sentence of NEWS's ordering paragraph
rests on: no shape guard (`check_batch_jobs()`, `check_fanin_jobs()`) and no
column-TYPE guard (`check_batch_audio_col()`, `check_batch_codec_col()`,
`check_batch_string_col()`) sits below the sweep in any verb — checked over
every export reaching `check_batch_inputs()` on 2026-08-28. Named individually
rather than as `check_batch_*_col()`, because that glob also catches
`check_batch_vocab_col()`, a column VALUE guard, which DOES sit below the sweep
in `picture_in_picture_batch()` and `compare_videos_batch()` — the same
type-versus-value split those verbs draw for their scalar arguments (M080
review round 3, N4).

**The consequence, taken deliberately.** On the two verbs carrying both, the
duplication refusal now sits below the scalar checks as well as below the
sweep, so a duplicated table that also carries a bad codec argument reports the
argument. That inverts a precedence M41 pinned (PR #43, commit `0a73edb8`; the
entry first credited M42, which is the codec `NULL`/column-`NA` semantics
milestone — M080 review round 3, N3); the pin moved to the test file
that owns the sweep's order rather than being deleted, because the order it
states is still a promise, just the other way up.

- **Falsified by** a report preferring the missing path over a check, OTHER
  than the derived-output duplication refusal the rule carves out, that sat
  above the sweep before the move that displaced it; or by a verb whose check
  cannot be stated above the sweep without reading a column the sweep has not
  validated. NOT falsified by a report preferring the missing path over a check
  that was always below the sweep — that is the shape the first wording of this
  rule wrongly forbade — and NOT by one preferring it over the carved-out
  duplication refusal, which is D057's whole point.
