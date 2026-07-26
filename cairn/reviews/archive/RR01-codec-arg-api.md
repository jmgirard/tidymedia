# RR01: `video_codec` API shape for the four codec-less re-encode verbs (M34)

- **Date:** 2026-07-26
- **Brief:** `cairn/reviews/RB01-codec-arg-api.md`
- **Reviewer:** independent Fable-level review (clean session; read only the
  materials the brief directed to)

## Q1 — Codec exposure: Option B, for all four verbs

**Verdict: Option B (user-facing `video_codec`) on all four verbs, with the
sentinel default from Q2. No split.**

Reasoning:

1. **Option A is a strict subset of Option B under the sentinel design.** The
   correct hardware-only behavior for these verbs (no `-c:v` at
   `hardware = "none"`, an nvenc encoder at `hardware = "nvenc"`) is exactly
   what Option B produces when the user leaves `video_codec` at its default.
   The marginal cost of Option B is one formal argument per verb; the marginal
   machinery is zero — `resolve_hw_encoder()` (R/ffmpeg.R:1432–1459) and
   `ffm_codec()` (R/ffm.R:509–527) already exist and are the ratified seam.

2. **The `format_for_web` precedent does not transfer.** `format_for_web` hides
   the codec because the verb *is* a fixed recipe — "H.264 + yuv420p + AAC +
   faststart, no per-row knobs" is its documented identity
   (R/ffmpeg.R:464–478). The four M34 verbs are configurable transforms
   (crop rectangle, cut points, stack direction, inset position); none has a
   fixed-recipe identity that a codec knob would dilute. The teachable boundary
   is: *fixed recipes hide the codec; configurable transforms expose it.*
   `standardize_video` (R/ffmpeg.R:650–654) and `anonymize_video`
   (R/ffmpeg.R:759–762) already sit on the "expose" side.

3. **Real user value, not coverage-chasing.** For a research pipeline the codec
   is part of the reproducibility contract: today these four verbs produce
   whatever the container default is *for the local FFmpeg build*, which is
   precisely the kind of environment-dependence the package exists to remove.
   Letting a lab pin `video_codec = "libx264"` (or choose libx265/AV1 for
   archival size) on a crop or segment is core D001 territory. It is also a
   functional prerequisite for using nvenc sensibly on non-H.264 targets
   (`video_codec = "libaom-av1"` + `hardware = "nvenc"` → `av1_nvenc`).

4. **GP1 is not offended.** No new engine capability (IP2 holds: only
   `ffm_codec(video=)` is threaded), one argument with D014's ratified name,
   reusing D-M31 machinery. GP1 guards against *coverage growth*; a knob that
   two sibling verbs already have and that D014 canonized is consolidation,
   not growth. A split verdict (B for crop/segment, A for the composites)
   would save nothing and create a second API rule users must memorize.

Caveat carried into Q5: on `segment_video`, `video_codec`/`hardware` are only
meaningful on the `reencode = TRUE` path; the `reencode = FALSE` combination
must abort, not silently ignore.

## Q2 — Default value: sentinel `NULL`; the container trap is real

**Verdict: option (b) — default `video_codec = NULL`, meaning "emit no
`-codec:v`", preserving today's container-default behavior byte-for-byte.
Reject a literal `"libx264"` default and reject an `"auto"` string sentinel.**

Why `NULL` and not `"libx264"`:

- The trap in the brief is confirmed: today these verbs never populate
  `codec_video`, and `ffm_groups()` emits `-codec:v` only when it is populated
  (R/ffm.R:1204–1207). `crop_video("in.webm", "out.webm", …)` currently gets
  the WebM default encoder; a `"libx264"` default would silently force H.264
  into WebM — a runtime failure or an invalid file, and a behavior change for
  *every* non-default-H.264 container, batch tables included.
- A `NULL` default keeps every existing compiled command byte-identical, so the
  existing compile-string test corpus stays valid and no user's pinned
  pipeline changes output — the cheapest possible "additive" API commitment
  under D014's clean-break policy (nothing to break later).
- `NULL` is already the package's "leave it alone" idiom
  (`width`/`height`/`fps` in `standardize_video`, `audio = NULL` in the
  composites). An `"auto"` string is worse on every axis: it occupies
  `check_token()`-valid namespace (R/utils.R:44–59), so it could collide with
  a real encoder name, and it needs special-casing everywhere a codec string
  flows.

What `resolve_hw_encoder`/`codec_family` must do at the sentinel:

- Today `codec_family(NULL)` crashes uninformatively: `grepl(pattern, NULL)`
  returns `logical(0)` and the `if` at R/ffmpeg.R:1411 errors with "argument is
  of length zero". The sentinel branch must therefore be handled **before**
  family inference. Extend `resolve_hw_encoder()` (one seam, per D-M31 — do
  not fork a second resolver) with an explicit `is.null(video_codec)` branch:
  - `hardware = "none"` → return `NULL` unchanged (verb skips
    `ffm_codec(video=)` entirely; no `-codec:v`).
  - `hardware = "nvenc"`, `video_codec = NULL` → treat the family as
    **`"h264"`** (the package-wide default family: `standardize_video`'s
    default is libx264, `format_for_web` is fixed-H.264). If `has_nvenc("h264")`
    → return `"h264_nvenc"`.
  - `hardware = "nvenc"`, nvenc unavailable, `fallback = FALSE` → abort (the
    existing message, R/ffmpeg.R:1450–1458).
  - `hardware = "nvenc"`, nvenc unavailable, `fallback = TRUE`,
    `video_codec = NULL` → message, return **`NULL`** — i.e. fall back to
    exactly what `hardware = "none"` would have produced (container default),
    not to an injected libx264. Injecting libx264 on the fallback path would
    re-open the WebM trap through the back door and change the file's codec
    relative to the no-nvenc run — violating the documented fallback contract
    "keeps output reproducible by never changing the codec silently"
    (R/ffmpeg.R:631–634 spirit).
- Document the residual sharp edge: `hardware = "nvenc"` with the `NULL`
  default assumes the H.264 family, so an `.webm` output needs an explicit
  AV1-family `video_codec`; a family/container mismatch fails loudly at FFmpeg
  runtime, which is acceptable for an opt-in GPU path whose `has_nvenc()` docs
  already warn that listing ≠ working GPU (R/ffmpeg.R:1368–1374).

## Q3 — `pixel_format`: defer; `video_codec` (+`hardware`/`fallback`) is the right minimal surface

**Verdict: do not add `pixel_format` in M34.**

- The pairing exists on `standardize_video`/`anonymize_video` because those
  verbs *impose a standard*: yuv420p is part of the deliverable, and each
  carries the matching even-dimension crop guard
  (R/ffmpeg.R:681–694, 796–800). None of the four M34 verbs imposes a format:
  under the Q2 sentinel they deliberately preserve whatever FFmpeg negotiates,
  and none has `anonymize_video`'s odd-dimension/yuv420p safety need — crop
  dimensions are user-chosen, and stack/overlay output dimensions derive from
  the inputs. Any odd-dimension × libx264 failure a user can hit with
  `video_codec = "libx264"` is already reachable today via the mp4 container
  default, so M34 introduces no new failure class.
- GP1 says defer surface without demonstrated need; D014 makes a later
  `pixel_format` addition purely additive (no break). A user who wants a pinned
  codec *and* pixel format *and* the safety guard already has
  `standardize_video`.

## Q4 — Blessed multi-input verbs: composes cleanly; no IP3/D009 stretch

**(a) No ordering or mapping hazard.** `-codec:v` is emitted from the `codecs`
group of `ffm_groups()` (R/ffm.R:1204–1214) independent of the `complex` flag,
and the group order is filters → codecs → … → map → output
(R/ffm.R:1231–1242). FFmpeg parses all output options for an output file
together, so relative order among `-filter_complex`, `-codec:v`, and `-map` is
immaterial; an un-indexed `-codec:v` applies to every output *video* stream,
and the complex path has exactly one — `[vout]` (D006, R/ffm.R:1169–1186). The
optional carried audio (`-map N:a`, R/ffmpeg.R:3232–3234, 3316–3318) is
untouched by `-codec:v`. The one genuinely new compiled shape —
`-filter_complex … [vout]` + `-codec:v X` + `-map "[vout]"` in one command —
has no existing test precedent, so M34 must pin it with a compile test (BC7).

**(b) Consistent with IP3/D009.** D009's rule is "Layer-2 verbs only compute
arguments"; the codec name is computed at Layer 2 and assembled by
`ffm_codec`/`ffm_compile` at Layer 1, exactly like the `audio =` index that
D009 itself blesses resolving to `ffm_map()`. The knob configures the output
encode, not the graph — the filtergraph assembly, labels, and single-video-
output model are untouched. No stretch.

## Q5 — `segment_video` × stream-copy

**(a) Error is correct.** `hardware = "nvenc"` + `reencode = FALSE` is a
contradiction: an encoder choice for a path defined by not encoding. Warning-
and-ignoring would silently discard whichever intent the user meant, and this
package's established pattern for contradictory instructions is a `cli_abort`
with a repair hint (the copy-vs-filter and copy-vs-accurate-seek guards,
R/ffm.R:1089–1129). The error should name the fix ("use `reencode = TRUE`, or
drop `hardware`").

**(b) Non-default `video_codec` + `reencode = FALSE` must also error**, for the
same reason plus a mechanical one: `segment_pipeline()` implements the copy
path via `ffm_copy()` (R/ffmpeg.R:1596–1601), which sets `codec_video =
"copy"`; threading a real codec would either be overwritten by `ffm_copy()` or
overwrite it, depending on call order — order-dependent silent behavior either
way. Honoring it would break the documented lossless contract; ignoring it
would break "never change the codec silently". The combination is not
meaningful. With the `NULL` sentinel the guard is trivial:
`if (!reencode && (!is.null(video_codec) || hardware != "none")) abort`.
Because `segment_video_batch` supports a **per-row** `reencode` column
(R/ffmpeg.R:1684–1708), this guard must run **per row** against the row's
resolved `reencode` and resolved codec/hardware — a batch-level check on the
scalar args alone would miss mixed tables. Putting the guard in the shared
`segment_pipeline()` gives both callers the check by construction (the M13
lesson already cited at R/ffmpeg.R:420–423).

**(c) Engine interaction: none new.** The only relevant `ffm_compile` guard is
"frame-accurate seek × codec `copy` aborts" (R/ffm.R:1122–1129); the
`reencode = TRUE` path threads a *real* codec, so it never trips, and the
`reencode = FALSE` path aborts at Layer 2 before compile. Seek placement
(output-seek after `-i` for re-encode, input-seek + `-avoid_negative_ts` for
copy, R/ffm.R:1138–1151) is orthogonal to output codec options. No `ffm.R`
change is needed — which is itself worth asserting (BC9).

## Q6 — Batch siblings: per-row `video_codec` column; `hardware`/`fallback` stay batch-wide

**Verdict: `video_codec` is a per-row override column via the `pick()`
convention; `hardware`/`fallback` remain batch-wide captured scalars.**

- Precedent is direct: `standardize_video_batch` already treats `video_codec`
  as a `pick()` column (R/ffmpeg.R:2195) while keeping `hardware`/`fallback`
  batch-wide (R/ffmpeg.R:2197–2198). The split is principled, not accidental:
  the codec is a per-file property (mixed-container jobs tables genuinely need
  per-row codecs), while nvenc availability is a machine property — a per-row
  `hardware` column has no realistic use and would multiply the
  fallback-message and abort surface per row (D-M31).
- **NA semantics:** because the scalar default is the `NULL` sentinel, the
  column needs a spelling for "this row uses the container default". Adopt the
  established column-NA-means-scalar-NULL convention from the fan-in verbs'
  `audio` columns (R/ffmpeg.R:3450–3451, 3506–3511): a `video_codec` cell of
  `NA` maps to `NULL` (no `-codec:v` for that row). This is what makes the
  mixed-container jobs table (the motivating case) expressible.
- **Validation (the known hazards, addressed):**
  1. *Upfront column-type check:* a present `video_codec` column must be
     character — except an **all-NA logical** column, which must be accepted as
     all-default (the logical-NA trap the brief cites; exact precedent:
     `picture_in_picture_batch`'s audio-column guard, R/ffmpeg.R:3614–3620).
     Note `check_batch_string_col()` (R/ffmpeg.R:2660–2670) rejects NA, so it
     cannot be reused as-is for this column; use the audio-column pattern.
  2. *Per-row re-validation:* per-row values bypass the scalar verb's guards,
     but `check_token()` lives in `ffm_codec()` itself (R/ffm.R:516–517), so
     every non-NA cell is token-validated per row by construction the moment
     the shared pipeline threads it. No extra per-row token check is needed —
     but the Q5 `reencode × codec/hardware` conflict check must also be
     per-row (see Q5b).
  3. *Batch-wide interplay:* with batch-wide `hardware = "nvenc"`, family
     resolution runs per row inside the shared pipeline (as
     `standardize_pipeline` already does, R/ffmpeg.R:679), so a row whose
     codec family has no nvenc mapping aborts the batch at build time with
     `codec_family()`'s existing message — acceptable fail-fast.
- Do **not** honor a per-row `hardware` or `fallback` column (parity with M31's
  documented "Batch-wide (not a per-row column)", R/ffmpeg.R:2996–2998); the
  `pick()` closure must not list them, so stray columns are inert (the
  established "other columns are ignored" contract).

## Beyond the brief

1. **Carried audio in the composites is re-encoded, not copied.** When
   `compare_videos`/`picture_in_picture` map an input's audio
   (R/ffmpeg.R:3232–3234, 3316–3318) no `-codec:a` is ever set, so FFmpeg
   re-encodes the audio to the container default — likely surprising next to
   the package's "audio is stream-copied" norm (`standardize_video`,
   R/ffmpeg.R:698–700). Out of M34's scope; worth a ROADMAP candidate
   (an `audio_codec`/copy decision for the composites) or at least a doc note.
2. **`codec_family(NULL)` failure mode.** `grepl(…, NULL)` → `logical(0)` →
   an uninformative base-R error at R/ffmpeg.R:1411. Even independent of M34,
   the function's contract would benefit from a `check_string()` at entry;
   M34's sentinel branch (Q2) must in any case be ordered before any
   `codec_family()` call.
3. **`format_for_web_batch` will silently ignore a `video_codec` column.** Its
   closure passes no per-row knobs (R/ffmpeg.R:3036–3040, documented "no
   per-row knobs"). Post-M34, users who learn the column convention on the
   four new verbs may pass such a column here and get silence. Existing,
   documented behavior — a one-line doc cross-reference would close it.

## Recommendations

- **R1 (apply):** Add `video_codec` (D014 spelling) + `hardware = c("none",
  "nvenc")` + `fallback = FALSE` to all four verbs and their `_batch`
  siblings — Option B, no split (Q1).
- **R2 (apply):** Default `video_codec = NULL` = emit no `-codec:v`; nvenc with
  the sentinel resolves to the h264 family; `fallback = TRUE` with the sentinel
  falls back to *no* `-codec:v` (container default), with the existing message
  (Q2).
- **R3 (apply):** Implement the sentinel as an explicit `NULL` branch inside
  `resolve_hw_encoder()` (one seam, D-M31), ordered before `codec_family()`
  (Q2, Beyond-2).
- **R4 (apply):** Defer `pixel_format`; it remains a purely additive later
  argument (Q3).
- **R5 (apply):** Abort (cli, with repair hint) on `reencode = FALSE` combined
  with non-`NULL` `video_codec` or `hardware != "none"`, enforced per row in
  the shared `segment_pipeline()` so both callers inherit it (Q5).
- **R6 (apply):** Batch: `video_codec` as a per-row `pick()` column with
  NA → sentinel and the all-NA-logical acceptance; `hardware`/`fallback` stay
  batch-wide formals (Q6).
- **R7 (consider):** In the four verbs' `@param hardware` docs, state that the
  sentinel assumes the H.264 family and that non-H.264 containers (e.g.
  `.webm`) need an explicit AV1/HEVC-family `video_codec` under nvenc (Q2).
- **R8 (consider):** ROADMAP candidate for the composites' carried-audio
  re-encode default (Beyond-1) and the `format_for_web_batch` doc
  cross-reference (Beyond-3).
- **R9 (reject — namespace collision):** an `"auto"` string sentinel; it lives
  inside `check_token()`-valid encoder-name space and duplicates `NULL`'s
  established idiom (Q2).
- **R10 (reject — container trap):** a literal `"libx264"` default; it
  silently changes every non-H.264-container invocation and invalidates the
  existing compiled-command corpus (Q2).

## Binding criteria

- **BC1:** `crop_video`, `segment_video`, `compare_videos`,
  `picture_in_picture` and their `_batch` siblings each gain formals
  `video_codec = NULL`, `hardware = c("none", "nvenc")`, `fallback = FALSE`
  (exact D014 spellings; no `vcodec`/`codec` alias), verified by
  `formals()`-level or documented-usage evidence.
- **BC2:** With all-default arguments, each of the four verbs (and batch
  siblings) compiles commands **byte-identical** to pre-M34: a passing
  regression test asserts the compiled string contains no `-codec:v` token and
  matches the pre-M34 literal for at least one single-input verb
  (`crop_video`) and one multi-input verb (`compare_videos`).
- **BC3:** `crop_video(…, video_codec = "libx265", run = FALSE)` compiles a
  command containing `-codec:v libx265`; a non-token value (e.g.
  `"libx264 -evil"`) aborts via `check_token()`.
- **BC4:** Under `withr::local_options(tidymedia.nvenc_encoders =
  "h264_nvenc")`, each of the four verbs with `hardware = "nvenc"` and default
  `video_codec` compiles `-codec:v h264_nvenc`; with `video_codec = "libx265"`
  and the option set to `"hevc_nvenc"`, compiles `-codec:v hevc_nvenc`.
- **BC5:** Under an empty nvenc pool (`tidymedia.nvenc_encoders =
  character(0)`): `hardware = "nvenc"`, `fallback = FALSE` aborts;
  `fallback = TRUE` with default `video_codec` emits a message and compiles
  with **no** `-codec:v`; `fallback = TRUE` with `video_codec = "libx264"`
  emits a message and compiles `-codec:v libx264`.
- **BC6:** `segment_video(…, reencode = FALSE, hardware = "nvenc")` and
  `segment_video(…, reencode = FALSE, video_codec = "libx264")` each abort
  with a `cli` error; in `segment_video_batch`, a jobs table whose per-row
  `reencode` column contains `FALSE` on a row with a non-NA resolved
  `video_codec` (column or batch-wide) aborts — evidenced by passing tests
  covering both the scalar and the per-row-column path.
- **BC7:** `compare_videos` and `picture_in_picture` with `video_codec` set
  compile a single command containing all of `-filter_complex`, the `[vout]`
  label, `-map "[vout]"`, and `-codec:v <codec>` (compile-string test, no
  binary).
- **BC8:** The four `_batch` siblings accept a per-row `video_codec` column:
  a character column may contain `NA` (that row compiles no `-codec:v`;
  non-NA rows compile their own codec); an **all-NA logical** column is
  accepted as all-default; a numeric `video_codec` column aborts up front.
  `hardware`/`fallback` are honored only as formals — a `hardware` jobs column
  does not alter per-row commands.
- **BC9:** M34 changes to `R/ffm.R` are documentation-only or absent:
  `ffm_codec()` and the compile path (`ffm_groups`/`ffm_compile`) have no
  functional diff on the milestone branch (IP2: no new engine capability).
- **BC10:** No `pixel_format` argument is added to any of the four verbs or
  their batch siblings in M34.
