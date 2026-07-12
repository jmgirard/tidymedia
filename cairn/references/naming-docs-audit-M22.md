# Naming & Docs Audit (M22)

_Assessment-only deliverable. Every recommendation here is a **proposal** for
user sign-off at M22 review; no `R/` source changes are made by M22. Approved
recommendations are executed by the follow-up milestone "Apply M22 naming/docs
recommendations" under the **clean-break** rename policy (no `lifecycle`
shims — the API is pre-0.2.0 and still soaking, D001/CRAN-candidate framing)._

Generated 2026-07-12 from `NAMESPACE`, `R/*.R`, `vignettes/*.Rmd`, `README.Rmd`.
Line citations are to the definition site.

## Method

- Inventoried all 79 `NAMESPACE` exports, grouped by architectural family.
- Checked every function name against the package's implicit conventions
  (`verb_object` for task verbs; `ffm_*` layer prefix; singular scalar / plural
  batch).
- Extracted every exported signature and derived a canonical argument
  vocabulary, then listed deviations.
- Attributed each roxygen block to its object and recorded `@examples` /
  `@return` / `@seealso` / `@family` presence (accounting for `@rdname` /
  `@inherit` shared doc pages).

## Headline

Docs are in **good** shape: every exported function reaches an `@examples` and
`@return` (directly or via a shared `@rdname`/`@inherit` page), and `@family` is
universal. The main doc gap is **`@seealso` cross-linking** (present on only 10
of ~50 doc pages). The higher-value target is **naming consistency**: a handful
of function names break the conventions, the `get_*` prefix carries two
unrelated meanings, argument vocabulary splits three ways for the same concept,
and several exports look like unintended public API.

---

## 1. Export inventory by family (T1)

### Layer 0 — escape hatch (3)
`ffmpeg` (ffmpeg.R:17) · `ffprobe` (ffprobe.R:17) · `mediainfo` (mediainfo.R:19)

### Layer 1 — engine `ffm_*` (23)
- Construction / compile / run / batch: `ffm_files` (ffm.R:19), aliased `ffm`
  (ffm.R:60) ·
  `ffm_compile` (ffm.R:1013) · `ffm_run` · `ffm_batch` (ffm_batch.R) ·
  `ffm_manifest` (ffm_manifest.R)
- Input / output: `ffm_copy` · `ffm_seek` · `ffm_map` · `ffm_drop` (ffm.R:228) ·
  `ffm_codec` (ffm.R:491) · `ffm_pixel_format` (ffm.R:607) ·
  `ffm_output_options` (ffm.R:968)
- Filters: `ffm_trim` (ffm.R:95) · `ffm_crop` (ffm.R:269) · `ffm_scale`
  (ffm.R:308) · `ffm_fps` (ffm.R:342) · `ffm_loudnorm` (ffm.R:407) ·
  `ffm_drawbox` (ffm.R:921)
- Blessed multi-input: `ffm_hstack` (643) · `ffm_vstack` (709) · `ffm_overlay`
  (788) · `ffm_concat` (857)

### Layer 2 — task verbs (18, all ffmpeg.R)
`extract_audio` · `extract_frame` · `extract_frames` · `crop_video` ·
`segment_video` · `segment_videos` · `separate_audio_video` ·
`concatenate_videos` · `format_for_web` · `standardize_video` ·
`standardize_videos` · `normalize_audio` · `normalize_audios` · `audio_as_mp3` ·
`compare_videos` · `picture_in_picture` · `anonymize_video` · `anonymize_videos`

### Metadata (15)
- ffprobe → tibbles: `probe_all` · `probe_container` · `probe_streams` ·
  `probe_video` · `probe_audio` (ffprobe.R)
- MediaInfo → tibbles/values: `mediainfo_parameter` · `mediainfo_query` ·
  `mediainfo_template` · `mediainfo_summary` (mediainfo.R)
- MediaInfo → scalars (`get_*`): `get_duration` (mediainfo.R:285) ·
  `get_framerate` (316) · `get_width` (335) · `get_height` (354) ·
  `get_samplingrate` (373)
- Output verification: `verify_media` (verify.R:43)

### Program management (12, incl. the 2 ffmpeg capability queries)
`find_ffmpeg` · `find_ffprobe` · `find_ffplay` · `find_mediainfo` · `set_ffmpeg`
· `set_ffprobe` · `set_ffplay` · `set_mediainfo` · `set_program` ·
`install_on_win` (program_management.R); plus ffmpeg **capability** queries
`get_codecs` · `get_encoders` (ffmpeg.R).

### Tidy-eval reexports & utils (8)
`enquo` · `enquos` · `as_label` · `as_name` · `:=` · `.data`
(utils-tidy-eval.R); `pad_integers` (utils.R) · `convert_fractions`
(ffprobe.R:236).

---

## 2. Function-name findings (T2)

Severity: **H** = breaks a convention / actively misleads · **M** = friction ·
**L** = cosmetic.

| # | Sev | Name(s) | Issue | Recommendation |
|---|-----|---------|-------|----------------|
| N1 | H | `get_codecs`, `get_encoders` | Share the `get_*` prefix with the file-metadata getters (`get_duration`…) but are **ffmpeg capability** queries — one prefix, two unrelated meanings. | Rename to a capability namespace, e.g. `ffmpeg_codecs()` / `ffmpeg_encoders()` (or `list_codecs()` / `list_encoders()`). Reserve `get_*` for per-file metadata. |
| N2 | H | `audio_as_mp3` | Breaks `verb_object`; every Layer-2 sibling is `verb_object` (`extract_audio`, `crop_video`). Also hard-codes one output format in the name. | Rename to a verb form with a format arg, e.g. `convert_audio(format = "mp3")` or `transcode_audio()`. Decide whether it stays mp3-only. |
| N3 | M | `get_samplingrate`, `get_framerate` | No-separator compounds, and `samplingrate` disagrees with the `sample_rate` **argument** used everywhere else (35 occurrences vs 3). | `get_sample_rate()` / `get_frame_rate()` — align name to the arg vocabulary (see A-item on rates). |
| N4 | M | `normalize_audios` | The plural-of-a-mass-noun reads awkwardly ("audios"); the batch-plural convention (`_videos`) doesn't transfer cleanly to "audio". | Either accept it for convention-consistency, or switch the batch convention for this pair (e.g. `normalize_audio_batch()`). Cross-cuts N-item below. |
| N5 | M | `probe_*` vs `get_*` vs `mediainfo_*` | Three metadata families, two backends (ffprobe vs MediaInfo), overlapping outputs (`probe_video` and `get_width` both yield width). Vignettes advertise all three. Boundary is real but **undocumented**, so users can't tell which to reach for. | Keep all three (they differ by backend + return shape) but **document the boundary explicitly** (family `@description` + a vignette table). Consider renaming `get_*` file-getters to signal the MediaInfo backend (e.g. `mi_duration()`), pending the N1 `get_*` cleanup. |
| N6 | M | `enquo` `enquos` `as_label` `as_name` `:=` (`.data` — see note) | Standard tidy-eval reexports, but the four quoting helpers + `:=` have **no internal use** and no exported data-masking API relies on them. Likely unintended surface. **`.data` is an exception — it IS used internally** (`filter_streams()`, `R/ffprobe.R:191`, behind `probe_streams`/`video`/`audio`). | Drop `enquo`/`enquos`/`as_label`/`as_name` (no user-facing NSE contract). **Keep `.data`** (internal use + tidyselect convention); keep `:=` only if a documented pmap/tidyselect pattern needs it, else drop. |
| N7 | M | `pad_integers`, `convert_fractions` | General-purpose helpers exported as public API; `convert_fractions` is an ffprobe-internal frac-string parser. | Un-export both (make internal), or move to a clearly-scoped util page if genuinely useful to users. Matches the DESIGN "candidates for cleanup" note. |
| N8 | L | `extract_frame` / `extract_frames` | Reads like "one frame / many frames from one video", but `extract_frames` is actually the **batch** (jobs-table) sibling. Technically consistent with the plural=batch rule, but the collision with "many frames" is a documentation trap. | Keep the names; **disambiguate in the docs** (title + first line must say "batch"). Revisit only if the N-item changes the batch-suffix convention. |
| N9 | L | `install_on_win` | `_on_win` platform suffix is unlike any other verb; fine but lonely. | Leave as-is (accurately scoped); note for consistency review only. |
| N10 | L | `separate_audio_video` | Long, but descriptive and unambiguous. | Keep. |

**Cross-cutting (N — batch suffix):** the package has two batch conventions —
plural noun (`*_videos`, `*_audios`) and `jobs`-table verbs. They coincide today,
but N4 shows the plural strains for non-count nouns. The scheme decision
(§6) must pick one rule and state it, so future batch verbs are predictable.

---

## 3. Argument-name consistency (T3)

Derived canonical vocabulary and every deviation. The recommended target column
assumes we **unify on `infile`/`outfile`/`infiles`** for all
file-processing verbs and **`file`** for read-only metadata (a defensible split:
"a thing being transformed" vs "a thing being read").

| Concept | Canonical (majority) | Deviations (with site) | Recommended target |
|---|---|---|---|
| Single input file | `infile` (all Layer-2, `probe_*`) | `file` (`get_*`, `mediainfo_*`, `verify_media`); `input` (`ffm_files`); `main`+`overlay` (`picture_in_picture`); `infile`+`audiofile`+`videofile` (`separate_audio_video`) | `infile` for transforms; `file` for read-only metadata. `ffm_files(input,output)` stays (Layer-1 primitive vocabulary). `picture_in_picture(main, overlay)` → keep semantic names but document; `separate_audio_video` keep. |
| Single output file | `outfile` | `output` (`ffm_files`) | `outfile`; `ffm_files` exempt as above. |
| Multiple inputs | `infiles` (`compare_videos`, `concatenate_videos`) | — | `infiles` (consistent already). |
| Codec selection | — (split) | `acodec` (`extract_audio`), `vcodec` (`standardize_video`, `anonymize_video`) vs `audio`/`video` (`ffm_codec`) | Layer-2: standardize on `audio_codec` / `video_codec` (full words, matches `pixel_format`, `sample_rate`, and `verify_media`'s `audio_codec`/`video_codec`). Retire `acodec`/`vcodec`. |
| Pixel format | `pixel_format` (consistent) | — | Keep. |
| Sample rate | `sample_rate` (35×) | `samplingrate` (in `get_samplingrate` name, N3) | `sample_rate` everywhere incl. the getter name. |
| Time bounds | `start` / `end` (`ffm_seek`, `ffm_trim`) | `ts_start` / `ts_stop` (`segment_video`, and `ffm_trim` also has `duration`); `timestamp` (`extract_frame`) | `start`/`end` across the board (retire `ts_start`/`ts_stop`, prefer `end` over `stop`). `timestamp` is a distinct single-instant concept — keep. |
| Run toggle | `run = TRUE` (all verbs) | — | Keep (exemplary consistency). |
| Re-encode toggle | `reencode` (`ffm_seek`, `segment_video`, `separate_audio_video`) | — | Keep. |
| Batch parallelism | `parallel = FALSE`, `...` | — | Keep. |

**Argument findings summary:** the two genuine harmonization targets are
**codec args** (`acodec`/`vcodec` → `audio_codec`/`video_codec`) and **time
bounds** (`ts_start`/`ts_stop` → `start`/`end`). The `infile`-vs-`file` split is
recommended to be **kept and documented** rather than flattened, because it
carries real "transform vs read" meaning.

---

## 4. Docs gap-fill checklist (T4, targeted scope)

Every export reaches `@examples` + `@return` (directly or via shared page), so
those are **not** gaps. Concrete to-dos:

- **Add `@seealso` cross-links** — missing on all of: the Layer-1 `ffm_*` filter
  and I/O verbs (`ffm_trim`, `ffm_seek`, `ffm_drop`, `ffm_crop`, `ffm_scale`,
  `ffm_fps`, `ffm_loudnorm`, `ffm_codec`, `ffm_map`, `ffm_copy`,
  `ffm_pixel_format`, `ffm_hstack`, `ffm_vstack`, `ffm_overlay`, `ffm_concat`,
  `ffm_output_options`, `ffm_compile`, `ffm_run`), the Layer-2 verbs without one
  (`extract_frame`, `extract_audio`, `separate_audio_video`, `audio_as_mp3`,
  `crop_video`, `format_for_web`, `standardize_video`, `normalize_audio`,
  `concatenate_videos`, `compare_videos`, `picture_in_picture`), all metadata
  families (`probe_*`, `mediainfo_*`, `get_*`), and program management.
  (`@family` already groups them; add `@seealso` to bridge **across** families,
  e.g. Layer-2 verb ↔ the `ffm_*` verbs it wraps, and `probe_*` ↔ `get_*` ↔
  `mediainfo_*` so users discover the alternative backend.)
- **Metadata-boundary prose** — add a `@description`/`@details` sentence to each
  metadata family page stating backend (ffprobe vs MediaInfo) and return shape
  (tibble vs scalar), plus a comparison table in `vignettes/metadata.Rmd`
  (per N5).
- **Batch disambiguation** — `extract_frames` (and every `*_videos`/`_audios`
  page) should say "batch" in the title/first line (per N8).
- **Verify after any rename** — `devtools::document()` + `spelling::update_wordlist()`
  will be needed in the execution milestone (new getter names like `sample_rate`
  are already in the wordlist via the arg; capability names like `ffmpeg_codecs`
  may add terms — see LESSONS M17 on the masked spelling NOTE).

No `@param` was found undocumented in the sweep; the execution milestone must
re-verify `@param` coverage for any renamed argument.

---

## 5. Ranked recommendations (for review sign-off)

1. **(H) N1** — split the overloaded `get_*` prefix: `get_codecs`/`get_encoders`
   → capability namespace; reserve `get_*` for file metadata.
2. **(H) N2** — `audio_as_mp3` → `verb_object` form with a format arg.
3. **(M) Codec args** — `acodec`/`vcodec` → `audio_codec`/`video_codec`.
4. **(M) N3 + rate args** — `get_samplingrate`/`get_framerate` →
   `get_sample_rate`/`get_frame_rate`.
5. **(M) Time-bound args** — `ts_start`/`ts_stop` → `start`/`end`.
6. **(M) N6** — drop the 4 unused tidy-eval quoting reexports (keep `.data`).
7. **(M) N7** — un-export `pad_integers`, `convert_fractions`.
8. **(M) N5 + docs** — document the metadata-family boundary; optionally signal
   the MediaInfo backend in `get_*` names.
9. **(M) Docs** — add the `@seealso` cross-link web (§4).
10. **(L) N4/N8/N9** — batch-suffix decision + batch/frame disambiguation prose.

---

## 6. Proposed target naming scheme (feeds the DECISIONS entry)

Recommended conventions to codify (subject to review sign-off):

- **Task verbs** are `verb_object` (`extract_audio`, `crop_video`); no verb
  encodes a fixed format/codec in its name (fixes N2).
- **Layer prefix** `ffm_*` marks Layer-1 engine surface; nothing outside Layer 1
  uses it (reinforces IP1's three-layer separation — this D-entry *references*
  IP1, it does not change it).
- **Batch sibling** convention: pick **one** — recommended is the plural-noun
  suffix (`*_videos`), accepting `normalize_audios`; the alternative
  (`*_batch`) is cleaner for mass nouns. State the chosen rule so future batch
  verbs are predictable (fixes N4/N-cross-cutting).
- **Metadata prefixes** carry backend meaning: `probe_*` = ffprobe→tibble,
  `mediainfo_*` = MediaInfo→tibble/value, file-metadata scalars stay a distinct
  prefix (fixes N1/N5). `get_*` is **not** used for capability queries.
- **Argument vocabulary:** `infile`/`outfile`/`infiles` for transforms, `file`
  for read-only metadata; full-word compound args (`audio_codec`, `video_codec`,
  `pixel_format`, `sample_rate`); time bounds `start`/`end` (+ `duration`,
  `timestamp` where semantically distinct); `run`, `reencode`, `parallel`
  keep their current spellings.
- **Rename policy:** clean break (no `lifecycle` shims) while pre-0.2.0.
