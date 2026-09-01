# RB06: Four API changes held open by the pre-0.2.0 clean-break window (M099)

- **Date:** 2026-09-01
- **Output required:** write findings to `cairn/reviews/RR06-d014-rename-window.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** tidymedia is an R interface to FFmpeg and MediaInfo for
reproducible media preprocessing in research pipelines: batch trimming,
cropping, format standardization, metadata as tibbles. Three layers
(`cairn/DESIGN.md`, D002): Layer 0 raw wrappers (`ffmpeg()`, `ffprobe()`,
`mediainfo()`); Layer 1 a linear pipe builder (`ffm_*`); Layer 2 task verbs
(`extract_audio()`, `segment_video()`, …) over Layer 1. Version 0.1.x; not yet
on CRAN. 89 exported objects.

**The policy under review.** D014 (`cairn/DECISIONS.md:149-175`) fixes the
naming vocabulary and a **clean-break rename policy**: until 0.2.0 reaches
CRAN, an exported name may be changed outright with no deprecation shim. The
maintainer has declined to replace this with a `lifecycle` deprecation policy
(M099 plan gate, 2026-08-31); the clean break stands and 0.2.0 closes it. A
CRAN readiness row exists but **no release window is declared** — you are not
asked when 0.2.0 ships, only what the surface should be when it does.

**Why this brief.** Four API changes have been carried as ROADMAP candidates
"until the window closes." Each was declined at an in-session question gate
when it first arose, with a stated falsifier that has not fired. M099 exists
to decide each one permanently — ship now, or decline and close the row — and
at its implementation gate on 2026-09-01 the maintainer escalated all four
here rather than take the session's recommendation. None of the four has been
reviewed independently before. Every accepted change is irreversible on the
exported surface once 0.2.0 ships (RB tripwire: `irreversible-api`).

**Second-escalation rule.** Two of the mechanisms below were *named* in earlier
briefs — `audio_stream` as background in RB02/RB03, and the nvenc helpers as
machinery to reuse in RB01 (`cairn/reviews/archive/`). Under the plugin's
second-escalation rule, removal of the mechanism is therefore listed among the
options for (a) and (d). Weigh it as a real option, not a formality.

**The session's recommendation**, for you to confirm or overturn: decline
(a), (b), (c) permanently; ship (d) as `has_hardware_encoder()`,
`hardware_encoder()`, `tidymedia.hardware_encoders`.

### Candidate (a): `audio_stream` / `audio`, and the two `NULL` readings

Two exported arguments both read as "0-based audio index":

- `audio_stream` on 18 verbs (`extract_audio`, `convert_audio`,
  `normalize_audio`, `separate_audio_video`, `standardize_video`, `crop_video`,
  `segment_video`, `anonymize_video`, `format_for_web`, and their nine `_batch`
  siblings) indexes **one input's audio streams**: `1` is the file's second
  track. Compiles `-map 0:a:<n>`.
- `audio` on 6 exports (`compare_videos`, `picture_in_picture`, their `_batch`
  siblings, `ffm_codec`, `ffm_copy`) indexes **the verb's inputs** on the two
  fan-in verbs (`1` = take audio from the second *file*); on the two `ffm_*`
  builders it is a codec string and a logical respectively.

`audio_stream = NULL` reads two ways: the **first track** on the four
extraction entry points (`extract_audio`, `convert_audio`, `normalize_audio`
+ `_batch`), compiling `0:a:0`; **every track** on the other fourteen
(`separate_audio_video` compiles bare `0:a`; the pass-through video verbs
compile `0:a?` so an audio-less input still runs). Each reading was chosen on a
measurement: an extraction verb writes one stream so must pick one; a
first-track default on the pass-through verbs would silently narrow
three-track inputs to one (D025, D026). M51 answered the two-name confusion
with documentation rather than a rename: a `?audio_stream` topic and generated
`@param` text (D032), with a test asserting every verb links to it.

The candidate row (`cairn/ROADMAP.md:48`) holds open: renaming `audio_stream`
or `audio`, or unifying the `NULL` readings. Falsifier as recorded: "a caller
confused by the two names or the two `NULL` readings; never the argument count
alone." No such report exists.

### Candidate (b): a per-verb `check_tracks =` argument

M44 added a diagnostic: when a verb is asked to pass audio through with no
`audio_stream` named and the input has more than one audio track, an FFprobe
call counts the tracks and a warning names the ones dropped. It costs one
FFprobe spawn per distinct input, on the `run = TRUE` path only. M082 gave it a
session-wide off switch, the option `tidymedia.check_tracks` (default `TRUE`,
read by `resolve_check_tracks()` at `R/timeout.R:116`, gating seven probe
sites). Per-call form today: `withr::local_options(tidymedia.check_tracks =
FALSE)`. A per-verb argument was declined at M082's gate on D047's reasoning
(a seam commits no exported signature). Candidate row `cairn/ROADMAP.md:30`
(a). Falsifier: "a caller needing two different answers inside one script."

### Candidate (c): a per-call `timeout =` argument

M69 added a wall-clock limit on every spawned program, carried by the option
`tidymedia.timeout` (whole seconds, `0` = none, default `0`; D047). D047
explicitly rejected `timeout =` arguments on the run-capable verbs as "the
largest irreversible-API commitment the package could make," naming the
session grain as the cost. M072 then shipped `with_timeout(expr, seconds)` and
`local_timeout(seconds)` (`R/timeout.R:182`, `:321`), which set the option for
one call's dynamic extent (D051). D051 measured the reach: the wrapper covers
all 53 exports through which a spawn can be seen (`tm_timeout_domain()`,
`tests/testthat/helper-timeout-sweep.R:104`), where an argument could reach the
31 that take `run =`. The recorded residual: a limit that varies **per row
inside one batch**, which a wrapper around the batch cannot express. Candidate
row `cairn/ROADMAP.md:44`. Falsifier: that per-row case, or "a report that
wrapping an expression is the wrong ergonomics."

### Candidate (d): backend-neutral names for the hardware-encoder surface

`hardware = c("none", "nvenc")` is an argument on 16 exported verbs (plus the
internal `resolve_hw_encoder()`, `R/ffmpeg.R:3095`). Three exported names hard-
code the single backend:

- `nvenc_encoder(codec)` → `"h264_nvenc"` etc. (`R/ffmpeg.R:3018`)
- `has_nvenc(codec)` → logical: is that encoder in the local FFmpeg build?
  (`R/ffmpeg.R:3025`; body in `nvenc_available()`, `:3048`)
- option `tidymedia.nvenc_encoders`: a character vector of encoder names to
  treat as available, read before any probe (D044; `R/ffmpeg.R:3050`), carried
  into `parallel = TRUE` workers (`R/timeout.R:553`), documented in three help
  topics, and used at ~150 test sites.

**M100** (planned, depends on M099's answer here) generalizes this into a
backend vocabulary and ships **videotoolbox** (Apple) as the second member:
`hardware = c("none", "nvenc", "videotoolbox")` at all 16 verbs, a per-backend
codec-family table, and a `backend =` argument on the availability helper.
M100 owns the *signature*; M099 owns the *names*. If (d) is declined, M100
ships `has_nvenc("h264", backend = "videotoolbox")` and a
`tidymedia.nvenc_encoders` option holding `"h264_videotoolbox"`. Note that
`hardware=`'s own widening is additive and is not a rename; only these three
names are the irreversible part.

Renaming here touches: the two exports, three `man/` topics (via roxygen),
`_pkgdown.yml:122-123`, `vignettes/workflow.Rmd:79` (`has_nvenc("h264")`),
`NEWS.md` (new entry; four historical mentions stay), and the tests. There is
no `lifecycle` dependency and D014 permits none to be added for this.

## Materials

Read these in full where a range is given; the rest as needed.

- `cairn/DECISIONS.md` — headings at `## D0NN — …`. Read: D014 (149-175);
  D023 (500-633); D025 (634-692); D026 (693-760); D032 (1089-1195); D044
  (1787-1860); D047 (1941-2005); D051 (2178-2240); D060 (2759-2815).
- `cairn/milestones/M099-d014-rename-window-review.md` — the milestone,
  including its AC1 sweep procedure and the work-log's branch-point sweep
  (18 / 6 / 0 / 0 formals; two hardware exports).
- `cairn/milestones/M100-videotoolbox-backend.md` — what depends on (d).
- `cairn/milestones/archive/M51-audio-index-docs.md`, `M082-track-check-opt-out.md`,
  `M072-per-call-timeout.md`, `M31-nvenc-encoding.md`.
- `cairn/ROADMAP.md:30`, `:44`, `:48` — the candidate rows and their falsifiers.
- `cairn/DESIGN.md` — GP1 (prefer refusing surface over growing it) and the
  IP block; D014's vocabulary rules are the naming constraint.
- `R/audio-stream-doc.R` — the generated `@param` machinery for (a).
- `R/ffmpeg.R:3005-3060` (the helpers and probe), `:3068-3200`
  (`codec_family()`, `resolve_hw_encoder()`, `check_nvenc_available()`);
  `R/cache.R` (the session memo); `R/timeout.R:1-130` (the three option
  seams), `:182-340` (`with_timeout()`, `local_timeout()`), `:540-560` (the
  parallel carrier).
- `R/tidymedia-package.R:100-200` — the landing topic's *Session options*
  section, which documents all three seams to users.
- `tests/testthat/helper-nvenc-memo.R:25-45` (`nvenc_hardware_exports()`),
  `helper-timeout-sweep.R:90-120` (`tm_timeout_domain()`).
- `vignettes/workflow.Rmd:70-90` — how users are taught the hardware check.

To run: `Rscript -e 'devtools::test()'` needs `ffmpeg`/`ffprobe` on `PATH` for
the executing tests (they `skip_if` absent); the surface can be enumerated with
`pkgload::load_all(); getNamespaceExports("tidymedia")` and `formals()`.

## Questions

Answer each with a disposition — **ship** (with the exact names or shape),
**decline permanently**, or **remove** where offered — and the evidence class
that would make the disposition wrong.

1. **(a) names.** Should `audio_stream` or `audio` be renamed before the window
   closes? Consider: whether "0-based index of two different things under two
   names both containing `audio`" is a confusion the docs can carry
   indefinitely, or a naming defect the clean break should fix now; what name
   would be better (e.g. `track`, `audio_track`, `audio_index`) and whether it
   fits D014's full-word compound vocabulary; and, per the second-escalation
   rule, whether either argument should be **removed** — e.g. whether `audio`
   on the two fan-in verbs earns its existence, or whether `ffm_codec()`'s and
   `ffm_copy()`'s reuse of the word `audio` for unrelated types should end.

2. **(a) `NULL`.** Should the two `NULL` readings be unified? If yes, in which
   direction, and how would the measured data loss D025/D026 record be
   avoided? If no, is the D032 documentation answer sufficient, or does one
   argument carrying two defaults across 18 verbs warrant a different
   mechanism (a sentinel value, a second argument, a per-family name)?

3. **(b).** Should the verbs that run the dropped-track probe gain a
   `check_tracks =` argument (about 18 formals), given the option seam and its
   `withr::local_options()` one-call form? State whether the "two answers
   inside one script" case is realistic enough to pay eighteen signatures for,
   and whether shipping it would obligate matching arguments for the other two
   seams.

4. **(c).** Should the 31 run-capable verbs (and `ffm_run()` / `ffm_batch()`)
   gain a `timeout =` argument, given `with_timeout()` / `local_timeout()`
   reach 53 exports? D047 called this the largest irreversible commitment
   available; D051 left the per-row-in-a-batch case as the only residual. Is
   a batch column (`timeout` in the `jobs` tibble, the batch verbs' per-row
   form) a better answer to that residual than a scalar argument, and is
   either worth shipping absent a request?

5. **(d) names.** Should `has_nvenc()`, `nvenc_encoder()` and
   `tidymedia.nvenc_encoders` be renamed before M100 adds videotoolbox? If yes,
   evaluate the proposed `has_hardware_encoder()`, `hardware_encoder()`,
   `tidymedia.hardware_encoders` against alternatives (`has_hw_encoder`,
   `encoder_name`, `hardware_encoder_name`, `tidymedia.encoders`,
   `tidymedia.available_encoders`), against D014's vocabulary (full-word
   compounds; `get_*` reserved for per-file scalars; capability queries are not
   `get_*`), and against the fact that the option holds *encoder names treated
   as available*, not backends. If no, say plainly whether
   `has_nvenc(codec, backend = "videotoolbox")` is an acceptable permanent
   export.

6. **(d) removal.** Per the second-escalation rule: should `nvenc_encoder()`
   be **unexported** (it is a one-line `paste0`, and M100 replaces its body
   with a table lookup), and should the availability helper be unexported too,
   leaving the option seam and `ffmpeg_encoders()` as the user-facing routes?
   Weigh against `vignettes/workflow.Rmd:79`, which teaches `has_nvenc()` as
   the check, and D044's GP1 trade, which accepted `refresh_ffmpeg_capabilities()`
   as a permanent export on the grounds that the option seam is not a refresh.

7. **Across the four.** Is there a consistency principle the four answers
   should share — e.g. "every session option has a per-call argument" or
   "never" — that the maintainer should record once, so the next such
   candidate is decided by rule rather than by gate? If so, state it in one
   sentence suitable for a D-entry.

## Constraints

- **D014's clean break stands** and is not replaced by a deprecation policy;
  do not propose `lifecycle` shims or a soft-deprecation period. Superseding
  D014 was declined at the M099 plan gate.
- **D014's vocabulary rules** (full-word compounds, `get_*` reservation,
  `verb_object` for task verbs, `ffm_*` for Layer 1 only) bind any new name.
- **IP1/D002/D003**: three layers, linear builder; a verb never glues its own
  command string. Nothing here should require a filtergraph DAG.
- **D024 / D034**: which probes may run on which paths is settled; (b) and (d)
  must not move a probe across that line.
- **D044**: the option seam is read before the memo on every call; a rename
  keeps that order.
- **M100's scope is fixed**: the `backend =` argument, the table, and the
  vocabulary widening are M100's. You are asked about names, not the design of
  the backend abstraction. If you believe the naming cannot be settled without
  changing M100's design, say so as a finding rather than redesigning it here.
- **Release timing is the maintainer's** (D050 plugin-side); do not recommend
  when 0.2.0 ships.
- Flag disagreement with any constraint explicitly rather than working around
  it.

## Output format

In `RR06-d014-rename-window.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason, and for every candidate a one-line disposition the
maintainer can record verbatim in a D-entry: **ship as `<names/shape>`** or
**declined permanently; reopened by `<evidence class>`**. Your report is
advisory: emit a `## Binding criteria` section ONLY if this brief's header slot
says `requested` (it does not).
