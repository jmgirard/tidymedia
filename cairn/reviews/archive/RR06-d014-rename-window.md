# RR06: Four API changes held open by the pre-0.2.0 clean-break window (M099)

- **Date:** 2026-09-01
- **Brief:** `cairn/reviews/RB06-d014-rename-window.md`
- **Advisory** — no binding criteria requested.

Everything below was checked against the branch `m099-d014-rename-window-review`
at its current HEAD, with `pkgload::load_all()` and the AC1 sweep re-run: 89
exports; `audio_stream` a formal on 18; `audio` on 6 (`compare_videos`,
`picture_in_picture`, their `_batch` siblings, `ffm_codec`, `ffm_copy`);
`hardware` on 16; `run` on 31; `check_tracks` and `timeout` on 0; the hardware
pattern grep over the export list returns exactly `has_nvenc` and
`nvenc_encoder`. Those numbers match the brief and M099's work log.

One observation frames all seven answers, so it comes first. Every candidate's
recorded falsifier is a **user report** ("a caller confused by", "a caller
needing", "a report that wrapping is the wrong ergonomics"). The package is not
on CRAN and has no user base to report anything. A falsifier that cannot fire
before the window closes carries no information when it has not fired, so "no
such report exists" is not evidence for declining; it is the absence of an
instrument. The four dispositions therefore have to be made on design grounds,
and the "reopened by" clauses below describe what would justify a deprecation
cycle *after* 0.2.0, not what would have justified a free rename before it.

## 1. (a) names

**`audio_stream`: decline the rename.** The name is right. It names what is
counted (a stream, FFmpeg's word, compiled to `-map 0:a:<n>`) with the full-word
compound D014 requires, and it is the pattern the ROADMAP's pending
`subtitle_stream` / `video_stream` selector row will follow. `audio_track` is
the friendlier word but would put the future `subtitle_stream` beside an
`audio_track`, or force `subtitle_track` on an argument that compiles `-map
0:s:<n>`; `audio_index` collides in the reader's mind with `probe_audio()`'s
`index` column, which counts every stream of every type — the exact confusion
`?audio_stream` already has to head off (`R/audio-stream-doc.R:165-168`);
`track` alone drops the compound and reads as the fan-in verbs' problem in
reverse.

**`audio` on the four fan-in exports: ship as `audio_input`.** This is the
one place the docs are carrying a naming defect rather than an inherent
subtlety. The two indices differ in *what is counted*, and one of the two names
says so while the other does not. `audio_stream = 1` announces its base;
`audio = 1` admits three readings at the call site — audio on (a truthy 1), the
second track, the second file — and only the third is right. The repair is the
same one the package already made for the other index: put the base in the
compound. `audio_input = 1` reads "take audio from input 1", and `input` is
already these verbs' own vocabulary: `compare_videos_batch()`'s jobs column is
`inputs`, and its `.f` is `function(inputs, output, ...)`
(`R/ffmpeg.R:7224`). `audio_input = NULL` → "no audio input" → the silent
output the verbs already produce, so the `NULL` reading needs no new prose.
D014 is satisfied (full-word compound; the argument vocabulary rule names
`infiles` for the file vector, and this is an index into it, not the vector).

Measured blast radius: four exported signatures (two scalar, two `_batch`), the
generated `audio_input_param()` text (one function, `R/audio-stream-doc.R:100`),
the `?audio_stream` topic's two paragraphs on `audio`, two abort hints naming
`{.arg audio}` (`R/ffmpeg.R:6668`, `:6821`, `:7156`), the batch column name at
the four `"audio"` string sites (`:7146-7207`, `:7402-7405`), the
`test-audio-index-docs.R` parameter enumeration, and 14 `audio =` call sites
across 11 test files. No `man/` edit is hand-written; `document()` regenerates.
D023's first bullet ("two names, two bases") and D032 are *strengthened*, not
superseded: the bullet argued against reusing one name for two bases, and this
gives the second base its own compound. D009's "expose an `audio =` index"
sentence becomes historical prose; it needs a one-line annotation, not a
supersession.

Why this overturns the session's decline rather than confirming it: D032's
closing argument — "a rename paid for by a headcount would be paid by every
existing caller" — is the argument for *after* 0.2.0. Before it, the caller
count is approximately the maintainer, which is what the window exists to
exploit. The argument-count trigger D032 correctly ruled out is not what is
firing here; what fires is that one of the two names fails the test the other
passes.

**Removal, per the second-escalation rule.** Do not remove `audio` from the
fan-in verbs: D009 made audio in stacked output explicit-map-only, so without
this argument a comparison or PiP with sound needs Layer 0. Do not touch
`ffm_codec(audio =)` or `ffm_copy(audio =)`. Those are Layer 1 `video`/`audio`
pairs mirroring FFmpeg's own `-c:v`/`-c:a` stream-type split; the word there
means "the audio stream type", the types differ (string, logical), and a Layer 1
caller is by definition reading FFmpeg semantics. Once the fan-in verbs say
`audio_input`, the `?audio_stream` section "`audio` names three things" shrinks
to two Layer 1 meanings that no longer share a word with any Layer 2 index.

*Reopened by:* a Layer 2 verb needing to select both an input and a track in
one call (a second fan-in verb that carries a chosen track from a chosen file),
which would need the two compounds to compose rather than coexist.

## 2. (a) `NULL`

**Decline unification permanently; the D032 documentation answer is
sufficient.** The two readings are not a convention that could have gone
either way; each is forced by what the verb writes. An extraction verb writes
one audio stream (`.wav`, `.mp3`), so its unselected case must pick one, and
"first" is the only non-heuristic pick (D023). A pass-through verb writes a
container, so its unselected case has a second answer, and D025/D026 measured
what choosing "first" there costs: three-track inputs silently narrowed to one
on verbs where the dropped-track warning does not run. Unifying toward
every-track is not possible on the extraction verbs at all (their product is
one stream), and toward first-track is the measured data loss. There is no
third direction.

Of the alternative mechanisms the brief lists, one deserves a specific
rejection because it looks clean: make the extraction verbs default to
`audio_stream = 0` and reserve `NULL` for every-track everywhere, so the two
defaults become visible in the signatures instead of hidden in `NULL`'s
reading. It fails on the diagnostic. D024's dropped-track probe fires on "the
caller made no selection" and is gated on `is.null(audio_stream)` at the four
scalar sites (`R/ffmpeg.R:574`, `:1356`, `:2634`, `:2679`) and on `NA` cells in
the batch column. With a `0` default the probe would need `missing()`, which
does not survive the `_batch` fan-out or any wrapper, or a sentinel that is
`NULL` under another name. A sentinel string (`"all"`) has the same problem
plus a type change on a numeric argument. A second argument or a per-family
name pays two signatures or two vocabularies to spell one concept D032 already
generates the docs for from one source. The generated `@param` machinery is the
right mechanism: the failure a second name would prevent (a block naming the
wrong siblings) is unrepresentable rather than merely documented.

*Reopened by:* a verb whose product could be either one stream or many
(extraction into a multi-track container where the caller wants all tracks),
which would be a third family rather than a unification and would need its own
reading.

## 3. (b) `check_tracks =`

**Decline permanently.** Three things make the eighteen signatures a poor
trade.

The per-call form already exists twice over. `withr::with_options(list(
tidymedia.check_tracks = FALSE), extract_audio_batch(jobs))` is one line, and
the warning itself carries class `tidymedia_dropped_audio` (`R/ffmpeg.R:412`),
so `suppressWarnings(..., classes = "tidymedia_dropped_audio")` is the
per-call form for a caller who wants silence and does not care about the
probe's cost — and six help topics already teach it.

The per-row grain, which is the one thing no option can express, is already
covered by the argument that exists: a row naming an `audio_stream` is never
probed (`warn_dropped_audio_batch()` returns above the seam when every row
named a track, `R/ffmpeg.R:447`). So (b)'s residual is smaller than (c)'s
and needs nothing.

The "two answers inside one script" case is realistic — a known-tracks batch
followed by unknown-tracks singles — but it is exactly what `with_options()`
around the batch is, and D060 has already declined the `with_*`/`local_*` pair
for this seam on the ground that a logical needs no pre-validation. Shipping
`check_tracks =` at 18 verbs would, by any consistency principle worth
recording, obligate `timeout =` at 31 (see Q7) and a `hardware_encoders =` at
16, since all three seams are read on the same paths. The ROADMAP row's
half (b) (probing lazily inside the fan-out) is a separate performance question
and stays a candidate.

*Reopened by:* a caller needing the check's answer to vary per row for rows
that do *not* name a track — which the column form of `audio_stream` cannot
express and which would be a `jobs` column, not a scalar argument.

## 4. (c) `timeout =`

**Decline the scalar argument permanently, on the 31 verbs and on
`ffm_run()` / `ffm_batch()`.** D047's costing stands and D051 removed its only
falsifier. Three additional points the brief's materials support:

A scalar argument cannot express the one residual D051 left. "A limit that
varies per row inside one batch" is a per-row value; `timeout = 600` on
`extract_audio_batch()` is the same grain as `with_timeout(extract_audio_batch(
...), 600)`. So the argument would ship 33 signatures without touching the
case that is its justification.

A `timeout` column in the `jobs` tibble is the right *shape* for that residual
and is the batch verbs' existing idiom for per-row values (`audio_stream`,
`audio`, `video_codec` all read a column overriding the scalar). Its mechanism
is not free, though: `ffm_batch()` runs `run_one(pipeline)` over pipeline
objects (`R/ffm_batch.R:139-145`), and the limit is read at the spawn site from
the option, so a per-row limit would have to travel *on the pipeline* — an
`ffm_*` field set by `.f` and honoured by `ffm_run()`, which is a Layer 1
design call, not a column alone. That is worth recording as the shape so the
next person does not reach for a scalar, but it is not worth building absent
a request; a caller with per-row limits today splits the batch.

A per-verb argument also introduces semantics the seam does not have: which
wins when both are set, and what `timeout = NULL` means beside
`options(tidymedia.timeout = NULL)`, which D051 records as *removing* the
option. `with_timeout()`'s `seconds` already answered that once; a second
spelling (`timeout`) of the same value is the vocabulary drift D014 retired
elsewhere.

*Reopened by:* the per-row case only, and the answer then is the `jobs` column
carried on the pipeline object, never a scalar argument.

## 5. (d) names

**Ship the rename.** `has_nvenc(codec, backend = "videotoolbox")` is not an
acceptable permanent export. A function named for one member of a vocabulary
answering for another member is the same defect D014 retired in
`audio_as_mp3()` ("no verb hard-codes a fixed format/codec in its name") and in
`acodec`; it is the rule already on the books, arriving in the capability
family. The option is worse: `options(tidymedia.nvenc_encoders =
"h264_videotoolbox")` is a sentence that is false on its face, and it is the
seam carried into every parallel worker and documented in three topics. This
is not a judgement call between two adequate names; it is D014 applied.

Names, against the alternatives:

- **`has_hardware_encoder(codec, ...)`** — apply. Full-word compound; `has_*`
  is the predicate shape the package already uses and is not `get_*`, so
  D014's reservation is untouched. `has_hw_encoder` is the abbreviation D014
  retired `acodec` for; reject.
- **`hardware_encoder(codec, ...)`** — apply. It pairs with the argument it
  serves (`hardware = "nvenc"` at 16 verbs ↔ `hardware_encoder(..., "nvenc")`),
  and with `has_hardware_encoder()` the two names differ by exactly the word
  that says which one asks the machine. `hardware_encoder_name` is more literal
  about the return type but the existing `nvenc_encoder()` returned a name
  under the shorter shape without confusion, and the `@return` says so.
  `encoder_name` loses the word that scopes it to the hardware table: a user
  would expect `encoder_name("libx264")` to answer.
- **`tidymedia.hardware_encoders`** — apply. It holds hardware-encoder names
  treated as available, so the word is accurate, and a user who has just read
  `has_hardware_encoder()` finds the seam by the same word. `tidymedia.encoders`
  overclaims: `ffmpeg_encoders()` never reads the option (D044 keeps it
  uncached and unoverridden), so an option of that name would appear to govern
  a function it does not. `tidymedia.available_encoders` is accurate and is the
  acceptable runner-up; it loses only the pairing.

D044's read order is unaffected by a rename; `nvenc_available()` reads the
option before the memo and keeps doing so under the new string.

One finding the naming *does* depend on, stated as the brief asks rather than
redesigned: M100 plans `backend =` as the helper's argument while the 16 verbs
spell the same vocabulary `hardware =`. That is the (a) defect in miniature —
one value (`"nvenc"`, `"videotoolbox"`) under two argument names — and the
verbs' `@param hardware` already says "The encoder backend" (`R/ffmpeg.R:1008`,
`:1425`, `:1544`), so the two words are already synonyms in the docs. The helper
should take `hardware =`, giving `has_hardware_encoder("h264", hardware =
"videotoolbox")`. That argument does not exist yet, so choosing its name is
inside M100's own scope and costs nothing; it should be settled at M100's plan
gate, not left to its implementation.

*Reopened by:* a third capability family (hardware *decoders*, `-hwaccel`)
that needs a parallel predicate, at which point `has_hardware_encoder` /
`has_hardware_decoder` is the shape the names were chosen to allow.

## 6. (d) removal

**Keep both exported, under the new names.** The second-escalation rule was
weighed as a real option; here is why each survives it.

`has_hardware_encoder()` earns its export on D044's own reasoning. The option
seam is not a query — it requires the caller to already know the encoder names,
and the alternative pre-flight check, `"h264_videotoolbox" %in%
ffmpeg_encoders()$name`, makes the user hand-assemble FFmpeg's name and spawns
a process the memo exists to avoid. The vignette teaches the predicate as the
check (`vignettes/workflow.Rmd:79`) and `skip_if_no_nvenc()` builds on it
(`tests/testthat/helper-skip.R:35`). Unexporting would push the one honest
"will `hardware = "videotoolbox"` work here?" question to the escape hatch.

`hardware_encoder()` is the weaker case and survives on two uses, not on its
size. First, it is the pure half of the pair — the one function on the
hardware path that runs no binary — so it is the tool for composing the seam
without hand-typing names: `options(tidymedia.hardware_encoders =
hardware_encoder("h264", "videotoolbox"))`, which is how a CI configuration or
a shared script declares a known environment. Second, once M100 replaces the
`paste0` with a table, this is the only exported view of that table — which
families each backend covers — and its help topic is where that table is
documented. `run = FALSE` also reveals the chosen encoder in the compiled
command, but only after the caller has built a whole pipeline to ask. A
one-line body is not an argument against an export; `has_nvenc()` was two
lines. Against GP1: the rename is already the break, so keeping the export
adds nothing to what 0.2.0 commits beyond the name itself.

If the maintainer weighs GP1 more heavily than I do, unexport
`hardware_encoder()` and keep the predicate; do not unexport the predicate.

## 7. Across the four

There is a principle, and it is two clauses because (a)/(d) and (b)/(c) are
different questions. For a D-entry:

> **An exported name carries the category and never one member of an open
> vocabulary (a backend, a codec, a container), and a compound argument name
> states what it counts; a session option never grows a per-call argument on
> the verbs — the per-call grain is a `with_*()`/`local_*()` pair, added only
> when the option's value must be refused before it is set (D052), and a
> per-row grain, if ever needed, is a `jobs` column carried on the pipeline,
> never a scalar argument.**

The first clause is D014's existing "no verb hard-codes a format in its name"
rule, widened from task verbs to every export and argument, so (d) is
recorded as an application of D014 rather than a new call and (a)'s
`audio_input` follows from the same sentence. The second clause is D047 + D051
+ D060 stated once, so the next seam (the `find_ffmpeg()` memo, a fourth
option) is decided by rule. Recording "never" is safe because the two
exceptions the package has already found — `with_timeout()` for a value that
must be validated, and per-row control via the `audio_stream` column — are both
inside the sentence.

A second, procedural principle worth one line in the same entry: **before
0.2.0, a candidate whose only falsifier is a user report is decided on design
grounds, since the falsifier cannot fire.** All four rows here carried
unfireable falsifiers and were declined at gates partly on their silence.

## Beyond the brief

1. **M100's `backend =` vs the verbs' `hardware =`** — covered under Q5; the
   one place where naming and M100's design meet. Settle at M100's plan gate.
2. **Rename mechanics for (d).** `tidymedia.nvenc_encoders` has ~150 test
   sites, 4 `R/` roxygen sites, 3 `man/` topics, none in `vignettes/` or
   `_pkgdown.yml`; `has_nvenc(` has 37 test sites and 31 roxygen mentions;
   `nvenc_encoder(` 9 test sites. All are mechanical string replacements; AC1's
   two-grep procedure (new-name pattern returns sites, old-name pattern returns
   nothing outside `NEWS.md`) is the right acceptance test. `NEWS.md`'s
   historical mentions stay, per AC1. `_pkgdown.yml:118-123` needs the section
   prose ("opt-in NVIDIA nvenc GPU encoding") reworded as well as the two
   `contents` rows swapped; M100 AC6 already lists it, but the row swap is
   M099's since it is the rename.
3. **Rename mechanics for (a).** `audio_input` touches the `?audio_stream`
   topic (`R/audio-stream-doc.R:155-238`), `audio_input_param()`, three abort
   hints, the batch column string at eight sites, `test-audio-index-docs.R`'s
   enumeration, and 14 test call sites. D009 and D023 need one-line
   annotations that `audio` is now `audio_input`; D032 gains a re-confirmation
   line, not a supersession. `_pkgdown.yml` is unaffected (no object renamed).
4. **`codec_family()`'s abort text hard-codes nvenc** (`R/ffmpeg.R:3072-3082`)
   and so does `check_nvenc_available()`'s (`:3180-3186`). M100 T3/T4 own these;
   noted so the (d) rename does not stop at the exported names and leave the
   internal helper names (`nvenc_available`, `check_nvenc_available`) as the
   last nvenc-only vocabulary. Internal names are not D014's concern, but
   `nvenc_available()` returning TRUE for a videotoolbox encoder is a comment
   waiting to be wrong.
5. **`?tidymedia`'s Session options section** (`R/tidymedia-package.R:189`)
   says the option "Names the NVIDIA hardware encoders this machine has". Under
   M100 that sentence describes the pool for every backend; M100 AC6's domain
   is the three `man/` topics that grep the option and would catch this, but
   the sentence should be rewritten with the rename, not after it.
6. **`refresh_ffmpeg_capabilities()`'s `@seealso`** (`R/cache.R:63-64`) links
   `has_nvenc` and `nvenc_encoder` by name; roxygen will fail `document()` on
   the stale links, which is the desired backstop, noted so it is not read as a
   regression.

## Recommendations

| # | Recommendation | Disposition |
|---|---|---|
| R1 | Rename `audio` → `audio_input` on `compare_videos`, `picture_in_picture`, and their `_batch` siblings; leave `audio_stream`, `ffm_codec(audio=)`, `ffm_copy(audio=)` | **apply** |
| R2 | Leave the two `NULL` readings and the generated-doc mechanism as they are; reject the `0`-default unification on the diagnostic-gating ground | **apply** (decline the change) |
| R3 | Do not add `check_tracks =`; the ROADMAP row's half (b) stays a candidate | **apply** (decline) |
| R4 | Do not add `timeout =` anywhere; record the `jobs`-column-on-the-pipeline shape as the answer to the per-row residual | **apply** (decline) |
| R5 | Rename to `has_hardware_encoder()`, `hardware_encoder()`, `tidymedia.hardware_encoders` | **apply** |
| R6 | M100's helper argument spelled `hardware =`, not `backend =` | **apply** at M100's plan gate |
| R7 | Keep both hardware helpers exported | **apply**; unexporting `hardware_encoder()` is the acceptable minority position |
| R8 | Record the Q7 two-clause principle and the "unfireable falsifier" rule in one D-entry | **apply** |
| R9 | `tidymedia.available_encoders` as the option name | **reject-with-reason**: accurate, but loses the pairing with the predicate's word; runner-up if `hardware_encoders` is refused |
| R10 | `has_hw_encoder`, `encoder_name`, `tidymedia.encoders` | **reject-with-reason**: abbreviation (D014); loses the hardware scope; overclaims over `ffmpeg_encoders()` |
| R11 | A generic `has_encoder(name)` over the whole pool | **consider**, later and additive — it does not replace the family-mapped predicate the verbs' interface calls for, and needs no rename window |

### Dispositions for the D-entry, verbatim

- **(a) names, `audio_stream`:** declined permanently; reopened by a Layer 2
  verb that must select both an input and a track in one call.
- **(a) names, `audio`:** ship as `audio_input` on `compare_videos`,
  `compare_videos_batch`, `picture_in_picture`, `picture_in_picture_batch`;
  `ffm_codec(audio =)` and `ffm_copy(audio =)` unchanged.
- **(a) `NULL`:** declined permanently; reopened by a verb whose product can be
  one stream or many, which would be a third family rather than a unification.
- **(b):** declined permanently; reopened by a caller needing the check to vary
  per row for rows that name no track — a `jobs` column, never a scalar
  argument.
- **(c):** declined permanently; reopened by a caller needing a limit that
  varies per row within one batch — a `jobs` column carried on the pipeline
  object, never a scalar argument.
- **(d) names:** ship as `has_hardware_encoder()`, `hardware_encoder()`,
  `tidymedia.hardware_encoders`; M100's helper argument spelled `hardware =`.
- **(d) removal:** declined permanently (both stay exported); reopened by a
  measured report that `hardware_encoder()` is used only through the
  predicate, at which point GP1 unexports it under a deprecation cycle.
