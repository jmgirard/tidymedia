# RB02: May a verb run a binary on its executing path purely to emit a diagnostic? (M44)

- **Date:** 2026-07-30
- **Output required:** write findings to `cairn/reviews/RR02-binary-on-executing-path.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**tidymedia** is an R interface to FFmpeg/MediaInfo for *reproducible media
preprocessing in research/data-science pipelines* — deliberately not "all of
ffmpeg in R" (D001). It has three layers (D002):

- **Layer 0** — raw CLI passthrough (`ffmpeg()` / `ffprobe()` / `mediainfo()`).
- **Layer 1** — the `ffm_*` pipe builder (`R/ffm.R`, `R/ffm_oop.R`): all FFmpeg
  command assembly, quoting, and copy-vs-re-encode logic lives here once.
  Deliberately linear (IP2).
- **Layer 2** — task verbs (`extract_audio()`, `crop_video()`, …) as thin
  wrappers that compute arguments and never glue their own command strings
  (IP1).

Every Layer-2 transform verb takes a `run` argument. `run = TRUE` (the default)
compiles the command and executes FFmpeg; `run = FALSE` compiles and returns the
command string without executing anything. This is a headline feature of the
package: a caller can inspect, log, or save the exact reproducible invocation
before committing to it, and the vignette and every roxygen `@examples` block
rely on it working with no binary present.

`cairn/DESIGN.md`'s Conventions section states:

> Command **compilation** is pure and CI-safe (no binaries); command
> **execution** tests `skip_if` the ffmpeg/mediainfo binaries are absent (D004).

**The one existing exception.** D013 (2026-07-12, from M16) carved a hole in
that convention for `normalize_audio(two_pass = TRUE)`. Two-pass loudness
normalization must run an FFmpeg *analysis* pass, parse its stderr for measured
values, and feed those values into a *correction* command. There is no
correction command without the analysis, so the binary must run before the final
command exists. D013 placed that orchestrator beside `ffm_run()` (in
`R/loudnorm_two_pass.R`), kept `ffm_compile()` pure, and recorded the
consequence that `run = FALSE` "no longer guarantees a binary-free call" under
`two_pass = TRUE`.

**The milestone this brief serves (M44, in progress).** Two audio verbs —
`extract_audio()` and `convert_audio()`, plus their `_batch` siblings — take
exactly **one** audio track out of an input. Since a prior milestone (M43) they
compile an explicit `-map 0:a:<n>`, defaulting to the first audio track, with an
`audio_stream` argument to name a different one (D023). The consequence: a
caller who feeds a three-track file to `extract_audio()` and names no
`audio_stream` silently loses two tracks. M44 exists to warn them.

Detecting that condition requires counting the input's audio streams, which
requires running **FFprobe**. Nothing about the compiled FFmpeg command changes
as a result — the probe's only effect is whether a warning is emitted.

**Why this needs independent review.** M44's implementing session drafted a
decision entry (D024, reproduced verbatim below) that does not merely add a
second exception but *reframes* the convention: it asserts the protected
boundary was always compilation rather than the verb, and that a binary may run
on a verb's executing path when its result does not enter the compiled command.
That reframing touches IP1 (the three-layer separation) and rewrites how a
standing D-entry is read. It also sets the precedent for at least three queued
pieces of work that will want to probe inputs (see Q6). A durable, independent
verdict is wanted before any probe code lands.

## The drafted decision entry under review

This is the exact text proposed for `cairn/DECISIONS.md`. Your review is of this
text and the rule it states.

```markdown
## D024 — Which paths may run a binary while building a command (2026-07-30, from M44, extends D013)

DESIGN.md's Conventions line reads: "Command **compilation** is pure and CI-safe
(no binaries); command **execution** tests `skip_if` the ffmpeg/mediainfo
binaries are absent (D004)." D013 qualified it once, for
`normalize_audio(two_pass = TRUE)`, whose analysis pass must run FFmpeg before
the correction command can be built. M44 needs it qualified again, and in a
direction D013 did not reach: counting an input's audio tracks needs FFprobe,
and the count decides only whether to *warn* — no compiled command changes.
This entry states the rule both carve-outs are instances of.

- **The boundary is compilation, not the verb.** `ffm_compile()` and every
  builder it walks stay binary-free, and `run = FALSE` on every verb in the
  package returns a compiled command with no binary having run. That is what
  the convention protects and it is unchanged: a caller can inspect any command
  on a machine with no FFmpeg, in CI, in a vignette chunk. What the convention
  never said — and what D013 read into it by carving an exception rather than
  drawing the line — is that a verb's *executing* path is equally pure. It is
  not, and never was: `run = TRUE` runs FFmpeg by definition.

- **A binary may run on the executing path when its result is not in the
  command.** M44's probe is the case D013's wording does not cover. D013 runs
  FFmpeg *to build a later command* — the measured values end up in the
  correction pass, so that analysis is part of compilation in everything but
  name, which is why it needed a carve-out at all. M44 runs FFprobe *to decide
  whether to warn*: the compiled command is byte-identical whether the probe
  runs, succeeds, fails, or never happens. So the carve-out widens on two axes
  at once — from one verb (`normalize_audio`) to four (`extract_audio`,
  `convert_audio`, and both `_batch` siblings), and from command-building to
  diagnostics.

- **A diagnostic probe fails silently; a command-building probe aborts.** The
  two carve-outs differ in what a missing or broken binary costs. D013's
  analysis pass has no correction command without it, so an absent FFmpeg or an
  unparseable measurement aborts. M44's probe has a working command either way,
  so an absent FFprobe or an unreadable input skips the warning and runs — a
  diagnostic that aborts the job it was meant to annotate is worse than no
  diagnostic. Rules out making the probe a hard requirement of the audio verbs,
  which would make FFprobe a runtime dependency of `extract_audio()`.

- **What stays ruled out.** A probe on the `run = FALSE` path, on any verb,
  including "just to warn earlier" — `run = FALSE` is the package's pure surface,
  and the roxygen `@examples` and the vignette both compile commands with no
  binary check gating them. Folding either probe into `ffm_compile()` or into a
  builder (D013's original exclusion, restated because widening a carve-out
  makes it tempting). And a probe on any verb whose output keeps every audio
  stream: this is licensed for the verbs that narrow a multi-track input to one
  track, not as a general permission to probe.
```

A companion one-line change to `cairn/DESIGN.md`'s Conventions section is
proposed alongside it:

```
- Command **compilation** is pure and CI-safe (no binaries); command
  **execution** tests `skip_if` the ffmpeg/mediainfo binaries are absent (D004).
  D013/D024 qualify what the *executing* path may run before the command.
```

## Materials

Read these (all paths repo-relative):

- `cairn/DECISIONS.md` — **D013** (lines 134–148, the existing carve-out under
  review) and **D023** (lines 500–553, the `audio_stream` selector M44 builds
  on). Also **D002**, **D004**, **D011** for context. The file is append-only;
  scan its `### D-`/`## D` headings rather than reading it whole.
- `cairn/DESIGN.md` (97 lines, read whole) — the Conventions section and the
  IP/GP principle block. IP1 is the three-layer separation.
- `cairn/milestones/M44-implicit-track-drop-warning.md` — the milestone's goal,
  scope, acceptance criteria and tasks. Its AC2 is the purity proof; its AC3 is
  the fail-silently requirement.
- `R/loudnorm_two_pass.R` (339 lines, read whole) — D013's orchestrator as
  built, especially `run_loudnorm_analysis()` (lines 105–124, the scalar
  analyze step that runs the binary and aborts on failure) and
  `run_loudnorm_analysis_batch()` (lines 136–158, the batch form).
- `R/ffmpeg.R`, specifically:
  - `audio_stream_map()` — lines 250–268, and the design comment above
    `extract_audio_pipeline()` at 271–303.
  - `extract_audio()` — lines 336–357; `convert_audio()` — lines 577–615.
  - `extract_audio_batch()` — lines 3567–3620; `convert_audio_batch()` —
    lines 3673–3745. Note that both build every row's pipeline through
    `ffm_batch()` and that their per-row `audio_stream` may arrive from a jobs
    column rather than the argument (`batch_stream_cell()`, line 3438;
    `check_batch_audio_col()`, line 3419).
- `R/ffm.R`: `ffm_run()` (line 1308) and `ffm_finish()` (lines 1360–1370) — the
  shared tail every Layer-2 verb calls, where `run` is honored.
- `R/ffm_batch.R` (231 lines, read whole) — note in particular that
  `ffm_batch()` builds **every** row's pipeline (lines ~105–110) *before*
  running any of them, and that `run` gates only the running.
- `R/ffprobe.R` (261 lines, read whole) — `probe_all()` (line 60),
  `probe_one()` (line 110, which builds an FFprobe token vector directly rather
  than through any builder), `probe_audio()` (line 194), and the `filter_streams()`
  resilience contract.
- `R/program_management.R`: `find_ffprobe()` (line 78) and `run_program()`
  (lines 108–150) — the shared shell-out, which **aborts** when the binary is
  absent.

Two decisions are already settled by the maintainer at M44's implementation gate
and are *not* open (they are context for your answers, not questions):

- The count will come from a **narrow one-shot FFprobe call**
  (`-select_streams a -show_entries stream=index`), not from `probe_audio()`,
  because a failed probe then simply returns nothing and `probe_all()`'s own
  "could not probe" warning never has to be suppressed.
- The `_batch` verbs will emit **one aggregated warning naming every affected
  row**, not one warning per row.

You do not need to run anything, but if you want to sanity-check the FFprobe
stream numbering: on a file with one video stream followed by three audio
streams, `probe_audio()`'s `index` column reads `1, 2, 3` (absolute stream
indices) while `audio_stream` takes `0, 1, 2` (position among audio streams).

## Questions

1. **Is a carve-out needed at all?** The null hypothesis is that D013 was
   over-cautious and D024 is unnecessary. The package already exports
   `probe_all()`, `probe_audio()` and `get_*()` scalars, all of which run
   FFprobe from Layer 2 as their whole purpose, and nobody considered them
   violations. On that reading, "command compilation is pure" is a claim about
   `ffm_compile()` and the `ffm_*` builders only — it never governed what a verb
   does around them — so M44's probe needs no new entry, and D013's real content
   was the narrower point that `run = FALSE` stops guaranteeing a binary-free
   call. Is that reading correct? If it is, should D024 be written at all, and
   if so should it be a *clarification* of an over-broad convention rather than
   an *extension* of a carve-out?

2. **Is "the result is not in the command" the right discriminator?** D024's
   central move is to distinguish a probe whose output enters the compiled
   command (D013's, which must abort on failure) from one that only decides
   whether to warn (M44's, which must fail silently). Does that line hold as a
   durable rule, or does it break down? Consider in particular: a probe that
   decides whether to *abort* rather than warn; a probe whose result changes a
   default the caller did not set; and a future probe that decides between two
   commands. Where exactly does the licence stop, and does D024's wording stop
   it there?

3. **The batch verb's ordering problem (most important technically).**
   `ffm_batch()` builds every row's pipeline before running any of them, and the
   verb has no hook inside the run loop. So M44's batch probe must run in the
   Layer-2 batch verb *before* `ffm_batch()` is called — that is, **before any
   compilation happens at all**, gated only by `run = TRUE`. Is a binary that
   runs before compilation, on a call whose `run` is `TRUE`, still "on the
   executing path" in D024's sense, or does this collapse the distinction D024
   is built on? If it does collapse it, what is the right structure: probe
   inside `ffm_batch()`'s run loop via a new hook (changing a shared engine
   contract for one diagnostic), probe up front in the verb and accept that
   framing, or something else? Note the cost profile: probes are all incurred up
   front, and a prior measurement put a full `probe_all()` at ~1.2 s per input
   (the narrow call should be materially cheaper, but the up-front concentration
   is unchanged).

4. **Where does the probe helper live under IP1?** `probe_one()` (`R/ffprobe.R:110`)
   builds an FFprobe token vector directly and hands it to `run_program()` —
   FFprobe argument assembly has never gone through the `ffm_*` builder, which
   models FFmpeg pipelines. Is a second such helper, added for M44 and called
   from four Layer-2 verbs, consistent with IP1's "Layer 2 verbs never glue
   their own command strings", or does IP1 require it to sit in `R/ffprobe.R`
   beside `probe_one()` (or in its own file, as D013's orchestrator got
   `R/loudnorm_two_pass.R`)? State the placement you would require.

5. **Does the silent-failure disposition have a hidden cost?** D024 requires
   that an absent FFprobe or an unreadable input skips the warning and runs.
   The consequence is a warning that is *silently unreliable*: on a machine
   without FFprobe, a caller loses tracks with no warning and no indication that
   the check did not run — arguably worse than a check that never existed,
   because its presence invites reliance. Is silent skipping right? Alternatives
   include a one-time-per-session message that the check is unavailable, or
   documenting FFprobe as a soft requirement of these verbs. Weigh this against
   M44's own plan note that "a silent-drop incident where a warning was present
   and still missed" would falsify the whole warning approach.

6. **Scope of the licence, given what is queued behind it.** D024 currently
   names four verbs and adds a predicate ("verbs that narrow a multi-track input
   to one track"). Three pieces of queued work will want to read it: (a) a
   ROADMAP candidate carrying `audio_stream` to four pass-through verbs
   (`standardize_video`, `crop_video`, `segment_video`, `anonymize_video`);
   (b) M45, which gives `separate_audio_video()` a multi-track escape and where
   `NULL` will mean *every* track; (c) a candidate noting that a failed
   `separate_audio_video()` leaves a zero-byte output file. Should D024 be
   written as an enumerated licence (these four verbs, extended by future
   entries), as a predicate, or as a general rule with named exclusions? Which
   formulation is least likely to be misread by the milestone that reads it next?

7. **Anything wrong with the drafted text as a durable record.** Independent of
   the substance: does the D024 text above misstate D013, overclaim, bury its
   operative rule, or fail to record what it rules out? Quote and correct any
   passage you would change. If you would reject the entry outright, say so and
   state what should replace it.

## Constraints

Fixed; do not relitigate (flag disagreement explicitly rather than working
around it):

- **IP1 (D002):** the three-layer separation. FFmpeg command assembly stays in
  Layer 1. You may rule on where an FFprobe helper belongs (Q4), but do not
  propose collapsing the layers.
- **IP2 (D003):** the builder stays linear; no filtergraph DAGs. M44 adds no
  engine capability.
- **D023:** `audio_stream` is a 0-based index among one input's audio streams,
  `NULL` means the first track, and the verbs compile an explicit `0:a:<n>` on
  every call. The selector's shape is settled; M44 only warns about the case
  where the caller did not use it.
- **M44's plan is fixed on warning, not erroring:** a multi-track input is legal
  input and `audio_stream` is the caller's resolution. Do not propose making an
  implicit drop an error.
- **M44's scope excludes any probe on the `run = FALSE` path** and excludes
  warning about dropped video or subtitle streams. If you believe either
  exclusion is wrong, say so as a finding — do not build a recommendation that
  assumes it away.
- The two maintainer-settled implementation choices listed at the end of
  Materials (narrow FFprobe call; one aggregated batch warning) are not open.
- **Pre-0.2.0, clean-break rename policy (D014):** no `lifecycle` shims. New
  arguments are additive and fine.

## Output format

In `RR02-binary-on-executing-path.md`: answer each question by number with your
reasoning and evidence (cite `file:line` where it matters); list any additional
findings separately under "Beyond the brief"; end with concrete recommendations,
each marked apply / consider / reject-with-reason. Where findings bind
implementation, also emit a `## Binding criteria` section: numbered `BC1…`, each
a measurable assertion checkable against evidence (a passing test, a compiled
command string, a file that exists), with any numeric projection stating its
tolerance. These are ingested VERBATIM into M44's acceptance criteria and
mechanically diffed against this file; departures are legal only through M44's
shown "Deviations from RR02" table. Keep binding criteria to what genuinely must
hold — over-constraining the plan is itself a cost, and M44 already carries six
acceptance criteria.

If your verdict is that no new decision entry is warranted (Q1's null
hypothesis), say so plainly and state what, if anything, should be recorded
instead — a "reviewed, no entry needed" outcome is a legitimate result here.
