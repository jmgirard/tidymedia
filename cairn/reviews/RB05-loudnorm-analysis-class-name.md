# RB05: The shared class for the loudnorm analysis-pass diagnostic (M087)

- **Date:** 2026-08-29
- **Output required:** write findings to `cairn/reviews/RR05-loudnorm-analysis-class-name.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

tidymedia is an R package wrapping FFmpeg and MediaInfo for reproducible media
preprocessing. Most task verbs come in two forms: a scalar verb over one file
(`normalize_audio()`) and a batch sibling over a jobs tibble
(`normalize_audio_batch()`). Failures are signalled as R conditions carrying
`tidymedia_*` S3 classes, so a caller can write
`tryCatch(tidymedia_ffmpeg_exit = function(cnd) …)` or
`suppressWarnings(classes = "tidymedia_dropped_audio")`.

Two-pass loudness normalization runs an FFmpeg `loudnorm` **analysis pass** and
then a correction pass. When the analysis pass does not yield usable
measurements, both forms abort — but under different class names:

- The scalar form (`R/loudnorm_two_pass.R:151`) aborts with class
  `tidymedia_ffmpeg_exit` alone. It fires only when that one FFmpeg run exited
  non-zero, and carries the exit status on `tm_status`.
- The batch form (`R/loudnorm_two_pass.R:253`) aborts with class
  `tidymedia_loudnorm_analysis` alone. It fires for any row that failed to run
  **or** printed no parseable measurement block — including rows that exited
  **zero** — and carries `tm_rows` and `tm_row_status`.

The consequence a caller meets: a handler written on
`tidymedia_loudnorm_analysis`, which is what `?normalize_audio_batch` documents,
catches nothing at all on the scalar call. Milestone M087 exists to fix that.
M087's plan has already settled that the two sites should share **some** event
class; what it deliberately did not settle, and escalates here, is what that
shared class should be *called*, and whether the sharing decision itself
survives independent scrutiny.

The naming question is escalated because a condition class is exported surface a
caller matches on by literal string. Renaming one after release breaks handlers
**silently** — the handler simply never fires; R raises no error for a class name
that matches nothing. The package is pre-0.2.0 and pre-CRAN, so a clean break is
still available, but this is the last cheap moment to take it.

This is the second review convened on this package's condition-class naming. The
first (RB04/RR04) settled the name for a failed FFmpeg run and, in doing so,
established the convention recorded as D062. You are asked below to weigh
removal of the shared-class mechanism, not only its naming.

## Materials

Read these, in this order:

1. `cairn/reviews/archive/RR04-ffmpeg-error-class-name.md` — the prior review
   that established the naming convention. Its section 1 is the reasoning you
   are being asked to apply to a second case.
2. `cairn/DECISIONS.md`, entry `## D062` — the convention as recorded, including
   its "What this does not decide" paragraph and its falsifier clause. Locate it
   with `grep -n '^## D062' cairn/DECISIONS.md` and read to the next `## D0`
   heading.
3. `R/loudnorm_two_pass.R` — the whole file. The two abort sites are at lines
   151 and 253; the comment at 248-252 states the current reasoning for why the
   batch site does **not** use `tidymedia_ffmpeg_exit`.
4. `R/ffmpeg.R` lines 640-760 — the multi-track separation diagnostic, which is
   the package's one existing case of a single class name riding an error at one
   site and a warning at another. Line 681 is the error site (two classes today),
   line 742 the warning site (one class).
5. `R/ffm.R` lines 1540-1630 and `R/tidymedia-package.R` lines 110-125 — the
   user-facing prose that enumerates which paths raise which class.
6. `cairn/milestones/M087-scalar-batch-condition-classes.md` — the milestone this
   brief serves. Its AC1 is what your answer settles.

To see the classes as a caller does, from the repo root:

```
Rscript -e 'devtools::load_all("."); print(class(tryCatch(normalize_audio("nonexistent.wav", "out.wav", two_pass = TRUE), error = function(e) e)))'
```

## Questions

1. **Should these two sites share a condition class at all?** The scalar abort
   reports "this FFmpeg run exited non-zero"; the batch abort reports "one or
   more rows yielded no usable measurement", a superset that includes rows
   exiting zero. Under D062's rule that a class names the event, are these one
   event or two? Answer with the caller's handler in view: what does someone
   writing `tryCatch()` around each form actually want to catch?

2. **Removal option — should the shared class be dropped from the design
   entirely?** This is put to you explicitly because the sharing mechanism is on
   its second escalation. If the two sites report genuinely distinct events, the
   alternative is to add no shared class, leave each site's name as it is today,
   and fix the caller's problem purely in documentation — stating in both topics
   which name each form raises. Weigh that against the shared-class design. Say
   which you would ship, and what evidence would change your answer.

3. **If they should share a class, what should it be called?** The incumbent is
   `tidymedia_loudnorm_analysis`. Note that `loudnorm_analysis` names a
   processing *phase*, not an event — which reads as a departure from the
   convention RR04 established and D062 records. Weigh keeping it against
   renaming; if renaming, propose the name and justify it on RR04's own terms.
   Consider that the event being named must cover both a non-zero exit and a
   zero-exit run that printed nothing parseable.

4. **If they share a class, should either site keep a narrower class beside it?**
   That is: should both sites raise exactly the shared name, or should the class
   vector differ — for instance the scalar site raising the shared class plus
   `tidymedia_ffmpeg_exit` (which is true there and false on a mixed batch)?
   D062 leaves hierarchies open. Say what shape you would ship and why.

5. **Renaming cost.** If your answer to Q3 is a rename, address the silent-break
   risk directly: a handler on the old name stops firing with no error. The
   package is pre-0.2.0 with a recorded clean-break policy (D014), unreleased on
   CRAN. Is the rename worth taking now, or does the incumbent name's being
   already documented and already matched-on outweigh its being off-convention?

6. **Secondary — the record D062 is owed.** Separately from the loudnorm pair,
   the multi-track separation diagnostic now carries two classes at its error
   site (`R/ffmpeg.R:681`) and one at its warning site (`R/ffmpeg.R:742`).
   D062's falsifier clause names "one recorded event at two severities under two
   names" as the shape that would falsify it. M087 plans to record this by
   appending an entry annotating D062 rather than by restoring symmetry. Is
   recording the right disposition, or does this asymmetry warrant a change to
   the code — and if recording, what must the annotation say to be honest about
   what it concedes?

## Constraints

Fixed; flag disagreement explicitly rather than working around it.

- **D062's two-part rule stands** unless you argue explicitly against it: a
  condition class is `tidymedia_<event>`, naming the fact that occurred and never
  the severity; condition data fields carry the `tm_` prefix. Q3 asks you to
  apply this rule, not to relitigate it — but Q1 and Q2 may legitimately conclude
  that applying it yields two classes rather than one.
- **Out of scope: `ffm_batch()`'s per-row result contract (D007).** The batch
  runner reduces each row's outcome to logicals and discards the condition, which
  is why the batch-side warning cannot carry an exit status. Changing that is a
  separate milestone. Do not propose answers that require it.
- **Out of scope: sweeping every class into the ecosystem's `pkg_error_*`
  shape.** D062's "What this does not decide" paragraph declines this; it is a
  larger decision than this milestone.
- **Out of scope: the package's other unclassed `cli_abort()` sites.** There are
  159 of them; classing them is on the backlog, not here.
- A deprecation cycle is **not** required: the project is pre-1.0 and the
  clean-break policy (D014) is recorded. Cost still counts as an argument, but
  "we must deprecate first" is not a constraint you need to honour.

## Output format

In `RR05-loudnorm-analysis-class-name.md`: answer each question by number with
your reasoning and evidence, citing `file:line` for every claim about the code;
list any additional findings separately under "Beyond the brief"; end with
concrete recommendations, each marked apply / consider / reject-with-reason.
Your report is advisory: this brief's header slot says `not requested`, so do
**not** emit a `## Binding criteria` section.
