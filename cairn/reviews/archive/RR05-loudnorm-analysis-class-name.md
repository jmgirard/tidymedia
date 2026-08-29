# RR05: The shared class for the loudnorm analysis-pass diagnostic (M087)

- **Date:** 2026-08-29
- **Brief:** cairn/reviews/RB05-loudnorm-analysis-class-name.md
- **Reviewer:** independent Fable-level review
- **Status:** advisory (no binding criteria requested)

Observed class vectors, from executing both forms against a non-media file
(the brief's `nonexistent.wav` probe never reaches FFmpeg — `normalize_audio()`
refuses an unreadable input at `R/ffmpeg.R:2240` with an unclassed abort, so a
garbage file that exists is the probe that actually exercises the two sites):

- scalar: `c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")`,
  `tm_status = 183L`
- batch: `c("tidymedia_loudnorm_analysis", "rlang_error", "error",
  "condition")`, `tm_rows = 1L`, `tm_row_status = 183L`

## 1. Should the two sites share a condition class?

**Yes.** Under D062's rule the class names the fact that occurred, and one fact
is common to every raising path of both sites: **the loudnorm analysis pass
yielded no usable measurement, so no correction could be built.** The scalar
abort (`R/loudnorm_two_pass.R:144-155`) reports that fact *plus* a second,
narrower one — the mechanism, a non-zero FFmpeg exit. The batch abort
(`R/loudnorm_two_pass.R:240-260`) reports only the common fact, because its
rows mix mechanisms (non-zero exits and zero exits with nothing parseable,
`R/loudnorm_two_pass.R:227-234`). These are not two events; they are one event,
with an extra fact established at the scalar site. Condition classes are
vectors precisely so a site can assert every fact that is true there — the
package already does this at `R/ffmpeg.R:681`.

With the caller's handler in view: someone wrapping either form writes the same
recovery — "this input could not be measured; log it, skip it, fall back to
single-pass." Nothing about that recovery differs by form; only the condition
fields do (`tm_status` versus `tm_rows`/`tm_row_status`), and fields are what a
handler branches on after the class has matched. Forcing that caller to
enumerate two class names for one recovery is the cost RR04 §4 refused to
impose in the other direction.

One fact the brief's framing understates: the batch class's extension already
has a second scalar counterpart. A scalar analysis run that exits **zero** but
prints no parseable block aborts in `parse_loudnorm_measurements()` at
`R/loudnorm_two_pass.R:112-118` — today with **no class at all**. So the
divergence is not merely ":151 spells the event one way and :253 another"; part
of the event's scalar extension is currently uncatchable by any name. Only a
shared class fixes that (see Beyond the brief, B1); documentation cannot.

## 2. The removal option

**Ship the shared class; reject the docs-only alternative.** Three reasons:

- The docs-only fix leaves the failure mode it is meant to cure. A handler
  written against one form and moved to the other — the exact migration the
  scalar/`_batch` sibling design invites — stops firing silently. Documenting
  the trap does not spring it any less; the brief's own argument about silent
  breakage (a class name that matches nothing raises no error) applies with
  full force to the *current* state, not only to a rename.
- Docs-only cannot reach `R/loudnorm_two_pass.R:112`: a zero-exit unparseable
  scalar run has no class to document. "Fix it purely in documentation" would
  mean documenting that one slice of the event is uncatchable.
- The answer to Q1 is that these are one event; D062 then *requires* one name
  for it. Keeping two names would need the Q1 answer to be "two events", which
  the handler analysis above does not support.

What would change my answer: evidence that callers of the two forms need
different recoveries for this event (none exists — the correction pass is
unreachable either way), or a caller population that only ever dispatches on
the mechanism (`tidymedia_ffmpeg_exit`) — impossible to serve batch-side, since
that class would be false for a zero-exit row (see Q4).

## 3. What should the shared class be called?

**Rename to `tidymedia_loudnorm_no_measurement`. Retire
`tidymedia_loudnorm_analysis`.**

On RR04's own terms, the incumbent fails the §1b test — *the name must describe
the event, not the category* — in exactly the way `tidymedia_ffmpeg_error`
failed it. `loudnorm_analysis` names a phase, and a caller who writes
`tryCatch(tidymedia_loudnorm_analysis = ...)` will read it as "anything that
goes wrong during the loudnorm analysis" and be silently wrong about three
neighbors: a timeout during the analysis run aborts as `tidymedia_timeout`
inside `run_program()` before any status exists; a missing FFmpeg binary aborts
unclassed in `run_program()`; and a silent input aborts unclassed at
`R/loudnorm_two_pass.R:103-110`. That is the same shape as RR04's "any FFmpeg
failure" trap, with the same silent consequence.

The event the class actually marks — at a non-zero exit, and at a zero exit
that printed nothing parseable — is precisely *no usable measurement was
obtained*. `tidymedia_loudnorm_no_measurement` states that fact, and it earns
its keep at the boundary that matters most: **silence**. A silent input *was*
measured — `classify_loudnorm_output()` returns `input_i = -inf` as a
recognized outcome, not a parse failure (`R/loudnorm_two_pass.R:79-85`), and
the batch continues-and-marks it rather than raising this class
(`R/loudnorm_two_pass.R:262`). A name meaning "no measurement" truthfully
excludes silence; a name meaning "the analysis phase" would claim it.

Against keeping the incumbent, two counterarguments deserve answers:

- *"`tidymedia_multitrack_separation` is also an operation noun, and RR04 cited
  it approvingly."* It names a situation-fact — a multi-track input was mapped
  whole into one output — which is only ever true when the diagnostic fires.
  "Loudnorm analysis" is true of every successful two-pass call; the phase ran.
  The two names are not alike in the way that matters.
- *"In handler position a name cannot be mistaken for success"* (RR04 §1b's
  closing note). That defense answered an ambiguity-about-success objection to
  `ffmpeg_exit`; it does not answer category overreach — RR04 rejects overreach
  in the same section.

On length: at 33 characters this becomes the longest class in the package, and
RR04 rejected a 29-character name as overlong. The cases differ: there the
short form (`ffmpeg_exit`) was equally correct, so length bought nothing; here
the short incumbent is what is being replaced *for* correctness. The tighter
`tidymedia_loudnorm_unmeasured` (29) was considered and set aside: what is
unmeasured is the input, not the pass, so the compound reads wrongly, and
`no_measurement` matches the abort's own message ("did not yield usable
measurements", `R/loudnorm_two_pass.R:241`) so docs, message, and class say one
thing. Dropping the `loudnorm_` scope (`tidymedia_no_measurement`) is rejected
on RR04's narrow-name precedent (`probe_timeout`/`batch_timeout`): other parts
of the package measure things too, and the event is specific to this pass.

## 4. Class-vector shape at each site

Ship exactly AC1's shape, which is also the shape M086 established at
`R/ffmpeg.R:681`:

- **Scalar abort** (`R/loudnorm_two_pass.R:151`):
  `class = c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit")`,
  keeping `tm_status`. Both facts are true at this site — no measurement, and a
  known non-zero exit — and carrying both keeps `?tidymedia`'s promise that a
  refused FFmpeg run is catchable by one class (`R/tidymedia-package.R:113-118`)
  true on this path, exactly as `R/ffmpeg.R:675-681`'s comment argues for the
  multi-track case. Context class first, mechanism second, matching that
  precedent.
- **Batch abort** (`R/loudnorm_two_pass.R:253`): the shared class **alone**,
  keeping `tm_rows`/`tm_row_status`. Adding `tidymedia_ffmpeg_exit` here would
  assert a fact that is false for any zero-exit row in the batch, and would
  break the field contract that class has carried at every site since M085 —
  everywhere it appears, a scalar integer `tm_status` comes with it
  (`R/ffm.R:1624-1625`, `R/loudnorm_two_pass.R:151-152`, `R/ffmpeg.R:681-682`);
  a mixed batch has no such scalar. The existing absence test
  (`tests/testthat/test-normalize-audios-two-pass.R:415`) is the right lock.
- **No further narrower class at either site.** Nothing narrower is true
  batch-wide, and the scalar site's two classes already name every fact
  established there. RR04 §2's rule stands: structure is added when a member
  and a caller need both exist.

## 5. Renaming cost

**Take the rename now.** Every site that names the incumbent is inside this
repository: the raising site (`R/loudnorm_two_pass.R:253`), three roxygen
sites (`R/ffmpeg.R:4393`, `R/ffm.R:1561`, `R/tidymedia-package.R:120`) plus
their generated `man/` counterparts, two `NEWS.md` entries (lines 41 and 63),
and eight test lines across `tests/testthat/test-normalize-audios-two-pass.R`
and `tests/testthat/test-ffmpeg-exit-condition.R`. M087's own tasks already
rewrite all of them: T2 edits the raising site and "every test asserting either
name", T4 re-derives every doc enumeration by running the sites, T6 rewrites
the class-vector assertions. The rename's *marginal* cost inside this milestone
is the `NEWS.md` edit.

The silent-break risk the brief names is real but has no population to fall
on: the package is unreleased on CRAN and pre-0.2.0, the clean-break policy
(D014) is recorded, and the only handlers written against the incumbent are the
package's own tests. Since the 0.2.0 release that would document
`tidymedia_loudnorm_analysis` has not shipped, the two M086 `NEWS.md` entries
should be edited in place to name the final class — users should meet one name,
not a name and its correction. After first release this calculus inverts
permanently, which is the brief's "last cheap moment", and I agree that this is
it.

## 6. The record D062 is owed

**Recording is the right disposition; do not change the code at
`R/ffmpeg.R:681` or `:742`.** The asymmetry is not the falsifier's shape.
D062's falsifier is "one recorded event at two severities under **two names**"
— the same event forced to rename because severity changed. What shipped is one
event under **one** name at both severities: `tidymedia_multitrack_separation`
appears at the error site (`R/ffmpeg.R:681`) and at the warning site
(`R/ffmpeg.R:742`) alike. The error site *additionally* carries
`tidymedia_ffmpeg_exit` because a second fact — a specific, known non-zero exit
— is established there (the branch only runs when `status` is a real number,
`R/ffmpeg.R:655-660`) and cannot be established at the warning site: the batch
runner reduces each row to `success = FALSE` and discards the condition (D007),
so no exit number exists for the warning to carry, and the `tm_status` field
contract could not be honored. The event-naming rule is exactly what kept the
shared name honest across severities; the falsifier did not fire.

Restoring symmetry would require one of two wrong moves: adding
`tidymedia_ffmpeg_exit` to the warning (asserting a mechanism the site cannot
evidence, false for rows that failed for non-exit reasons, and a class without
its contractual field), or dropping it from the error (un-doing M086's point,
that a refused run answers to one class on every path).

For the annotation to be honest, it must concede what the asymmetry narrows,
not merely report it. It should say, in substance:

1. The rule held: the shared event carries one name at both severities. The
   falsifier's shape — two names for one event across severities — did not
   occur.
2. What the code has now established, which D062 did not state: a site's class
   vector asserts every event *established at that site*, so the vectors at two
   severities of one event may differ — by additional classes naming additional
   facts, never by the shared event's own name.
3. The concession: "same event, same class *vector*" is therefore **not** the
   convention, and a handler on a mechanism class (`tidymedia_ffmpeg_exit`)
   does not see batch-severity signals of events whose scalar form carries it.
   Bridging that gap is documentation's job (AC3), not the class system's.
4. The constraint that forces it here: D007 discards the per-row condition, so
   the warning site can evidence neither a non-zero exit nor a `tm_status`.
5. A sharpened falsifier: falsified if the shared event's own name ever differs
   across severities, or if a class is ever attached at a site that cannot
   carry that class's contractual fields.

## Beyond the brief

- **B1 — the scalar zero-exit unparseable abort is unclassed and inside the
  shared event.** `parse_loudnorm_measurements()`'s "could not parse" abort
  (`R/loudnorm_two_pass.R:112-118`) fires when the analysis run exited zero but
  printed no parseable finite block — precisely the case the batch class is
  documented to cover (`R/tidymedia-package.R:118-120`). AC1 as written pairs
  only `:151` and `:253`, so a caller of the scalar form catching the shared
  class would still miss this path. Give it the shared class **alone** (no
  `tidymedia_ffmpeg_exit` — the exit was zero; no `tm_status` exists). This is
  a one-argument edit inside a file T2 already touches, and without it M087's
  goal sentence stays false for that path. The silence abort three lines above
  (`:103-110`) must **not** get the shared class — see B2.
- **B2 — the silence asymmetry deserves a documented sentence, not the shared
  class.** A silent input aborts on the scalar form (`R/loudnorm_two_pass.R:
  103-110`, unclassed) but is set aside and marked on the batch form
  (`R/loudnorm_two_pass.R:262-268`, the `silent` column). That is a behavioral
  scalar/batch divergence of the same family M087 documents; when T3 is in
  `?normalize_audio`/`?normalize_audio_batch`, one sentence stating it is
  cheap. When the unclassed-abort backlog reaches `:104`, its class should name
  a distinct event (the input is silent — a fact about the input, and a
  *successful* measurement of `-inf`), never the shared class.
- **B3 — the brief's caller-view probe never reaches the site it demonstrates.**
  `normalize_audio("nonexistent.wav", ...)` aborts unclassed in
  `check_file_readable()` (`R/ffmpeg.R:2240`) before any FFmpeg spawn; the
  observed vector is `c("rlang_error", "error", "condition")`. Worth knowing
  for T4's derive-by-running discipline: the probe input must exist and be
  unreadable *as media* (a text file with a `.wav` name works, exit 183 on this
  machine's FFmpeg).

## Recommendations

1. **Apply:** one shared event class across the scalar and batch analysis-pass
   aborts (Q1, Q2).
2. **Apply:** name it `tidymedia_loudnorm_no_measurement`; retire
   `tidymedia_loudnorm_analysis` now, editing the unreleased M086 `NEWS.md`
   entries in place to the final name (Q3, Q5).
3. **Apply:** scalar `:151` raises `c("tidymedia_loudnorm_no_measurement",
   "tidymedia_ffmpeg_exit")` with `tm_status`; batch `:253` raises the shared
   class alone with `tm_rows`/`tm_row_status`; no other classes at either site
   (Q4).
4. **Apply:** also give the scalar unparseable abort
   (`R/loudnorm_two_pass.R:112`) the shared class, alone (B1).
5. **Apply:** record the multi-track asymmetry as a D062 annotation carrying
   the five points in §6; leave `R/ffmpeg.R:681` and `:742` as they are (Q6).
6. **Consider:** one documented sentence on the silence asymmetry while T3 is
   in the two normalize topics; a future distinct event class for the silent
   abort, never the shared one (B2).
7. **Reject:** the docs-only alternative — it leaves the moved-handler trap
   armed and the `:112` path uncatchable (Q2).
8. **Reject:** keeping `tidymedia_loudnorm_analysis` — a phase name that
   overpromises over the timeout, silence, and missing-binary neighbors, RR04
   §1b's trap in a second dress, and this is the last cheap rename (Q3, Q5).
9. **Reject:** `tidymedia_ffmpeg_exit` on the batch abort or the batch
   warning — false for zero-exit rows and unable to carry the `tm_status` the
   class promises everywhere it appears (Q4, Q6).
