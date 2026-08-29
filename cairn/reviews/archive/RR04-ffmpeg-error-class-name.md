# RR04: The class name for a failed FFmpeg run (M085)

- **Date:** 2026-08-29
- **Brief:** cairn/reviews/RB04-ffmpeg-error-class-name.md
- **Reviewer:** independent Fable-level review
- **Status:** advisory (no binding criteria requested)

## 1. What should the class be called?

**Recommendation: `tidymedia_ffmpeg_exit`.** Reject the plan's
`tidymedia_ffmpeg_error`.

**(a) The absence of `error` in existing names is a convention worth keeping,
not an accident.** The nine shipped classes all name a *fact about what
happened* (`timeout`, `copy_map_conflict`, `dropped_audio`,
`multitrack_separation`) rather than a severity, and the package has already
cashed that convention in: `tidymedia_multitrack_separation` rides on an error
at one site and a warning at another (`R/ffmpeg.R:662-680`, `723-740`). A name
containing `error` could never be reused that way without lying. Severity is
already carried by the condition's base classes (`error`/`warning`/`condition`),
so putting it in the package class buys a caller nothing —
`tryCatch(tidymedia_ffmpeg_error = )` and `tryCatch(tidymedia_ffmpeg_exit = )`
are equally catchable; only the second still reads correctly if the same fact
ever needs to be signalled at warning severity (a batch-adjacent context, say).

**(b) The name must describe the event, not the category.** The class covers
exactly one failure mode: FFmpeg was found, ran, and returned non-zero. A
missing binary aborts unclassed in `run_program()`
(`R/program_management.R:110-112`) and a timeout aborts as `tidymedia_timeout`
before any status exists (`R/ffm.R:1569-1584` shows the timeout path never
reaches the status check). A caller who writes
`tryCatch(tidymedia_ffmpeg_error = ...)` will read that as "any FFmpeg
failure" and be silently wrong about two of the three failure modes. The
package's own precedent already resolves this tension the same way:
`tidymedia_probe_timeout` and `tidymedia_batch_timeout` are deliberately
narrow names that do not answer to `tidymedia_timeout`. Name the narrow event.

On the objection that "exit" alone is ambiguous (FFmpeg also exits with 0):
conditions are only signalled on failure, so in the position where the name is
read — a handler argument — `tidymedia_ffmpeg_exit` cannot be mistaken for
success. If the ambiguity still bothers, `tidymedia_ffmpeg_nonzero_exit` is
the precise form, but at 30 characters it is the longest class in the package
for no added catchability; I do not recommend it.

**(c) Ecosystem convention cuts the other way, and should lose here.** The
rlang/tidyverse house style is `pkg_error_detail` with the severity word in
second position (`vctrs_error_incompatible_type`, `dplyr_error_join_*`,
`rlang_error`). Note that even under that style the plan's
`tidymedia_ffmpeg_error` is wrong — the severity word trails instead of
leading, matching no convention at all. Adopting the tidyverse shape properly
would mean `tidymedia_error_ffmpeg_exit`, and consistency would then demand
renaming the nine existing classes, which is a different and larger decision
than this milestone. One class in ecosystem shape amid nine in package shape
is the worst outcome. Follow the package's own convention.

## 2. One class, or a class vector?

**One class. The maintainer's decision is right, and no narrower parent earns
its keep today.**

A hypothetical `tidymedia_ffmpeg_failure` parent would have exactly one child
at ship time: the missing-binary abort stays unclassed after M085, and the
timeout already has `tidymedia_timeout`, which deliberately does not
participate in hierarchies (see the probe/batch timeout siblings). A parent
with one member is structure on spec, and the package's precedent is flat
names.

The decisive point is that deferring costs nothing: condition classes are
character vectors, and *appending* a parent class later
(`class = c("tidymedia_ffmpeg_exit", "tidymedia_ffmpeg_failure")`) breaks no
handler written against the child. Hierarchy can be added in the future
milestone that classes the remaining aborts, when there would actually be two
members to unify — and only if a caller need has materialized by then.

## 3. `tm_status`: right name, right type?

**Yes to `tm_status`; store it as `as.integer(status)`; say one sentence about
signals.**

- **Name.** `status` alone does not collide with rlang's reserved condition
  fields (`message`, `class`, `call`, `trace`, `parent`, `body`, `footer`,
  `use_cli_format`) or base R's (`message`, `call`), but the `tm_` prefix is
  already the package's stated defense against that namespace ever growing
  (`R/timeout.R:372-374`, read back at `R/ffm.R:1579-1580`). Two prefixed
  fields plus one unprefixed one would make the convention unreliable exactly
  where reliability is its point. `tm_status` it is.

- **Type.** `system2()` documents the `status` attribute as an integer, but
  the field's contract should not be "whatever the attribute plumbing
  delivered". Coerce at the abort site: `tm_status = as.integer(status)`. This
  makes `identical(cnd$tm_status, 1L)` and `expect_equal(cnd$tm_status, 1L)`
  dependable and costs one function call. (The loudnorm site gets the same
  coercion.)

- **Negative statuses.** The regex being retired already accepted them
  (`"exited with status -?[0-9]+"`, `R/ffmpeg.R:781`), so parity requires the
  field to carry any non-zero integer without validation — do not assert
  `tm_status > 0`. Documentation needs exactly one sentence: the field is the
  status as `system2()` reported it, and a signal-killed FFmpeg surfaces as
  whatever number the OS/shell maps the signal to (128+n on most Unixes). Do
  not promise more semantics than that; the package does not interpret the
  number and should not start.

## 4. Same class on the loudnorm analysis abort?

**Confirm.** Both sites report the identical fact — FFmpeg ran to completion
and returned non-zero — differing only in prose and in which command failed. A
caller's reason for catching the class ("an FFmpeg run failed; log the status
and move on / retry / surface it") applies equally to both. Splitting them
would force every such caller to enumerate two classes forever, for a
distinction the condition object already carries three other ways: the
message, the `call` field (`run_loudnorm_analysis()` passes `call = call`
through), and the failing command printed in the bullets.

If a future caller genuinely needs to dispatch on "the loudnorm analysis pass
specifically", the additive fix is to *prepend* a narrower class at that site
(`class = c("tidymedia_loudnorm_analysis", "tidymedia_ffmpeg_exit")`) — a
non-breaking change that can wait for the need to exist. Nothing about M085
forecloses it.

## 5. What must the shipped documentation say?

**Minimum: two places.**

1. **`ffm_run()`'s roxygen** — the raiser is where a user who just caught the
   error will look. One paragraph (a `@section Errors:` reads better than
   overloading `@return`, since the class is about the failure path, not the
   return value): a non-zero FFmpeg exit aborts with class
   `tidymedia_ffmpeg_exit` carrying the exit status as the integer field
   `tm_status`; the same class is signalled when the `loudnorm` analysis pass
   behind `normalize_audio(two_pass = TRUE)` fails. That second clause matters
   because `run_loudnorm_analysis()` is internal — the class must be
   discoverable without reading source.

2. **`NEWS.md`** — the class and field are new programmatic surface; the entry
   is also where a pre-1.0 user learns the contract exists at all.

The package-level help page (`R/tidymedia-package.R`) already serves as the
"handle conditions programmatically" hub for the timeout family; adding one
sentence naming `tidymedia_ffmpeg_exit` there is cheap and keeps the hub
complete, but it is a *consider*, not part of the minimum. Task-verb pages do
not each need it: the error propagates from `ffm_run()`, which their docs
already stand on.

## 6. What does this commit the package to that it should not?

Nothing that binds harmfully, with three observations:

- **Naming trajectory.** Shipping a tenth class in package shape
  (`tidymedia_<event>`) further entrenches that shape against a future switch
  to tidyverse `pkg_error_*` form. That is fine — but if the maintainer has
  any appetite for the ecosystem shape, the moment to decide is before the
  first CRAN release, in one sweep over all ten classes, not per milestone.
  Recorded here so the choice is made once, deliberately.

- **`tm_` is now load-bearing convention.** A second condition family using
  the prefix makes `tm_` the de facto namespace for every future condition
  data field. Good — but a future milestone should not ship an unprefixed
  field without revisiting this, and the convention deserves a line in
  DECISIONS or LESSONS when the remaining aborts get classed.

- **The batch candidate is unconstrained.** `ffm_batch()` rows record failure
  as data (D007), and `assemble_measured()`'s per-row
  `list(status = "error")` (`R/loudnorm_two_pass.R:219`) is a list field, not
  a condition — no namespace is shared with `tm_status`. A future milestone
  that preserves the per-row exit number will put it in a data column
  (plausibly named `status`), and nothing in M085 collides with or
  pre-commits that column's name or type. The only soft commitment: if that
  milestone *also* wants a raised condition somewhere, `tidymedia_ffmpeg_exit`
  + `tm_status` is now the shape it must reuse for the same fact — which is
  the point of doing this.

## Beyond the brief

- **The consumer should narrow by class, not by `error`.**
  `run_separation_audio()`'s `tryCatch(error = )` plus regex plus NA-sentinel
  logic (`R/ffmpeg.R:646-661`) collapses once the class exists: catch
  `tidymedia_ffmpeg_exit` directly (`rlang::try_fetch()` or `tryCatch`), and
  the missing-binary and timeout cases never enter the handler at all —
  D024's fail-open behavior for those cases becomes structural rather than a
  branch. The handler's remaining fail-open duty (unanswerable track count,
  single-track input) still re-raises the original condition via `stop(cnd)`,
  unchanged.

- **Replace the wording test, don't just delete it.** The test at
  `tests/testthat/test-separate-av-multitrack.R:132-146` pins the
  message-coupling; its successor should pin the new contract instead:
  `expect_error(..., class = "tidymedia_ffmpeg_exit")` and an assertion that
  `cnd$tm_status` is a non-zero integer, on the same guaranteed-failure
  pipeline (AAC copy into `.mp3`). That keeps the "reworded/restructured abort
  fails loudly" property the old test existed for.

- **Byte-identical messages are compatible with the change.** `cli_abort()`
  takes `class = ` and `...` fields without touching the formatted message, so
  the constraint that prose stays byte-identical at both sites is trivially
  satisfiable — worth stating because it means the milestone's diff can be
  verified as "class/field only" by snapshot tests that already exist, if any
  cover these messages.

## Recommendations

1. **Apply:** name the class `tidymedia_ffmpeg_exit`; reject
   `tidymedia_ffmpeg_error` (severity word breaks package convention and
   overpromises category coverage; see Q1).
2. **Apply:** single flat class, no parent; add hierarchy additively later
   only if a second member and a caller need both exist (Q2).
3. **Apply:** field `tm_status`, coerced with `as.integer()` at both abort
   sites; no positivity validation; one documentation sentence on
   signal-terminated statuses (Q3).
4. **Apply:** same class and field on `run_loudnorm_analysis()`'s abort (Q4).
5. **Apply:** document in `ffm_run()` roxygen (`@section Errors:`, naming the
   loudnorm path too) and `NEWS.md` (Q5).
6. **Consider:** one sentence naming the class on the package-level help
   page's condition-handling discussion (Q5).
7. **Consider:** when implementing, switch `run_separation_audio()` to catch
   by class rather than `error = ` + sentinel, and replace — not merely
   delete — the wording-coupling test with a class/field test (Beyond the
   brief).
8. **Consider:** before first CRAN release, decide once whether the package
   ever wants tidyverse `pkg_error_*` naming; if not, record the
   `tidymedia_<event>` convention (and the `tm_` field prefix) in DECISIONS so
   future classes don't relitigate it (Q6).
9. **Reject:** `tidymedia_ffmpeg_nonzero_exit` — precise but overlong for no
   added catchability (Q1); a `tidymedia_ffmpeg_failure` parent class today —
   structure with one member, addable later without breakage (Q2).
