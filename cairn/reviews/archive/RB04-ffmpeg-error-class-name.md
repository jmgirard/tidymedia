# RB04: The class name for a failed FFmpeg run (M085)

- **Date:** 2026-08-29
- **Output required:** write findings to `cairn/reviews/RR04-ffmpeg-error-class-name.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`tidymedia` is an R package (version 0.1.0.9000, not yet on CRAN) that wraps the
FFmpeg and MediaInfo command-line tools for reproducible media preprocessing:
batch trimming, cropping, format standardization, metadata extraction. It is
built in three layers — raw CLI escape hatches (`ffmpeg()`, `ffprobe()`,
`mediainfo()`); a pipe builder (`ffm_*`) that assembles and runs one FFmpeg
command; and task verbs (`extract_audio()`, `segment_video()`, …) that are thin
wrappers over the builder.

`ffm_run()` executes a built pipeline. When FFmpeg exits non-zero, `ffm_run()`
aborts with a `cli::cli_abort()` carrying no class at all — an ordinary
`rlang_error` / `simpleError`. Two consequences follow.

First, a package user who wants to handle a failed FFmpeg run programmatically
has nothing to catch by: they must match the text of the message. Second, the
package itself does this. `run_separation_audio()` in `R/ffmpeg.R` needs to know
whether a failure was a non-zero FFmpeg *exit* (as opposed to a missing binary,
an unreadable path, or a timeout) before it enriches the abort with a
multi-track diagnostic. It finds out by regex-matching the string
`"exited with status <n>"` out of the formatted message, in the internal helper
`ffmpeg_exit_status()`. A test exists whose only job is to fail if anyone
rewords `ffm_run()`'s message, because rewording would silently kill the
enrichment.

Milestone M085 retires that parse. It gives the non-zero-exit abort a condition
class and a field carrying the exit status as an integer, so both a caller and
the package read the status off the condition object instead of off its prose.

The class name is the reason for this review. R condition classes are ordinary
character vectors on the condition object, matched by name in `tryCatch()`,
`withCallingHandlers()`, `rlang::try_fetch()`, and `testthat::expect_error()`.
Once the package ships a name, user code is written against that string, and
renaming it later is a breaking change for every such caller. The package is
pre-1.0 and pre-CRAN, so the cost of getting it wrong is currently low and will
not stay low.

The milestone's plan proposed `tidymedia_ffmpeg_error` with a field `tm_status`.
The maintainer escalated the name here rather than settling it in the
implementing session.

## Materials

All paths are relative to the repository root. Read these; nothing else is
required.

**The two abort sites the milestone classes.**

- `R/ffm.R:1586-1595` — `ffm_run()`'s non-zero-exit abort. Read the surrounding
  function `ffm_run()` (`R/ffm.R:1548-1598`) for how the status is obtained: it
  comes off the `"status"` attribute that `system2(stdout = TRUE)` sets on its
  return value.
- `R/loudnorm_two_pass.R:130-152` — `run_loudnorm_analysis()`'s non-zero-exit
  abort, which composes the same fact into different prose ("The `loudnorm`
  analysis pass failed (FFmpeg exited with status N)."). The milestone gives
  this site the same class and field.

**The parse being retired, and its consumer.**

- `R/ffmpeg.R:776-784` — `ffmpeg_exit_status()`, the regex helper. Unexported;
  its only call site is `R/ffmpeg.R:656`.
- `R/ffmpeg.R:636-700` — `run_separation_audio()`, the consumer, including the
  comment block at lines 650-660 that explains the parse and will be rewritten.
- `tests/testthat/test-separate-av-multitrack.R:132-146` — the wording-coupling
  test the milestone deletes.

**The naming precedent to weigh the candidates against.** The package already
ships nine classed conditions. Their sites:

| Class | Site | Signalled as |
|---|---|---|
| `tidymedia_timeout` | `R/timeout.R:362-379` | error |
| `tidymedia_copy_map_conflict` | `R/ffm.R:718-729` | error |
| `tidymedia_multitrack_separation` | `R/ffmpeg.R:662-680` and `R/ffmpeg.R:723-740` | error at the first site, warning at the second |
| `tidymedia_probe_timeout` | `R/ffprobe.R:293-305`, `R/ffm_manifest.R:130-141` | warning |
| `tidymedia_batch_timeout` | `R/ffm_batch.R:189-198` | warning |
| `tidymedia_sequential_plan` | `R/ffm_batch.R:224-230` | warning |
| `tidymedia_dropped_audio` | `R/ffmpeg.R:390-402` | warning |
| `tidymedia_absorbed_timeout` | `R/timeout.R:437` | internal restart-ish marker |
| `tidymedia_ffm` | `R/ffm_oop.R` | not a condition — the pipeline S3 object |

Note that no existing condition class contains the word `error`, that
`tidymedia_multitrack_separation` names a *situation* and is reused across an
error and a warning, and that `tidymedia_probe_timeout` /
`tidymedia_batch_timeout` are narrower siblings of `tidymedia_timeout` that do
not inherit from it — a caller catching `tidymedia_timeout` does not catch them.

**The field-name precedent.** Only one condition carries data fields today:
`tidymedia_timeout` carries `tm_program` and `tm_limit` (`R/timeout.R:372-374`).
The `tm_` prefix exists because `cli::cli_abort()` passes `...` through to
`rlang::abort()`'s condition fields, which share a namespace with rlang's own
(`message`, `call`, `trace`, `parent`, `body`, `footer`, `use_cli_format`).
`ffm_run()`'s own timeout handler reads those fields back at `R/ffm.R:1579-1580`.

**How condition classes are documented today.** They are not exported symbols
and have no help page of their own. They appear in roxygen prose on the verbs
that raise them, as the argument to a suppression or handling call — e.g.
`R/ffmpeg.R:5774` and seven other sites naming
`suppressWarnings(classes = "tidymedia_dropped_audio")`, and
`R/tidymedia-package.R:66`.

**Running things.** The package is checked with `devtools::check()` and tested
with `devtools::test()` (testthat 3rd edition). Execution tests skip when the
FFmpeg binary is absent. You are not asked to run anything; reading suffices.

## Questions

1. **What should the class be called?** Weigh at least these three candidates
   against the naming precedent above and against how the name will read in a
   caller's `tryCatch()`:
   - `tidymedia_ffmpeg_error` — the plan's proposal.
   - `tidymedia_ffmpeg_exit` — names the specific event (FFmpeg ran to
     completion and returned non-zero).
   - a name of your own construction, if neither fits.

   Address specifically: (a) that no existing class uses the word `error`, and
   whether that is a convention worth keeping or an accident worth breaking;
   (b) whether the name should describe the *event* (a non-zero exit) or the
   *category* (an FFmpeg failure), given that the package has other FFmpeg
   failure modes — a missing binary (`run_program()`, `R/program_management.R:110-112`,
   unclassed) and a timeout (`tidymedia_timeout`) — which this class will
   **not** cover; (c) whether R-ecosystem convention (rlang, cli, tidyverse
   condition-class naming) favors one form.

2. **Is one class right, or should the abort carry a class vector?** The
   maintainer has already decided against adding a broad `tidymedia_error`
   parent, on the ground that the package's other aborts stay unclassed after
   this milestone and would not answer to it. Independently of that decision:
   is there a *narrower* parent-child pair that earns its keep here — for
   instance a general "FFmpeg failed" class with the non-zero-exit case as a
   subclass — given that the missing-binary and timeout cases exist today and
   the milestone leaves the first of them unclassed? If you think the
   maintainer's single-class decision is wrong, say so explicitly rather than
   working around it.

3. **Is `tm_status` the right field name, and is an integer the right type?**
   The plan specifies an integer equal to the status FFmpeg returned. Consider:
   the `tm_` prefix precedent; whether `status` collides with anything in
   `rlang::abort()`'s reserved field names or in base R's condition machinery;
   whether the field should be integer, or should preserve exactly what
   `attr(out, "status")` gave (which `system2()` documents as an integer but
   which arrives via an attribute); and whether a negative status (a
   signal-terminated FFmpeg) needs anything said about it.

4. **Should the same class also go on the `loudnorm` analysis abort?** The
   milestone's plan says yes: two sites reporting the same fact in different
   prose should be one class to a caller. Confirm or dispute. If you dispute
   it, name what a caller gains from telling the two apart and what mechanism
   should carry that distinction (a second class in the vector, a field, a
   separate class).

5. **What must the shipped documentation say?** The class is not an exported
   symbol, so it has no help page. Given that the package documents its
   existing classes only as arguments to `suppressWarnings(classes = )` in verb
   prose, where should this one be named so a user can find it — `ffm_run()`'s
   `@return` or a `@section`, the package-level help page, `NEWS.md`, or more
   than one of these? Name the minimum that discharges the promise.

6. **What does this name commit the package to that it should not?** Beyond
   the questions above, name any way the proposed design (class + `tm_status`
   field, on these two sites) would constrain a future milestone that classes
   the package's remaining unclassed aborts, or that reports exit numbers on
   `normalize_audio_batch(two_pass = TRUE)`'s per-row failures (which today
   record `list(status = "error")` and discard the number,
   `R/loudnorm_two_pass.R:219`) — both explicitly out of M085's scope and
   already recorded as candidate work.

## Constraints

These are fixed. Flag disagreement explicitly rather than silently working
around them.

- **The three-layer architecture** (D002, D003): all command assembly lives in
  the `ffm_*` builder. This milestone adds no command-assembly behavior; it
  changes only what the abort carries.
- **`ffm_batch()`'s `success = FALSE` contract** (D007) does not change. Batch
  rows record failure as data, not as a raised condition, and nothing here
  makes a batch row abort.
- **The diagnostic-probe licence** (D024, as narrowed by D034/D035/D040/D045):
  a diagnostic may probe the executing path but never decides *whether*
  execution proceeds, never changes what was compiled, and fails open. The
  multi-track enrichment in `run_separation_audio()` sits inside that licence
  and must keep failing open — re-raising the original condition with its
  message, class and trace unchanged — when it cannot answer.
- **Message text does not change** at either abort site. This milestone adds a
  class and a field; the prose a user reads stays byte-identical, so that
  reworded-message churn is not confounded with the behavior change.
- **New user-facing conditions use `cli::cli_abort()` / rlang**, never
  assertthat, which is being retired from this package.
- **The package is pre-1.0 and pre-CRAN.** A breaking change is cheap today.
  Do not weight backward compatibility with an installed user base that does
  not exist; do weight the cost of renaming after the first release.

## Output format

In `RR04-ffmpeg-error-class-name.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Your report is advisory: this brief does **not** request
binding criteria, so emit no `## Binding criteria` section.
