# RR07: The hardware-capability helpers' signature under a two-backend vocabulary (M100)

- **Date:** 2026-09-01
- **Brief:** `cairn/reviews/RB07-hardware-helper-signature.md`
- **Advisory** — no binding criteria requested.

Checked against `master` at 0436981 (the branch `m100-videotoolbox-backend` is
cut from it and carries no code yet). Measurements taken for this report:
`ffmpeg -encoders` on the authoring machine lists `h264_videotoolbox`,
`hevc_videotoolbox`, `prores_videotoolbox` and no `*_nvenc`;
`grep -rln "tidymedia.hardware_encoders" man/` returns exactly
`hardware_encoder.Rd`, `refresh_ffmpeg_capabilities.Rd`,
`tidymedia-package.Rd`; `grep -n 'hardware = c(' R/ffmpeg.R` returns 17 sites
and all 17 spell `c("none", "nvenc")`; non-comment lines calling
`has_hardware_encoder(` under `R/`, `tests/`, `vignettes/` number 31 (a few are
`test_that()` titles), and lines calling `hardware_encoder(` number 15 (5 in
`R/ffmpeg.R`, 10 in tests, plus `tests/testthat/test-parallel-option-carry.R:308`
which composes the two). `README.Rmd` mentions neither helper nor `hardware`.

One fact frames questions 1–3, so it comes first. The predicate is defined
through the mapper: `nvenc_available()` (`R/ffmpeg.R:3050`) computes
`enc <- hardware_encoder(codec)` and answers `enc %in% pool`. Whatever
argument the predicate takes, it hands to the mapper. A signature the two do
not share therefore has to be reconciled inside that one line, and every
option below is judged partly on whether it keeps that relation a single
expression.

## 1. The `hardware =` default on `has_hardware_encoder()`

**Recommend (c): no default, on both helpers.** `has_hardware_encoder(codec,
hardware)` with `rlang::arg_match(hardware, values = <the table's backends>)`.

The reasoning, option by option.

**(a) `"nvenc"`.** The default would be the first backend by shipping date,
not by any property. At the 16 verbs `"none"` is first because it is the off
position of a toggle; the helpers have no off position (question 2), so the
only candidates for first place are members, and a member as a silent default
is the defect D077 named — "a function named for one backend answering for
another" — moved from the name into the default value. D014 does not reach
default values, so this is not a rule violation; it is the same reader
confusion by another route: the vignette's `has_hardware_encoder("h264")` on
the authoring machine answers `FALSE` beside a build that lists
`h264_videotoolbox`. The composition D077 relies on is worse off:
`options(tidymedia.hardware_encoders = hardware_encoder("h264"))` on a Mac
declares `h264_nvenc` available, and the option seam is read before any probe
(D044), so nothing corrects it. (a) keeps 31 call lines unchanged and is the
only option whose cost is zero today; that is its whole case.

**(b) "any available backend".** The answer is honest — "can this machine
encode this family in hardware" — but it does not compose with the argument
the predicate exists to pre-flight. The verbs' `hardware =` takes one member;
a `TRUE` from "any" does not say which. On the authoring machine the vignette
as written would then read `has_hardware_encoder("h264")` → `TRUE`, followed by
`hardware = "nvenc"` → abort: a pre-flight check that passes and a call that
refuses, which is the one outcome a pre-flight check exists to prevent. The
vignette would have to name the backend on the check anyway, so the default
would serve no documented call. Is the changed answer on Apple hardware a fix
or a silent change? It is a fix relative to the *name* M099 chose (the
`NEWS.md:307–313` entry says the helpers answer for nvenc "because nvenc is
the one backend", a sentence written to expire), and a silent change relative
to `skip_if_no_nvenc()` (`tests/testthat/helper-skip.R:35`), whose skip
message "nvenc encoder not listed" would become false on a Mac (the one-frame
probe below it would still skip, so no test breaks; the message lies). And
(b) cannot be given to the mapper (question 3), so the pair would default
differently — the predicate's body would no longer be
`hardware_encoder(codec, hardware) %in% pool` but a loop over backends, which
is a second place the table is walked.

**(c) required.** Every call names its backend; the check and the verb call it
guards spell the same value (`has_hardware_encoder("h264", "nvenc")` beside
`hardware = "nvenc"`), which is the teaching the vignette wants; the two
helpers share one signature and the predicate stays one line over the mapper.
The cost is the sweep the brief prices: 31 predicate lines, 10 mapper test
lines, the vignette chunk, the skip helper, and one `expect_equal(hardware_encoder(),
"h264_nvenc")` (`tests/testthat/test-nvenc.R:13`) that asserts the default
and must go. All are in this repository; the package is 0.1.0.9000 with no
external caller, which is the condition D014's clean-break policy was written
for and the condition RR06 used to price every rename in the same series. A
removed default is a break only against a caller, and there is none.

What decides it is an asymmetry, not the head-count. An argument with no
default can gain one later as an additive change; an argument with a default
cannot lose it, or change it, without a deprecation cycle once 0.2.0 ships.
(c) is the only option that keeps (a) and (b) available at zero cost after
release, which is the reversibility the brief says the maintainer is buying.

**Falsifiers.** A reverse dependency or user script calling either helper
before 0.2.0 ships (then the break has a victim and (a) is the cheaper path);
or a third consumer of the predicate for which a backend-free default has a
meaning — none exists in the repository today, since every call site either
names nvenc implicitly (the skip helper, the option-carry test) or is a test
of the seam.

**Had 0.2.0 already shipped:** (a), and reluctantly. Removing a default is
a break, changing its meaning to (b) is a silent behavior change on Apple
hardware, and a vendor default that is merely confusing is the least bad of
three costs paid by strangers. The pre-release position inverts this because
the only caller is the one making the change, and because (c) is the state
from which both other answers remain reachable.

## 2. Whether the helpers accept `"none"`

**Narrower set: the helpers take the backends only, `c("nvenc",
"videotoolbox")`, and `arg_match()` refuses `"none"`.**

`"none"` is a value of the *toggle*, not of the vocabulary. At the resolver
(`R/ffmpeg.R:3101–3102`) it means "return `video_codec` unchanged", which
needs a `video_codec` to return; neither helper has one — both take a family.
Neither candidate answer for the predicate survives inspection: `TRUE` ("the
software encoder is always available") is false on a build without `libx264`,
and `FALSE` ("no hardware encoder is called none") would make
`skip_if_not(has_hardware_encoder("h264", "none"))` skip, i.e. treat the
setting that always works at the verbs as unavailable. For the mapper,
`hardware_encoder("h264", "none")` has no string to return that is not a
guess at the caller's software codec. A value with no defined answer is
refused, and `arg_match()`'s refusal lists the accepted set, so a caller who
typed `"none"` by analogy with the verbs is told the two members in the same
error.

What the mismatch costs a reader: one sentence in the shared `@param
hardware`, of the shape "one of the backends the verbs' `hardware =` accepts;
`"none"` is the verbs' off position and has no meaning here". The mismatch is
the honest one — the verbs' set is `{off} ∪ backends` and the helpers' set is
`backends` — and the alternative, accepting `"none"` with an answer picked to
avoid an error, hides a question with no answer inside a value that has one.

Implementation note that is not a design choice: define the backend set once,
as the names of the per-backend table, and have the helpers `arg_match()`
against it. The 16 verbs and the resolver must keep their literal
`c("none", "nvenc", "videotoolbox")` defaults, because AC1's sweep reads each
verb's accepted set off its formal's default and a computed default would read
as a call, not a vector.

## 3. Whether `hardware_encoder()` takes the same argument and default

**Confirm D077, and required on the mapper too.** After M100 the mapper is the
lookup into the per-backend table, and a lookup into a two-column table takes
two keys. `hardware_encoder("h264", "videotoolbox")` is `"h264_videotoolbox"`;
`hardware_encoder("av1", "videotoolbox")` is the (family, backend) pair the
table lacks and must abort naming both — which is the refusal AC2 asks of the
verbs, so site it here, once, and let the predicate and the resolver reach it
through the mapper. `codec_family()` (`R/ffmpeg.R:3070`) then stays what it
is, a backend-free inference from a software codec name (`"libx264"` →
`"h264"` is true under every backend), and only its abort text changes: it
should stop naming nvenc, since a family it cannot infer is not a backend's
fault. This is a narrower reading of T3 ("generalize `codec_family()` to name
the backend it was asked about") than the plan's wording, and I think the
better one: the family question and the table question are different
questions with different refusals.

Purity is kept: the mapper reads the table and nothing else, so
`options(tidymedia.hardware_encoders = hardware_encoder("h264", "videotoolbox"))`
still asks the machine nothing, which is the property D077 rests on.

**If question 1 is nonetheless answered (b).** The mapper cannot take "any": it
returns one string, and choosing which by probing would make it the predicate.
The only coherent shape is then a *required* `hardware` on the mapper and an
"any" default on the predicate, with the predicate's body becoming a loop
`any(vapply(backends, function(b) hardware_encoder(codec, b) %in% pool, ...))`
that still goes through the mapper for every name. Two exports that share a
name stem and an argument but not its default is a cost the documentation
must carry in a sentence, and it is the reason (c) is preferred over (b) here
rather than a reason against (b) on its own.

## 4. The AC6 amendment

**Correct, holds the original promise, and does not widen it.** Against the
audit's three questions:

*What state satisfies it exactly as written.* Four instruments, each with a
checkable state: the three `man/` topics the grep returns (the amendment's
"the same three" makes the count itself an assertion, which the original
lacked — the original's grep, `tidymedia.nvenc_encoders`, returns zero files
at HEAD and so quantified over nothing); `vignettes/workflow.Rmd:74–87`
rewritten so the check names a backend and the prose names two; the
`_pkgdown.yml` section prose unchanged and still neutral ("re-checks rather
than rewrites" is satisfiable: the state is "still reads as it does"); a
`NEWS.md` entry naming videotoolbox and each backend's families. "Describes a
vocabulary, not one backend" is a reading, not a grep, in both versions —
that softness is inherited, not introduced.

*Does a standing decision make it unreachable.* No. D077 fixed the option name
the amendment greps for; RR06 Q5 fixed the argument spelling the rewritten
vignette will show. Nothing in scope forbids editing any of the four.

*Universal claim over an unenumerated domain.* The claim "over a domain wider
than `man/`" is immediately enumerated by the four items that follow, so the
domain is the list. But the list is not the user-facing text that names one
backend. The 16 verbs' roxygen — `@param hardware` at `R/ffmpeg.R:1008`,
`:1425`, `:1544` and siblings reads "or `"nvenc"` for NVIDIA GPU encoding",
and every `@seealso` points at the predicate "for the `hardware = "nvenc"`
toggle" (45 occurrences of `hardware = "nvenc"` in `R/ffmpeg.R`, 94 roxygen
lines mentioning nvenc) — is generated into 16 `man/` topics, is read by every
user of `hardware =`, and is outside both the original and the amended AC6.
`devtools::check()` will not notice a `@param` that documents two values for
an argument accepting three. This is a gap in the criterion as originally
written, so the amendment is not the place to close it: adding those topics
to AC6 would be exactly the widening the brief rules inadmissible. Close it as
implementation (T8 already says "update the three help topics"; the 16 verbs'
`@param hardware` and `@seealso` text belongs in that task) and, if the
maintainer wants it *tested*, as a separate criterion raised at the amendment
gate under its own name, not as AC6.

Two wording notes, both narrowing or neutral: "lines 74-87" will be wrong the
moment the vignette is edited — anchor by content ("the paragraph and chunk
under `standardize_video_batch()` that teach `hardware = "nvenc"`") in both
versions' spirit; and "under the option name M099 gave it after this plan was
written" is history, which the work log already holds — a criterion states the
target state and reads cleaner without it. Neither changes what is promised.

## 5. Removal, per helper

The second-escalation rule is met by weighing removal for each; neither
survives it as a recommendation.

**`has_hardware_encoder()` — keep.** Growing `hardware =` strengthens its
case rather than weakening it. Before M100 the alternative pre-flight check was
`"h264_nvenc" %in% ffmpeg_encoders()$name`; after M100 it is that expression
with a hand-assembled `<family>_<backend>` string, which is the name-typing
D044 and D077 built the pair to avoid, and it spawns the process the memo
exists to skip. The predicate is also in the timeout domain
(`tests/testthat/helper-timeout-sweep.R:134`) with a documented carve-out
(`R/tidymedia-package.R:111`); removing it removes a documented behavior of
the timeout seam. Nothing supersedes D077's decline.

**`hardware_encoder()` — keep.** D077's reopening condition — a measured
report that the mapper is used only through the predicate — has not fired,
and the repository contains a direct use that is not through the predicate
(`tests/testthat/test-parallel-option-carry.R:307–308` composes both to set
the option a worker inherits). The new argument makes the mapper the exported
view of the table D077 anticipated, and its help topic the place where "which
families each backend covers" is documented; unexporting it would leave that
table documented only by the refusals at the verbs. Growing an argument is the
same change on both helpers, so it does not distinguish them; RR06's minority
position (unexport the mapper, keep the predicate) is still the acceptable
minority position and no more than that. No deprecation path is needed since
nothing is removed.

## Beyond the brief

1. **Adding `hardware =` to the helpers puts them inside two sweeps that
   quantify over "every export with a `hardware` formal".**
   `nvenc_hardware_exports()` (`tests/testthat/helper-nvenc-memo.R:32`) reads
   the namespace for that formal; M100's AC1 reuses it, and its floor is
   `expect_gte(length(h), 16L)`, which 18 passes. Two consequences follow
   regardless of how question 1 is answered:
   - `test-nvenc-memo-grid.R:41–47` builds each member's call from its formals
     (`nvenc_grid_args()`, which sets `args$hardware <- "nvenc"` at
     `helper-nvenc-memo.R:122`) and asserts exactly one probe per cell. A
     `hardware_encoder(hardware = "nvenc")` cell probes zero times and fails;
     a `has_hardware_encoder(hardware = "nvenc")` cell probes once and passes
     by accident of its `codec` default.
   - AC1 asserts "each verb's accepted set is its `hardware` formal's own
     default" with `"none"` first. Under (c) the helpers have no default to
     read; under (a) their default does not begin with `"none"`. Either way
     the helpers fail AC1's sweep as written.
   - `test-nvenc-probe-blame.R:25` intersects `tm_timeout_domain()` with the
     same set; the predicate is in the timeout domain, so it joins that
     sweep's members and is crossed with wrong forms over `codec`.
   The fix is in the tests, not the design: AC1's domain is "the 16 verbs and
   the resolver", so the sweep should exclude the two helpers by name (or by
   a property, e.g. "has a `video_codec` or `jobs` formal") with a comment
   saying why, and AC5 tests the helpers separately, which it already does.
   This is not a widening of AC1; it is the enumeration procedure catching two
   members that were never in its domain.
2. **`check_nvenc_available()`'s gate is `identical(hardware, "nvenc")`
   (`R/ffmpeg.R:3165`).** Under M100 a `hardware = "videotoolbox"` call returns
   early from the front-door check at every fan-out verb, so a missing
   videotoolbox encoder in a `_batch` call would abort inside `purrr::pmap()`
   instead of at the verb — the exact defect D035 sited the front-door call to
   prevent. AC3 tests the abort, not where it is blamed. The gate must become
   `hardware == "none"` (or a table membership test) and the wording sites at
   `:3117`, `:3120`, `:3184` must take the backend from the argument. Worth one
   assertion in AC3's test that the abort's `call` is the batch verb.
3. **`skip_if_no_nvenc()` must name nvenc explicitly under (c)** and its skip
   message is already the right one for that; AC4's videotoolbox sibling
   should not call the predicate at all (the plan already says so).
4. **The M099 `NEWS.md` bullet and M100's are in the same unreleased
   section.** "nvenc is the one hardware backend" will be false in the same
   version's notes the moment M100's bullet lands beneath it. Since neither
   has shipped, M100 may edit the M099 sentence rather than contradict it.
5. **The question (b) is trying to answer has a better-typed answer than a
   logical.** "What hardware can this machine encode `h264` with" is a
   character vector of backends — `character(0)`, `"videotoolbox"` — and
   composes with `hardware =` directly, which `TRUE` never can. That is an
   additive export beside the predicate, in the spirit of RR06 R11's
   `has_encoder(name)` row, and needs no rename window; it is not for M100.

## Recommendations

| # | Recommendation | Disposition |
|---|---|---|
| R1 | `hardware` has no default on `has_hardware_encoder()`; sweep the 31 call lines, the vignette chunk, `skip_if_no_nvenc()`, and drop `test-nvenc.R:13`'s default assertion | **apply** |
| R2 | `hardware` has no default on `hardware_encoder()` either; the pair share one signature and the predicate stays `hardware_encoder(codec, hardware) %in% pool` | **apply** |
| R3 | Helpers accept the backend set only, defined once as the table's names; `"none"` is refused by `arg_match()` with one sentence in `@param hardware` explaining the mismatch | **apply** |
| R4 | The (family, backend)-not-in-table refusal lives in the mapper; `codec_family()` stays backend-free and its abort stops naming nvenc (narrows T3's wording) | **apply** |
| R5 | Adopt the AC6 replacement; anchor the vignette range by content and drop the history clause | **apply**, wording edits optional |
| R6 | Put the 16 verbs' `@param hardware`/`@seealso` text under T8; raise a separate AC at the gate only if the maintainer wants it tested | **apply** (as task); the AC is the maintainer's call |
| R7 | Keep both helpers exported | **apply** |
| R8 | Exclude the two helpers from `nvenc_hardware_exports()`'s domain (AC1, memo grid, probe-blame) by name or property, with a comment; AC5 covers them | **apply** before AC1's sweep is extended |
| R9 | Widen `check_nvenc_available()`'s gate from `identical(hardware, "nvenc")` and assert the batch-verb blame under AC3 | **apply** |
| R10 | Default `"nvenc"` (option a) | **reject-with-reason**: a vendor as silent default under a vendor-free name, and `hardware_encoder("h264")` composes a false option on Apple hardware; the right answer only after 0.2.0 |
| R11 | Default "any available" (option b) | **reject-with-reason**: a `TRUE` that does not say which backend cannot pre-flight `hardware =`; the vignette would name the backend anyway; forces the pair to diverge |
| R12 | Accept `"none"` with a defined answer | **reject-with-reason**: `TRUE` is false without `libx264`, `FALSE` marks the always-working setting unavailable, and the mapper has nothing to return |
| R13 | A backend-vector export answering "what can this machine encode `h264` with" | **consider**, later and additive; not M100 |
