# M121: A directory becomes a jobs tibble

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — one new export
- **Branch/PR:** `m121-directory-to-jobs-tibble`

## Goal

One export turns a directory of media files into the jobs tibble `ffm_batch()`
takes, so the batch story does not start with a hand-rolled `list.files()`.

## Scope

**In:** one new exported function, its refusals, its reference page and index row,
and the two vignette lines that stop hand-rolling the call.

**Out:** an `output` column convention or a path-deriving helper → not this
milestone; the demonstrated call derives its own. A `_batch` sibling → D014 requires
one for a task verb, and this is a metadata utility that produces the jobs table
rather than consuming it. Resume or skip-existing for long batches → ROADMAP
candidate row. Anything that grows toward FFmpeg feature coverage → GP1, D001.

## Acceptance criteria

- [x] AC1: A newly exported function returns a tibble carrying an `input` column of
      full paths to the media files in a named directory, selected by a type or
      extension argument, and a demonstrated `ffm_batch()` call consumes that
      tibble's `input` column together with an `output` column derived from it,
      without reshaping the returned object.
- [x] AC2: These three refusals — a directory that does not exist, a type outside the
      accepted set, and a call matching no file — are each raised with the frame of
      the verb the caller typed, per D074 and D087.
- [x] AC3: `_pkgdown.yml` carries a reference-index row for the new export and `man/`
      a topic with a `\value{}` section and a runnable example, both in the commit
      that exports it.
- [x] AC4: `vignettes/workflow.Rmd:40` and `vignettes/metadata.Rmd:121` call the new
      export in place of their hand-rolled `list.files()`; those are the only two
      such calls in `vignettes/` as measured 2026-09-07.
- [ ] AC5: `devtools::check()` reports 0 errors and 0 warnings, and the `verify` slot
      of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1: Settle the export's name and its selector argument's name under D014 and
      D078's full-word compound rule. Note D079: if every candidate default for the
      type argument is one member of the set it ranges over, the argument takes no
      default.
- [x] T2: Implement, returning a tibble with `input` as a full path.
- [x] T3: Add the three refusals, threading `call` through an internal implementation
      per D087 — `tests/testthat/test-exported-call-formal.R` enforces that no export
      publishes the internal `call` formal. Fire every `cli_abort()` branch per the
      profile's test-doctrine, including the argument-form refusals beyond the three
      AC2 names.
- [x] T4: Roxygen, `devtools::document()`, `_pkgdown.yml` row.
- [x] T5: Rewrite the two vignette lines; rebuild.
- [ ] T6: `devtools::test()`, `devtools::check()`, `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), fresh-context [O] reader. Findings against this milestone: AC1's "accepts that tibble with no reshaping" was satisfiable by an `.f` that ignores its arguments (repaired — the demonstrated call now names the columns it consumes); AC2's "Each of its refusals" quantified over the function's whole refusal set while the em-dash list enumerated three by hand, leaving the argument-form refusals and D074's `resolve_timeout()` refusal outside it (repaired — narrowed to "These three refusals", with the remainder moved to T3 under the profile's every-branch doctrine). D079's no-default rule and D014/D078's naming constraints were flagged as unpriced by the criteria and moved to T1. AC4 passed all six questions clean.
- 2026-09-07: plan gate chose one directory-listing export over refusing the feature under GP1, because batch over many files is a stated differentiator in DESIGN.md's purpose and both batch-teaching vignettes hand-roll the same `list.files()` shape to feed `ffm_batch()`; falsified by the export going uncited in the vignettes and README a release later.
- 2026-09-10: implement session started; branch `m121-directory-to-jobs-tibble` cut from `origin/master` at cc4761b.
- 2026-09-10: T1 question gate settled three open choices, all at the recommendation. The export is `ffm_jobs(directory, type, extension = NULL, recursive = FALSE)`. Milestone-local decision **M121-1 — `ffm_jobs()` is Layer-1 surface, its `type` is required, and its extension vocabulary is closed** (applies D014's `ffm_*` rule, D078's naming rule and D079's no-default rule; leaves all three standing): the jobs-table constructor takes the `ffm_*` prefix, because DESIGN's Layer-1 family already holds `ffm_batch()` and `ffm_manifest()`, neither of which assembles a command — the family is the engine's surface, not command assembly alone, so `ffm_jobs()` sorts beside the runner that consumes it. `type` takes no default: its three values (`"video"`, `"audio"`, `"image"`) are the set it ranges over and any default would be one member of it, which is D079's rule exactly; `recursive = FALSE` keeps its default because `FALSE` is a toggle's off position, D079's named carve-out. `type`'s vocabulary and each type's extension list are closed, and `extension` may only narrow within the type it is given — a closed set is what lets both refusals name what they accept. The cost is a caller whose container is outside the lists: an `.mxf` folder gets "No video files were found" though FFmpeg would read it. That is GP1's trade taken deliberately — growing the lists toward FFmpeg's container coverage is the drift D001 refuses, and such a caller uses `list.files()` directly as they did before this export existed. The returned tibble carries `input` and nothing else, because `ffm_batch()` passes every column to `.f` by name and a second column would become an argument every `.f` has to accept. **Falsified by** a report of a media file the closed extension lists miss on a machine whose FFmpeg reads it, which turns the refusal into a wrong answer rather than a scope line.
- 2026-09-10: minor plan amendment — T1, T2 and T3 land in one commit rather than three. The decision, the body and the refusals are one file (`R/ffm_jobs.R`) written once; splitting them into three commits would have staged a function with no guards and no tests as an intermediate state. No criterion, scope line or task text changed.
- 2026-09-10: T2/T3 — `R/ffm_jobs.R` implements `ffm_jobs()` as a wrapper over the internal `tm_ffm_jobs(..., call)` per D087, plus the closed vocabulary in `media_types()` / `media_extensions()`. Eight `cli_abort()`/rlang branches, every one fired by `tests/testthat/test-ffm-jobs.R` (82 assertions): AC2's three named refusals, a path that exists but is not a directory, the argument-form refusals on `directory`, `type` (missing and non-string), `recursive` and `extension`, and an `extension` outside its type. Blame is read with `blamed_verb()` on each and is `ffm_jobs` throughout. AC1's hand-off is tested by an `ffm_batch(run = FALSE)` call over the unreshaped table plus a derived `output`, asserting each compiled command names its own row's input and output — an `.f` ignoring its arguments fails it.
- 2026-09-10: T4 — roxygen written, `devtools::document()` run (`NAMESPACE` gains `ffm_jobs`, `man/ffm_jobs.Rd` written with `\value{}` and a runnable example), `_pkgdown.yml` gains the reference row next to `ffm_batch`. `NEWS.md` gains the New-features entry the consistency gate requires.
- 2026-09-10: T5 — `vignettes/workflow.Rmd:40` and `vignettes/metadata.Rmd:121` now call `ffm_jobs()`; `grep -rn "list.files" vignettes/` returns nothing. The workflow vignette's downstream jobs-table block dropped its hand-built `tibble::tibble()` and adds `output` to the returned table instead.
