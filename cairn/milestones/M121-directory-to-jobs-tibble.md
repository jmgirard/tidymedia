# M121: A directory becomes a jobs tibble

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — one new export
- **Branch/PR:** —

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

- [ ] AC1: A newly exported function returns a tibble carrying an `input` column of
      full paths to the media files in a named directory, selected by a type or
      extension argument, and a demonstrated `ffm_batch()` call consumes that
      tibble's `input` column together with an `output` column derived from it,
      without reshaping the returned object.
- [ ] AC2: These three refusals — a directory that does not exist, a type outside the
      accepted set, and a call matching no file — are each raised with the frame of
      the verb the caller typed, per D074 and D087.
- [ ] AC3: `_pkgdown.yml` carries a reference-index row for the new export and `man/`
      a topic with a `\value{}` section and a runnable example, both in the commit
      that exports it.
- [ ] AC4: `vignettes/workflow.Rmd:40` and `vignettes/metadata.Rmd:121` call the new
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

- [ ] T1: Settle the export's name and its selector argument's name under D014 and
      D078's full-word compound rule. Note D079: if every candidate default for the
      type argument is one member of the set it ranges over, the argument takes no
      default.
- [ ] T2: Implement, returning a tibble with `input` as a full path.
- [ ] T3: Add the three refusals, threading `call` through an internal implementation
      per D087 — `tests/testthat/test-exported-call-formal.R` enforces that no export
      publishes the internal `call` formal. Fire every `cli_abort()` branch per the
      profile's test-doctrine, including the argument-form refusals beyond the three
      AC2 names.
- [ ] T4: Roxygen, `devtools::document()`, `_pkgdown.yml` row.
- [ ] T5: Rewrite the two vignette lines; rebuild.
- [ ] T6: `devtools::test()`, `devtools::check()`, `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), fresh-context [O] reader. Findings against this milestone: AC1's "accepts that tibble with no reshaping" was satisfiable by an `.f` that ignores its arguments (repaired — the demonstrated call now names the columns it consumes); AC2's "Each of its refusals" quantified over the function's whole refusal set while the em-dash list enumerated three by hand, leaving the argument-form refusals and D074's `resolve_timeout()` refusal outside it (repaired — narrowed to "These three refusals", with the remainder moved to T3 under the profile's every-branch doctrine). D079's no-default rule and D014/D078's naming constraints were flagged as unpriced by the criteria and moved to T1. AC4 passed all six questions clean.
- 2026-09-07: plan gate chose one directory-listing export over refusing the feature under GP1, because batch over many files is a stated differentiator in DESIGN.md's purpose and both batch-teaching vignettes hand-roll the same `list.files()` shape to feed `ffm_batch()`; falsified by the export going uncited in the vignettes and README a release later.
