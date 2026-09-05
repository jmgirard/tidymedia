# M113: One call says which programs tidymedia found, and a remembered location can be forgotten

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — two new exports
- **Branch/PR:** —

## Goal

Export `program_status()`, which reports the resolved path and reported version
of all four programs in one call, and `unset_program()`, which removes a
remembered location `set_program()` wrote.

## Scope

**In:** `program_status()` returning one row per program for `ffmpeg`,
`ffprobe`, `ffplay` and `mediainfo`; `unset_program(program)` removing the
`<program>_location.txt` file `set_program()` wrote (`R/program_management.R:20`)
and, where one exists, the pre-M097 `rappdirs` file `find_program()` still
reads (`:59-101`); both wired into `_pkgdown.yml`, `NEWS.md` and the program
management family.

**Out:** four `unset_*()` wrappers mirroring the `set_*()` exports — one
generic is the reversible default; the wrappers become a candidate row.
Migrating or rewriting the legacy file → nothing here changes what
`find_program()` reads, only what exists for it to read. A dispatching
`install_ffmpeg()` → declined at M108's gate, unchanged. Version probes for
`ffplay` and `mediainfo` beyond what each binary's own version flag answers.
Teaching either function in a vignette → M114.

## Acceptance criteria

- [ ] AC1: `program_status()` returns a tibble with one row for each of
      `ffmpeg`, `ffprobe`, `ffplay` and `mediainfo`, carrying the resolved path
      or `NA` and the version string the binary reported or `NA`. Evidence: the
      printed tibble in two states — every program present, and none resolvable
      (an emptied `PATH` AND a redirected empty config dir, since `PATH` alone
      leaves `find_program()`'s config and legacy lookups intact).
- [ ] AC2: `program_status()` returns that tibble rather than aborting or
      warning when a program is unresolvable, for each of the four taken alone.
      Evidence: four runs, one program hidden per run.
- [ ] AC3: After `set_program()` writes a location and `unset_program()` runs
      for that program, no file named `<program>_location.txt` exists under
      either the current config dir or the legacy `rappdirs` dir, and
      `find_program()` returns what it returned before the `set_program()` call
      — including `NULL` with its warning where that is what it returned.
      Evidence: a test over the three states (never remembered, remembered,
      forgotten) x two config locations, showing the file paths and the
      `find_program()` answer at each.
- [ ] AC4: `unset_program()` called for a program with no remembered location
      raises a classed condition carrying a `tm_program` field, per D062's
      naming rule and D086's field rule, rather than failing silently or
      unclassed. Evidence: the class vector and the message.
      (RB tripwire: irreversible-api)
- [ ] AC5: Both exports appear in `_pkgdown.yml`, carry `@examplesIf`-guarded
      examples where they touch a binary, join `@family program management
      functions`, and have a `NEWS.md` entry.
- [ ] AC6: `devtools::document()` produces no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings).

## Coverage

- AC1 → T2, T3
- AC2 → T3
- AC3 → T4, T5
- AC4 → T4, T5
- AC5 → T6
- AC6 → T6

## Tasks

- [ ] T1: Settle the two names against D014/D078 before writing either body:
      `program_status()` and `unset_program()` pair with the family's existing
      `find_program()`/`set_program()`, and `get_*` is reserved for per-file
      metadata scalars. (RB tripwire: irreversible-api)
- [ ] T2: Build `program_status()` over the internal `tool_versions()` shape
      (`R/ffm_manifest.R:121`), widened from two programs to four; a program
      with no version flag answer contributes `NA`, not an abort.
- [ ] T3: Tests for AC1 and AC2, including the both-hidden state — emptied
      `PATH` plus a redirected `R_USER_CONFIG_DIR` — and one run per program.
- [ ] T4: `unset_program()`: remove the current-dir file and any legacy file,
      report a classed refusal when neither exists.
- [ ] T5: Tests for AC3 and AC4 across the three states and both config
      locations. The M097 lesson holds: one `R_USER_CONFIG_DIR` collapses the
      two directories together on Windows, so redirect the legacy library by a
      recording mock rather than by the envvar.
- [ ] T6: Roxygen, `_pkgdown.yml` rows, `NEWS.md`, `document()`, `test()`,
      `check()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned three findings against this milestone's draft: the absent-program evidence used an emptied `PATH`, which `find_program()`'s config and legacy fallbacks defeat; the forget criterion compared against a pre-`set_program()` state that need not exist and named one config file where two can hold a location; and the criteria said "a new exported function" without naming it, so nothing bound them to one pair. All three fixed before writing; none needed a gate question.
- 2026-09-05: plan gate chose one generic `unset_program()` over four `unset_*()` wrappers mirroring the `set_*()` exports, because the wrappers are additive later and four exports is the larger trade against GP1. Falsified by a report that the asymmetry with `set_ffmpeg()` and its siblings is what a caller trips on.
- 2026-09-05: plan gate chose widening the internal `tool_versions()` (`R/ffm_manifest.R:121`) over a fresh probe, because it already spawns `-version` for two of the four programs and is the shape the manifest records. Falsified by a program whose version flag that helper's parsing cannot read.

## Decisions

## Review
