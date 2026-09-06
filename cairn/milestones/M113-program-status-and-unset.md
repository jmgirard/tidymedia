# M113: One call says which programs tidymedia found, and a remembered location can be forgotten

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — two new exports
- **Branch/PR:** `m113-program-status-and-unset`

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

- [x] T1: Settle the two names against D014/D078 before writing either body:
      `program_status()` and `unset_program()` pair with the family's existing
      `find_program()`/`set_program()`, and `get_*` is reserved for per-file
      metadata scalars. (RB tripwire: irreversible-api)
- [x] T2: Build `program_status()` over the internal `tool_versions()` shape
      (`R/ffm_manifest.R:121`), widened from two programs to four; a program
      with no version flag answer contributes `NA`, not an abort.
- [x] T3: Tests for AC1 and AC2, including the both-hidden state — emptied
      `PATH` plus a redirected `R_USER_CONFIG_DIR` — and one run per program.
- [x] T4: `unset_program()`: remove the current-dir file and any legacy file,
      report a classed refusal when neither exists.
- [x] T5: Tests for AC3 and AC4 across the three states and both config
      locations. The M097 lesson holds: one `R_USER_CONFIG_DIR` collapses the
      two directories together on Windows, so redirect the legacy library by a
      recording mock rather than by the envvar.
- [x] T6: Roxygen, `_pkgdown.yml` rows, `NEWS.md`, `document()`, `test()`,
      `check()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned three findings against this milestone's draft: the absent-program evidence used an emptied `PATH`, which `find_program()`'s config and legacy fallbacks defeat; the forget criterion compared against a pre-`set_program()` state that need not exist and named one config file where two can hold a location; and the criteria said "a new exported function" without naming it, so nothing bound them to one pair. All three fixed before writing; none needed a gate question.
- 2026-09-05: plan gate chose one generic `unset_program()` over four `unset_*()` wrappers mirroring the `set_*()` exports, because the wrappers are additive later and four exports is the larger trade against GP1. Falsified by a report that the asymmetry with `set_ffmpeg()` and its siblings is what a caller trips on.
- 2026-09-05: plan gate chose widening the internal `tool_versions()` (`R/ffm_manifest.R:121`) over a fresh probe, because it already spawns `-version` for two of the four programs and is the shape the manifest records. Falsified by a program whose version flag that helper's parsing cannot read.
- 2026-09-05: T2/T4 — `program_status()` and `unset_program()` written in `R/program_management.R`; `tool_versions()` widened with a `programs`/`locations` pair so the manifest's own two-program call is unchanged, and `parse_version_line()` gained MediaInfo's `--version` shape, which names no "version" token.
- 2026-09-05: T3/T5 — tests in `tests/testthat/test-program-status-and-unset.R`; the three fixtures the family shares moved from `test-program-management.R` to `helper-program-config.R` so a second test file can see them.
- 2026-09-05: T6 discovery, minor amendment — `program_status()` joins the computed timeout domain (it spawns), and the M094 blame sweep found it raising nothing for an invalid `options(tidymedia.timeout = )`: `capture_version()`'s `tryCatch()` turned that refusal into a silent NA. `tool_versions()` now resolves the limit once above the probes and threads `call`, so the refusal blames `program_status()`. Registered in `tm_timeout_recorded_domain()`, the call-spec table and the absorber partition; the two written-membership censuses measured at `ae5ff1c` get a named `tm_timeout_post_baseline()` exclusion, since a function that did not exist then has no pre-change reading to record.
- 2026-09-05: T6 second sweep finding — the M096 wrong-argument sweep asserts its table covers the whole timeout domain, and `program_status()` takes no arguments, so it can contribute no cell. `tm_timeout_argumentless()` computes that set and the test pins it, so a second argumentless export reddens there rather than vanishing from the sweep.
- 2026-09-05: T6 — roxygen, `_pkgdown.yml` rows, `NEWS.md`; `devtools::document()` no diff, `pkgdown::check_pkgdown()` clean, `devtools::test()` 12,843 pass / 0 fail / 18 skip, `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-09-05: implement question gate settled four choices — both names as planned, the three-column shape, a warning rather than an abort for nothing-to-forget, and no `confirm` argument. Recorded below; T1 closed by the naming half.

## Decisions

### The two names, and what `unset_program()` does at its two edges (2026-09-05, implement question gate)

`program_status()` and `unset_program()` ship as planned. Both name a category
rather than one member of an open set, and neither takes the `get_*` prefix the
package reserves for per-file metadata scalars; the pair reads against the
family's existing `find_program()` / `set_program()`, which is what a reader
guesses from. `program_versions()` was declined because the tibble also carries
the resolved location, and `forget_program()` because it breaks the set/unset
pairing.

The tibble is three columns: `program`, `location`, `version`, one row per
program, `NA` in either value column where there is no answer. The path column
is `location` because that is what `set_program(location =)` and
`find_program()` already call it. A fourth `found` logical was declined: it
restates `is.na(location)`.

`unset_program()` on a program with nothing remembered warns and returns
`FALSE` invisibly rather than aborting. The end state the caller asked for
already holds, so a repeated call is a no-op and not a failure; this is how
`find_program()` reports a program it cannot find. The warning is classed
`tidymedia_no_remembered_location` — the event, not the severity — and carries
`tm_program`.

`unset_program()` takes no `confirm` argument. `set_program()` asks because
writing a file that outlives the session is a side effect of a call named
"set"; deleting that file is the whole of what `unset` names, so a prompt would
ask the caller to confirm the thing they typed.

- **Falsified by** a caller who calls `unset_program()` by mistake and cannot
  recover the path it forgot, or by a handler that needs the nothing-remembered
  case to stop a script.

## Review
