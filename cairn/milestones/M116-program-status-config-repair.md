<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M116: A broken or stale remembered location is reported, not fatal or silent

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — changes what the four `find_*()` exports,
  `program_status()`, `unset_program()` and the batch manifest's timeout
  warning do and say.
- **Branch/PR:** `m116-program-status-config-repair`

## Goal

Close the six defects M113's review measured in what `program_status()` and its
seam report about a remembered program location.

## Scope

**In:** `find_program()`'s handling of a config file it cannot read as one line;
the advice and condition class on its stale-location warning; which of its
warnings `program_status()` surfaces; the wording of `tool_versions()`'s timeout
warning; the memo drop on `unset_program()`'s partial-removal path; and
`tool_versions()`'s `locations` length check.

**Out:** naming the package's other unclassed aborts and warnings, including
`find_program()`'s plain not-found warning → the naming-pass candidate row.
Extending the timeout sweeps' coverage of `program_status()` → the
conditionally-spawning-member candidate row; this milestone changes that
warning's text, not the sweeps' domain. A fourth `program_status()` column
showing the remembered location → weighed and rejected at the plan gate, no row
(the warning carries the fact instead).

## Acceptance criteria

- [ ] AC1: A remembered-location file `readLines()` does not return exactly one
      line from — an empty file, or one holding two or more — makes
      `find_program()` warn with a `tidymedia_*` condition class carrying the
      program and the file path, and return `NULL`, where today it raises R's
      own `if` error. Tests cross both axes the state is free in: each of
      `find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()`, `find_mediainfo()`
      against the empty form, and each against the two-line form, asserting the
      class by name.
      `program_status()` over a config directory holding one such file returns
      four rows — the row whose `program` is that one carrying `NA` in
      `location` and `version`, the other three what the same call returns
      without the malformed file — and raises that warning once rather than
      suppressing it.
- [ ] AC2: `find_program()`'s warning for a remembered location whose binary is
      gone keeps its `set_program()` advice, adds `unset_program()` as the
      repair for the remembered location itself, adds the `install_on_win()`
      offer on the same derived condition the not-found branch uses
      (`tm_os()` is `"windows"` and the program is one `tm_install_registers`
      lists), and carries a `tidymedia_*` class holding the program and the
      location. Tests cross operating system against program, including the
      `windows` + `mediainfo` cell where the installer bullet must be absent.
- [ ] AC3: `program_status()` raises that stale-location warning rather than
      suppressing it, while a program that was never configured contributes
      none. A four-program call with one stale location and one
      never-configured program raises exactly one warning of that class,
      carrying the stale program's name. `?program_status`'s sentence promising
      `NA` "rather than a warning" narrows to the never-configured case, and
      `?find_program` documents both new conditions.
- [ ] AC4: `tool_versions()`'s `tidymedia_probe_timeout` warning names each
      timed-out program by the spelling `program_status()`'s `program` column
      uses (`"ffmpeg"`, not `"FFmpeg"`), and its remaining bullets read the same
      on both callers. Tests pin the emitted message on the `program_status()`
      path and on the `ffm_batch(manifest = TRUE)` path, and a mutant restoring
      the manifest-naming sentence turns the `program_status()` test red.
- [ ] AC5: `unset_program()` discards the memoized FFmpeg capabilities whenever
      a removal took, before aborting `tidymedia_location_not_removed`, and
      leaves them alone where nothing was removed. Tests fire both partial
      forms — current-directory file removed with the legacy file left, and the
      reverse — asserting the abort class and an empty memo, plus the
      total-failure case asserting the memo survives.
- [ ] AC6: `tool_versions()` aborts with a `tidymedia_*` class when a non-`NULL`
      `locations` differs in length from `programs`, leaving the `NULL` default
      path reaching `ffm_batch()` unchanged. Tests fire the silently-recycling
      length-1 case, the length-3-against-4 case base R warns on, and the
      empty-`list()` case, and assert `ffm_batch(manifest = TRUE)` still works.
- [ ] AC7: `devtools::test()` and `devtools::check()` clean (0 errors,
      0 warnings), `devtools::document()` produces no diff, and `NEWS.md`
      carries the user-visible changes.

## Coverage

- AC1 → T1, T2, T5
- AC2 → T3
- AC3 → T5
- AC4 → T6
- AC5 → T4
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1: Regression tests for the malformed remembered-location state, crossing
      the four `find_*()` exports against the empty and two-line forms, under
      `tm_redirect_config()` (`tests/testthat/helper-program-config.R`). Record
      in the work log that each is red first, and which R error it raises.
- [x] T2: `find_program()` guards what it read back before
      `R/program_management.R:79`'s `if (Sys.which(location) == "")` — the
      `length(loc) != 1L` shape `count_audio_streams()` documents at
      `R/ffprobe.R:213-219` — classing the malformed state and returning `NULL`.
- [x] T3: Widen and class the stale-location warning at
      `R/program_management.R:80-88`, deriving the installer bullet from
      `tm_os()`/`tm_install_registers` the way the not-found branch at `:101-104`
      does rather than restating the condition; tests cross OS against program.
- [x] T4: `unset_program()` drops the memo on any removal that took, above the
      `tidymedia_location_not_removed` abort at `R/program_management.R:291-303`;
      tests for both partial forms and the total-failure case.
- [x] T5: `program_status()` (`R/program_management.R:183-185`) stops suppressing
      the two classed config warnings and keeps suppressing the plain not-found
      one; narrow `?program_status`, document both conditions on
      `?find_program`, add the mixed four-program test.
- [x] T6: Reword `tool_versions()`'s timeout warning (`R/ffm_manifest.R:154-166`)
      to the `program` column spelling and a caller-neutral sentence about what
      `NA` means; rename the local that overwrites the `programs` argument at
      `:154`; pin the message on both callers with the mutation probe
      `tests/testthat/test-timeout-silence.R:660-680` models.
- [x] T7: `tool_versions()` length check below the `locations = NULL` default at
      `R/ffm_manifest.R:134`, with its three cases.
- [ ] T8: `NEWS.md` entry, `devtools::document()`, `devtools::check()`.

## Work log

- 2026-09-06: created by /milestone-plan.
- 2026-09-06: plan-gate criteria audit ran in full mode ([O], fresh context, user-facing tier), returning 15 findings and confirming all six premises; 11 with one clear repair were fixed in the criteria before writing (two instrument clauses moved to tasks, three probe families crossed, one census reworded, one substring grep replaced by a pinned message plus mutant, the length guard carved out from its own `NULL` default, the surface tier widened); 3 went to the gate as questions and 1 (D044's route census) to D089.
- 2026-09-06: plan gate chose a classed warning returning `NULL` for a malformed config file over a classed abort and over leaving each caller to guard, because a four-program report must still answer for the other three; falsified by a caller that cannot distinguish "not found" from "found nothing readable" and needs to.
- 2026-09-06: plan gate chose surfacing the stale-location warning from `program_status()` over a fourth column reporting the remembered location, because the column changes the shape every reader of that table sees for a state that is rare and repairable; falsified by a caller that must branch on staleness programmatically rather than be told about it.
- 2026-09-06: plan gate chose dropping the memo on any removal that took over dropping it on every exit, because a total failure removed nothing and the memo still describes the binary lookups keep answering with; falsified by a partial-removal form where "took" is not decidable from the filesystem.
- 2026-09-06: plan gate chose classing only the two warnings this milestone changes over classing `find_program()`'s plain not-found warning too, because that name is the naming pass's call; falsified by the two new classes proving unusable without the third.
- 2026-09-06: implement question gate named the three new condition classes, all three recommendations taken: `tidymedia_location_unreadable` (`tm_program`, `tm_file`) for a config file that is not one line, `tidymedia_location_gone` (`tm_program`, `tm_location`) for a remembered location whose binary is absent, and `tidymedia_locations_mismatch` for `tool_versions()`'s length refusal.
- 2026-09-06: T1 red first — all four `find_*()` exports against both malformed forms aborted rather than warned; measured errors were `argument is of length zero` (empty file) and `the condition has length > 1` (two lines).
- 2026-09-06: T2 — `find_program()` guards the read-back with the `length() != 1L` shape before the `Sys.which()` test, warning `tidymedia_location_unreadable` (`tm_program`, `tm_file`) and returning `NULL`; T1's 40 assertions green.
- 2026-09-06: T3 — stale-location warning classed `tidymedia_location_gone` (`tm_program`, `tm_location`) and widened with the `unset_program()` repair; the installer offer moved into `tm_install_bullet()`, which both find_program() branches now splice, and tests cross three operating systems against four programs on each branch.
- 2026-09-06: T4 — the memo drop moved above the `tidymedia_location_not_removed` abort and keyed on `length(left) < length(present)`, replacing the success-path-only call; mutant check with the guard forced to `FALSE` reddened both partial forms and the total-success case and left the removed-nothing case green.
- 2026-09-06: T5 — `program_status()` selects by the two classes to be raised rather than by the one to be muffled; `?program_status` narrowed to the never-configured case and `?find_program` documents both conditions with their fields.
- 2026-09-06: T6 — the timeout warning names programs from `names(probes)` under a separate local, and its second bullet says what `NA` means without naming a caller; two M69-era tests that pinned the display-label spelling updated, and a mutant carrying the retired manifest sentence fails the new assertions.
- 2026-09-06: T7 — `tool_versions()` aborts `tidymedia_locations_mismatch` (`tm_n_programs`, `tm_n_locations`) on a non-`NULL` `locations` of the wrong length, above the resolution of the `NULL` default.
- 2026-09-06: minor amendment — `?refresh_ffmpeg_capabilities` said there were two ways to discard the memo; with `unset_program()` it is three, so the list was corrected alongside T4's change.

## Decisions

## Review
