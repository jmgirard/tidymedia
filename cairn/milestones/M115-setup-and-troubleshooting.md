# M115: A new user can tell whether the setup worked, and fix it when it did not

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** —
- **Surface tier:** user-facing — the README and the getting-started path every new user takes
- **Resolves:** —
- **Branch/PR:** `m115-setup-and-troubleshooting`

## Goal

Close the getting-started gap: no "did it work?" step after installing the
binaries, a macOS route that dead-ends off `PATH`, and README chunks that
execute without guarding on the binaries they need.

## Scope

**In:** a verification step in `README.Rmd` after each platform's install
instructions, built on `program_status()` (M113); the macOS manual route at
`README.Rmd:77-81`, which drags FFmpeg into `/Applications` and never mentions
`set_ffmpeg()`; guards on `README.Rmd:110-133`'s executing chunks; naming
`install_on_win()` and `set_program()` in `find_program()`'s not-found warning
(`R/program_management.R:95-99`); the `find_program` help-page example, which
emits two warnings unconditionally on a machine without the binaries.

**Out:** a macOS or Linux downloader → the standing candidate row, declined at
M108's gate under GP1. A setup vignette — the README is where a new reader
already is; a vignette becomes a candidate row if the README section outgrows
it. Everything M114 covers.

## Acceptance criteria

- [ ] AC1: Each of the three platform sections in `README.Rmd` ends with a
      runnable check whose output tells the reader whether the binary was
      found, and with the recovery call to make when it was not. Evidence: the
      three sections quoted from the knitted `README.md`.
- [ ] AC2: The macOS manual route names the step that makes an FFmpeg outside
      `PATH` usable. Evidence: the section quoted, and a transcript of that
      route followed to a working `find_ffmpeg()`.
- [ ] AC3: Every chunk in `README.Rmd` whose `eval` option is not `FALSE` and
      which calls a function that spawns a program is guarded on that program's
      presence, verified by the same knitr-parsing sweep M114 commits, run over
      `README.Rmd`. Evidence: the sweep's listing of every chunk in the file.
- [ ] AC4: `devtools::build_readme()` succeeds with no FFmpeg, ffprobe or
      MediaInfo reachable on a `PATH` that still reaches pandoc, and the
      knitted `README.md` contains no error output. Evidence: the build log and
      the three `Sys.which()` answers recorded as empty.
- [ ] AC5: `find_program()`'s not-found warning names the recovery calls
      available on the platform it is running on, and the M110 condition
      contract is unchanged — the warning keeps its class and fields. Evidence:
      the warning text on each platform branch and the class vector.
- [ ] AC6: `man/find_program.Rd`'s example produces no warning on a machine
      with no binaries. Evidence: the example run under an emptied `PATH` and a
      redirected config dir.
- [ ] AC7: `devtools::document()` produces no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings).

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T3
- AC5 → T4
- AC6 → T5
- AC7 → T6

## Tasks

- [x] T1: Add the verification step to each platform section, calling
      `program_status()` from M113.
- [x] T2: Rewrite the macOS manual route so it ends at a working lookup, and
      follow it once end to end rather than deriving the prose from the code.
- [ ] T3: Guard `README.Rmd:110-133`'s chunks in the idiom the vignettes use,
      run M114's chunk sweep over the file, and re-knit. The M089 lesson holds:
      `build_readme()` always reports a `temp_libpath` diff on two knitted
      `ffm_compile()` outputs — check that this is the only unrelated diff and
      revert it.
- [ ] T4: Widen `find_program()`'s not-found warning (`:95-99`) to name
      `install_on_win()` on Windows and `set_program()` everywhere, keeping its
      class and fields.
- [ ] T5: Guard the `find_program` example with `@examplesIf` or make it
      non-spawning.
- [ ] T6: `document()`, `test()`, `check()`, `NEWS.md` entry for the warning
      change.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader, over the draft this milestone was split from (M114's). Its chunk-guard and `PATH`-emptied findings applied here identically and are fixed in AC3 and AC4; no finding was specific to this half.
- 2026-09-05: plan gate chose splitting the setup and troubleshooting half out of the documentation scope the user selected, over one large docs milestone, because this half depends on M113's `program_status()` and the other half depends on nothing. Falsified by the two halves needing to cross-reference each other so tightly that they cannot be reviewed apart.
- 2026-09-05: plan gate chose fixing the README over adding a setup vignette, because a reader hitting a missing binary is already in the README and installing the package is the only step before it. Falsified by the section outgrowing the README, which is the candidate row's trigger.
- 2026-09-06: gate chose a shown-not-run check block (a live chunk would ship the maintainer's own paths and version as the reader's example answer, and would need a guard that blanks it on the machine the section is written for), one check per platform per program, and extending `tools/build_vignettes_without_binaries.R` rather than adding a second script.
- 2026-09-06: T2 done. The macOS manual route now names the separate `ffprobe` download, adds the `set_ffmpeg()`/`set_ffprobe()` step the Applications folder makes necessary, and ends at `program_status()`. Route followed once end to end on macOS 26.6.2 with a `PATH` reaching neither program and the config seam redirected: `find_ffmpeg()` warned "Failed to find ffmpeg", the two `set_*()` calls returned `TRUE`, and `find_ffmpeg()` then returned the staged path with `program_status()` showing both versions. The staged binaries were copies of this machine's own FFmpeg 9.0.1 rather than a fresh evermeet.cx download -- the step under test is the lookup, not the download. The two evermeet.cx addresses were checked by their `content-disposition` filenames (`ffmpeg-126386-gc27482a18d7.7z`, `ffprobe-9.0.1.7z`, observed 2026-09-06), which is what says the snapshot address gives `ffmpeg` alone.
- 2026-09-06: T1 done. Seven `program_status()` check steps added to `README.Rmd`, one per platform route under each program, each with the recovery call for that route. Recovery paths are placeholders rather than asserted install locations, except the two the README already named. `README.md` is re-knitted once in T3.

## Decisions

## Review
