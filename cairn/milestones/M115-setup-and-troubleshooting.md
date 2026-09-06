# M115: A new user can tell whether the setup worked, and fix it when it did not

- **Status:** review
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** —
- **Surface tier:** user-facing — the README and the getting-started path every new user takes
- **Resolves:** —
- **Branch/PR:** `m115-setup-and-troubleshooting` / https://github.com/jmgirard/tidymedia/pull/119

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

- [x] AC1: Each of the three platform sections in `README.Rmd` ends with a
      runnable check whose output tells the reader whether the binary was
      found, and with the recovery call to make when it was not. Evidence: the
      three sections quoted from the knitted `README.md`.
- [x] AC2: The macOS manual route names the step that makes an FFmpeg outside
      `PATH` usable. Evidence: the section quoted, and a transcript of that
      route followed to a working `find_ffmpeg()`.
- [x] AC3: Every chunk in `README.Rmd` whose `eval` option is not `FALSE` and
      which calls a function that spawns a program is guarded on that program's
      presence, verified by the same knitr-parsing sweep M114 commits, run over
      `README.Rmd`. Evidence: the sweep's listing of every chunk in the file.
- [x] AC4: `devtools::build_readme()` succeeds with no FFmpeg, ffprobe or
      MediaInfo reachable on a `PATH` that still reaches pandoc, and the
      knitted `README.md` contains no error output. Evidence: the build log and
      the three `Sys.which()` answers recorded as empty.
- [x] AC5: `find_program()`'s not-found warning names the recovery calls
      available on the platform it is running on, and the M110 condition
      contract is unchanged — the warning keeps its class and fields. Evidence:
      the warning text on each platform branch and the class vector.
- [x] AC6: `man/find_program.Rd`'s example produces no warning on a machine
      with no binaries. Evidence: the example run under an emptied `PATH` and a
      redirected config dir.
- [x] AC7: `devtools::document()` produces no diff; `devtools::test()` and
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
- [x] T3: Guard `README.Rmd:110-133`'s chunks in the idiom the vignettes use,
      run M114's chunk sweep over the file, and re-knit. The M089 lesson holds:
      `build_readme()` always reports a `temp_libpath` diff on two knitted
      `ffm_compile()` outputs — check that this is the only unrelated diff and
      revert it.
- [x] T4: Widen `find_program()`'s not-found warning (`:95-99`) to name
      `install_on_win()` on Windows and `set_program()` everywhere, keeping its
      class and fields.
- [x] T5: Guard the `find_program` example with `@examplesIf` or make it
      non-spawning.
- [x] T6: `document()`, `test()`, `check()`, `NEWS.md` entry for the warning
      change.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader, over the draft this milestone was split from (M114's). Its chunk-guard and `PATH`-emptied findings applied here identically and are fixed in AC3 and AC4; no finding was specific to this half.
- 2026-09-05: plan gate chose splitting the setup and troubleshooting half out of the documentation scope the user selected, over one large docs milestone, because this half depends on M113's `program_status()` and the other half depends on nothing. Falsified by the two halves needing to cross-reference each other so tightly that they cannot be reviewed apart.
- 2026-09-05: plan gate chose fixing the README over adding a setup vignette, because a reader hitting a missing binary is already in the README and installing the package is the only step before it. Falsified by the section outgrowing the README, which is the candidate row's trigger.
- 2026-09-06: gate chose a shown-not-run check block (a live chunk would ship the maintainer's own paths and version as the reader's example answer, and would need a guard that blanks it on the machine the section is written for), one check per platform per program, and extending `tools/build_vignettes_without_binaries.R` rather than adding a second script.
- 2026-09-06: T3 done. `README.Rmd`'s setup chunk now sets `has_ffmpeg`/`has_ffprobe`/`has_mediainfo` and writes its `Sys.which()` answers to stderr, in the vignettes' idiom; the three chunks that start a program carry a guard naming it. The two chunks that only compile a command (`ffm_compile()`, `extract_audio(run = FALSE)`) were measured to start nothing and to work with no binaries reachable, so they carry no guard. Both `tools/` sweeps now cover `README.Rmd` beside the vignettes and both report clean over six files (`unguarded spawning chunks: none`; `chunks starting a program their guard does not name: none`). Filenames kept their `vignette_` prefix: renaming them would strand M114's archive pointer and force a correction line onto an over-budget `ROADMAP.md`.
- 2026-09-06: T6 done. `NEWS.md` gains two Documentation bullets (the README's checks and the macOS manual route; the guarded example and chunks) and one Configuration bullet for the widened warning. `devtools::document()` no diff; `devtools::test()` 0 failures, 12,940 passing, 18 skipped; `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes (7m 26s). The first check run raised one spelling NOTE on "README's" and "findable"; both were reworded rather than added to the wordlist, and the three tools were re-run after that edit -- both sweeps clean, the no-binaries README build clean with `Sys.which(): ffmpeg=[] ffprobe=[] mediainfo=[]` in its log.
- 2026-09-06: T5 done. The `find_program` help-page example carries `@examplesIf nzchar(Sys.which("ffmpeg")) && nzchar(Sys.which("mediainfo"))`, the idiom `program_status()` already uses. The Rd's own extracted example (`tools::Rd2ex`) was sourced under an emptied `PATH` and a redirected config seam, with `find_ffmpeg()` confirmed NULL there: 0 conditions signalled. Control on this machine, binaries present: both calls run and print their paths, so the guard silences the warning rather than the example.
- 2026-09-06: T4 done. `find_program()`'s not-found warning now says "Check that it is installed, then use `set_<program>()` to point tidymedia at it", and on Windows adds an `install_on_win()` bullet for the three programs that installer registers. The two facts are read from the installer's own seam and list (`tm_os()`, `tm_install_registers`), not restated, so the advice cannot drift from what the installer does; naming it for `mediainfo` or on a Mac would point at a call that refuses the caller. Three tests over the four programs crossed with windows/darwin/linux. The condition's class vector and field names were measured on the pre-change code at 78263f7 (`rlang_warning`/`warning`/`condition`; `call`, `footer`, `message`, `parent`) and are asserted against those figures rather than against the new code. Each new test shown red first: making the offer unconditional reddens the platform test, dropping the `set_<program>()` clause reddens the everywhere test.
- 2026-09-06: T3 also extended `tools/build_vignettes_without_binaries.R` with a `readme`/`both` target, per the gate. The README target builds in a scratch copy of the tree so the working tree's `README.md` is not overwritten with a binary-less knit. Proved able to fail before being trusted: with the `probe_all()` guard removed the build exits 1 (`Quitting from README.Rmd:193-195`), and with that chunk also set to `error = TRUE` the artifact scan catches `#> Warning: Failed to find ffprobe.` and `#> Error in purrr::map()` and exits 1.
- 2026-09-06: `README.md` re-knitted. The M089 `temp_libpath` diff appeared on exactly the two `ffm_compile()` output lines and was reverted; nothing else unrelated changed. `devtools::test()`: 0 failures, 12900 passing, 18 skipped.
- 2026-09-06: T2 done. The macOS manual route now names the separate `ffprobe` download, adds the `set_ffmpeg()`/`set_ffprobe()` step the Applications folder makes necessary, and ends at `program_status()`. Route followed once end to end on macOS 26.6.2 with a `PATH` reaching neither program and the config seam redirected: `find_ffmpeg()` warned "Failed to find ffmpeg", the two `set_*()` calls returned `TRUE`, and `find_ffmpeg()` then returned the staged path with `program_status()` showing both versions. The staged binaries were copies of this machine's own FFmpeg 9.0.1 rather than a fresh evermeet.cx download -- the step under test is the lookup, not the download. The two evermeet.cx addresses were checked by their `content-disposition` filenames (`ffmpeg-126386-gc27482a18d7.7z`, `ffprobe-9.0.1.7z`, observed 2026-09-06), which is what says the snapshot address gives `ffmpeg` alone.
- 2026-09-06: T1 done. Seven `program_status()` check steps added to `README.Rmd`, one per platform route under each program, each with the recovery call for that route. Recovery paths are placeholders rather than asserted install locations, except the two the README already named. `README.md` is re-knitted once in T3.
- 2026-09-06: review ran. All seven criteria met with fresh evidence; consistency gate pass; three fresh-context reviewers returned eleven findings, none reaching the return floor. PR #119.

## Decisions

## Review

Verified 2026-09-06 on macOS 26.6.2 against branch `m115-setup-and-troubleshooting`
at `b5ad99e`, PR #119.

- **AC1 — met.** All seven platform install routes in the knitted `README.md`
  end with a `tidymedia::program_status()` step naming what a good answer looks
  like and the recovery call for that route: MediaInfo Debian/Ubuntu (`:47`,
  `set_mediainfo("/path/to/mediainfo")`), MediaInfo Windows (`:63`, re-run
  `set_mediainfo()` with the path found in the Step-2 folder), MediaInfo Mac
  (`:80`, same), FFmpeg Debian/Ubuntu (`:99`, `set_ffmpeg("/path/to/ffmpeg")`),
  FFmpeg Windows (`:115`, read `install_on_win()`'s message or
  `set_ffmpeg("C:/path/to/ffmpeg.exe")`), macOS Homebrew (`:127`,
  `brew --prefix ffmpeg` then `set_ffmpeg()`), macOS Manual (`:154`, re-run the
  Step-4 call with the path actually found). `grep -c "program_status()"
  README.md` = 7. The criterion asks that each of three sections end with such a
  check; every section in the file does.
- **AC2 — met.** The macOS Manual route (`README.md:134-158`) names the step:
  Step 4 says the Applications folder is not on the `PATH` and gives
  `set_ffmpeg("/Applications/ffmpeg")` and `set_ffprobe("/Applications/ffprobe")`,
  with the reason both are needed. Route followed end to end this session with
  `PATH` emptied and the config seam redirected at a scratch dir: before Step 4,
  `find_ffmpeg()` warned "Failed to find ffmpeg." with the `set_ffmpeg()`
  recovery; the two `set_*()` calls each returned `TRUE`; after them
  `find_ffmpeg()` and `find_ffprobe()` returned the staged paths and
  `program_status()` showed both at version 9.0.1, `ffplay` and `mediainfo` `NA`.
  Step 1's two addresses re-checked 2026-09-06 by `content-disposition`:
  `https://evermeet.cx/ffmpeg/get` -> `ffmpeg-126386-gc27482a18d7.7z`,
  `https://evermeet.cx/ffmpeg/getrelease/ffprobe/7z` -> `ffprobe-9.0.1.7z`, which
  is what makes the "separate download" sentence true.
- **AC3 — met.** `Rscript tools/vignette_chunk_guards.R` lists all eight
  `README.Rmd` chunks: five evaluated and non-spawning (the `include = FALSE`
  setup, `library()`, the `system.file()` path, the `ffm_compile()` chain, and
  `extract_audio(run = FALSE)`); three evaluated and spawning, each guarded --
  `has_ffprobe` (`probe_all()`), `has_mediainfo` (`get_duration()`/`get_width()`),
  `has_ffmpeg` (`ffmpeg_codecs()`). Sweep verdict over all six files:
  `unguarded spawning chunks: none`. The companion identity sweep
  (`tools/vignette_chunk_program_identity.R`) reports `chunks starting a program
  their guard does not name: none`, with the three README rows naming
  ffprobe / mediainfo / ffmpeg against their matching guards.
- **AC4 — met.** `Rscript tools/build_vignettes_without_binaries.R readme`
  exited 0. Its log records the reduced `PATH`
  (a pandoc-only shim dir plus `R.home("bin")`, `/usr/bin`, `/bin`, `/usr/sbin`,
  `/sbin`) and `Sys.which()` empty for all three programs; the config seam was
  redirected at scratch dirs and `find_ffmpeg()`, `find_ffprobe()` and
  `find_mediainfo()` all returned `NULL` there, so no remembered location
  answered either. From inside the knit, the setup chunk's stderr line reads
  `tidymedia README build, Sys.which(): ffmpeg=[] ffprobe=[] mediainfo=[]`. The
  artifact scan reports `error or warning lines in the knitted README.md: none`.
- **AC5 — met.** Measured over the four programs crossed with `tm_os()` mocked
  to windows / darwin / linux, on an emptied `PATH` with the config seam
  redirected. Everywhere: "Failed to find {program}." plus
  "Check that it is installed, then use `set_<program>()` to point tidymedia at
  it." On windows and only for the programs `tm_install_registers` names
  (`ffmpeg`, `ffprobe`, `ffplay`) a third bullet is added: "Or run
  `install_on_win()` to download FFmpeg and remember where it landed." Windows +
  `mediainfo` does NOT get that bullet, nor does any darwin or linux branch.
  Condition contract unchanged: class vector `rlang_warning` / `warning` /
  `condition`, fields `call`, `footer`, `message`, `parent` on every branch --
  and `git diff master...HEAD -- R/program_management.R` shows the call was and
  remains a bare `cli::cli_warn()` with no `class =` argument, so nothing in the
  M110 contract could have moved.
- **AC6 — met.** `tools::Rd2ex("man/find_program.Rd")` extracts the example
  wrapped in `if (nzchar(Sys.which("ffmpeg")) && nzchar(Sys.which("mediainfo")))
  withAutoprint({...})`. Sourced under an emptied `PATH` and a redirected config
  dir, with `find_ffmpeg()` confirmed `NULL` there: 0 conditions signalled.
- **AC7 — met.** `devtools::document()` run on a clean `git archive HEAD` copy
  left `man/` and `NAMESPACE` byte-identical to the committed tree (`diff -r`
  silent on both). `devtools::test()`: `FAIL 0 | WARN 10 | SKIP 18 | PASS
  12940`. `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes,
  7m 44.6s.

### Consistency gate

- `cairn_validate.py` — exit 0, all 16 checks PASS, 7 advisories OK (`release
  window` did not fire).
- `cairn_impact.py` — skipped; `Principles touched:` is `—` and the diff changes
  no `DESIGN.md` principle.
- `devtools::document()` no diff — see AC7.
- Generated files not hand-edited — `man/find_program.Rd`'s only change is the
  `\examplesIf` wrapper roxygen produces from the `@examplesIf` tag; the
  no-diff `document()` run confirms it.
- `README.md` in sync with `README.Rmd` — re-knitted with
  `devtools::build_readme()` on a `git archive HEAD` copy and diffed against the
  committed file: the only difference is the M089 `temp_libpath` sandbox path on
  the two `ffm_compile()` output lines, which changes on every knit. No other
  line differs.
- `pkgdown::check_pkgdown()` — `No problems found.`
- `NEWS.md` (the profile's declared changelog) carries this milestone's
  user-visible changes: two Documentation bullets and one Configuration bullet,
  no milestone numbers.
- New top-level files — none in the diff; nothing owed an `.Rbuildignore` entry.
- `devtools::check()` clean — see AC7.

Gate result: pass.

### Independent review

Three fresh-context reviewers, distinct evidence bases (user-facing tier, code
in the diff, so the full fan-out).

**[S] prior-review record — no findings.** Judged the diff against the archived
`## Review` sections of M097, M113 and M114 (the only ones whose findings touch
these files) and `LESSONS.md`. The M114 lesson it could have regressed — a chunk
guarded on its headline program can still spawn a second, unguarded one — is the
apparatus this diff extends rather than weakens. Secondary surface probed:
`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so no
PR-thread walk.

**[S] blame-history — one finding.**

- B1. AC7 unticked and no AC7 write-up in the milestone file. **Rejected**: the
  reviewer read the file while `devtools::check()` was still running; AC7 is
  recorded and ticked above.

**[O] diff-bug — ten findings, ranked by the reviewer.**

- O1. The Debian/Ubuntu, Windows and macOS Homebrew FFmpeg checks say the
  `ffmpeg` and `ffprobe` rows may both be `NA` but offer only `set_ffmpeg()`,
  while the macOS manual route added in the same commit states the governing
  fact that the two are looked up separately. A reader with an ffmpeg-only build
  runs `set_ffmpeg()`, re-runs `program_status()`, and the `ffprobe` row is still
  `NA` with no next step.
- O2. `tools/vignette_chunk_program_identity.R` has no precondition that the
  three binaries are present, where its sibling stops (`vignette_chunk_guards.R`)
  when any is off `PATH`. On a machine without MediaInfo every `has_mediainfo`
  chunk evaluates `FALSE`, spawns nothing, and a newly added unguarded
  `get_duration()` chunk would still be reported clean. With zero spawns
  anywhere, `do.call(rbind, list())` is `NULL` and the script dies with
  "incorrect number of dimensions".
- O3. The README guards read `Sys.which()` where the package resolves through
  `find_program()`, which also reads the remembered location. A contributor with
  FFmpeg off `PATH` who has run `set_ffmpeg()` re-knits and silently ships a
  `README.md` with three example outputs blank.
- O4. The Windows `install_on_win()` offer fires for `ffplay`, which
  `tm_install_registers` lists as a candidate rather than a guarantee — the
  installer's own `absent_optional` path exists because an archive can complete
  without producing it. `find_ffplay()` then advises a call that may not fix it.
- O5. The sibling not-found warning (a remembered location whose file is gone,
  `R/program_management.R:77-84`) was not widened, so a Windows user who deleted
  an `install_on_win()` install gets narrower advice than one who never
  configured anything. Outside the scope line, which named `:95-99` only.
- O6. `covered_programs()` expands `has_ffmpeg` to `ffmpeg` and greps, so a
  chunk written `eval = !has_ffmpeg` would read as covered. No current chunk is
  written that way.
- O7. The knitted-artifact scan greps `^#> (Error|Warning)`, and the `#>` prefix
  comes from `README.Rmd`'s own `comment` option — the scan is not independent of
  the artifact it checks in the way its header claims.
- O8. `build_vignettes_without_binaries.R` copies the whole tree, `.git`
  included, into `tempdir()` on every run.
- O9. `README.md` still renders a `temp_libpath` sandbox path as the reader's
  example answer on two lines.
- O10. Two added `NEWS.md` lines run past the ~78-column wrap the file keeps.

**Return floor:** none of the eleven findings demonstrates an acceptance
criterion failing. AC3's evidence was gathered on this machine with all three
binaries present, so O2's empty-domain path did not affect it (the sweep
reported 13 spawning rows). Status stays `review`; the ranked list goes to the
maintainer at the merge gate.
