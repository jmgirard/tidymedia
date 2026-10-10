# M138: The shipped docs and the remembered-location checks stop assuming what the host's FFmpeg build and `which` provide

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing. It adds an export and a condition class, and it changes the examples and vignettes CRAN runs.
- **Branch/PR:** —

## Goal

Make `R CMD check` pass on a machine whose FFmpeg build has no `libx264`, and
whose `which` does not answer for an absolute path. That is what the two CRAN
Fedora flavors reported against the current release. The deadline is
2026-10-31.

## Scope

**In:** the two causes behind the CRAN ERRORs. First, every shipped-doc site that
runs an encode names its encoder and is guarded on this build having it, behind a new
exported `has_encoder()`. Second, one internal predicate answers whether a location
names a runnable program, reading the filesystem rather than `Sys.which()`, for
`find_program()`, `tm_set_program()` and `tm_usable_binary()`. With it comes the
diagnosis that tells a file that cannot be run from one that is not there. A Fedora CI
job in CRAN's own containers is also in.

**Out:** refusing a caller's own missing encoder, which is M139.
`find_program()`'s directory-shaped config path, which `readLines()` still dies on,
stays a candidate row. So does `ffmpeg_encoders()`'s unclassed `argument of length 0`
where no binary resolves.

## Acceptance criteria

- [ ] AC1: Each shipped-doc site that runs an FFmpeg encode names the encoder it uses,
      rather than inheriting the output container's default, and is guarded on
      `has_encoder()` for that encoder. The domain is enumerated by procedure, never a
      copied list: `grep -n "examplesIf\|nzchar(Sys.which" R/*.R` for the example
      blocks, `tools/vignette_chunk_guards.R` for the chunks. Review re-runs it and
      quotes it. It returns six sites today, at `R/ffm.R:1607`, `R/ffm_manifest.R:22`,
      `R/ffmpeg.R:5266`, and `vignettes/verification.Rmd:108`, `:138`, `:169`.
- [ ] AC2: The package passes its checks on a build without `libx264`. (a) With the
      committed wrapper `tools/ffmpeg_build_model/ffmpeg` first on `PATH`, which hides
      the encoders named in `TIDYMEDIA_HIDE_ENCODERS` from its `-encoders` output and
      answers a call naming one of them as the real binary answers an unknown encoder,
      `R CMD check --as-cran` reports 0 errors and 0 warnings, and `devtools::test()`
      with `NOT_CRAN=true` reports 0 failures, 0 errors and at least one skip naming a
      hidden encoder. Both runs are made twice: once modelling the CRAN machines, with
      `libx264` hidden and `libmp3lame` and `aac` present, and once hiding all three.
      (b) On the merge-base commit the first configuration reports the `tidymedia-Ex.R`
      examples ERROR quoting `Unknown encoder 'libx264'`, and at least one fixture
      error. That is the defect this green is read against. (c) An r-hub run on
      `fedora-clang-devel` and `fedora-gcc-devel` reports 0 errors and 0 warnings on
      both. Every run's relevant lines are quoted in the Review section.
- [ ] AC3: `has_encoder()` answers one logical for one encoder name, and each clause here has
      its own test. It reads `cached_encoder_names()`, and not the `tidymedia.hardware_encoders`
      option, which names the hardware pool only, so a session that set that option still gets a
      true answer for `libx264`. It calls `resolve_timeout()` before it consults the memo, so an
      unusable `tidymedia.timeout` is refused as the hardware sibling refuses it (D074, D075).
      Under a mocked `cached_encoder_names()` that omits `libx264` it answers FALSE for that name,
      and TRUE for a name the mock lists. Where `find_ffmpeg()` finds no location at all it
      answers FALSE, with no error and no warning, while a warning naming a repairable remembered
      location still reaches the caller (D088). Where a location resolves, `ffmpeg -encoders` is
      spawned at most once per session, and `refresh_ffmpeg_capabilities()` makes the next call
      ask again.
- [ ] AC4: One vectorized internal predicate decides whether a location names a runnable program,
      and `find_program()`, `tm_set_program()` and `tm_usable_binary()` all ask it. A location
      holding a directory separator is decided by `file.exists()`, a not-a-directory test and
      `file.access(path, 1L)`, never by `Sys.which()`. One holding none is still looked up on
      `PATH` by `Sys.which()`, and a test asserts that `set_program()` accepts such a name and
      writes it as typed. `tm_usable_binary()` keeps the non-empty test on top of the predicate,
      so D083's truncated `ffmpeg.exe` is still refused on Windows and all three forms D084
      partitions still reach `tidymedia_program_unusable`. After this milestone `grep -n
      "Sys.which" R/*.R | grep -v ":[0-9]*: *#"` reports calls at two sites only:
      `find_program()`'s bare-name lookup, and the predicate's bare-name branch.
- [ ] AC5: A committed test holds a mocked `base::Sys.which()` answering `""` for every
      argument that holds a directory separator. The mock is a stipulated model of the
      failure, warranted by the quoted CRAN log rather than measured here. Under it
      `find_program()` returns the remembered location with no warning, `set_program()`
      accepts a path that exists, and `tm_usable_binary()` answers TRUE for a non-empty
      executable file. On the merge-base commit that test fails with
      `tidymedia_location_gone` for a stub that exists, and with FALSE from
      `tm_usable_binary()`. The install-side arm is asserted at `tm_usable_binary()`,
      under the install mock that holds `tm_os()` at `"windows"`, because D086's
      platform gate refuses `install_on_win()` first here. Red and green are both quoted
      in the Review section.
- [ ] AC6: A remembered location naming a file that exists and cannot be executed is
      diagnosed as that, and not as absent. `find_program()` warns that the file cannot
      be run and returns `NULL`, under the class `tidymedia_location_not_executable`
      with `tm_program` and `tm_location`, threading no `call`, as its two siblings do.
      An absent location keeps `tidymedia_location_gone` and its present wording. A test
      fires each of the two and asserts the class. It skips on Windows, where the bit
      does not exist and the absent diagnosis stands, which is a gap
      `cairn/DESIGN.md`'s Known issues records.
- [ ] AC7: `cairn/PROFILE.md`'s verify and consistency-gate slots are clean with the
      real binaries present, the universal cairn-file checks included, and `NEWS.md`
      carries an entry for each user-visible change. `has_encoder()` has its
      `_pkgdown.yml` row in the same commit, and `.Rbuildignore`'s `^tools$` already
      covers the new wrapper.

## Coverage

- AC1 → T5
- AC2 → T1, T2, T3, T4, T5, T9
- AC3 → T2
- AC4 → T6
- AC5 → T8
- AC6 → T7, T8
- AC7 → T2, T5, T10

## Tasks

- [ ] T1: Commit `tools/ffmpeg_build_model/ffmpeg` with its header. Then run
      `R CMD check --as-cran` and the `NOT_CRAN=true` suite under it at the
      merge-base, and record what reddens (AC2b).
- [ ] T2: Add exported `has_encoder()` on `cached_encoder_names()`, with no
      option override, `resolve_timeout()` first, and FALSE where no location is
      found. Add its tests, its `_pkgdown.yml` row and `devtools::document()`.
- [ ] T3: Add `skip_if_no_encoder()` to `tests/testthat/helper-skip.R`, calling
      `testthat::skip_on_cran()` before it asks the pool (D090). Guard the
      `tests/testthat/helper-media.R` fixture builders that name an encoder.
- [ ] T4: Guard the execution tests that name an encoder in the executed call.
      The set is what the T1 run reddens. Re-run it until clean.
- [ ] T5: Make each AC1 site name its encoder and guard on it. Regenerate
      `man/`, `README.md` and the vignettes.
- [ ] T6: Write the shared path predicate. Route `find_program()`
      (`R/program_management.R:85`), `tm_set_program()` (`:934`) and
      `tm_usable_binary()` (`:1712`) through it, keeping the non-empty test at
      the install check. Measure whether a tilde location still spawns.
      `run_program()` hands `system2()` the location unquoted
      (`R/program_management.R:1157`), so `sh` expands it. On that measurement
      the location stays written as typed.
- [ ] T7: Add the `tidymedia_location_not_executable` diagnosis. Document it in
      the find-a-program page's Problems section.
- [ ] T8: Rewrite the tests pinning the old rule: the tilde pair at
      `tests/testthat/test-program-management.R:2636` and `:2677`, the
      `Sys.which` premises at `:2619-2622`, and the directory mock at `:2667`.
      Add the AC5 model-mock test, with its merge-base red recorded.
- [ ] T9: Add the r-hub workflow with `rhub::rhub_setup()`. Run it on
      `fedora-clang-devel` and `fedora-gcc-devel`, and record both results.
- [ ] T10: Write the records. `NEWS.md` entries. Correct `cairn/DESIGN.md`'s
      Conventions sentence about examples and vignettes, and add AC6's Windows
      gap to Known issues. One D-entry superseding D090's "What this does not
      cover" paragraph. One D-entry annotating D083 and D084's "What the check
      asks". Extend the `Sys.which` lesson in `cairn/LESSONS.md`, paying for the
      bytes by compressing the line it extends. Annotate the two candidate rows
      this milestone closes.

## Work log

- 2026-10-10: created by /milestone-plan, from the CRAN mail of 2026-10-10 and the check log for `r-devel-linux-x86_64-fedora-clang` at 0.2.1. Merge-base `e83440ac`.
- 2026-10-10: question set. A public `has_encoder()` over an internal predicate: granted. A Fedora CI job in CRAN's own containers: granted, so `rhub::rhub_setup()` adds a workflow and T9 runs it. A 0.2.2 release window: declared, attaching to this milestone alone.
- 2026-10-10: the user asked for the front-door refusal everywhere a caller names an encoder. It is M139, planned in this run and depending on this one. It touches about 20 front doors, the jobs-column forms, the D036/D075/D085 ordering and the spawn-trace baselines. None of that can be allowed to delay the CRAN deadline, and nothing of it is dropped.
- 2026-10-10: plan gate chose a wrapper that models one build's encoder set. The alternative was one that fails any call naming a hidden encoder, and it cannot see a site inheriting the container's default. AC1's rule that every running site names its encoder is what empties that half. Falsified by a shipped-doc site that must inherit a default.
- 2026-10-10: plan gate chose one shared predicate with a bare-name branch, over replacing `Sys.which()` outright. `set_program()` accepts a bare name found on `PATH` (`tests/testthat/test-program-management.R:3149-3155`). Falsified by a caller whose bare name must not be searched for.
- 2026-10-10: criteria audit ran in full mode, in a fresh Opus reader. It returned findings on six of the seven criteria, and all were taken. Narrowed the wrapper's promise to one build's encoder set and added AC1's names-its-encoder rule (Q3, Q4). Kept D083's non-empty test at `tm_usable_binary()` (Q2, D083, D084). Narrowed `has_encoder()`'s silence to the no-location-found case, and said a repairable-location warning still reaches the caller (Q2, D088). Added the option-seam and `resolve_timeout()` clauses (D044, D074, D075). Dropped an unreachable whole-suite `Sys.which` mock. Re-sited the install-side red under the install mock that holds `tm_os()` (Q1, D086). Named the new class (D062). Replaced a miscited line pin with a named assertion. Added the `.Rbuildignore`, `_pkgdown.yml` and universal-gate clauses to AC7. Added the r-hub Fedora run as the only instrument that measures the machines that failed.
- 2026-10-10: the audited criteria were re-punctuated and split into shorter sentences for the repo's writing hook. No clause changed what it promises.
- 2026-10-10: `CRAN-SUBMISSION` is modified in the working tree from the 0.2.1 submission, and this plan leaves it unstaged.
- 2026-10-10: `cairn/ROADMAP.md` was 49,782 bytes against its 24,000-byte budget, and `cairn/LESSONS.md` is 17 bytes under its 20,000. So this plan adds no new candidate row. Each remainder annotates an existing row, and T10 pays for its lesson bytes by compressing the line it extends. The file is 51,113 bytes and 59 lines after this plan, one line under its 60-line cap, so the next milestone row breaches it. `/cairn-triage` is the remedy for both figures.

## Decisions

## Review
