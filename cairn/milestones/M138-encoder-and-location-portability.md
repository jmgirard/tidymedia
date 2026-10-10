# M138: The shipped docs and the remembered-location checks stop assuming what the host's FFmpeg build and `which` provide

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing. It adds an export and a condition class, and it changes the examples and vignettes CRAN runs.
- **Branch/PR:** m138-encoder-and-location-portability

## Goal

Make `R CMD check` pass on a machine whose FFmpeg build has no `libx264`, and
whose `which` does not answer for an absolute path. That is what the two CRAN
Fedora flavors reported against the current release. The deadline is
2026-10-31.

## Scope

**In:** the two causes behind the CRAN ERRORs. First, every shipped-doc site that runs an encode names
its encoder and is guarded on this build having it, behind a new exported `has_encoder()`. Second, one
internal predicate answers whether a location names a runnable program, reading the filesystem rather
than `Sys.which()`, for `find_program()`, `tm_set_program()` and `tm_usable_binary()`. With it comes the
diagnosis that tells a file that cannot be run from one that is not there. A Fedora CI job is also in.

**Out:** refusing a caller's own missing encoder, which is M139. `find_program()`'s directory-shaped
config path, which `readLines()` still dies on, stays a candidate row. So does `ffmpeg_encoders()`'s
unclassed `argument of length 0` where no binary resolves.

## Acceptance criteria

- [ ] AC1: Each shipped-doc site that runs an FFmpeg encode names the encoder it uses, rather than inheriting
      the output container's default, and is guarded on `has_encoder()` for that encoder. The domain is
      enumerated by procedure, never a copied list: `grep -n "examplesIf\|nzchar(Sys.which" R/*.R` for the
      example blocks, `tools/vignette_chunk_guards.R` for the chunks. Review re-runs it and quotes it. It
      returns six sites today, at `R/ffm.R:1607`, `R/ffm_manifest.R:22`, `R/ffmpeg.R:5266`, and
      `vignettes/verification.Rmd:108`, `:138`, `:169`.
- [ ] AC2: The package passes its checks on a build without `libx264`. (a) With the committed wrapper
      `tools/ffmpeg_build_model/ffmpeg` first on `PATH`, which hides the encoders named in
      `TIDYMEDIA_HIDE_ENCODERS` from its `-encoders` output and answers a call naming one of them as the real
      binary answers an unknown encoder, `R CMD check --as-cran` reports 0 errors and 0 warnings beside the
      incoming-feasibility one of a re-checked release, and `devtools::test()` with `NOT_CRAN=true` reports 0
      failures, 0 errors and at least one skip naming a hidden encoder. Both runs are made twice: once modelling
      the CRAN machines, with `libx264` hidden and `libmp3lame` and `aac` present, and once hiding all three.
      (b) On the merge-base commit the first configuration reports the `tidymedia-Ex.R` examples ERROR quoting
      `Unknown encoder 'libx264'`, and at least one fixture error. That is the defect this green is read
      against. (c) A committed GitHub Actions job runs `R CMD check --as-cran` inside a Fedora container with
      Fedora's own `ffmpeg-free` installed, quoted from a run made on this branch, with `NOT_CRAN` left unset so
      the run is the reading CRAN itself made. Two of the four lines the job records before the check are gates.
      The criterion is met only where the recorded `ffmpeg -encoders` shows `libx264` absent. The location half
      counts as measured only where `Sys.which()` answers `""` for an absolute path to a stub the job just
      created while answering for the bare name `ffmpeg`, and where it does not, that half rests on AC5's
      stipulated mock, which is what AC5 is for. The check reports 0 errors, and a `grep` over the saved log
      finds no WARNING or NOTE naming `libx264`, any encoder the run's own `-encoders` listed, one of the four
      remembered-location condition classes, or the configuration directory path. A WARNING or NOTE about the
      container's own environment is listed in the Review section rather than fixed, and so is the
      incoming-feasibility WARNING. The red control is CRAN's own check log for 0.2.1 on
      `r-devel-linux-x86_64-fedora-clang`, a real machine's reading of both defects. Every run's relevant lines
      are quoted in the Review section, with this run's URL and its error, warning and note counts.
- [ ] AC3: `has_encoder()` answers one logical for one encoder name, and each clause here has its own test. It
      reads `cached_encoder_names()`, and not the `tidymedia.hardware_encoders` option, which names the hardware
      pool only, so a session that set that option still gets a true answer for `libx264`. It calls
      `resolve_timeout()` before it consults the memo, so an unusable `tidymedia.timeout` is refused as the
      hardware sibling refuses it (D074, D075). Under a mocked `cached_encoder_names()` that omits `libx264` it
      answers FALSE for that name, and TRUE for a name the mock lists. Where `find_ffmpeg()` finds no location
      at all it answers FALSE, with no error and no warning, while a warning naming a repairable remembered
      location still reaches the caller (D088). Where a location resolves, `ffmpeg -encoders` is spawned at most
      once per session, and `refresh_ffmpeg_capabilities()` makes the next call ask again.
- [ ] AC4: One vectorized internal predicate decides whether a location names a runnable program, and
      `find_program()`, `tm_set_program()` and `tm_usable_binary()` all ask it. A location holding a directory
      separator is decided by `file.exists()`, a not-a-directory test and `file.access(path, 1L)`, never by
      `Sys.which()`. One holding none is still looked up on `PATH` by `Sys.which()`, and a test asserts that
      `set_program()` accepts such a name and writes it as typed. `tm_usable_binary()` keeps the non-empty test
      on top of the predicate, so D083's truncated `ffmpeg.exe` is still refused on Windows and all three forms
      D084 partitions still reach `tidymedia_program_unusable`. After this milestone `grep -n "Sys.which" R/*.R
      | grep -v ":[0-9]*: *#"` reports calls at two sites only: `find_program()`'s bare-name lookup, and the
      predicate's bare-name branch.
- [ ] AC5: A committed test holds a mocked `base::Sys.which()` answering `""` for every argument that holds a
      directory separator. The mock is a stipulated model of the failure, warranted by the quoted CRAN log
      rather than measured here. Under it `find_program()` returns the remembered location with no warning,
      `set_program()` accepts a path that exists, and `tm_usable_binary()` answers TRUE for a non-empty
      executable file. On the merge-base commit that test fails with `tidymedia_location_gone` for a stub that
      exists, and with FALSE from `tm_usable_binary()`. The install-side arm is asserted at
      `tm_usable_binary()`, under the install mock that holds `tm_os()` at `"windows"`, because D086's platform
      gate refuses `install_on_win()` first here. Red and green are both quoted in the Review section.
- [ ] AC6: A remembered location naming a file that exists and cannot be executed is diagnosed as that, and not
      as absent. `find_program()` warns that the file cannot be run and returns `NULL`, under the class
      `tidymedia_location_not_executable` with `tm_program` and `tm_location`, threading no `call`, as its two
      siblings do. An absent location keeps `tidymedia_location_gone` and its present wording. A test fires each
      of the two and asserts the class. It skips on Windows, where the bit does not exist and the absent
      diagnosis stands, which is a gap `cairn/DESIGN.md`'s Known issues records.
- [ ] AC7: `cairn/PROFILE.md`'s verify and consistency-gate slots are clean with the real binaries present, the
      universal cairn-file checks included, and `NEWS.md` carries an entry for each user-visible change.
      `has_encoder()` has its `_pkgdown.yml` row in the same commit, and `.Rbuildignore`'s `^tools$` already
      covers the new wrapper.

## Coverage

- AC1 → T5, T11
- AC2 → T1, T2, T3, T4, T5, T9, T12
- AC3 → T2
- AC4 → T6
- AC5 → T8, T13
- AC6 → T7, T8, T14
- AC7 → T2, T5, T10

## Tasks

- [x] T1: Commit `tools/ffmpeg_build_model/ffmpeg` with its header, then run the check and the `NOT_CRAN=true`
      suite under it at the merge base and record what reddens (AC2b).
- [x] T2: Add exported `has_encoder()` on `cached_encoder_names()`, with no option override, `resolve_timeout()`
      first, and FALSE where no location is found. Add its tests, its `_pkgdown.yml` row and `document()`.
- [x] T3: Add `skip_if_no_encoder()` to `tests/testthat/helper-skip.R`, calling `testthat::skip_on_cran()` before
      it asks the pool (D090), and guard the `helper-media.R` fixture builders that name an encoder.
- [x] T4: Guard the execution tests that name an encoder in the executed call. The set is what the wrapper runs
      redden. Re-run until clean, in both configurations.
- [x] T5: Make each AC1 site name its encoder and guard on it. Regenerate `man/`, `README.md` and the vignettes.
- [x] T6: Write the shared path predicate and route `find_program()`, `tm_set_program()` and
      `tm_usable_binary()` through it, keeping the non-empty test at the install check. Measure whether a tilde
      location still spawns, and keep writing the location as typed if it does.
- [ ] T7: Add the `tidymedia_location_not_executable` diagnosis, and document it in the find-a-program page's
      Problems section. Correct the two sentences that still promise a two-case partition: `?find_ffmpeg`'s
      "one of two classes" and `?program_status`'s "There are two cases".
- [x] T8: Rewrite the tests pinning the old rule: the tilde pair, the `Sys.which` premises, and the directory
      mock in `test-program-management.R`. Add the AC5 model-mock test, with its merge-base red recorded.
- [x] T9: Add `.github/workflows/fedora-cran-model.yaml`, on `workflow_dispatch` and `pull_request`, in a Fedora
      container. It installs `ffmpeg-free` BY NAME, because that package does not `Provide: ffmpeg` and RPM
      Fusion's `ffmpeg` carries `libx264`, which would make the run vacuous. It installs pandoc, so AC1's four
      vignette sites are inside the run. Before the check it records the four lines AC2(c) names. Remove the
      r-hub workflow this task first added, and record the run's figures.
- [x] T10: Write the records: `NEWS.md`; `cairn/DESIGN.md`'s Conventions sentence and AC6's Windows gap in Known
      issues; one D-entry superseding D090's "What this does not cover" paragraph; one annotating D083 and D084's
      "What the check asks"; the `Sys.which` lesson extended within its byte budget; and the two candidate rows
      this milestone closes.
- [ ] T11: AC1's own rule at `?ffm_run`, whose example re-encodes audio to the MP4 default with no `-c:a`: name
      that encoder and extend the guard, then re-read every running site for a second encoder the container
      decides rather than the code.
- [ ] T12: AC2(c)'s gates. Make the two recorded lines gate rather than echo, stop discarding the check's exit
      status, widen the log grep to what the criterion states, and install the three Suggests the list omits.
- [ ] T13: AC5's stated instrument. Assert the install-side arm under the install mock that holds `tm_os()` at
      `"windows"`, as the criterion names.
- [ ] T14: the ten fix-now findings the review pass settled, listed in the Review section: the vignette's `jobs`
      chunk, the reached-limit table's provenance, the wrapper's path substitution and its prefix claim,
      `tm_usable_binary()`'s NA parity, the NEWS sentence for the Windows location shape, the `path.expand()`
      comment, and the unmeasured Windows `file.access()` claim.

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
- 2026-10-10: T1 instrument committed. `tools/ffmpeg_build_model/ffmpeg` hides any encoder named in `TIDYMEDIA_HIDE_ENCODERS`, drops its rows from `-encoders`, and answers a call naming it with the real binary's own text and status, by asking the real binary about a name no build has and putting the hidden name back. Measured on this machine: the wrapper gives exit 8 and `Unknown encoder 'libx264'`, and a call that names no encoder still writes its file.
- 2026-10-10: AC2(b) red control recorded at the merge base `18996d41`. `R CMD check --as-cran` under the wrapper with `TIDYMEDIA_HIDE_ENCODERS=libx264` gives `Status: 2 ERRORs, 1 WARNING`. Both ERRORs are the examples step, plain and `--run-donttest`, failing in `?ffm_run` with `[vost#0:0] Unknown encoder 'libx264'`. The WARNING is CRAN incoming feasibility reporting 0.2.1 against the released 0.2.1, which the version bump at release clears. Log kept at `scratchpad/mbcheck/check.log`.
- 2026-10-10: T2 done. `has_encoder()` is exported, with `tm_encoder_available(name, call)` carrying the body so the refusal names the function the caller typed (D087). The limit is read FIRST, above the location question rather than inside it, so an unusable `tidymedia.timeout` is refused on a machine with no FFmpeg as well as on one with it. The hardware sibling reads it inside its fall-through because its option seam asks FFmpeg nothing, and `has_encoder()` has no such seam.
- 2026-10-10: T2 decided, not asked: `has_encoder()` does not read `tidymedia.hardware_encoders`. That option names the machine's hardware pool, so reading it would answer FALSE for every software encoder in any session that set it, and about 80 test sites set it. A test pins the two questions apart.
- 2026-10-10: T2 refactor: `program_status()`'s inline `audible` vector became `tm_audible_location_classes()`, because `has_encoder()` muffles on the same partition for the same reason (D088) and two copies drift. T7 extends the one vector.
- 2026-10-10: T3 to T8 and T10 written. `skip_if_no_encoder()` added, calling `skip_if_no_ffmpeg()` first so `skip_on_cran()` still leads (D090). Nine `tests/testthat/helper-media.R` fixtures that name an encoder now guard on it. Ten execution tests that name one in the executed call now guard on it, the set the merge-base wrapper run reddened. The three example sites and four `vignettes/verification.Rmd` chunks name their encoder and guard on it. `tm_program_state()` and `tm_runnable_program()` replace the `Sys.which()` question at `find_program()`, `tm_set_program()` and `tm_usable_binary()`, which keeps its own non-empty test. `tidymedia_location_not_executable` added, and the audible partition with it. `test-location-portability.R` holds AC5's and AC6's tests. Records: NEWS, the two DESIGN corrections, D095, D096, and the lesson.
- 2026-10-10: the suite found two guards of this repo's own catching the change, both repaired rather than suppressed. `test-input-path-front-door.R`'s one-`file.access()`-site sweep now names `tm_program_state()` beside `unreadable_paths()`, with the comment saying the two ask different questions (readability against the execute bit). And `has_encoder()` joined the computed `tm_timeout_domain()`, because it reads the limit, so `helper-timeout-sweep.R` gained its call spec and `tm_timeout_post_baseline()` gained its name. The reached-limit comparison now runs over the baseline domain only, for the reason that helper's own comment gives: the recorded table says what each member did at `ae5ff1c`, and measuring a newer export today and filing it under that ref would compose a history rather than record one. The never-silent rule (D049) still binds the new member.
- 2026-10-10: CHECKPOINT, not a completion. Every task's work is written and `test-has-encoder.R`, `test-location-portability.R`, `test-timeout-refusal-blame.R` and `test-input-path-front-door.R` pass on their own. No task box is ticked yet, because the profile's verify slot has not been seen clean since the two guard repairs landed. Still owed: the full suite green, AC2's four wrapper runs on the new code, `devtools::check()`, the claim audit, and T9's dispatch, which needs the maintainer's hands for the r-hub token.
- 2026-10-10: T9 part-done. `.github/workflows/rhub.yaml` is the canonical r-hub workflow, copied verbatim from `https://raw.githubusercontent.com/r-hub/actions/main/workflows/rhub.yaml` on 2026-10-10 (sha256 `a7dc1fed557dcddea3c95c4dc4f904f8098434a5fd9f95c0f7636163c0c4a230`), which that file's own header names as its canonical location. The run cannot be dispatched from here: every job reads `secrets.RHUB_TOKEN`, which `rhub::rhub_setup()` provisions through an email-validated token, so AC2(c) needs two commands from the maintainer. `rhub` is not installed on this machine and this milestone does not install it.
- 2026-10-10: four more of this repo's computed-domain guards caught the change, each updated with the reason rather than re-recorded silently. `tm_timeout_recorded_domain()` gains `has_encoder`, which is the drift the guard exists to report. `tm_timeout_absorbers()` gains `tm_encoder_available`, with the justification the partition's comment asks for: its handler wraps `find_ffmpeg()` alone, which spawns nothing, so no timeout condition can arise inside it, and the spawn that can time out sits outside the handler. The unguarded-argument grid goes to 1600 rows, 1168 kept and 432 dropped, and the one new dropped cell is `has_encoder/name -> <none>`, which belongs there: the legal-string form of a question about an encoder name is answered FALSE rather than refused, the same reading the sweep has always carried for `segment_video/outfiles`.
- 2026-10-10: claim audit: 44 claims read, 9 corrected — `R/ffmpeg.R`, `R/program_management.R`, `NEWS.md`, `tools/ffmpeg_build_model/ffmpeg`, `tests/testthat/test-has-encoder.R`, `tests/testthat/test-location-portability.R`. The nine: a comment still said two location warnings are audible where there are now three; two comments above the code this branch rewrote still described the `Sys.which()` mechanism it replaced, one of them the partition count at `program_status()`; a test comment claimed every assertion in it failed at the merge base, where two of the four are premises that passed there; the hardware-option site count was carried over as "about 80" and is 144 `test_that()` blocks; the wrapper header asserted Fedora's encoder list without saying where it was read; the same header attributed its prefix-hiding to Fedora without support; and two NEWS sentences overstated, one saying examples "now name their encoder" where three of the four sites already did, the other saying the Windows reading is unchanged where only the new warning is what does not arise there.
- 2026-10-10: the Fedora encoder list is read, not assumed. The `--enable-encoder=` line in the `r-devel-linux-x86_64-fedora-clang` check log for 0.2.1 holds 215 encoders. `libopenh264`, `libmp3lame`, `aac` and `srt` are present, and neither `libx264` nor `libx264rgb` is. That is what the wrapper's two configurations model, and what makes its prefix rule faithful rather than convenient.
- 2026-10-10 substantive amendment: AC2(c) and T9 drop r-hub for a Fedora container job, at the maintainer's selection. The reason: `fedora-clang-devel` and `fedora-gcc-devel` are r-hub version 1 platform names, the current r-hub's Linux containers that could be enumerated from here are Fedora-based but special-purpose (atlas, blis, gcc-asan, gcc15, gcc16, mkl, nosuggests, valgrind) with no Fedora devel flavor among them, and nothing establishes that any of them carries FFmpeg, which would have made the run vacuous. The bar moves with it, from "0 errors and 0 warnings on two r-hub flavors" to "0 errors in one Fedora container" plus a grep over the saved log, because a `--as-cran` run of a released version number always carries the incoming-feasibility WARNING, which the new bar permits by name.
- 2026-10-10: re-audit: AC2 (full) — nine repairs, all taken. The trigger and the quoted ref were unnamed. The three figures were unnamed. `NOT_CRAN` was unpinned, which decides whether the run is CRAN's own reading or a fuller one. The `/usr/bin/which` existence probe could not discriminate, because CRAN's machines ran an example gated on `nzchar(Sys.which("ffmpeg"))`, so their `which` worked for bare names and the defect is behavioral rather than a missing file. The recorded values were notes rather than gates, so the green could have been read off a container reproducing neither condition. The no-WARNING clause had no procedure, so it was held to a weaker standard than AC1 and AC4 in the same milestone. The FFmpeg package had to be named, because `ffmpeg-free` does not `Provide: ffmpeg` and RPM Fusion's `ffmpeg` carries `libx264`. And the pandoc exemption would have removed four of AC1's six sites from the run, so the job installs pandoc.
- 2026-10-10: the re-audit also corrected what this milestone can claim about the cause of the 82 test failures. `Sys.which()` worked for a bare name on the machines that failed, so the cause is a `which` that does not resolve an argument holding a separator, not an absent `which`. D096 states the seam and not a cause, so it stands as written. `tests/testthat/test-location-portability.R` mocks `Sys.which()` and so passes on any host: the tests that would feel a hostile host are `test-program-management.R`'s, which run under `--as-cran` because they spawn nothing.
- 2026-10-10: the Fedora run happens locally once, at the maintainer's selection, because GitHub offers a hand-started workflow only for a file already on the default branch and this repo opens the milestone PR after the merge approval. The committed job then guards every later pull request on its own. The container is `fedora:42` on aarch64 rather than CRAN's x86_64, which is a disclosed limit: the encoder allowlist is a build configuration rather than an architecture fact, and the lookup question is not architecture-dependent either.
- 2026-10-10: GATE 1 reproduced, GATE 2 not. On `fedora:42`, `ffmpeg-free-7.1.4-1.fc42` lists zero `libx264` rows and ships `libopenh264`, which is the same picture as CRAN's configure line. But R 4.5.3 there answers `Sys.which("/tmp/wp/stub")` with the path itself, and `Sys.which("ffmpeg")` with `/usr/sbin/ffmpeg`, so the lookup condition is NOT present on this image and that half rests on AC5's stipulated mock, which is the clause the re-audit added for exactly this outcome. It also narrows what anyone can claim about the cause: it is not "Fedora's `which`", because this Fedora resolves a path. Something about CRAN's R-devel Fedora images differs, and D096 states the seam rather than a cause, so it stands.
- 2026-10-10: AC2(c) is green on Fedora. `R CMD check --as-cran` in `fedora:42` with `ffmpeg-free` reports `Status: 2 WARNINGs` and 0 ERRORs. `checking examples ... OK`, `checking examples with --run-donttest ... OK`, `checking tests ... [80s/84s] OK` with `[ FAIL 0 | WARN 0 | SKIP 302 | PASS 13908 ]`, and `checking re-building of vignette outputs ... OK`. The grep over the log finds nothing naming `libx264`, a remembered-location class, or the configuration directory. Both WARNINGs are the container's own environment, which the criterion exempts by name: CRAN incoming feasibility reporting 0.2.1 against the released 0.2.1 with a URL check that could not reach the network, and `qpdf` absent for the PDF size check. Logs kept at `scratchpad/fedora-check-green.log` and `scratchpad/fedora-gates-green.log`.
- 2026-10-10: eight container runs were needed, and the first seven failed on the image's own toolchain rather than on the package. `fs` does not compile on Fedora aarch64, which took `testthat` and `rmarkdown` with it. Fedora's `R-rlang` is 1.1.6 against a declared floor of 1.2.0. `git clone` refuses a tree owned by another user inside the container. A mixed RPM and CRAN dependency set left `lifecycle` and then `R6` missing. And colima does not share this scratchpad path, so the logs come out by `docker cp` rather than a bind mount. The run that worked takes `R-fs` from `dnf` and everything else from CRAN with `dependencies = TRUE`, plus `glibc-langpack-en` for the locale.
- 2026-10-10: every task is done and the profile's verify slot is clean, so this goes to review. The four suite configurations all report 0 failures: real binaries, `libx264` hidden, and all three hidden, the last with 143 skips naming the encoder they miss. `R CMD check --as-cran` reports 0 errors in all four readings taken, which are the two wrapper configurations, the real-binary one, and Fedora. `devtools::document()` produces no diff, `pkgdown::check_pkgdown()` finds no problems, and `README.Rmd` is untouched so `README.md` needs no rebuild.
- 2026-10-10: return 1 worked. T11: `?ffm_run`'s example names `audio = "aac"` beside `libx264` and guards on both, and the other three running sites were re-read by compiling each command: `-codec:a libmp3lame -vn` at `?ffm_manifest` and the vignette chunks, `-codec:a aac -map 0:a:0` at `?normalize_audio_batch`. No running site now leaves an encoder to the container.
- 2026-10-10: T12: the Fedora job's gate 1 exits non-zero where `libx264` is present, the check's exit status is no longer discarded, a missing `Status:` line fails rather than passes, the log grep is scoped to the WARNING and NOTE blocks and derives its encoder names from the build's own `-encoders` listing, and the dependency install takes every Suggests with `dependencies = TRUE`. Proven able to fail: against the real Fedora log it passes, and with a planted encoder mention and a planted location class it catches both.
- 2026-10-10: T13: AC5's install-side arm now runs under `tm_local_windows()`, which holds `tm_os()` at `"windows"`, as the criterion names.
- 2026-10-10: T7 done properly this time. `tidymedia_location_not_executable` is documented on both pages, and the two sentences that promised a two-case partition now name three cases, including that the new one does not arise on Windows.
- 2026-10-10: T14, the ten fix-now findings. The vignette's `jobs`-reading chunk is guarded on `has_mp3`, and a sweep over the file confirms no chunk reads a `has_mp3` name under a weaker guard. The reached-limit table gained a second, dated vector so every member is compared again, including `program_status`, which the first attempt had stopped comparing. The wrapper matches only an argument that IS an encoder name, so a path component called `libx264` is left alone, and it now fails a `libx264rgb` call to match its own prefix rule. `tm_usable_binary()` answers FALSE rather than NA for `NA` and for a bare name. NEWS tells Windows users to name the executable extension. The `path.expand()` comment says what was measured. And the unmeasured claim that Windows reads `file.access(mode = 1)` as existence is gone: `?file.access` documents nothing there, so the predicate does not ask it on Windows at all, which also removes the risk that it would have refused every binary the installer unpacks.
- 2026-10-10: the criteria were reflowed to fit the 150-line cap and not reworded. Proven: each of the seven is byte-identical to the committed version after the whitespace normalisation the binding-criteria check uses. An earlier attempt did reword AC2 while compressing it, and was reverted — on a return the work moves, not the promise.
- 2026-10-10: review return 1: AC1 fails at `?ffm_run`, whose example re-encodes audio to the MP4 default with no `-c:a`, so one encoder at that site is still decided elsewhere. AC2(c) fails because the committed Fedora workflow echoes its two gates rather than gating on them, discards the check's exit status, and installs a dependency list that omits three Suggests. AC5 fails because the install-side arm is asserted without the `tm_os()` harness the criterion names. And T7 is ticked but undone: the new condition class is documented nowhere, while `?find_ffmpeg` and `?program_status` still promise a two-case partition that this milestone made false. Ten further findings are fix-now and five are candidate rows, all settled and recorded in the Review section.
- 2026-10-10: M139's second criteria audit returned while this milestone was in progress. Its findings are recorded in M139's own work log, and that file now says its criteria need a re-cut through /milestone-plan before M139 is implemented. Nothing of M139's scope is dropped, and M138 is unaffected.

## Decisions

## Review

### Pass 1 (2026-10-10) — returned

spawned: diff-bug, blame-history, prior-review.

**Outcome: a floor return.** Three criteria fail as written and two real defects
slipped past the implement phase. No acceptance-criterion box is ticked in this
pass: the fixes touch the domains of several, so every tick waits for the pass
that follows them.

Evidence gathered before the return:
- AC4: `grep -n "Sys.which" R/*.R | grep -v ":[0-9]*: *#"` returns exactly two
  call sites, `R/program_management.R:86` (the predicate's bare-name branch) and
  `:103` (`find_program()`'s own lookup). The three consumers ask the predicate
  at `:146`, `:1035` and `:1821`. Verified, not ticked, pending the pass after
  the fixes.
- AC1's enumeration re-run: the procedure returns three example sites and the
  vignette chunks. Its recorded referent has drifted — `R/ffmpeg.R:5266` is now
  `:5347`, and the three `vignettes/verification.Rmd` line numbers moved when
  the setup chunk grew. Review does not edit criterion text, so this is
  recorded rather than amended.
- `tools/vignette_chunk_guards.R` reported one unguarded spawning chunk on the
  branch and none at the merge base: putting `has_encoder()` in the setup chunk
  made it start FFmpeg. Fixed at `5ecfa20b` by moving the question into its own
  chunk guarded on `has_both`; the tool reports none again.
- The suite and check sweep was stopped mid-run when the return became certain,
  so this pass carries no AC2 or AC7 figures of its own.

**Floor findings — what sends it back.**

diff-bug #2: `?ffm_run`'s example names `libx264` for video and re-encodes audio
to the MP4 container's default with no `-c:a`. Measured: the compiled command is
`-y -i … -vf "scale=w=160:h=120" -codec:v libx264 out.mp4`. AC1 requires every
running site to name the encoder it uses rather than inherit the container's
default, so AC1 fails at the one site the milestone exists for, and D095's claim
that the rule "leaves no site whose encoder is decided elsewhere" is false. The
wrapper cannot see it: with `aac` hidden, `has_encoder("aac")` answers FALSE
while the pipeline still encodes with `aac`, because no argument names it — so
AC2's all-three-hidden run passed vacuously here. — floor return

diff-bug #1 and #11: AC2(c) says two of the four recorded lines are gates and
that the criterion is met only where the encoder line shows `libx264` absent.
The committed `.github/workflows/fedora-cran-model.yaml` only echoes both,
nothing exits non-zero, `R CMD check … || true` discards the status, and the
error test greps for `^Status:.*ERROR`, so a check that dies before writing a
`Status:` line reports `OK: no error`. Its dependency list also omits `furrr`,
`future` and `roxygen2`, all in Suggests, so a run of this file would carry a
NOTE the recorded reading does not have, and its grep is narrower than the
criterion's. AC2(c) fails as written. — floor return

diff-bug #13: AC5 says the install-side arm is asserted "under the install mock
that holds `tm_os()` at `"windows"`". The committed test calls
`tm_usable_binary()` directly with no such mock. The assertion is sound but the
criterion's stated instrument is absent, so AC5 cannot be read off the test.
— floor return

diff-bug #6: T7 is ticked and was not done. `tidymedia_location_not_executable`
appears in no roxygen block and no `man/` page, while `?find_ffmpeg` still reads
"The warning has one of two classes that you can catch" and `?program_status`
still reads "There are two cases" — both false after this diff, both enumerating
a partition missing the state this milestone adds. Shipped help is now wrong.
— floor return

**Fix now, below the floor.**

diff-bug #7: `vignettes/verification.Rmd`'s chunk at `:217` reads `jobs`, defined
only in a `has_mp3` chunk, while being guarded on `has_both`. On a build without
`libmp3lame` it renders `object 'jobs' not found` into the shipped vignette;
`error = TRUE` is why the hidden-encoder run did not catch it. — fix now

diff-bug #3: the reached-limit exclusion I added removed a comparison that was
passing for `program_status`, and masked a real mismatch rather than recording
it: measured, `tm_timeout_reached_master()[["has_encoder"]]` is `"warn"` while
`tm_force_timeout("has_encoder", …)` aborts. The repair is a dated post-baseline
measurement beside the `ae5ff1c` list, so every member is compared and each
value carries its own provenance. — fix now

diff-bug #4: the wrapper substitutes its sentinel into any argument holding the
hidden name as a word, path components included, so a temp path containing
`libx264` is rewritten and the reverse `sed` puts the name back — the diagnostic
then names a directory that does exist. — fix now

diff-bug #5: the wrapper's prefix rule is listing-only. `libx264rgb` is dropped
from `-encoders` but still encodes, so the header's claim is false as stated and
a site naming `libx264rgb` would pass here and fail on Fedora. — fix now

diff-bug #8: `tm_usable_binary(NA_character_)` and `tm_usable_binary("sh")` now
answer `NA` where the old body answered FALSE, and `tm_program_state()` has an
undocumented fourth state against its own header. Latent from the one caller,
but an `NA` in `unpacked[!…]` would garble the refusal message. — fix now

diff-bug #9: NEWS says the lookup moved to the filesystem and does not say that a
Windows location saved without an executable extension stops resolving. D096's
falsifier names the shape; the release note does not. — fix now

diff-bug #10: `path.expand()` in `tm_program_state()` is a no-op — measured,
`file.info()` and `file.access()` both expand `~` themselves — and its comment's
second clause does not follow from its first. The M107 shape. — fix now

diff-bug #14c: `cairn/DESIGN.md`'s Known issues and D096 both assert that Windows
reads `file.access(path, 1L)` as existence, written as measured and measured
nowhere. It is load-bearing: were it to answer -1, `tm_usable_binary()` would
refuse every binary `install_on_win()` unpacks. — fix now

prior-review #4, already fixed at this pass: the absorber comment asserted which
arm reaches a line, read off the branch structure. It now cites the reached-limit
sweep as the derivation. — fixed

prior-review #1, already fixed at this pass: the skip helper claimed one
`ffmpeg -encoders` for the whole session. `setup-nvenc-memo.R`'s state inspector
discards the memo before and after every `test_that()` block, so it is one per
guarded block. Measured 2026-10-10: 46ms cold, 6ms warm. — fixed

**Rejected, with the ground.**

prior-review #1's own claim, that a warm memo makes the sweep's `has_encoder`
cell silent and the suite red: false against the code. `setup-nvenc-memo.R:10-13`
installs a state inspector that calls `forget_ffmpeg_capabilities()` before and
after every block, so the memo is always cold when the cell runs, which is why
three sweep runs were green. — rejected, false

blame-history #3a, that the empty-file premise now asserts less: the test asserts
`tm_runnable_program(f$empty)` TRUE beside `tm_usable_binary(f$empty)` FALSE,
which is exactly "refused for its size and not for failing to resolve".
— rejected, false

blame-history #8's first half, that not every Fedora failure is covered because
`make_test_video()` kept the plain guard: the wrapper run is the oracle for which
tests execute an encoder, and the `libx264`-hidden suite reports 0 failures.
— rejected, false

blame-history #2, that a directory mapping to "absent" is coarser than D084's
wording: it matches the behaviour before this diff, where `Sys.which()` refused a
directory and `find_program()` reported it as gone. — rejected, planned change

diff-bug #12, the blanket muffling in `tm_encoder_available()`: the same policy
`program_status()` has carried since D088, consistent rather than new. The
reviewer says so itself. — rejected, not new

prior-review #3, waldo and the base-namespace `Sys.which` mock: the test this
replaced mocked the same binding, so the precedent is the repo's own, and nothing
shows waldo calls `Sys.which`. — rejected, style

**Follow-up (candidate rows).**

- blame-history #1 and prior-review #2, the Windows path shapes: an extensionless
  absolute path, and a `~` location the predicate accepts and `CreateProcess`
  may not expand. R's own `?Sys.which` defines a Windows executable as a file
  with extension `.exe`, `.com`, `.cmd` or `.bat`, so the extensionless form was
  not an executable to the old code either, and this project cannot measure
  either shape. D096's falsifier names them.
- blame-history #4, the `file.access` pin: the sweep now allows two callers and
  cannot tell a read check from an execute check, so a second readability reader
  inside the predicate would pass unseen.
- diff-bug #14a, `set_program()`'s refusal still says "Can't find an executable"
  for a file that exists and cannot be run, now that the predicate can tell them
  apart. Pre-existing, and outside AC6's domain, which is `find_program()`.
- diff-bug #14d, the Fedora job reinstalls its whole dependency tree on every
  pull request with no caching.
- blame-history #8's second half, `skip_if_no_encoder()` asks the package's
  resolved FFmpeg while `skip_if_no_ffmpeg()` and `run_ffmpeg_fixture()` read
  `PATH`, so the two can disagree where a remembered location overrides `PATH`.

