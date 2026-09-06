# M114: Verification, provenance and timeouts are taught in prose, not only on a reference page

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Surface tier:** user-facing — vignettes shipped in the package and on the pkgdown site
- **Resolves:** —
- **Branch/PR:** `m114-verification-and-timeout-vignette`

## Goal

Give `verify_media()`, `ffm_manifest()` and the timeout surface the narrative
documentation they have never had — an advertised pkgdown pillar and a
session-blocking hazard, both currently reachable only from reference pages.

## Scope

**In:** a vignette teaching verification and provenance (`verify_media()`,
`ffm_manifest()`, `ffm_batch(manifest = TRUE)`, checksums) and the timeout
surface (`with_timeout()`, `local_timeout()`, `options(tidymedia.timeout)`);
its `_pkgdown.yml` `articles:` row; a cross-link from `workflow.Rmd:189-195`,
which discusses reproducibility today without naming the manifest facility.

**Out:** the setup and troubleshooting story — README's first-call step, its
macOS dead-end, its unguarded chunks → M115. The four builder verbs missing
from `tidymedia.Rmd`'s Layer 1 tour (`ffm_fps`, `ffm_drawbox`, `ffm_loudnorm`,
`ffm_output_options`) and the capability family (`ffmpeg_codecs`,
`ffmpeg_encoders`, `hardware_encoder`, `refresh_ffmpeg_capabilities`) →
candidate row. No behaviour change to any function this vignette teaches.

## Acceptance criteria

- [ ] AC1: The new vignette calls `verify_media()`, `ffm_manifest()` and
      `ffm_batch(manifest = TRUE)` in evaluated chunks and shows each one's
      output, and states what a manifest records that a compiled command does
      not. Evidence: the built vignette's rendered output for each of the three.
- [ ] AC2: The new vignette calls `with_timeout()` and `local_timeout()` in
      evaluated chunks and states the bound base R actually gives — the limit
      plus up to 40 s, per D056 — rather than promising the limit. Evidence:
      the rendered output and the sentence, quoted.
- [ ] AC3: Every chunk in `vignettes/` whose `eval` option is not `FALSE` and
      which calls a function that spawns a program is guarded on that program's
      presence. Verified by a script that parses every `.Rmd` under
      `vignettes/` with `knitr`, lists each chunk's label and evaluated `eval`
      value, and reports the guard each spawning chunk carries. Evidence: that
      listing, over every chunk in every vignette, not only the added ones.
- [ ] AC4: The vignettes build with no FFmpeg, ffprobe or MediaInfo reachable,
      on a `PATH` that still reaches pandoc. Evidence: the build log, plus the
      three `Sys.which()` answers recorded as empty inside the build.
- [ ] AC5: The vignette has an `articles:` row in `_pkgdown.yml`,
      `pkgdown::check_pkgdown()` passes, and `workflow.Rmd`'s reproducibility
      section links to it. Evidence: the check output and the diff.
- [ ] AC6: `devtools::check()` clean (0 errors, 0 warnings) with vignettes
      rebuilt.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T6

## Tasks

- [x] T1: Read `R/verify.R` and `R/ffm_manifest.R` and write the verification
      half against executed calls, not against the roxygen — the M088 lesson is
      that prose derived from one path over-generalizes.
- [x] T2: Write the provenance half: what `ffm_manifest()` records, and the
      reproducibility claim `workflow.Rmd:189-195` makes today without it.
- [x] T3: Write the timeout half from `?tidymedia`'s existing "Bounding a run
      that hangs" section plus D047/D048/D049/D056, stating the measured bound.
- [x] T4: Write the AC3 chunk sweep as a committed script; run it over the
      existing four vignettes first, so the added chunks are measured by an
      instrument that already reports the current state.
- [x] T5: Build under a reduced `PATH` that keeps pandoc; record the three
      `Sys.which()` answers from inside a setup chunk.
- [x] T6: `_pkgdown.yml` row, `workflow.Rmd` cross-link, `check_pkgdown()`,
      `devtools::check()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned three findings against this milestone's draft: "a vignette teaches X" named no world-state, so any mention satisfied it; the chunk-guard criterion was universal over a hand list rather than a procedure, and mis-cited `vignettes/metadata.Rmd:18-19` as a guard when it is the flag definition; and the `PATH`-emptied build also hides pandoc, so the build could fail for a reason the criterion would misread. All three fixed before writing; none needed a gate question.
- 2026-09-05: plan gate chose one vignette covering verification, provenance and timeouts over three separate ones, because a reader meets all three at the same moment — after a batch has run and something went wrong. Falsified by a reader who wants the timeout material without the manifest material, which the vignette's own section structure can answer before a split is warranted.
- 2026-09-05: plan gate chose a knitr-parsed sweep over every vignette chunk over asserting the guards on the added chunks only, because the added chunks are a hand list and the M118-class failure is the site the list omits. Falsified by a spawning call the sweep's function list does not name.
- 2026-09-06: measured before the implementation gate, this machine with ffmpeg/ffprobe/mediainfo on `PATH`, `base::system`/`system2` traced: the eleven currently-evaluated `run = FALSE` chunks in `tidymedia.Rmd`, `workflow.Rmd` and `batch.Rmd` make 0 spawns each; controls `probe_all()` 2, `verify_media()` 2, `extract_audio(run = TRUE)` 4.
- 2026-09-06: implementation gate took all four recommendations. AC3's sweep decides "spawning chunk" by MEASURING each chunk (knit under a spawn counter) rather than by a call-graph name list, which would have demanded guards on those eleven zero-spawn chunks; the vignette is `verification.Rmd`, "Checking results and bounding runs"; guarded chunks render empty on a binary-less build as `metadata.Rmd` already does, no hand-copied static output; the sweep is a `tools/` developer script, since `vignettes/` is absent from the built package a test would run against.

- 2026-09-06: T1-T3 landed in one commit rather than three. Every claim in the vignette is derived from an executed call, not from the roxygen: `verify_media()`'s pass/fail/`NA` shapes, the tolerance rule, the extra-field resolution order, `ffm_batch(verify =)`'s `verified` column and its non-aborting failure, the manifest's nine columns and CSV write, the two "no manifest attached" aborts, `with_timeout()`/`local_timeout()`'s return and restoration, and the fractional-limit refusal message.
- 2026-09-06: T3 states the bound as the limit plus up to 40 s, quoting D056's measured 42.0 s under a 2 s limit on Linux and macOS, rather than promising the limit.
- 2026-09-06: found while deriving T3, out of scope here: `tidymedia.Rmd:41` says a task verb "returns the path it wrote", and the evaluated chunk above it shows `extract_audio()` returning the compiled command string. Candidate row added.

- 2026-09-06: T4 sweep (`tools/vignette_chunk_guards.R`) knits each vignette twice — pass 1 on this machine's full `PATH`, counting each chunk's calls to `system()`/`system2()` where the program started is one of ffmpeg/ffprobe/mediainfo; pass 2 in a child process whose `PATH` reaches none of the three, recording per chunk whether knitr still evaluated it. A chunk that spawned in pass 1 and still evaluates in pass 2 is UNGUARDED and exits 1. Counting only the three media programs is load-bearing: `Sys.which()` itself shells out, so an unfiltered count reports every guarded setup chunk as a spawning chunk.
- 2026-09-06: sweep run over all five vignettes: 64 chunks, 15 started a program, every one guarded, none unguarded. The eleven `run = FALSE` chunks in the existing vignettes measure 0 spawns, matching the pre-gate measurement.
- 2026-09-06: sweep proven able to fail — an unguarded `probe_all()` chunk planted in `verification.Rmd` was reported as the single UNGUARDED row, exit 1; reverted.

- 2026-09-06: T5 build ran through `tools/build_vignettes_without_binaries.R`, which puts a scratch directory holding a symlink to pandoc alone ahead of R's own bin and `/usr/bin:/bin` — necessary because pandoc and ffmpeg share `/opt/homebrew/bin` on this machine, so dropping the directory would drop pandoc and fail the build for an unrelated reason. All five vignettes rebuilt; `verification.Rmd`'s setup chunk reported from inside the build `ffmpeg=[] ffprobe=[] mediainfo=[]`. `devtools::build_vignettes()` added `^doc$` and `^Meta$` to `.Rbuildignore`, kept.

- 2026-09-06: T6 added the `verification` `articles:` row, the `workflow.Rmd` cross-link (a paragraph in its Reproducibility section plus a Where-to-next entry) and the NEWS Documentation entry. `pkgdown::check_pkgdown()`: "No problems found". `tools/pkgdown_duplicate_topics.R`: 80 contents entries, 81 man topics, none unmatched, none repeated.
- 2026-09-06: `vignettes/audio.m4a` — a build artifact `tidymedia.Rmd`'s evaluated chunk writes into `vignettes/` — was swept into the T1-T3 commit by `git add -A`. Untracked again here and `.gitignore` given entries for vignette build outputs. The new vignette knits into `tempdir()` and writes nothing beside the sources; the older three still do.
- 2026-09-06: prose corrected against the source before the final check: the manifest bullet list had called `input`/`output` things the command cannot carry, which the command does carry; the tolerance and structural-check claims were re-derived; the 42.0 s figure now carries its measurement date; and a sentence was added recording that `manifest =`, `checksums =` and `verify =` reach the `*_batch()` verbs through `...`, verified by calling `extract_audio_batch()` with each.

- 2026-09-06: completion checks on the final tree. `devtools::check(document = TRUE, vignettes = TRUE)` 0 errors / 0 warnings / 0 notes in 17m 5s, with "checking tests", "checking package vignettes" and "checking re-building of vignette outputs" all OK and `document()` leaving no diff. `devtools::test()` separately clean earlier on identical R code: FAIL 0, WARN 10, SKIP 18, PASS 12900. `tools/vignette_chunk_guards.R` exit 0 over the final text: 64 chunks, 15 spawning, all guarded. `tools/build_vignettes_without_binaries.R` exit 0, `ffmpeg=[] ffprobe=[] mediainfo=[]` from inside the build. `cairn_validate` all checks passed.
- 2026-09-06: the new candidate row was merged into the existing vignette-documentation row rather than added, because a separate line put `ROADMAP.md` at 60 of its <60-line cap. The file is 59 lines / 27,485 bytes — under the line cap, still over the 24,000-byte budget it was already over before this milestone, with `/cairn-triage` still the named remedy.
- 2026-09-06: status to review.

## Decisions

## Review
