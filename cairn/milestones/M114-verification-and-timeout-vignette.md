# M114: Verification, provenance and timeouts are taught in prose, not only on a reference page

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Surface tier:** user-facing — vignettes shipped in the package and on the pkgdown site
- **Resolves:** —
- **Branch/PR:** `m114-verification-and-timeout-vignette` / https://github.com/jmgirard/tidymedia/pull/118

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
- [x] AC2: The new vignette calls `with_timeout()` and `local_timeout()` in
      evaluated chunks and states the bound base R actually gives — the limit
      plus up to 40 s, per D056 — rather than promising the limit. Evidence:
      the rendered output and the sentence, quoted.
- [x] AC3: Every chunk in `vignettes/` whose `eval` option is not `FALSE` and
      which calls a function that spawns a program is guarded on that program's
      presence. Verified by a script that parses every `.Rmd` under
      `vignettes/` with `knitr`, lists each chunk's label and evaluated `eval`
      value, and reports the guard each spawning chunk carries. Evidence: that
      listing, over every chunk in every vignette, not only the added ones.
- [ ] AC4: The vignettes build with no FFmpeg, ffprobe or MediaInfo reachable,
      on a `PATH` that still reaches pandoc. Evidence: the build log, plus the
      three `Sys.which()` answers recorded as empty inside the build.
- [x] AC5: The vignette has an `articles:` row in `_pkgdown.yml`,
      `pkgdown::check_pkgdown()` passes, and `workflow.Rmd`'s reproducibility
      section links to it. Evidence: the check output and the diff.
- [x] AC6: `devtools::check()` clean (0 errors, 0 warnings) with vignettes
      rebuilt.

## Coverage

- AC1 → T1, T2, T7
- AC2 → T3
- AC3 → T4, T7
- AC4 → T5, T7
- AC5 → T6
- AC6 → T6, T7

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
- [x] T7: review return — show `ffm_batch(manifest = TRUE)`'s own output (AC1),
      close the remembered-location seam the no-binary build left open (AC4),
      and land the fix-now findings: the sweep's space-in-path blind spot, the
      cross-chunk guard mismatch, the elided md5 columns, the `ffprobe_version`
      and fractional-limit clauses, the NEWS 40-second wording, and the check
      NOTE's `behaviour`.

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
- 2026-09-06: review gathered fresh evidence against branch head `1843446`; draft PR #118 opened. AC2, AC3, AC5 and AC6 verified and ticked; AC1 and AC4 failed, so status returns to in-progress. AC1: `ffm_batch(manifest = TRUE)` is called in three evaluated chunks but every one assigns its result, so its output is shown nowhere in the rendered vignette. AC4: the no-binary build ran with FFmpeg still reachable, because `find_program()` falls back to a config file the script does not clear, so the build does not evidence that the guards carried it. Also to fix on the return: R CMD check's 1 NOTE (`behaviour` at `verification.Rmd:234` against the package's `behavior`), and review findings 2, 3, 6, 8, 9 and 11 recorded in the Review section.

- 2026-09-06: T7, AC1. The two `ffm_batch(manifest = TRUE)` chunks assigned their result and printed only `ffm_manifest(res)`, so the batch call's own output appeared nowhere. The first one now prints `res` as well, and the prose says what it shows — an ordinary four-column batch result, the manifest riding along on it as an attribute. Measured by knitting `vignettes/verification.Rmd` on this machine with all three programs on `PATH`: the rendered page carries the 1 × 4 result and, below it, the 7-column manifest.
- 2026-09-06: T7, AC4. `tools/build_vignettes_without_binaries.R` now points `R_USER_CONFIG_DIR` and `XDG_CONFIG_HOME` at one empty scratch directory before building, and asserts `find_ffmpeg()`, `find_ffprobe()` and `find_mediainfo()` all return `NULL` — the resolver the package itself calls, rather than `Sys.which()` alone. `HOME` is deliberately left alone: moving it moves the user library and would break the build for an unrelated reason, so instead the two resolved seam paths are checked to have landed under the scratch directory. They are compared as written with runs of `/` collapsed, because the directories do not exist yet for `normalizePath()` to resolve and on macOS it rewrites the scratch path's `/var` to `/private/var`.
- 2026-09-06: the AC4 strengthening measured, not assumed. An unguarded FFmpeg-only chunk (`ffm_files(...) |> ffm_drop("video") |> ffm_run()`) planted in `verification.Rmd`: the pre-fix script at `98df1de` built it and exited **0**, because this machine's `ffmpeg_location.txt` resolved FFmpeg on the reduced `PATH`; the fixed script exits **1**, "failed re-building 'verification.Rmd'". That is finding [O]1's failure reproduced and then closed. Plant reverted, tree confirmed clean.
- 2026-09-06: T7, finding 2. `spawn_record()` read the command line's first whitespace-delimited token, so an FFmpeg under a directory holding a space reported no spawn at all and a wholly unguarded vignette would have exited 0. It now tests every leading run of tokens, plus a leading double-quoted path, and matches on each candidate's basename. Nine cases run against the function directly: `/opt/homebrew/bin/ffmpeg -y -i x.mp4`, `/Volumes/My Tools/bin/ffmpeg -y -i x.mp4` and its double-quoted form all count; `which ffprobe`, `which 'ffprobe'`, `command -v ffmpeg`, `/usr/bin/ls -l` and `/usr/bin/env echo "ffmpeg"` all do not; `/opt/bin/mediainfo --Output=JSON file.mp4` counts. Matching on the whole prefix's basename is what keeps `Sys.which()` out — "which ffprobe" has no `/`, so its basename is the whole two-word string.
- 2026-09-06: T7, findings 3, 6, 8, 9 and 11, each derived from an executed call or the source, not composed. **3**: the `run = FALSE` abort chunk was guarded `has_ffmpeg` but uses `jobs`, built in a `has_both` chunk; guard changed to `has_both`. **6**: the 9-column manifest prints `input_md5`/`output_md5` as a footer line, so a second call selecting those two columns now shows the values, with a sentence saying why. **8**: `ffprobe_version` is no longer called "the version that actually ran" — FFprobe processes nothing in a job like this one; it is the version tidymedia resolved and had on hand. **9**: the fractional-limit prose named `options(tidymedia.timeout = 0.5)` while the chunk shows the `with_timeout()` seam; it now names both, and the option path was confirmed to refuse — "`tidymedia.timeout` must be a whole number, not the number 0.5". **11**: NEWS said a program "outlives" R's wait by 40 seconds, which is backwards; it now says R waits up to 40 seconds past the limit.
- 2026-09-06: T7, the check NOTE. `vignettes/verification.Rmd`'s one `behaviour` is now `behavior`, matching the 25 sites in `R/` and `workflow.Rmd`. `Comparing 'spelling.Rout' to 'spelling.Rout.save' ... OK` in the check below.
- 2026-09-06: T7 instruments re-run over the final text. `tools/vignette_chunk_guards.R` exit 0: 64 chunks in five vignettes, 15 started a program, every one guarded, "unguarded spawning chunks: none" — unchanged from the pre-return run, the hardened matcher having nothing to catch on a machine whose paths hold no spaces. Discrimination re-proved after the hardening: a planted unguarded `probe_all()` chunk was the single UNGUARDED row, exit 1; reverted. `tools/build_vignettes_without_binaries.R` exit 0, `find_ffmpeg(): NULL`, `find_ffprobe(): NULL`, `find_mediainfo(): NULL`, and from inside the build `ffmpeg=[] ffprobe=[] mediainfo=[]`.
- 2026-09-06: checkpoint committed with `devtools::check(document = TRUE, vignettes = TRUE)` still running on the final tree — `devtools::test()` was already clean on it (FAIL 0, WARN 10, SKIP 18, PASS 12900) and the check had reached "checking tests" with `Comparing 'spelling.Rout' to 'spelling.Rout.save' ... OK`, which is the NOTE this return had to clear, but the run had not yet reported its own result line. T7 stays unchecked until it does.
- 2026-09-06: the check the line above was waiting on finished on that same tree: `devtools::check(document = TRUE, vignettes = TRUE)`, 5m 38.7s, **`Status: OK` — 0 errors, 0 warnings, 0 notes**, with "checking tests", "checking package vignettes" and "checking re-building of vignette outputs" all OK and `document()` leaving no diff. The 1 NOTE the return carried is gone from R CMD check's own status line, not only from devtools' summary. `cairn_validate` all sixteen checks PASS, seven advisories OK. T7 checked; status to review.

## Decisions

## Review

Fresh evidence gathered 2026-09-06 on branch head `1843446`, against `origin/master` at `6af61cf` (default branch had not moved; no merge needed). Draft PR #118.

**AC1 — NOT MET.** `verify_media()` is called in four evaluated chunks and every report is rendered (pass/fail/`NA`/extra-field shapes); `ffm_manifest()` is called in three and renders a 7-column manifest, a 9-column one with checksums, and the "No provenance manifest is attached" abort. `ffm_batch(manifest = TRUE)` is called in three evaluated chunks, but every one of them assigns to `res` or `compiled`, so **none of its output is shown** — the rendered page carries a batch result only for the earlier `verify =` call, which is a different invocation. The manifest-vs-command sentence and its four bullets are present and correct. Measured by knitting `vignettes/verification.Rmd` on this machine with all three programs on `PATH`.

**AC2 — met.** `with_timeout()` and `local_timeout()` are each called in an evaluated chunk; both render, and both show `getOption("tidymedia.timeout")` back to `NULL` afterwards. The bound is stated as "waited for up to **40 seconds longer than the limit you set** — measured on 2026-08-28 at 42.0 s under a 2 s limit, on Linux and macOS alike", not as the limit. Checked against D056's case A1 (42.00 s container, 42.03 s host): the numbers and date match.

**AC3 — met.** `tools/vignette_chunk_guards.R` exit 0 over the final text: 64 chunks in five vignettes, 15 started one of the three programs, every one guarded, "unguarded spawning chunks: none". The listing covers every chunk in every vignette, not only the added ones. Discrimination re-proved fresh at review, not taken from the work log: an unguarded `probe_all()` chunk planted in `verification.Rmd` was reported as the single UNGUARDED row and the script exited 1; the plant was reverted and the tree confirmed clean.

**AC4 — NOT MET.** `tools/build_vignettes_without_binaries.R` exit 0; all five vignettes rebuilt on a `PATH` of a pandoc-only shim plus R's bin and `/usr/bin:/bin:/usr/sbin:/sbin`; `Sys.which()` empty for all three programs both in the script's own table and, from inside the build, in `verification.Rmd`'s setup chunk (`ffmpeg=[] ffprobe=[] mediainfo=[]`). The criterion's evidence clause is therefore satisfied — but finding [O]1 below shows the criterion's own claim is not: `find_program()` falls back to a config file, this machine has `~/Library/Preferences/org.R-project.R/R/tidymedia/ffmpeg_location.txt` holding `/opt/homebrew/bin/ffmpeg` (verified present at review), and FFmpeg runs under exactly that reduced `PATH`. FFmpeg was reachable during the build, so the build does not evidence that the guards carried it.

**AC5 — met.** `_pkgdown.yml` gains one `articles:` row, `verification`. `pkgdown::check_pkgdown()`: "No problems found". `tools/pkgdown_duplicate_topics.R`: 80 contents entries, 81 man topics, none unmatched, none repeated. `workflow.Rmd`'s Reproducibility section gains a paragraph pointing at `vignette("verification")` and its Where-to-next list a matching entry; both in the diff.

**AC6 — met, with one NOTE.** `devtools::check(document = TRUE, vignettes = TRUE)`, 14m 58s: **0 errors, 0 warnings**, and "checking tests", "checking package vignettes" and "checking re-building of vignette outputs" all OK. `document()` left no diff. R CMD check's own status line reports **1 NOTE**, which devtools' summary did not surface: the `spelling` package's `.Rout.save` comparison differs because `vignettes/verification.Rmd:234` spells `behaviour`, where the rest of the package spells `behavior` (25 sites across `R/` and `vignettes/workflow.Rmd`, and `inst/WORDLIST` carries neither).

**Consistency gate — passed.** `cairn_validate.py` exit 0, all sixteen checks PASS and all seven advisories OK. No `DESIGN.md` principle changed (`Principles touched: —`), so `cairn_impact.py` did not apply. Toolchain slot: `document()` no diff (above); no generated file hand-edited; `README.Rmd`/`README.md` untouched by this branch and last committed together; `check_pkgdown()` passes; `NEWS.md` has a Documentation entry with no milestone numbers; the one new top-level directory, `tools/`, already carried `^tools$` in `.Rbuildignore`, and the build added `^doc$` and `^Meta$`; `devtools::check()` as above.

### Independent review

Three fresh-context reviewers, none having seen the implementation, each on a distinct evidence base. Every reported finding is logged; the gate was not reached, so no maintainer triage was taken — dispositions below are the review's own reading, and the return list is what the milestone goes back for.

**[S] blame-history — no findings.** Traced `workflow.Rmd`'s reproducibility section to M30, the `.Rbuildignore`/`.gitignore` entries to M111 and the original scaffold, the `_pkgdown.yml` `articles:` pattern to every prior vignette addition, and the timeout prose to D047/D048/D049/D056 and M078. Nothing reverses a prior decision, resurrects a fixed bug or contradicts a D-entry.

**[S] prior-review record — no findings.** Archived `## Review` sections on the touched files, plus `LESSONS.md` in full. M111's tarball rule (a `.gitignore` path is not out of the tarball) is honoured rather than regressed by the `^doc$`/`^Meta$` additions; M55's NEWS-splice finding does not apply, the NEWS change being a pure insertion. Probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so the GitHub thread surface was not walked.

**[O] diff-bug — fifteen findings, ranked as reported.**

1. **AC4's build never had FFmpeg unreachable.** The script asserts only `Sys.which()`; `find_program()` also reads a config file, which this machine has. Verified at review. Removing a guard would leave the build green. Fix: clear the config seam in the child (`R_USER_CONFIG_DIR`/`XDG_CONFIG_HOME` to an empty temp dir) and assert `find_ffmpeg()`/`find_ffprobe()`/`find_mediainfo()` return `NULL`. → **return**, with AC4.
2. **The sweep reports "no spawn" for every chunk if a binary path contains a space.** `tools/vignette_chunk_guards.R:56-63` takes the command line's first whitespace-delimited token; `R/ffmpeg.R:33` passes the path unquoted. A wholly unguarded vignette would then exit 0. → **fix now** on the return.
3. **Cross-chunk guard mismatch.** The `eval = has_ffmpeg, error = TRUE` chunk at `vignettes/verification.Rmd:213` uses `jobs`, created only in the `eval = has_both` chunk at `:112`. Verified at review. On a machine with FFmpeg but no FFprobe the page renders `object 'jobs' not found` exactly where it promises the manifest abort, and `error = TRUE` keeps the build green. The chunk spawns nothing, so the guard can be `has_both` or dropped. → **fix now** on the return.
4. **The sweep cannot tell "guarded" from "guarded on the wrong program".** Pass 2 removes all three programs together, so a chunk spawning FFprobe under an `has_ffmpeg` guard reads "guarded". Three chunks in the new vignette are in that position via `extract_audio()`'s dropped-track probe. Benign today (`count_audio_streams()` returns `NA` when FFprobe is absent) but AC3's "that program's presence" is asserted by an instrument that does not test it. → **follow-up**, candidate row.
5. **The condition-class sentence sits under a wider warn list.** `probe_all()`'s warning (`R/ffprobe.R:163`) and the MediaInfo readers' (`R/mediainfo.R:291`) carry no class; only the version-probe (`R/ffm_manifest.R:164`) and dropped-track (`R/ffprobe.R:305`) sites do. Verified at review: the vignette's sentence names exactly those two, so it is literally accurate and matches `R/tidymedia-package.R:67-70`; the trap is the paragraph above it listing `probe_all()` and the `get_*()` helpers as warn-path members. → **follow-up**, candidate row (it is the shipped roxygen's shape, not this branch's).
6. **The checksums chunk elides the columns it teaches.** The 9-column manifest prints `# ℹ 2 more variables: input_md5 <chr>, output_md5 <chr>`, so the md5 values the prose promises never appear. → **fix now** on the return, alongside AC1.
7. **Two `error = TRUE` chunks can never fail a build,** so AC4's evidence covers 14 of 16 chunks. → **noted**, subsumed by 1.
8. **`ffprobe_version` is not "the version that actually ran".** In a plain job FFprobe processes nothing; `tool_versions()` spawns it only for `-version`. → **fix now** on the return (one clause).
9. **The fractional-limit demo does not show the sentence's subject.** The prose blames `options(tidymedia.timeout = 0.5)`; the chunk shows `` `seconds` must be a whole number ``. Both paths do refuse, so the claim is true and the shown output does not evidence it. → **fix now** on the return (one clause).
10. **"on Linux and macOS alike" is stronger than D056's framing,** which calls the host figures "context, not a second platform under test". Matches shipped `R/timeout.R:180`. → **follow-up**, candidate row.
11. **NEWS attaches the 40 s to the wrong noun** — "how long R waits, which a program … outlives by up to 40 seconds" says the program survives R's return, which is the `stdout = ""` regime D056 says tidymedia never takes. → **fix now** on the return.
12. **Both tools drop `/usr/bin` when FFmpeg lives there,** which is a normal Linux CI image; the reduced `PATH` would then lose the system tools and fail for an unrelated reason. → **follow-up**, candidate row.
13. **The sweep's `eval`/guard columns are parsed, not measured,** and a chunk header quoted in prose or a guard containing a comma can desync them; a loud header/chunk count mismatch catches it today and the verdict column does not depend on it. → **reject**, cosmetic.
14. **The sweep's operational definition is narrower than AC3's wording** — it decides by whether a spawn happened on the dev machine, and both passes knit with `error = TRUE` while the real build uses `error = FALSE`. → **follow-up**, candidate row.
15. **`.gitignore`'s `vignettes/*.R` and `vignettes/*.mp4` are broader than the artifacts they target** and would silently swallow a future committed helper or fixture. Verified at review: no tracked file matches today. → **follow-up**, candidate row.

### Gate outcome — returned

Two acceptance criteria are not met, so the milestone returns to `/milestone-implement` rather than reaching the merge gate: **AC1**, measured directly — `ffm_batch(manifest = TRUE)`'s output is shown nowhere in the rendered vignette — and **AC4**, whose build ran with FFmpeg still reachable through the config seam. The check NOTE and findings 2, 3, 6, 8, 9 and 11 are the fix-now work to land with them; findings 4, 5, 10, 12, 14 and 15 are candidate rows, and 7 and 13 need nothing.
