# M111: The shipped metadata and the tarball say what CRAN's incoming checks expect

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — DESCRIPTION and the tarball's contents are what an installer and the CRAN page show
- **Branch/PR:** `m111-shipped-metadata-and-tarball` / https://github.com/jmgirard/tidymedia/pull/115

## Goal

Rewrite `DESCRIPTION`'s `Title` and `Description` to the form CRAN's incoming
checks accept, and stop shipping four files and one URL that should not be in
the tarball.

## Scope

**In:** the `Title` and `Description` fields; removing
`inst/extdata/ffmpeg_location.rds`, `inst/extdata/mediainfo_location.rds` and
`tests/testthat/_problems/`; the retired Homebrew branch in `README.Rmd:74`;
`_pkgdown.yml`'s duplicate `hardware_encoder`/`has_hardware_encoder` rows and
the alias-only sections that hide `set_mediainfo` and `probe_video` from the
reference index.

**Out:** `cran-comments.md`, the version bump, win-builder and R-hub → the
CRAN readiness candidate row, promoted when a window is declared. README's
macOS dead-end and its unguarded chunks → M114. `SystemRequirements`'
URL-bearing form, which is legal and stays.

## Acceptance criteria

- [x] AC1: `R CMD check --as-cran` over the built tarball, run with
      `_R_CHECK_CRAN_INCOMING_=TRUE` and `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`,
      reports no NOTE naming the `Title` field or the `Description` field.
      Evidence: the check's complete NOTE list, quoted.
- [x] AC2: `Title` is in title case and writes 'tidyverse' in single quotes;
      `Description` writes 'FFmpeg', 'MediaInfo' and 'tidyverse' in single
      quotes, contains neither the substring "The goal of" nor a leading
      "tidymedia", and runs to at least two sentences. Evidence: both fields
      quoted verbatim, with each clause checked against the quoted bytes.
- [x] AC3: The tarball `R CMD build` produces contains no path matching
      `extdata/ffmpeg_location.rds`, `extdata/mediainfo_location.rds`, or
      `testthat/_problems/`. Evidence: `tar -tzf` over the built tarball,
      grepped for the three patterns, showing no hits and a non-zero total.
- [x] AC4: `urlchecker::url_check()` over the package reports no URL needing a
      change. Evidence: the checker's full output.
- [x] AC5: No topic name appears more than once across `_pkgdown.yml`'s
      `contents:` entries, verified by a script that parses the file and
      reports every repeated entry, and `pkgdown::check_pkgdown()` passes.
      Evidence: both outputs.
- [x] AC6: `devtools::document()` produces no diff and `devtools::test()` and
      `devtools::check()` are clean — 0 errors, 0 warnings — with each NOTE
      the check reports quoted and justified. Evidence: the three tails.

## Coverage

- AC1 → T1, T6
- AC2 → T1
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: Rewrite `Title` and `Description` in `DESCRIPTION:2-13`. The
      Description says what the package does with 'FFmpeg' and 'MediaInfo' —
      batch transformation and metadata extraction as tibbles — rather than
      restating the Title.
- [x] T2: Delete `inst/extdata/ffmpeg_location.rds` and
      `inst/extdata/mediainfo_location.rds`. Confirm first that nothing
      resolves either name at runtime: grep `R/` and `tests/` for
      `system.file`, `extdata` and `location` together, not for the basenames
      alone, which a `paste0()` would defeat.
- [x] T3: Delete `tests/testthat/_problems/`; record its one file's fate in
      the work log so the deletion is not silent.
- [x] T4: Point `README.Rmd:74`'s Homebrew URL at the branch Homebrew
      documents today, re-knit with `devtools::build_readme()`, and run
      `urlchecker::url_check()`.
- [x] T5: Collapse `_pkgdown.yml:120-123`'s duplicate rows to one topic and
      name `set_program`/`find_program`/`probe_container`'s aliases in their
      sections so `set_mediainfo` and `probe_video` are findable by scanning.
- [x] T6: Build the tarball, run the `--as-cran` check with the two
      environment variables set, then `devtools::check()` clean.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned four findings against this milestone's draft: the `tools:::.check_package_description()` criteria were vacuous, "not a restatement of the Title" was unmechanizable, the NOTE-count baseline was unfixed, and the `*_location.rds` basename grep already passed before any deletion. All four fixed before writing; none needed a gate question.
- 2026-09-05: plan gate chose `R CMD check --as-cran` with the incoming checks forced on over `tools:::.check_package_description()`, because the latter runs no title-case or boilerplate check at all — measured 2026-09-05, it returns empty against the current defective DESCRIPTION under both `strict = FALSE` and `strict = TRUE`. Falsified by an `--as-cran` run that stays silent on a Title CRAN then rejects by hand.
- 2026-09-05: plan gate chose to assert the removals against the BUILT TARBALL's contents over a grep for the removed basenames in the sources, because the sources never name them — the grep passes today, before any deletion. Falsified by a `.Rbuildignore` rule that hides a file from the tarball while it stays on disk and in the installed package.
- 2026-09-05: implement started on `m111-shipped-metadata-and-tarball`. Gate chose the Title "Media File Preprocessing and Metadata for the 'tidyverse'" over a title-cased copy of the shipped wording, and chose page names (`find_program`, `set_program`) over the `find_ffmpeg`/`set_ffmpeg` aliases in the reference index so the MediaInfo functions are visible when scanning.
- 2026-09-05: T3's deleted file, `tests/testthat/_problems/test-timeout-silence-55.R`, held a stale copy of `test-timeout-silence.R:59`'s `tm_timeout_absorbers()` assertion, omitting `separate_audio_video` — the live assertion supersedes it, so the deletion loses nothing.
- 2026-09-05: discovered sub-task under T3 — `_problems/` is in `.gitignore` but was not in `.Rbuildignore`, so `R CMD build` shipped it. Added `^tests/testthat/_problems$` so a later failing run cannot put it back in the tarball; the directory is deleted as well, not merely hidden.
- 2026-09-05: checkpoint, work half-done. T1-T5 edits are in the tree; no task checked off, `devtools::test()` still running past 10 minutes and README not yet re-knitted.
- 2026-09-05: T1 done. `devtools::test()` clean over the rewritten DESCRIPTION (FAIL 0 | WARN 10 | SKIP 18 | PASS 12614, 22m34s), `devtools::document()` produces no diff, `spelling::spell_check_package()` finds nothing.
- 2026-09-05: T4 done. Homebrew's installer URL moved from `install/master/` to `install/HEAD/`; README re-knitted. `urlchecker::url_check()` then found a second URL needing a change, the lifecycle badge's `https://www.tidyverse.org/lifecycle/#experimental` (301). Its redirect target `https://tidyverse.org/lifecycle/` returns 404 — measured 2026-09-05 with `curl -sI` — so urlchecker's own suggestion was not taken; the badge now points at `https://lifecycle.r-lib.org/articles/stages.html#experimental` (200). Re-run over 24 URLs: "All URLs are correct!".
- 2026-09-05: T5 done. Dropped the `has_hardware_encoder` row (an alias of `hardware_encoder`) and swapped `find_ffmpeg`/`set_ffmpeg` for the page names `find_program`/`set_program`. Added `tools/pkgdown_duplicate_topics.R`, which resolves each `contents:` entry to its `.Rd` file before counting, plus an `^tools$` `.Rbuildignore` entry; the script reports 80 entries, no repeated topic and no unmatched entry, and `pkgdown::check_pkgdown()` finds no problems (it stayed silent on the duplicate, so the script is the instrument). Shown able to fail: re-adding `has_hardware_encoder` makes it name `hardware_encoder.Rd <- hardware_encoder, has_hardware_encoder` and exit 1, and a planted `not_a_real_topic` entry is reported unmatched.
- 2026-09-05: `pkgdown::build_reference_index()` re-run over the edited file — `set_mediainfo()`, `find_mediainfo()`, `probe_video()` and `has_hardware_encoder()` each appear once in the rendered `docs/reference/index.html`, so listing page names rather than aliases loses no name from the scan.
- 2026-09-05: T2 and T3 verified against the built tarball. `R CMD build` produced 254 paths; `extdata/ffmpeg_location.rds`, `extdata/mediainfo_location.rds` and `testthat/_problems/` each match 0 of them, and the surviving `inst/extdata/` is the three files that should ship (`mediainfo_template_brief.txt`, `mediainfo_template_extended.txt`, `sample.mp4`). Nothing in `R/` or `tests/` resolves either deleted `.rds`: the only `extdata` reads are `sample.mp4` and `mediainfo_template_{brief,extended}.txt`, and no source names `location` beside `system.file` or `extdata`.
- 2026-09-05: T6 done. `R CMD check --as-cran` over the built tarball with `_R_CHECK_CRAN_INCOMING_=TRUE` and `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`: Status 1 NOTE, and the NOTE's whole body is the maintainer line plus "Version contains large components (0.1.0.9000)" — the dev-version suffix, whose bump this milestone puts Out. No NOTE names `Title` or `Description`. `devtools::test()` FAIL 0 | WARN 10 | SKIP 18 | PASS 12614; `devtools::check()` 0 errors, 0 warnings, 0 notes (17m5s); `devtools::document()` produces no diff.
- 2026-09-05: added two NEWS.md entries under Requirements, for the rewritten `Title`/`Description` and for the three paths the tarball no longer carries.
- 2026-09-05: all six tasks checked, status set to review.
- 2026-09-05: review opened PR #115 (draft). AC2, AC3, AC4 and AC5 verified with fresh evidence and ticked; AC1 and AC6 still running (`--as-cran`, `devtools::test()`, `devtools::check()`), three fresh-context reviewers still running. `cairn_validate` passes; `devtools::document()` no diff.
- 2026-09-05: gate directed fix-now on review findings 1, 4 and 7. Added `^tests/testthat/testthat-problems\.rds$` to `.Rbuildignore`; rewrote the NEWS tarball bullet so it no longer says the two `.rds` files were never part of the package or that testthat writes `_problems/`; added a header line to `tools/pkgdown_duplicate_topics.R` naming its `yaml` requirement. Finding 6's section move stays rejected: the dev-version block has no `## Documentation` section, and creating one for a single bullet is churn. AC1, AC3 and AC6 re-run against the fixed tree.
- 2026-09-05: step-7 approval: PR #115 approved for merge. Finding 2 (the script's silent skip of parenthesised entries) accepted as a follow-up candidate row; findings 3, 5, 6, 8, 9 and 10 rejected with reasons recorded above.
- 2026-09-05: CI wait hit the harness ceiling with checks still pending (pkgdown pass, seven legs pending); watcher stopped, no merge made. Approval marker `cairn/.merge-approved` stands for PR #115; re-enter at `/milestone-review M111`, which re-derives the check and merge state.

## Decisions

## Review

Evidence gathered 2026-09-05 on `m111-shipped-metadata-and-tarball` at `0b587c2`,
against `master` at `b6f195c`. PR #115.

- AC2 — PASS. `Title:` reads verbatim `Media File Preprocessing and Metadata for
  the 'tidyverse'`; `Description:` reads verbatim `Batch preprocessing and
  metadata extraction for audio, video and image files, built on the
  command-line programs 'FFmpeg' (<https://ffmpeg.org/>) and 'MediaInfo'
  (<https://mediaarea.net/en/MediaInfo>). Trim, crop, scale, convert and
  standardize files one at a time or across a whole directory, and read
  container and stream metadata back as tibbles for use with the 'tidyverse'.`
  Checked against those bytes by script: every Title word capitalized except the
  minor words `and`, `for`, `the` and the quoted package name `'tidyverse'`,
  which keeps its own lowercase spelling; `'tidyverse'` single-quoted in the
  Title; `'FFmpeg'`, `'MediaInfo'` and `'tidyverse'` each single-quoted in the
  Description; substring `The goal of` absent; Description does not start with
  `tidymedia`; sentence count 2.
- AC3 — PASS. `R CMD build` produced `tidymedia_0.1.0.9000.tar.gz`, 254 paths.
  `tar -tzf` grepped for the three patterns: `extdata/ffmpeg_location.rds` 0
  hits, `extdata/mediainfo_location.rds` 0 hits, `testthat/_problems/` 0 hits.
  The surviving `inst/extdata/` is the three files that should ship
  (`mediainfo_template_brief.txt`, `mediainfo_template_extended.txt`,
  `sample.mp4`).
- AC4 — PASS. `urlchecker::url_check()` over 24 URLs: `All URLs are correct!`,
  no row reported and nothing to change.
- AC5 — PASS. `Rscript tools/pkgdown_duplicate_topics.R` exits 0: 80 contents
  entries against 81 `man/` topics, `entries matching no topic: none`,
  `repeated topics: none`. `pkgdown::check_pkgdown()`: `No problems found.`
  (The one `man/` topic outside the index is `tidyeval.Rd`, a boilerplate
  doc-only topic with no export; `check_pkgdown()` is silent on it.)
- AC1 — PASS. `R CMD check --as-cran` over `tidymedia_0.1.0.9000.tar.gz` with
  `_R_CHECK_CRAN_INCOMING_=TRUE` and `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`:
  `Status: 1 NOTE`. The complete NOTE list is one NOTE, `checking CRAN incoming
  feasibility`, whose whole body is `Maintainer: 'Jeffrey Girard
  <me@jmgirard.com>'` plus `Version contains large components (0.1.0.9000)` —
  the dev-version suffix, whose bump this milestone puts Out. No NOTE names the
  `Title` field or the `Description` field. Tests ran inside the check (546s,
  OK); vignettes, PDF and HTML manual all OK.
- AC6 — PASS. `devtools::document()` produces no diff (`git status` clean but
  for this milestone file). `devtools::test()`: `FAIL 0 | WARN 10 | SKIP 18 |
  PASS 12614`. `devtools::check()`: `Status: OK`, `0 errors | 0 warnings | 0
  notes`, 17m 30.9s. No NOTE to quote or justify.

### Independent fresh-context review

Surface tier is user-facing, so the full three-lens fan-out ran, each lens on
its own evidence base, none having seen the implementation.

- **[S] blame-history lens — 0 findings.** Traced every deleted and modified
  line. The two `*_location.rds` files were added by `d97fdab9` and made dead by
  M097, which moved the remembered location to `tools::R_user_dir()`; the
  `_pkgdown.yml` alias rows went stale at the same `d97fdab9` rename and this
  branch is the first to catch them up. Nothing undone that a past milestone
  added deliberately.
- **[S] prior-PR-comments lens — 0 findings.** The existence probe
  (`pulls/comments?per_page=1`) returned `[]`, so the per-PR walk was skipped;
  the archived `## Review` sections touching these files (M055, M074, M098,
  M099, M110) record nothing this diff regresses. It checked the M055 NEWS-splice
  failure mode (the new bullets do not eat a neighbour) and the M089 README
  `temp_libpath`-noise lesson (the noise rides alongside real content changes,
  so the lesson's revert case does not apply).
- **[O] diff-bug lens — 10 findings**, ranked, verified individually below.

Findings and dispositions (rank order as reported):

1. `.Rbuildignore` guards `tests/testthat/_problems`, but the file testthat
   actually writes on a failing run, `tests/testthat/testthat-problems.rds`
   (`.gitignore:24`), matches no `.Rbuildignore` pattern, so the leak class this
   milestone closed is still open for it. **CONFIRMED** by hand:
   `.Rbuildignore` has 20 patterns and none matches that path. Disposition:
   put to the maintainer at the gate.
2. `tools/pkgdown_duplicate_topics.R` drops any `contents:` entry containing a
   parenthesis without reporting it, so a duplicate between a pkgdown selector's
   expansion and a literal entry would be invisible. **CONFIRMED as latent, not
   live** — `grep '(' _pkgdown.yml` matches one `desc:` prose line and no
   `contents:` entry, so AC5's instrument does parse every entry the file
   actually has today. Disposition: follow-up candidate row.
3. `_pkgdown.yml` now lists `find_program`, which `NAMESPACE` does not export
   (`set_program` is exported; `find_ffmpeg`/`find_ffprobe`/`find_ffplay`/
   `find_mediainfo` are). **REFUTED as a user-visible defect**: `man/
   find_program.Rd` carries all four exported names as aliases, and the work
   log's `build_reference_index()` run shows `find_mediainfo()` rendering in
   `docs/reference/index.html`. The entry string is a source-file detail with no
   rendered consequence, and the page-name choice was an implement-gate
   decision. Rejected.
4. Two factual slips in the new NEWS bullet: "three paths that were never part
   of it" (the two `*_location.rds` were the location cache until `d97fdab9`),
   and "a `tests/testthat/_problems/` directory that a failing test run leaves
   behind" (testthat writes `testthat-problems.rds` and `_snaps/`, not that
   directory). **CONFIRMED.** User-facing changelog prose, so the derived-claims
   rule applies. Disposition: put to the maintainer at the gate.
5. The `_problems/` deletion is not in the diff — the directory was gitignored
   and never tracked — so AC3's `tar -tzf` check for it would pass on a fresh
   clone regardless. **CONFIRMED as an observation about the criterion's
   strength**, not a defect in the work: the branch's durable guard for that
   path is the `.Rbuildignore` line, which is in the diff. Rejected, recorded.
6. The tarball-contents NEWS bullet sits under `## Requirements`, where "what
   the built package carries" is not a requirement. Style; rejected unless taken
   with finding 4's rewrite.
7. `tools/pkgdown_duplicate_topics.R` calls `yaml::read_yaml()` and nothing
   declares `yaml` — not `Suggests`, not the script header. **CONFIRMED**
   (`grep yaml DESCRIPTION` is empty). The script is `.Rbuildignore`d and
   developer-only, so a `Suggests` entry would be a gated dependency change;
   a header line naming the requirement is the proportionate fix. Disposition:
   put to the maintainer at the gate.
8. `^tools$` would silently drop genuine build-time helpers if `tools/` is ever
   used for them. Speculative future risk, not a defect in the diff. Rejected.
9. Scope names `probe_video` and T5 says to name `probe_container`'s aliases,
   but the Media metadata section is untouched. **REFUTED as a defect**:
   `man/probe_container.Rd` aliases `probe_video`, and pkgdown renders it — the
   scope's aim is met by the rendered index. The task text and the diff are out
   of step; the outcome is not. Rejected.
10. `README.md` embeds a machine-specific temp path in two chunk outputs.
    Pre-existing; the plan routes README's unguarded chunks to M114. Rejected.

### Re-verification after the gate's fix-now work

All six criteria re-executed at `e064e5a`, the tree that carries the three
fix-now edits. Every result is unchanged: `R CMD build` 254 paths with 0 hits
for each of the three patterns (and 0 for `testthat-problems`, the newly guarded
path); `R CMD check --as-cran` `Status: 1 NOTE`, the same maintainer line plus
`Version contains large components (0.1.0.9000)`, no NOTE naming `Title` or
`Description`; `devtools::test()` `FAIL 0 | WARN 10 | SKIP 18 | PASS 12614`;
`devtools::check()` `0 errors | 0 warnings | 0 notes` (17m 21.3s);
`devtools::document()` no diff; `tools/pkgdown_duplicate_topics.R` exit 0 with
80 entries, none repeated, none unmatched; `cairn_validate.py` all checks
passed. `urlchecker::url_check()` was not re-run — the fix-now edits touch no
URL, and the earlier run over 24 URLs stands.

### Consistency gate

`cairn_validate.py` exit 0, all 16 checks PASS and all 7 advisories OK — the
`release window` advisory did not fire. No `DESIGN.md` principle changed
(`Principles touched: —`), so `cairn_impact.py` does not apply.

Toolchain half, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` no diff; `NAMESPACE`/`man/` regenerate clean (the [O]
reviewer independently regenerated `man/` in an isolated copy — `diff -rq`
zero differences); `README.md` re-knitted from `README.Rmd` in the same commit
(`ae58d25`); `pkgdown::check_pkgdown()` `No problems found`; `NEWS.md` carries
two entries for this milestone's user-visible changes, neither naming a
milestone number; the two new top-level paths (`tools/`, and the
`tests/testthat/_problems` guard) have `.Rbuildignore` entries and the check
reports no NOTE about them; `devtools::check()` clean.
