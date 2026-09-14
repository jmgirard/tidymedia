# M130: The crop, web-format, standardize and strip-metadata help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m130-plain-video-edit-help-pages`

## Goal

The M130 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M130 domain in `cairn/references/plain-docs.md`: `crop_video()`, `format_for_web()`, `standardize_video()` and `strip_metadata()` with their batch pages (8 pages, 322 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M132, M133 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: Every page on the M130 domain list exists at head, and the prose sweep over the M130 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [x] AC2: Every page on the M130 domain list exists at head, and the prose sweep over the M130 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [x] AC3: Each page in the M130 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [x] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M130 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [x] AC5: For every `man/*.Rd` file outside the M130 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [x] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T4
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: Record in a new M130 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `crop_video()` and `format_for_web()`, and their batch pages.
- [x] T3: Rewrite `standardize_video()` and `strip_metadata()`, and their batch pages.
- [x] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.
- 2026-09-13: re-cut by /milestone-plan under D093: M130 keeps 8 of its 18 own pages, and the anonymize, segment, concatenate, compare and picture-in-picture pages moved to M134. The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-14: implement started on branch `m130-plain-video-edit-help-pages`, cut from `9353ac4f`. No open choices, so no question gate.
- 2026-09-14: T1 done. `### M130` ledger in `cairn/references/plain-docs.md`: base sweep over 8 pages read 380 sentences, 48 findings (31 words, 15 term, 2 dash), no AC4 identifiers, no page names the glossary.
- 2026-09-14: T2 done. `crop_video`, `format_for_web` and their batch pages rewritten, each names the glossary, and the sweep over the 4 pages exits 0. A first draft reworded `crop_video_batch`'s `video_codec`, which is M129 shared text inherited by two M134 pages, so it was put back. `devtools::test()`: 0 failed, 0 errors, 5 skipped.
- 2026-09-14: T3 done. `standardize_video`, `strip_metadata` and their batch pages rewritten, each names the glossary, and the sweep over the 4 pages exits 0. The `?standardize_video` resolution rules are now a three-item list. No page outside the domain changed. `devtools::test()`: 0 failed, 0 errors, 5 skipped.
- 2026-09-14: [O] claim audit reader (fresh, wrote none of the lines) read every added roxygen line against the code and the base text. It found no branch-added false claim. It found 3 changed sentences that did not make the base claim, which were put back, and the same reader confirmed all 3. It found 4 base claims false, which are ledger rows and the M130 follow-up candidate row in `cairn/ROADMAP.md`.
- claim audit: 70 claims read, 3 corrected — R/ffmpeg.R, man/crop_video_batch.Rd, man/standardize_video_batch.Rd, man/strip_metadata.Rd, man/strip_metadata_batch.Rd
- 2026-09-14: T4 done. No test pinned changed wording, so no test changed. Sweep over the 8 pages exits 0 with no finding. All 8 name the glossary. No AC4 identifiers at base. `git diff --name-only 9353ac4f -- man/` lists only the 8 domain pages. `devtools::document()` leaves `man/` unchanged. `devtools::check()`: 0 errors, 0 warnings, 0 notes. `devtools::test()`: 0 failed, 0 errors, 5 skipped. `pkgdown::check_pkgdown()`: no problems. Ledger filled.

## Decisions

## Review

- AC1 (2026-09-14): the base domain list (8 pages, filter from the ledger over `git show 9353ac4f`) all exist at head, and `git diff --diff-filter=A 9353ac4f HEAD -- man/` adds none. `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R` over the 8 pages read 467 sentences, printed no finding and exited 0, so no `[term …]` or `[dash in Rd source]` line.
- AC2 (2026-09-14): the same run printed no `[<n> words]` line and exited 0.
- AC3 (2026-09-14): every page's `--prose` output matches a stem (`strip_metadata_batch` has only `stream`). A `tools::Rd2txt()` read finds, on all 8 pages, a sentence with `glossary` and `vignette("tidymedia")`. The same read on base `crop_video` gives FALSE, so the check can fail.
- AC4 (2026-09-14): a regex scan of the 8 pages at `9353ac4f` finds 0 matches, so there is nothing to find at head.
- AC5 (2026-09-14): `git diff --name-only 9353ac4f HEAD -- man/` lists only the 8 domain pages, so no page outside the domain is in scope.
- AC6 (2026-09-14): `devtools::document()` left `git status` empty. `devtools::test()`: FAIL 0, WARN 12, SKIP 5, PASS 13866. `devtools::check()`: 0 errors, 0 warnings, 0 notes. `pkgdown::check_pkgdown()`: no problems found.
- Consistency gate (2026-09-14): `cairn_validate` exit 0, all checks passed. No principle changed, so `cairn_impact` skipped. README.Rmd, README.md, NEWS.md and `_pkgdown.yml` are unchanged against `9353ac4f`, and scope puts no NEWS entry (D091). No new top-level file. Branch is level with `origin/master` at `9353ac4f`.
- Independent review (2026-09-14): user-facing tier, so three lenses. [S] prior-review: no prior-review evidence, 0 findings. [S] blame-history: 2 findings (S1, S2). [O] diff-bug: no branch-added false claim, 6 branch findings (O1-O6) and 5 pre-existing false claims (P1-P5). No finding shows an acceptance criterion failing, so no return. Dispositions are recorded at the step-7 gate.
- Triage (2026-09-14, user chose "Apply as recommended"). Fix now, in `4dfab631`: O1 `?strip_metadata` Details made re-encoding read as the only way to remove per-stream data; O2 `?crop_video_batch` hardware text had a dangling "one"; O3 `?strip_metadata` Description narrowed "GPS/location" to "GPS location" and called it "the task function for IRB de-identification"; O4 `?strip_metadata` See Also "it" pointed at the wrong sentence; O5 `?format_for_web_batch` dropped "fixes both codecs by identity"; S2 two batch See Also lines coined "single-file function" beside the existing "single-input form".
- Rejected: O6 odd roxygen source line breaks (renders correctly; the S2 edit rewrapped them anyway). S1 restore "front door" on `?crop_video_batch` (a maintainer term under the plain-docs rules; the claim is kept).
- Follow-up: P1-P5 pre-existing false claims (strip_metadata per-stream tags survive; FLAC copy into .mp4 fails; every part of the standard is an argument; sibling batch functions read codec columns; ffm_crop() is the one wrapped pipeline function) became items (e)-(i) of the M130 follow-up row.
- Fresh reader of the fix-now edits (2026-09-14): every changed sentence makes the base claim, is true against the code and has at most 19 words. It found one more pre-existing false claim, `check_dim()` accepts any string and refuses an integer, which became item (j) (P6). It also restated ledger row (c), already on the row. Its clarity points, "for example for an IRB" and "fixes which codecs" beside encoder-sense "codec", and three roxygen lines over 80 characters, were rejected: the claims are correct, and the help renders correctly.
- After the fixes (2026-09-14): sweep over the 8 pages exits 0 with no finding, all 8 name the glossary, `devtools::document()` leaves no diff, `devtools::test()` FAIL 0 / SKIP 5 / PASS 13866, `pkgdown::check_pkgdown()` no problems. `devtools::check()` was not rerun, since the fix changed only roxygen text inside existing blocks.
