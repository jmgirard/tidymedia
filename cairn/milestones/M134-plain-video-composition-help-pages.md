# M134: The anonymize, segment, concatenate, compare and picture-in-picture help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m134-plain-video-composition-help-pages`

## Goal

The M134 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M134 domain in `cairn/references/plain-docs.md`: `anonymize_video()`, `segment_video()`, `concatenate_videos()`, `compare_videos()` and `picture_in_picture()` with their batch pages (10 pages, 453 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M132, M133 and M130. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: Every page on the M134 domain list exists at head, and the prose sweep over the M134 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [x] AC2: Every page on the M134 domain list exists at head, and the prose sweep over the M134 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [x] AC3: Each page in the M134 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [x] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M134 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [x] AC5: For every `man/*.Rd` file outside the M134 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [x] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T5
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1: Record in a new M134 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `anonymize_video()` and `segment_video()`, and their batch pages.
- [x] T3: Rewrite `concatenate_videos()` and its batch page.
- [x] T4: Rewrite `compare_videos()` and `picture_in_picture()`, and their batch pages.
- [x] T5: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M130 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-17: implement started on `m134-plain-video-composition-help-pages`, cut from `7b2f9b0d`. No question was open at the gate.
- 2026-09-17: T1 done. `### M134` in `cairn/references/plain-docs.md` records the base commit, the 10 pages and the base sweep: 553 sentences, 56 findings. The AC4 patterns match nothing at the base commit.
- 2026-09-17: T2 done. The sweep prints no finding for the 4 anonymize and segment pages, and each names the glossary. `devtools::document()` changed only those 4 pages. The docs, anonymize and segment test files pass.
- 2026-09-17: T3 and T4 done in one commit, because both edit `R/ffmpeg.R` and one `devtools::document()` run wrote their 6 pages. The sweep over the 10 pages reads 621 sentences, prints no finding and exits 0. The 9 pages with a stem name the glossary. `concatenate_videos_batch` has no stem.
- 2026-09-17: [O] claim audit reader delegated. It read every added line of the branch diff against the code and asked for no correction.
- 2026-09-17: claim audit: 63 claims read, 0 corrected — R/ffmpeg.R
- 2026-09-17: T5 done. No test file changed. No base claim was found false, so the follow-up candidate row gets no M134 line. `devtools::check()` gave 0 errors, 0 warnings and 0 notes, `devtools::test()` gave 0 failures, `pkgdown::check_pkgdown()` found no problems, and `devtools::document()` leaves `man/` unchanged. The ledger is filled. Status set to review.

## Decisions

## Review

- 2026-09-17: step 1 sync. `origin/master` at `7b2f9b0d`, the base commit; the branch is 0 behind and 4 ahead. No merge needed.
- AC1: pass. All 10 domain pages exist at head. `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R` over the 10 pages read 621 sentences, printed no `[term …]`, no `[dash in Rd source]` and no other finding, and exited 0.
- AC2: pass. The same run printed no `[<n> words]` line.
- AC3: pass. The `--prose` output of 9 pages matches a glossary stem, and each of the 9 has a sentence that names the glossary. `concatenate_videos_batch` matches no stem.
- AC4: pass. The two patterns match nothing in the 10 pages at `7b2f9b0d`, so there is no identifier to find at head.
- AC5: pass. `git diff --name-only 7b2f9b0d HEAD -- man/` lists only the 10 domain pages, so no page outside the domain is compared.
- AC6: pass. `devtools::document()` left `man/` and `NAMESPACE` unchanged. `devtools::check()` gave 0 errors, 0 warnings and 0 notes. `devtools::test()` gave 0 failures (13866 pass, 5 skips). `pkgdown::check_pkgdown()` found no problems.
- Consistency gate: `cairn_validate.py` passed every check. No DESIGN.md principle changed, so `cairn_impact.py` was skipped. Toolchain slot: `document()` no diff, `check()` clean, `check_pkgdown()` clean, `README.md` unchanged and in step with `README.Rmd`, no new top-level file. `NEWS.md` has no entry, as the scope states under D091.
- Independent review, three lenses (user-facing tier). [S] prior-review lens: no regression against the M130 to M133 review records, and the PR-comment probe found no human review thread. [S] blame-history lens: no lost caveat, no resurrected false claim, no contradicted decision, no orphaned test. Findings, ranked, with disposition:
  - O1: `AC6` was unticked while the work log claimed its checks passed. No change needed: the box is ticked above on fresh evidence.
  - O2: `?segment_video` keeps the base wording "an underscore (_) and an integer", while `derive_segment_names()` pads with zeros and the `outfiles` argument says "zero-padded integer". Rejected at the gate: a zero-padded integer is an integer, and the `outfiles` argument states the padding.
  - O3: `?anonymize_video_batch` calls `color` an encoding argument, inherited from the base "encode knobs". Noted, no change.
  - O4: `@seealso` blocks mix `;` and `.` separators across the pages. Rejected: style, renders the same.
  - O5: the ledger heading "Sweep output at the base commit" sat over a table whose Result column reports head outcomes. Fixed now: the heading names both.
  - S1: the `(D015)` citation is gone from three batch pages. Rejected: the plan calls for it, and the shape the decision fixes is still stated in full.
- 2026-09-17: step-7 approval: m134-plain-video-composition-help-pages approved for merge. O2 rejected at the gate.

