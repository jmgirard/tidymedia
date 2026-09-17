# M133: The separate-audio-video and normalize-audio help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m133-plain-separate-normalize-help-pages`

## Goal

The M133 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M133 domain in `cairn/references/plain-docs.md`: `separate_audio_video()` and `normalize_audio()` with their batch pages (4 pages, 305 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). The failure sections say what a user sees and can do, and the rest moves to code comments. Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M132, M130 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are, apart from text moved into them. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: Every page on the M133 domain list exists at head, and the prose sweep over the M133 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [x] AC2: Every page on the M133 domain list exists at head, and the prose sweep over the M133 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [x] AC3: Each page in the M133 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [x] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M133 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [x] AC5: For every `man/*.Rd` file outside the M133 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [x] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T4
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: Record in a new M133 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `separate_audio_video()` and its batch page.
- [x] T3: Rewrite `normalize_audio()` and its batch page.
- [x] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M129 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template. The split from M132 is logged in M132's work log.
- 2026-09-17: implement started on `m133-plain-separate-normalize-help-pages`, cut from `4b932266`. No question gate: the plan left no choice open.
- 2026-09-17: T1 done. The `### M133` ledger section records the base commit, the 4 pages, the 10 AC4 identifiers and the base sweep (324 sentences, 63 `[<n> words]`, 33 `[term …]`, 7 dash lines).
- 2026-09-17: T2 done. `?separate_audio_video` and its batch page sweep clean (149 and 132 sentences). Both name the glossary. The four-condition sentences became lists. The phrase `not \emph{how} FFmpeg exited` stays on the batch page because `test-ffmpeg-exit-condition.R` pins it.
- 2026-09-17: T3 done. `?normalize_audio` and its batch page sweep clean (106 and 137 sentences). Both name the glossary. Three phrases that `test-ffmpeg-exit-condition.R` pins stay, each on one source line.
- 2026-09-17: [O] delegation: fresh-context claim audit reader, read-only. I checked its 4 findings against the base text and applied all 4.
- claim audit: 116 claims read, 4 corrected — R/ffmpeg.R, man/normalize_audio.Rd, man/normalize_audio_batch.Rd, man/separate_audio_video.Rd, man/separate_audio_video_batch.Rd
- 2026-09-17: T4 done. No test file changed. Sweep at head: 526 sentences, no finding, exit 0. `check()` 0/0/0, `test()` 0 failures, `check_pkgdown()` no problems, `document()` no diff. One doubtful base claim on `?normalize_audio_batch` went to the ledger and the follow-up candidate row. Status set to review.
- 2026-09-17: review ran. 6 of 6 criteria pass. 10 findings, 5 fixed on the branch, 1 noted and 4 rejected.
- 2026-09-17: step-7 approval: m133-plain-separate-normalize-help-pages approved for merge

## Decisions

## Review

Evidence is from 2026-09-17 at `5f033063`. The branch is 0 commits behind `origin/master`. No PR exists yet.

- AC1: the domain grep with the M133 name filter lists the 4 pages, and no `man/*.Rd` file is new since `4b932266`. The sweep over the 4 pages exits 0 and prints no `[term …]` or `[dash in Rd source]` line.
- AC2: the same run prints no `[<n> words]` line. It reads 526 sentences (137, 107, 133 and 149).
- AC3: the `--prose` output of each of the 4 pages matches a glossary stem, and each page has one sentence that names the glossary.
- AC4: the pattern finds 10 identifiers in the 4 pages at `4b932266`. `git grep -wF` finds each one in 1 to 7 `man/*.Rd` files at head.
- AC5: `git diff --name-only 4b932266 HEAD -- man/` lists only the 4 domain pages, so no page is outside the domain.
- AC6: `devtools::document()` leaves the tree clean. `devtools::check()` gives 0 errors, 0 warnings and 0 notes. `devtools::test()` gives 0 failures (13866 passes, 5 skips, 12 warnings). `pkgdown::check_pkgdown()` finds no problems.
- Consistency gate: `cairn_validate.py` exits 0 with all checks passed. No principle changed, so `cairn_impact.py` does not run. `README.Rmd` did not change. `NEWS.md` gets no entry (D091). No new top-level file.

### Findings

Three fresh reviewers read the diff. The prior-review reviewer found nothing to report. The user chose the dispositions at the approval gate on 2026-09-17.

- O1 (diff): `R/ffmpeg.R:6283`. The `@return` of `separate_audio_video_batch()` now gives `verified` and the manifest without the `run = TRUE` condition that the base text had. `R/ffm_batch.R:145` shows that only a run produces them. Checked against the code and confirmed. Disposition: Fixed now: "A run also gives `verified` and the provenance manifest".
- O2 (diff): `R/ffmpeg.R:6327`. "Each bullet" now follows a new list of four conditions, so it is not clear that it means the bullets of the warning. Disposition: Fixed now: "Each bullet of the warning".
- O3 (diff): the work log page counts (149, 132, 106, 137) sum to 524, and the ledger says 526. The fresh sweep reads 526, so the ledger is right and two work log counts are one low. Disposition: Noted here. The ledger count of 526 stands, and the work log stays as written.
- O4 (diff): `R/ffmpeg.R:4928`. "carries the same row numbers" became "carries the row numbers", so the text no longer ties `tm_rows` to the rows just named. Disposition: Fixed now: "the same row numbers".
- O5 (diff): `R/ffmpeg.R:6242`. The "because" moved. The base gave the extension as the reason the function derives no output paths. The head gives the source codec as the reason for the extension. Disposition: Fixed now: the "because" is back on the reason for no output paths.
- O6 (diff): the ledger row for `separate_audio_video_batch` does not record that "scalar verb" also became "the one-file function it wraps" in See Also. Disposition: Fixed now in the ledger row.
- S1 (history): "scalar" became "one-file" on these pages only, and other pages still say "scalar verb". Disposition: Rejected: the plan called for it, because "scalar" is a maintainer term under rule 5. The other pages belong to M134.
- S2 (history): "aborts without probing" became "without asking FFmpeg" on two of four copies of that sentence. Disposition: Rejected: the plan called for it, and the other two copies are outside the M133 domain.
- S3 (history): the bold "best-effort" became "not guaranteed" without bold. Disposition: Rejected: the plan called for it, because "best-effort" is on the term list and the claim stays.
- S4 (history): the glossary sentences are new text. The reader confirmed they are accurate. Disposition: Rejected: AC3 requires these sentences.

