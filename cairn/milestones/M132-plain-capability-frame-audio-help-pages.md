# M132: The FFmpeg-capability, frame and audio conversion help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m132-plain-capability-frame-audio-help-pages`

## Goal

The M132 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M132 domain in `cairn/references/plain-docs.md`: `ffmpeg()`, `ffmpeg_codecs()`, `ffmpeg_encoders()`, `hardware_encoder()`, and `extract_frame()`, `sample_frames()`, `extract_audio()` and `convert_audio()` with their batch pages (12 pages, 345 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M133, M130 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: Every page on the M132 domain list exists at head, and the prose sweep over the M132 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [x] AC2: Every page on the M132 domain list exists at head, and the prose sweep over the M132 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [x] AC3: Each page in the M132 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [x] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M132 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [x] AC5: For every `man/*.Rd` file outside the M132 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T2, T3, T4, T5
- AC3 → T2, T3, T4, T5
- AC4 → T1, T5
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1: Record in a new M132 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `ffmpeg()`, `ffmpeg_codecs()`, `ffmpeg_encoders()` and `hardware_encoder()`.
- [x] T3: Rewrite `extract_frame()` and `sample_frames()`, and their batch pages.
- [x] T4: Rewrite `extract_audio()` and `convert_audio()`, and their batch pages.
- [x] T5: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M129 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-13: plan gate chose splitting M129's 16 pages into this milestone and M133 over one 16-page milestone, because the 16 pages hold about 650 sentences; falsified by M129 shrinking these pages so far that one milestone would have been about 12 pages of work.
- 2026-09-17: implement started on `m132-plain-capability-frame-audio-help-pages`, cut from `efb2b2c4`. No question gate: the plan left nothing open.
- 2026-09-17: T1 done. Ledger section `### M132` records base `efb2b2c4`, 12 pages, 369 sentences, 45 findings (23 words, 22 term, 0 dash) and 3 identifiers.
- 2026-09-17: T2 done. `?ffmpeg`, `?ffmpeg_codecs`, `?ffmpeg_encoders` and `?hardware_encoder` have no sweep finding, and the three with a stem name the glossary. The seven docs test files pass.
- 2026-09-17: T3 done. `?extract_frame`, `?sample_frames` and their batch pages have no sweep finding. The three with a stem name the glossary.
- 2026-09-17: T4 done. `?extract_audio`, `?convert_audio` and their batch pages have no sweep finding and name the glossary. The sweep over the 12 pages exits 0. The inherited `run`, `parallel` and `...` text was left as it was, because pages outside the domain inherit it.
- 2026-09-17: [O] delegation: fresh claim audit reader over the branch diff outside `cairn/`. I checked its 3 action items against the diff and applied them.
- 2026-09-17: claim audit: 118 claims read, 3 corrected — R/ffmpeg.R, man/ffmpeg.Rd, man/hardware_encoder.Rd
- 2026-09-17: T5 done. No test pins changed wording. The sweep over 12 pages reads 480 sentences and exits 0. `devtools::test()` gave 0 failures and 5 nvenc skips, run before the 3 audit wording fixes. `devtools::check()` after them gave 0 errors, 0 warnings and 0 notes, and left `man/` unchanged. `pkgdown::check_pkgdown()` found no problems. One doubtful base claim on `?hardware_encoder` went to the ledger and the `ffm_*()` help-text follow-up row.
- 2026-09-17: the simple-english lint hook reports hits in plan-owned text and old log lines of the tracking files. Implement left them, because it does not own that text.

## Decisions

## Review

Reviewed 2026-09-17 at `29bd0880`. The branch was cut from `efb2b2c4`, which is still the head of `master`, so no merge was needed. The domain list was rebuilt by command from the base commit: 12 pages, none added since, all 12 present at head.

- AC1: the sweep over the 12 pages at head exits 0 and prints no `[term …]` and no `[dash in Rd source]` line.
- AC2: the same sweep prints no `[<n> words]` line. The `--prose` output has 480 sentences.
- AC3: 10 pages have a glossary stem in their `--prose` output, and each has a sentence with `glossary` and `vignette("tidymedia")` in its `tools::Rd2txt()` text. `ffmpeg` and `extract_frame` have no stem and do not name the glossary.
- AC4: the base pages give 3 identifiers, `tidymedia.check_tracks`, `tidymedia.hardware_encoders` and `tidymedia_dropped_audio`. `git grep -wF` finds each one in `man/*.Rd` at head.
- AC5: `git diff --name-only efb2b2c4 HEAD -- man/` lists 12 files, all in the domain. No page outside the domain changed, so there is nothing to compare.
