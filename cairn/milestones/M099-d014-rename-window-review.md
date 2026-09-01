<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M099: D014's pre-0.2.0 rename window is reviewed before it closes

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR06 (advisory, no binding criteria)
- **Principles touched:** —
- **Branch/PR:** `m099-d014-rename-window-review`

## Goal

Decide, for each of the four candidate API changes D014's clean-break policy
holds open, whether it ships now or is declined permanently — while a rename is
still free of a deprecation cycle.

## Scope

Surface tier: **user-facing** — every candidate under review renames an argument
or adds one to exported verbs.

**In:** the four candidate API changes D014's pre-0.2.0 window covers —
(a) renaming `audio_stream` / `audio`, or unifying their two `NULL` readings
(D023, D025, D026, D032);
(b) a per-verb `check_tracks =` argument (D047, M082);
(c) a per-call `timeout =` argument on the run-capable verbs (D044, D047, M071,
M072);
(d) **backend-neutral naming for the hardware-encoder surface**, so M100 can add
videotoolbox without a second vocabulary: the exported `has_nvenc()` and
`nvenc_encoder()`, and the documented `tidymedia.nvenc_encoders` option, each
name a single backend. Widening `hardware=`'s own vocabulary is additive and is
M100's, not a rename; these three names are the irreversible part.
`(RB tripwire: irreversible-api)` — an accepted change is irreversible on the
exported surface, taken under D014's clean break with no `lifecycle` shim.

**Out:**
- Superseding D014 with a deprecation policy — declined at this plan's question
  gate; the clean break stands. Not deferred.
- Any rename outside the four candidates above → stays a candidate row.
- Building the videotoolbox backend, backend detection, and the widened
  `hardware=` vocabulary → M100, which depends on this milestone's (d) call.
- The release itself → the standing `CRAN readiness` ROADMAP candidate row.

## Acceptance criteria

- [ ] AC1 Each of the four candidates leaves the surface in one of exactly two
      states, and a procedure says which — each procedure scoped to the names
      that candidate puts under test, never to a whole grep output or the whole
      89-name export vector, which other candidates' work moves for unrelated
      reasons. Candidates (a)-(c) rename or add a **formal**: the sweep walks
      `getNamespaceExports("tidymedia")` and tests `names(formals())` for a name
      `N`, enumerating the domain rather than recalling it (at HEAD it returns
      18 verbs for `audio_stream`, matching D032's count). Candidate (d) renames
      **exported objects** and one **option string**: the exported domain is
      enumerated by pattern, not recalled —
      `grep("nvenc|cuda|gpu|videotoolbox|qsv|vaapi|amf", getNamespaceExports("tidymedia"), ignore.case = TRUE)`,
      which at HEAD returns exactly `has_nvenc` and `nvenc_encoder` — and the
      option by `grep -rn "<pattern>" R/ man/ tests/ vignettes/ _pkgdown.yml`,
      deliberately excluding `NEWS.md`, whose four hits are historical release
      prose that must not be rewritten. **Shipped:** the NEW-name pattern
      returns the read site and the docs, and the OLD-name pattern returns
      nothing — two greps, since a pattern fixed to the old name can never
      return the new one. No `lifecycle` shim, per D014. **Unchanged:** each
      procedure returns for those names exactly what it returns at the branch
      point.
- [ ] AC2 Candidate (a) may also ship as a behavior change with no surface
      change — unifying the two `NULL` readings, which alters what
      `audio_stream = NULL` selects without touching any `formals()`. Where it
      ships in that form, a test asserts the unified reading at each verb the
      AC1 sweep returns for `audio_stream`, and D025 and D026 are superseded,
      since each states the split reading this would remove.
- [ ] AC3 For every candidate that ships in any form, its documentation matches
      the surface: `devtools::document()` produces no diff, `_pkgdown.yml` gains
      a row for any newly exported object AND loses the row for any name removed
      — a rename is both, and a stale row fails `pkgdown::check_pkgdown()`, and the vignettes and `README.Rmd`
      compile against the shipped names.
- [ ] AC4 `NEWS.md` names every change that shipped, in user-facing wording; a
      candidate that did not ship produces no entry.
- [ ] AC5 `devtools::test()` clean and `devtools::check()` reports 0 errors and
      0 warnings with every NOTE justified (PROFILE `verify` and
      `consistency-gate` slots).

## Tasks

1. [x] Read the four candidates and every D-entry they cite; write up what each
   change would look like at the call site, and run the AC1 sweep at the branch
   point to record each candidate's starting verb set.
2. [x] Question gate: put the four dispositions to the user, one option set each,
   stating that the choice is permanent once 0.2.0 reaches CRAN, and that (d)
   decides the vocabulary M100 then builds against.
3. [x] Write the D-entry recording the four dispositions, each naming the verb set
   the AC1 sweep must return and the evidence class that would reopen it; where
   a candidate is declined, state that D014's window closes at 0.2.0.
4. For each shipped disposition: write its tests first, then make the change.
   Supersede D025 and D026 if candidate (a) ships as a `NULL`-reading change.
5. Update `man/`, `_pkgdown.yml`, vignettes, `README.Rmd`, and `NEWS.md` for
   what shipped.
6. [x] Update the ROADMAP rows: remove a row whose change shipped; on a declined
   row, replace the promotion condition that assumed the window was open.
7. Run the AC1 sweep, `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Coverage

- AC1 → T1, T2, T4, T7
- AC2 → T4
- AC3 → T5, T7
- AC4 → T5
- AC5 → T7

## Decisions
<!-- owner: implement/review; append-only -->

- 2026-09-01 M099-D1 (from RR06 Q1/Q2, promoted to D077): candidate (a) ships as `audio` → `audio_input` on `compare_videos`, `compare_videos_batch`, `picture_in_picture`, `picture_in_picture_batch`; `audio_stream`, `ffm_codec(audio =)`, `ffm_copy(audio =)` unchanged; the `NULL` readings stay. AC1's sweep for (a) therefore returns `audio_input` on exactly those four and `audio` on the two `ffm_*` builders; `audio_stream` stays at 18.
- 2026-09-01 M099-D2 (from RR06 Q3/Q4, promoted to D077): candidates (b) and (c) declined permanently; AC1's `check_tracks` and `timeout` sweeps return 0 at review as at the branch point. RR06's per-row shape (a `jobs` column carried on the pipeline object) is the recorded reopening route.
- 2026-09-01 M099-D3 (from RR06 Q5/Q6, promoted to D077): candidate (d) ships as `has_hardware_encoder()`, `hardware_encoder()`, `tidymedia.hardware_encoders`, both helpers exported; the AC1 hardware pattern gains `hardware` and returns exactly those two names, the option grep the new string only. RR06 R9-R10 (`tidymedia.available_encoders`, `has_hw_encoder`, `encoder_name`, `tidymedia.encoders`) rejected for the reasons D077 records.
- 2026-09-01 M099-D4 (from RR06 Q7, promoted to D078): the naming-and-seams principle is recorded once so the next seam or capability name is decided by rule.

## Work log
<!-- owner: implement/review -->

- 2026-08-31 plan: criteria audit ran in FULL mode (surface tier user-facing), fresh-context [O] reader. It returned five findings, all disposed here: AC1 and AC4 of the draft bound recording acts (a D-entry's existence, a ROADMAP row's wording) rather than deliverable properties under D-118/D-120 — the record is now a task and the criteria bind the exported surface; the draft's vacuity clause let a user-facing milestone complete with no surface change at all, and is gone; the draft's absence check was unsatisfiable for candidate (a), since `grep -rn "audio" R/ man/ vignettes/ README.Rmd` returns 2,193 hits, and ill-typed for (b) and (c), which add an argument and have no old name — replaced by the `formals()` sweep over `getNamespaceExports()`, which returns 18 verbs for `audio_stream` at HEAD and matches D032's count; and no branch fitted candidate (a)'s behavior-only `NULL`-reading form, now AC2.
- 2026-08-31 plan: alternative rejected — superseding D014 with a `lifecycle` deprecation policy, which would keep renames available after 0.2.0. Declined at the question gate in favor of reviewing the three candidates while the clean break is still free. Falsified by a rename becoming necessary after 0.2.0 ships, which is exactly the cost this milestone exists to price.
- 2026-08-31 plan: alternative rejected — leaving the three rows as candidates and letting the release close D014's window unexamined. Lost at the question gate; the maintainer chose to review them before any release.
- 2026-08-31 plan amendment: candidate (d), backend-neutral naming for the hardware-encoder surface, added at the user's request after the status audit surfaced that `hardware=` has only ever accepted `"none"` and `"nvenc"` — measured at 16 exported verb signatures, ~118 nvenc-named internal call sites and 8 dedicated test files, with `has_nvenc()`/`nvenc_encoder()` exported and `tidymedia.nvenc_encoders` documented in three help topics. Alternative rejected: folding the videotoolbox implementation into this milestone, which would put it well past the >7 criteria / >10 task advisories and mix a decide milestone with a build one; it became M100, depending on this milestone's (d) call. Falsified by (d) proving unanswerable without first building a second backend, which would invert the dependency.
- 2026-08-31 plan: criteria audit re-ran in FULL mode over the (d) amendment and M100 together, fresh-context [O] reader. On this file it returned four findings, all fixed here: AC1's shipped-state test for (d) named a grep pattern fixed to the OLD name, which by construction can never return the new one (now two greps — the same defect the log above records fixing for candidate (a), reintroduced in the amendment); the unchanged-state test compared whole grep output and the whole 89-name export vector, which candidate (c)'s own work moves via `helper-timeout-sweep.R` (now scoped to the names under test); (d)'s domain was a two-name hand-list where (a)-(c) enumerate (now a pattern grep over the export list, returning exactly `has_nvenc` and `nvenc_encoder` at HEAD); and AC3's pkgdown clause was addition-only, though a rename is a removal too and a stale row fails `check_pkgdown()`. Coverage AC1 no longer routes to T3, the record-writing task, which cannot satisfy a surface-bound criterion.
- 2026-08-31 plan: (d)'s gate inherits one consequence from M100 — declining (d) leaves the exported availability helper shipping as `has_nvenc(codec, backend = )`, since M100 gives it a backend argument either way. Recorded here so the disposition is taken with that surface in view.
- 2026-09-01 implement: branch `m099-d014-rename-window-review` cut from master at a654f9b; status in-progress. T1: AC1 sweep at the branch point over 89 exports — `audio_stream` formal on 18 verbs, `audio` on 6 (`compare_videos`, `picture_in_picture`, their `_batch` siblings, `ffm_codec`, `ffm_copy`), `check_tracks` and `timeout` on 0; the hardware pattern grep returns exactly `has_nvenc`, `nvenc_encoder`; `tidymedia.nvenc_encoders` appears in 4 files under `R/`, 3 under `man/`, 25 under `tests/`, none in `vignettes/` or `_pkgdown.yml`. Sweep script kept out of the repo (scratchpad); its output is reproducible from the AC1 procedure.
- 2026-09-01 implement: T2 question gate posed with all four dispositions; the user asked whether any had been briefed (none — RB01-RB05 cover other questions, the four were settled at in-session gates) and chose **Escalate via `/milestone-brief`** on every candidate (a)-(d). No disposition recorded; the D-entry (T3) waits on the RR. Handing off to `/milestone-brief`.
- 2026-09-01 brief: blocked on RB06 (`cairn/reviews/RB06-d014-rename-window.md`), one brief covering candidates (a)-(d); advisory, no binding criteria requested. Second-escalation removal options listed for (a) and (d), since RB02/RB03 named `audio_stream` and RB01 named the nvenc helpers. Committed on the milestone branch rather than master, since the milestone was already in-progress on its branch.
- 2026-09-01 ingest RR06: all seven questions answered; the maintainer took every disposition at the ingest gate — ship (a) as `audio_input` (overturning the session's decline; D032's docs-not-API half superseded, quoted to the user first), ship (d) with both helpers exported, decline (a)-`NULL`, (b), (c) permanently, spell M100's helper argument `hardware =`, record the Q7 principle. Written as D077 and D078 and M099-D1..D4; T3 ticked. Recommendations triaged: R1-R8 apply, R9-R10 reject-with-reason (D077), R11 (a generic `has_encoder(name)`) to the M100-out candidate row as additive later work. RR06 Beyond-the-brief items 2-6 (rename mechanics, `codec_family()`/`check_nvenc_available()` abort text, `?tidymedia` Session options sentence, `_pkgdown.yml` section prose, `refresh_ffmpeg_capabilities()` `@seealso`) fold into T4/T5. RB06/RR06 archived; status in-progress.
- 2026-09-01 implement (checkpoint, suite result pending): T2 ticked — every disposition settled at the RR06 ingest gate. T4/T5 edits landed: (a) `audio` → `audio_input` on the four fan-in verbs, their pipelines, `check_audio_codec_needs_audio()`'s abort text, `check_batch_audio_col()`'s default column, the `?audio_stream` topic (its "names three things" section now says the bare name is not an index), 14 test files and `vignettes/tidymedia.Rmd`; spelling `audio =` on those verbs now fails as an R partial-match error, since `audio` prefixes both `audio_input` and `audio_codec` (observed, not composed). (d) `has_nvenc`/`nvenc_encoder`/`tidymedia.nvenc_encoders` → `has_hardware_encoder`/`hardware_encoder`/`tidymedia.hardware_encoders` by word-boundary replacement over `R/`, `tests/`, `vignettes/`, `_pkgdown.yml`, `data-raw/` and `DESIGN.md`; helper topic retitled, `_pkgdown.yml` section prose and `?tidymedia` option sentence reworded (RR06 items 2, 5, 6); `man/nvenc_encoder.Rd` removed, `document()` idempotent, `check_pkgdown()` clean. Internal `nvenc_available()`/`check_nvenc_available()` and their abort text left for M100 (RR06 item 4). New `test-d014-rename-window.R` runs the AC1 sweep; it failed six ways before the rename. T6 ticked: ROADMAP rows for (a)-`audio_stream`/`NULL`, (b), (c) now carry D077's per-row reopening shapes; 23,775 bytes. Two NEWS entries under Breaking changes. M100's plan text still spells the old names; it depends on this milestone and is amended at its own gate.
