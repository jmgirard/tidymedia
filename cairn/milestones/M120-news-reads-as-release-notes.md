# M120: NEWS.md reads as release notes

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — NEWS.md ships and is the first thing a reader checks
- **Branch/PR:** `m120-news-release-notes` — https://github.com/jmgirard/tidymedia/pull/124

## Goal

The development section of `NEWS.md` reads as one set of release notes rather than
an append-only per-milestone log.

## Scope

**In:** collapsing the 1,968-line development section into one section with each
heading appearing once, covering every export added or removed and every rename
since the 0.1.0 heading.

**Out:** naming a release version number, cutting a release section, or bumping
`DESCRIPTION` → the release walk's act (`/cairn-release`), which this milestone
leaves untouched. Rewriting the four already-released sections → not needed; each
already has no repeated heading.

## Acceptance criteria

- [x] AC1: `NEWS.md` opens with a single development-version `#` heading, and the
      development section names no release version number.
- [x] AC2: A sweep over every `#` section of `NEWS.md` finds no `##` heading repeated
      within any one section. Today the development section carries `## New features`
      at lines 3, 267 and 1402, `## Breaking changes` at 48, 523 and 1943, `## Bug
      fixes` at 868, 1845 and 1961, and `## Documentation` at 20 and 1666.
- [x] AC3: The development section names every export in the symmetric difference
      between `NAMESPACE` at head and `NAMESPACE` at commit `4b04fad9`, the commit
      that introduced the `# tidymedia 0.1.0` heading — 40 added and 14 removed as
      measured 2026-09-07.
- [x] AC4: Parse every top-level `name <- function(...)` definition in `R/` at commit
      `4b04fad9` and at the branch head, take the names `export()`ed in both
      `NAMESPACE`s, and enumerate the formal argument names present at `4b04fad9` and
      absent at head. For each, the content above the `# tidymedia 0.1.0` heading names
      that argument together with the argument that replaced it, or states it was
      removed with nothing in its place. Measured 2026-09-09 at branch head `1df6e23`:
      49 names are exported in both `NAMESPACE`s, 48 of them parse as such a definition
      at both (`.data` is a reexported rlang pronoun, not a function), and the
      enumeration returns `extract_audio()`'s `acodec` and `segment_video()`'s
      `ts_start` and `ts_stop`.
- [x] AC5: The `verify` slot of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T1, T3
- AC4 → T1, T3
- AC5 → T4

## Tasks

- [x] T1: Compute the symmetric difference of the two `NAMESPACE`s and read D014,
      D077 and D078 for the renames that are not `NAMESPACE` entries. The removed set
      measured 2026-09-07 is `:=`, `as_label`, `as_name`, `audio_as_mp3`,
      `convert_fractions`, `enquo`, `enquos`, `ffm`, `get_codecs`, `get_encoders`,
      `get_framerate`, `get_samplingrate`, `mediainfo_summary`, `pad_integers` —
      breaking changes for any 0.1.0 caller.
- [x] T2: Collapse the development section: one occurrence of each `##` heading,
      entries merged under it in reader order rather than milestone order.
- [x] T3: Check the collapsed section against T1's two lists and fill what is missing.
- [x] T4: Confirm the four released sections below are untouched; run the profile's
      `verify` slot.

- [x] T5: `NEWS.md:365-368` — drop `picture_in_picture_batch()` from the
      `inputs` list-column group; it takes fixed `main`/`overlay`/`output`
      columns (`R/ffmpeg.R:7510-7518`, D015).
- [x] T6: `NEWS.md:375-376` — the `output`-required sentence leaves
      `separate_audio_video_batch()` described as auto-deriving; it requires
      `audiofile` and `videofile` and derives neither (`R/ffmpeg.R:6610-6620`).
- [x] T7: `NEWS.md:30-35` — remove both "already" claims and the
      rename-the-columns instruction; neither `audio_codec`/`video_codec` nor
      `segment_video_batch`'s `start`/`end` columns existed at `4b04fad9`.
- [x] T8: `NEWS.md:370-373` — add `_anonymized` (`R/ffmpeg.R:2194,2434`) to the
      auto-naming list and give the restarting `<basename>_<n>` to
      `segment_video_batch()` as well as the frame-writing verbs
      (`R/ffmpeg.R:3911,4134`).
- [x] T9: `NEWS.md:1217` — two vignettes are new, not three; the reordered one
      is the third, not the fourth.
- [x] T10: `NEWS.md:517-523` — restore a qualifier separating the family-not-
      encodable refusal from the backend-unavailable one; the former is refused
      whatever `fallback` is set to (`R/ffmpeg.R:3273-3276`, comment `:3325-3340`).
- [ ] T11: `NEWS.md:96-99` — the second-track fixture is the standardize /
      anonymize / segment observation; `format_for_web()` was measured on a
      third-track file. Split the claim or drop the verb it does not cover.
- [ ] T12: restore the `two_pass = TRUE` ordering guarantee the collapse dropped
      — a bad loudness target refused before the analysis pass measures the
      input — or state in the work log why it should not be announced.
- [ ] T13: `NEWS.md:181-192` — the `audio` -> `audio_input` bullet announces a
      rename on four verbs that are all new exports since `4b04fad9`; D091's own
      rule drops it, announcing `audio_input` once as the new argument it is.
- [ ] T14: `cairn/ROADMAP.md:20` — remove the sentence claiming the unreleased
      `NEWS.md` section still explains `unset_program()` via `find_program()`;
      the rewrite left one mention (`NEWS.md:1278`), the one calling it internal.
- [ ] T15: `cairn/DECISIONS.md:4317` — D091's "until 0.2.0 reaches CRAN"
      overstates D014, which says "pre-0.2.0 and still soaking". Editable, not
      yet merged.
- [ ] T16: `NEWS.md:318` — the accepted value is `"center"`, not `centre`
      (`R/ffmpeg.R:7133,7529`); reword `Relatedly` (`NEWS.md:1008`) or add it to
      `inst/WORDLIST`. `devtools::check()` must return to 0 NOTEs, or the NOTE
      must be justifiable.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: AC1's "no release version number is named" had no stated domain and was unsatisfiable over the whole file, which holds four real release headings (repaired — scoped to the development section); AC3's one-way set difference named only the 40 additions while AC4's two decision entries recovered some but not all of the 14 removals, leaving `ffm`, `pad_integers`, `convert_fractions`, `mediainfo_summary` and the `get_framerate`/`get_samplingrate` renames covered by neither (repaired — AC3 made symmetric, AC4 given D078 and narrowed to non-`NAMESPACE` renames). AC2 and AC5 passed all six questions clean.
- 2026-09-09: T1. Symmetric difference of `NAMESPACE` at head against `4b04fad9` measured
  with `comm` over the sorted `export()` lines: 40 added, 14 removed, matching the plan's
  2026-09-07 counts and its removed list exactly. All 14 removed names are already named
  once each in the development section; of the 40 added, three are named nowhere --
  `concatenate_videos_batch`, `strip_metadata`, `strip_metadata_batch` -- so T3's fill is
  those three. Non-`NAMESPACE` renames read from D014 (`acodec`/`vcodec` ->
  `audio_codec`/`video_codec`, `ts_start`/`ts_stop` -> `start`/`end`, the full-word
  compounds `pixel_format`/`sample_rate`, and `infile`/`outfile`/`infiles`/`file`), D077
  (`audio` -> `audio_input` on the four fan-in verbs; `tidymedia.nvenc_encoders` ->
  `tidymedia.hardware_encoders`; helper argument `hardware =`) and D078 (states the rule
  behind D077's applications; adds no rename of its own). Every one is already named in
  the section, so AC4 needs no fill, only preservation through the collapse.
- 2026-09-09: question gate. Three choices, all taken at the recommendation. (1) The
  development section ends with six headings -- Breaking changes, New features, Bug fixes,
  Performance, Documentation, Requirements -- so the 13 one-off announcement headings and
  the Configuration, Standardized function and argument names, Verification & provenance,
  Multi-input verbs and Safe execution headings dissolve into them; 27 headings become 6.
  (2) Bullets describing one feature at successive stages of the cycle merge into one entry
  rather than being re-filed unchanged, so prose is rewritten and the section shrinks well
  below 1,982 lines. (3) The eight stale function names the section still announces --
  `normalize_audios`, `segment_videos`, `has_nvenc`, `anonymize_videos`, `extract_frames`,
  `nvenc_encoder`, `standardize_videos`, `write_mediainfo_template`, 19 mentions -- are
  corrected to the shipped names; the `find_program()` cross-reference split is left to the
  [high] candidate row cut to hold it.
- 2026-09-09: AC2's illustrative line numbers have drifted 13 lines since 2026-09-07 (M119
  added content above them): `## New features` is at 3, 280 and 1415 rather than 3, 267 and
  1402, and the other three sets moved likewise. The criterion's binding sentence -- the
  sweep finding no `##` heading repeated within any one `#` section -- is unaffected, and
  the counts it asserts (three, three, three and two occurrences) are correct as measured
  today, so the wording is left alone rather than amended.
- 2026-09-09: T2+T3 (minor amendment: run as one editing pass rather than two, since
  the collapse rewrites the very bullets the fill lands in; task order and wording
  otherwise unchanged). The development section went from 1,982 lines under 27 `##`
  headings to 1,362 under six -- Breaking changes, New features, Bug fixes,
  Performance, Documentation, Requirements. The 13 one-off announcement headings and
  the Configuration, Standardized function and argument names, Verification &
  provenance, Multi-input verbs and Safe execution headings all dissolved into those
  six. Same-feature bullets merged: the four loudnorm headings became one two-pass
  entry, the twelve "reported against the function you called" bullets became one
  entry with a checks list and an ordering paragraph, and the eleven scattered
  `audio_stream` bullets became one Breaking entry for the behavior change plus one
  New-features entry for the argument. Intra-cycle renames were dropped rather than
  reported as breaking changes: `has_nvenc`/`nvenc_encoder`, `normalize_audios` and
  the other four plural batch names, and `convert_audio()`'s `format` argument all
  arrived and were renamed inside this cycle, so a 0.1.0 reader never saw them; the
  `_batch` verbs and the hardware helpers are now announced as the new exports they
  are. T3's fill added `strip_metadata()`, `strip_metadata_batch()` and
  `concatenate_videos_batch()`, whose prose was derived from their roxygen at
  `R/ffmpeg.R:1630-1668`, `:4888-4926` and `:7215-7250`, not composed.
- 2026-09-09: the eight stale function names the gate dispositioned are gone (sweep
  over the section for `\bname\b` returns zero for each). The eighth,
  `write_mediainfo_template()`, turned out to name no function anywhere in the
  package: `NAMESPACE` exports it not and `R/` defines it not, and the real caller of
  `check_file_exists()` is `mediainfo_template()` (`R/mediainfo.R:214`). The release
  note now names that. `R/utils.R:165-172`'s comment carries the same dead name and
  is where the note had copied it from; comment-only and outside this milestone's
  scope, so it was folded into the existing [high] `find_program()` candidate row
  rather than fixed here or given a row of its own (ROADMAP is at 59 of 60 lines).
- 2026-09-09: two tests assert on `NEWS.md`'s own wording and both failed on the
  first `verify` run -- a real regression this milestone introduced, not a
  pre-existing failure. `test-check-tracks-docs.R:126` matches the literal
  `defaults to TRUE`, which the rewrite had marked up as `` `TRUE` ``;
  `test-front-door-ordering.R:435` matches an ordering sentence the merge had
  dropped, one the two `_batch` verbs' help pages also carry. Both restored, the
  ordering sentence back in the Bug-fixes ordering paragraph where the merge should
  have put it. This is the guard the criteria could not supply: AC1-AC4 measure
  headings, exports and renames, and neither would have caught either loss.
- 2026-09-09: T4. The four released sections below the development heading are
  byte-for-byte identical to `HEAD:NEWS.md` from `# tidymedia 0.1.0` down (diff of
  the two tails is empty), so nothing below the line moved. `verify` slot clean:
  `devtools::test()` at FAIL 0 | WARN 12 | SKIP 5 | PASS 13,266. The five skips are
  all hardware-encoder probes (`test-nvenc.R:435,446,458`,
  `test-video-codec.R:480,489`); none reads `NEWS.md`. No roxygen changed, so
  `document()` was not run.
- 2026-09-09: acceptance measured. AC1: one `# tidymedia (development version)`
  heading and no tidymedia release version number in the section -- the lone `0.2.0`
  it carried ("a version of tidymedia before 0.2.0") is reworded to "an earlier
  version", which is also what Scope Out reserves for the release walk; the version
  numbers that remain are dependency and FFmpeg versions (1.1.0, 1.2.0, 2.5.0,
  3.0.3, 4.1.0, 4.5.0, 6.1.1, 9.0.1). AC2: a sweep over every `#` section finds no
  `##` heading repeated within any one of them, the four released sections included.
  AC3: all 40 added and all 14 removed exports named, matched on word boundaries.
  AC4: `audio_codec`/`video_codec` (from `acodec`/`vcodec`), `start`/`end` (from
  `ts_start`/`ts_stop`), `audio_input` (from `audio`) and
  `tidymedia.hardware_encoders` (from `tidymedia.nvenc_encoders`, now at zero
  occurrences) each named as renames; `hardware =` named. AC5 as above.
- 2026-09-09: status -> review.
- 2026-09-09: review opened on PR #124; `origin/master` unmoved, no merge needed. The five
  acceptance boxes arrived pre-ticked with no Review evidence; unticked at entry and re-ticked
  one at a time under AC fencing. AC1, AC2, AC3 and AC5 pass with fresh evidence (AC3 against a
  discriminating control). AC4 fails: `tidymedia.nvenc_encoders` occurs zero times in `NEWS.md`,
  so the option rename D077 records is not named, and the 2026-09-09 work-log line claiming it
  named reads zero occurrences as a pass.
- 2026-09-09: amendment return: AC4 — "It names every rename recorded in `cairn/DECISIONS.md`
  D014, D077 and D078 since that commit which is not itself a `NAMESPACE` entry and whose old
  name was present at commit `4b04fad9` — the argument and option renames a 0.1.0 caller could
  have written." AC4 as written sweeps in renames of names introduced and renamed inside this
  development cycle, which the implement question gate had already dispositioned as dropped
  rather than announced; the option is that class (`git grep nvenc_encoders 4b04fad9 -- R/` is
  empty). Status -> in-progress for this amendment alone. First amendment return on AC4; zero
  defect returns on this milestone.
- 2026-09-09: amendment gate 1. The user chose narrowing over adding the option rename to
  the notes, fixing AC4 as: "...which is not itself a `NAMESPACE` entry and whose old name
  was present at commit `4b04fad9` -- the argument and option renames a 0.1.0 caller could
  have written."
- 2026-09-09: re-audit: AC4 (full) -- eight findings, the first three bearing on whether the
  criterion can be checked the same way twice: the domain is fixed by three hand-picked
  decision entries rather than by a procedure (D062 and D064 record public renames the list
  never looked at); the em-dash gloss promises an option rename the new clause admits zero
  of; "present at `4b04fad9`" does not say present where, and the readings disagree about
  `audio` -> `audio_input`. Also: the criterion never requires the old name to appear, cites
  no probe, D078 is a null member of the citation list, and `NEWS.md:31` announces `vcodec`,
  which `git grep vcodec 4b04fad9` shows never existed at 0.1.0.
- 2026-09-09: amendment gate 2. Two choices, both taken at the recommendation. (1) AC4's
  domain rebased onto a formals diff over the functions exported at both commits, built and
  run here: 49 exported at both, 48 parsed at both (`.data` is a reexported pronoun), and the
  removed-argument set is `extract_audio()`'s `acodec` and `segment_video()`'s `ts_start` and
  `ts_stop` -- the same three names, settled by command rather than by recall. (2) The
  `vcodec` mention corrected in `NEWS.md` rather than deferred. Rejected without asking: the
  reader's request that AC4 cite a discriminating control, which would bind a property of the
  verifying instrument rather than of the release notes -- review runs the control, as it did
  for AC3.
- 2026-09-09: re-audit: AC4 (full) -- eleven findings; the second on this criterion, so no
  further reader is spawned and the disposition went to the user. Fixed here, each with one
  clear answer: the prose claimed a completeness the diff lacks (narrowed to what the diff
  computes); "together with the argument that replaced it" was unsatisfiable for an argument
  removed with no successor (alternative added); the check was unbounded over the file, and
  `acodec` occurs at `NEWS.md:1452` inside the RELEASED 0.1.0 section, so a whole-file grep
  would have passed on released text alone (bounded to the content above the `# tidymedia
  0.1.0` heading); the parse rule lived only in a scratch script (stated inline); the figures
  were pinned to a moving `head` (pinned to `1df6e23`). Put to the user: the breaking-change
  tightening, declined as a widening after a return; the two record gaps, both accepted.
- 2026-09-09: no second `amendment return: AC4` line is written here -- `/milestone-review`
  already logged this return in that shape, and a second naming the same criterion would read
  as the second-occurrence stop. This milestone carries one amendment return and zero defect
  returns.
- 2026-09-09: `NEWS.md:30-35` corrected. It announced `acodec` and `vcodec` as renamed to
  `audio_codec`/`video_codec`; `git grep vcodec 4b04fad9` is empty across that whole tree, so
  no 0.1.0 caller could have written `vcodec` -- the class D091 now says to drop. The same
  sentence claimed "every codec argument in the package is spelled the same way", which a
  formals sweep over the current exports falsifies: `hardware_encoder(codec, hardware)` and
  `has_hardware_encoder(codec, hardware)` both take a bare `codec`. Both replaced by claims
  derived from the two trees.
- 2026-09-09: D091 written -- the changelog announces only what a caller of the last release
  could have written. It is the rule this milestone turned on, which until now existed only in
  the question-gate line above; the second reader flagged that a later reader could not tell
  it from an arbitrary cutoff.
- 2026-09-09: the four surfaces no criterion covers -- jobs-table columns, session options,
  condition-class names, argument reordering -- absorbed as (n) into the shipped-docs
  candidate row rather than added as a new row (search-first; `ROADMAP.md` is at 59 of its 60
  lines). All four checked by hand at review and none is wrong today. `ROADMAP.md` is now
  36,719 bytes against its 24,000 budget, worse by 845; `/cairn-triage` remains the remedy and
  is now overdue by five passes.
- 2026-09-09: all five acceptance boxes unticked. AC4's wording changed and `NEWS.md` changed
  under AC1-AC3, so the review evidence recorded for them is stale; re-review measures all
  five fresh.

- 2026-09-09: review returned the milestone. All five acceptance criteria pass with
  fresh evidence (AC1-AC4 each against a discriminating control) and the universal
  consistency checks are clean, so nothing failed on the criteria. What failed is the
  deliverable: `devtools::check()` returned 1 NOTE on two words this branch added to
  `NEWS.md`, one of them (`centre`, `NEWS.md:318`) a value the code does not accept;
  and the [O] lens found ten further branch-introduced defects in the newly written
  release-note prose -- two that send a reader to build a jobs table the verb rejects
  (`NEWS.md:365-368`, `:375-376`), two user-visible guarantees the collapse lost
  (`NEWS.md:517-523`, and the `two_pass` ordering sentence dropped with no
  replacement), three claims that read as measured and are not (`NEWS.md:30-35`,
  `:1217`, `:96-99`), the milestone's own new rule unapplied to `NEWS.md:181-192`, and
  two stale tracking claims (`ROADMAP.md:20`, `DECISIONS.md:4317`). Two pre-existing
  findings rejected as out of scope. Put to the user at the merge gate with a
  recommendation to return; the user chose to send it back. Logged as T5-T16; status
  -> in-progress. First defect return on this milestone (one amendment return, on its
  own track). Steps 8-10 not run; PR #124 stays a draft.
- 2026-09-10: question gate (return session). Two choices, both taken at the
  recommendation. (1) T12's dropped two-pass ordering guarantee is restored rather
  than argued away: it is observable by a 0.1.0 caller who spends an analysis pass on
  a call that should have been refused, it is true at HEAD
  (`check_loudnorm_targets()` at `R/ffmpeg.R:2638` precedes `run_loudnorm_analysis()`
  at `:2678`), and `origin/master:NEWS.md:1126-1127` supplies the wording, so the
  restore re-files prose rather than composing it. (2) T16's `Relatedly`
  (`NEWS.md:1008`) is reworded rather than added to `inst/WORDLIST`, keeping the
  wordlist for domain vocabulary; `centre` -> `center` is a wrong claim
  (`R/ffmpeg.R:7133,7529`) and not a gate question.
- 2026-09-10: minor amendment (cadence, no task or criterion wording changed). T5-T16
  are prose edits to `NEWS.md` and two `cairn/` files, and the profile's `verify` slot
  is the full suite (13,266 assertions). Running it per task would spend it twelve
  times over edits no task makes to `R/`. Per task the two tests that assert on
  `NEWS.md`'s own wording are run (`test-check-tracks-docs.R`,
  `test-front-door-ordering.R` -- the pair that caught the collapse's two dropped
  sentences on 2026-09-09); the full `verify` slot runs once at completion, where
  step 9 requires it regardless.
- 2026-09-10: T5. `picture_in_picture_batch()` is out of the fan-in `inputs`
  list-column group and has its own bullet naming the fixed `main`/`overlay` columns,
  derived from its roxygen at `R/ffmpeg.R:7505-7518` ("not a list-column; D015").
  `concatenate_videos_batch()` and `compare_videos_batch()` were each checked against
  their own `@param jobs` (`R/ffmpeg.R:7226-7229`, `:7290-7291`) and do carry the
  list-column, so the group is correct once it is the only two.
- 2026-09-10: T6. The closing sentence of the batch entry named the classes that
  require a destination as "the audio verbs and the fan-in verbs", which left
  `separate_audio_video_batch()` -- listed above under single-input transforms, whose
  `output` the same paragraph calls optional -- reading as auto-deriving. It requires
  `audiofile` and `videofile` and derives neither (`R/ffmpeg.R:6470-6473`,
  `:6609-6618`), so it now has its own clause. `picture_in_picture_batch()` joined the
  requiring list for the same reason T5 split it out of the fan-in group: it derives
  no output either. `extract_audio_batch()` and `convert_audio_batch()` re-checked
  against their own `@param jobs`, both **required**, and the reason each gives -- the
  destination extension picks the format -- is what the sentence now states.
- 2026-09-10: T7. Both "already" claims and the columns instruction are gone. What the
  0.1.0 tree actually holds, measured at `4b04fad9`: `git grep audio_codec -- R/` is
  empty and so is `git grep vcodec -- R/`, and the only `_batch` name in that
  `NAMESPACE` is `ffm_batch` (Layer 1) -- no task-verb batch sibling existed, so a
  0.1.0 caller had no jobs table to rename a column in, and `vcodec` was never
  reachable either. The rename that IS reachable is three scalar arguments, which is
  what the bullet now says. The naming rationale is kept but re-tensed to the present
  package rather than asserted of the old tree. The no-alias sentence is derived from
  `formals()` at head: `extract_audio()` takes `infile`, `outfile`, `audio_codec`,
  `audio_stream`, `run` and `segment_video()` twelve names, neither carrying `...`, so
  an old name raises an unused-argument error rather than being swallowed.
- 2026-09-10: T8. The auto-naming claim is rebuilt from the derivation sites rather
  than patched. Every `jobs$output <- derive_*_names(...)` assignment in `R/ffmpeg.R`
  was enumerated: `_anonymized` (`:2434`), `_standardized` (`:4800`), `_stripped`
  (`:4957`), `_normalized` (`:5246`), `_cropped` (`:6264`), `_web` (`:6417`), and the
  restarting `<basename>_<n>` at `:4134` (`derive_segment_names()`, source extension)
  and `:4326` (`derive_frame_names()`, image `format`). So the list was missing
  `_anonymized` as the review found, and the restarting name belongs to
  `segment_video_batch()` as well as `extract_frame_batch()`. A third error the review
  did not name fell out of the same enumeration: "the frame-writing verbs" also
  covered `sample_frames_batch()`, which assigns no `output` at all -- it resolves an
  `outdir` (`derive_frames_dir()`, `:4364`) and an FFmpeg pattern
  (`derive_frame_pattern()`, `:224`), so it gets its own clause. `_web.mp4`'s fixed
  extension is from the comment at `:6128-6129`; `formals(sample_frames_batch)`
  confirms the scalar `outdir` the new sentence mentions.
- 2026-09-10: T9. `git ls-tree 4b04fad9 vignettes/` lists three (`batch.Rmd`,
  `metadata.Rmd`, `tidymedia.Rmd`) and HEAD has five, so two are new --
  `verification.Rmd` and `workflow.Rmd`, which are exactly the two the bullet's own
  body then describes, the reordered one being "Get started" (`tidymedia.Rmd`). Count
  and ordinal both corrected in the opening sentence; the body needed no change.
- 2026-09-10: T10. The hardware paragraph now keeps the two refusals apart and says
  which one `fallback` is for. The distinction is stated outright in
  `tm_hardware_encoder()` (`R/ffmpeg.R:3143-3151`): the (family, backend)-not-in-table
  refusal "fires under `fallback = TRUE` too -- a pair no backend can encode is a
  wrong argument, not a machine that lacks something", and the front-door comment at
  `:3325-3342` gives the same reading plus what the `fallback` early return DOES
  guard, an encoder the table holds and this build does not list. The families named
  are read off `hardware_backend_families()` (`:3028-3033`) against
  `hardware_codec_families()` (`:3049-3051`) rather than copied from
  `origin/master`'s prose: nvenc covers h264/hevc/av1 and so has no `prores`,
  videotoolbox covers h264/hevc and so has neither `prores` nor `av1`.

## Review

Reviewed 2026-09-09 on PR #124, against branch head `427d8d3`, after the AC4
amendment. `origin/master` had not moved since the branch was cut (0 behind, 5
ahead), so no merge was needed and every measurement below is against this tree.
The evidence recorded at the first review pass is superseded: AC4's wording
changed and `NEWS.md` changed under AC1-AC3, so all five were re-measured.

**AC1 - pass, against a discriminating control.** `grep -n '^# ' NEWS.md`
returns five `#` headings: `# tidymedia (development version)` at line 1, then
the four released ones at 1363, 1423, 1467 and 1508. One development-version
heading, and it carries no number. Sweeping the development section (lines
1-1362) for `[0-9]+\.[0-9]+(\.[0-9]+)*` returns 14 distinct strings, each read
in context: `4.1.0`/`4.5.0` are R versions, `1.1.0`/`1.2.0` rlang,
`2.5.0`/`3.0.3` withr, `6.1.1`/`9.0.1` FFmpeg, and `42.0`, `2.0`, `1.7`, `0.46`
and `0.1` are seconds and a tolerance. No tidymedia release version. The
control: the same sweep over `origin/master:NEWS.md`'s development section
returns `0.2.0` at its line 15 ("a version of tidymedia before 0.2.0"), so the
check fails when a release version is present. The one judgment call is
`pre-1.0` at lines 5 and 205 ("The package is pre-1.0 and", "the package's
pre-1.0 clean-break policy"): read as naming the versioning policy's threshold
rather than naming a release version for this section. Recorded, not dropped.

**AC2 - pass, against a discriminating control.** A parse of `NEWS.md` grouping
every `##` heading under its enclosing `#` section finds no heading occurring
twice in any section: the development section carries six (Breaking changes,
New features, Bug fixes, Performance, Documentation, Requirements), `0.1.0`
three, `0.0.0.9002` three, `0.0.0.9001` three, `0.0.0.9000` none. The control:
the same parse over `origin/master:NEWS.md` reports the development section's
20 headings with `## New features` x3, `## Breaking changes` x3, `## Bug fixes`
x3 and `## Documentation` x2 - the four repeats and the exact counts the
criterion cites for the pre-collapse state, so the check both fails when
repeats are present and confirms the criterion's stated starting point.

**AC3 - pass, against a discriminating control.** `comm` over the sorted
`export()` lines of `NAMESPACE` at head against `4b04fad9`: 63 exports then, 89
now, 40 added and 14 removed - the criterion's counts, and the removed set is
the plan's list exactly (`:=`, `as_label`, `as_name`, `audio_as_mp3`,
`convert_fractions`, `enquo`, `enquos`, `ffm`, `get_codecs`, `get_encoders`,
`get_framerate`, `get_samplingrate`, `mediainfo_summary`, `pad_integers`).
Every one of the 54 names is matched in the development section on word
boundaries that also exclude a preceding `.`: 0 missing added, 0 missing
removed. The control: the same check over `origin/master:NEWS.md`'s development
section reports `concatenate_videos_batch`, `strip_metadata` and
`strip_metadata_batch` missing - the three T3 filled - so the check fails when
names are absent.

**AC4 - pass, against a discriminating control.** The formals diff the
criterion specifies, run here from an R parse of `R/` at both trees (`git
archive` of `4b04fad9` and of head into scratch dirs; a top-level definition is
`name <- function(...)` or `name = function(...)` with `name` a symbol). It
reports 49 names exported in both `NAMESPACE`s and 48 parsed as such a
definition at both - `.data` the sole exclusion, a reexported rlang pronoun -
matching the criterion's figures. `427d8d3` changed only `NEWS.md` and three
`cairn/` files, so the tree the criterion pinned to `1df6e23` is the tree
measured. The removed-argument set is `extract_audio()`'s `acodec` and
`segment_video()`'s `ts_start` and `ts_stop`, three names, again as stated.
All three are named with their replacements in the content above the `#
tidymedia 0.1.0` heading, at `NEWS.md:30-35`: `acodec` -> `audio_codec`,
`ts_start`/`ts_stop` -> `start`/`end`. Both replacements check out against the
current formals - `extract_audio(infile, outfile, audio_codec, audio_stream,
run)` and `segment_video(infile, start, end, ...)` - so the note's claim is
derived, not composed. No removed argument needed the removed-with-no-successor
alternative. The control for the criterion's bound: `acodec` also occurs at
`NEWS.md:1452`, inside the released `0.1.0` section, so a whole-file grep would
have reported it named on released text alone; bounding the check to lines
1-1362 is what makes it discriminate.

**AC5 - pass.** The `verify` slot, run 2026-09-09 against this tree:
`Rscript -e 'devtools::test()'` at `[ FAIL 0 | WARN 12 | SKIP 5 | PASS 13266 ]`.
The five skips are hardware-encoder probes (`test-nvenc.R:435,446,458`,
`test-video-codec.R:480,489`); none reads `NEWS.md`. Two of the suite's tests do
assert on `NEWS.md`'s wording (`test-check-tracks-docs.R:126`,
`test-front-door-ordering.R:435`) and both pass here - the pair that caught the
collapse's two dropped claims during implementation. No roxygen changed on this
branch (`git diff origin/master...HEAD --name-only` is `NEWS.md`,
`cairn/DECISIONS.md`, `cairn/ROADMAP.md` and this file), so `document()` was not
required.

**Consistency gate — universal half clean, toolchain half 1 NOTE.**
`python3 scripts/cairn_validate.py` exits 0: every check PASS, 136 advisory
warnings, all of them the work-log line-wrapping advisory, which never fails a
gate. The `release window` advisory did not fire. `Principles touched: —` and
`DESIGN.md` is not in the diff, so `cairn_impact.py` was skipped.

The `r-package` profile's toolchain half:
- `devtools::document()` exits 0 and leaves no diff (`git status` shows only
  this milestone file, edited by this review).
- `pkgdown::check_pkgdown()`: "No problems found."
- README: neither `README.Rmd` nor `README.md` is in the diff, and both were
  last committed together at `0df9835`, so the knit is in sync.
- Changelog: `NEWS.md` *is* the milestone's deliverable; no milestone number
  reaches user-facing text (`grep -nE '\bM[0-9]{2,3}\b' NEWS.md README.md` is
  empty).
- No new top-level file: the only top-level path in the diff is `NEWS.md`, which
  already ships; `.Rbuildignore:7` still carries `^cairn$`.
- `devtools::check()`: **0 errors, 0 warnings, 1 NOTE** (5m 38s). The NOTE is
  the `spelling` test: two words this branch introduced into `NEWS.md` and that
  are in neither `inst/WORDLIST` nor the dictionary — `Relatedly` (`NEWS.md:1008`)
  and `centre` (`NEWS.md:318`). Both are absent from `origin/master:NEWS.md`, so
  the NOTE is branch-introduced. `Relatedly` is a wordlist gap and justifiable.
  `centre` is not: the sentence reads "corner or centre `position`", and the
  value the code accepts is `"center"` (`R/ffmpeg.R:7133,7529`), so a reader
  following the note writes a value that aborts. Carried below as finding G1
  rather than justified.

## Review findings

Three fresh-context lenses (user-facing tier, so the full fan-out). Every
finding reported is logged here with its disposition; each was re-verified
against the code before triage, and the verdicts below are mine, not the
reviewers'.

**[O] diff-bug lens — 12 findings.** It re-measured and confirmed AC1-AC4
independently, confirmed the released sections byte-identical, and confirmed
that every function name, condition class, `tm_*` field and option name the
development section mentions exists in `R/`. Its findings are about the prose
the collapse newly wrote. Verified CONFIRMED:

- **O1. `NEWS.md:365-368` — `picture_in_picture_batch()` takes no `inputs`
  list-column.** The branch-new sentence groups it with the fan-in verbs that
  "carry an `inputs` list-column plus a required `output` column". Its roxygen
  (`R/ffmpeg.R:7510-7518`) documents fixed `main`/`overlay`/`output` columns and
  cites D015 by name ("not a list-column; D015"). A reader following the note
  builds a jobs table the verb rejects.
- **O2. `NEWS.md:375-376` — "The audio verbs and the fan-in verbs require
  `output` and derive nothing" mis-describes `separate_audio_video_batch()`.**
  The note lists that verb under single-input transforms, whose `output` it says
  is optional and auto-derived. The code requires `audiofile` *and* `videofile`
  and derives neither (`R/ffmpeg.R:6610-6620`, roxygen at `:6470-6473`).
- **O3. `NEWS.md:30-35` — both "already" claims are false at the 0.1.0 tree.**
  `git grep audio_codec 4b04fad9 -- R/` is empty, so the re-encode verbs did not
  "already take" `audio_codec`/`video_codec`; `segment_video_batch` is one of the
  40 added exports, so its `start`/`end` columns did not "already use those
  names". The bullet then tells a 0.1.0 caller to "Rename the arguments and the
  columns" — columns they could not have had. This is the sentence `427d8d3`
  rewrote to be derived rather than composed.
- **O4. `NEWS.md:370-373` — the auto-naming list is wrong in two ways.** It omits
  `_anonymized`, which `anonymize_video_batch()` derives
  (`R/ffmpeg.R:2194-2196,2434`), and it attributes the restarting
  `<basename>_<n>` to "the frame-writing verbs" alone, when
  `segment_video_batch()` derives it too (`R/ffmpeg.R:3911,4134`).
- **O5. `NEWS.md:1217` — "Three new vignettes and a reordered fourth."**
  `git ls-tree 4b04fad9 vignettes/` lists three; HEAD has five. Two are new
  (`verification.Rmd`, `workflow.Rmd`), and the bullet itself goes on to name
  only those two plus the reordered "Get started". A merge artifact.
- **O6. `NEWS.md:517-523` — the merge dropped a qualifier that kept two error
  cases apart.** `origin/master`'s development section said asking a backend for
  a family it cannot encode is refused "whatever `fallback` is set to". HEAD says
  that case "is an error" and four lines later that "`fallback = TRUE` re-encodes
  in software with a message instead", so a reader concludes `fallback = TRUE`
  rescues av1 under videotoolbox. `R/ffmpeg.R:3273-3276` and the comment at
  `:3325-3340` state a `(family, backend)` pair the table lacks is refused on
  both arms.
- **O7. `NEWS.md:96-99` — one measured run is asserted of four verbs, and is
  wrong for one.** "On a three-track test file whose default flag sat on the
  second track ... the second is what came out" now covers `standardize_video()`,
  `anonymize_video()`, `segment_video()` and `format_for_web()`.
  `origin/master`'s development section recorded two different fixtures: the
  second-track file for the standardize/anonymize case (its line 701) and the
  segment case (its line 669), and a *third*-track file for
  `format_for_web()`/`normalize_audio()` (its lines 597-598). The merged sentence
  reads as one observation and is a composition of two.
- **O8. A user-visible ordering guarantee was dropped with no replacement.**
  `origin/master`'s development section (its lines 1127-1128) stated that under
  `two_pass = TRUE` a bad loudness target is refused "before the analysis pass
  measures the input, instead of after that measurement was already spent".
  Neither phrase occurs anywhere in HEAD's lines 1-1362, and it is not in the
  merged Bug-fixes ordering paragraph. No acceptance criterion covers this.
- **O9. `NEWS.md:181-192` — the `audio` -> `audio_input` rename is the class D091
  says to drop, and it survived.** `compare_videos`, `compare_videos_batch`,
  `picture_in_picture` and `picture_in_picture_batch` are all in the 40 added
  exports, so no caller of 0.1.0 could have written `audio =` on any of them.
  D091 (written on this branch) says such a name "is announced once, as the new
  export or argument it is, with no history of what it was called on the way" —
  the reasoning that removed `tidymedia.nvenc_encoders`, `has_nvenc()` and
  `vcodec`. The bullet also spends a paragraph on renaming a `jobs` column that
  never shipped.
- **O10. `cairn/ROADMAP.md:20` — the branch edited this row and left a sentence
  in it that its own rewrite falsified.** The row still reads "The unreleased
  `NEWS.md` section has the same split, explaining `unset_program()` in terms of
  `find_program()` while a later entry calls it internal." At HEAD `find_program`
  occurs once in the development section (`NEWS.md:1278`), in the entry that
  calls it internal; the `unset_program()` bullet does not mention it.
- **O11. `cairn/DECISIONS.md:4317` — D091 overstates the entry it cites.** "D014
  lets a name change outright until 0.2.0 reaches CRAN"; D014
  (`cairn/DECISIONS.md:176-177`) says only "the API is pre-0.2.0 and still
  soaking (D001)". The CRAN-arrival threshold is an extrapolation, and D091's
  argument turns on where that boundary sits. D091 has not merged, so it is still
  editable rather than superseded-only.
- **O12. Pre-existing, carried through unchanged.** `NEWS.md:1303-1304` "The
  other eight declared floors were exercised" — `DESCRIPTION` carries 10
  versioned `Imports`, so excluding `rlang` leaves nine. Verified present
  verbatim in `origin/master:NEWS.md:99-100`, so the diff did not introduce it.
  Also `NEWS.md:178-180`'s "collapses the two into one `codec` column", likewise
  verbatim from `origin/master`.

**[S] blame-history lens — no open finding.** It independently re-derived the
`vcodec` correction, the two test-caught dropped sentences
(`test-front-door-ordering.R:435`, `test-check-tracks-docs.R:126`), and the AC4
amendment, and found each already resolved at HEAD. It confirmed D091 does not
contradict D014/D077/D078/D079, that the released sections are byte-identical,
and that the genuinely 0.1.0-reachable renames (`get_samplingrate`,
`get_framerate`, `get_codecs`, `get_encoders`) are still announced as renames.
Note that it did **not** find O8 or O9, which are the same class it was hunting.

**[S] prior-review lens — no regression, one borderline observation.** The
existence probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`
returned `[]`, so there are no inline PR review comments at all and the per-PR
thread walk was skipped; the archived `## Review` sections were the evidence
base (M074, M080, M087, M090, M094, M097, M099, M106, M119). None of M119's five
fixed composed-claims is touched — all sit in the released sections. Its one
observation: `NEWS.md:481`'s "`NULL` now means the same thing on every codec
argument in the package" reuses the total quantifier the branch had just had to
fix at `NEWS.md:30`. Verified present verbatim in `origin/master:NEWS.md`, so it
is pre-existing, and the paragraph at `NEWS.md:501-508` already carves out
`convert_audio()`/`convert_audio_batch()` as the deliberate exception.

**Gate finding G1 (from the consistency gate, not a lens).** `NEWS.md:318` writes
"corner or centre `position`"; the accepted value is `"center"`
(`R/ffmpeg.R:7133,7529`). Branch-introduced, and it is what makes the
`devtools::check()` NOTE unjustifiable as it stands. `Relatedly`
(`NEWS.md:1008`) is the same NOTE's other word and is a wordlist gap, not a
wrong claim.

**Triage position taken to the gate.** Ten of the twelve [O] findings are
branch-introduced defects in shipped release-note prose, and none of them is a
style nitpick, a linter's job, or a complaint about an unmodified line. Two
(O1, O2) send a reader to build a jobs table the verb rejects; two (O6, O8) are
user-visible guarantees the collapse lost, one of them with no replacement
anywhere; three (O3, O5, O7) are claims that read as measured and are not; one
(O9) is the milestone's own new rule not applied to a bullet it governs. No
acceptance criterion covers any of them — AC1-AC5 measure headings, exports,
removed formals and the test suite, and all five pass. So this is not a
criterion failure; it is the return floor's other arm, a load-bearing defect in
what the deliverable does for its readers, which is the maintainer's judgment to
make at the gate. O12 and the `NEWS.md:481` observation are pre-existing and
rejected as out of scope. My recommendation to the maintainer is to return the
milestone to `in-progress` rather than fix ten prose defects inside the review
phase.

**PR-conversation read (PR #124, 2026-09-09, before the gate).** No reviews at
all (`pulls/124/reviews` empty), so no `CHANGES_REQUESTED` and the blocking rule
does not fire. No unresolved review threads (`reviewThreads` filtered to
`isResolved: false` returns none; `hasNextPage` false). One conversation comment.
- conversation: codecov[bot] PR — noted (reports coverage unchanged at 98.43%,
  4161 lines both sides, base `50d1995` against head `427d8d3`; requests
  nothing, author type `Bot`).
