# M120: NEWS.md reads as release notes

- **Status:** review
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

- [ ] AC1: `NEWS.md` opens with a single development-version `#` heading, and the
      development section names no release version number.
- [ ] AC2: A sweep over every `#` section of `NEWS.md` finds no `##` heading repeated
      within any one section. Today the development section carries `## New features`
      at lines 3, 267 and 1402, `## Breaking changes` at 48, 523 and 1943, `## Bug
      fixes` at 868, 1845 and 1961, and `## Documentation` at 20 and 1666.
- [ ] AC3: The development section names every export in the symmetric difference
      between `NAMESPACE` at head and `NAMESPACE` at commit `4b04fad9`, the commit
      that introduced the `# tidymedia 0.1.0` heading — 40 added and 14 removed as
      measured 2026-09-07.
- [ ] AC4: Parse every top-level `name <- function(...)` definition in `R/` at commit
      `4b04fad9` and at the branch head, take the names `export()`ed in both
      `NAMESPACE`s, and enumerate the formal argument names present at `4b04fad9` and
      absent at head. For each, the content above the `# tidymedia 0.1.0` heading names
      that argument together with the argument that replaced it, or states it was
      removed with nothing in its place. Measured 2026-09-09 at branch head `1df6e23`:
      49 names are exported in both `NAMESPACE`s, 48 of them parse as such a definition
      at both (`.data` is a reexported rlang pronoun, not a function), and the
      enumeration returns `extract_audio()`'s `acodec` and `segment_video()`'s
      `ts_start` and `ts_stop`.
- [ ] AC5: The `verify` slot of `cairn/PROFILE.md` is clean.

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

## Review

Reviewed 2026-09-09 on PR #124, against branch head `7fa561f`. `origin/master`
had not moved since the branch was cut (0 commits behind), so no merge was needed
and the evidence below is against the current tree.

**AC1 — pass.** `grep -n '^# ' NEWS.md` returns five `#` headings: `# tidymedia
(development version)` at line 1, then the four released ones at 1363, 1423, 1467
and 1508. One development-version heading, and it carries no number. A sweep of
the development section (lines 1-1362) for `[0-9]+\.[0-9]+(\.[0-9]+)*` returns 14
distinct strings: `1.1.0`, `1.2.0`, `2.5.0`, `3.0.3`, `4.1.0`, `4.5.0`, `6.1.1`
and `9.0.1` are dependency and FFmpeg versions; `42.0`, `2.0`, `1.7`, `0.46` and
`0.1` are seconds and a tolerance. No `0.2.0` remains. The one judgment call is
`pre-1.0`, at lines 5 and 205 ("The package is pre-1.0 and", "in line with the
package's pre-1.0 clean-break policy"): read as naming the versioning policy's
threshold rather than naming a release version for this section, which is what
Scope Out reserves for the release walk. Recorded here rather than dropped.

**AC2 — pass.** A parse of `NEWS.md` grouping every `##` heading under its
enclosing `#` section finds no heading occurring twice in any section: the
development section carries six (Breaking changes, New features, Bug fixes,
Performance, Documentation, Requirements), `0.1.0` three, `0.0.0.9002` three,
`0.0.0.9001` three, `0.0.0.9000` none. The three-and-two-occurrence counts the
criterion cites for the pre-collapse state are what the sweep found before the
collapse; after it, zero repeats.

**AC3 — pass, against a discriminating control.** `comm` over the sorted
`export()` lines of `NAMESPACE` at head against `4b04fad9` gives 63 exports then,
89 now, 40 added and 14 removed — the criterion's counts, and the removed set is
the plan's list exactly. Every one of the 54 names is matched in the development
section on non-word-character boundaries: 0 missing added, 0 missing removed. The
control: the same check run against `origin/master:NEWS.md`'s development section
reports `concatenate_videos_batch`, `strip_metadata` and `strip_metadata_batch`
missing — the three T3 filled — so the check fails when the names are absent.

**AC4 — FAIL.** Of the non-`NAMESPACE` renames the three entries record, the
argument renames are named as renames: `acodec`/`vcodec` -> `audio_codec`/
`video_codec` and `ts_start`/`ts_stop` -> `start`/`end` at `NEWS.md:30-36`, and
`audio` -> `audio_input` at `:182-192`. D078 adds no rename of its own. The
**option** rename D077 records — `tidymedia.nvenc_encoders` ->
`tidymedia.hardware_encoders` — is not named: the old string occurs zero times in
`NEWS.md`, the new one five times (lines 199, 527, 612, 618, 1163), and the nine
`nvenc` mentions in the section are all the backend string `"nvenc"`. The
work-log line of 2026-09-09 that records this criterion met states the option
rename is "named as renames ... now at zero occurrences"; zero occurrences of the
old name is the criterion failing, not meeting it.

The work is right and the criterion is not. `git grep nvenc_encoders 4b04fad9 --
R/` is empty and the 0.1.0 export list carries no `nvenc` or `hardware` name at
all, so the option was introduced *and* renamed inside this development cycle —
the same class the question gate dispositioned as dropped rather than announced
("`has_nvenc`/`nvenc_encoder` ... arrived and were renamed inside this cycle, so
a 0.1.0 reader never saw them"). That principle was settled at the implement
question gate, after AC4 was written, and narrowed AC4's effective domain without
amending it. AC4 as written sweeps in renames of names no 0.1.0 caller could have
used. Routed as an amendment return, not a defect return.

**AC5 — pass.** The `verify` slot: `Rscript -e 'devtools::test()'` at
`[ FAIL 0 | WARN 12 | SKIP 5 | PASS 13266 ]`, run 2026-09-09 against this tree.
The five skips are hardware-encoder probes (`test-nvenc.R:435,446,458`,
`test-video-codec.R:480,489`); none reads `NEWS.md`. No roxygen changed on this
branch (`git diff origin/master...HEAD --name-only` is `NEWS.md`,
`cairn/ROADMAP.md` and this file), so `document()` was not required.

**Consistency gate — partial, then stopped.**
`python3 scripts/cairn_validate.py` exits 0: all checks passed, 78 advisory
warnings, all of them the work-log line-wrapping advisory, which never fails a
gate. The `release window` advisory did not fire. No `DESIGN.md` principle
changed (`Principles touched: —`), so `cairn_impact.py` was skipped. The
`r-package` profile's toolchain half was not reached: the criterion failure above
takes the gate's exit, so `devtools::check()` and `pkgdown::check_pkgdown()` were
not run. The branch touches no `R/`, `man/`, `NAMESPACE`, `README`, `_pkgdown.yml`
or top-level file, so nothing those checks cover was modified.

**Steps 5-9 not run.** The three review lenses, the approval gate and the merge
were not reached. PR #124 stays a draft.
- 2026-09-09: `verify` slot re-run after the `NEWS.md` correction: `devtools::test()` at
  FAIL 0 | WARN 12 | SKIP 5 | PASS 13,266, same five hardware-encoder skips. No roxygen
  changed, so `document()` was not run. Status -> review.
