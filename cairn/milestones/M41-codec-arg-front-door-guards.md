# M41: Front-door validation parity for the codec arguments

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m41-codec-arg-front-door-guards` / [#43](https://github.com/jmgirard/tidymedia/pull/43)

## Goal

Every codec argument on every task verb rejects a non-string value at the front
door, naming its own argument and its own verb.

## Scope

**In:** the one silent-default bug — `normalize_audio_batch(audio_codec = NA)`
compiles the default command instead of erroring, because `batch_codec_cell()`
([ffmpeg.R:3153](../../R/ffmpeg.R#L3153)) maps a *scalar* `NA` to the `NULL`
sentinel and no front-door guard stops it. Plus the six remaining verb/argument
pairs whose abort either leaks Layer-1's `video`/`audio` parameter name, blames a
`*_pipeline()` helper, or fires inside `purrr::pmap()` rather than at the front
door — seven non-compliant pairs in all, measured by T2's script (T3). Plus a committed script that regenerates the pre-milestone compiled
commands from a git ref, so "this guard rejects nothing that worked before" is
re-verifiable at review rather than a transcript.

**Out:** what `NULL` and column `NA` *mean* per verb → M42. That covers the
`standardize_video` vs `anonymize_video` `NULL` disagreement, the `extract_audio`
vs `extract_audio_batch` disagreement, and `standardize_video_batch`'s
`video_codec` column rejecting `NA`. This milestone changes which values are
*refused*, never what an accepted value does — with one deliberate exception,
adopted at the 2026-07-29 amendment gate and stated in AC4: a bad *scalar* codec
argument that a matching `jobs` column used to override in silence is now
refused, completing the M37 review's repair of `separate_audio_video_batch`.

## Acceptance criteria

- [x] AC1: `normalize_audio_batch(jobs, audio_codec = NA)` at the default
      `two_pass = FALSE` aborts with a message naming `audio_codec`. Before the
      fix that call compiles the default command (`-af "loudnorm=..." -codec:v
      copy`, no `-codec:a`), identical to `audio_codec = NULL`; the regression
      test is shown to fail against the pre-fix sources the T2 script
      reconstructs. (`two_pass = TRUE` already aborts via
      [ffmpeg.R:2969](../../R/ffmpeg.R#L2969) — the silent compile is
      default-path-only.)
- [x] AC2: For every task verb and `_batch` sibling whose `video_codec` or
      `audio_codec` argument *sets* a codec — `verify_media()` excluded, its
      same-named arguments being expected probe values, not settings — passing a
      non-string scalar (`NA`, a number, a length-2 character vector) aborts with
      a message naming that verb's own argument, `video_codec` or `audio_codec`,
      never Layer-1's `video` or `audio`, and with the condition's `call` being
      the Layer-2 verb. The `*_pipeline()` helpers keep their existing checks,
      which the per-row column path inherits
      ([ffmpeg.R:438](../../R/ffmpeg.R#L438),
      [ffmpeg.R:3392](../../R/ffmpeg.R#L3392)); only the scalar-argument abort's
      `call` is constrained.
- [x] AC3: At `parallel = FALSE`, each abort AC2 inspects carries no
      `In index: <n>` in its message, on the same condition AC2 inspects —
      showing the scalar check ran before the fan-out, not inside
      `purrr::pmap()`.
- [x] AC4: Measured as the diff between the baseline the T2 script regenerates
      from the pre-milestone ref and the same grid run on the branch — every AC2
      verb/argument pair × the five value scenarios, with each `_batch` pair
      probed at `col = absent` and `col = present`, and each `_batch` verb also
      probed with an invalid `jobs` alongside a bad scalar codec — the set of
      rows whose outcome changed is **exactly** these two groups:
      (a) **21 rows at `col = absent`** — the three non-string scenarios (`NA`,
      a number, a length-2 character vector) on each of the seven pairs T3
      enumerated as non-compliant. One of them,
      `normalize_audio_batch audio_codec na`, moves `compiled -> abort` (AC1's
      silent-default bug); the other twenty were already aborts and change only
      their message text, their blamed call, or their `In index:` status.
      (b) **12 rows at `col = present`** — the same three scenarios on the four
      `_batch` verbs M41-D2 names (`standardize_video_batch`,
      `anonymize_video_batch`, `extract_audio_batch`, `normalize_audio_batch`),
      each moving `compiled -> abort`.
      And no others: no `default` or `null` row changes at either `col` setting,
      no row changes on any other verb/argument pair, and no invalid-`jobs` row
      changes which error it reports.
- [x] AC5: `extract_audio_batch`'s new `audio_codec` guard passes
      `allow_null = TRUE`, so `extract_audio_batch(audio_codec = NULL)` still
      compiles (`-vn`, no `-codec:a`) while `extract_audio(audio_codec = NULL)`
      still aborts; a code comment names that disagreement and points at M42.
- [x] AC6: `devtools::test()` and `devtools::check()` clean — 0 errors, 0
      warnings.

## Coverage

- AC1 → T2, T4
- AC2 → T3, T5, T6, T7, T11, T15
- AC3 → T5, T7
- AC4 → T2, T8, T10, T13, T14, T16
- AC5 → T5, T7
- AC6 → T1, T8, T12, T16

## Tasks

- [x] T1: Verify/install `spelling` in the active R 4.6 library. Environment
      repair, not a dependency change (no D-entry).
- [x] T2: Commit the baseline regeneration script under `data-raw/`: reconstruct
      `R/*.R` from a named git ref via `git show`, source them, and record the
      compiled command *or* abort message per AC2 pair per scenario. Capture the
      pre-milestone baseline from the default branch.
- [x] T3: Enumerate the AC2 verb/argument set from source into the work log —
      non-compliant and compliant pairs — so T7 runs over a fixed list.
- [x] T4: Regression test first, shown red against T2's reconstructed pre-fix
      tree, then the fix: front-door
      `check_string(audio_codec, allow_null = TRUE)` in `normalize_audio_batch`.
- [x] T5: Front-door guards for the remaining sites — `standardize_video_batch`
      and `anonymize_video_batch` `video_codec`, `extract_audio_batch`
      `audio_codec` (`allow_null`, plus the AC5 comment), `standardize_video`
      `video_codec`.
- [x] T6: Make `normalize_audio` and `convert_audio` blame the verb rather than
      their shared `*_pipeline()` helper — thread `call` or hoist the check.
- [x] T9 (discovered): Two `test-video-codec.R` execution tests shell out to
      *mediainfo* under only `skip_if_no_ffprobe()`, so they fail rather than
      skip where that CLI is absent. Blocks AC6, so fixed here.
- [x] T7: Parameterized test over T3's list: message and `call` for `NA`, a
      number and a length-2 vector on every pair, plus AC3's `In index:` absence
      at `parallel = FALSE`. Prove it discriminates by reverting a guard (M39).
- [x] T8: Re-run T2's script against both refs and confirm every `NULL`/default
      outcome matches. `@param` prose, `document()`, NEWS, `test()` + `check()`.
- [x] T10: Extend T2's grid with the codec-column dimension — each `_batch` verb
      probed with a matching codec column absent and present — re-run both refs.
- [x] T11: The four actioned round-1 findings: F8 fail-soft the sweep, F3 the
      `anonymize_video_batch` guard shape, F13 the unchecked `git show`, F19 the
      comment's false D021 citation.
- [x] T12: Correct the NEWS entry's "which values are accepted is unchanged",
      then `test()` + `check()` clean.
- [x] T13 (round 2, A1): Repoint the `picture_in_picture_batch` call template in
      both the instrument and the test at D015's `main`/`overlay`/`output`
      columns, so its `default`/`null` cells compile instead of aborting on
      missing columns and its `col = present` half stops duplicating `absent`.
- [x] T14 (round 2, A6): Move `standardize_video_batch`'s and
      `anonymize_video_batch`'s new codec guards after the jobs-shape check, so
      an invalid `jobs` keeps reporting first as it did pre-milestone; add an
      invalid-`jobs` dimension to the instrument, and a test, proving no verb
      changed which error it reports.
- [x] T15 (round 2, A7/A5): Give `convert_audio`'s hoisted guard the
      `if (!is.null(x)) check_string(x)` shape, so it stops advertising a `NULL`
      its own batch sibling's message denies; and correct NEWS's false
      "has always done" history for `separate_audio_video_batch`.
- [x] T16: Re-run the instrument against both refs and confirm AC4's enumerated
      changed-set exactly; `devtools::test()` + `devtools::check()` clean.

## Work log

- 2026-07-29: review-time FIX for A1r3/A3r3 (88/80), at the maintainer's direction at the round-3 gate. All four new guards moved to the end of their verb's front-door validation — `standardize_video_batch`, `anonymize_video_batch` and `normalize_audio_batch` to just before `ffm_batch()`, `normalize_audio` to below the `two_pass` block. 8 of the 9 measured cases are now byte-identical on both refs; AC4's enumerated set still matches (33 rows, 21/12, 0 vacuous). New regression test pins all six doubly-invalid calls, mutation-verified by hoisting one guard back (reddens two test blocks). A2r3 accepted and declared in NEWS instead — `standardize_video`'s dimension checks sit inside its pipeline, so restoring that precedence would mean duplicating `check_dim()` at the front door.
- 2026-07-29: LESSON for the log — `git checkout -- <file>` to undo a mutation experiment also discarded four uncommitted guard relocations, because the relocations had not been committed first. Redone and verified identical. Commit before mutating, always.
- 2026-07-29: SUPERSEDES the T14 line below that says "**11** of the 17 batch verb/argument pairs" are codec-before-`jobs` on `origin/master`. The measured split is **10 codec / 7 jobs**, on both refs. The miscount omitted `anonymize_video_batch audio_codec` (codec-first on `origin/master` under M39's placement) and counted M41's own two guards as codec-first on a ref where no such guard exists. Round-3 finding A7r3 (63); the code comment carrying the same number is corrected in place, this line supersedes the history. The 17-entry map itself was re-verified pair by pair and is correct.
- 2026-07-29: round-3 review found ONE actioned defect class (3 findings, 80/85/88), confirmed by my own before/after measurement rather than on report: M41's new front-door guards preempt validation that used to run first, so a call wrong about two things now reports a different one. Nine measured cases across `standardize_video_batch`, `anonymize_video_batch`, `normalize_audio_batch`, `standardize_video` and `normalize_audio`. `extract_audio_batch` is the control and is unaffected. AC4 as written still passes — it is scoped to the T2 grid, and the grid does not reach these calls.
- 2026-07-29: status -> review (round 3). All 16 tasks checked, `devtools::test()` 0 FAIL / 0 WARN / 15 SKIP / 2429 PASS, `devtools::check()` `Status: OK` 0 errors / 0 warnings / 0 notes (3m 6s), `devtools::document()` no diff. M41 authored no prose-guard — its tests assert runtime condition messages, not doc wording — so guard-doctrine §8's fresh-reader step does not apply.
- 2026-07-29: T16 checked AC4's enumerated set clause by clause, by script rather than by eye: 306 rows per side, grids the same size, **0** vacuous cells on either side, 33 changed rows total — 21 at `col = absent` on exactly the seven T3 pairs (only `normalize_audio_batch audio_codec na` moving `compiled -> abort`, the other 20 already aborts), 12 at `col = present` on exactly the four M41-D2 pairs (all `compiled -> abort`), **0** `default`/`null` rows and **0** `jobs = invalid` rows. Every clause of AC4 as amended, measured.
- 2026-07-29: T16 mutation-verified both new tests rather than trusting them (M39 lesson): reverting the `picture_in_picture_batch` template to the `inputs` shape reddens the jobs-shape test, and putting `standardize_video_batch`'s guard back ahead of the jobs check reddens the precedence test. Both files restored byte-identical, tree clean.
- 2026-07-29: T13/T14/T15 in one checkpoint — the three fixes interleave in `R/ffmpeg.R`, the instrument and the test file, and were verified together; each has its own log line below.
- 2026-07-29: T14 found A6 UNDERSTATED the split. Measured on `origin/master`, codec-before-`jobs` precedence holds on **11** of the 17 batch verb/argument pairs, not two: `compare_videos_batch` both, `crop_video_batch` both, `picture_in_picture_batch` both, `segment_video_batch` both, `standardize_video_batch` `audio_codec` (M39), plus M41's two. So the majority position is codec-first and the inconsistency is inherited, not M41's. Only the two M41 moved are moved back; normalizing the rest would change error text on verbs this milestone never touched — the exact fault being repaired.
- 2026-07-29: T14 pins that map as data in the test file rather than asserting a rule the package does not follow. A first draft asserted `jobs`-first universally and went red on 5 pre-existing pairs — a useful failure, since it is what measured the 11-vs-6 split.
- 2026-07-29: T14 — `standardize_video_batch`'s and `anonymize_video_batch`'s `video_codec` guards moved below the jobs-shape block; both now report the `jobs` error on a doubly-invalid call exactly as they did pre-milestone. Their `audio_codec` guards keep M39's placement: moving those would be the same unasked-for change pointing the other way.
- 2026-07-29: T14 added a `jobs = valid/invalid` dimension to the instrument (306 rows/side, was 255), so precedence is measured rather than argued; zero `jobs = invalid` rows differ between the refs.
- 2026-07-29: T13 — the `picture_in_picture_batch` template in the instrument AND the test used an `inputs` list-column; D015 requires named `main`/`overlay`/`output`, so every cell aborted on the missing columns and the verb contributed nothing to AC4's evidence. Both repointed.
- 2026-07-29: T13 added `codec_guard_vacuous()` to the instrument and a CI test that each batch template's `jobs` shape is one its own verb accepts — A1's *class*, not just its instance. The instrument check is what catches a cell measuring nothing; the test is what fails in CI when a template drifts.
- 2026-07-29: T13 also closed review A2 (logged sub-80) since the vacuity check surfaced it: an `audio_codec` column on `compare_videos_batch`/`picture_in_picture_batch` with `audio = NULL` hits D017's "needs an audio stream" before the scalar argument, so those two `col = present` cells measured nothing. A `col_extra` slot names an audio input for them; vacuous cells now 0 on both refs, was 2.
- 2026-07-29: T15 A7 — `convert_audio`'s hoisted guard changed to `if (!is.null(x)) check_string(x)`, so it and `convert_audio_batch` now give byte-identical messages for the same bad value; `NULL` still compiles `-q:a 0` (D021). Chose this over giving the batch sibling `allow_null`, which would have been more accurate about NULL but changes a message M41 never touched and would add rows to AC4's enumerated set.
- 2026-07-29: T15 A5 — NEWS's "which is what `separate_audio_video_batch()` has always done" replaced; that verb arrived without codec arguments and gained these guards later, so "already refused it" is the true claim.
- 2026-07-29: also strengthened `codec_guard_diff()` for the enumerated AC4: it now refuses to report over two baselines covering different cells, since matching runs over `after`'s keys and a `before`-only row would vanish silently (review A17's live half).
- 2026-07-29: AMENDMENT (substantive, gated) — AC4 restated as an **enumerated changed-set** criterion per the thrash-trigger-(b) remedy: the diff against the pre-milestone ref contains exactly 21 `col = absent` rows and 12 `col = present` rows, and no others. The two prior wordings were global negatives about new rejections ("adds no new rejection", then "exactly one class"), each falsified by a class nobody had named; an enumeration cannot be, since an unnamed class arrives as an extra row and fails visibly. Rejected at the gate: a third re-cut adding an AC1 carve-out to the same predicate shape. Coverage gains AC4 → T13/T14/T16, AC2 → T15, AC6 → T16; tasks T13–T16 added.
- 2026-07-29: implement gate chose restoring the pre-milestone `jobs`-before-codec error precedence on `standardize_video_batch`/`anonymize_video_batch` (A6) over keeping the new guards beside their sibling and enumerating the flip, because guard *placement* is not what AC2/AC3 constrain and the milestone's repeated failure mode is unenumerated behaviour change; falsified by a caller for whom the codec complaint is the more useful first error on a doubly-invalid call.
- 2026-07-29: over-cap remedy — the amendment pushed the plan-owned body to 151/149, so the heaviest section (Tasks) was compressed in one pass; completed T1–T12 keep their identity and shed detail the work log already holds. Now 145/149.
- 2026-07-29: REVIEW RETURN 2 — AC4 fails again, by a new mechanism (finding A10, scored 95). The amended criterion claims "exactly one class of new rejection" and describes only the column-override class, but the 33-row diff also contains `normalize_audio_batch audio_codec na` at `col = absent` moving `compiled -> abort` with no column present — AC1's silent-default bug, which AC4 does not carve out. Confirmed from my own AC1 measurement, not taken on report. AC4 unticked; every other criterion measured green this round. Status back to `in-progress`. Returns: 2.
- 2026-07-29: THRASH TRIGGER (b) — AC4 has now failed twice, each by a new mechanism of the same shape: a global negative about new rejections that measurement keeps falsifying. Recommended remedy is to change the approach rather than re-cut the predicate: restate AC4 as an enumerated changed-set criterion (the diff against the pre-milestone ref contains exactly these rows and no others), which cannot be falsified by a class nobody named. Trigger (a) not reached.
- 2026-07-29: review-time FIX for A3/C1 (92/84) — the executed suite gained the `col` dimension, so M41-D2's adopted refusal is now regression-tested on every `_batch` verb. Confirmed discriminating by mutation: making `normalize_audio_batch`'s guard column-conditional (exactly the alternative M41-D2 rejected) previously left the whole suite green at 2162 PASS, and now produces 3 targeted failures naming `col = present`. Suite 0 FAIL / 0 WARN / 15 SKIP / 2417 PASS; `check()` `Status: OK` 0/0/0.
- 2026-07-29: four more actioned findings for the return — A1 the instrument is still blind on `picture_in_picture_batch` (wrong jobs shape, so its `col = present` half duplicates `absent` and its default/null cells never compile); A6 guard-vs-`jobs` precedence split 2-to-3 across the batch verbs, changing which error a doubly-invalid call reports; A5 NEWS's "has always done" is false history; A7 the `convert_audio` hoist created a fresh scalar/batch message divergence. 23 sub-80 findings logged in the Review section, not dropped.
- 2026-07-29: status -> review (round 2). All tasks checked, `devtools::test()` 0 FAIL / 0 WARN / 15 SKIP / 2162 PASS, `devtools::check()` `Status: OK` 0 errors / 0 warnings / 0 notes. M41 authored no prose-guard (its tests assert runtime condition messages, not doc wording), so the guard-doctrine §8 fresh-reader step does not apply.
- 2026-07-29: NOTE FOR REVIEW — AC1/AC2/AC3/AC5/AC6 were ticked in round 1 against a tree that has since moved: F3 changes `anonymize_video_batch`'s message wording, F8 the sweep's control flow, F13/F19 the instrument and a comment. Those ticks stand as round 1's record, but every criterion needs fresh evidence this round, AC2 most of all since a guard's message text changed.
- 2026-07-29: T12 `devtools::check()` first run flagged `Relatedly` in my own new NEWS prose — the same self-inflicted spelling NOTE T8 hit. Reworded rather than added to `inst/WORDLIST`, on T8's precedent; re-run came back `Status: OK` with 0 notes.
- 2026-07-29: T12 NEWS corrected — the false "which values are accepted is unchanged" clause dropped from the message/blame bullet, and a third bullet added for the newly-refused case, naming the four verbs, citing `separate_audio_video_batch` as the existing precedent, and stating that accepted values are untouched. Also moved all three M41 bullets from the file's OLDEST `## Bug fixes` section to the newest one (review F4 — logged sub-80, but free to fix inside prose T12 had to rewrite anyway).
- 2026-07-29: T10 measured AC4's amended halves green: 255 rows per side (was 170; 85 of them `col = present`), 33 rows changed vs `origin/master`, and **zero** `default` or `null` rows changed at EITHER `col` setting. Of the 33, 21 sit at `col = absent` (the 7 originally-repaired pairs × 3 non-string shapes) and 12 at `col = present`, every one `compiled -> abort`, falling on exactly the four pairs M41-D2 names and on no other.
- 2026-07-29: T10 found review finding F11 (non-injective diff key, scored 30) does NOT hold — the key was already joined by a literal `\037` byte, which the review read as an empty separator. `col` was added to that key and to `codec_guard_report()`'s grouping, without which the two halves of each batch pair collapse onto one key and `match()` silently pairs `absent` against `present`.
- 2026-07-29: T11 F8 fixed and MEASURED, not eyeballed: the sweep now `next`s past a pair that did not abort. Blanking `normalize_audio_batch`'s guard (the only mutation that makes `cnd` NULL) gives FAIL 5 / PASS 507 with the `next` and FAIL 2 / PASS 261 without it, dying on `conditionMessage()` applied to NULL — so the pre-F8 sweep silently lost 246 assertions, which is the coverage claim F8 made. Both mutated files restored byte-identical.
- 2026-07-29: T11 F3 — `anonymize_video_batch`'s `video_codec` guard moved from `check_string(allow_null = TRUE)` to `if (!is.null(x)) check_string(x)`: behaviourally identical (NULL reaches the same per-row abort either way) but the message no longer offers `NULL` as legal when `anonymize_pipeline()` refuses it a few lines later, and it now matches both the scalar sibling's wording and `separate_audio_video_batch`'s guard shape.
- 2026-07-29: T11 F13 — the NAMESPACE `git show` in the baseline script's imports bootstrap now checks `attr(, "status")` like every other git call in the file; unchecked, a failed fetch yielded an empty imports env and the "could not find function" masquerade the file's own header warns about, arriving as a fake codec abort on every row.
- 2026-07-29: T11 F19 — the `extract_audio_batch` comment no longer cites D021 as recording the scalar/batch NULL split. It states what is actually true: D021 says `extract_audio` "accepts neither `NULL` nor `NA`", never mentions the batch verb, and is contradicted by measurement; the repair is a superseding entry from M42, not an edit to history and not a citation here.
- 2026-07-29: AMENDMENT (substantive, gated) — F2's new rejection on the four `_batch` verbs is ADOPTED, not reverted: AC4 rewritten to permit exactly that one class and Scope's contract-neutrality sentence qualified to name it. Decided at the gate on the ground that [ffmpeg.R:3828](../../R/ffmpeg.R#L3828) already refuses a bad scalar codec on `separate_audio_video_batch` for the identical reason (M37 review), so reverting would have left the package refusing the value on one batch verb and ignoring it on four. Coverage gains AC4 → T10 and AC2 → T11; tasks T10–T12 added. Rationale and the rejected alternative in M41-D2.
- 2026-07-29: minor amendment — T11 (the four actioned findings) runs before T10 (the grid extension), because T10's re-measurement is only meaningful against the final code; same reordering the T2/T3 entry below records, no task content changed.
- 2026-07-29: REVIEW RETURN 1 — AC4 fails as written. Independent review found that four batch verbs (`standardize_video_batch`, `anonymize_video_batch`, `extract_audio_batch`, `normalize_audio_batch`) newly REJECT a scalar `video_codec`/`audio_codec` of `NA` when `jobs` carries a matching codec column: `pick()` lets the column override the scalar, so the scalar was dead weight and a bad value in it was previously ignored. Re-measured directly: all four COMPILED on `origin/master` and abort on the branch. AC4's headline clause is "The guards add no new rejection", so this fails it; not narrowed to its NULL/default operationalization to make it pass. T2's script never builds a call template carrying a codec column, which is why its evidence missed this. Status back to `in-progress`.
- 2026-07-29: four more actioned findings to fix on return — F8 the parameterized sweep is not fail-soft (no `next` after the abort assertion, so `conditionMessage(NULL)` throws and kills the rest of the 20-verb loop); F3 `anonymize_video_batch`'s guard advertises `NULL` while `anonymize_pipeline()` refuses it six lines later; F13 unchecked `git show` in the baseline script's imports bootstrap; F19 a code comment citing a claim D021 does not make. Fourteen sub-80 findings logged in the Review section, not dropped.
- 2026-07-29: F19 also exposes a false claim in D021 itself — it asserts `extract_audio` "accepts neither `NULL` nor `NA`", but `extract_audio_batch(audio_codec = NULL)` compiled on `origin/master` and still compiles, and `extract_audio_batch` appears nowhere in DECISIONS.md. DECISIONS.md is history under IP4, so this is superseded by a new entry, never edited in place; the correction is M42's to make since M42 owns these semantics. Left for the maintainer to route.
- 2026-07-29: T8 COMPLETE — `devtools::check()` `Status: OK`, **0 errors / 0 warnings / 0 notes** (3m 3s), so AC6 is met and the checkpoint above is superseded. Both NOTEs from the first run were self-inflicted and are gone: the `typo'd` spelling hit and the two committed `.rds` scratch files. Status moved to `review`.
- 2026-07-29: CHECKPOINT, T8 INCOMPLETE — everything T8 asks for is done and committed except the final `devtools::check()` confirmation, which was still in its testthat stage when this checkpoint was made. The first check run returned 0 errors / 0 warnings / 2 NOTEs, both self-inflicted (the `typo'd` spelling hit and the two committed `.rds` scratch files); both causes are fixed here and the re-run had already cleared those two stages. T8 stays unchecked and the milestone stays `in-progress` until a check run is seen clean end to end.
- 2026-07-29: T8 `devtools::check()` also caught scratch debris I had committed myself: `baseline-origin-master.rds` and `baseline-worktree.rds`, RDS dumps my probe wrappers wrote into the repo root (cwd is the package root when running them), swept into commit 7df5216 by a `git add -A` I ran without checking `git status` first — the exact 'never sweep strangers into a checkpoint commit' rule. Removed from the index and from disk, and the `saveRDS()` calls deleted from the scratch wrappers so a re-run cannot recreate them. `data-raw/codec-guard-baseline.R` itself never wrote files; only my throwaway wrappers did.
- 2026-07-29: T8 — no `@param` needed updating: nothing in the roxygen documented an error for a non-string codec value (zero matches for that prose), so the guards changed no *documented* behaviour, and `devtools::document()` produced no diff. Deliberately did NOT document `NULL`'s per-verb meaning on `standardize_video` or `extract_audio_batch`, though both accept it: describing what an accepted value does is a contract statement, which this milestone's Scope reserves for M42.
- 2026-07-29: T8 NEWS entry added under the development version's Bug fixes — one bullet for the silent `NA` compile, one for the message/blame/timing repair naming the six affected verbs and stating that which values are accepted is unchanged.
- 2026-07-29: T8 `devtools::check()` first run failed the spelling comparison on `typo'd` in my own NEWS prose — the exact NOTE T1 exists to keep visible. Reworded rather than added to `inst/WORDLIST`, since informal contraction was not worth a wordlist entry.
- 2026-07-29: T7 added `tests/testthat/test-codec-arg-front-door.R`: the 34-pair list held as data, each pair asserted for abort + own-argument name + no Layer-1 `video`/`audio` name + `conditionCall()` being the verb + no `In index:` at `parallel = FALSE`; plus a completeness test that fails if a verb gains a codec argument without joining the sweep (`verify_media`'s two excluded on the record), plus a NULL-meaning test pinning the four per-verb NULL contracts M41 leaves alone. Suite 0 FAIL / 0 WARN / 2162 PASS.
- 2026-07-29: T7 mutation-verified rather than eyeballed (M39 lesson): blanking each of the 7 guards M41 added (ffmpeg.R lines 495, 802, 1171, 1368, 2587, 2973, 3347) turns the new test file RED every time, so none is false coverage.
- 2026-07-29: the same mutation sweep found 6 PRE-EXISTING scalar guards whose removal leaves the suite green -- `crop_video`, `compare_videos`, `picture_in_picture` `video_codec`/`audio_codec`. Not a defect and deliberately not touched: those verbs meet the front-door contract twice over, because `apply_video_codec()`/`apply_audio_codec()` already thread `call` and name the caller's argument. The test asserts the contract, not one mechanism for it, so it cannot distinguish which of two satisfies it -- and deleting both would still redden it. No candidate row filed.
- 2026-07-29: T5 added `check_string(<arg>, allow_null = TRUE)` front doors to `anonymize_video_batch` `video_codec`, `standardize_video_batch` `video_codec`, `standardize_video` `video_codec`, and `extract_audio_batch` `audio_codec` (the last carrying AC5's comment on the scalar/batch NULL disagreement and its M42 pointer). `anonymize_video_batch` and `standardize_video_batch` had byte-identical guard blocks but differ on what NULL does, so each got its own comment rather than a shared one.
- 2026-07-29: T6 hoisted duplicate front-door checks into `convert_audio` and `normalize_audio` per the implement-gate choice; both previously blamed their shared `*_pipeline()` helper, and both helpers keep their existing checks so the `_batch` siblings' per-row validation is untouched.
- 2026-07-29: T5/T6 measured green against the pre-milestone ref — non-compliant pairs 7 -> 0, and the diff is exactly 21 rows (7 pairs x na/number/vec2) with **zero** `default` or `null` rows changed, so AC4's contract-neutrality holds by measurement rather than by argument. `devtools::test()` 0 FAIL / 0 WARN / 1646 PASS.
- 2026-07-29: T2 script bug found and fixed before it could mislead T8: `base[[arg]] <- NULL` *deletes* a list element in R, so the `null` scenario was re-running `default` and every null row matched its default row for that reason alone. `base[arg] <- list(NULL)` stores the NULL. Also scrubbed `tempdir()` (per-session random suffix) out of compiled commands so two runs are comparable.
- 2026-07-29: with the null scenario actually exercised, AC4's and AC5's stated NULL outcomes are confirmed by measurement: `anonymize_video_batch` `video_codec` and `extract_audio` `audio_codec` abort on NULL; `extract_audio_batch` compiles `-vn` with no `-codec:a`; `standardize_video`/`_batch` drop `-codec:v libx264`; `convert_audio` gives `-q:a 0` (D021); `normalize_audio`/`_batch` emit no `-codec:a` (D019). The plan was right and the first probe was wrong.
- 2026-07-29: `anonymize_video_batch(video_codec = NULL)` aborts *inside* `purrr::pmap()` carrying `In index: 1`, and AC4 requires that be preserved -- so T7 asserts In-index absence only for AC2's non-string scenarios, never for NULL.
- 2026-07-29: T4 regression test written first and shown red on the pre-fix tree for the right reason -- `Expected normalize_audio_batch(jobs, audio_codec = NA, run = FALSE) to throw a error`, it returned a tibble carrying the silently compiled command; green after the front-door `check_string(audio_codec, allow_null = TRUE)`. Full suite 0 FAIL / 1646 PASS.
- 2026-07-29: T1 done as a verification, not an install — `spelling` is already present at 2.3.2 in the R 4.6.1 library and `inst/WORDLIST` carries 102 entries.
- 2026-07-29: minor amendment — T2 ran before T3, reversing the plan's order, because T3's enumeration is an *output* of T2's script rather than an input to it; no task content changed.
- 2026-07-29: implement gate chose hoisting a duplicate front-door `check_string()` into `convert_audio`/`normalize_audio` over threading `call` into their shared `*_pipeline()` helpers, because the helpers are shared with the `_batch` siblings and threading would also change the batch verbs' per-row messages that AC4 asks be proven unchanged; falsified by a third caller of either helper needing the verb-accurate blame that only threading gives.
- 2026-07-29: every new guard takes `allow_null = TRUE` — the only setting that cannot add a `NULL` rejection and so cannot violate AC4; `extract_audio`'s existing NULL-rejecting `check_string()` is deliberately left alone (AC5).
- 2026-07-29: T2 committed `data-raw/codec-guard-baseline.R` (+ `^data-raw$` in `.Rbuildignore`); it reconstructs `R/*.R` and `NAMESPACE` from a git ref, sources them under a rebuilt imports env, and probes 34 verb/argument pairs × 5 scenarios (default/null/na/number/vec2) at `run = FALSE`, recording compiled command or abort message, `conditionCall()`, and `In index:` presence.
- 2026-07-29: T2 self-test — `origin/master` reconstructed against the working tree gives a 170-row baseline and a **zero-row** diff, so the ref path is sound; building the imports env from NAMESPACE was required, since bare `glue()`/`tibble()` otherwise abort as "could not find function" and masquerade as codec aborts.
- 2026-07-29: T3 measured the AC2 set at 34 pairs (36 minus `verify_media`'s 2), of which **7** are non-compliant — the plan's Scope said six-plus-one as "seven remaining", corrected in place to six remaining / seven total.
- 2026-07-29: T3 non-compliant 1/7 — `normalize_audio_batch` `audio_codec`: `NA` **silently compiles** `-af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy` with no `-codec:a`, identical to `NULL` (AC1 confirmed).
- 2026-07-29: T3 non-compliant 2/7 — `standardize_video` `video_codec`: blames `ffm_codec(p, video = video_codec)` and leaks Layer-1's name `video`.
- 2026-07-29: T3 non-compliant 3/7 — `standardize_video_batch` `video_codec`: `In index: 1`, blames `purrr::pmap()`, leaks `video`.
- 2026-07-29: T3 non-compliant 4/7 — `extract_audio_batch` `audio_codec`: `In index: 1`, blames `purrr::pmap()`, leaks `audio` — the only pair failing all three AC2/AC3 counts.
- 2026-07-29: T3 non-compliant 5/7 — `anonymize_video_batch` `video_codec`: `In index: 1`, blames `purrr::pmap()`; names `video_codec` correctly but fires mid-fan-out.
- 2026-07-29: T3 non-compliant 6/7 — `convert_audio` `audio_codec`: blames `convert_audio_pipeline()`; `NULL` short-circuits to `-q:a 0` before the check, so the hoisted guard must allow NULL (D021).
- 2026-07-29: T3 non-compliant 7/7 — `normalize_audio` `audio_codec`: blames `normalize_audio_pipeline()` on the default `two_pass = FALSE` path.
- 2026-07-29: T3 compliant (27 pairs, T7 asserts these stay put) — `anonymize_video` both, `anonymize_video_batch` `audio_codec`, `compare_videos`/`_batch` both, `convert_audio_batch`, `crop_video`/`_batch` both, `extract_audio`, `picture_in_picture`/`_batch` both, `segment_video`/`_batch` both, `separate_audio_video`/`_batch` both, `standardize_video`/`_batch` `audio_codec`.
- 2026-07-29: minor amendment — added discovered task T9: `devtools::test()` was 2 FAIL / 2 WARN on a clean checkout of `master` before any M41 code change, both from `test-video-codec.R` execution tests calling `get_width()` (mediainfo) under only an ffprobe skip guard. Adding `skip_if_no_mediainfo()` to both leaves the suite 0 FAIL / 0 WARN / 15 SKIP / 1644 PASS. Pre-existing defect, not introduced here.
- 2026-07-29: this machine has the MediaInfo *GUI* (26.05) but not the CLI (`MediaArea.MediaInfo`), so `Sys.which("mediainfo")` is empty and the two T9 tests now skip locally; they still run wherever the CLI is installed, which is the behaviour the helper exists for.
- 2026-07-29: created by /milestone-plan.
- 2026-07-29: plan gate chose a front-door duplicate check over threading `arg`/`call` through `ffm_codec()` because the passthrough still runs inside `purrr::pmap()` and so cannot satisfy AC3; falsified by an engine-side seam that reports the caller's argument *before* the fan-out.
- 2026-07-29: plan gate chose a committed ref-based regeneration script over a testthat snapshot fixture and over an implementation-time transcript because it re-derives the baseline as fresh evidence at review without adding a churn-prone second snapshot file; falsified by the script failing to reconstruct a sourceable pre-milestone tree from a ref.
- 2026-07-29: plan gate chose preserving `extract_audio_batch(audio_codec = NULL)`'s current acceptance over fixing the scalar/batch split here, because it keeps M41 contract-neutral; falsified by a report that the batch verb's `NULL` acceptance is itself the user-visible bug.
- 2026-07-29: plan chose splitting guards (M41) from semantics (M42) over one milestone because the guard work needs no D-entry and the semantics work does; falsified by the semantics fix landing on the same code sites, making two PRs redundant.
- 2026-07-29: implement session start — branch cut from `master` @ 0a0ad90; `spelling` is present at 2.3.2 in the R 4.6.1 library, so T1 is a verification rather than an install. `Rscript` is not on the shell PATH; it lives at `C:\Program Files\R\R-4.6.1\bin\Rscript.exe`.
- 2026-07-29: R is 4.6.1 via winget (the R-4.4.1 directory is a stale leftover, not a second install). `archive` was absent from the 4.6 library during investigation — probes sourced `R/*.R` directly to work around it — and is now installed at 1.1.13, so `load_all()` succeeds; `spelling` remains absent and T1 installs it.

## Decisions

### M41-D1 — A duplicate front-door check, not a threaded `call` (2026-07-29)

`convert_audio()` and `normalize_audio()` validated their `audio_codec` inside a
`*_pipeline()` helper, so the abort blamed the helper — a name no caller typed.
Two ways to fix it, and the milestone took the first:

- **Chosen: hoist a second `check_string()` into the verb**, leaving the
  helper's check exactly as it was. The cost is that the value is checked twice
  on the scalar path.
- **Rejected: thread a `call` argument through the helper.** No duplication, but
  both helpers are shared with the `_batch` sibling for per-row validation, so
  threading also rewrites the batch verbs' per-row messages — the very outcomes
  AC4 asks be proven unchanged. It buys tidiness at the price of widening a
  contract-neutral milestone's blast radius.

Hoisting also makes all seven of M41's guards one shape, which is what let the
mutation sweep certify them uniformly.

**Falsified by:** a third caller of either `*_pipeline()` helper needing the
verb-accurate blame that only threading provides — at which point the duplicate
front doors become the redundant copies and threading is the cheaper fix.

**Not a D-entry:** M41 changes which values are refused, never what an accepted
value does, so nothing cross-cutting was decided. The `NULL`/`NA` semantics this
deliberately leaves alone are M42's, per D021's closing note.

### M41-D2 — The bad scalar is refused even when a column overrides it (2026-07-29)

On a `_batch` verb, `pick()` prefers a `jobs` column over the scalar argument of
the same name, so a scalar `video_codec`/`audio_codec` that the table also
carried was never read — and a non-string value in it was ignored rather than
refused. M41's front doors refuse it. Review finding F2 measured the change on
`standardize_video_batch`, `anonymize_video_batch`, `extract_audio_batch` and
`normalize_audio_batch`: all four compiled on `origin/master`, all four abort on
the branch. AC4 as planned forbade any new rejection, so the gate had to choose.

- **Chosen: adopt the refusal and amend AC4.** `separate_audio_video_batch`
  already does exactly this at [ffmpeg.R:3828](../../R/ffmpeg.R#L3828), added by
  the M37 review for the same reason — "`video_codec = NA` silently emitted
  nothing whenever `jobs` happened to carry a codec column". Adopting makes all
  five batch verbs agree; the cost is that M41 is no longer strictly
  contract-neutral, so Scope, AC4 and NEWS all say so.
- **Rejected: gate each guard on the column's absence.** Preserves AC4 as
  written, but keeps a bad scalar silently ignored in exactly the case a caller
  is least likely to notice, and leaves one batch verb refusing the value while
  four ignore it.

**Falsified by:** a caller who deliberately passes a placeholder non-string
scalar alongside a complete codec column — for whom the refusal is a regression
rather than a repair.

**Still not a D-entry:** the choice is about which values are *refused*, never
what an accepted value *means*, so it does not join the D016–D021 semantics
family; and it follows the M37 review's in-file precedent rather than setting new
policy.

## Review

### Round 3 — 2026-07-29

`origin/master` (0a0ad90) is an ancestor of HEAD and local `master` is in sync, so
no merge was needed and all evidence is fresh against the merge base. CI green on
the exact review commit `c9c0c25` — 9/9 checks (4 R-CMD-check platforms, pkgdown,
test-coverage, 2 codecov). PR #43 remains a draft. Evidence for AC1–AC5 is one
session's re-run of `data-raw/codec-guard-baseline.R` against both refs:
**306 observations per side** (34 verb/argument pairs × 5 scenarios, the 17
`_batch` pairs also probed at `col = present`, and each `_batch` pair's three
non-string scenarios also probed with an invalid `jobs`).

- **AC1 — measured, passes.** On the branch, `normalize_audio_batch(jobs, audio_codec = NA)`
  at default `two_pass = FALSE` aborts "`audio_codec` must be a single string or
  `NULL`, not `NA`." — names `audio_codec`, no `In index:`. Against the tree the
  script reconstructs from `origin/master` the same call **compiled**
  `-af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy`, `identical()` to the `NULL`
  call's command and carrying no `-codec:a`.
- **AC2 — measured, passes, 0 violations** over 153 observations (51
  verb/argument/`col` cells × 3 non-string shapes, scoped to `jobs = valid`; the
  `jobs = invalid` cells are AC4's precedence probe, where the call is
  deliberately wrong about the table too). Every one aborts; every message names
  that verb's own argument; **0** match the Layer-1 leak "`video` must be" /
  "`audio` must be"; every `conditionCall()` deparses to the Layer-2 verb — the
  blamed set is exactly the 20 verbs, with no `*_pipeline()`, `pmap` or `ffm_`
  among them. `verify_media()` excluded per the criterion, and confirmed absent
  from the grid.
- **AC3 — measured, passes, 0 violations.** No abort in those same 153
  observations carries `In index:` at explicit `parallel = FALSE`.
- **AC4 — measured, passes, every clause.** Grids equal at 306 rows; **0** vacuous
  cells on either side (a cell whose `default` call does not compile satisfies the
  comparison while measuring nothing — the round-2 defect). **33** rows changed:
  **21 at `col = absent`** on exactly the seven pairs T3 enumerated, of which
  exactly one — `normalize_audio_batch audio_codec na` — moves `compiled → abort`
  and the other twenty were already aborts changing only message, blame or index;
  **12 at `col = present`** on exactly the four pairs M41-D2 names, every one
  `compiled → abort`. And no others: **0** `default` rows, **0** `null` rows, **0**
  `jobs = invalid` rows.
- **AC5 — measured, passes.** `extract_audio_batch(audio_codec = NULL)` compiles
  `-y -i "<in>" -vn "a.aac"`: `-vn` present, no `-codec:a`.
  `extract_audio(audio_codec = NULL)` still aborts "`audio_codec` must be a single
  string, not `NULL`." The guard passes `allow_null = TRUE` and its comment states
  the scalar/batch disagreement and routes it to M42.
- **AC6 — measured, passes.** Both re-run at review on the exact review tree:
  `devtools::test()` 0 FAIL / 0 WARN / 15 SKIP / **2429 PASS**, and
  `devtools::check()` `Status: OK`, **0 errors / 0 warnings / 0 notes** (3m 8s).

**Consistency gate.** `cairn_validate` exit 0 — all 16 CHECKs PASS, 7 advisories OK,
one WARN: `sizing` at 16 tasks against the >10 tripwire, which fired because two
review returns added tasks to a finished plan, not from mis-sizing. `cairn_impact`
skipped — `DESIGN.md` untouched, no principle changed. Toolchain gate (r-package):
`devtools::document()` no diff · `pkgdown::check_pkgdown()` "No problems found" ·
`NAMESPACE`, `man/`, `data/`, `_pkgdown.yml`, `README*` all untouched · `NEWS.md`
carries the entry with no milestone numbers in user-facing text ·
`data-raw/` has its `^data-raw$` `.Rbuildignore` entry.

**Independent review — three lenses, then a scorer.** Three fresh-context reviewers
with distinct evidence bases (the diff; `git blame` history; the prior-review
record), then a separate Sonnet scorer that did not generate the findings and was
given the diff and the plan. The blame lens reported **no history conflicts** —
D019's analysis-before-refusal ordering intact, D021's `-q:a 0` sentinel intact,
`col_extra`'s `audio = 0` a valid D009 index, and no CI coverage lost by T9
(macOS/Windows runners install neither binary; the Linux runners install both).
The prior-review lens reported **no regressions** across all five points it was
pointed at; its `gh api .../pulls/comments` probe returned `[]`, so it worked from
archived `## Review` sections. The diff lens returned 19 findings; **3 scored 80+
and are actioned**, 16 below 80 are logged here rather than dropped (IP3).

**Actioned (score ≥ 80) — one defect class, confirmed by the orchestrator's own
measurement against both refs, not taken on report.** M41's new front-door guards
preempt validation that used to run first, so a call wrong about *two* things now
reports a different one of them. Nine measured cases:

- **A1r3 (88) — the A6 fix is incomplete.** T14 moved the new scalar guards below
  each verb's jobs-*shape* block, but they still sit above the second tier: the
  override-column type checks, `check_audio_codec_not_copy(jobs$audio_codec)`, and
  the derived-output duplicate check. Measured `origin/master` → branch:
  `standardize_video_batch(tibble(input = c(f,f)), video_codec = NA)` moves from
  the duplicated-`input` abort to the `video_codec` abort; the same call with a
  numeric `pixel_format` column moves from the `pixel_format` abort;
  `anonymize_video_batch` and `normalize_audio_batch` move the same way on
  duplicated `input`; and `normalize_audio_batch(tibble(input = f, output = o,
  audio_codec = "copy"), audio_codec = NA)` moves from ``` `audio_codec` can't be
  "copy" ``` to `check_string`. `extract_audio_batch` is the control and does
  **not** change — its guard sits below all of its jobs validation. The instrument
  is blind to this: `col = present` always injects a *valid* codec, and
  `jobs = invalid` only ever sets `jobs <- "oops"`, which trips the very first
  `is.data.frame()` check.
- **A2r3 (85) — `standardize_video`'s guard flips against the dimension checks.**
  Measured: `standardize_video(f, o, width = 0, video_codec = NA)` reported
  ``` `width` must be a single FFmpeg expression or number ``` on `origin/master`
  and reports the `video_codec` abort on the branch. Pre-M41 the codec error came
  from `ffm_codec()` inside `standardize_pipeline()`, i.e. after `ffm_scale()`'s
  `check_dim()`. The grid never passes an invalid `width`/`height`/`fps`.
- **A3r3 (80) — the `two_pass = TRUE` path changed, and the grid fixes
  `two_pass`.** The loudness verbs' new guards were hoisted above the entire
  `if (two_pass)` block, which already validated the argument via `check_token()`
  → `check_string()` *without* `allow_null`. Measured:
  `normalize_audio(f, o, audio_codec = NA, two_pass = TRUE)` changes message from
  "must be a single string" to "must be a single string or `NULL`"; and with
  `channels = 0` added it flips from the `channels` abort to the `audio_codec`
  abort. Round-1 finding F7 (68) named this dimension gap and was not actioned.

**Triage — FIXED IN THIS ROUND at the maintainer's direction (2026-07-29 gate).**
All four new guards moved to the END of their verb's front-door validation, which
is where the pre-milestone code effectively checked them (per row, inside the
fan-out). Re-measured against `origin/master`: **8 of the 9 cases are now
byte-identical on both refs** — every A1r3 case and both A3r3 cases. AC4's
enumerated set still matches exactly (33 rows, 21/12, 0 vacuous cells) and
`extract_audio_batch` stays the unchanged control. A regression test pins all six
doubly-invalid calls, and mutation-verified: hoisting `standardize_video_batch`'s
guard back above the jobs checks reddens both it and the precedence test.

**A2r3 accepted and declared, not fixed.** `standardize_video`'s dimension checks
live inside `standardize_pipeline()`, so restoring that one precedence would mean
duplicating `check_dim()` at the front door — scope this milestone does not have.
`standardize_video(f, o, width = 0, video_codec = NA)` therefore still reports the
codec problem where it once reported `width`. Declared in NEWS as a knock-on, in
plain words, naming the verb and stating that no previously-accepted value is now
refused.

**Note on AC4.** AC4 as amended is scoped by its own wording to the T2 grid
("Measured as the diff between the baseline the T2 script regenerates … and the
same grid run on the branch"), and within that grid the changed set matched the
enumeration exactly. So AC4 **passes as written** and is ticked on that evidence;
it is deliberately not reinterpreted to fail. What the three findings show is that
the grid does not span the space — which is a defect in the evidence instrument
and in the guards, not a criterion failure. Recorded plainly so the distinction
survives: the criterion is met and the milestone still changed behaviour nobody
declared.

**Logged, not actioned (score < 80):** A4r3 (76) both citations of the
`separate_audio_video_batch` precedent still point at ffmpeg.R:3828; the guards are
at 3870–3871 (round 2's A11, unfixed) · A5r3 (70) the new jobs-shape test guards
only the test file's templates, not the `.Rbuildignore`d instrument's, so the
CI-catches-drift claim in its own comment overreaches · A6r3 (65) `codec_guard_diff()`'s
`in_index` axis still ignores an NA↔TRUE/FALSE transition (the unfixed half of
round 2's A17) · A7r3 (63) the precedence comment and work log say "eleven of
seventeen" where the table has 10 codec / 7 jobs — **corrected in this round**, see
below · A8r3 (62) the committed instrument asserts none of AC4's enumerated counts,
so the clause-by-clause check is still a per-round manual script · A9r3 (55) the new
jobs-shape test's `tryCatch(condition=)` could pass vacuously on a warning, and its
detector misses two real shape messages · A10r3 (55) `col_extra` uses `[[<-`, the
NULL-deletion trap this file documents twice elsewhere · A11r3 (55)
`codec_guard_vacuous()` inspects only the `default` scenario · A12r3 (52) the
`extract_audio_batch` comment's "D021 never mentions this batch verb" is an
over-read — D021's `check_batch_string_col()` clause does address the batch verb's
column (round 2's A20) · A13r3 (50) the NEWS bullets sit at section 13 of 19, not
near the top (round 1's F4) · A14r3 (45) NEWS's "Every … argument" still overclaims;
token-invalid strings and `anonymize_video_batch(video_codec = NULL)` still leak
`In index:` (F1/A4/A9, third round raised) · A15r3 (45) the instrument merges git
stderr into reconstructed sources (round 2's A14) · A16r3 (45) `codec_guard_diff()`
scrubs paths only from compiled rows, not from `call`/abort messages · A17r3 (40)
the precedence classifier labels any non-`jobs` message "codec" · A18r3 (30) the
`\037` key separator is a raw control byte that renders as `sep = ""` (round 2's C2)
· A19r3 (12) a blank line makes one NEWS list loose.

**Review-time correction.** A7r3 is an error this session introduced: the
precedence comment in `tests/testthat/test-codec-arg-front-door.R` and the T14 work-log
line both said "eleven of these seventeen pairs" where the measured split is **10
codec / 7 jobs** on both refs. The miscount omitted `anonymize_video_batch audio_codec`
(codec-first on `origin/master` via M39's placement) and counted M41's own two guards
as codec-first on a ref where they do not exist. The comment is current knowledge and
is corrected in place; the work-log line is history and is superseded, not edited. The
17-entry map itself was verified correct pair by pair and is unchanged.

### Round 2 — 2026-07-29 — SENT BACK (AC4 fails again, by a new mechanism)

`origin/master` is an ancestor of HEAD and local `master` is in sync, so no merge
was needed and all evidence is fresh against the merge base. CI green on the exact
review commit `c7f8596` (9/9 checks: 4 R-CMD-check platforms, pkgdown, coverage).
PR #43 remains a draft. Evidence for AC1–AC5 is one session's re-run of
`data-raw/codec-guard-baseline.R` against both refs: 255 observations per side
(34 verb/argument pairs × 5 scenarios, with the 17 `_batch` pairs probed at both
`col = absent` and `col = present`).

- **AC1 — measured, passes.** On the branch, `normalize_audio_batch(jobs, audio_codec = NA)`
  at default `two_pass = FALSE` aborts "`audio_codec` must be a single string or
  `NULL`, not `NA`." — names `audio_codec`, no `In index:`. Against the tree the
  script reconstructs from `origin/master` the same call **compiled**
  `-af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy`, byte-identical to the `NULL`
  call and carrying no `-codec:a`.
- **AC2 — measured, passes, 0 violations** over 153 observations (51 verb/argument/col
  cells × 3 non-string shapes): every one aborts; every message names that verb's own
  argument; none matches the Layer-1 leak "`video` must be"/"`audio` must be"; every
  `conditionCall()` deparses to the Layer-2 verb, the blamed set being exactly the 20
  verbs, with no `*_pipeline()`, `pmap` or `ffm_` among them. `verify_media()` excluded
  per the criterion.
- **AC3 — measured, passes, 0 violations.** No abort in those same 153 observations
  carries `In index:` at explicit `parallel = FALSE`.
- **AC4 — FAILS AS WRITTEN (finding A10).** Both halves the criterion *describes*
  measure green: zero `default`/`null` rows changed at either `col` setting (of 102
  compared), and the 12 `col = present` changes are all `compiled → abort` on exactly
  the four pairs M41-D2 names. But AC4 claims the guards add "exactly one class of new
  rejection" and describes only the column-override class, while the 33-row diff
  contains a second: `normalize_audio_batch audio_codec na` at `col = absent`, with no
  column present, also moves `compiled → abort`. That is AC1's silent-default bug —
  legitimate, and separately required by AC1 — but AC4 does not carve it out, so the
  criterion is false against the very evidence it cites. Not reinterpreted to pass;
  round 1 returned this milestone for exactly that discipline.
- **AC5 — measured, passes.** `extract_audio_batch(audio_codec = NULL)` compiles
  `-y -i "<in>" -vn "<out>"`: `-vn` present, no `-codec:a`.
  `extract_audio(audio_codec = NULL)` still aborts "`audio_codec` must be a single
  string, not `NULL`." The guard passes `allow_null = TRUE` and its comment states the
  scalar/batch disagreement and routes it to M42.
- **AC6 — measured, passes.** `devtools::test()` 0 FAIL / 0 WARN / 15 SKIP / 2417 PASS
  and `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes (3m 4s), both
  re-run on the review tree after this round's coverage fix.

**Consistency gate.** `cairn_validate` exit 0 — all 16 CHECKs PASS, 7 advisories OK,
one WARN: `sizing` at 12 tasks against the >10 tripwire, which fired because review
returns added tasks to a finished plan, not from mis-sizing. `cairn_impact` skipped, no
`DESIGN.md` principle changed. Toolchain gate (r-package): `devtools::document()` no
diff · `pkgdown::check_pkgdown()` "No problems found" · `NAMESPACE`, `man/`, `data/`,
`_pkgdown.yml`, `README*` all untouched · `NEWS.md` carries the entry · `data-raw/` has
its `^data-raw$` `.Rbuildignore` entry.

**Independent review — three lenses, then a scorer.** Three fresh-context reviewers
with distinct evidence bases (the diff; `git blame` history; the prior-review record),
then a separate Sonnet scorer that did not generate the findings and was given the diff
and the plan. 30 findings scored; **7 at 80+ actioned**, 23 below 80 logged here rather
than dropped (IP3). The prior-review lens's `gh api .../pulls/comments` probe returned
`[]`, so it worked from archived `## Review` sections. The blame lens confirmed D019's
analysis-before-refusal ordering preserved, D021's `-q:a 0` sentinel intact, no prior
guard ever added-then-removed on these pairs, and no CI coverage lost by T9.

**Actioned (score ≥ 80):**

- **A10 (95) — AC4 is contradicted by its own evidence.** See AC4 above. Verified by
  the orchestrator from its own AC1 measurement, not taken on report. Requires a gated
  amendment: state AC4 as an enumerated changed-set rather than a global negative.
- **A3 (92) / C1 (84) — M41-D2's adopted change had zero regression coverage.** The
  whole substance of round 2 was measured only by the `.Rbuildignore`d instrument.
  Confirmed by mutation: making one guard column-conditional (the alternative M41-D2
  rejected) left the full suite at 0 FAIL / 2162 PASS. **FIXED IN THIS ROUND** — the
  test sweep gained the same `col` dimension the instrument has; the same mutation now
  yields 3 targeted failures and the suite is 2417 PASS. C1 reached it independently
  from M39's review F3, which taught this exact lesson.
- **A1 (85) — the instrument is still structurally blind on one verb.**
  `picture_in_picture_batch` requires named `main`/`overlay`/`output` columns (D015),
  but the instrument and the test both hand it `inputs = list(c(s,s))`. Its
  `default`/`null` cells abort on missing columns and never compile, so AC4's
  "zero rows changed" is vacuous there, and its `col = present` half duplicates
  `absent`. Same blindness class that caused round 1's return, on a different verb.
- **A6 (82) — guard-vs-`jobs` precedence split 2-to-3, and an unmeasured behaviour
  change.** `anonymize_video_batch` and `standardize_video_batch` guard the codec
  *before* the `is.data.frame(jobs)` check; the other three batch verbs guard after.
  Measured: `standardize_video_batch("oops", video_codec = NA)` reported the `jobs`
  error on `origin/master` and reports the codec error on the branch. Every instrument
  template passes a valid `jobs`, so the grid cannot see it.
- **A5 (80) — NEWS states a false history.** "which is what `separate_audio_video_batch()`
  has always done" — that verb arrived in M29 with no codec arguments and gained these
  guards in M37.
- **A7 (80) — the `convert_audio` hoist created a fresh scalar/batch message
  divergence.** `convert_audio` now says "must be a single string or `NULL`" while
  `convert_audio_batch` still says "must be a single string" and accepts `NULL`
  regardless — the mirror image of the defect F3 was actioned to remove, introduced by
  this diff.

**Logged, not actioned (score < 80):** A11 (78) both citations of the
`separate_audio_video_batch` precedent point at the wrong lines (actual guards 3847–48)
· A17 (70) `codec_guard_diff()` is asymmetric on `in_index`, drops before-only rows, and
carries a dead `nrow(b)` guard · A19 (65) the new code comments narrate review-finding
IDs and a DECISIONS dispute inside `R/ffmpeg.R` · A2 (60) `col = present` degrades to an
unrelated D017 abort on `compare_videos_batch audio_codec`, and nothing asserts the
injected column won the `pick()` · A12 (60) AC4 unticked while the log called it green,
now moot · A4 (55) NEWS's "Every … argument" overclaims; token-invalid strings still
leak · A18 (55) the 20 call templates are hand-duplicated across instrument and test,
already divergent per A1 · A20 (55) the comment's "D021 never mentions this batch verb"
is an over-read of D021's `check_batch_string_col()` bullet · C3 (55) F15 is now
deliberately entrenched rather than resolved · A13 (50) path scrubbing misses
list-columns, 14 of 89 compiled rows keep machine paths · A16 (50) `tryCatch(condition=)`
in both instrument and test treats a warning as the outcome · A14 (45) the instrument
merges git stderr into reconstructed sources · B1 (45) "same shape" holds for only one of
the four guards · A9 (40) the Goal's "non-string" is broader than what shipped, since
`anonymize_video_batch(video_codec = NULL)` still aborts mid-`pmap` · A21 (38) a 428-line
instrument nothing ever executes · A8 (35) five other messages deny a `NULL` they accept
(pre-existing) · A15 (35) the reconstructed-ref env falls through to the search path ·
A22 (25) the AC1 test's `tryCatch(error=)` degrades to a type error on regression ·
C2 (25) F11's refutation confirmed; the `\037` separator stays undocumented · B2 (20)
D021's own text stays uncorrected pending M42 · B3 (15) verification that the adoption
contradicts no prior decision · A23 (10) a PASS-count gap fully explained by this
round's added assertions.

**Thrash rule — trigger (b) fires.** Returns for this milestone: **2**, so trigger (a)
(the third return) has not been reached. But AC4 has now failed twice, each time by a
new mechanism of the same shape: round 1 because a new rejection existed that the
criterion forbade, round 2 because a second new rejection exists that the amended
criterion does not carve out. The shape is AC4 asserting a global negative about new
rejections that measurement keeps falsifying. The remedy is to reconsider the approach
rather than re-cut the same predicate: state AC4 as an **enumerated changed-set**
criterion — the diff against the pre-milestone ref contains exactly these rows, and no
others — which is checkable and cannot be falsified by a class nobody thought to name.

**Disposition.** Status back to `in-progress`. AC4 needs a gated amendment
(`/milestone-implement` step 6), and A1, A6, A5, A7 need fixing in the same return;
A3/C1 was fixed during this review and its evidence is recorded above.

### Round 1 — 2026-07-29

Reviewed 2026-07-29 on branch `m41-codec-arg-front-door-guards`, PR
[#43](https://github.com/jmgirard/tidymedia/pull/43). `origin/master` had not
moved since the branch was cut (`git merge-base --is-ancestor` confirms), so no
merge was needed and all evidence below is fresh against the merge base.

All AC1–AC5 evidence comes from re-running `data-raw/codec-guard-baseline.R`
against `origin/master` and the branch in one session: 34 verb/argument pairs ×
5 scenarios = 170 observations per side.

### Acceptance criteria

- **AC1 — measured.** On the branch, `normalize_audio_batch(jobs, audio_codec = NA)`
  at default `two_pass = FALSE` aborts with "`audio_codec` must be a single string or `NULL`, not `NA`." — names `audio_codec`, carries no `In index:`.
  Against the pre-fix tree the script reconstructs from `origin/master`, the same
  call **compiled** `-y -i "<in>" -af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy
  "<out>"`, byte-identical to the `audio_codec = NULL` call (`identical()` TRUE)
  and carrying no `-codec:a`. The regression test was also shown red on the
  pre-fix tree during implementation, failing as `Expected ... to throw a error`.
- **AC2 — measured, 0 violations.** Over all 34 pairs × the three non-string
  shapes (`NA`, `1`, `c("aac","mp3")`) = 102 observations: every one aborts, every
  message names that verb's own `video_codec`/`audio_codec`, none matches
  "`video` must be" or "`audio` must be" (the Layer-1 leak), and every
  `conditionCall()` deparses to the Layer-2 verb. `verify_media()` excluded per
  the criterion.
- **AC3 — measured, 0 violations.** No abort in those same 102 observations
  carries `In index:` at explicit `parallel = FALSE`, on the same conditions AC2
  inspects.
- **AC4 — FAILS as written.** The `default`/`null` comparison passed (zero rows
  differ), but the criterion's headline clause is "The guards add no new
  rejection", and finding F2 below demonstrates four. Re-measured directly:
  with a jobs table carrying a matching codec column and the scalar argument
  `= NA`, `standardize_video_batch`, `anonymize_video_batch`,
  `extract_audio_batch` and `normalize_audio_batch` all **compiled on
  `origin/master`** and **abort on the branch**. `pick()` lets a column override
  the scalar, so the scalar was dead weight and a bad value there was ignored.
  T2's script builds every call template without a codec column, so the AC4
  evidence was structurally blind to the one place the contract moved. Not
  reinterpreted to pass: the criterion says what it says.
  Original passing sub-measurement, retained: comparing the two baselines, zero
  `default` or `null` rows differ. Exactly 21 rows changed in total, all on the
  three non-string scenarios (7 each), across exactly the 7 repaired pairs. The
  two pairs the criterion names as NULL-aborting today —
  `anonymize_video_batch` `video_codec` and `extract_audio` `audio_codec` — abort
  before and after with byte-identical messages.
- **AC5 — measured.** `extract_audio_batch(audio_codec = NULL)` compiles
  `-y -i "<in>" -vn "<out>"`: `-vn` present, no `-codec:a`. `extract_audio(audio_codec = NULL)`
  still aborts. The guard at [ffmpeg.R:3347](../../R/ffmpeg.R#L3347) passes
  `allow_null = TRUE` and its comment states the scalar/batch disagreement
  explicitly and routes it to M42 and D021.
- **AC6 — measured.** `devtools::check()` re-run at review on the exact review
  tree: `Status: OK`, **0 errors / 0 warnings / 0 notes** (3m 4s). `devtools::test()`
  0 FAIL / 0 WARN / 15 SKIP / 2162 PASS; `check()` runs the same suite via
  `testthat.R` and it passed inside the clean check.

### Consistency gate

`cairn_validate` exit 0 — all 16 CHECKs PASS (including `coverage complete`,
`weight caps`, `mirror agreement`) and all 8 advisories OK. No `DESIGN.md`
principle changed, so `cairn_impact` was skipped as a clean no-op.

Toolchain gate (r-package profile): `devtools::document()` no diff ·
`pkgdown::check_pkgdown()` "No problems found" · `NAMESPACE`, `man/`, `data/`,
`_pkgdown.yml` untouched (0 files, so no generated-file drift and no new export
owing a reference-index row) · `README.Rmd`/`README.md` untouched · `NEWS.md`
carries the entry · new top-level `data-raw/` has its `^data-raw$`
`.Rbuildignore` entry.

Returns to `in-progress` for this milestone: **0**. No thrash trigger.

### Diffstat

9 files, +753/−13. `R/ffmpeg.R` +52/−0 — pure additions, which is
contract-neutrality visible in the diff shape.

### Independent review — three lenses, then a scorer

Three fresh-context reviewers with distinct evidence bases (the diff; `git blame`
history; the prior-review record), then a separate Sonnet scorer that did not
generate the findings. 19 findings scored; **5 at 80+ are actioned**, 14 below 80
are logged here rather than dropped (IP3).

The prior-review lens reported **zero** regressions: its
`gh api .../pulls/comments` probe returned `[]`, so it worked from archived
`## Review` sections, and it independently re-derived the mutation claim by hand.
The blame lens confirmed D019's analysis-before-refusal ordering is preserved,
D021's `convert_audio` NULL semantics intact, no prior guard attempt ever
reverted, and — answering the coverage worry T9 raised — **no CI coverage was
lost**: macOS/Windows runners install neither ffmpeg nor mediainfo (so those two
tests already skipped there), and the Linux runner installs both together (so
they still run real assertions).

**Actioned (score ≥ 80):**

- **F2 (95) — new rejection on four batch verbs; AC4 fails.** See AC4 above.
  Verified by the orchestrator, not taken on report. Probably a *desirable*
  change — it mirrors the M37-review fix recorded at
  [ffmpeg.R:2827](../../R/ffmpeg.R#L2827) — but it contradicts Scope's
  contract-neutrality claim and NEWS's "which values are accepted is unchanged",
  so it must be either reverted or adopted deliberately via amendment.
- **F8 (87) — the parameterized sweep is not fail-soft.**
  [test-codec-arg-front-door.R:150](../../tests/testthat/test-codec-arg-front-door.R#L150):
  no `next` after the abort assertion, so a non-aborting guard sends
  `conditionMessage(NULL)` on to throw and kill the whole `test_that`, silently
  losing coverage of every later verb. The mutation sweep blanked one guard at a
  time and so never saw it.
- **F3 (82) — a guard advertising a value its own verb refuses.**
  `anonymize_video_batch(video_codec = NA)` says "must be a single string or
  `NULL`" while `NULL` aborts six lines later in `anonymize_pipeline()`. Its
  scalar sibling says "must be a single string". Verified.
- **F13 (82) — unchecked `git show` in the instrument's imports bootstrap.**
  [codec-guard-baseline.R:53](../../data-raw/codec-guard-baseline.R#L53) omits the
  `attr(, "status")` check the same file's other git calls make, so a failed
  NAMESPACE fetch yields an empty imports env and the "could not find function"
  masquerade the file's own header warns about.
- **F19 (80) — a code comment citing a claim D021 does not make.**
  [ffmpeg.R:3345](../../R/ffmpeg.R#L3345) says D021 records the scalar/batch NULL
  disagreement. D021 instead asserts `extract_audio` "accepts neither `NULL` nor
  `NA`", and never mentions `extract_audio_batch` (0 matches). Measured:
  `extract_audio_batch(audio_codec = NULL)` compiled on `origin/master`. So D021
  itself carries a false claim about the batch verb — and DECISIONS.md is history
  under IP4, so that is superseded by a new entry, never edited.

**Logged, not actioned (score < 80):** F1 (62) NEWS overclaims "every" argument —
token-invalid *strings* still leak, verified identical on both refs, but AC2/AC3
scope to non-strings so the code meets its criteria · F7 (68) the probe grid never
varies codec column / `two_pass` / `hardware` / `reencode` / token-invalid
strings, which is why F1 and F2 escaped it · F6 (78) `codec_guard_diff()` drops
rows present in `before` but absent from `after` · F12 (60) vestigial
`nrow(b)` guard · F4 (55) NEWS entry landed in the oldest `## Bug fixes` section
of a newest-first block · F15 (55) guard-vs-`jobs` precedence inconsistent across
the four batch verbs · F14 (48) `tryCatch(condition =)` would record a
pre-compile message instead of the command · F9 (40) anti-leak assertion keyed to
one exact phrasing · F10 (40) `expect_match(msg, arg)` asserts the string
appears, not that the argument is named · F5 (30) undocumented NULL contract in
error text — deliberate per Scope · F11 (30) non-injective diff key · F16 (22)
vestigial `skip_if_no_ffprobe()` · F17 (12) DESCRIPTION under-declares testthat
(pre-existing) · F18 (8) stale.

### Disposition

**Sent back to `in-progress`.** AC4 fails as written (F2). Criteria are not
reinterpreted at review, so the fix is a gated amendment deciding whether the
four batch verbs' new rejection is adopted (amend Scope + AC4, correct NEWS) or
reverted, then a re-review. Returns for this milestone: **1**.
