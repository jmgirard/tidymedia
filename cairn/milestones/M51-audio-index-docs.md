# M51: Make the package's two 0-based audio indices legible

- **Status:** review
- **Branch:** `m51-audio-index-docs`
- **PR:** https://github.com/jmgirard/tidymedia/pull/54
- **Priority:** normal
- **Depends on:** M49, M50
- **Driving RR:** —
- **Principles touched:** —

## Goal

Give a reader who meets both `audio_stream` and `audio` one documented place
that says what each one counts, and cross-link the two families whose help
pages are currently disjoint.

## Scope

**In:** a user-facing concept topic covering the two counting bases, the two
`NULL` readings, the difference between an `NA` cell and an absent column, and
`audio`'s three meanings in the exported API; a `@family` grouping the
audio-track-selection verbs plus `@seealso` links across the two families;
routing the shared `@param audio_stream` sentences through one source so the
sibling-verb enumeration cannot go stale (M49's Out); vignette coverage for
`audio_stream` and a correction to `vignettes/tidymedia.Rmd:141`; the
`_pkgdown.yml` entry; `inst/WORDLIST`; a D-entry re-confirming D023's two-name
call at the new count.

**Out:** renaming either argument, or unifying the two `NULL` readings — either
needs its own milestone under D014's pre-0.2.0 clean break, and D025/D026 both
name a caller-confusion report as the falsifier that would reopen it → new
candidate row. Adding a `?tidymedia-package` topic (`R/tidymedia-package.R` has
no `_PACKAGE` sentinel) → new candidate row. Any behavior change.

## Acceptance criteria

- [x] AC1 A documentation-only topic exists, built from a roxygen `@name` block
      in the `R/utils-tidy-eval.R:1-15` shape but **without** its `@keywords
      internal` (`:11`), and it states in prose: that `audio_stream` counts one
      input's audio streams while `audio` counts a verb's inputs; which verb
      families read `audio_stream = NULL` as the first track and which as every
      track; that `audio = NULL` emits no map at all and so drops audio
      (`R/ffmpeg.R:5190`, `:5335`), unlike `audio_stream = NULL`; that an `NA`
      cell in a `_batch` override column means that row's `NULL` sentinel while
      an absent column means the scalar argument applies; and that `audio` also
      names a codec on `ffm_codec()` and a logical on `ffm_copy()`.
- [x] AC2 The topic is reachable from the reference index: it has a
      `_pkgdown.yml` entry and `pkgdown::check_pkgdown()` still reports no
      problems, as it does today.
- [x] AC3 Every exported verb carrying `audio_stream` (eighteen after M49) and
      every exported verb carrying `audio` as an input index
      (`compare_videos()`, `picture_in_picture()`, and their `_batch`
      siblings) links to the new topic. Asserted by a test that enumerates the
      parameter across `man/*.Rd` and fails when a topic carrying it lacks the
      link — so a future verb that gains the argument and not the link is
      caught.
- [x] AC4 The sentences naming the every-track and first-track families exist
      in exactly one place in `R/`, with the eighteen `@param audio_stream`
      blocks inheriting them rather than restating them; the rendered `.Rd`
      files still each carry the text. A stale enumeration is then
      unrepresentable rather than merely detected.
- [x] AC5 `audio_stream` appears in at least one vignette with a runnable
      chunk. `vignettes/tidymedia.Rmd:141` currently reads "Each manages its
      own stream labels internally, so audio is dropped unless you map it back"
      — the fix is that a reader cannot carry that sentence to the
      pass-through verbs, whether by scoping its wording or by adding the
      contrast; the vignette names at least one pass-through verb as behaving
      differently.
- [x] AC6 `compare_videos_batch()` and `picture_in_picture_batch()` state what
      `audio` means rather than only deferring to the scalar verb
      (`R/ffmpeg.R:5350`, `:5471`), and `extract_audio_batch()` /
      `convert_audio_batch()` carry the "FFmpeg error, not an R one" sentence
      their scalar siblings have and they lack (`R/ffmpeg.R:4183`, `:4305`
      against `:490`, `:974`).
- [x] AC7 A `cairn/DECISIONS.md` entry re-confirms or revises D023's first
      bullet — two names, two bases — now that the argument spans eighteen
      verbs and two `NULL` readings in three spellings (`0:a:0`, `0:a:0?`,
      `0:a?`), and records what would reopen it.
- [x] AC8 `devtools::document()` produces no diff, `devtools::test()` clean,
      `devtools::check()` reports 0 errors / 0 warnings including the spelling
      NOTE (`inst/WORDLIST` updated via `spelling::update_wordlist()`), and
      NEWS carries a documentation entry.

## Coverage

- AC1 → T2
- AC2 → T2, T5
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T6
- AC6 → T3
- AC7 → T7
- AC8 → T5, T7

## Tasks

- [x] T1 Write the enumerating test first (AC3): walk `man/*.Rd` for
      `audio_stream` and for `audio`-as-input-index, and assert each topic
      carries the cross-link. It must go red before the docs change.
- [x] T2 Author the concept topic and register it in `_pkgdown.yml`.
- [x] T3 Add the `@family` tag and `@seealso` links across all twenty-two
      affected verbs; fix the two batch `@param audio` blocks and the two
      missing "FFmpeg error" sentences.
- [x] T4 Introduce the shared `@param audio_stream` source — an
      `@inheritParams` donor or a `man-roxygen` template, neither of which the
      package has today — and collapse the eighteen copies onto it, keeping the
      per-family differences that are genuinely per-family.
- [x] T5 `devtools::document()`; `pkgdown::check_pkgdown()`;
      `spelling::update_wordlist()`.
- [x] T6 Vignette chunk for `audio_stream` and the `vignettes/tidymedia.Rmd:141`
      correction; re-knit any affected vignette.
- [x] T7 Append the D-entry, write NEWS, add the two candidate rows named in
      Out, and run the profile's verify slot plus `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose one shared `@param audio_stream` source over keeping eighteen hand-written copies guarded by a drift test, because the test encodes the family lists a second time and catches drift only at review; falsified by the shared source proving unable to express a per-family difference without a conditional, at which point the copies plus the guard return.
- 2026-07-31: sequenced after M50 so the concept topic and the vignette describe the quoted specifier form rather than needing a second pass.
- 2026-07-31: implementation gate settled three open choices, all as recommended: the shared `@param audio_stream` source is an internal R function whose result roxygen inserts via an inline `` `r ` `` call (probed in a scratch package first — the result is inserted verbatim, so the existing `\code{\link{}}` register survives, and the family lists become directly unit-testable); the concept topic is named `audio_stream` so `?audio_stream` reaches it; the `@family` grouping stays as scoped.
- 2026-07-31: criteria audit ([O], fresh context) returned three findings: AC4 offered two incompatible end-states, one of which made AC3's assertion vacuous — resolved at the gate in favour of the shared source; AC7 said "three `NULL` readings" where D026 and this file's own AC1 say two (in three spellings); and the Coverage table orphaned T1, the task writing the very test AC3 relies on. All fixed above. The audit confirmed the eighteen-verb arithmetic (fourteen `.Rd` files carry `audio_stream` today, plus M49's four) and every other citation in the file.

- 2026-07-31: T1 done — `tests/testthat/test-audio-index-docs.R` enumerates `\item{}` parameter names across `man/*.Rd` and demands a link to the concept topic; red before the docs change (18 `audio_stream` topics and all 6 `audio` topics unlinked). It covers `ffm_codec()`/`ffm_copy()`'s non-index `audio` too rather than allowlisting them, so there is no second list to keep in step; it skips where `man/` is absent (a built tarball).
- 2026-07-31: T2 done — `R/audio-stream-doc.R` carries the `audio_stream` topic (aliases `audio-tracks`, `audio_indices`, no `@keywords internal`) plus the shared-text functions T4 will consume; `_pkgdown.yml` gains a "Concepts" section and `pkgdown::check_pkgdown()` still reports no problems. The topic's own family sentence is generated from the same vectors the `@param` blocks will use, so the hub cannot drift from the spokes either.
- 2026-07-31: minor amendment — AC1's two source cites for `audio = NULL` emitting no map were stale (`R/ffmpeg.R:5028` is now the codec-column reshape). Verified and corrected to `R/ffmpeg.R:5190` and `:5335`, the two `if (!is.null(audio))` map sites. Criterion substance unchanged. Also confirmed while there that an out-of-range `audio` is an R error (`:5271`, `:5419` bound it with `max =`), which the topic now states.
- 2026-07-31: T4 done (taken before T3, a minor reorder: T3 edits the same eighteen blocks T4 rewrites wholesale, so doing T4 first avoided editing text about to be replaced). `audio_stream_param()` in `R/audio-stream-doc.R` now generates all eighteen `@param audio_stream` blocks through an inline `r` call; each verb supplies only its own action verb, family, batch flag and caveat. Net -167/+18 lines in `R/ffmpeg.R`, and the four blocks carrying stale sibling lists are now correct by construction. The scripted edit preserved `R/ffmpeg.R`'s CRLF endings deliberately (M49 review H1) and asserted 18 blocks matched before writing. Suite: 3237 pass, the one remaining failure is T3's `audio` half.
- 2026-07-31: T3 done — `@family audio selection functions` on all twenty-two verbs; `compare_videos()`/`picture_in_picture()` and both `_batch` siblings now generate `@param audio` from a shared `audio_input_param()`, which is also where the batch verbs stop merely deferring to the scalar verb (AC6); `ffm_codec()`/`ffm_copy()` gain a pointer to the concept topic so all four meanings of `audio` reach it. AC6's other half needed no separate edit — the shared `audio_stream` text carries the "FFmpeg error, not an R one" sentence, so `extract_audio_batch()`/`convert_audio_batch()` now have it. Suite 3238 pass / 0 fail; the cross-link test is green in both halves.
- 2026-07-31: T5 done — `devtools::document()` produces no diff, `pkgdown::check_pkgdown()` reports no problems (AC2), and `spelling::update_wordlist()` added the two new words the concept topic introduces ("arity", "unselected"); the package now spell-checks clean.
- 2026-07-31: T6 done — `vignettes/tidymedia.Rmd` gains a "Choosing an audio track" subsection with two runnable `run = FALSE` chunks (both render M50's quoted `-map "0:a:1"`), stating the two families' defaults and pointing at `?audio_stream`. The line-141 sentence is now scoped to the multi-input builders and carries the contrast, so it can no longer be read as a claim about the pass-through verbs. Knitted locally to confirm both chunks execute; spell-check still clean.
- 2026-07-31: T7 done — D032 appended (re-confirms D023's first bullet at eighteen verbs, records what shipped instead of a rename, and keeps D025/D026's caller-confusion falsifier); NEWS gains two Documentation bullets; the two candidate rows named in Out are on the ROADMAP. `devtools::check()` reports 0 errors / 0 warnings / 0 notes, vignettes re-build clean, and the spelling test passes. Weight caps clear: ROADMAP 51 lines, plan-owned body 113.
- 2026-07-31: all tasks done; status review. Note for the reviewer: AC6's four `R/ffmpeg.R` line cites (`:5350`, `:5471`, `:4183`, `:4305`) are pre-M51 positions and have shifted, as AC1's had — the blocks they name are now generated by `audio_input_param()` and `audio_stream_param()` in `R/audio-stream-doc.R`. The new doc test skips under `R CMD check`, where `man/` is absent from the built tarball; it runs under `devtools::test()`.
- 2026-07-31: review round 1 — three fresh-context reviewers plus a scorer. Blame-history and prior-PR-comments returned no findings; the diff-bug lens returned 23, of which 3 scored at or above 80 (F1 90, F3 82, F5 85) and were fixed, plus 14 sub-threshold ones fixed anyway because they were verified-false sentences in prose this milestone authored, and 6 logged. Two were corrections to D032 itself, which is unmerged draft rather than history. Post-fix: 3242 pass / 0 fail, `check()` 0/0/0, `cairn_validate` exit 0.

## Decisions

- **M51-D1 (2026-07-31) — the shared `@param` source is an R function, evaluated
  by roxygen at `document()` time.** The plan offered an `@inheritParams` donor
  or a `man-roxygen/` template; a third option won at the gate. `R/audio-stream-doc.R`
  holds the two verb vectors and builds each block's text from them, and every
  block reaches it through an inline `` `r audio_stream_param(...)` `` call.
  Rules out `@inheritParams`, which copies a whole block, so expressing the two
  `NULL` readings needs two donors and writes the family lists twice — the exact
  duplication AC4 exists to remove. Rules out `man-roxygen/`, which works but
  puts the text outside `R/` in brew syntax and leaves it checkable only by
  reading rendered `.Rd`. Probed in a scratch package before committing to it:
  roxygen inserts the result verbatim, so the package's existing
  `\code{\link{}}` register survives untouched, and the family vectors stay
  ordinary R data a unit test can read directly. The cost is that `document()`
  now loads the package to render these blocks, which it already did.

## Review

Fresh evidence, 2026-07-31, on `m51-audio-index-docs` at PR #54.

- **AC1** — `man/audio_stream.Rd` exists, generated from the `@name audio_stream`
  block in `R/audio-stream-doc.R`; `grep -c 'keyword{internal}'` returns 0, so it
  keeps the `R/utils-tidy-eval.R` shape without that tag. Its four rendered
  `\section{}` blocks are "The two indices", "What `NULL` means, and it is not the
  same thing", "In a `_batch` jobs table" and "`audio` names three things", which
  carry all five required statements: the two counting bases; the two `NULL`
  family readings; that `audio = NULL` emits no map and so drops audio, unlike
  `audio_stream = NULL`; the `NA`-cell-versus-absent-column rule; and `audio`'s
  codec and logical meanings on `ffm_codec()` / `ffm_copy()`.
- **AC2** — `_pkgdown.yml` gains a "Concepts" section listing `audio_stream`;
  `pkgdown::check_pkgdown()` re-run at review reports "No problems found", as it
  did before the change.
- **AC3** — 18 `man/*.Rd` files document `audio_stream` and all 18 link to the
  topic; 6 document `audio` and all 6 link to it (the four input-index verbs plus
  `ffm_codec()` / `ffm_copy()`, which the test covers rather than allowlists).
  The guard is load-bearing, verified by mutation at review: replacing
  `\link{audio_stream}` with `\code{audio_stream}` in `man/crop_video.Rd` turned
  the suite red naming exactly that file; restored, green again.
- **AC4** — the family-naming sentences are generated by
  `audio_stream_family_sentence()` from the two vectors in `audio_stream_families`,
  and `grep -rn 'extraction family|pass-through family' R/` outside
  `R/audio-stream-doc.R` returns one hit, an internal code comment at
  `R/ffmpeg.R:2865` that enumerates no verbs. 19 rendered `.Rd` files carry the
  sentence (the 18 verbs plus the topic itself), so the text is present
  everywhere and authored once. Re-verified after the review fixes, which
  renamed the two families to "first-track" / "every-track" and also routed the
  topic's `@seealso` through `rd_verb_list()` — so the one remaining
  hand-written verb list in `R/` is gone too (finding F15).
- **AC5** — `vignettes/tidymedia.Rmd` gains a "Choosing an audio track" section
  with two runnable `run = FALSE` chunks calling `extract_audio()` and
  `crop_video()` with `audio_stream = 1`; knitted at implementation, both emit
  M50's quoted `-map "0:a:1"`, and `R CMD check`'s vignette re-build passes at
  review. The `:141` sentence now reads "so *these multi-input builders* drop
  audio unless you map it back ... the single-input pass-through verbs above are
  the opposite, keeping every audio track by default" — scoped to the builders
  and naming the pass-through verbs as behaving differently. Both chunks were
  re-worked at review (findings F5, F14, F16): the section no longer claims
  every audio-touching verb takes `audio_stream`, the second chunk now shows the
  every-track default it illustrates rather than a named track, and the
  hand-copied family lists are replaced by a pointer to the generated topic.
- **AC6** — `man/compare_videos_batch.Rd` and `man/picture_in_picture_batch.Rd`
  each now carry their own `\item{audio}` stating the input-index meaning, the
  silent-output default and the per-row `NA` rule, rather than only deferring to
  the scalar verb. `man/extract_audio_batch.Rd` and `man/convert_audio_batch.Rd`
  both now contain "FFmpeg error, not an R one", inherited from the shared
  `audio_stream_param()` closing sentence.
- **AC7** — `cairn/DECISIONS.md:1089` holds D032, which quotes D023's first
  bullet verbatim, re-confirms it at eighteen verbs, records the documentation
  answer shipped in place of a rename, and states the falsifier (a caller-confusion
  report, per D025/D026, under D014's pre-0.2.0 clean break) while ruling out the
  argument count alone as a trigger. Corrected at review before merge (findings
  F1, F3): the entry named a `0:a:0?` map spelling that exists nowhere in `R/`
  and omitted the real bare `0:a`, and its "verbatim" D023 quote had dropped that
  bullet's closing rules-out sentence. Both fixed in place — D032 is unmerged
  draft authored by this milestone, not history to be superseded.
- **AC8** — re-run after the review fixes: `devtools::document()` leaves the tree
  clean apart from the milestone file; `devtools::test()` 3242 pass / 0 fail / 5 skip;
  `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes, with the
  spelling test passing after `spelling::update_wordlist()` added "arity" and
  "unselected" (`inst/WORDLIST` +2). NEWS.md gains two Documentation bullets
  (+14 lines).

### Consistency gate

- `cairn_validate.py` exit 0 — all 16 CHECKs pass. One advisory: `sizing (split
  tripwires)` warns that M51 has 8 acceptance criteria against a 7 tripwire.
  Noted, not actioned: the milestone is complete and shipped as one reviewable
  documentation PR, so a retrospective split would create tracking work with no
  reviewable output.
- No `DESIGN.md` principle changed, so `cairn_impact.py` is not run.
- Profile `consistency-gate` slot (r-package): `document()` no diff; generated
  `man/` and `NAMESPACE` regenerate from roxygen (the no-diff run proves it);
  `README.Rmd` untouched so `README.md` stays in sync; `pkgdown::check_pkgdown()`
  clean; `NEWS.md` carries this milestone's user-visible changes with no milestone
  numbers; no new top-level files, so no `.Rbuildignore` entry is owed; full
  `devtools::check()` clean.
- CI on PR #54: all five jobs green (ubuntu release/devel/oldrel-1, windows,
  test-coverage) on the pre-fix commit; re-run after the review fixes.

### Independent review

Three fresh-context reviewers, then a fresh scorer that generated none of the
findings and was given the diff and the plan.

- **[S] blame-history**: no findings. Checked each of the 167 deleted prose
  lines against the generated replacement and reported every fact reproduced,
  no D-entry contradicted, no fixed bug resurrected.
- **[S] prior-PR-comments**: no findings. Confirmed the diff does not repeat
  M49's CRLF-normalisation regression (`R/ffmpeg.R` stays CRLF, 47+/175-, not a
  whole-file rewrite), does not repeat M50's miscount pattern, and leaves M50's
  quoting call sites untouched. GitHub PR-comment probe returned empty, so that
  surface was skipped.
- **[O] diff-bug**: 23 findings. It independently reproduced the mechanism in a
  scratch copy — `roxygenise()` regenerates `man/` and `NAMESPACE`
  byte-identically, and the new guard is load-bearing — and confirmed every
  changed line in `R/ffmpeg.R` and `R/ffm.R` is a roxygen comment, so the
  documentation-only scope holds.

**Actioned (scored at or above 80), all fixed on the branch:**

- **F1 (90)** — D032 named a map spelling that does not exist (`0:a:0?`) and
  omitted the real bare `0:a`. The entry now enumerates the four spellings that
  do exist with their source lines, and states why a *named* track carries no
  `?`. Verified by grepping every map literal in `R/`.
- **F3 (82)** — D032's "verbatim" quote of D023's first bullet had dropped that
  bullet's closing rules-out sentence. Restored in full.
- **F5 (85)** — the new vignette sentence "Every verb that touches audio takes
  the same `audio_stream` argument" is false for `compare_videos()` /
  `picture_in_picture()`, which take `audio` instead. Rescoped to single-input
  verbs that select a track.

**Also fixed, below the 80 threshold (verified false statements in prose this
milestone itself authored — a documentation milestone shipping known-wrong
sentences was not defensible, and each was a one-line fix):**

- **F4 (78)** — the topic called `compare_videos()`/`picture_in_picture()` "the
  verbs that take more than one file"; `concatenate_videos()` does too. Reworded
  to what actually distinguishes them: they combine inputs and must choose whose
  sound to keep.
- **F6 (78)** — "`audio_stream = NULL` always maps something" is false on the
  pass-through verbs, whose `0:a?` deliberately matches nothing on a silent
  input. Reworded, and the difference between the video-passing verbs and the
  audio-producing ones is now stated outright.
- **F7 (75)** — the generated sentence labelled `separate_audio_video()` part of
  "the pass-through family", which its own code comments deny (`0:a`, no `?`,
  aborts where a true pass-through verb exits 0). Families renamed to
  "first-track" / "every-track", which is what they actually share, and the
  abort difference is now documented on that verb's block.
- **F8 (78)** — `normalize_audio_batch()` lost two substantive sentences in the
  collapse: the derived-output-extension warning and the no-audio-input error.
  Both restored into the shared normalize caveat, so both verbs now carry them.
- **F9 (65)** — `separate_audio_video()` lost the `probe_audio()` index-column
  disambiguation from its own page. Restored to that verb's caveat.
- **F10 (62)** — the `ffm_codec()`/`ffm_copy()` cross-links rendered as run-on
  sentences with no separator. Repunctuated, with "(default = ...)" back in the
  package's usual position.
- **F11 (60) + F12 (50)** — the guard skipped under `R CMD check`, where `man/`
  is absent, so the drift it exists to catch would have been caught by nobody in
  CI; and its `../../../man` fallback could bind to the wrong `man/`. The test
  now reads `tools::Rd_db()` when the source tree is absent and accepts only
  `../../man` otherwise. Verified: `Rd_db()` on the freshly installed package
  returns the same 18 and 6 topic counts, and `R CMD check` now reports 3242
  pass / 5 skip — identical to `devtools::test()`, so the four doc tests ran
  under check rather than skipping.
- **F13 (30)** — an emptied family would have silently deleted the sentence from
  all 18 blocks. `rd_verb_list()` now refuses a family under two members, with a
  test pinning it.
- **F14 (72)** — the vignette chunk illustrating "keeps every track" passed
  `audio_stream = 1`, showing the opposite. Now shows the default.
- **F15 (52)** — the topic's `@seealso` hand-wrote a second, partial family
  list. Now generated from the same vectors.
- **F16 (52)** — the vignette hand-copied both family lists. Replaced by a
  pointer to the topic.
- **F19 (60)** — "Every `_batch` verb takes the same argument as a scalar
  default and also accepts a per-row column" over-generalises to `hardware`,
  `parallel` and `two_pass`, which read no column. Scoped, with the exceptions
  named.
- **F23 (40)** — the topic carried no `@family`, so it was absent from the 22
  verbs' family lists. Added.

**Logged, not actioned (6):**

- **F2 (15)** — AC7's own "three spellings" wording carries F1's error, but it is
  plan text this diff did not introduce; D032 is corrected and AC7's wording is
  plan-owned, amendable only via gate. Recorded here so the next reader of AC7
  is not misled.
- **F17 (35)** — generated blocks link to their own topic (a verb appears in its
  own family list). Cosmetic, and the cost of generating the list from one
  source.
- **F18 (25)** — em-dash in the untouched roxygen versus `--` in the new file, so
  one page shows two dash conventions. Pure style.
- **F20 (40)** — only `compare_videos_batch()` carries the per-row validation
  sentence. Justified: its input count varies per row where
  `picture_in_picture_batch()` is fixed at two.
- **F21 (30)** — the test covers `ffm_codec()`/`ffm_copy()` beyond AC3's letter,
  so a future Layer-1 `audio` argument must carry the link. Deliberate and
  logged at implementation; the alternative is an allowlist that rots.
- **F22 (22)** — the two aliases are not cross-linked anywhere. Normal R alias
  behaviour; `?audio-tracks` works regardless.
