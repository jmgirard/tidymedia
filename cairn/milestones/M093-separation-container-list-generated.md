<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M093: The separation help pages render their container list from the vector

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m093-separation-container-list-generated`

## Goal

Both separation help pages take their nine-container enumeration and its count
word from `multi_audio_extensions` itself, so the prose cannot disagree with the
vector it describes.

## Scope

Surface tier: **user-facing** — the deliverable is the text a reader sees on
`?separate_audio_video` and `?separate_audio_video_batch`.

**In:** two renderers beside `multi_audio_extensions` (`R/ffmpeg.R:658`) — one
emitting the Rd enumeration, one the length as an English word — and the four
hand-written sites they replace: the enumeration and count word in the scalar
help page (`R/ffmpeg.R:1023-1024`, `:1029`) and in the batch one
(`R/ffmpeg.R:6058-6060`, `:6065`, `:6070`). The source comment at
`R/ffmpeg.R:649` ("Each of the nine above says capacity in its own words") is
reworded off its count in the same pass; it is a comment, so it cannot be
generated, and D071 relies on it as readable evidence. The mechanism is the one
`R/audio-stream-doc.R` already uses — R data plus an inline `` `r fn()` ``
roxygen call — whose header states the reason: it makes a stale enumeration
unrepresentable (M51, after M48 left three blocks naming the wrong family).

**Out:** `NEWS.md`'s enumerations (`:563-564` among others) stay hand-written —
a historical record states what was true at that release, not what is true now.
The two-container *exemplars* — `audio_stream_extras$separation_container`
(`R/audio-stream-doc.R:133-141`) and the cli messages at `R/ffmpeg.R:745` and
`:859` — also stay hand-written: an exemplar illustrates, it does not claim the
whole list, so only a *removal* from the vector could falsify one, and binding
them would make a cli abort's wording depend on a vector order nothing
guarantees. Promotable on that removal → ROADMAP candidate row.

## Acceptance criteria

- [ ] AC1. `R/ffmpeg.R` carries two renderers beside `multi_audio_extensions`,
      each taking the vector as its sole argument and defaulting to it. Given a
      3-member stand-in, the first returns
      `\code{.a}, \code{.b} and \code{.c}` — items comma-joined, one `and`
      before the last — and the second returns `three`. Each refuses a vector
      under 2 members the way `rd_verb_list()` does, so an emptied vector cannot
      vanish silently from the rendered text; and the word renderer aborts on any
      length it cannot name rather than returning `NA` or `character(0)`, probed
      at one length past its last nameable value.
- [ ] AC2. Both roxygen blocks call those renderers with no argument, shown by
      two mutations of the vector, each followed by `devtools::document()` and
      then reverted:
      **grow** — appending `"xyz"` makes `man/separate_audio_video.Rd` and
      `man/separate_audio_video_batch.Rd` each name `.xyz` in its enumeration,
      and leaves **no** occurrence of `nine` in either file, with each file's
      `ten` count equal to its committed `nine` count (2 apiece);
      **shrink** — dropping `"ts"` removes `\code{.ts}` from both enumerations
      and leaves no occurrence of `nine`, with each file's `eight` count equal
      to that same committed count.
      After both reverts, `devtools::document()` restores the two files
      byte-identical to their committed state. Evidence: the four diffs.
- [ ] AC3. Every Rd topic naming a member of `multi_audio_extensions` — the
      topics a search of the whole Rd corpus (`rd_sources()`, whitespace
      collapsed) for the token `\code{.opus}` returns, of which there are at
      least 2 — contains, verbatim, the string AC1's enumeration renderer
      returns for the committed vector. No topic in that corpus carries the
      enumeration's surrounding marker clause without also containing that
      string.
- [ ] AC4. `Rscript -e 'devtools::test()'` clean; `devtools::document()` leaves
      no uncommitted diff; `Rscript -e 'devtools::check()'` clean (0 errors,
      0 warnings; NOTEs justified).

## Coverage

- AC1 → T1
- AC2 → T2, T4
- AC3 → T2, T3
- AC4 → T4

## Tasks

- [x] T1. Add `multi_audio_rd_list()` and `multi_audio_rd_count()` beside
      `multi_audio_extensions` (`R/ffmpeg.R:658`), each `function(exts =
      multi_audio_extensions)`, following `rd_verb_list()`
      (`R/audio-stream-doc.R:28`) for the join and the under-2 refusal. Unit-test
      both on stand-in vectors: the 3-member form, the under-2 refusal, and the
      word renderer one past its last nameable length.
- [x] T2. Replace the enumeration and both count words in each block with
      inline `` `r ` `` calls (`R/ffmpeg.R:1023-1024`, `:1029`; `:6058-6060`,
      `:6065`, `:6070`); reword the `:649` comment off its count. Re-wrap so no
      marker clause AC3 matches on is split across roxygen lines — it is today
      (`:1024-1025`), which is why a grep for it currently returns one file, not
      two. Run `devtools::document()`.
- [x] T3. Add `tests/testthat/test-separation-container-docs.R`: enumerate
      topics via `rd_sources()` (`tests/testthat/helper-rd.R`) on the
      `\code{.opus}` token with whitespace collapsed, floor of 2, assert the
      renderer's returned string appears verbatim in each, plus the
      `test-nvenc-docs.R:56-65` converse. `skip_if` no Rd source.
- [ ] T4. Run AC2's grow and shrink mutations, capture the four diffs, confirm
      both reverts are byte-identical; then `devtools::test()`,
      `devtools::document()` (no diff) and `devtools::check()`.

## Work log

- 2026-08-30: created by /milestone-plan. Absorbs the 2026-08-30 ROADMAP candidate row from M091's review (F4 round 3, F1 round 4 — the vector grew twice inside M091 and a hand copy went stale on each occasion).
- 2026-08-30: plan gate chose renderers sited beside `multi_audio_extensions` in `R/ffmpeg.R` over a separate doc-source file on `R/audio-stream-doc.R`'s model, because the vector's own comment sites it as Layer-2 knowledge kept beside the separation helpers (IP1/D002) and splitting the renderer from its datum reintroduces the distance this milestone exists to close; falsified by a third consumer outside the separation helpers needing the same rendering, which would make the shared home the cheaper one.
- 2026-08-30: plan gate chose generating the count word over rewording the prose to carry no count, because "Those nine are an exclusion list and not a survey" tells a reader how much to expect and the count is the half that went stale twice; falsified by the word renderer needing a length vocabulary wider than a small lookup, which would make the reword cheaper than the table.
- 2026-08-30: plan gate chose leaving the two-container exemplars hand-written over binding them to the vector, because an exemplar makes no claim about the whole list and binding one would make a cli abort's wording depend on a vector order nothing guarantees; falsified by a removal from `multi_audio_extensions` leaving an exemplar naming a container the list no longer holds.
- 2026-08-30: plan gate chose shipping the Rd guard (AC3) over M51's renderer-tests-only precedent, because generation prevents drift only while the blocks stay wired to the renderers and a block reverted to hand-written prose would otherwise ship green; falsified by the guard reddening for Rd-shape reasons rather than for drift.
- 2026-08-30: criteria audit ran in **full** mode (declared tier: user-facing), [O] fresh-context reader, over the step-2 criteria. Returned 7 findings. Six fixed here and reported at the gate: AC1's argument contradiction; AC1's missing upper bound on the count renderer (could render `NA` into help); AC2 covering one of the two count-word occurrences per topic; AC3 asserting set equality over the wrong sentence (the marker sentence contains `.avi` and `.nut`, so it was unsatisfiable); AC3's marker grep returning one file not two on the current tree, because the clause is wrapped; AC3's domain resting on a recalled prose marker rather than a vector-derived token. The seventh — mutation-probe range — went to the gate and returned append+shrink; AC2's shrink half was re-run through the full audit's six questions before being written and passed all six.

- 2026-08-30: T1 — `multi_audio_rd_list()` and `multi_audio_rd_count()` added beside `multi_audio_extensions`; count vocabulary set to two–twelve at the implement gate (a length past it aborts `document()` naming the length). Unit tests in `tests/testthat/test-separation-container-docs.R`, 11 passing.

- 2026-08-30: T2 — both roxygen blocks now paste the enumeration and both count words in through inline `r` calls; the `:649` source comment reworded to "Each container above". `document()` rewrapped both Rd files with no change to the rendered wording. Suite: 8826 pass, 0 fail.

- 2026-08-30: T3 — Rd guard added: the two `\code{.opus}` topics must each carry the renderer's string verbatim (whitespace-collapsed), plus the converse over the shared marker clause, both with a floor of 2. Discrimination shown by planting a stale hand copy (`.ts` dropped from the batch block): both guards red, then reverted. A *faithful* hand copy passes — the property asserted is that the prose matches the vector, which is what AC3 promises.

## Decisions

## Review
