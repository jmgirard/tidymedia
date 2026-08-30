<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M093: The separation help pages render their container list from the vector

- **Status:** review
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

- [x] AC1. `R/ffmpeg.R` carries two renderers beside `multi_audio_extensions`,
      each taking the vector as its sole argument and defaulting to it. Given a
      3-member stand-in, the first returns
      `\code{.a}, \code{.b} and \code{.c}` — items comma-joined, one `and`
      before the last — and the second returns `three`. Each refuses a vector
      under 2 members the way `rd_verb_list()` does, so an emptied vector cannot
      vanish silently from the rendered text; and the word renderer aborts on any
      length it cannot name rather than returning `NA` or `character(0)`, probed
      at one length past its last nameable value.
- [x] AC2. Both roxygen blocks call those renderers with no argument, shown by
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
- [x] AC3. Every Rd topic naming a member of `multi_audio_extensions` — the
      topics a search of the whole Rd corpus (`rd_sources()`, whitespace
      collapsed) for the token `\code{.opus}` returns, of which there are at
      least 2 — contains, verbatim, the string AC1's enumeration renderer
      returns for the committed vector. No topic in that corpus carries the
      enumeration's surrounding marker clause without also containing that
      string.
- [x] AC4. `Rscript -e 'devtools::test()'` clean; `devtools::document()` leaves
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
- [x] T4. Run AC2's grow and shrink mutations, capture the four diffs, confirm
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

- 2026-08-30: T4 — mutation probes run. Grow (append `"xyz"`): each Rd file gains one `\code{.xyz}`, `nine` word-count 0, `ten` word-count 2 (committed `nine` count was 2 apiece). Shrink (drop `"ts"`): `\code{.ts}` count 0 in both, `nine` 0, `eight` 2 apiece. Both reverts restored the two files to their committed md5s with an empty `git diff man/`.
- 2026-08-30: T4 — final checks: `devtools::test()` 8830 pass / 0 fail / 12 warn / 5 skip; `devtools::document()` no diff; `devtools::check()` 0 errors, 0 warnings, 0 notes. Status → review. No `NEWS.md` entry: the rendered help text is unchanged (the Rd diff is line rewrapping only), so nothing user-visible changed.
- 2026-08-30: review — PR #97 opened; every criterion executed with fresh evidence and the consistency gate clean. Three-lens fan-out returned six minor findings, all from the diff lens: one fixed now (the count-vocabulary offset comment), one filed as a follow-up at hygiene, one logged as a task-text divergence, three rejected with reasons. No return.

## Decisions

## Review

**AC1 — verified 2026-08-30.** Both renderers exercised from a loaded package.
`formals()` gives each exactly one argument, `exts`, and calling with no
argument is `identical()` to calling with `multi_audio_extensions`. On the
3-member stand-in `c("a","b","c")` the list renderer returns
`\code{.a}, \code{.b} and \code{.c}` and the word renderer returns `three`.
Both refuse length 0 and length 1 (`length(exts) >= 2 is not TRUE` /
`n >= 2 is not TRUE`). The word renderer names length 12 (`twelve`, its last
nameable value) and at 13 aborts with
`multi_audio_rd_count() cannot name a length of 13; extend
multi_audio_count_words` — no `NA`, no `character(0)`.

**AC2 — verified 2026-08-30.** Committed baseline: `nine` word-count 2 in each
of the two Rd files, `ten` 0, `eight` 0; md5 `9714a5c9…` and `775a83e9…`.
*Grow* (append `"xyz"`, `document()`): each file gains one `\code{.xyz}` inside
its enumeration; `nine` 0 in both; `ten` 2 in both, equal to the committed
`nine` count. *Revert* (`document()`): both md5s restored, `git diff man/`
empty. *Shrink* (drop `"ts"`, `document()`): `\code{.ts}` 0 in both; `nine` 0
in both; `eight` 2 in both, again equal to the committed count. *Revert*: both
md5s restored, `git diff man/` empty. The four diffs show the enumeration and
both count-word occurrences per file moving together and nothing else changing.

**AC3 — verified 2026-08-30.** Over the whole Rd corpus read through
`rd_sources()` (81 topics), whitespace collapsed: the token `\code{.opus}`
returns exactly 2 topics — `separate_audio_video.Rd` and
`separate_audio_video_batch.Rd`, meeting the floor of 2 — and both contain,
verbatim, the string `multi_audio_rd_list()` returns for the committed vector
(`\code{.mka}, … \code{.opus} and \code{.ts}`). The marker clause
"containers named here as holding several" appears in exactly those same 2
topics, and 0 of them carry it without the renderer's string.

**AC4 — verified 2026-08-30.** `devtools::test()`: FAIL 0, WARN 12, SKIP 5,
PASS 8830. `devtools::check()`: Status OK — 0 errors, 0 warnings, 0 notes
(2m 54s). `devtools::document()` leaves `NAMESPACE`, `man/` and `data/` with no
uncommitted diff. All three re-run after the review's one fix-now edit below;
the targeted guard file is 15 pass / 0 fail.

### Consistency gate — 2026-08-30

Universal: `cairn_validate.py` exit 0, all checks passed, no advisories fired
(`release window` included). No `DESIGN.md` principle changed
(`Principles touched: —`), so `cairn_impact.py` does not apply.

Toolchain (`r-package` profile's `consistency-gate` slot): `document()` no diff;
no hand-edited generated file (the no-diff run covers `NAMESPACE`, `man/`,
`data/`); `README.Rmd`/`README.md` untouched by the diff; `pkgdown::check_pkgdown()`
"No problems found"; no `NEWS.md` entry — the rendered help wording is unchanged
(the Rd diff is line rewrapping only, confirmed independently by the diff lens),
so nothing user-visible changed; the one added file is under `tests/testthat/`,
so no new `.Rbuildignore` entry is due and `check()` raises no NOTE;
`check()` clean as recorded above.

### Independent review — 2026-08-30

Declared tier user-facing and the diff touches R source, so the full three-lens
fan-out ran, each lens fresh-context with a distinct evidence base.

**[S] blame-history:** no findings. Confirmed the `` `r fn()` `` mechanism is
M51's own precedent, that D069/D070/D071 and the gate logic are untouched, that
the reworded `:649` comment still credits D071, and that the Scope Out
exclusions (`NEWS.md`, the two exemplars) are absent from the diff.

**[S] prior-review record:** no prior-review evidence of a reintroduced or
contradicted finding; zero findings. `gh api .../pulls/comments?per_page=1`
returned `[]`, so the GitHub walk was skipped; the archive is the record here,
and M091's review is what this milestone closes rather than repeats.

**[O] diff-bug:** no correctness defects; six ranked minor findings, verbatim
below with disposition.

1. *"The AC3 domain token is a plausible future false positive."* The guard's
   topic set is every Rd containing `\code{.opus}`; a later help page mentioning
   `.opus` as an example would be pulled into the domain and required to carry
   the whole enumeration, reddening for an Rd-shape reason rather than for
   drift. Latent only: today the domain is the two intended topics.
   **Rejected** — this is verbatim the falsifier the plan gate recorded against
   shipping the guard ("falsified by the guard reddening for Rd-shape reasons
   rather than for drift"). The condition has not fired; recording it a second
   time as a candidate would duplicate the gate's own line.
2. *"T2's stated re-wrap was not performed; the test solves it differently."*
   `are an exclusion list and not a survey` is still split across lines in both
   blocks; `collapsed_rd()` collapsing whitespace makes the guard
   wrap-independent instead. **Logged, no change** — AC3 is met and the shipped
   route is the stronger one (a guard that cannot be broken by rewrapping), but
   the task text reads as done-as-written when the approach changed. Recorded
   here so the divergence is on the record rather than in the diff alone.
3. *"`stop()` instead of `cli::cli_abort()`"* at the one new error site, against
   CLAUDE.md's convention. **Rejected** — the site fires only inside
   `devtools::document()` and can never reach a caller, and the sibling renderer
   this one was told to follow (`rd_verb_list()`) uses base `stopifnot`. The
   convention governs user-facing conditions.
4. *"The vocabulary's 'starts at two' offset is implicit and duplicated."*
   `n - 1L` and `length(...) + 1L` both encode it and nothing at
   `multi_audio_count_words` says the vector begins at two. **Fixed now** — a
   four-line comment at the vector names the offset and warns that prepending
   `"one"` shifts every word. Guard file re-run: 15 pass / 0 fail;
   `document()` no diff.
5. *"Two drift paths the milestone leaves open."* Both blocks hand-write
   `\code{.avi}` and `\code{.nut}` as containers NOT in the vector; adding
   either to `multi_audio_extensions` would make each page contradict itself,
   and no guard notices. **Follow-up** — filed at the post-merge hygiene pass
   onto the existing two-container-exemplar candidate row, whose subject is the
   same class of hand-written container name left outside the enumeration. Its
   promotion condition is the mirror of that row's: an ADDITION of `avi` or
   `nut` to the vector, where the exemplar half promotes on a removal.
6. *"Cosmetic: the re-wrap left ragged roxygen lines."* **Rejected** — the help
   renderer reflows, nothing user-visible changes, and formatter-class nits are
   out of scope at review.

Return floor: none of the six demonstrates an acceptance criterion failing, and
none is a load-bearing defect in what the package does for a caller. No status
change; every finding logged above.

