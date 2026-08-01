# M51: Make the package's two 0-based audio indices legible

- **Status:** in-progress
- **Branch:** `m51-audio-index-docs`
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

- [ ] AC1 A documentation-only topic exists, built from a roxygen `@name` block
      in the `R/utils-tidy-eval.R:1-15` shape but **without** its `@keywords
      internal` (`:11`), and it states in prose: that `audio_stream` counts one
      input's audio streams while `audio` counts a verb's inputs; which verb
      families read `audio_stream = NULL` as the first track and which as every
      track; that `audio = NULL` emits no map at all and so drops audio
      (`R/ffmpeg.R:5028`, `:5173`), unlike `audio_stream = NULL`; that an `NA`
      cell in a `_batch` override column means that row's `NULL` sentinel while
      an absent column means the scalar argument applies; and that `audio` also
      names a codec on `ffm_codec()` and a logical on `ffm_copy()`.
- [ ] AC2 The topic is reachable from the reference index: it has a
      `_pkgdown.yml` entry and `pkgdown::check_pkgdown()` still reports no
      problems, as it does today.
- [ ] AC3 Every exported verb carrying `audio_stream` (eighteen after M49) and
      every exported verb carrying `audio` as an input index
      (`compare_videos()`, `picture_in_picture()`, and their `_batch`
      siblings) links to the new topic. Asserted by a test that enumerates the
      parameter across `man/*.Rd` and fails when a topic carrying it lacks the
      link — so a future verb that gains the argument and not the link is
      caught.
- [ ] AC4 The sentences naming the every-track and first-track families exist
      in exactly one place in `R/`, with the eighteen `@param audio_stream`
      blocks inheriting them rather than restating them; the rendered `.Rd`
      files still each carry the text. A stale enumeration is then
      unrepresentable rather than merely detected.
- [ ] AC5 `audio_stream` appears in at least one vignette with a runnable
      chunk. `vignettes/tidymedia.Rmd:141` currently reads "Each manages its
      own stream labels internally, so audio is dropped unless you map it back"
      — the fix is that a reader cannot carry that sentence to the
      pass-through verbs, whether by scoping its wording or by adding the
      contrast; the vignette names at least one pass-through verb as behaving
      differently.
- [ ] AC6 `compare_videos_batch()` and `picture_in_picture_batch()` state what
      `audio` means rather than only deferring to the scalar verb
      (`R/ffmpeg.R:5350`, `:5471`), and `extract_audio_batch()` /
      `convert_audio_batch()` carry the "FFmpeg error, not an R one" sentence
      their scalar siblings have and they lack (`R/ffmpeg.R:4183`, `:4305`
      against `:490`, `:974`).
- [ ] AC7 A `cairn/DECISIONS.md` entry re-confirms or revises D023's first
      bullet — two names, two bases — now that the argument spans eighteen
      verbs and two `NULL` readings in three spellings (`0:a:0`, `0:a:0?`,
      `0:a?`), and records what would reopen it.
- [ ] AC8 `devtools::document()` produces no diff, `devtools::test()` clean,
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

- [ ] T1 Write the enumerating test first (AC3): walk `man/*.Rd` for
      `audio_stream` and for `audio`-as-input-index, and assert each topic
      carries the cross-link. It must go red before the docs change.
- [ ] T2 Author the concept topic and register it in `_pkgdown.yml`.
- [ ] T3 Add the `@family` tag and `@seealso` links across all twenty-two
      affected verbs; fix the two batch `@param audio` blocks and the two
      missing "FFmpeg error" sentences.
- [ ] T4 Introduce the shared `@param audio_stream` source — an
      `@inheritParams` donor or a `man-roxygen` template, neither of which the
      package has today — and collapse the eighteen copies onto it, keeping the
      per-family differences that are genuinely per-family.
- [ ] T5 `devtools::document()`; `pkgdown::check_pkgdown()`;
      `spelling::update_wordlist()`.
- [ ] T6 Vignette chunk for `audio_stream` and the `vignettes/tidymedia.Rmd:141`
      correction; re-knit any affected vignette.
- [ ] T7 Append the D-entry, write NEWS, add the two candidate rows named in
      Out, and run the profile's verify slot plus `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose one shared `@param audio_stream` source over keeping eighteen hand-written copies guarded by a drift test, because the test encodes the family lists a second time and catches drift only at review; falsified by the shared source proving unable to express a per-family difference without a conditional, at which point the copies plus the guard return.
- 2026-07-31: sequenced after M50 so the concept topic and the vignette describe the quoted specifier form rather than needing a second pass.
- 2026-07-31: implementation gate settled three open choices, all as recommended: the shared `@param audio_stream` source is an internal R function whose result roxygen inserts via an inline `` `r ` `` call (probed in a scratch package first — the result is inserted verbatim, so the existing `\code{\link{}}` register survives, and the family lists become directly unit-testable); the concept topic is named `audio_stream` so `?audio_stream` reaches it; the `@family` grouping stays as scoped.
- 2026-07-31: criteria audit ([O], fresh context) returned three findings: AC4 offered two incompatible end-states, one of which made AC3's assertion vacuous — resolved at the gate in favour of the shared source; AC7 said "three `NULL` readings" where D026 and this file's own AC1 say two (in three spellings); and the Coverage table orphaned T1, the task writing the very test AC3 relies on. All fixed above. The audit confirmed the eighteen-verb arithmetic (fourteen `.Rd` files carry `audio_stream` today, plus M49's four) and every other citation in the file.

## Decisions

## Review
