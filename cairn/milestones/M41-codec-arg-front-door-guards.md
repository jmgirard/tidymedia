# M41: Front-door validation parity for the codec arguments

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** —

## Goal

Every codec argument on every task verb rejects a non-string value at the front
door, naming its own argument and its own verb.

## Scope

**In:** the one silent-default bug — `normalize_audio_batch(audio_codec = NA)`
compiles the default command instead of erroring, because `batch_codec_cell()`
([ffmpeg.R:3153](../../R/ffmpeg.R#L3153)) maps a *scalar* `NA` to the `NULL`
sentinel and no front-door guard stops it. Plus the seven remaining verb/argument
pairs whose abort either leaks Layer-1's `video`/`audio` parameter name, blames a
`*_pipeline()` helper, or fires inside `purrr::pmap()` rather than at the front
door. Plus a committed script that regenerates the pre-milestone compiled
commands from a git ref, so "this guard rejects nothing that worked before" is
re-verifiable at review rather than a transcript.

**Out:** what `NULL` and column `NA` *mean* per verb → M42. That covers the
`standardize_video` vs `anonymize_video` `NULL` disagreement, the `extract_audio`
vs `extract_audio_batch` disagreement, and `standardize_video_batch`'s
`video_codec` column rejecting `NA`. This milestone is deliberately
contract-neutral: it changes which values are *refused*, never what an accepted
value does.

## Acceptance criteria

- [ ] AC1: `normalize_audio_batch(jobs, audio_codec = NA)` at the default
      `two_pass = FALSE` aborts with a message naming `audio_codec`. Before the
      fix that call compiles the default command (`-af "loudnorm=..." -codec:v
      copy`, no `-codec:a`), identical to `audio_codec = NULL`; the regression
      test is shown to fail against the pre-fix sources the T2 script
      reconstructs. (`two_pass = TRUE` already aborts via
      [ffmpeg.R:2969](../../R/ffmpeg.R#L2969) — the silent compile is
      default-path-only.)
- [ ] AC2: For every task verb and `_batch` sibling whose `video_codec` or
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
- [ ] AC3: At `parallel = FALSE`, each abort AC2 inspects carries no
      `In index: <n>` in its message, on the same condition AC2 inspects —
      showing the scalar check ran before the fan-out, not inside
      `purrr::pmap()`.
- [ ] AC4: The guards add no new rejection and no new acceptance of `NULL`: for
      every argument in AC2, a `NULL` call and a default call produce the same
      outcome after the milestone as before it — the same compiled command where
      one compiles today, or the same abort where `NULL` aborts today
      (`anonymize_video`/`_batch` `video_codec`, `extract_audio` `audio_codec`).
      Compared against the baseline the T2 script regenerates from the
      pre-milestone ref.
- [ ] AC5: `extract_audio_batch`'s new `audio_codec` guard passes
      `allow_null = TRUE`, so `extract_audio_batch(audio_codec = NULL)` still
      compiles (`-vn`, no `-codec:a`) while `extract_audio(audio_codec = NULL)`
      still aborts; a code comment names that disagreement and points at M42.
- [ ] AC6: `devtools::test()` and `devtools::check()` clean — 0 errors, 0
      warnings.

## Coverage

- AC1 → T2, T4
- AC2 → T3, T5, T6, T7
- AC3 → T5, T7
- AC4 → T2, T8
- AC5 → T5, T7
- AC6 → T1, T8

## Tasks

- [ ] T1: Install `spelling` into the active R 4.6 library — a declared
      `Suggests`, absent after the 4.6 upgrade, and the package M17's lesson
      needs (`spelling::update_wordlist()`; `devtools::check()` masks the
      `spelling.Rout` NOTE). Environment repair, not a dependency change (no
      D-entry).
- [ ] T2: Commit the baseline regeneration script under `data-raw/`: reconstruct
      `R/*.R` from a named git ref via `git show` into a temp dir, source them,
      and print the compiled command *or* the abort message for every AC2
      verb/argument pair at its default and `NULL` call. Capture the
      pre-milestone baseline from the default branch.
- [ ] T3: Enumerate the AC2 verb/argument set from source into this file's work
      log — the eight non-compliant pairs and the compliant ones — so T7's test
      runs over a fixed list, not a re-derivation.
- [ ] T4: Regression test first, shown red against T2's reconstructed pre-fix
      tree, then the fix: front-door
      `rlang::check_string(audio_codec, allow_null = TRUE)` in
      `normalize_audio_batch` ([ffmpeg.R:2891](../../R/ffmpeg.R#L2891)).
- [ ] T5: Front-door guards for the remaining sites: `standardize_video_batch`
      `video_codec` ([ffmpeg.R:2547](../../R/ffmpeg.R#L2547)),
      `anonymize_video_batch` `video_codec`
      ([ffmpeg.R:1145](../../R/ffmpeg.R#L1145)), `extract_audio_batch`
      `audio_codec` with `allow_null = TRUE` plus the AC5 comment
      ([ffmpeg.R:3290](../../R/ffmpeg.R#L3290)), and `standardize_video`
      `video_codec` ([ffmpeg.R:780](../../R/ffmpeg.R#L780)).
- [ ] T6: Make `normalize_audio` ([ffmpeg.R:1329](../../R/ffmpeg.R#L1329)) and
      `convert_audio` ([ffmpeg.R:485](../../R/ffmpeg.R#L485)) blame the verb
      rather than their `*_pipeline()` helper — thread `call` or hoist the check.
- [ ] T7: Parameterized test over T3's list: message and `call` for `NA`, a
      number, and a length-2 vector on every pair, plus AC3's `In index:`
      absence at `parallel = FALSE`. Prove it discriminates by reverting one
      guard and confirming it goes red (M39 lesson).
- [ ] T8: Re-run T2's script against the pre-milestone ref and the branch;
      confirm every `NULL`/default outcome matches. Update `@param` prose where a
      guard changes the documented error, `devtools::document()`, NEWS entry,
      `devtools::test()` + `devtools::check()` clean.

## Work log

- 2026-07-29: created by /milestone-plan.
- 2026-07-29: plan gate chose a front-door duplicate check over threading `arg`/`call` through `ffm_codec()` because the passthrough still runs inside `purrr::pmap()` and so cannot satisfy AC3; falsified by an engine-side seam that reports the caller's argument *before* the fan-out.
- 2026-07-29: plan gate chose a committed ref-based regeneration script over a testthat snapshot fixture and over an implementation-time transcript because it re-derives the baseline as fresh evidence at review without adding a churn-prone second snapshot file; falsified by the script failing to reconstruct a sourceable pre-milestone tree from a ref.
- 2026-07-29: plan gate chose preserving `extract_audio_batch(audio_codec = NULL)`'s current acceptance over fixing the scalar/batch split here, because it keeps M41 contract-neutral; falsified by a report that the batch verb's `NULL` acceptance is itself the user-visible bug.
- 2026-07-29: plan chose splitting guards (M41) from semantics (M42) over one milestone because the guard work needs no D-entry and the semantics work does; falsified by the semantics fix landing on the same code sites, making two PRs redundant.
- 2026-07-29: R is 4.6.1 via winget (the R-4.4.1 directory is a stale leftover, not a second install). `archive` was absent from the 4.6 library during investigation — probes sourced `R/*.R` directly to work around it — and is now installed at 1.1.13, so `load_all()` succeeds; `spelling` remains absent and T1 installs it.

## Decisions

## Review
