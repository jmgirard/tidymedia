# M41: Front-door validation parity for the codec arguments

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m41-codec-arg-front-door-guards`

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

- [x] T1: Install `spelling` into the active R 4.6 library — a declared
      `Suggests`, absent after the 4.6 upgrade, and the package M17's lesson
      needs (`spelling::update_wordlist()`; `devtools::check()` masks the
      `spelling.Rout` NOTE). Environment repair, not a dependency change (no
      D-entry).
- [x] T2: Commit the baseline regeneration script under `data-raw/`: reconstruct
      `R/*.R` from a named git ref via `git show` into a temp dir, source them,
      and print the compiled command *or* the abort message for every AC2
      verb/argument pair at its default and `NULL` call. Capture the
      pre-milestone baseline from the default branch.
- [x] T3: Enumerate the AC2 verb/argument set from source into this file's work
      log — the eight non-compliant pairs and the compliant ones — so T7's test
      runs over a fixed list, not a re-derivation.
- [x] T4: Regression test first, shown red against T2's reconstructed pre-fix
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
- [x] T9 (discovered): Two `test-video-codec.R` execution tests call `get_width()`
      — which shells out to *mediainfo* — while guarding only on
      `skip_if_no_ffprobe()`, so they fail rather than skip wherever the
      mediainfo CLI is absent, against the convention CLAUDE.md states and the
      existing `skip_if_no_mediainfo()` helper serves. Blocks AC6, so fixed here.
- [ ] T8: Re-run T2's script against the pre-milestone ref and the branch;
      confirm every `NULL`/default outcome matches. Update `@param` prose where a
      guard changes the documented error, `devtools::document()`, NEWS entry,
      `devtools::test()` + `devtools::check()` clean.

## Work log

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

## Review
