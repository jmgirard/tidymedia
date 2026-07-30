# M41: Front-door validation parity for the codec arguments

- **Status:** in-progress
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
`video_codec` column rejecting `NA`. This milestone is deliberately
contract-neutral: it changes which values are *refused*, never what an accepted
value does.

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
- [ ] AC4: The guards add no new rejection and no new acceptance of `NULL`: for
      every argument in AC2, a `NULL` call and a default call produce the same
      outcome after the milestone as before it — the same compiled command where
      one compiles today, or the same abort where `NULL` aborts today
      (`anonymize_video`/`_batch` `video_codec`, `extract_audio` `audio_codec`).
      Compared against the baseline the T2 script regenerates from the
      pre-milestone ref.
- [x] AC5: `extract_audio_batch`'s new `audio_codec` guard passes
      `allow_null = TRUE`, so `extract_audio_batch(audio_codec = NULL)` still
      compiles (`-vn`, no `-codec:a`) while `extract_audio(audio_codec = NULL)`
      still aborts; a code comment names that disagreement and points at M42.
- [x] AC6: `devtools::test()` and `devtools::check()` clean — 0 errors, 0
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
- [x] T5: Front-door guards for the remaining sites: `standardize_video_batch`
      `video_codec` ([ffmpeg.R:2547](../../R/ffmpeg.R#L2547)),
      `anonymize_video_batch` `video_codec`
      ([ffmpeg.R:1145](../../R/ffmpeg.R#L1145)), `extract_audio_batch`
      `audio_codec` with `allow_null = TRUE` plus the AC5 comment
      ([ffmpeg.R:3290](../../R/ffmpeg.R#L3290)), and `standardize_video`
      `video_codec` ([ffmpeg.R:780](../../R/ffmpeg.R#L780)).
- [x] T6: Make `normalize_audio` ([ffmpeg.R:1329](../../R/ffmpeg.R#L1329)) and
      `convert_audio` ([ffmpeg.R:485](../../R/ffmpeg.R#L485)) blame the verb
      rather than their `*_pipeline()` helper — thread `call` or hoist the check.
- [x] T7: Parameterized test over T3's list: message and `call` for `NA`, a
      number, and a length-2 vector on every pair, plus AC3's `In index:`
      absence at `parallel = FALSE`. Prove it discriminates by reverting one
      guard and confirming it goes red (M39 lesson).
- [x] T9 (discovered): Two `test-video-codec.R` execution tests call `get_width()`
      — which shells out to *mediainfo* — while guarding only on
      `skip_if_no_ffprobe()`, so they fail rather than skip wherever the
      mediainfo CLI is absent, against the convention CLAUDE.md states and the
      existing `skip_if_no_mediainfo()` helper serves. Blocks AC6, so fixed here.
- [x] T8: Re-run T2's script against the pre-milestone ref and the branch;
      confirm every `NULL`/default outcome matches. Update `@param` prose where a
      guard changes the documented error, `devtools::document()`, NEWS entry,
      `devtools::test()` + `devtools::check()` clean.

## Work log

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

## Review

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
