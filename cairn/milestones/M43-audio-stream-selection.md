# M43: Pick which audio track the extraction verbs take

- **Status:** review
- **Priority:** normal
- **Depends on:** M41, M42
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m43-audio-stream-selection`

## Goal

Let a caller name which audio track `extract_audio()` and `convert_audio()` take
from a multi-track file, instead of FFmpeg choosing invisibly.

## Scope

**In:** an `audio_stream` argument on `extract_audio()`, `convert_audio()` and
both `_batch` siblings, selecting a track by 0-based index within the input's
audio streams — the same base as the `audio =` input index D009 already blessed
as resolving to `ffm_map()`. With no selector the verbs compile an explicit map
to track 0, so selection stops depending on FFmpeg's implicit
default-disposition heuristic. Plus the Layer-1 enablement: `ffm_map()` today
rejects a vector and silently overwrites on chaining, while `ffm_compile()`
already renders `object$map` element-wise.

**Precondition, not a dependency:** `convert_audio()`'s `-map a` crash on
multi-track input ships first as a hotfix (it maps every audio stream, so FFmpeg
aborts with "Exactly one MP3 audio stream is required", exit 65514, zero-byte
output). This milestone must not regress that fix.

**Out:** warning a caller when tracks are dropped implicitly → M44, which owns
the run-path probe and the D013 extension it needs. Carrying `audio_stream` to
the verbs that pass audio *through* (`separate_audio_video`,
`standardize_video`, `crop_video`, `segment_video`, `anonymize_video`) and its
interaction with the `audio =` input index on
`compare_videos`/`picture_in_picture` → a ROADMAP candidate row, plannable once
this milestone sets the argument's shape. Video/subtitle selection → not in
scope; `strip_metadata()`'s `-map 0` already keeps every stream.

## Acceptance criteria

- [ ] AC1: `extract_audio()` compiles an explicit audio-stream map on every call
      (today it emits no `-map` at all) and `convert_audio()` keeps the hotfix's
      explicit map. Two consequences are carried, not discovered: the
      byte-identity assertion M40 left at `test-ffmpeg.R:124-137`, which pins
      `-q:a 0 -map a`, is rewritten; and on a file whose second track carries the
      container's DEFAULT disposition the extracted track changes (measured:
      `spa` → `eng`). Both appear in NEWS. `convert_audio()`'s `-q:a 0` output
      stays byte-identical for a single-track input (measured).
- [ ] AC2: `audio_stream` on `extract_audio()`, `extract_audio_batch()`,
      `convert_audio()` and `convert_audio_batch()` selects by 0-based index
      within the input's audio streams, validated at the front door as
      `rlang::check_number_whole(min = 0, allow_null = TRUE)` — the in-repo
      precedent at [ffmpeg.R:4309](../../R/ffmpeg.R#L4309). `audio_stream = 1`
      against AC5's fixture yields the `spa` track, asserted on the output's
      `language` tag written to `.m4a` or `.mka`; the tag does not survive to
      `.aac` or `.mp3`, so those containers cannot carry this evidence.
- [ ] AC3: `ffm_map()` accepts a character vector and emits one `-map` per
      element; chaining appends, and `replace = TRUE` restores replacement so
      `ffm_map(ffm_copy(p), "0:a:1", replace = TRUE)` compiles only `-map 0:a:1`.
      `ffm_compile()` is unchanged. No internal caller's behavior changes — each
      sets a map once per pipeline ([ffmpeg.R:309](../../R/ffmpeg.R#L309),
      [ffmpeg.R:447](../../R/ffmpeg.R#L447),
      [ffmpeg.R:509](../../R/ffmpeg.R#L509),
      [ffmpeg.R:3944](../../R/ffmpeg.R#L3944),
      [ffmpeg.R:4089](../../R/ffmpeg.R#L4089),
      [ffm.R:605](../../R/ffm.R#L605)) — and `ffm_map()`'s "Set the Stream
      Mapping" docs plus `ffm_copy(streams=)`'s prose are corrected.
- [ ] AC4: The `_batch` siblings honour an `audio_stream` override column,
      guarded `is.numeric(x) || (is.logical(x) && all(is.na(x)))` (the M34 shape
      at [ffmpeg.R:3165](../../R/ffmpeg.R#L3165)), re-validated per row (M32),
      with a hint parameterized to this column's meaning rather than inherited
      (M40). A cell of `NA` keeps that row on the track-0 default, overriding the
      argument — the family's sentinel meaning, not "defer to the argument". The
      `@param jobs` column enumeration lists it (M39).
- [ ] AC5: A `make_multitrack_video()` generator joins the six in
      `tests/testthat/helper-media.R` — 1 video plus 3 aac tracks tagged
      `eng`/`spa`/`fra` from lavfi `sine` sources at distinct frequencies, no
      committed media, matching that file's stated reason for generating
      fixtures.
- [ ] AC6: `devtools::document()` no-diff; `devtools::test()` and
      `devtools::check()` clean — 0 errors, 0 warnings, with
      `spelling::update_wordlist()` run for new terms (M17). NEWS covers
      `audio_stream`, `extract_audio()`'s changed default track, and
      `ffm_map()`'s API change.

## Coverage

- AC1 → T3, T6
- AC2 → T3, T4, T5
- AC3 → T2
- AC4 → T4, T5
- AC5 → T1
- AC6 → T6

## Tasks

- [x] T1: Add `make_multitrack_video()` to `tests/testthat/helper-media.R` beside
      the existing six generators — video plus three `-b:a 32k` aac tracks
      tagged eng/spa/fra from lavfi `sine` sources at distinct frequencies.
- [x] T2: `ffm_map()` ([ffm.R:555](../../R/ffm.R#L555)) — accept a character
      vector, append on chaining, add `replace = TRUE`; correct its docs and
      `ffm_copy(streams=)`'s prose; compile tests over every existing call site.
- [x] T3: `audio_stream` on `extract_audio()` and `convert_audio()` — front-door
      `check_number_whole(min = 0, allow_null = TRUE)`, resolving through
      `ffm_map()` to `0:a:<n>`, default track 0. Rewrite the superseded
      byte-identity test at `test-ffmpeg.R:124-137`.
- [x] T4: `audio_stream` on `extract_audio_batch()` and `convert_audio_batch()`:
      batch-wide argument plus the per-row override column with AC4's guard,
      hint, and per-row re-validation.
- [x] T5: Execution tests on T1's fixture — `audio_stream = 1` → `spa` via the
      `.m4a` language tag, default → track 0, and the batch column overriding the
      argument. Prove they discriminate by mutating the resolved map to ignore
      `audio_stream` (M39 lesson).
- [x] T6: `@param` prose for all four verbs; NEWS entries per AC6;
      `spelling::update_wordlist()`; `devtools::document()`, `test()`, `check()`.

## Work log

- 2026-07-29: created by /milestone-plan.
- 2026-07-29: plan gate chose an explicit map to track 0 over respecting the container's DEFAULT disposition, because reproducibility across FFmpeg versions is the package's stated purpose and the disposition route needs a probe to make explicit anyway; falsified by a research workflow whose sources rely on the default flag to mark the intended track.
- 2026-07-29: plan gate chose `ffm_map()` append-plus-`replace` over append-only and over vector-with-replace, because append-only leaves no way to narrow `ffm_copy()`'s all-streams map and a repeated map duplicates the output stream — the exact failure the `convert_audio` hotfix removes; falsified by `replace` going unused across every call site once the selector lands.
- 2026-07-29: plan chose `audio_stream` as the argument name over reusing `audio`, because D009 documents `audio =` as a 0-based *input* index on `compare_videos`/`picture_in_picture` while this indexes streams within one input; falsified by the follow-up unifying both under one argument.
- 2026-07-29: plan gate chose hotfixing `convert_audio()`'s `-map a` crash separately over folding it into this milestone, so a broken verb is not gated behind M41 and M42; falsified by the hotfix's deterministic map proving insufficient without the selector.
- 2026-07-29: split from M44 because 9 acceptance criteria hit the sizing tripwire; the run-path probe and its D013 extension are independently shippable, so the selector ships without waiting on a convention decision.
- 2026-07-29: the precondition hotfix shipped — `convert_audio_pipeline()` now maps `0:a:0` instead of `a`, so AC1's "keeps the hotfix's explicit map" reads against that literal. It also landed `make_multitrack_video()` in `helper-media.R` to the shape AC5 specifies (3 aac tracks tagged eng/spa/fra, distinct sine frequencies, `.mkv`), so verify T1 rather than re-adding it; four `-map a` assertions were retargeted, and the M40 byte-identity test at `test-ffmpeg.R:124-137` was already rewritten for the map, narrowing what AC1 leaves to do.

- 2026-07-30: T1 verified, not re-authored — the precondition hotfix already landed `make_multitrack_video()` at AC5's shape (3 aac tracks, eng/spa/fra, distinct sine frequencies, `.mkv`, no committed media).
- 2026-07-30: implement gate confirmed both plan choices unchanged — `ffm_map()` ships `replace =` per AC3 (unused in-package, but its only alternative strands `ffm_copy()`'s all-streams map with no way to narrow it, and the carry follow-up needs it), and `extract_audio()` keeps `-vn` beside its new map.
- 2026-07-30: T2 done — `ffm_map()` takes a character vector, appends on chaining, gains `replace = TRUE`; `check_string()` replaced by a spelled-out character-vector guard (rlang's `check_character()` is unexported). `ffm_compile()` untouched. Docs corrected on `ffm_map()` and `ffm_copy(streams=)`; a new test pins ≤1 `-map` per compiled command across every in-package call site.
- 2026-07-30: T3–T5 landed in one commit rather than three — the shared `audio_stream_map()` helper, both scalar verbs, both batch verbs and the tests were one working set, and splitting the commit after the fact would have invented a sequence the work did not have.
- 2026-07-30: T3 done — `audio_stream_map()` resolves the selector to `0:a:<n>` and carries the per-row `check_number_whole(min = 0, allow_null = TRUE)`; both scalar verbs check again at their own front door so a bad argument blames the verb. `extract_audio()` now compiles an explicit map on every call (it emitted none before) and keeps `-vn`; `convert_audio()`'s default still compiles the hotfix's literal `-map 0:a:0`, asserted byte-identically.
- 2026-07-30: T4 done — `check_batch_audio_col()` gained `col`/`na_means` parameters rather than being copied, so the `audio_stream` column's hint says "keep the first audio track" where the composite verbs' `audio` column says "drop audio"; `batch_stream_cell()` resolves an `NA` cell to the NULL sentinel, kept separate from `batch_codec_cell()`. Both `@param jobs` enumerations updated.
- 2026-07-30: T5 done — execution tests read the output's `language` tag from `.m4a`/`.mka`: `audio_stream = 1` yields `spa`, the default yields `eng`, `= 2` yields exactly one stream tagged `fra`, and a batch `audio_stream` column beats the argument row-wise while an `NA` cell falls to track 0. Mutating `audio_stream_map()` to ignore the selector turns 10 tests red, so they discriminate (M39).
- 2026-07-30: T6 done — NEWS carries `audio_stream` (New features) plus two breaking-change entries, `extract_audio()`'s named default track and `ffm_map()`'s append semantics. `spelling::update_wordlist()` added one word (`WebM`, pre-existing, unlisted because `tests/spelling.R` runs with `error = FALSE`). `devtools::document()` no-diff, `check()` `Status: OK` (0/0/0), `pkgdown::check_pkgdown()` clean. `build_readme()` re-knit one command; its temp-libpath churn on an unrelated chunk was reverted (M24).
- 2026-07-30: promoted the two cross-cutting choices to **D023** rather than leaving them milestone-local — the `audio_stream`-vs-`audio` indexing split constrains the carry candidate and narrows D009, and `ffm_map()`'s append contract binds every future verb.
- 2026-07-30: `R/ffmpeg.R`'s CRLF line endings verified intact after every edit (4715 → 4844 lines, matching the diff's net insertions; M35 lesson).

## Decisions

## Review
