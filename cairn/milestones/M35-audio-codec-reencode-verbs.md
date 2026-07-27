# M35: `audio_codec` for the four re-encode verbs — stream-copy by default

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP1, GP2
- **Branch/PR:** `m35-audio-codec-reencode-verbs`

## Goal

Give `crop_video`, `segment_video`, `compare_videos`, `picture_in_picture`
(+ `_batch` siblings) an `audio_codec` argument defaulting to `"copy"`, so they
stop silently re-encoding audio to the local FFmpeg build's container default.

## Scope

**In:** an `audio_codec` formal on the same eight verbs M34 gave `video_codec`,
placed beside it. Default `"copy"` (stream-copy, matching the norm
`standardize_video`/`anonymize_video` already document at R/ffmpeg.R:633);
a named encoder re-encodes; `NULL` emits no `-codec:a`. Batch: a per-row
`audio_codec` column, `NA` → unset, reusing `check_batch_codec_col(col=)`
(R/ffmpeg.R:2847) and `batch_codec_cell()` (R/ffmpeg.R:2866). Contradiction
guards on the stream-copy and no-audio-mapped paths. Riding along: the missing
`audio` column guard on `compare_videos_batch` (R/ffmpeg.R:3787–3800) and the
loose one on `picture_in_picture_batch` (R/ffmpeg.R:3937–3940); a doc
cross-reference that `format_for_web_batch` honors no per-row codec column.

**Out:** `separate_audio_video`'s `reencode = TRUE` path (R/ffmpeg.R:303) and
`normalize_audio`'s always-re-encoded audio (R/ffmpeg.R:1250) → a candidate row
each; both need their own arg-shape call against an existing `reencode` switch.
Audio `sample_rate`/`channels` on these four → not proposed. `pixel_format`
stays deferred (D016).

## Acceptance criteria

- [x] AC1 All eight verbs carry a formal `audio_codec = "copy"` beside
      `video_codec` (exact D014 spelling; no `acodec`/`codec` alias), proven by
      a `formals()`-level test over all eight; `R/ffm.R` has no functional diff
      (IP1/IP3 — Layer 2 computes, Layer 1 unchanged).
- [x] AC2 The only compiled-command change is the added `-codec:a` token:
      pinned literals for all eight assert the pre-M35 command plus
      `-codec:a copy`. `audio_codec = "aac"` compiles `-codec:a aac`; `NULL`
      compiles no `-codec:a`; a non-token value aborts via `check_token()`.
- [x] AC3 `segment_video(reencode = FALSE)` aborts (cli, with a repair hint)
      for any `audio_codec` other than `"copy"`, enforced per row inside
      `segment_pipeline()` so `segment_video_batch` inherits it — tests cover
      the scalar arg and a mixed per-row `reencode` column.
- [x] AC4 The composites emit `-codec:a` only when `audio` maps a stream, and
      abort when `audio = NULL` meets a named encoder. With `audio = 0` each
      compiles one command carrying `-filter_complex`, `[vout]`,
      `-map "[vout]"`, `-map 0:a`, and `-codec:a copy`.
- [x] AC5 All four `_batch` siblings accept a per-row `audio_codec` column:
      character with `NA` cells (that row compiles no `-codec:a`), an all-`NA`
      logical column accepted, a numeric column aborted up front. Separately,
      `compare_videos_batch` gains an up-front `audio` column guard and
      `picture_in_picture_batch`'s is tightened to
      `is.numeric(x) || (is.logical(x) && all(is.na(x)))`; both boundaries
      tested.
- [x] AC6 Execution evidence: cropping a `make_test_video()` fixture with
      default arguments yields an output whose audio stream codec, read via
      `probe_audio()`, is identical to the input's — `skip_if` the binaries are
      absent.
- [x] AC7 `devtools::test()` and `devtools::check()` clean (0 errors,
      0 warnings; NOTEs justified), `devtools::document()` no diff; roxygen
      `@param audio_codec` on all eight plus the audio-behavior prose
      (R/ffmpeg.R:3485, 3595); a `NEWS.md` entry naming the changed default
      and its container caveat, with no milestone numbers; the
      `format_for_web_batch` cross-reference in place.

## Coverage

- AC1 → T1, T2, T3, T4, T5
- AC2 → T1, T2, T3, T4, T5
- AC3 → T3
- AC4 → T4, T5
- AC5 → T6, T7
- AC6 → T8
- AC7 → T9

## Tasks

- [x] T1 Add `apply_audio_codec()` beside `apply_video_codec()`
      (R/ffmpeg.R:1560) resolving `"copy"` / named / `NULL` onto
      `ffm_codec(audio =)`; place it above any roxygen block (M28 lesson).
- [x] T2 `crop_video` + `crop_video_pipeline` (R/ffmpeg.R:424–478): formal,
      thread T1, pinned compile tests.
- [x] T3 `segment_video` + `segment_pipeline` (R/ffmpeg.R:1639, 1733–1757):
      formal, thread on the re-encode path, per-row copy-conflict abort.
- [x] T4 `compare_videos` + pipeline (R/ffmpeg.R:3446–3472, 3523): formal,
      emit only when audio is mapped, abort on named-encoder-with-no-audio.
- [x] T5 `picture_in_picture` + pipeline (R/ffmpeg.R:3554–3585, 3638): same
      shape as T4.
- [x] T6 Remaining three `_batch` siblings (segment's landed in T3): `audio_codec` per-row column via `pick()` +
      `batch_codec_cell()`, guarded by
      `check_batch_codec_col(jobs, "audio_codec")`.
- [x] T7 Batch `audio` column guards: add compare's missing check, tighten
      pip's to the M34 shape; test both boundaries.
- [x] T8 Execution test: crop `make_test_video()`, `probe_audio()` input and
      output, assert the codec is unchanged.
- [x] T9 Docs + gate: roxygen on all eight, audio-behavior prose,
      `format_for_web_batch` cross-reference, `NEWS.md` entry, `document()`,
      `test()`, `check()`.

## Work log

- 2026-07-26: created by /milestone-plan. Absorbs three candidate rows (RR01 Beyond-1, RR01 Beyond-3, the M34 review's pip guard-parity item); the plan-time audit widened the hole from the two composites to all four M34 verbs, and split two further verbs out to candidate rows. D017 records the arg shape.
- 2026-07-26: set in-progress; branch `m35-audio-codec-reencode-verbs` cut from master.
- 2026-07-26: T1 — `apply_audio_codec()` added beside `apply_video_codec()`; NULL returns the pipeline untouched, otherwise token-checked with the caller's `call` and threaded to `ffm_codec(audio =)`. Covered indirectly from T2 (internal helper, per the profile's test-doctrine). test() green: 1357 pass, 0 fail.
- 2026-07-26: T2 — `crop_video` gains `audio_codec = "copy"` after `video_codec`; new `tests/testthat/test-audio-codec.R` pins the default literal byte-for-byte (`-codec:a copy` lands between `-vf` and `-map 0`). Two pre-existing pins updated for the deliberate default change: `test-ffmpeg.R` no longer asserts filter/map adjacency, and M34's crop byte-pin narrows to its own claim (no `-codec:v`), pointing at the new file for the full literal. test() green: 1369 pass, 0 fail.
- 2026-07-26: T3 — `segment_video` + `segment_pipeline` gain `audio_codec`, applied after `ffm_copy()` so the copy path stays idempotent; the new per-row guard aborts when a stream copy meets anything but `"copy"` (NULL included, since `ffm_copy()` would overwrite it). Minor task refinement: `segment_video_batch`'s formal + per-row column landed here rather than in T6, because AC3's per-row evidence needs them; T6 now covers the remaining three siblings. M34's segment byte-pin narrowed like crop's. test() green: 1382 pass, 0 fail.
- 2026-07-26: T4+T5 — both composites gain `audio_codec`, applied only inside the `if (!is.null(audio))` branch so the default (`audio = NULL`, no track carried) still compiles no `-codec:a` and M34's composite byte-pins hold untouched. A named encoder with no audio mapped aborts; NULL stays legal there since it only ever means "emit nothing". Compile test pins the full complex shape: `-filter_complex` + `[vout]` + `-map "[vout]"` + `-map N:a` + both codecs in one command. test() green: 1403 pass, 0 fail.
- 2026-07-26: T6 — `crop_video_batch`, `compare_videos_batch`, `picture_in_picture_batch` gain the `audio_codec` formal plus the per-row column via `pick()`/`batch_codec_cell()`, guarded by `check_batch_codec_col(jobs, "audio_codec")` (M34's helper took a `col` argument already, so the all-NA-logical acceptance and the all-NA-numeric rejection come for free and are tested on both boundaries). test() green: 1417 pass, 0 fail.
- 2026-07-26: T7 — extracted `check_batch_audio_col()` beside `check_batch_codec_col()` and pointed both composites at it, replacing pip's loose inline guard and giving compare the up-front guard it never had. The tightened shape rejects an all-NA character column (which the old one admitted by accident) and `c(TRUE, FALSE)`, while accepting both all-NA logical and all-NA numeric. test() green: 1424 pass, 0 fail.
- 2026-07-26: T8 — execution test added with a new `make_mp3_audio_video()` helper. Deviation from the plan's `make_test_video()`: that fixture's AAC audio in an MP4 is *also* the container default, so copy and re-encode would be indistinguishable from the output. MP3-in-MP4 discriminates — the default keeps `mp3`, `audio_codec = NULL` yields `aac`, so the test proves both the copy and the escape hatch. Binary-gated by `skip_if_no_ffmpeg`/`skip_if_no_ffprobe`.
- 2026-07-26: T9 — roxygen on all eight (verified: every one of the eight `.Rd` files names `audio_codec`), composite description prose updated, `format_for_web_batch`'s jobs doc now states that a `video_codec`/`audio_codec` column is ignored there and points at `standardize_video_batch` (closes RR01 Beyond-3), NEWS.md gains a Breaking changes section, vignette prose notes the carried track is stream-copied. AC1's formals test asserts the spelling, the `"copy"` default and position-before-`hardware` across all eight, plus that the fixed-recipe verbs did not gain it. `R/ffm.R` zero diff vs master (IP1/IP3). Gate: test() 1462 pass / 0 fail, `pkgdown::check_pkgdown()` clean, `document()` no diff, `R CMD check` Status: OK after `spelling::update_wordlist()` added FLAC + transcoding — the M17 lesson reproduced exactly (devtools::check() printed 0 notes while R CMD check showed 1 NOTE).
- 2026-07-26: all nine tasks complete; status review. Gate clean (test 1462/0, check Status: OK, pkgdown clean, document() no diff).

## Decisions

- 2026-07-26 (review): F5 was actioned although it scored 74, below the 80 bar. The score governs whether a *defect* is fixed; it does not license a Review section that claims evidence which was not gathered. AC5 says all four batch siblings, so the tests were added rather than the claim trimmed.

## Review

**PR:** https://github.com/jmgirard/tidymedia/pull/37 · reviewed 2026-07-26 on
`m35-audio-codec-reencode-verbs` (master unmoved since the branch was cut).

**AC1 — formals + Layer 1 untouched.** All eight verbs report
`audio_codec = "copy"`, each positioned before `hardware`, none carrying an
`acodec`/`codec` alias; `format_for_web`/`_batch` correctly did not gain it
(D016/D017 boundary rule). `git diff master..HEAD -- R/ffm.R` is empty — zero
Layer 1 diff, so IP1/IP3 hold.

**AC2 — only the added token changed.** Compiled 15 default invocations against
a detached master worktree and compared per line: 8 differ by exactly one
inserted `-codec:a copy` and nothing else, 7 are byte-identical, and **0 changed
in any other way**. The 7 unchanged are the predicted set — `segment_video`'s
copy path (already `-codec:a copy` via `ffm_copy`), the four composite forms
carrying no audio, `format_for_web` (fixed AAC recipe) and `standardize_video`
(already copying). `audio_codec = "aac"`, `NULL` and a non-token value are
covered by passing tests.

**AC3 — stream-copy conflict.** `segment_video(reencode = FALSE)` aborts for
both `"aac"` and `NULL`; the guard sits in `segment_pipeline()`, so a
`segment_video_batch` jobs table with a mixed per-row `reencode` column aborts
too, while the same table at the default compiles both rows with `-codec:a copy`.

**AC4 — composites.** Default (`audio = NULL`) emits no `-codec:a` on both verbs;
`audio = 0` emits `-codec:a copy` beside `-map 0:a`; one compiled command carries
`-filter_complex`, `[vout]`, `-map "[vout]"`, `-map N:a` and both codecs
together; a named encoder with no audio mapped aborts, `NULL` stays legal.

**AC5 — batch columns and the audio guards.** Per-row `audio_codec` verified on
all four siblings (character with `NA` → no token for that row); an all-`NA`
logical column accepted, all-`NA` numeric and numeric rejected. Both composites
now share `check_batch_audio_col()`: all-`NA` logical and all-`NA` numeric
accepted, character, all-`NA` character and `c(TRUE, FALSE)` rejected —
`compare_videos_batch` had no up-front guard at all before this.

**AC6 — execution.** `make_mp3_audio_video()` fixture (MP3 in MP4, where the
container default is AAC): the cropped output's `probe_audio()$codec_name` equals
the input's `mp3`, and `audio_codec = NULL` yields `aac`. Binary-gated.

**AC7 — gate.** `devtools::test()` 1465 pass / 0 fail / 4 skip (the M35 file
alone: 106 pass, 0 fail, 0 skip). `R CMD check` **Status: OK**
(0/0/0). `pkgdown::check_pkgdown()` clean. `devtools::document()` no diff. All
eight `.Rd` files name `audio_codec`; `format_for_web_batch.Rd` carries the
ignored-column cross-reference. `NEWS.md` has a Breaking changes section; no
milestone IDs in NEWS/README/vignettes. CI green (9 checks).

**Consistency gate.** `cairn_validate` exit 0, all checks passed, no advisories.
`cairn_impact --changed`: no changed principles in `DESIGN.md`. Toolchain slot
checks all recorded above. Returns to `in-progress` this milestone: 0.

**Independent review — three lenses + scorer.**

Blame-history [S] and prior-review-record [S] both no-op'd clean. The former
traced the two modified pinned tests to their M34 commits and judged the
coverage relocated (stricter `expect_equal` in the new file) rather than lost,
and confirmed `picture_in_picture_batch`'s replaced guard preserves the intent
M32's review gave it. The latter found M34's F2 lesson (all-NA typing) *applied*
rather than repeated, RR01 Beyond-1/Beyond-3 genuinely closed, Beyond-2 open and
uncontradicted; its GitHub review-thread probe returned empty, so that surface
was skipped by design.

Diff-bug [O] found **no functional defect** — it independently re-verified all
ten `*_pipeline()` call sites for the inserted parameter, the sentinel
round-trip, the per-row reality of the segment guard, and that `"copy"` is the
only value reaching the command without a `check_token()` pass. Six
documentation/hygiene findings, scored by a fresh [S] scorer:

Actioned (≥80):
- **F1 (92) — the branch silently converted `R/ffmpeg.R` from CRLF to LF**,
  rewriting all 4000 lines: the diff read 4172/3999 instead of the true 209/36
  and `git blame` on the largest source file pointed at this milestone. Caused by
  the implementation's Python rewrites. CRLF restored; diffstat now 209/36, and
  a sweep confirmed no other touched file changed endings.
- **F2 (85) — the new `format_for_web_batch` cross-reference pointed users at a
  verb that cannot solve their problem.** A reader hitting D017's FLAC-in-`.mp4`
  trap was sent to `standardize_video_batch()`, which has no `audio_codec` at all
  and hard-codes `audio = "copy"`. Reworded: the redirect is now scoped to
  per-row *video* codecs, states that verb stream-copies audio, and points at a
  verb that does take an audio codec. (The finding's claim that its `video_codec`
  is batch-wide was wrong — R/ffmpeg.R:2446 reads it per row.)
- **F5 (74) — actioned despite scoring below the bar, because AC fencing
  requires it:** the all-NA-logical and wrong-type column tests covered three
  siblings and skipped `segment_video_batch`, while AC5's wording and this
  Review's own evidence line claimed all four. The recorded evidence must be
  true, so the two boundary cases were added rather than the claim narrowed.

Logged; F4 and F6 actioned at the merge gate on the user's direction (<80):
- **F3 (45)** — M34's unreleased NEWS bullet says these verbs "compile exactly
  the commands they did before", which the new Breaking-changes bullet contradicts
  for the audio token. Scorer judged each locally true in isolation (M34's scopes
  to `-codec:v`); a wording nit.
- **F4 (78)** — actioned at the user's direction at the merge gate. Two byte-identity
  claims this diff falsified but left standing: a comment in `crop_video_pipeline()`
  and `test-video-codec.R`'s file header. Both narrowed to the video half; the
  header now also records that the composite pins are unaffected, since those
  verbs map no audio by default and stay byte-identical to pre-M34.
- **F6 (55)** — actioned at the user's direction at the merge gate; see D018
  and the GP2 addition to `Principles touched`. GP2 (frame-accurate cutting) is arguably traded on
  `segment_video`'s audio, which now snaps to a packet boundary (measured
  `start_time=0.007007` vs the old `0.000000`, under one audio frame; video
  identical). Neither the `Principles touched` slot nor D017 mentions it.
