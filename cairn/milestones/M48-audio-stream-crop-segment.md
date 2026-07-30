# M48: Carry the track selector to `crop_video()` and `segment_video()`, and make `ffm_copy()` idempotent again

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M47
- **Driving RR:** RR03
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** `m48-audio-stream-crop-segment`

## Goal

Put `crop_video()` and `segment_video()` (+ `_batch`) on M47's map rule, and stop
a repeated `ffm_copy()` duplicating every output stream.

## Scope

**In:** `audio_stream` on `crop_video()`, `segment_video()`,
`crop_video_batch()`, `segment_video_batch()`, under M47's rule —
`-map 0:v? -map 0:a?` when `NULL`, `-map 0:v? -map 0:a:<n>` when named,
on both `segment_video()` branches. On the `reencode = FALSE` branch the map
replaces `ffm_copy()`'s `-map 0` rather than appending beside it, which gives
`ffm_map(replace = TRUE)` its first in-package caller (M43 shipped it with none).
`ffm_copy()` and `ffm_concat()` become idempotent again. NEWS.

**Out:**
- D018's GP2 trade on `segment_video()`'s audio stream: `audio_stream` selects
  which track carries the packet-boundary cut, never how it is cut.
- Subtitle and data carriage, and a `subtitle_stream`/`video_stream` selector →
  the existing M45-Out candidate row. `crop_video()` carries a subtitle today
  into `.mkv` and stops; the same change makes it stop failing into `.mp4`.
- The `audio =` (D009) documentation reconciliation → new candidate row.
- `hardware = "nvenc"` probing under `run = FALSE` → new candidate row (M47 Out).

## Acceptance criteria

- [ ] AC1 With `audio_stream` unset, `crop_video()`, `segment_video(reencode =
      TRUE)` and `segment_video(reencode = FALSE)` each compile exactly two
      `-map` arguments — `-map 0:v?` then `-map 0:a?` — asserted as committed
      literal command strings.
- [ ] AC2 With `audio_stream = 2` each of those three compiles exactly two,
      `-map 0:v?` then `-map 0:a:2`; on the `reencode = FALSE` branch no
      `-map 0` survives, the selector narrowing `ffm_copy()`'s map via
      `ffm_map(replace = TRUE)` rather than appending beside it.
- [ ] AC3 With ffmpeg present, on a 3-audio-track, 1-subtitle `.mkv`:
      `crop_video(audio_stream = 2)` into `.mkv` writes exactly one audio
      stream and it is `fra`; and `crop_video()` into `.mp4` exits 0, where on
      master it fails (measured exit 8, no default mp4 subtitle encoder).
- [ ] AC4 With ffmpeg present a doubled `ffm_copy()` over a 5-stream `.mkv`
      writes 5 streams, not the 10 master writes.
- [ ] AC5 Both `_batch` siblings take an `audio_stream` argument and an
      `audio_stream` jobs column overriding it per row, `NA` being the column form
      of `NULL`; a one-row batch call compiles byte-identically to the scalar call;
      and `segment_video()`'s own fan-out carries it to every segment it produces.
- [ ] AC6 A wrongly typed `audio_stream` column aborts before any row runs,
      naming the column and saying `NA` keeps every audio track.
- [ ] AC7 At the default `hardware`, no entry point runs a binary when
      `run = FALSE` (counting mock over `run_program()`, `find_ffmpeg()`,
      `find_ffprobe()`).
- [ ] AC8 The per-verb map-count invariant in `tests/testthat/test-ffm.R` states
      the rule every verb now follows and covers each one M47 and M48 touched;
      `NEWS.md` records the argument, the `ffm_copy()` fix and its new abort, and
      the subtitle-carriage change; `devtools::document()` produces no diff,
      `devtools::test()` is clean, `devtools::check()` 0 errors / 0 warnings.
- [ ] AC-9 (BC1): `ffm_copy(streams = TRUE)` sets the pipeline map by assignment through
      `ffm_map(object, "0", replace = TRUE)`; after `ffm_copy() |> ffm_copy()` and
      after `ffm_concat() |> ffm_copy()`, `ffm_args()` contains exactly one
      `"-map"` token (tolerance: exact) and the token following it is `"0"`.
- [ ] AC-10 (BC2): `ffm_map()`'s contract is unchanged: no de-duplication is added to
      `ffm_map()`, and the tests at `tests/testthat/test-ffm.R:407` (append) and
      `:417` (`replace = TRUE` narrows) pass without modification.
- [ ] AC-11 (BC3): `ffm_copy(streams = TRUE)` on a pipeline whose map is non-empty and not
      identical to `"0"` aborts with a classed `cli` condition whose message names
      `streams = FALSE`; the pinned failing case is
      `ffm_map("0:v") |> ffm_copy()`, and the message is worded around the
      pipeline's existing map rather than presuming the user called `ffm_copy()`
      directly.
- [ ] AC-12 (BC4): `ffm_map("0:v") |> ffm_copy(streams = FALSE)` compiles `-codec:v copy
      -codec:a copy` and exactly one `-map` token, `"0:v"` (tolerance: exact).
- [ ] AC-13 (BC5): With ffmpeg present, a doubled `ffm_copy()` remux of a multi-stream
      `.mkv` fixture writes an output whose ffprobe stream count exactly equals
      the input's (tolerance: 0); the test `skip_if`s when ffmpeg is absent.
- [ ] AC-14 (BC6): `strip_metadata()`'s compiled command is byte-identical to its
      pre-M48 master baseline, asserted as a committed literal.
- [ ] AC-15 (BC7): `R/ffm.R`'s `@param streams` prose no longer documents appending; it
      states the assignment and the abort; `NEWS.md` records both the idempotence
      fix and the new abort; `devtools::document()` produces no diff.
- [ ] AC-16 (BC8): A decision entry records the `ffm_copy()` contract (assigns;
      conflicting prior map aborts; `ffm_map()` still appends) without editing
      D023's existing bullets.

### Deviations from RR03

| BC | Departure | Why |
|---|---|---|
| BC6 | "byte-identical to its pre-M48 master baseline, asserted as a committed literal" is satisfied against a committed `sprintf()` **template** recording that form (`-y -i "%s" -codec:v copy -codec:a copy -map_metadata -1 -map_chapters -1 -fflags +bitexact -map 0 "%s"`), the input and output paths being the only substitutions. | The compiled command embeds the absolute input path, which in tests is a `withr` tempfile, so a byte-identical committed literal cannot exist. The `baseline_pair()` pattern BC6 cites (`test-separate-av-multitrack.R:32-37`) is itself a template. Agreed at the 2026-07-30 ingest gate. |

## Coverage

- AC1 → T1, T3, T4
- AC2 → T3, T4
- AC3 → T7
- AC4 → T2
- AC5 → T4, T5
- AC6 → T5
- AC7 → T3, T4, T5
- AC8 → T6, T7
- AC9 → T2
- AC10 → T2
- AC11 → T2
- AC12 → T2
- AC13 → T2
- AC14 → T2
- AC15 → T2, T7
- AC16 → T2

## Tasks

- [x] T1 Record the three current commands as committed literals (the
      `baseline_pair()` pattern, `test-separate-av-multitrack.R:32-37`) and add
      the failing-first compile tests.
- [x] T2 Restore `ffm_copy()`/`ffm_concat()` idempotence per RR03: `ffm_copy()`
      sets its map with `ffm_map(object, "0", replace = TRUE)` (`R/ffm.R:639`),
      `ffm_map()` untouched, plus a `tidymedia_*`-classed abort when
      `streams = TRUE` meets a non-empty map not identical to `"0"` — message
      pipeline-state-worded (`ffm_concat()` calls `ffm_copy()` internally) and
      naming both `streams = FALSE` and `ffm_map(replace = TRUE)`. New tests go
      BELOW `test-ffm.R:417` so AC-10's line references stay true. Add the
      doubled-copy compile and execution tests, the guard and escape tests, the
      `strip_metadata()` baseline template, rewrite `ffm_copy()`'s `@param
      streams` (`R/ffm.R:610-613`) and touch the M43 comment at `:586-590`.
- [x] T3 `crop_video()` / `crop_video_pipeline()` (`R/ffmpeg.R:1103`, `:1040`):
      argument before `run`, guard last in the front-door block (M41), and
      replace `ffm_map(p, "0")` (`:1047`) with M47's resolver.
- [x] T4 `segment_pipeline()` (`R/ffmpeg.R:2620`) on both branches, and
      `segment_video()` (`:2523`) carrying the argument into the internal jobs
      tibble it builds. Ordering constraint from T2's guard: the
      `ffm_map(..., replace = TRUE)` narrowing must stay AFTER the
      `if (!reencode) ffm_copy(p)` line — hoisting it above, the shape
      `standardize_pipeline()` uses, aborts every `reencode = FALSE` call.
- [ ] T5 `crop_video_batch()` (`R/ffmpeg.R:4396`) and `segment_video_batch()`
      (`:2743`): argument, `check_batch_audio_col(jobs, "audio_stream",
      na_means = …)`, `batch_stream_cell()` in each closure.
- [x] T6 Rewrite the per-verb map-count invariant in `tests/testthat/test-ffm.R`
      to the new rule and extend it to every verb M47 and M48 touched.
- [ ] T7 Roxygen on all four plus the `@param jobs` enumerations (M39);
      `devtools::document()`; execution tests on the multi-track and
      subtitle fixtures; NEWS.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose to fold the `ffm_copy()`/`ffm_concat()` idempotence fix in here over leaving it a candidate row, because this milestone narrows `ffm_copy()`'s map on `segment_video(reencode = FALSE)` and so re-enters and re-reads that contract anyway — the promotion condition the candidate row itself named; falsified by the fix needing tests or a design call that outgrow this milestone's budget.
- 2026-07-30: plan gate chose to keep `crop_video` and `segment_video` in one milestone over isolating `segment_video`, because crop is a single pipeline line and the shared `check_batch_jobs()` while segment carries the branch split and the fan-out, giving one milestone of roughly M43's size rather than a third planning cycle for a trivial verb; falsified by segment's two branches costing more than a working session on their own.
- 2026-07-30: status -> in-progress on branch `m48-audio-stream-crop-segment`.
- 2026-07-30: implement gate amended AC1/AC2's map literal from `-map 0:v` / `-map 0:a` to `-map 0:v?` / `-map 0:a?` — the criteria were written before M47 implementation added the trailing `?`, which D026's third bullet records as load-bearing (a bare `-map 0:a` aborts FFmpeg at exit 234 on a video-only input); the named specifier keeps no `?`. The same correction was already made to the ROADMAP row at 0445a62.
- 2026-07-30: implement gate kept the two adjacent candidate rows out of scope — `format_for_web`/`normalize_audio`'s missing `-map`, and always-quoting map specifiers (117 literals across 15 test files) — because M48 already sits at the >~7-criteria split tripwire; both rows stand as written.
- 2026-07-30: implement gate escalated T2's `ffm_copy()` idempotence spelling via `/milestone-brief` (RB tripwire: irreversible-api). The session's recommendation was `ffm_copy(streams = TRUE)` calling `ffm_map(replace = TRUE)`, leaving `ffm_map()`'s appending contract untouched; the user chose Fable review over settling it here.
- 2026-07-30: blocked on RB03 (`cairn/reviews/RB03-ffm-copy-idempotence.md`) — seven questions on which spelling restores `ffm_copy()`/`ffm_concat()` idempotence, carrying options A (`ffm_copy()` uses `replace = TRUE`), B (`unique()` in `ffm_map()`) and C (`ffm_copy()` appends `"0"` only when absent), plus whether the fix should signal rather than stay silent.
- 2026-07-30: ingest audit of RR03's BC1–BC8 by a fresh-context [O] reader — verified every line reference and reproduced all three doubling compositions; found BC6 unsatisfiable as written and four criteria weaker than the report's own recommendations, plus an unstated ordering constraint on `segment_pipeline()`. Findings recorded in M48-D2 and raised at the ingest gate; none softened.
- 2026-07-30: ingested RR03 — `Driving RR: RR03`, BC1–BC8 as AC-9…AC-16 verbatim with Coverage lines, one Deviations row for BC6 agreed at the gate, `ffm_copy()` contract promoted to D027, T2/T4 amended, status back to in-progress. AC1–AC8 compressed in one pass to hold the 150-line cap (148/149), which also retired three stale `R/ffmpeg.R` line references in T3/T4/T5 and the `test-ffm.R:438` reference in AC8/T6.
- 2026-07-30: T1 — `test-audio-stream-crop-segment.R` records the three pre-M48 commands as committed templates (from master at 0b9985a) and adds the AC1/AC2 compile tests. Red as intended: 10 failures, all `unused argument (audio_stream = …)` or the old `-map 0`; T3/T4 turn them green.
- 2026-07-30: T2 — `ffm_copy()` assigns its map via `ffm_map(..., replace = TRUE)` and `check_copy_map_conflict()` aborts (`tidymedia_copy_map_conflict`) on a different stated mapping; `ffm_map()` untouched; new `make_multitrack_subtitle_video()` 5-stream fixture; seven tests below `test-ffm.R:417` covering the doubled copy, the concat composition, the guard through both entry points, the literal `identical("0")` carve-out, the `streams = FALSE` escape, the `strip_metadata()` template and the 5-in/5-out execution case.
- 2026-07-30: T3, T4 — `audio_stream` on `crop_video()`/`crop_video_pipeline()` and `segment_video()`/`segment_pipeline()`, both branches, via `pass_through_maps()`. T3 omits the scalar front-door guard, following M47 review F8 rather than the task's own wording: it would be the only guard reporting before `ffm_crop()`'s dimension checks (M41's precedence trap), and `pass_through_maps()` carries the identical check with `call` resolving to the verb. `segment_pipeline()`'s narrowing sits below its `ffm_copy()` call, per the ingest audit.
- 2026-07-30: T6 — the map-count invariant now states the three-number rule (2 pass-through / 1 single-stream-or-all-streams / 0 unstated) and covers 13 entry points, adding `concatenate_videos`, `format_for_web` and `normalize_audio`; the two zeros pin the standing candidate row's gap so closing it is visible here. Nine existing baselines updated across five test files where `-map 0` or an adjacent seek moved. `devtools::test()` clean.

## Decisions

**2026-07-30 — M48-D1: RR03 ingested; the `ffm_copy()` contract promoted to D027.**
RR03 answered T2's escalated question with option A hardened by a guard:
`ffm_copy()` assigns its map via `ffm_map(object, "0", replace = TRUE)`,
`ffm_map()` untouched, plus an abort when a conflicting prior map is present.
The substance is cross-cutting (a Layer-1 contract future milestones read), so
it is recorded as **D027** rather than here; this entry records the triage.
Applied: recommendations 1–4 (option A, the guard, the decision entry + NEWS +
roxygen, the Q7 test set), all as binding criteria AC-9…AC-16. Applied also
recommendation 5 (one roxygen sentence noting `ffm_map()` is the builder's only
accumulating verb) as part of T7. Recommendation 6 applied as a ROADMAP
candidate-row edit, not milestone work. Rejected by the report and not
relitigated here: option B (`unique()` in `ffm_map()`), option C (append `"0"`
only when absent), any `lifecycle` affordance, and a warning in place of the
abort.

**2026-07-30 — M48-D2: the ingest audit's findings, and the one departure.**
The fresh-context [O] audit of BC1–BC8 verified every line reference and
reproduced all three doubling compositions, and found **BC6 unsatisfiable as
written** — `strip_metadata()`'s command embeds the absolute input path, a
`withr` tempfile in tests, so no committed *literal* can be byte-identical; the
`baseline_pair()` pattern BC6 cites is itself a `sprintf()` template. BC6 is
ingested verbatim with the template reading recorded as the single row of the
Deviations table, agreed at the ingest gate. Four criteria are satisfiable but
weaker than the report's own recommendations, and are met by implementing the
stronger reading rather than by departing: BC3 names only `streams = FALSE`
where recommendation 2 requires both spellings (and `streams = FALSE` is
unreachable on the `ffm_concat()` path) and says "classed" without naming a
class, so the abort takes an explicit `tidymedia_*` class per the package's
precedent (`R/ffmpeg.R:384`, `:657`) and names both; BC2 identifies two tests by
line number, so T2's new tests go below `test-ffm.R:417`; BC5 says
"multi-stream" where AC4 says five, so the fixture carries five streams; BC7
says "`R/ffm.R`'s `@param streams`" where the file has two such blocks, so
`ffm_copy()`'s (`:610-613`) is the one rewritten. The audit also surfaced an
ordering constraint no criterion states — `segment_pipeline()` must keep the
`ffm_map(..., replace = TRUE)` narrowing *after* its `ffm_copy()` call or the
new guard aborts every `reencode = FALSE` segment — now pinned in T4.

## Review
