# M52: Collapse `probe_one()`'s per-stream FFprobe loop into one call

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m52-probe-one-single-call`

## Goal

Cut `probe_one()` from N+1 FFprobe spawns per file to one, by reading the
container and every stream from a single compact-format call.

## Scope

**In:** `probe_one()` (`R/ffprobe.R:161-185`) issues one
`ffprobe -show_format -show_streams -of compact` call instead of a
`-show_format` call plus one `-select_streams` call per stream, and a
compact-line parser beside `format_probe()` (`R/ffprobe.R:278-284`) that does
four things the current parser does not have to: dispatch each line by its
keyless leading section field (`stream|…`, `format|…`, with the format line
arriving **last**); split on unescaped `|`; unescape `\n`, `\r`, `\|` and
`\\`; and restore the nested-section prefixes to the casing today's output
uses, since the compact writer emits `tag:` / `disposition:` where
`default=nw=1` emits `TAG:` / `DISPOSITION:`.

**Out:** parallelizing across files → M53. The batch verbs' up-front
dropped-track probe → the standing candidate row (its open question is API
shape, and the lazy option reopens the `ffm_batch()` hook D024/RR02 Q3
rejected). `count_audio_streams()` (`R/ffprobe.R:132-154`), already one spawn
and deliberately narrow. Any *net* change to which columns `probe_all()`
returns — the normalization above exists precisely to keep that promise, so a
renamed or extra column is a defect here, not a deliverable.

## Acceptance criteria

- [ ] AC1 On a fixture with at least three streams, `probe_one()` spawns
      exactly one FFprobe process, where the pre-change count is
      `nb_streams + 1`. Asserted by a mock that **counts** invocations, not by
      timing and not by a mock that errors (M44's lesson: a `stop()`ing mock
      proves nothing where the caller catches). The one-spawn count also holds
      on the early-return paths, which already spawn once
      (`R/ffprobe.R:167`, `:170-173`).
- [ ] AC2 `probe_all()`'s output is unchanged: for every fixture in the suite,
      both returned tibbles compare identical in names, column order, row
      order, types and values against a baseline recorded from the pre-change
      ref and committed before any source edit (T1). This is the criterion the
      writer switch most endangers — measured at plan time, an unnormalized
      compact parse renames every `TAG:`/`DISPOSITION:` column and adds a
      spurious `stream` column — so a passing AC2 is what proves the
      normalization complete.
- [ ] AC3 The parser round-trips every escape the compact writer emits,
      verified at plan time as `\n`, `\r`, `\|` and `\\`: a stream tag whose
      value contains a literal `|`, one containing an embedded newline, and one
      containing a carriage return each come back as the original string, in
      one cell, adding no column and no row.
- [ ] AC4 The same input fixes a latent corruption rather than merely avoiding
      one: a newline-bearing tag today makes `format_probe()` read the value's
      trailing lines as further `key=value` pairs and emit bogus columns
      (reproduced at plan time against the current per-stream call). After this
      milestone it is one cell. Evidence is a test that runs red against
      pre-change source — written in T3, before T4 rewrites `probe_one()` —
      and green after.
- [ ] AC5 `typed = TRUE` and `typed = FALSE` both produce output identical to
      their recorded baselines, and the resilience contract is intact — a file
      with no readable streams, an unprobeable file, and a mixed vector of both
      still yield all-`NA` rows plus one warning rather than an abort
      (`R/ffprobe.R:75-94`, documented at `:48-50`).
- [ ] AC6 `devtools::test()` clean and `devtools::check()` reports 0 errors /
      0 warnings; NEWS records the speedup in user-facing terms.

## Coverage

- AC1 → T2, T4
- AC2 → T1, T3, T4
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T1, T5
- AC6 → T6

## Tasks

- [ ] T1 Record and commit the pre-change baseline before touching source:
      `probe_all()` output for every suite fixture under both `typed` values,
      plus the current per-file spawn count (M44's lesson — `git checkout`
      restores from the index, so a baseline taken on uncommitted work is
      worthless).
- [ ] T2 Tests first: the counting mock over `run_program()` that pins the
      per-file spawn count, red at `nb_streams + 1`.
- [ ] T3 Write the compact-line parser beside `format_probe()` with its own
      unit tests — section dispatch, unescaping all four escapes, prefix
      normalization, and the AC4 regression, all red before T4.
- [ ] T4 Rewrite `probe_one()` onto the single call and route it through the
      new parser.
- [ ] T5 Resilience and `typed` parity tests against T1's baseline.
- [ ] T6 NEWS; run the profile's verify slot and `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose `-of compact` over `-of default` section markers and over `-of json` + jsonlite: markers are marginally faster but a value carrying a bare `[/STREAM]` line mis-splits the stream table across row boundaries, and json is robust but makes jsonlite an Imports on a package whose scope discipline (GP1/D001) leans against growth; falsified by a compact-writer value that escapes neither `|` nor newline, which would put json back in play.
- 2026-07-31: plan measurements (ffmpeg 8.1.2, macOS; a 5-stream `.mkv`, ten copies): today 60 spawns / 1.700 s; one call with section markers 10 spawns / 0.267 s; one call with `-of compact` 10 spawns / 0.358 s (~4.7x). A title tag of `a\n[/STREAM]\nb` yields six bare close-markers for five streams under the marker writer, and corrupts the CURRENT per-stream output too; `-of compact` renders it `\n`, a literal `|` as `\|` and a CR as `\r`. T1 re-records the spawn count on the branch.
- 2026-07-31: the question gate ran before the criteria audit this once, because the writer choice determined which criteria existed to audit; the audit ran on the drafted wording before commit, which is what it exists for.
- 2026-07-31: criteria audit ([O], fresh context) returned five findings, all fixed above and all confirmed by re-measurement. The important one: AC2 was unsatisfiable as drafted, because `-of compact` emits `tag:`/`disposition:` where `default=nw=1` emits `TAG:`/`DISPOSITION:` and prepends a keyless section field, so byte-identical columns need normalization the Scope had not named. Also: the format line arrives last in a combined call, so the parser must dispatch by section; the escape set includes `\r`, which the Scope had omitted; AC4 demanded a test failing on a ref that never had it; and the resilience contract was cited at `:249-258`, which is `filter_streams()`, not `:75-94`.
- 2026-08-06: implementation started on `m52-probe-one-single-call`; question gate skipped, nothing genuinely open (the writer choice was settled at plan time and the parser shape is fixed by AC2's unchanged-output promise).
- 2026-08-06: re-measured the compact writer's escape set on ffmpeg 8.1.2 before any edit, confirming the plan's four escapes and that `=` is deliberately unescaped, so a per-field split on the first `=` still holds. AC4's corruption reproduces: a newline-bearing tag under `default=nw=1` emits a bare continuation line that `format_probe()` reads as a `key=value` pair.

## Decisions

## Review
