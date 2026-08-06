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
arriving **last**); split on unescaped `|`; unescape the **six** sequences the
writer emits — `\\`, `\|`, `\n`, `\r`, `\b` and `\f`; and restore the nested-section prefixes to the casing today's output
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
- [ ] AC2 `probe_all()`'s output is unchanged: for every fixture in the suite
      **except the escape fixture**, both returned tibbles compare identical in
      names, column order, row order, types and values against a baseline
      recorded from the pre-change ref and committed before any source edit
      (T1). The escape fixture is excluded here and owned by AC4, whose whole
      subject is that the pre-change output on it is *wrong* — its baseline
      records a truncated tag value and a bogus `break` column, so an
      identical-output assertion there would pin the corruption in place. This
      is the criterion the writer switch most endangers — measured at plan time, an unnormalized
      compact parse renames every `TAG:`/`DISPOSITION:` column and adds a
      spurious `stream` column — so a passing AC2 is what proves the
      normalization complete.
- [ ] AC3 The parser round-trips every escape the compact writer emits,
      re-measured during implementation as **six** rather than the four
      recorded at plan time: `\\`, `\|`, `\n`, `\r`, `\b` and `\f`. BEL, TAB
      and vertical tab were measured passing through raw and need no decoding,
      which is why the set is six and not nine. A stream tag whose value
      contains each of the six, and one carrying a raw byte the writer leaves
      alone, each come back as the original string, in one cell, adding no
      column and no row.
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

- [x] T1 Record and commit the pre-change baseline before touching source:
      `probe_all()` output for every suite fixture under both `typed` values,
      plus the current per-file spawn count (M44's lesson — `git checkout`
      restores from the index, so a baseline taken on uncommitted work is
      worthless).
- [x] T2 Tests first: the counting mock over `run_program()` that pins the
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
- 2026-08-06: T1 done. `data-raw/probe-baseline.R` records five synthetic fixtures against a git ref, pairing the combined call's raw compact text with the pre-change `probe_one()`/`probe_all()` tibbles built from the same file, so AC2 becomes a binary-free pure-function test rather than a re-probe. It reuses M41's `codec_guard_env()` ref loader rather than copying it. Committed as `tests/testthat/fixtures/probe-baseline.rds`.
- 2026-08-06: the baseline captured AC4's corruption as recorded pre-change fact: on the escape fixture the per-stream parse truncates the audio title tag to `line` and emits a bogus `break` column holding `break`. The recorded pre-change spawn counts are 3/5/2/2/3 for the five fixtures, each `nb_streams + 1`.
- 2026-08-06: substantive amendment, user-approved at a mini gate — AC2 now exempts the escape fixture, which AC4 owns. As written the two criteria contradicted each other on that one file: AC2 demanded byte-identical output against a baseline that records the corruption AC4 exists to remove, so satisfying both was impossible and satisfying AC2 literally would have pinned the bug.
- 2026-08-06: T2 done and RED by design — the suite stays red until T4 lands the single call, so the profile's verify slot is not clean at this checkpoint. `test-probe-single-call.R` counts `run_program()` invocations through a mock that delegates to the real binding, so the tibbles stay real. Three of its four assertions fail at the pre-change counts; the unprobeable-file case already spawns once and is pinned as an invariant rather than a change.
- 2026-08-06: substantive amendment, user-approved at a mini gate — the Scope's escape list and AC3 widen from four sequences to six. Byte-level measurement (ffmpeg 8.1.2, one tag per byte, read with `od -c` so a raw control byte is told apart from a two-character escape) shows the writer also escapes `\b` and `\f`, while BEL, TAB and vertical tab pass through raw. Decoding only the planned four would return a form feed as a literal backslash-f, the corruption class AC3 exists to prevent. The plan-time note that `\a` is escaped was an artifact of reading `od -c`'s rendering of a raw BEL as an escape.

## Decisions

## Review
