# M52: Collapse `probe_one()`'s per-stream FFprobe loop into one call

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m52-probe-one-single-call` / https://github.com/jmgirard/tidymedia/pull/55

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
writer emits — `\\`, `\|`, `\n`, `\r`, `\b` and `\f`; and give each
nested-section key the name today's output uses, which is two
different repairs: the compact writer emits `tag:` / `disposition:` where
`default=nw=1` emits `TAG:` / `DISPOSITION:`, and it prefixes stream side data
(`side_datum/display_matrix:rotation`) where `default=nw=1` prints it bare
(`rotation`), so that prefix is dropped rather than cased.

**Out:** parallelizing across files → M53. The batch verbs' up-front
dropped-track probe → the standing candidate row (its open question is API
shape, and the lazy option reopens the `ffm_batch()` hook D024/RR02 Q3
rejected). `count_audio_streams()` (`R/ffprobe.R:132-154`), already one spawn
and deliberately narrow. Any *net* change to which columns `probe_all()`
returns — the normalization above exists precisely to keep that promise, so a
renamed or extra column is a defect here, not a deliverable.

## Acceptance criteria

- [x] AC1 On a fixture with at least three streams, `probe_one()` spawns
      exactly one FFprobe process, where the pre-change count is
      `nb_streams + 1`. Asserted by a mock that **counts** invocations, not by
      timing and not by a mock that errors (M44's lesson: a `stop()`ing mock
      proves nothing where the caller catches). The one-spawn count also holds
      on the early-return paths, which already spawn once
      (`R/ffprobe.R:167`, `:170-173`).
- [ ] AC2 `probe_all()`'s output is unchanged: for every fixture in the suite
      **except those whose recorded pre-change output is itself corrupt**, both
      returned tibbles compare identical in names, column order, row order,
      types and values against a baseline recorded from the pre-change ref and
      committed before any source edit (T1). Two fixtures are exempt, both for
      the same reason and neither for convenience: the escape fixture, owned by
      AC4, and the rotated fixture, whose multi-line `displaymatrix` value the
      pre-change parser read as three further `key=value` columns. On an exempt
      fixture the requirement is instead that every pre-change column which is
      not one of those artifacts survives with its pre-change name and value —
      `rotation` in particular, which review round 1 found renamed away — and
      that the new output adds no column and no row. This is the criterion the
      writer switch most endangers — an unnormalized compact parse renames every
      `TAG:`/`DISPOSITION:` column, adds a spurious `stream` column, and renames
      stream side-data columns out of existence — so a passing AC2 is what
      proves the normalization complete.
- [x] AC3 The parser round-trips every escape the compact writer emits,
      re-measured during implementation as **six** rather than the four
      recorded at plan time: `\\`, `\|`, `\n`, `\r`, `\b` and `\f`. BEL, TAB
      and vertical tab were measured passing through raw and need no decoding,
      which is why the set is six and not nine. A stream tag whose value
      contains each of the six, and one carrying a raw byte the writer leaves
      alone, each come back as the original string, in one cell, adding no
      column and no row.
- [x] AC4 The same input fixes a latent corruption rather than merely avoiding
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
- [x] AC6 `devtools::test()` clean and `devtools::check()` reports 0 errors /
      0 warnings; NEWS records the speedup in user-facing terms.

## Coverage

- AC1 → T2, T4
- AC2 → T1, T3, T4, T7, T8
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T1, T5, T7, T8
- AC6 → T6, T9

## Tasks

- [x] T1 Record and commit the pre-change baseline before touching source:
      `probe_all()` output for every suite fixture under both `typed` values,
      plus the current per-file spawn count (M44's lesson — `git checkout`
      restores from the index, so a baseline taken on uncommitted work is
      worthless).
- [x] T2 Tests first: the counting mock over `run_program()` that pins the
      per-file spawn count, red at `nb_streams + 1`.
- [x] T3 Write the compact-line parser beside `format_probe()` with its own
      unit tests — section dispatch, unescaping all four escapes, prefix
      normalization, and the AC4 regression, all red before T4.
- [x] T4 Rewrite `probe_one()` onto the single call and route it through the
      new parser.
- [x] T5 Resilience and `typed` parity tests against T1's baseline.
- [x] T6 NEWS; run the profile's verify slot and `devtools::check()`.
- [x] T7 Widen the baseline fixture family to the inputs the writer switch
      actually changes — a rotated video carrying display-matrix side data —
      and re-record the pre-change baseline against `master`, mapping the
      `five` fixture's fifth input so its name is true.
- [x] T8 Restore side-data column parity (the compact writer's side-data
      prefix is stripped, not uppercased) and make field splitting and
      unescaping byte-safe, so a line invalid in the session locale keeps its
      row; re-verify AC2 and AC5 against the widened baseline.
- [x] T9 Correct the NEWS claim that the returned tibbles are unchanged and the
      stale per-stream cost comment; re-run the verify slot and
      `devtools::check()`.

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
- 2026-08-06: T3 done. `parse_compact_probe()` and three helpers land beside `format_probe()`; `test-probe-compact-parser.R` is binary-free and green, rebuilding the recorded pre-change tibbles from the recorded text for the four non-escape fixtures. Field splitting walks the characters rather than substituting a placeholder byte, because the writer passes BEL, TAB and vertical tab through raw, so no byte is free to stand in for a separator. Unescaping is one pass via `regmatches<-`; sequential `gsub()`s would decode `\\n` into a newline.
- 2026-08-06: AC4 asks for evidence that runs red against pre-change source, which a recorded-versus-parsed comparison cannot give since it is green in both directions. Added the live counterpart in `test-probe-single-call.R` over a new `make_hostile_tag_video()` helper; it is red now on both the bogus `break` column and the truncated tag value, and the recorded baseline stands beside it as the frozen half.
- 2026-08-06: `format_probe()` kept for now so this checkpoint still loads; its only two callers are the `probe_one()` sites T4 replaces, so it and its test go there.
- 2026-08-06: T4 done. `probe_one()` is one `run_program()` call through `parse_compact_probe()`; the writer options `print_section=1:nokey=0:escape=c` are pinned rather than inherited because the parser depends on all three. Full suite green. `format_probe()` and its test removed as dead; its first-`=`-only contract is re-asserted against the new parser rather than dropped.
- 2026-08-06: measured on the branch, ten probes of a 4-stream file: 1.709 s before, 0.456 s after (3.75x), spawns per file 5 to 1. The plan's 4.7x was on a 5-stream file, so the two are consistent rather than in conflict — the win grows with stream count.
- 2026-08-06: T5 done. `test-probe-typed-resilience.R` feeds the recorded text to the REAL `probe_all()` through a mocked `run_program()`/`find_ffprobe()` pair, so `typed` parity is checked through the package's own composition rather than a test-side copy of it, and the file needs no binary. Both `typed` values reproduce the recorded pre-change output on all four non-escape fixtures. Resilience covered as four cases: unprobeable, no readable streams, a mixed vector, and two failures warning once rather than twice. Full suite 3397 passing, 0 failures, 5 skips.
- 2026-08-06: T6 done. NEWS gains a `## Performance` section for the single-call read and a `## Bug fixes` entry for the newline-tag corruption; both claims are enforced by named tests, and the timing figure is stated as a local measurement rather than a guarantee. `devtools::document()` produces no diff and `devtools::check()` is 0 errors / 0 warnings / 0 notes. Status to `review`.

- 2026-08-06: review round 1 RETURNED to `in-progress` (defect return #1). What failed: `probe_all()` renames stream side-data columns, so `$streams$rotation` — present on essentially every phone video — is `"90"` on `master` and absent on this branch, which the Scope's Out clause names a defect rather than a deliverable (F1, scored 96, verified end-to-end at review). Two more actioned: a tag byte invalid in the session locale makes the whole stream row vanish silently where the old parser errored loudly (F2, 88), and NEWS asserts the returned tibbles are unchanged (F4, 85). CI was green on all three workflows and every gate check passed; the fixture family is what missed this, being lavfi-synthesized with no side data and no non-ASCII bytes (F3, 74). AC2 and AC5 unticked pending re-verification against a widened fixture set; AC1/AC3/AC4/AC6 keep their evidence.

- 2026-08-06: return-1 gate. F1's cause is narrower than casing: `default=nw=1` prints stream side data with NO prefix at all, so parity strips `side_datum/<type>:` rather than uppercasing it, and `tag:`/`disposition:` stay the only prefixes that uppercase. F2's trigger is the session locale rather than the file — ffmpeg's own muxers rewrite an invalid tag byte to U+FFFD on write and macOS refuses such a filename, so the reproduction lives at the parser, where `strsplit(line, "")` returns `NA` on a string invalid in `LC_CTYPE` and the row silently vanishes; user chose byte-safe parsing that keeps the row over restoring the pre-change abort.
- 2026-08-06: substantive amendment, user-approved at a mini gate — AC2's exemption widens from the escape fixture to any fixture whose recorded pre-change output is itself corrupt, stating what must hold instead. Measured cause: the pre-change writer prints the display-matrix side data's value across four lines, so `master`'s own parse of a rotated `.mp4` emits three bogus columns named after matrix rows; demanding byte-identical output there would pin that corruption exactly as it would have on the escape fixture.
- 2026-08-06: T7 done and RED by design, as T2 was: the baseline now carries a sixth `rotated` fixture and the parser tests loop over it, so the suite stays red until T8 lands the side-data parity. Re-recorded against `master`: six fixtures, `five` now genuinely five streams (its fifth input was never mapped), pre-change spawn counts 3/6/2/2/3/2, still `nb_streams + 1` each. The rotated fixture's recorded pre-change streams tibble is 68 columns, three of them the display matrix's continuation lines read as columns, with `rotation` = `90` beside them.
- 2026-08-06: T8 done. `compact_section_case()` becomes `compact_key_name()`: `tag:`/`disposition:` still uppercase, `side_datum/<type>:` is now stripped, and an unrecognized prefix is left alone rather than guessed at, since a wrong rename is silent where a compact-shaped name is visible. Verified end to end on a rotated `.mp4`: `probe_all(f)$streams$rotation` is `90` again, no matrix-row column remains, and the matrix is one cell. Every parser operation moved to `useBytes = TRUE`, which keeps the row for a line invalid in the session locale and also retires the per-character split review measured at ~21 ms on a 200 KB line.
- 2026-08-06: on the rotated fixture the "artifact" AC2 exempts is read as the whole split — the columns the old parser cut out AND the `displaymatrix` cell it truncated to `""` — exactly as `hostile` is read, where AC4 owns both the bogus `break` column and the truncated `TAG:title`. Every other column, `rotation` and `side_data_type` included, is asserted identical to the recorded pre-change value under both `typed` settings.
- 2026-08-06: T9 done. NEWS no longer claims the returned tibbles are wholly unchanged; the Performance entry excepts the invented columns and the Bug fixes entry names the rotated-video case those columns come from, a claim the new fixture's test fails without. `count_audio_streams()`'s comment no longer says `probe_all()` costs a process per stream. Full suite: 0 failures, 3422 passing, 5 skips, 4 warnings that are the dropped-track diagnostic firing in tests this branch does not touch. `devtools::document()` produces no diff; `devtools::check()` is 0 errors / 0 warnings / 0 notes.
- 2026-08-06: substantive amendment, user-approved at a mini gate — the Scope's In clause said the nested-section repair was casing only, which is the reading that produced the defect return; it now names both repairs, casing for `tag:`/`disposition:` and prefix removal for side data. Status back to `review`; AC2 and AC5 stay unticked for review to re-verify against the widened fixture set.
- 2026-08-06: two below-threshold review findings gated in because they sit in files this pass edits anyway: the `count_audio_streams()` comment's per-stream cost claim (F15) and the `five` fixture's unmapped fifth input (F18).

## Decisions

## Review

Reviewed 2026-08-06 on `m52-probe-one-single-call`, PR #55. All evidence below
is from commands run at review, not from implementation-time transcripts.

**AC1 — one spawn per file.** `test-probe-single-call.R` counts `run_program()`
invocations through a mock that delegates to the real binding, so the returned
tibbles stay real; 10 assertions pass. On the 5-stream fixture the count is 1
and the streams tibble carries 5 rows. The early-return paths (video-only,
audio-only) and the unprobeable path each count 1. The pre-change counts are
recorded in the T1 baseline as `nb_streams + 1` across all five fixtures
(3/5/2/2/3 for 2/4/1/1/2 streams).

**AC2 — output unchanged.** `test-probe-compact-parser.R` rebuilds the recorded
pre-change tibbles from the recorded compact text for the four non-escape
fixtures; names, column order, row order, types and values all compare
identical. Column counts match exactly per fixture: container 15/12/15/15,
streams 68/68/62/50. The escape fixture is exempt by the 2026-08-06 amendment
and is AC4's.

**AC3 — every escape round-trips.** The six sequences the writer emits (`\\`,
`\|`, `\n`, `\r`, `\b`, `\f`) each round-trip to the original string in one
cell, adding no column and no row; a raw TAB, which the writer leaves alone, is
covered in the same test. 75 assertions pass in the parser file.

**AC4 — the latent corruption is fixed, not merely avoided.** Two independent
pieces of evidence. Recorded: the T1 baseline holds the pre-change output for
the escape fixture, where the newline-bearing tag yields 69 columns including a
bogus `break` column and a title truncated to `line`; the new parse yields 68
columns, no `break`, the title whole as `line\nbreak`, and the same row count.
Live: the test over `make_hostile_tag_video()` was run red against pre-change
source at T3 (failing on both the bogus column and the truncated value) and
passes now.

**AC5 — `typed` parity and resilience.** `test-probe-typed-resilience.R` runs
the real `probe_all()` over the recorded text through a mocked `run_program()`,
so parity is checked through the package's own composition rather than a
test-side copy; both `typed` values reproduce the recorded pre-change output on
all four non-escape fixtures, and under `typed = FALSE` every stream column is
character. The resilience contract is covered as four cases — an unprobeable
file, a file with no readable streams, a mixed vector, and two failures warning
once rather than twice — plus `probe_video()`/`probe_audio()` on wholly
unreadable input. 39 assertions pass.

**AC6 — suite and check clean, NEWS written.** Full `devtools::test()` at review:
3397 passing, 0 failures, 0 errors, 5 skips. Fresh `devtools::check()` at
review: Status OK, 0 errors / 0 warnings / 0 notes. NEWS carries a new
`## Performance` section for the single-call read (five processes to one; the
local 1.709 s → 0.456 s figure stated as a measurement, not a guarantee) and a
`## Bug fixes` entry for the newline-tag corruption; both behavioral claims are
enforced by named tests.

**Consistency gate.** `cairn_validate` exits 0 with every check passing and no
advisories. No DESIGN principle changed, so the impact report is skipped. The
`r-package` profile's toolchain slot: `devtools::document()` produces no diff,
`pkgdown::check_pkgdown()` reports no problems, README.Rmd/README.md are
untouched by this branch, the declared changelog has entries for this
milestone's user-visible changes, and the branch adds no top-level file needing
an `.Rbuildignore` entry.

### Independent review — round 1 (2026-08-06): RETURNED

CI green on all three workflows (R-CMD-check, pkgdown, test-coverage). Three
fresh-context lenses ran with distinct evidence bases; 18 findings were scored
by a separate Sonnet scorer holding the diff and the plan.

**Actioned (scored ≥80), all three verified by the reviewing session itself:**

- **F1 (96) — side-data columns are renamed; `rotation` disappears.**
  `default=nw=1` printed stream side data with no prefix (`side_data_type`,
  `displaymatrix`, `rotation`); `-of compact` prints it as
  `side_datum/display_matrix:rotation`, and `compact_section_case()` uppercases
  the whole prefix, yielding `SIDE_DATUM/DISPLAY_MATRIX:rotation`. Verified
  end-to-end on a rotated `.mp4`: `probe_all(f)$streams$rotation` is `"90"` on
  `master` and the column does not exist on this branch. Display-matrix side
  data is on essentially every phone video, and the new names vary by
  side-data type, so a mixed batch gets sparse per-type columns where it had
  one shared `rotation`. This is exactly what the Scope's Out clause names a
  defect rather than a deliverable.
- **F2 (88) — metadata invalid in the session locale silently deletes the
  stream row.** `strsplit(line, "", fixed = TRUE)` returns `NA_character_`
  (warning only) on a string invalid in `LC_CTYPE`, so `compact_fields()`
  yields the literal `"NA"`, the section dispatch drops the line, and the row
  vanishes. Verified: one Latin-1 `0xE9` byte in a tag gives a 0-row streams
  tibble. The old `sub()`-based parser errored loudly on the same input, so
  this trades a loud failure for silent data loss. Reachable via legacy
  Windows-1252 tags and non-UTF-8 Windows codepages.
- **F4 (85) — NEWS asserts something F1 falsifies.** The new `## Performance`
  entry says "Nothing about the returned tibbles changes: the same columns, in
  the same order, with the same values and types."

**Logged below threshold (15 findings), surfaced not dropped.** F15 (78) the
`count_audio_streams()` comment still says `probe_all()` costs one process per
stream, which this milestone makes false. F3 (74) AC2's five lavfi fixtures
carry no side data and no non-ASCII bytes, so the baseline structurally could
not catch F1 or F2 — derivative of them, but it is why they reached review.
F8 (66) duplicate keys abort rather than warn, and the new prefix scheme makes
same-type side-data entries collide into identical names. F18 (66) the
generator's `five` fixture yields four streams — its fifth lavfi input is never
mapped. F14 (58) per-character splitting costs ~21 ms on a 200 KB line.
F11 (48) the early-return spawn test binds its results without inspecting them.
F6 (47), F5 (44), F7 (40), F9 (36) defensive-parsing gaps on section lines real
FFprobe does not emit. F12 (45) `NA` is laundered into `"NA"`. F10 (42) the
mocked `run_program()` ignores its arguments, so the pinned writer options are
unasserted off the binary-gated path. F17 (35) a garbled comment in
`test-ffprobe.R`. F13 (25) a latent byte-vs-character offset that cannot fire
while prefixes stay ASCII. F16 (25) a stale figure on an unmodified ROADMAP
line.

**Disposition.** Defect return #1. F1 scored ≥90 on a defect in what the
package does for its users, which meets the return floor on its own; F2 and F4
are fixed with it. AC2 and AC5 are unticked — their recorded evidence stands
for the fixtures tested, but F3 shows that fixture family excludes the inputs
where the writer switch actually changes behavior, so both must be re-verified
against a widened set. AC1, AC3, AC4 and AC6 are unaffected and keep their
evidence. No criterion is amended: AC2's wording already binds to "every
fixture in the suite", so adding side-data and non-ASCII fixtures strengthens
it without a text change.
