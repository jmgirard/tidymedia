# Deferred review findings on the repo's own test instruments

_Working artifact produced by M083. Holds the grouped ROADMAP candidate rows
that carried review findings about tidymedia's own **instruments** — the
guards, sweeps, grids and harnesses that measure the package — rather than
about the runtime they measure. No finding here is a defect in shipped
behavior; every one was logged rather than actioned at its own review._

_The rows are reproduced **verbatim** as they stood at M083's branch point,
each under a heading naming the milestone whose review filed it. Finding ids,
promote-on clauses, `— added` trailers and cross-references are unchanged;
M083 moved them off `cairn/ROADMAP.md` to bring that file back under its
byte budget and changed no word of them. `cairn/ROADMAP.md` keeps one grouped
row pointing here, whose promote-on clause is the union of the five below._

_Read this page whenever a milestone is about to touch one of the five
instruments, and when a bug is reported that one of these gaps could have let
through._

**Provenance.** Ingested 2026-08-28 by M083 from `cairn/ROADMAP.md` at commit
`8021df1`, the branch point of `m083-roadmap-byte-budget` — a first-hand move
of this repo's own records, not an external source. The five rows are the ones
`awk '/^## Candidates/,0' cairn/ROADMAP.md | grep '^- ' | grep -i 'instrument'`
enumerates at that commit. Pagination: —.
Extraction: copied verbatim from the frozen blob, which git still holds, so the
text here is checkable against its origin at any time and there is nothing to
re-verify against a moving source — observed 2026-08-28. The M086 section below
was not part of that move: it was authored here on 2026-08-29 at M086's
post-merge pass, first-hand from that milestone's review, and has no ROADMAP
blob behind it — observed 2026-08-29.

## What is here

Eight sections. The first five are the rows `cairn/ROADMAP.md` carried, in its
order; the last three were added directly here at later hygiene passes:

- **M081 — the flag-guard sweep.** Filed by the M081 review.
- **M079 — the floor-measurement harness under `data-raw/`.** Filed by the M079 review.
- **M071 — the parallel-carry harness.** Filed by the M071 review.
- **M70 — the timeout-silence guards.** Filed by the M70 review.
- **M62 / M63 / M64 / M080 — the input-guard blame grid.** Accumulated across
  four reviews; the row records which of its items were promoted out to M080
  and which stayed, and carries the §7 disposition the M080 post-merge pass made.
- **M086 — the two-pass batch analysis grid.** Added 2026-08-29 by M086's
  post-merge pass under its own §7 disposition; one finding kept, two pruned
  and recorded as pruned.
- **M087 — the condition-class pairing and topic guards.** Added 2026-08-29 by
  M087's post-merge pass; two findings kept, three pruned and recorded as pruned.
- **M091 — the container gate's case fold on the batch path.** Added
  2026-08-30 by M092's T1, so the triage ledger's domain covers it.

Each row below names its own finding ids; they are not restated here, so this
list can never drift from them.

## M081 — the flag-guard sweep

- M081's three deferred review findings on the flag-guard INSTRUMENT, none a defect in shipped behavior and each outside the domain its criterion states. `flag_guard_verbs()` fixes its two guard names by hand — the shape M081 otherwise refuses — so a third flag guard would join `unchecked_flag_guards()`' walk and stay silently outside the exported-route sweep, with no assertion turning red because the specs and the member list stay mutually consistent (F3). `tm_bare_flag_operands()` reads only `!`, `&&` and `||`, so a guard branching via bare `if (flag)`, `while (flag)` or `isTRUE(flag)` is the same crash class and passes; the namespace was swept and holds no live instance today, and AC1 names the three operators as its domain (F7). And the AC6 completeness reader derives its `vocab` from the entries themselves, so a carrier declared in no entry is undetectable — the SAME shape as the M62/M63/M64 row's F8 below, cross-referenced rather than restated, now found a second time on a second instrument (F2). Promote on a flag guard reaching a user with a bare base-R crash that one of these gaps let through — the class of evidence that would falsify logging them rather than fixing them. — added 2026-08-28 — M081 review F2/F3/F7, all logged; M62/M63/M64 row F8; D059


## M079 — the floor-measurement harness

- M079's ten deferred review findings on the floor harness itself, grouped because they are about the INSTRUMENT rather than the floors it measures; none fails an acceptance criterion and nothing under `data-raw/` ships. Coverage gaps: no probe plants a defect at `withr-floor.R`'s or `r-floor.R`'s fetch BRANCH, only at the validator both call (review measured the withr branch directly instead); probe D3 asserts a second `install_pin()` returns `NULL`, which a full reinstall also does, so it cannot tell reuse from reinstall; the two `observed_elapsed()` probes parse hand-written lines rather than anything `emit()` produced, so the summary column could be rewired without turning them red (review reproduced this and verified that clause by running `timeout-bound.R` directly); no probe plants a package that compiles and will not load, so what dropping `--no-test-load` buys has host-side evidence only from the container smoke run; and the G-probes call `packageVersion()` on `testthat` and `dplyr`, so a host without them errors for an unrelated reason. Script defects: `can_reuse()` reaches `fetch_tarball()` unwrapped via `linkingto_of()`, so a network failure inside the REUSE check aborts the run instead of joining the per-floor failure list, and reuse now needs the tarball at all (a persisted `TM_LIBROOT` with a fresh `TM_SCRATCH` re-downloads what it meant to skip); the MOVE line annotates a requirement read from a PINNED tarball with the requirer's INSTALLED version; `is_package_tarball()` accepts a `DESCRIPTION` anywhere in the archive, so `foo/inst/DESCRIPTION` passes; `run_under()` builds `R_LIBS=%s` unquoted and the `path.expand()` beside it is decorative under a tempdir-derived root; `stage_root()` ignores every `file.symlink()` return, so a partly staged root under-tests silently; and `imports-floors.R`'s header still names pkgload/testthat/devtools as the harness while `HOLDBACK_SET` is testthat+furrr. Promote on a floor measurement whose result is wrong, unattributable, or unreproducible because of one of these — the class of evidence that would falsify logging them rather than fixing them. — added 2026-08-28 — M079 review F3, F4, F6, F8, F9, F10, F11, F12, F13, F15


## M071 — the parallel-carry harness

- M071's five review findings on its own carry harness, grouped because they are about the parallel-carry INSTRUMENT rather than the runtime it measures; none is a defect in shipped behavior and all five were logged rather than actioned. AC2's option-unset control asserts `length(probes) == 2L` over 4 jobs, which assumes future hands at least one job to each of two workers: `ffm_batch()`'s internal `future_pmap`/`future_map` take no `.options`, so the harness's `chunk_size = 1` reaches only its own probe maps and every AC1/AC2/AC3 case runs under default scheduling (F4). The fan-out domain guard compares the SET of file basenames containing `furrr::future_` plus a total count of 4, so deleting `carry_options(...)` from any of the four sites leaves it green, and swapping one fan-out for another inside one file passes too — only the behavioral AC1 tests catch an unwiring, and those skip on Windows, on CRAN and without furrr (F5); the same guard's domain-emptiness blindness reached a red `test-coverage` build at review and was fixed there, but its coverage weakness is untouched. Both refusal tests assert only `rlang_error` plus class-and-message equality BETWEEN the branches, so a regression failing both identically for an unrelated reason passes (F7). AC1's `probe_all` case matches `"timed out rather than being unreadable"` and never checks a filename appears, which the criterion asks for (F8). And under a sequential plan with `parallel = TRUE` — a combination the package supports, warning and carrying on — `carry_options()` runs in-process, so any `options(tidymedia.*)` the caller's own `.f` sets during the batch is silently rolled back, a case D050's "the sequential branches are untouched" does not cover (F9). Promote on the first carry regression that reaches a user THROUGH one of these gaps, or alongside the next milestone touching the fan-out sites. — added 2026-08-27 — M071 review F4/F5/F7/F8/F9, all logged; D050


## M70 — the timeout-silence guards

- The M70 review's eight guard-strength findings, grouped because they are about the timeout-silence INSTRUMENT rather than the runtime it measures; none is a defect in shipped behavior and all eight were logged rather than actioned. `tm_condition_api` lists only R's own condition functions, so the absorber partition cannot see the package's own `absorb_timeout()` — `probe_one`, `mediainfo_read` and `mediainfo_parameter` all swallow through it and none appears in the recorded absorber list, weakening the explanation though not AC1's grid (O2). `tm_program_arg()` returns `NULL` unless the `program` argument is a character literal, and `run_program()`'s formal default is `program = "the program"`, so a future call omitting the argument would abort with a fourth literal while the set assertion stays green — no site omits it today, verified by grep at review, and the mutation probe only VARIES a literal, never removes it (O3). `ffm_batch(parallel = TRUE, run = TRUE)` is exercised by no test: the only parallel batch test passes `run = FALSE`, and the AC1 grid never sets `parallel`, so the `vapply(results, [[, logical(1), "success")` contract change rides on inspection alone (O4). **O4 is taken by M071 (2026-08-26)**, whose AC3 drives a real timeout through `ffm_batch(parallel = TRUE, run = TRUE)`. AC2's "exactly one warning" is asserted at `count_audio_streams_all()` and never at a `_batch` verb, and the AC1 grid asserts only AT LEAST one condition, so a refactor moving the probe into a per-row loop would satisfy every current guard (O5). `run_with_progress()`'s changed return contract is covered only behind `skip_if_no_ffmpeg()`, and CI's macOS and Windows runners install no media binaries, so a mismatch surfaces as a hard `vapply` type error on a user's machine (O6). The AC1 `warned` verdict is `grepl("timed out", ..., fixed = TRUE)` on cli-FORMATTED text, which splits at `cli.width` 20-22 for `probe_all()` and 28-31 for `count_audio_streams_all()`, where both warnings carry classes that would test exactly — the same shape M46's lesson warns about (O7). The doc guards grep the whole of NEWS.md rather than its timeout paragraph, so an unrelated future release note containing "no warning" would redden a guard about M69's retired disclosure (O8). And `probe_all_impl()`'s threaded `call` would make its argument refusals name `infile` through `verify_media()`, which has no such argument — unreachable today because `check_file_exists()` refuses first (O11). Promote on the first timeout regression that reaches a user THROUGH one of these gaps, or alongside the next milestone touching the sweep. — added 2026-08-26 — M70 review O2/O3/O4/O5/O6/O7/O8/O11, all logged; D049


## M62 / M63 / M64 / M080 — the input-guard blame grid

_Corrected M083: "the candidate row below" named the struck M080 SHIPPED-predicate row, which M083 deleted from `cairn/ROADMAP.md` after its promotion to M081. The row's own text is unchanged below._

- The M62 and M63 review findings logged below the action threshold, grouped because they are about the same instrument rather than the runtime. **N1 is closed (M63, corrected here 2026-08-08):** it read that the AC1 site test matched `"does not exist"`, a string the multi-path abort's literal did not contain, so a copy of that branch alone would not turn it red — M63's uniform wording puts `can't be found or read` in both literals and the test is now red against either branch alone. Still open from M62: for the eleven `slots = 1L` verbs the grid's `all` form is still a one-row cell, so the only cell carrying two DISTINCT absent paths on a plain character `input` column is the factor-typed one (N2, 79); `check_batch_inputs(jobs, c("main", "overlay"))` builds its `arg` from every declared carrier, so one missing column of two renders "`jobs$main` and `jobs$overlay` name 1 file that does not exist", over-naming a carrier that is correct, and no reader checks attribution (N3, 68); and `reject_duplicate_outputs()` sits above the sweep, so NEWS's "one path typed wrong in twenty rows is one missing file" is observable only when outputs are explicit — with derived outputs the colliding-output guard reports first (N7, 74). M63 added four of its own on the same grid: the new `unreadable` form inherits N2's one-row-cell limit for the `slots = 1L` verbs (C1, 70); `tm_refused_input()` uses `rlang::catch_cnd()`'s default `classes = "condition"`, so a condition signalled before the abort would read as "not refused" (A5, 68); the site-uniqueness test now asserts only that the retired wordings are gone and fences no new third wording (A8, 68); and `input_guard_reword()` applies its substitutions to every message rather than keying on the `input` class, so an unrelated guard containing `"Missing:"` could be waved through as wording-only (A9, 68). Promote on the first report of one of these messages misleading a caller, or alongside the next milestone touching this grid. — added 2026-08-08, corrected + extended 2026-08-08 — M62 review round 2 N2/N3/N7 (79/68/74) and M63 review C1/A5/A8/A9 (70/68/68/68), all logged; M64's review adds five on the sibling blame instruments, all logged 2026-08-08: the mutation harness's controls-neutered check passes vacuously if Rscript crashes (F5, 78); the precedence crossing list omits S1 codec-token, S2 x-column and S6 outdir/interval-type crossings, none flip-bearing (F10, 72); the nvenc-ordering test carries no guard-liveness control (F11, 62); the baseline/precedence usage docs say `origin/master` where the ACs say merge-base, equal today (F7, 55); `check_dim(NA_real_)` crashes bare, pre-existing but now fronted (F4, 25). **Four items promoted to M080 on 2026-08-28** — the ones that are defects in SHIPPED behavior rather than in the instrument: N3's carrier over-naming, N7's derived-output duplication guard hiding the path, F4's bare `check_dim(NA_real_)` crash, and a fourth this plan's criteria audit found and the row never filed — `check_dim(NA_character_)` is ACCEPTED and compiles `crop=w=NA` into the command. The nine instrument findings stay here, unchanged: M62 N2, M63 C1/A5/A8/A9, M64 F5/F7/F10/F11. M080 adds a tenth, found by re-running the grid: `input_guard_blame_unexpected()` asserts the cells whose blame moved are exactly the unreadable ones, which was M63's claim about the M62->M63 ref pair, so on any later pair it reports all 30 unreadable cells and cannot be read as pass/fail (A10, 2026-08-28). **§7 disposition, 2026-08-28 (M080 post-merge):** the row was dispositioned rather than extended again — M080's four SHIPPED-predicate findings left it for the candidate row below, and six instrument findings joined it as the explicit extension the user chose. Those six: the `scalar_arg` and `column_type:stream` crossings gate verb membership on a `grepl(..., fixed = TRUE)` over deparsed bodies, so a reformat or a named-argument reorder drops a verb with no reader complaining, since `input_guard_uncovered()` re-derives from the same declaration (N6, 2026-08-28); `input_guard_error_crossing()`'s `scalar_arg` classifier matches a bad scalar `video_codec` that no crossing supplies, so that half can never fire and reads as coverage the grid lacks (N7); M080's Scope Out called the NA sweep's excluded set "the table-taking `check_*` predicates" when the filter excludes only >1 required formal or a formal named `jobs` — `check_regions()` and `check_region_values()` are table-taking and IN the domain, so the plan gate's own falsifier for that choice has already fired (F5); the sweep probes `f(vals[[i]])` positionally after deriving the required formal by name and discarding it, so a predicate whose required formal is not first would pass vacuously (F6); 12 of the 60 sweep cells accept all four NA types silently and assert nothing (F7); and the carrier-completeness reader derives its vocabulary from what the entries themselves declare, so a carrier omitted from every entry is undetectable (F8). — extended 2026-08-08, dispositioned 2026-08-28 — M62/M63 reviews; M64 review; D040; D041; M080


## M086 — the two-pass batch analysis grid

_Added 2026-08-29 at M086's post-merge hygiene pass, under the §7 disposition
its ROADMAP row records. M086's review filed three instrument findings and
only this one was kept; the other two were pruned, and what they were is
recorded here so the pruning is legible rather than silent._

- M086's AC4 grid asserts `tm_rows` and `tm_row_status` against a **mocked**
  Phase 1: `tests/testthat/test-normalize-audios-two-pass.R:355-361` replaces
  `run_loudnorm_analysis_batch()` wholesale, and the failed-row fixtures are
  hand-built `structure("some ffmpeg error", status = 1L)`. Nothing in the
  suite ties `assemble_measured()`'s expected input shape to what
  `run_program()` actually returns, so a change to that return shape would
  leave `tm_row_status` silently all-`NA` with the grid still green. M086's
  review checked the real path by hand — a corrupt input gave `tm_rows = 1L`,
  `tm_row_status = 183L` on ffmpeg 9.0.1 — so this is a coverage gap, not a
  defect. Promote on a `tm_row_status` that is wrong or all-`NA` in a real
  batch, or alongside the next milestone touching Phase 1's return contract.
  — added 2026-08-29 — M086 review F9 (`[O]` 7)

_Pruned at the same disposition, recorded rather than carried._ **(i)** The AC1
loop's `expect_gt(length(unique(statuses)), 1L)`
(`tests/testthat/test-ffmpeg-exit-condition.R:198`) rests on FFmpeg's exit
numbering — 234 for a muxer refusal against 254 for a failed output open — and
so could redden on correct code on a build that returns one number for both.
Pruned because CI measured it green on **ffmpeg 6.1.1-3ubuntu5** as well as the
local **9.0.1**, three majors apart, at commit d095a1d. **(ii)**
`adts_refuses_multistream()` (same file, lines 135-147) calls
`system2("ffmpeg", ...)` directly instead of through `find_ffmpeg()`, and its
`tryCatch(..., error = function(e) 1L)` makes "could not run ffmpeg at all"
read as "the muxer refuses". Pruned because it misfires only with FFmpeg
configured off-PATH via `set_ffmpeg()`, which no supported path exercises.

## M087 — the condition-class pairing and topic guards

_Added 2026-08-29 at M087's post-merge hygiene pass, under the §7 disposition
its ROADMAP row records. M087's two review passes filed five instrument
findings; two were kept and three pruned, and what those were is recorded here
so the pruning is legible rather than silent._

- The AC4 pairing test binds a class claim to a **topic**, not to a **site**:
  `tests/testthat/test-ffmpeg-exit-condition.R` asserts only that every class a
  site observes appears in each paired help topic, never that a topic omits a
  class its paired sites do not raise. That is why the same over-attribution
  shipped green twice — `?tidymedia` naming `tidymedia_ffmpeg_exit` for
  `R/loudnorm_two_pass.R:112`, caught by a human reader at pass 1, and
  `?normalize_audio_batch` doing the same, caught at pass 2. The obvious
  strengthening (over the union of a topic's paired sites) would catch neither,
  since both topics are also paired with `:151`, which does raise the exit
  class. A test that would catch them must bind a claim to a site, which is a
  design call of its own. Promote on a third topic over-attributing a class, or
  alongside a milestone reworking the topic-pairing instrument.
  — added 2026-08-29 — M087 review pass 1 F5, pass 2 F1/F2
- The pairing probe catches with `condition = function(e) e` at three **error**
  sites (`scalar_exit`, `batch_loudnorm`, `scalar_sep`, same file), so a
  `tidymedia_`-classed *warning* signalled before the abort would be captured
  instead and asserted against topics for a site nobody tested — passing the
  probe's non-empty-class guard while testing the wrong condition. `error =`
  binds each probe to its site. Latent today; the dropped-track check on the
  `normalize_audio` sites is the live candidate. Promote alongside any milestone
  adding a warning to one of the five sites.
  — added 2026-08-29 — M087 review pass 2 F5

_Pruned at the same disposition, recorded rather than carried._ **(i)** The AC5
guard (`tests/testthat/test-package-topic.R`) asserts the vignette paragraph
precedes the first `\section{`, which `\description{}` would also satisfy, where
AC5 says outside every section; pruned because AC5 itself holds and the
paragraph's real offset was measured inside `\details{}`. **(ii)** Four Rd-text
assertions match strings sitting at roxygen wrap boundaries, so a benign reflow
by `document()` would fail them with no behaviour change; pruned as a
maintenance cost rather than a blind spot — a reflow failure is loud and
self-explaining. **(iii)** Two mocked tests in
`test-ffmpeg-exit-condition.R` substitute `run_program` but leave
`find_ffmpeg()`/`find_ffprobe()` live, so with no binaries present they pass
while emitting "Failed to find ffmpeg" warnings instead of skipping; pruned as
cosmetic, since `find_program()` warns rather than aborting and the assertions
under test never reach a binary.

## M091 — the container gate's case fold on the batch path

_Added 2026-08-30 by M092's T1 — the eighth filing, and the reason the page's
triage covers eight sections rather than seven. M091's final review round sent
this one to a candidate row; M092 absorbs that disposition and files the
finding's own text here, where the ledger can reach it._

- `holds_multiple_audio()` folds case (`tolower(tools::file_ext(path))`,
  `R/ffmpeg.R:670-671`) and both call sites depend on that fold — the scalar
  fail-open at `R/ffmpeg.R:728` and the batch row-drop at `R/ffmpeg.R:899`.
  Only the scalar one is exercised: `tests/testthat/test-separate-av-multitrack.R:1191`
  drives `separate_audio_video()` into `OUT.MKA` and asserts no multi-track
  blame, and no test anywhere passes an uppercase extension through
  `separate_audio_video_batch()`. So replacing the batch site's call with an
  exact-case extension match leaves the suite green while the false blame M091
  exists to remove keeps arriving on any batch row whose `audiofile` is spelled
  `.MKA`, `.MP4` or `.MOV` — a case FFmpeg itself reads as the same muxer.
  Promote on a batch caller receiving the multi-track advice for an output that
  already holds several audio streams. — added 2026-08-30 — M091 review round 4;
  D069; D071
