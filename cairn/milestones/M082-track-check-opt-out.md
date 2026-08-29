# M082: The track check has an off switch, and says what it costs

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m082-track-check-opt-out` / https://github.com/jmgirard/tidymedia/pull/86

## Goal

Give the dropped-audio-track diagnostic a session-wide off switch, and make
every verb that runs it say what it costs.

## Scope

Surface tier: **user-facing** — the deliverable is a new documented option,
six verbs' help pages, and a NEWS entry.

**In:** a third option seam, `tidymedia.check_tracks`, defaulting `TRUE`, read
by a new `resolve_check_tracks()` modelled on `resolve_timeout()`
(`R/timeout.R:26`); gating every dropped-track probe site on it; carrying it
into workers via `carried_option_values()` (`R/timeout.R:462`); a `cli`
progress bar over `warn_dropped_audio_batch()`'s serial probe sweep
(`R/ffmpeg.R:427`); roxygen on the four verbs that do not state the probe's
cost; the stale verb list at `R/tidymedia-package.R:55`; NEWS.

**Out:**
- A per-verb `check_tracks =` argument → stays a candidate row. The gate chose
  the seam on D047's own reasoning; an argument is still available under D014's
  pre-0.2.0 clean break and would need a superseding entry.
- `with_check_tracks()` / `local_check_tracks()` → not planned. `withr` is a
  hard dependency (D052), so `withr::local_options()` is the documented form.
- Moving the probe inside the fan-out so its cost parallelizes → stays a
  candidate row; it needs the `ffm_batch()` hook D024/RR02 Q3 rejected.
- The two `count_audio_streams_all()` sites in `run_separation_audio()`
  (`R/ffmpeg.R:637`) and `warn_failed_separation_batch()` (`R/ffmpeg.R:746`) →
  untouched. They are a different diagnostic, reached only after a run has
  already failed, so they carry none of the upfront cost this milestone is
  about.

## Acceptance criteria

- [x] AC1 — Every dropped-track probe site in `R/ffmpeg.R`, the set
      `grep -n 'warn_dropped_audio' R/ffmpeg.R` returns, is gated on the seam.
      For each site, a test on a multi-track input counts calls to
      `count_audio_streams_all()`: zero under
      `options(tidymedia.check_tracks = FALSE)`, and at least one — with the
      `tidymedia_dropped_audio` warning signalled — under the default. The
      count is a counter, not a `stop()`ing mock (M44 lesson). At each of those
      sites a value that is not a length-1 non-`NA` logical (`"yes"`, `NA`,
      `c(TRUE, TRUE)`, `1`) aborts naming `tidymedia.check_tracks` as the
      offending argument.
- [x] AC2 — With the option unset, every verb the AC1 procedure enumerates
      signals the same conditions on the same inputs as it does on `master`,
      including `normalize_audio()` under both `two_pass = TRUE` and
      `two_pass = FALSE` signalling exactly one warning for one drop (M075).
- [x] AC3 — A `parallel = TRUE` batch run under
      `options(tidymedia.check_tracks = FALSE)` leaves the option unset in the
      parent afterwards, and a worker sees `FALSE` — the same round trip
      `carried_option_values()` already makes for `tidymedia.timeout`.
- [x] AC4 — `warn_dropped_audio_batch()` reports progress across the inputs
      its sweep visits: on a jobs table whose rows naming no `audio_stream`
      cover N distinct inputs, the sweep drives one `cli` progress bar whose
      total is N and which reaches N/N. Measured on a table that also carries
      at least one row naming a track, so that N is smaller than the table's
      own distinct-input count. No bar is created in either of two further
      cells: the seam `FALSE`, and every row naming a track.
- [x] AC5 — Every verb the AC1 procedure enumerates documents, in its own
      help topic, that the check costs one FFprobe call per distinct input and
      how to turn it off; the three `_batch` verbs also state that those probes
      run serially at the front door before the fan-out. Verified against the
      **installed** help, not `man/*.Rd` in the source tree (M51/M59 lesson).
- [x] AC6 — `?tidymedia-package` names `normalize_audio()` and
      `normalize_audio_batch()` among the verbs behind the dropped-track check
      (stale since M075) and documents `tidymedia.check_tracks` beside the
      other two seams; NEWS.md gains an entry naming the option, its default,
      and the cost it lets a caller decline.
- [x] AC7 — `devtools::check()` clean: 0 errors, 0 warnings, 0 notes beyond
      those already on `master`.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T6, T7
- AC7 → T7

## Tasks

- [x] T1 — Add `resolve_check_tracks()` beside `resolve_timeout()`
      (`R/timeout.R:26`): `getOption("tidymedia.check_tracks", default = TRUE)`
      through `rlang::check_bool(arg = "tidymedia.check_tracks", call = call)`.
      Unit tests for the accepted and refused values first.
- [x] T2 — Gate each site the AC1 grep returns. Scalar sites
      (`R/ffmpeg.R:544`, `:1015`, `:2217`, `:2262`) fold the call into the
      existing `isTRUE(run) && is.null(audio_stream)` condition; the batch
      sites gate inside `warn_dropped_audio_batch()` (`R/ffmpeg.R:427`) so one
      early return covers all three. **Both** `normalize_audio()` sites: its
      `if (two_pass)` block falls through (M075 lesson).
- [x] T3 — Per-site tests: the invocation counter of AC1, the AC2 refusals,
      and the AC3 default-behaviour cells including normalize's two branches.
- [x] T4 — Add `tidymedia.check_tracks` to `carried_option_values()`
      (`R/timeout.R:462`), carried raw like the encoder override rather than
      resolved like the timeout, plus the round-trip test.
- [x] T5 — Progress bar over the `count_audio_streams_all()` sweep in
      `warn_dropped_audio_batch()`, with the bar-total and seam-off tests.
- [x] T6 — Roxygen: the cost and the seam on `extract_audio()`,
      `convert_audio()`, `extract_audio_batch()`, `convert_audio_batch()`,
      matching the wording M075 shipped at `R/ffmpeg.R:2092` and `:4262`; the
      seam on all six; `R/tidymedia-package.R:55`'s verb list and options
      section. `devtools::document()`, then the installed-help test.
- [x] T7 — `devtools::document()`, `devtools::document()`, `devtools::test()`,
      `devtools::check()`. (NEWS entry moved into T6, where the documentation
      guard that reads it lives.)

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: plan gate chose the `tidymedia.check_tracks` option seam over a per-verb `check_tracks =` argument because D047 already declined that trade for the timeout — a seam commits no exported signature, leaving D014's pre-0.2.0 clean break unspent — and `R/timeout.R:459` records that a third seam is one line; falsified by a report of a caller needing two different answers inside one script, which is the case a session-wide switch cannot serve and D051 had to ship `with_timeout()` for.
- 2026-08-28: plan gate chose `withr::local_options()` over exported `with_check_tracks()`/`local_check_tracks()` wrappers because `withr` is a hard dependency (D052) and a boolean needs none of the whole-number validation that earned `with_timeout()` its existence; falsified by the seam growing a value that needs refusing before it is set.
- 2026-08-28: plan gate chose a progress bar over relocating the batch probe into the fan-out because the latter needs the `ffm_batch()` hook D024/RR02 Q3 rejected; falsified by a measured batch where the serial sweep, not the encoding, dominates wall clock even with progress shown.
- 2026-08-28: plan gate chose aborting on a malformed option value over falling back to `TRUE`, following `resolve_timeout()`; falsified by a report of a stale option in a startup file breaking calls unrelated to the check.
- 2026-08-28: criteria audit ran in **full** mode (user-facing tier), inline rather than in a fresh-context [O] subagent — subagents are disabled in this session, so the instrument was weaker than the skill specifies. Two findings, both fixed before the gate: the seam-gating and the verb-documentation criteria quantified over "all six verbs" as a hand-list, repaired to quantify over the set the stated `grep` returns (bounded-promise rule); the unchanged-default criterion promised "the existing dropped-track test file passes unmodified", an instrument property, narrowed to the verbs' default behaviour. Nine drafted criteria then tripped the >7 sizing advisory and were merged to seven — option validation folded into the gating criterion, the package topic and NEWS into one documentation criterion — rather than split into a second milestone: the code and the contract it documents are one deliverable, and D024 requires a diagnostic that can silently not run to say so.
- 2026-08-28: amendment (substantive, AC4) — the per-input-condition instrument is not reproducible: measured on R 4.6.1, the same `cli` bar signalled 3 conditions over 5 instant updates, 0 over 3 updates delayed 50 ms each, and N+1 only when every update forces a redraw, which also bypasses `cli.progress_show_after` and would render a bar on every batch call. AC4 narrowed at the mini gate to the bar's own totals (N distinct inputs, reaching N/N; no bar when the seam is `FALSE`), which `cli`'s `logger` progress handler reports deterministically.
- 2026-08-28: the amended AC4 wording took the criteria audit's full-mode questions inline rather than in a fresh-context [O] reader — subagents are disabled in this session, so the instrument was weaker than the skill specifies. One repair before the gate: the first draft named `cli`'s `logger` handler in the criterion itself, an instrument property, and was rewritten to state the bar's behaviour and leave the observation channel to the test.
- 2026-08-28: T1 — `resolve_check_tracks()` added beside `resolve_timeout()` (`R/timeout.R`), reading `tidymedia.check_tracks` through `rlang::check_bool()`; the four malformed values AC1 names each abort naming the option and the kind of value refused, and the abort blames the caller's frame. Discrimination checked: swapping `check_bool()` for `isTRUE()` turns two of the four tests red. `R/timeout.R`'s opening comment said the timeout was the only seam changing what happens rather than what is reported, which this seam makes false; corrected in the same commit.
- 2026-08-28: T2 — all five probe sites gated: the four scalar sites take `resolve_check_tracks()` as the LAST conjunct of the existing `isTRUE(run) && is.null(audio_stream)` chain, so a `run = FALSE` or track-naming call still reads no option and cannot be aborted by a malformed one; the batch form gates once inside `warn_dropped_audio_batch()`, below its rows check for the same reason, covering all three `_batch` verbs. Full suite green with the option unset.
- 2026-08-28: T3 — a named site table drives three tests over all seven probe sites (the four scalar sites plus the shared batch site through each `_batch` verb): zero `count_audio_streams_all()` calls and no warning with the seam `FALSE`; at least one call and exactly one warning with it unset, normalize's two `two_pass` branches included; and an abort naming the option at every site on a malformed value, which needs no binary. A fourth test pins that a `run = FALSE` or track-naming call reads no option at all. Discrimination checked: deleting the scalar conjunct turns 10 assertions red, deleting the batch return 8. `catch_drop()` moved from test-audio-track-drop.R to helper-audio-track-drop.R rather than copied.
- 2026-08-28: T4 — `tidymedia.check_tracks` joins `carried_option_values()` carried raw, so an unset seam stays unset in the worker; resolving buys nothing because the front-door probe already refuses a malformed value in this process. Three tests: the carrier's own list, a six-element fan-out where every worker reads the parent's `FALSE` with an unset-parent control, and a `parallel = TRUE` `ffm_batch()` run that leaves the parent's setting exactly as it found it under both `FALSE` and unset. Discrimination checked: deleting the carried line turns the in-process test red. The worker-side tests needed `devtools::install()` first — the file's fingerprint guard skips them while the installed carrier differs from the source, which it did until the install.
- 2026-08-28: T5 — the sweep in `count_audio_streams_all()` takes an optional `progress` argument, `TRUE` at the batch dropped-track site and nowhere else; one loop serves both cases rather than an `lapply()` beside a `for()`. Four tests read the bar through `cli`'s `logger` progress handler: four rows over three distinct inputs give `0/3 created` and `3/3 terminated (done)`, and no bar exists with the seam off, with every row naming a track, or at the scalar sites. Discrimination checked: dropping `progress = TRUE` turns the first red. The open D024 question is settled in `cairn/DECISIONS.md` as D060.
- 2026-08-28: T6 — the four verbs that never stated the probe's cost now do, in M075's wording; all six state the seam and the `withr` form; the three `_batch` verbs state the serial front-door sweep and the bar. `?tidymedia` gained a *Session options* section covering all three seams, and its dropped-track sentence — which named `separate_audio_video()` and omitted the loudness pair — now names the three verbs that run the check and says the separation verb runs a different diagnostic after a failed run. A new guard walks the namespace for exported functions reaching `warn_dropped_audio` (it finds exactly the six) and reads their Rd, squishing whitespace because Rd hard-wraps. Discrimination checked: deleting one verb's seam sentence and adding the serial-sweep sentence to a scalar verb each turn it red. The first spelling of the seam assertion was not discriminating — `local_options(tidymedia.check_tracks = FALSE)` contains the session form as a substring, so it passed with the session sentence deleted; both forms are now matched separately.
- 2026-08-28: minor amendment — the NEWS entry moved from T7 to T6, because the documentation guard that reads it is T6's and a task is not checked off with its own verify slot red.
- 2026-08-28: T7 — `devtools::document()` leaves no diff, `devtools::test()` is clean, and `devtools::check()` reports 0 errors, 0 warnings, 0 notes (2m 54s). The documentation guard was additionally run against the INSTALLED help by hand, after `devtools::install()`: `tools::Rd_db("tidymedia")` reports the cost sentence, both seam forms and the serial-sweep sentence on exactly the verbs that should carry each, and `Session options` on the package topic. That path is what the guard takes under `R CMD check`, where the installed package has no `man/` directory (confirmed: `dir.exists()` on the installed `man/` is FALSE). No exported object was added, so `_pkgdown.yml` needs no row; README.Rmd names no option and is unchanged.
- 2026-08-28: implement gate chose the probe sweep's bar independent of the batch verbs' `progress =` argument, because that argument governs `ffm_batch()`'s run-time bar while this sweep is a front-door cost the caller has not declined, and `cli.progress_show_after` already hides the bar on sweeps under two seconds; falsified by a report of the bar appearing on a batch whose caller had switched progress off and did not want it.
- 2026-08-28: open for implement — AC4's progress bar makes the probe's HAVING RUN observable as something other than a condition, which D024's operative rule ("changes nothing observable except whether a diagnostic condition is signalled") does not obviously cover. `cli`'s progress mechanism does signal conditions, but that was not verified here. Settle it in the milestone's decision log before T5 ships, and promote to `cairn/DECISIONS.md` alongside the seam entry.

- 2026-08-28: review — evidence gathered for all seven criteria on PR #86; AC1, AC2, AC3, AC5, AC6, AC7 pass, consistency gate clean (cairn_validate exit 0, document() no diff, pkgdown clean, check 0/0/0). Three fresh-context lenses ran; the two Sonnet lenses reported zero defects, the Opus diff lens nine findings, all logged with dispositions in the Review section.
- 2026-08-28: amendment return: AC4 — "`warn_dropped_audio_batch()` reports progress across its probe sweep: on a jobs table of N distinct inputs the sweep drives one `cli` progress bar whose total is N and which reaches N/N; with the seam `FALSE` no bar is created." Falsified on a mixed jobs table: three distinct inputs, one row naming an `audio_stream`, bar total 2 against N = 3. The criterion names no procedure bounding "N distinct inputs"; the shipped behaviour (counting the inputs the sweep visits) is correct.
- 2026-08-28: amendment return: AC4 — "`warn_dropped_audio_batch()` reports progress across the inputs its sweep visits: on a jobs table whose rows naming no `audio_stream` cover N distinct inputs, the sweep drives one `cli` progress bar whose total is N and which reaches N/N. Measured on a table that also carries at least one row naming a track, so that N is smaller than the table's own distinct-input count. No bar is created in either of two further cells: the seam `FALSE`, and every row naming a track." This line executes the return logged above rather than opening a second one — one amendment return on AC4, not two. Narrowing chosen at the mini gate: the shipped bar counts the inputs the sweep visits, which is the behaviour that should ship, so the promise moved to that domain rather than the code moving to the promise.
- 2026-08-28: the amended AC4 wording took the criteria audit's full-mode questions (user-facing tier) inline rather than in a fresh-context [O] reader — subagents are unavailable in this session, so the instrument was weaker than the skill specifies. Two repairs before the gate: the first draft bounded the domain as "whose track-naming rows are excluded", a procedure description rather than a promise, rewritten to name the rows the count is taken over; and the negative clause "no bar is created" was rewritten as two named measured cells, since as a bare universal it quantified over every jobs table.
- 2026-08-28: T5 (amended AC4) — a fourth bar cell added: three distinct inputs, one row naming a track, bar reads `0/2 created` / `2/2 terminated (done)`. Discrimination checked: swapping the sweep's `jobs$input[rows]` for `jobs$input` turns exactly this test red and leaves the other 76 assertions green.
- 2026-08-28: review finding F5 fixed — the sweep's result assignment is now `res[i] <- list(count_audio_streams(...))`, which keeps a `NULL` answer as an element where `res[[i]] <-` deletes it (measured: a length-3 list stays 3 under the first form, becomes 2 under the second), so the `vapply()` below it cannot be length-mismatched against `uniq`. Latent, not live: `count_audio_streams()` returns a count, `NA_integer_` or a timeout sentinel and no `NULL` today.
- 2026-08-28: review finding F1 fixed at all three sites — NEWS.md, the `?tidymedia` options section and `R/timeout.R`'s seam comment named the switch's beneficiary as a jobs table whose rows all name an `audio_stream`, which is the one population that pays nothing for the check either way: the scalar sites gate on `is.null(audio_stream)` and the batch form returns above the seam when no row asked to be probed. Each now names a large batch whose tracks the caller already knows, and says plainly that a row naming a track is never probed.
- 2026-08-28: review finding F3 fixed by superseding entry D061 rather than an edit (history is append-only): D060's sentence that the bar does not reach D024's outcome clause is wrong on the ran-vs-skipped axis, which the milestone's own no-bar tests pin as observable. D060's load-bearing defence — the bar rides `cli_message`/`cliMessage`, the same channel as the warning — stands unchanged, as does the bar.
- 2026-08-28: review findings F9 and F6 took two ROADMAP candidate rows: the two shapes M082 left behind (per-verb argument, probing inside the fan-out), cross-referencing the retired M44 row that states them and their promotion conditions rather than restating it; and the `See vignette(…)` paragraph captured by whatever `\section{}` precedes it in `?tidymedia`, pre-existing on `master` and moved rather than introduced here.
- 2026-08-28: amendment round complete — `devtools::document()` leaves no diff, `devtools::test()` is 0 failures / 8223 passing / 5 skips (all binary-capability skips), and `devtools::check()` reports 0 errors, 0 warnings, 0 notes (2m 42s). Status back to review; AC4 is the one criterion needing fresh evidence, against its amended wording.
- 2026-08-28: re-review after the AC4 amendment — amended AC4 passes on its named mixed-table cell (bar total 2 against the table's own 3 distinct inputs, reaching 2/2; no bar with the seam `FALSE` or with every row naming a track), so all seven criteria now carry fresh evidence. Consistency gate re-run clean: `cairn_validate` exit 0, `document()` no diff, `pkgdown` clean, suite 8223 passing / 0 failing / 5 skips, `check()` 0/0/0. Three lenses re-ran over the incremental diff; the two Sonnet lenses reported zero findings, the Opus diff lens four (F10-F13), all logged with dispositions.

## Decisions

- 2026-08-28: the question the plan left open — whether a progress bar over the probe sweep breaks D024's rule that the probe may change nothing observable except whether a diagnostic condition is signalled — is settled as **inside the licence**, and promoted to `cairn/DECISIONS.md` D060 alongside the seam itself, since both extend D024/D047 rather than deciding anything local to this milestone.

## Review

Reviewed 2026-08-28 on branch `m082-track-check-opt-out`, PR
https://github.com/jmgirard/tidymedia/pull/86. `master` had not moved since the
branch was cut (`git merge-base --is-ancestor origin/master HEAD` succeeded), so
no merge was needed before gathering evidence. Diffstat: 20 files, +814 / -102.

### Acceptance-criteria evidence

- **AC1 — pass.** `grep -n 'warn_dropped_audio' R/ffmpeg.R` returns seven probe
  sites: four scalar (`:564`, `:1041`, `:2248`, `:2293`) and three `_batch` call
  sites (`:4525`, `:5188`, `:5337`) all reaching `warn_dropped_audio_batch()`
  (`:427`). Read at those lines: each scalar site carries
  `resolve_check_tracks()` as the last conjunct of
  `isTRUE(run) && is.null(audio_stream) && …`; the batch form returns early on
  `!resolve_check_tracks()` below its rows check. `test-check-tracks-seam.R`
  drives all seven through a named site table: 12 tests, 76 passing, 0 failing.
  The counter is `local_mocked_bindings(count_audio_streams_all = …)`
  incrementing an integer, not a `stop()`ing mock. Zero calls and zero warnings
  with the seam `FALSE`; ≥1 call and exactly one `tidymedia_dropped_audio`
  warning with it unset; each of `"yes"`, `NA`, `c(TRUE, TRUE)` and `1` aborts
  with "`tidymedia.check_tracks` must be `TRUE` or `FALSE`" at every site.
- **AC2 — pass.** The same site table's default-behaviour test covers every verb
  the AC1 procedure enumerates, `normalize_audio()` at both `two_pass = TRUE`
  and `two_pass = FALSE`, each signalling exactly one warning for one drop. The
  pre-existing `test-audio-track-drop.R` (35 tests, 106 passing) is green
  unchanged in substance, and the full suite is 0 failures / 8222 passing /
  5 skips (all binary-capability skips).
- **AC3 — pass.** `test-parallel-option-carry.R`: 22 tests, 96 passing,
  0 failing, 0 skipped (the file's fingerprint guard did not fire — the package
  was installed before the run). `carried_option_values()` carries
  `tidymedia.check_tracks` raw, present in the list whether set or unset. A
  six-element `furrr` fan-out under `options(tidymedia.check_tracks = FALSE)`
  spans ≥2 worker PIDs and every worker reads `FALSE`, against an unset-parent
  control where every worker reads its own default. A `parallel = TRUE`
  `ffm_batch()` run leaves the parent's setting exactly as it found it under
  both the `FALSE` and the unset cell — so the criterion holds on either
  reading of its "leaves the option unset in the parent" clause.
- **AC4 — pass (re-review, against the amended wording).** Measured directly,
  not only through the suite: on a three-row jobs table of three distinct
  inputs, one row naming an `audio_stream`, the sweep's rows cover N = 2
  distinct inputs and `cli`'s `logger` handler reports exactly
  `c("0/2 created", "2/2 terminated (done)")` — one bar, total 2, reaching 2/2,
  with N smaller than the table's own distinct-input count of 3. The two
  further cells draw no bar at all: the seam `FALSE` and every row naming a
  track both yield `character(0)`. `test-check-tracks-seam.R` now carries the
  mixed-table cell alongside the original: 13 tests, 77 passing, 0 failing,
  0 skipped (was 12/76 before the amendment round). The earlier round's failure
  — a bar totalling 2 against a criterion that quantified over the table's own
  three distinct inputs — is exactly the cell the amended criterion now names,
  and the shipped code is unchanged by the amendment.
- **AC5 — pass.** Verified against the **installed** help, not `man/`: the doc
  guard was run from a scratch directory outside the source tree, where
  `rd_sources()` falls through to `tools::Rd_db("tidymedia")` (81 topics) —
  6 tests, 45 passing, 0 failing, 0 skipped. The guard walks the namespace for
  exported functions reaching `warn_dropped_audio` (finds exactly the six) and
  asserts on each verb's own topic: "one FFprobe call per distinct input", the
  session form `options(tidymedia.check_tracks = FALSE)` and the `withr` form
  matched separately, and "serially at the front door" present on exactly the
  three `_batch` verbs and absent from the three scalar ones.
- **AC6 — pass.** Same installed-help run: the `tidymedia-package` topic's
  dropped-track sentence names `extract_audio()`, `convert_audio()` and
  `normalize_audio()` and says `separate_audio_video()` runs a different
  multi-track diagnostic; a `Session options` section documents
  `options(tidymedia.timeout`, `options(tidymedia.check_tracks` and
  `options(tidymedia.nvenc_encoders`. NEWS.md's entry names the option, states
  it "defaults to TRUE", and names "one FFprobe call per distinct input" as the
  cost declined. No milestone number appears in NEWS.md.
- **AC7 — pass.** `devtools::check()` on the branch: **0 errors, 0 warnings,
  0 notes**, `Status: OK`, 2m 57.5s. Nothing to compare against `master`'s
  note list, since this branch carries none.

### Re-review after the AC4 amendment

The consistency gate, the three lenses and the outcome below are the re-review
round's. The pre-amendment round's evidence for AC1-AC3 and AC5-AC7 stands
above unchanged; every check below was re-run fresh on the amended branch.

### Consistency gate

`cairn_validate.py` exit 0 — all 16 checks PASS, all 7 advisories OK
(`release window` did not fire), re-run on the amended branch. No `DESIGN.md`
principle changed (`Principles touched: —`), so `cairn_impact.py` was skipped.
Toolchain checks from the `r-package` profile's `consistency-gate` slot, all
re-run fresh: `devtools::document()` leaves no diff in `man/`, `NAMESPACE` or
`DESCRIPTION`; `pkgdown::check_pkgdown()` reports "No problems found";
`README.Rmd`/`README.md` untouched; NEWS.md carries the entry and names no
milestone number; the diff adds no top-level file, so no `.Rbuildignore` entry
is owed; `devtools::test()` is 1283 tests / 8223 passing / 0 failing / 5 skips
(all binary-capability skips); `devtools::check()` is **0 errors, 0 warnings,
0 notes**, `Status: OK`, 2m 36.2s.

### Independent fresh-context review

Three lenses, distinct evidence bases, none having seen the implementation.
The blame-history [S] lens and the prior-review [S] lens each reported zero
defects: the M075 two-pass fix is preserved at both `normalize_audio()` sites,
the seam's last-conjunct ordering matches the timeout seam's precedent, the
raw carry follows the encoder override's rule rather than the limit's, and the
M44 counting-mock and M51/M59 installed-help lessons are both honoured. The
prior-review lens additionally probed
`gh api repos/jmgirard/tidymedia/pulls/comments` and found no inline review
comments on any PR in this repo, so the GitHub thread surface was skipped per
its gating rule. The diff-bug [O] lens reported nine findings, ranked below in
its order, each verified against the implementation before disposition.

- **F4 — the bar's total is not "one per distinct input" on a mixed jobs
  table.** *Amendment return.* Verified: `0/2` on a three-distinct-input table
  where one row names a track. Falsifies AC4 as written and makes the verbs'
  "one FFprobe call per distinct input" sentence an overstatement by the count
  of rows that named a stream. The shipped behaviour is right; AC4's wording
  is what needs the gated amendment.
- **F1 — the documented motivating case for the switch is the one case where
  it buys nothing.** *Fix now (proposed).* Verified: `warn_dropped_audio_batch()`
  returns at `length(rows) == 0` *above* the seam, and the scalar sites gate on
  `is.null(audio_stream)`, so a table whose every row names an `audio_stream`
  already costs zero FFprobe calls on `master`. NEWS.md's "That is worth
  declining on a large jobs table whose rows all name an `audio_stream`
  already", `R/tidymedia-package.R`'s "it is worth having on a large jobs table
  whose rows all name an `audio_stream` anyway" (also ambiguous about what "it"
  is), and `R/timeout.R`'s "one a caller who always names an `audio_stream`
  never gets anything for" all point at the wrong population — the switch helps
  a large table whose rows do *not* name a track.
- **F3 — D060's "the rule's 'outcome' clause is not reached" is overbroad.**
  *Fix now (proposed), by a superseding entry.* Verified: D024's outcome clause
  enumerates ran / skipped / succeeded / failed; D060 rebuts only
  succeeded-vs-failed, while the milestone's own tests pin that no bar exists
  when the sweep is *skipped* — so ran-vs-skipped is observable through the bar.
  D060's load-bearing defence (cli progress signals `cli_message`/`cliMessage`,
  the same mechanism as the warning) survives intact; the one sentence does not.
  History is append-only, so this takes a superseding entry, not an edit.
- **F5 — `res[[i]] <- count_audio_streams(...)` would shrink the list on a
  `NULL`.** *Fix now (proposed), one line.* Verified latent, not live:
  `count_audio_streams()` returns a count, `NA_integer_`, or a timeout
  sentinel, never `NULL`. `lapply()` preserved a `NULL`; the rewritten `for`
  does not, and the following `vapply(res, …, integer(1))` would then be
  length-mismatched against `uniq`.
- **F9 — the two deferred shapes have no candidate rows.** *Fix at hygiene
  (proposed).* Verified: Scope says a per-verb `check_tracks =` argument and
  probing inside the fan-out each "stays a candidate row", but both live only
  in the prose of the *retired* M44 row in `cairn/ROADMAP.md`, which itself
  says they "need their own row".
- **F6 — the `See vignette(…)` navigation paragraph now closes the new
  *Session options* section.** *Follow-up candidate (proposed).* Verified in
  `man/tidymedia-package.Rd`: the paragraph falls inside `\section{Session
  options}`. It was already section-captured on `master` (inside *Bounding a
  run that hangs*), so the diff moved a pre-existing defect rather than
  introducing one.
- **F2 — the sweep's bar is not gated on the batch verbs' own `progress`
  argument.** *Reject, with the falsifier standing.* The design point is
  correct and deliberate: D060 states it and records "a report of the bar
  appearing on a batch whose caller had switched progress off and did not want
  it" as a named falsifier. The finding's magnitude claim did not reproduce
  here: a real four-input `extract_audio_batch()` swept in 0.26 s and rendered
  no bar, and `Checking audio tracks` appears zero times in the whole
  `R CMD check` log. Promote the falsifier on a real report.
- **F7 — NEWS's "carried into `parallel = TRUE` workers" implies a worker-side
  effect that does not exist yet.** *Reject.* Verified: the sentence is true of
  the carrier, which is what it claims; no package code reads the option in a
  worker today because the probe runs at the front door, which the same
  paragraph says.
- **F8 — new console noise in the test suite.** *Reject; did not reproduce.*
  `Checking audio tracks` appears zero times in the `R CMD check` log and zero
  times in the `devtools::test()` run.

### Independent fresh-context review (re-review round)

The full three-lens fan-out ran again — user-facing tier, executable surface
touched — this time over the incremental diff `git diff b3088fa..HEAD` (9
files, +80 / -14), the amendment round's changes, which no reviewer had seen.
The blame-history [S] lens and the prior-review [S] lens each reported zero
findings: the `res[i] <- list(...)` form restores exactly what the `lapply()`
it replaced did, D061 appends rather than editing D060 per the repo's
append-only convention, and the reworded seam claims check out against
`R/ffmpeg.R`'s gating. The prior-review lens re-probed
`gh api repos/jmgirard/tidymedia/pulls/comments`, found it empty again, and
skipped the GitHub thread surface. The diff-bug [O] lens reported four
findings, ranked below in its order.

- **F10 — the three `_batch` verb topics still overstate the cost on a mixed
  jobs table.** *Fix now (proposed).* Verified: `R/ffmpeg.R:4293`, `:5095` and
  `:5227` each read "costs **one FFprobe call per distinct input**, so a
  repeated input is probed once", and each says the check is skipped only when
  *every* row names a track — none carries the per-row exemption. This is the
  same misstatement F1 repaired at its three sites (`NEWS.md`,
  `R/tidymedia-package.R:134`, `R/timeout.R:47` each now say a row naming an
  `audio_stream` is not probed at all); the round missed these three. It
  overstates in the harmless direction — a caller reading it budgets more
  probes than run — so it is not a return-floor defect, but it is the one
  place the shipped documentation is still wrong about the thing this
  milestone documents. AC5's guard matches the fixed substring "one FFprobe
  call per distinct input", so the qualifier can be appended without touching
  the criterion.
- **F11 — D061 concedes D024's clause is reached without naming the exception
  that still licenses the bar.** *Reject.* Verified against
  `cairn/DECISIONS.md:588-596` and `:2816-2841`: the finding is right that
  D061's "What still stands" paragraph makes the channel-identity argument
  rather than spelling out that a `cli` progress condition *is* a signalled
  diagnostic condition and so falls under the clause's own exception. But that
  is the same argument one step earlier, and D060 — which D061 leaves standing
  and re-states — carries it. A superseding entry to add one restating sentence
  costs more than it buys.
- **F12 — the F5 fix restores an abort rather than a fail-open.** *Reject.*
  Verified: mocking `count_audio_streams()` to return `NULL` makes
  `vapply()` at `R/ffprobe.R:283` abort. But that is the behaviour on `master`
  too, so the diff neither introduced nor worsened it, and the finding itself
  records the case as unreachable — `count_audio_streams()` returns a count,
  `NA_integer_` or a timeout sentinel. A pre-existing issue the diff did not
  introduce.
- **F13 — the mixed-table cell does not by itself separate "distinct inputs"
  from "rows swept".** *Reject.* Verified: its table has three rows, three
  distinct inputs and two swept, so `length(rows)` and
  `length(unique(inputs))` coincide at 2 and a `length(rows)` mutant would
  pass it. The sibling cell at `test-check-tracks-seam.R:183` (four rows, three
  distinct inputs) pins distinctness, and the lens confirms the two cells
  together cover AC4's wording. A coverage nit on a criterion that passes.

### Outcome

All seven acceptance criteria verified with fresh evidence. AC4's amended
wording passes on the cell it names — a mixed table where N = 2 is smaller
than the table's own three distinct inputs — with the shipped code unchanged
by the amendment. The consistency gate is clean. Thirteen findings across two
review rounds are logged with their dispositions; none was dropped. No finding
meets the return floor: F10 is a documentation overstatement in the
conservative direction, and F11-F13 are rejected on the reasons recorded above.
