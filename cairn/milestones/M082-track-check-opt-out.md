# M082: The track check has an off switch, and says what it costs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m082-track-check-opt-out`

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

- [ ] AC1 — Every dropped-track probe site in `R/ffmpeg.R`, the set
      `grep -n 'warn_dropped_audio' R/ffmpeg.R` returns, is gated on the seam.
      For each site, a test on a multi-track input counts calls to
      `count_audio_streams_all()`: zero under
      `options(tidymedia.check_tracks = FALSE)`, and at least one — with the
      `tidymedia_dropped_audio` warning signalled — under the default. The
      count is a counter, not a `stop()`ing mock (M44 lesson). At each of those
      sites a value that is not a length-1 non-`NA` logical (`"yes"`, `NA`,
      `c(TRUE, TRUE)`, `1`) aborts naming `tidymedia.check_tracks` as the
      offending argument.
- [ ] AC2 — With the option unset, every verb the AC1 procedure enumerates
      signals the same conditions on the same inputs as it does on `master`,
      including `normalize_audio()` under both `two_pass = TRUE` and
      `two_pass = FALSE` signalling exactly one warning for one drop (M075).
- [ ] AC3 — A `parallel = TRUE` batch run under
      `options(tidymedia.check_tracks = FALSE)` leaves the option unset in the
      parent afterwards, and a worker sees `FALSE` — the same round trip
      `carried_option_values()` already makes for `tidymedia.timeout`.
- [ ] AC4 — `warn_dropped_audio_batch()` reports progress across its probe
      sweep: on a jobs table of N distinct inputs the sweep drives one `cli`
      progress bar whose total is N and which reaches N/N; with the seam
      `FALSE` no bar is created.
- [ ] AC5 — Every verb the AC1 procedure enumerates documents, in its own
      help topic, that the check costs one FFprobe call per distinct input and
      how to turn it off; the three `_batch` verbs also state that those probes
      run serially at the front door before the fan-out. Verified against the
      **installed** help, not `man/*.Rd` in the source tree (M51/M59 lesson).
- [ ] AC6 — `?tidymedia-package` names `normalize_audio()` and
      `normalize_audio_batch()` among the verbs behind the dropped-track check
      (stale since M075) and documents `tidymedia.check_tracks` beside the
      other two seams; NEWS.md gains an entry naming the option, its default,
      and the cost it lets a caller decline.
- [ ] AC7 — `devtools::check()` clean: 0 errors, 0 warnings, 0 notes beyond
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

- [ ] T1 — Add `resolve_check_tracks()` beside `resolve_timeout()`
      (`R/timeout.R:26`): `getOption("tidymedia.check_tracks", default = TRUE)`
      through `rlang::check_bool(arg = "tidymedia.check_tracks", call = call)`.
      Unit tests for the accepted and refused values first.
- [ ] T2 — Gate each site the AC1 grep returns. Scalar sites
      (`R/ffmpeg.R:544`, `:1015`, `:2217`, `:2262`) fold the call into the
      existing `isTRUE(run) && is.null(audio_stream)` condition; the batch
      sites gate inside `warn_dropped_audio_batch()` (`R/ffmpeg.R:427`) so one
      early return covers all three. **Both** `normalize_audio()` sites: its
      `if (two_pass)` block falls through (M075 lesson).
- [ ] T3 — Per-site tests: the invocation counter of AC1, the AC2 refusals,
      and the AC3 default-behaviour cells including normalize's two branches.
- [ ] T4 — Add `tidymedia.check_tracks` to `carried_option_values()`
      (`R/timeout.R:462`), carried raw like the encoder override rather than
      resolved like the timeout, plus the round-trip test.
- [ ] T5 — Progress bar over the `count_audio_streams_all()` sweep in
      `warn_dropped_audio_batch()`, with the bar-total and seam-off tests.
- [ ] T6 — Roxygen: the cost and the seam on `extract_audio()`,
      `convert_audio()`, `extract_audio_batch()`, `convert_audio_batch()`,
      matching the wording M075 shipped at `R/ffmpeg.R:2092` and `:4262`; the
      seam on all six; `R/tidymedia-package.R:55`'s verb list and options
      section. `devtools::document()`, then the installed-help test.
- [ ] T7 — NEWS entry; `devtools::document()`, `devtools::test()`,
      `devtools::check()`.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: plan gate chose the `tidymedia.check_tracks` option seam over a per-verb `check_tracks =` argument because D047 already declined that trade for the timeout — a seam commits no exported signature, leaving D014's pre-0.2.0 clean break unspent — and `R/timeout.R:459` records that a third seam is one line; falsified by a report of a caller needing two different answers inside one script, which is the case a session-wide switch cannot serve and D051 had to ship `with_timeout()` for.
- 2026-08-28: plan gate chose `withr::local_options()` over exported `with_check_tracks()`/`local_check_tracks()` wrappers because `withr` is a hard dependency (D052) and a boolean needs none of the whole-number validation that earned `with_timeout()` its existence; falsified by the seam growing a value that needs refusing before it is set.
- 2026-08-28: plan gate chose a progress bar over relocating the batch probe into the fan-out because the latter needs the `ffm_batch()` hook D024/RR02 Q3 rejected; falsified by a measured batch where the serial sweep, not the encoding, dominates wall clock even with progress shown.
- 2026-08-28: plan gate chose aborting on a malformed option value over falling back to `TRUE`, following `resolve_timeout()`; falsified by a report of a stale option in a startup file breaking calls unrelated to the check.
- 2026-08-28: criteria audit ran in **full** mode (user-facing tier), inline rather than in a fresh-context [O] subagent — subagents are disabled in this session, so the instrument was weaker than the skill specifies. Two findings, both fixed before the gate: the seam-gating and the verb-documentation criteria quantified over "all six verbs" as a hand-list, repaired to quantify over the set the stated `grep` returns (bounded-promise rule); the unchanged-default criterion promised "the existing dropped-track test file passes unmodified", an instrument property, narrowed to the verbs' default behaviour. Nine drafted criteria then tripped the >7 sizing advisory and were merged to seven — option validation folded into the gating criterion, the package topic and NEWS into one documentation criterion — rather than split into a second milestone: the code and the contract it documents are one deliverable, and D024 requires a diagnostic that can silently not run to say so.
- 2026-08-28: amendment (substantive, AC4) — the per-input-condition instrument is not reproducible: measured on R 4.6.1, the same `cli` bar signalled 3 conditions over 5 instant updates, 0 over 3 updates delayed 50 ms each, and N+1 only when every update forces a redraw, which also bypasses `cli.progress_show_after` and would render a bar on every batch call. AC4 narrowed at the mini gate to the bar's own totals (N distinct inputs, reaching N/N; no bar when the seam is `FALSE`), which `cli`'s `logger` progress handler reports deterministically.
- 2026-08-28: the amended AC4 wording took the criteria audit's full-mode questions inline rather than in a fresh-context [O] reader — subagents are disabled in this session, so the instrument was weaker than the skill specifies. One repair before the gate: the first draft named `cli`'s `logger` handler in the criterion itself, an instrument property, and was rewritten to state the bar's behaviour and leave the observation channel to the test.
- 2026-08-28: implement gate chose the probe sweep's bar independent of the batch verbs' `progress =` argument, because that argument governs `ffm_batch()`'s run-time bar while this sweep is a front-door cost the caller has not declined, and `cli.progress_show_after` already hides the bar on sweeps under two seconds; falsified by a report of the bar appearing on a batch whose caller had switched progress off and did not want it.
- 2026-08-28: open for implement — AC4's progress bar makes the probe's HAVING RUN observable as something other than a condition, which D024's operative rule ("changes nothing observable except whether a diagnostic condition is signalled") does not obviously cover. `cli`'s progress mechanism does signal conditions, but that was not verified here. Settle it in the milestone's decision log before T5 ships, and promote to `cairn/DECISIONS.md` alongside the seam entry.

## Decisions

## Review
