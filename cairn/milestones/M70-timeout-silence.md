# M70: No timeout is silent

- **Status:** review
- **Priority:** normal
- **Depends on:** M69
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m070-timeout-silence / PR #74

## Goal

Close the gap M69 disclosed rather than fixed: some call paths absorb a
wall-clock timeout with no warning at all, so a bounded hang under
`remove_audio()` or in a provenance manifest is invisible to the caller. After
this, every call a reached limit kills either aborts or says so.

**Surface tier: user-facing** — exported readers and verbs, their runtime
behavior, and published documentation.

## Scope

**In:** the two no-warning paths M69's AC8 discloses — `count_audio_streams()`
(`R/ffprobe.R:199`, reached from `R/ffmpeg.R:438,545,638,748,1017`) and
`capture_version()` (`R/ffm_manifest.R:127`); a call-graph sweep that
enumerates the domain the silence rule quantifies over, replacing M69's
hand-list; J2's split program literal (`probe_one()` says `ffprobe`,
`R/ffprobe.R:204,242`, where the Layer 0 hatch says `FFprobe`,
`R/ffprobe.R:22`); J7's `tm_timed_out` attribute (`R/ffprobe.R:158`) leaking
onto a public return against `@param parallel`'s identity promise; the third
no-warning path the T1 sweep found and M69's hand-list did not: `ffm_batch()`
(`R/ffm_batch.R:127`) records every job failure as `success = FALSE` and signals
no condition, so a reached limit is silent through it and through the 15
`_batch` verbs and `segment_video()`, which fan out through it; the doc
rewrite that retires M69's disclosure; a D-entry.

**Out:**
- Per-call `timeout =` arguments, and making `parallel = TRUE` workers see the
  option → the existing ROADMAP candidate row.
- A tighter kill than base R's SIGINT/SIGTERM/SIGKILL ladder (`processx` or
  equivalent) → new ROADMAP candidate row. M69's return-2 gate rejected it and
  the row was never written; this plan writes it.
- Changing what `count_audio_streams()` RETURNS. D024 licenses the probe only
  while its outcome changes nothing but whether a warning fires, and a warning
  is inside that licence where a changed count is not.

## Acceptance criteria

- [x] AC1 For every function the T1 sweep returns, a forced timeout through it
      signals at least one condition the caller can see — an abort of class
      `tidymedia_timeout`, or a warning naming the timeout. None absorbs it
      silently. Linux and macOS only, since the writer-less FIFO fixture cannot
      be built on Windows (M69 AC3); the sweep itself runs on every platform.
- [x] AC2 `count_audio_streams()`'s timeout warning fires once per call at the
      point the counts are assembled, naming how many inputs timed out — not
      once per file. Evidence: a 3-input batch with 2 timed-out inputs signals
      exactly one warning, and the counts it returns are identical to those the
      silent version returned, so D024's licence still holds.
- [x] AC3 `probe_all()`'s return carries no `tm_timed_out` attribute;
      `verify_media()` reaches its refusal through its own non-absorbing probe.
      Evidence: `identical()` holds between the `parallel = TRUE` and
      `parallel = FALSE` returns as `@param parallel` promises, and a hung
      input still makes `verify_media()` abort rather than report a mismatch.
- [x] AC4 Each program is named by one literal across every path that can abort
      about it, asserted against the real call path rather than a mock's
      literal. Evidence: a timeout raised through `probe_one()` and one raised
      through `ffprobe()` name the program identically.
- [x] AC5 `?tidymedia` and `NEWS.md` state the uniform rule — a reached limit
      either aborts or warns, never passes unremarked — and M69's no-warning
      disclosure is gone from both, along with the guard that fenced it.
- [x] AC6 `cairn/DECISIONS.md` gains a D-entry recording the silence rule, the
      per-call warning grain chosen over per-file, D024's licence as the reason
      a warning is available where a changed return is not, and the falsifier.
      It supersedes the disclosure half of M69's D-entry.
- [x] AC7 The `verify` slot of `cairn/PROFILE.md` is clean —
      `devtools::document()`, `devtools::test()` and `devtools::check()` (0
      errors, 0 warnings) — and CI is green on the PR.

## Coverage

- AC1 → T1, T2, T3, T9
- AC2 → T2
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1 Build the sweep that enumerates the silence rule's domain: deparse
      `mget(ls(asNamespace("tidymedia"), all.names = TRUE))` — M59's route, so
      it runs under `R CMD check` as well as `devtools::test()` — close the
      call graph to every function reaching `run_program()`, `system()` or
      `system2()`, and keep those that install a condition handler around it.
      The handler set is R's own condition API, recorded in the guard; a
      function installing none propagates by R's semantics and is out of the
      domain for that stated reason. Record current membership; mutation-probe.
- [x] T2 `count_audio_streams()`: signal the timeout at the aggregation sites
      (`R/ffmpeg.R:438,748` and the scalar sites `545,1017,638`), once per
      call. Tests first, including the unchanged-counts assertion AC2 needs.
- [x] T3 `capture_version()`: signal a timeout rather than recording `NA`
      silently. Tests first.
- [x] T4 Drop the `tm_timed_out` attribute; give `verify_media()` its own
      non-absorbing probe. Tests first, including the `identical()` assertion.
- [x] T5 One literal per program across the abort paths. Mutation probe must
      VARY the literal, not merely delete the assertion — deleting it
      re-certifies the mock, which is the defect J2 found.
- [x] T6 Rewrite both doc sections to the uniform rule; retire M69's
      disclosure guard with the disclosure. Mutation-probe the new guard.
- [x] T7 Write the D-entry.
- [x] T8 Run the `verify` slot end to end; open the PR and confirm CI green
      before review, which is the gate that caught M69's AC3 failure.
- [x] T9 `ffm_batch()`: keep `run_one()`'s caught error object, test it for
      class `tidymedia_timeout`, and warn once at the end of the run naming how
      many jobs timed out. Non-timeout job failures keep today's silent
      `success = FALSE`. Tests first.

## Work log

- 2026-08-26: created by /milestone-plan, splitting M69 after its third defect return fired thrash trigger (a).
- 2026-08-26: implement gate — AC1 forcing = injected `abort_timeout()` condition per swept member, with the writer-less-FIFO fixture kept for anchor paths; AC3 shape = internal `absorb=` flag on the shared `probe_all()` worker; AC4 literal = display case (`FFmpeg`/`FFprobe`/`MediaInfo`) everywhere.
- 2026-08-26: T1 — sweep built (`tests/testthat/helper-timeout-sweep.R`, `test-timeout-silence.R`). Domain = the 53 exported functions reaching `system`/`system2` through a symbol-mention closure over the namespace; membership recorded, mutation-probed (empty seed set collapses it; `ffm_compile` and the pure builders stay out; `run_program()` is derived, not seeded), and every member carries a call spec so a member with no way to be driven fails rather than being skipped. Deliberately NOT M62's call-head graph: `probe_all()` reaches FFprobe only via `purrr::map(infile, probe_one)`, so a head-only walk drops it, the four `probe_*()` accessors and `verify_media()`. Pinned as a test.
- 2026-08-26: T2 — `count_audio_streams()` returns the absorbed-timeout sentinel instead of a silent NA, and a new `count_audio_streams_all()` assembles the counts for all five sites (`R/ffmpeg.R:437,543,636,745,1014`), warning once per call with class `tidymedia_probe_timeout` and the count of timed-out inputs. Returned counts unchanged (NA for a killed probe), so D024's licence still holds.
- 2026-08-26: T3 — `capture_version()` returns the sentinel; `tool_versions()` warns once per call naming which tools the limit killed (class `tidymedia_probe_timeout`). Recorded manifest value unchanged (NA). A missing binary is still a silent NA.
- 2026-08-26: T9 — `ffm_batch()` warns once per run (class `tidymedia_batch_timeout`) naming how many jobs, and how many verifications, the limit killed; `run_one()` now returns a per-job record so the fact survives `unlist()` off the parallel workers. Non-timeout failures keep today's silent `success = FALSE`. AC1's 53-member grid now passes, with a mutation probe standing a swallowing implementation in front of one member, and three real-FIFO anchors (`ffprobe()` condition identity, `count_audio_streams_all()`, `ffm_batch()`). `local_blocking_input()` moved to `helper-timeout-sweep.R` so both suites share one fixture.
- 2026-08-26: T4 — `probe_all()`'s body factored into `probe_all_impl(absorb =)`; `absorb = FALSE` re-raises, so `verify_media()` gets its refusal from the shared body instead of a `tm_timed_out` attribute on a public return. Attribute gone; `identical()` holds between the `parallel = TRUE` and `parallel = FALSE` returns with a hung input.
- 2026-08-26: T5 — `probe_one()`/`count_audio_streams()` (`ffprobe`), the MediaInfo readers (`mediainfo`) and `capture_version()` now use the display literals the Layer 0 hatches already used. `tm_program_literals()` reads the `program` argument out of every `run_program`/`guard_timeout`/`abort_timeout`/`capture_version` call node in the namespace (named or positional) and the guard asserts exactly `FFmpeg`/`FFprobe`/`MediaInfo`; the mutation probe VARIES a literal rather than deleting the assertion. Real-path evidence: a FIFO-hung `probe_one()` and `ffprobe()` name the program identically.
- 2026-08-26: T6 — `?tidymedia` and `NEWS.md` now state the uniform rule ("a reached limit is never silent") over two lists, and say the lists are derived from the call graph rather than recalled. M69's scoped-claim and no-warning-disclosure guards retired from `test-runtime-timeout.R`; the replacement guard in `test-timeout-silence.R` asserts the new rule, asserts the retired text is absent, and mutation-probes both against a stand-in carrying M69's sentences. `doc_timeout_sources()` moved to `helper-rd.R` so both suites share one reader.
- 2026-08-26: T7 — D049 written, superseding D048's fourth and fifth bullets (the disclosure and the rejected fourth hand-partition); the rest of D048 stands.
- 2026-08-26: T8 — `devtools::document()` no diff, `devtools::test()` 6331 passing, `devtools::check()` 0/0/0. PR #74 opened.
- 2026-08-26: CI caught what local checks could not. The AC1 grid forced the timeout VERDICT, which still let the three Layer 0 hatches really shell out; CI's macOS and Windows runners install no media binaries, so `system(intern = TRUE)` raised a cmdError on "command not found" before the verdict was consulted and six cells measured the runner's PATH. The forcing now injects at `run_program()` and `guard_timeout()` — the two wrappers in front of every `system()`/`system2()` call — so no cell spawns anything. Verified locally with the binaries off PATH; a guard pins that the four spawn sites are exactly those two wrappers' reach.
- 2026-08-26: self-review found a blame regression from T4: moving `probe_all()`'s body into `probe_all_impl()` left its own argument refusals reading "Error in `probe_all_impl()`". `call` threaded through every refusal; guard added.
- 2026-08-26: all nine tasks done; CI green on all seven checks at 00f7bfc (macOS, Windows, Ubuntu release/devel/oldrel-1, pkgdown, test-coverage). Status → review.
- 2026-08-26: substantive amendment (Scope In + T9 + Coverage AC1). The T1 sweep found a third no-warning path M69's hand-list missed — `ffm_batch()` absorbs every job failure into `success = FALSE` and signals nothing, leaving 17 of the 53 swept exports silent under a forced timeout. AC1 as written already binds them; Scope gains the path, T9 gains the fix (warn on timed-out jobs only; non-timeout failures unchanged).
- 2026-08-26: review — all seven ACs verified with fresh evidence; consistency gate clean (`cairn_validate` exit 0, `document()` no diff, `check()` 0/0/0, pkgdown clean); CI green on nine checks. Three review lenses returned 13 findings, no correctness bug: three fixed now (doc the three condition classes and the `suppressWarnings()` recipe; the `parallel = TRUE` clause on the batch warning promise; one cli `.arg` token), two rejected as style/speculative, eight routed to a new candidate row. Return floor not reached.

## Decisions

## Review

Reviewed 2026-08-26 on branch `m070-timeout-silence`, PR #74. `origin/master`
(7ce9d51) is an ancestor of HEAD, so no merge was needed before gathering
evidence.

### Acceptance criteria — fresh evidence

- **AC1 — verified.** `tm_timeout_domain()` returns **53** exported functions;
  the AC1 grid drives a forced timeout through every one and each signals at
  least one visible condition (`test-timeout-silence.R`, "no swept function
  absorbs a forced timeout silently"). The grid spawns nothing — the forcing
  injects at `run_program()`/`guard_timeout()`, and a guard pins those two
  wrappers as the whole reach of the four spawn sites, so the cells measure the
  package rather than the runner's PATH. The mutation probe (a swallowing
  implementation stood in front of one member) reddens it. Three real-FIFO
  anchors pass on macOS with `NOT_CRAN=true`.
- **AC2 — verified.** Direct measurement, not only the suite: a 3-input call
  with 2 forced timeouts through the real `count_audio_streams_all()` signalled
  **exactly one** `tidymedia_probe_timeout` warning — "The audio-track check
  timed out on 2 inputs after 2 seconds", naming both inputs — and returned
  counts `NA, NA, 3`, which is what the silent version returned, so D024's
  licence still holds. A non-timeout probe failure stays a silent `NA`.
- **AC3 — verified.** `grep -rn tm_timed_out R/` finds the token only inside
  two historical comments; no code sets it. `expect_identical(names(attributes(
  out)), "names")` holds on `probe_all()` with a hung input, and
  `identical()` holds between the `parallel = TRUE` and `parallel = FALSE`
  returns. `verify_media()` aborts with class `tidymedia_timeout` naming
  FFprobe, through `probe_all_impl(absorb = FALSE)` rather than an attribute.
- **AC4 — verified.** `tm_program_literals()`, reading the `program` argument
  out of every `run_program`/`guard_timeout`/`abort_timeout`/`capture_version`
  call node in the namespace, yields exactly `FFmpeg`, `FFprobe`, `MediaInfo`.
  Real-path evidence: a FIFO-hung `probe_one()` and a FIFO-hung `ffprobe()`
  name the program identically. The mutation probe VARIES a literal and the
  guard reddens.
- **AC5 — verified.** `?tidymedia` and `NEWS.md` both open the timeout section
  with "A reached limit is never silent" and state the abort/warn rule over two
  derived lists. `grep` for M69's retired text ("no warning", "not a complete
  partition", "three answers", `count_audio_streams`, `tool_versions`) finds
  nothing in `R/tidymedia-package.R`, `NEWS.md` or `man/tidymedia-package.Rd`.
  M69's two disclosure guards are gone from `test-runtime-timeout.R` (-86
  lines); the replacement guard reddens against a stand-in carrying M69's
  sentences.
- **AC6 — verified.** D049 is written, superseding D048's fourth and fifth
  bullets and leaving the rest standing. It records the uniform rule, the
  derived domain, the third path the sweep found, the per-call warning grain
  over per-file, D024's licence as the reason a warning is available where a
  changed return is not, the sentinel staying off public returns, and the
  falsifier.
- **AC7 — verified.** `devtools::document()` produces no diff;
  `devtools::test()` is 6331 passing / 0 failing / 5 skipped;
  `devtools::check()` is Status OK, 0 errors / 0 warnings / 0 notes. CI on
  PR #74 is green on all nine checks (macOS, Windows, Ubuntu
  release/devel/oldrel-1, pkgdown, test-coverage, both codecov gates).

No `Driving RR:` on this milestone, so the projection-vs-outcome record
no-ops.

### Consistency gate

`cairn_validate.py` exits 0 — all sixteen PASS checks pass, seven advisories
OK, and the `release window` advisory did not fire. No `DESIGN.md` principle
changed, so `cairn_impact.py` was skipped. Toolchain slot: `document()` no
diff; generated files unedited (`NAMESPACE` unchanged, `man/` regenerates
clean); `README.Rmd` untouched so `README.md` is in sync;
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries the
user-visible change with no milestone numbers; no new top-level files;
`devtools::check()` clean.

### Independent review — three lenses, distinct evidence bases

Executable surface and a user-facing tier, so the full fan-out ran: an Opus
diff-bug lens against the ACs/DESIGN/DECISIONS, a Sonnet blame-history lens on
the modified lines, and a Sonnet prior-review lens over the archive.

The prior-review lens reported **no finding**: it checked the diff against
M69's J2 and J7, D047's status-124 rule, D024's licence, M44's per-call
warning-collapse gate and D042's call-threading rule, and found each honoured
rather than reintroduced.

The blame-history lens found no violation of prior milestone intent, and
raised two minor items (below).

Thirteen findings in all, ranked as their reviewers ranked them, each with its
disposition. The diff-bug lens found no functional correctness bug in the
shipped behavior.

| # | Finding | Disposition |
|---|---|---|
| O1 | The two new classed warnings are undocumented, and one of them breaks a documented suppression recipe (`tidymedia_probe_timeout`, `tidymedia_batch_timeout` appear in no `.Rd` and no NEWS entry; a user who followed the documented `suppressWarnings(classes = "tidymedia_dropped_audio")` line now gets an unsuppressed `tidymedia_probe_timeout` from the very same probe, with no documented class to add) | **fixed now** |
| O9 | The universal claim and the caveat that qualifies it are several paragraphs apart (`?tidymedia` promises `ffm_batch()` warns "how many jobs the limit killed", unqualified, while the sentence making it true — workers do not see the option — is two paragraphs below) | **fixed now** |
| O10 | `{.arg tidymedia.timeout}` marks an option as a function argument, where every other reference writes `{.code options(tidymedia.timeout = )}` | **fixed now** |
| O2 | `tm_timeout_absorbers()` cannot see the package's own absorber — `tm_condition_api` lists only R's condition functions, so `probe_one`, `mediainfo_read` and `mediainfo_parameter`, which swallow via `absorb_timeout()`, never appear in the recorded absorber list | follow-up (new candidate row) |
| O3 | AC4's literal guard passes on a *missing* `program=` argument — `run_program()`'s formal default is `program = "the program"`, so a call omitting it would abort with a fourth literal while `tm_program_literals()` contributes nothing for that call | follow-up (new candidate row) |
| O4 | `ffm_batch(parallel = TRUE)`'s changed result contract is exercised by no test, and cannot time out anyway — the only parallel batch test passes `run = FALSE`, and `multisession` workers do not see the option, so `ran_out` is structurally `0` there | follow-up (new row for the untested half; the unreachability half extends the existing timeout-residues row) |
| O5 | AC2's stated evidence is tested one level below the batch it names — every test calls `count_audio_streams_all()` directly, never a `_batch` verb, and the AC1 grid asserts only *at least one* condition | follow-up (new candidate row) — the criterion itself is met (see AC2 above, measured directly), but the "exactly one at the verb level" assertion is unfenced |
| O6 | `run_with_progress()`'s changed return contract is only covered behind `skip_if_no_ffmpeg()`, and a mismatch is a hard `vapply` type error | follow-up (new candidate row) |
| O7 | The AC1 `warned` verdict is a substring match on cli-*formatted* text — `probe_all()`'s bullet fails the match at `cli.width` 20-22 and `count_audio_streams_all()`'s at 28-31, though both warnings carry classes that would test exactly | follow-up (new candidate row) |
| O8 | The doc guards grep the whole of NEWS.md, not the timeout paragraph, so an unrelated future release note containing "no warning" would redden a guard about M69's retired disclosure | follow-up (new candidate row) |
| O11 | Latent blame mismatch through `verify_media()` — `probe_all_impl()`'s threaded `call` would make its argument refusals name `infile`, which `verify_media()` has not; unreachable today because `check_file_exists()` refuses first | follow-up (new candidate row) |
| [S] blame 1 | `tests/testthat/helper-rd.R:41-42` has an awkward mid-sentence line wrap in a comment moved out of `test-runtime-timeout.R` | **rejected** — pure style nitpick, out-of-scope taxonomy |
| [S] blame 2 | `run_one()`'s `verify_out <<- verify_out + 1L` sits two closures deep inside `ffm_batch()`'s frame, and a future refactor adding a layer would silently create a new local | **rejected** — resolves correctly today by R's lexical scoping, consistent with the `ran_out`/`hit` pattern used throughout the branch; a speculative future refactor is not a defect in this diff |

**Return floor: not reached.** No finding demonstrates an acceptance criterion
failing inside its named procedure's domain — AC4 holds because no call site
omits `program=` today (all sites verified by grep), and AC2's criterion is met
at the site its own text names ("the point the counts are assembled"), measured
directly above. Nothing is a load-bearing defect in what the package does for
its users. The three fix-now items are documentation and one cli style token.

**Fix-now work** (committed on the branch before the approval marker):
`?tidymedia` now names the three condition classes and says explicitly that
adding `"tidymedia_probe_timeout"` to the documented `suppressWarnings()`
recipe also silences the notice that the limit stopped the check (O1); the
`ffm_batch()` sentence gains the `parallel = TRUE` clause (O9); the cli `.arg`
token becomes plain prose (O10). Re-verified after the edits:
`devtools::document()` no diff, `devtools::test()` 6331 passing / 0 failing,
`devtools::check()` 0/0/0.

