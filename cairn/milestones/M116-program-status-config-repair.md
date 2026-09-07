<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M116: A broken or stale remembered location is reported, not fatal or silent

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — changes what the four `find_*()` exports,
  `program_status()`, `unset_program()` and the batch manifest's timeout
  warning do and say.
- **Branch/PR:** `m116-program-status-config-repair` — https://github.com/jmgirard/tidymedia/pull/120

## Goal

Close the six defects M113's review measured in what `program_status()` and its
seam report about a remembered program location.

## Scope

**In:** `find_program()`'s handling of a config file it cannot read as one line;
the advice and condition class on its stale-location warning; which of its
warnings `program_status()` surfaces; the wording of `tool_versions()`'s timeout
warning; the memo drop on `unset_program()`'s partial-removal path; and
`tool_versions()`'s `locations` length check.

**Out:** naming the package's other unclassed aborts and warnings, including
`find_program()`'s plain not-found warning → the naming-pass candidate row.
Extending the timeout sweeps' coverage of `program_status()` → the
conditionally-spawning-member candidate row; this milestone changes that
warning's text, not the sweeps' domain. A fourth `program_status()` column
showing the remembered location → weighed and rejected at the plan gate, no row
(the warning carries the fact instead).

## Acceptance criteria

- [x] AC1: A remembered-location file `readLines()` does not return exactly one
      line from — an empty file, or one holding two or more — makes
      `find_program()` warn with a `tidymedia_*` condition class carrying the
      program and the file path, and return `NULL`, where today it raises R's
      own `if` error. Tests cross both axes the state is free in: each of
      `find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()`, `find_mediainfo()`
      against the empty form, and each against the two-line form, asserting the
      class by name.
      `program_status()` over a config directory holding one such file returns
      four rows — the row whose `program` is that one carrying `NA` in
      `location` and `version`, the other three what the same call returns
      without the malformed file — and raises that warning once rather than
      suppressing it.
- [x] AC2: `find_program()`'s warning for a remembered location whose binary is
      gone keeps its `set_program()` advice, adds `unset_program()` as the
      repair for the remembered location itself, adds the `install_on_win()`
      offer on the same derived condition the not-found branch uses
      (`tm_os()` is `"windows"` and the program is one `tm_install_registers`
      lists), and carries a `tidymedia_*` class holding the program and the
      location. Tests cross operating system against program, including the
      `windows` + `mediainfo` cell where the installer bullet must be absent.
- [x] AC3: `program_status()` raises that stale-location warning rather than
      suppressing it, while a program that was never configured contributes
      none. A four-program call with one stale location and one
      never-configured program raises exactly one warning of that class,
      carrying the stale program's name. `?program_status`'s sentence promising
      `NA` "rather than a warning" narrows to the never-configured case, and
      `?find_program` documents both new conditions.
- [ ] AC4: `tool_versions()`'s `tidymedia_probe_timeout` warning names each
      timed-out program by the spelling `program_status()`'s `program` column
      uses (`"ffmpeg"`, not `"FFmpeg"`), and its remaining bullets read the same
      on both callers. Tests pin the emitted message on the `program_status()`
      path and on the `ffm_batch(manifest = TRUE)` path, and a mutant restoring
      the manifest-naming sentence turns the `program_status()` test red.
- [x] AC5: `unset_program()` discards the memoized FFmpeg capabilities whenever
      a removal took, before aborting `tidymedia_location_not_removed`, and
      leaves them alone where nothing was removed. Tests fire both partial
      forms — current-directory file removed with the legacy file left, and the
      reverse — asserting the abort class and an empty memo, plus the
      total-failure case asserting the memo survives.
- [x] AC6: `tool_versions()` aborts with a `tidymedia_*` class when a non-`NULL`
      `locations` differs in length from `programs`, leaving the `NULL` default
      path reaching `ffm_batch()` unchanged. Tests fire the silently-recycling
      length-1 case, the length-3-against-4 case base R warns on, and the
      empty-`list()` case, and assert `ffm_batch(manifest = TRUE)` still works.
- [ ] AC7: `devtools::test()` and `devtools::check()` clean (0 errors,
      0 warnings), `devtools::document()` produces no diff, and `NEWS.md`
      carries the user-visible changes.

## Coverage

- AC1 → T1, T2, T5, T9, T10
- AC2 → T3, T9, T10
- AC3 → T5, T10
- AC4 → T6, T11, T12
- AC5 → T4
- AC6 → T7
- AC7 → T8, T11, T13, T15, T16

## Tasks

- [x] T1: Regression tests for the malformed remembered-location state, crossing
      the four `find_*()` exports against the empty and two-line forms, under
      `tm_redirect_config()` (`tests/testthat/helper-program-config.R`). Record
      in the work log that each is red first, and which R error it raises.
- [x] T2: `find_program()` guards what it read back before
      `R/program_management.R:79`'s `if (Sys.which(location) == "")` — the
      `length(loc) != 1L` shape `count_audio_streams()` documents at
      `R/ffprobe.R:213-219` — classing the malformed state and returning `NULL`.
- [x] T3: Widen and class the stale-location warning at
      `R/program_management.R:80-88`, deriving the installer bullet from
      `tm_os()`/`tm_install_registers` the way the not-found branch at `:101-104`
      does rather than restating the condition; tests cross OS against program.
- [x] T4: `unset_program()` drops the memo on any removal that took, above the
      `tidymedia_location_not_removed` abort at `R/program_management.R:291-303`;
      tests for both partial forms and the total-failure case.
- [x] T5: `program_status()` (`R/program_management.R:183-185`) stops suppressing
      the two classed config warnings and keeps suppressing the plain not-found
      one; narrow `?program_status`, document both conditions on
      `?find_program`, add the mixed four-program test.
- [x] T6: Reword `tool_versions()`'s timeout warning (`R/ffm_manifest.R:154-166`)
      to the `program` column spelling and a caller-neutral sentence about what
      `NA` means; rename the local that overwrites the `programs` argument at
      `:154`; pin the message on both callers with the mutation probe
      `tests/testthat/test-timeout-silence.R:660-680` models.
- [x] T7: `tool_versions()` length check below the `locations = NULL` default at
      `R/ffm_manifest.R:134`, with its three cases.
- [x] T8: `NEWS.md` entry, `devtools::document()`, `devtools::check()`.
- [x] T9: Both `find_program()` warnings name `unset_program("<program>")`, the
      call the package exports, in place of the unexported `unset_ffmpeg()`;
      the repair suite asserts the real spellings, with each advice bullet
      instrumented so its own removal reddens (review [O]1, [O]2, [O]3).
- [x] T10: Doc and comment repairs — `?find_program`'s unreadable condition
      widened to what the guard fires on, `?program_status`'s unreadable case
      naming the pre-0.2.0 directory too, and `R/ffm_manifest.R:120-123`'s
      "warnings suppressed" comment corrected to T5's two-class handler
      (review [O]5, [O]6, [S-prior]1).
- [x] T11: AC4's wording assertions factored into one predicate the mutation
      probe runs over both the message the source path emits and the retired
      stand-in, so a reverted wording reddens the probe (review [O]4); then
      `devtools::document()`, `devtools::test()`, `devtools::check()` re-run at
      the repaired head.
- [x] T12: AC4's missing pin — a test that drives `ffm_batch(manifest = TRUE)`
      itself, with `ffm_run()` stood in for so no binary is needed, captures the
      `tidymedia_probe_timeout` warning the batch raises, and compares its
      message whole against the `program_status()`-path message; the tautologous
      stand-in at `test-tool-versions-report.R:74` retired or repaired
      (re-review [O]2, floor).
- [x] T13: `NEWS.md` repairs — the false claim that a single-blank-line config
      used to fail with an R error removed (it warned), and the Configuration
      headline narrowed to what shipped, which is what `readLines()` returns
      rather than every unreadable config path; the directory-at-the-config-path
      gap goes to a ROADMAP candidate row (re-review [O]1, [O]5).
- [x] T14: The memo census in prose — `R/cache.R:8-10` restates D044's two
      routes where D089 records four, and `R/cache.R:47-48` with
      `?refresh_ffmpeg_capabilities` state the `unset_program()` route without
      D089's removed-nothing carve-out (re-review [S-blame]1, [O]6).
- [x] T15: Comments and docs T2 falsified or overreached — `R/ffprobe.R:211-215`
      and `tests/testthat/test-audio-track-drop.R:150-153` still say a malformed
      config aborts; `?find_program`'s unreadable bullet promises a "missing"
      form the guard cannot see; and `R/ffm_manifest.R:141` names
      `{.arg locations}` in an abort blamed on a frame without that argument
      (re-review [S-blame]2, [O]3, [O]4).
- [x] T16: `devtools::document()`, `devtools::test()`, `devtools::check()` at
      the repaired head.

## Work log

- 2026-09-06: created by /milestone-plan.
- 2026-09-06: plan-gate criteria audit ran in full mode ([O], fresh context, user-facing tier), returning 15 findings and confirming all six premises; 11 with one clear repair were fixed in the criteria before writing (two instrument clauses moved to tasks, three probe families crossed, one census reworded, one substring grep replaced by a pinned message plus mutant, the length guard carved out from its own `NULL` default, the surface tier widened); 3 went to the gate as questions and 1 (D044's route census) to D089.
- 2026-09-06: plan gate chose a classed warning returning `NULL` for a malformed config file over a classed abort and over leaving each caller to guard, because a four-program report must still answer for the other three; falsified by a caller that cannot distinguish "not found" from "found nothing readable" and needs to.
- 2026-09-06: plan gate chose surfacing the stale-location warning from `program_status()` over a fourth column reporting the remembered location, because the column changes the shape every reader of that table sees for a state that is rare and repairable; falsified by a caller that must branch on staleness programmatically rather than be told about it.
- 2026-09-06: plan gate chose dropping the memo on any removal that took over dropping it on every exit, because a total failure removed nothing and the memo still describes the binary lookups keep answering with; falsified by a partial-removal form where "took" is not decidable from the filesystem.
- 2026-09-06: plan gate chose classing only the two warnings this milestone changes over classing `find_program()`'s plain not-found warning too, because that name is the naming pass's call; falsified by the two new classes proving unusable without the third.
- 2026-09-06: implement question gate named the three new condition classes, all three recommendations taken: `tidymedia_location_unreadable` (`tm_program`, `tm_file`) for a config file that is not one line, `tidymedia_location_gone` (`tm_program`, `tm_location`) for a remembered location whose binary is absent, and `tidymedia_locations_mismatch` for `tool_versions()`'s length refusal.
- 2026-09-06: T1 red first — all four `find_*()` exports against both malformed forms aborted rather than warned; measured errors were `argument is of length zero` (empty file) and `the condition has length > 1` (two lines).
- 2026-09-06: T2 — `find_program()` guards the read-back with the `length() != 1L` shape before the `Sys.which()` test, warning `tidymedia_location_unreadable` (`tm_program`, `tm_file`) and returning `NULL`; T1's 40 assertions green.
- 2026-09-06: T3 — stale-location warning classed `tidymedia_location_gone` (`tm_program`, `tm_location`) and widened with the `unset_program()` repair; the installer offer moved into `tm_install_bullet()`, which both find_program() branches now splice, and tests cross three operating systems against four programs on each branch.
- 2026-09-06: T4 — the memo drop moved above the `tidymedia_location_not_removed` abort and keyed on `length(left) < length(present)`, replacing the success-path-only call; mutant check with the guard forced to `FALSE` reddened both partial forms and the total-success case and left the removed-nothing case green.
- 2026-09-06: T5 — `program_status()` selects by the two classes to be raised rather than by the one to be muffled; `?program_status` narrowed to the never-configured case and `?find_program` documents both conditions with their fields.
- 2026-09-06: T6 — the timeout warning names programs from `names(probes)` under a separate local, and its second bullet says what `NA` means without naming a caller; two M69-era tests that pinned the display-label spelling updated, and a mutant carrying the retired manifest sentence fails the new assertions.
- 2026-09-06: T7 — `tool_versions()` aborts `tidymedia_locations_mismatch` (`tm_n_programs`, `tm_n_locations`) on a non-`NULL` `locations` of the wrong length, above the resolution of the `NULL` default.
- 2026-09-06: minor amendment — `?refresh_ffmpeg_capabilities` said there were two ways to discard the memo; with `unset_program()` it is three, so the list was corrected alongside T4's change.
- 2026-09-06: T8 — `devtools::document()` no diff; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes (17m52s); `devtools::test()` 0 failures, 13,155 passing, 18 skipped, the 10 warnings all the pre-existing dropped-track diagnostic in the audio-stream suites. NEWS entries added under Configuration; the first draft failed the spelling leg on "catchable", reworded rather than added to the wordlist.
- 2026-09-06: T7's blame test strengthened after review of the diff — it asserted only the class, so it was rewritten around a named wrapper and now asserts the condition's call names that wrapper.
- 2026-09-06: status → review; all eight tasks checked.
- 2026-09-06: review opened; draft PR #120 pushed, three fresh-context lenses spawned, `devtools::check()` running. Evidence gathering in progress — no criterion ticked yet.
- 2026-09-06: review returned the milestone to `in-progress`. AC2 FAILED: the gone-location warning advises `unset_ffmpeg()` (`R/program_management.R:122`, and `:106` on AC1's branch), a function the package does not export, so `unset_program()` is not offered as the repair; and the `set_program()` half is uninstrumented — deleting that bullet left all 178 assertions green, `"set_ffmpeg()"` being a substring of `"unset_ffmpeg()"`. AC3-AC6 verified and ticked; AC1 and AC7 met but left unticked because the repair moves the code they were measured against. Defect return 1 for this milestone.
- 2026-09-06: return question gate — both recommendations taken: the two warnings advise `{.code unset_program("{program}")}` rather than the bare `{.fn unset_program}`, because `unset_program()` takes no default (D079) and the caller needs the argument; and AC4's mutation probe is repaired by factoring its assertions into one predicate run over the real message and the stand-in, rather than deleting the probe or deriving the stand-in from the shipped text.
- 2026-09-06: minor amendment — three return-repair tasks T9-T11 added for the seven findings marked fix-on-return, and the Coverage lines updated together; [O]7 is left to maintainer triage at re-review as the Review section directs.
- 2026-09-06: T9 — both warnings now render `unset_program("ffmpeg")`; the repair suite asserts that spelling, asserts no `unset_<program>` wrapper is named, and each bullet was mutation-checked in turn: deleting the `set_program()` bullet and deleting the `unset_program()` bullet each reddened the suite, which the pre-return substring pairing did not.

- 2026-09-06: T10 work done, box left open pending the suite — `?find_program`'s unreadable bullet now names the blank and missing forms the guard also rejects, `?program_status`'s unreadable case says the condition is raised from whichever config file the lookup reached rather than naming only the `R_user_dir()` one, and `R/ffm_manifest.R`'s comment now says `program_status()` muffles the plain not-found warning and lets the two classed ones through; the NEWS clause describing the unreadable state widened to match.
- 2026-09-06: T11 work done, box left open pending the suite — AC4's wording assertions factored into `tm_timeout_wording_holds()`, which the probe runs over the message `tool_versions()` emits as well as over the retired stand-in; reverting the retired sentence into `R/ffm_manifest.R` reddened the probe's own `expect_true` leg (four failures, one of them at the probe), which the literal-against-itself shape could not.
- 2026-09-06: checkpoint — T9 verified and checked; T10 and T11 written and their targeted suites green, with `devtools::document()` producing no diff, but the full `devtools::test()` and `devtools::check()` at this head have not returned, so both boxes stay open and the status stays `in-progress`.

- 2026-09-06: T10 and T11 checked at the repaired head — `devtools::test()` 0 failures, 13,168 passing, 18 skipped, 10 warnings, 9 of them located by name to `warn_dropped_audio()` (`R/ffmpeg.R:2721`) in the audio-stream suites; `devtools::document()` no diff; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes (14m48s).
- 2026-09-06: T10 gained a test cell rather than shipping the widened wording as prose alone — a config file holding one blank line now asserts `tidymedia_location_unreadable` with `tm_program` and `tm_file`, so the third shape the help page and NEWS now name is instrumented; AC1's own crossing is unchanged.
- 2026-09-06: status → review; all eleven tasks checked. AC1, AC2 and AC7 left for review to re-measure at this head; [O]7 left where the Review section put it, maintainer triage at re-review.

- 2026-09-06: re-review returned the milestone to `in-progress`. AC4 FAILED on its instrument clause: the criterion asks that tests pin the emitted message on the `ffm_batch(manifest = TRUE)` path, and no test does — the test named for that caller calls `tool_versions()` twice with arguments the message does not read, so its whole-message comparison is a tautology. AC1, AC2, AC3, AC5 and AC6 re-verified and ticked at `b4cb7a7`; AC7 not ticked, and `NEWS.md` carries a false claim about the prior behaviour of a blank-line config ([O]1) and a headline broader than what shipped ([O]5). AC4's tick was written in this session and withdrawn: it was composed from the test's own comment rather than from what the test calls. Defect return 2 for this milestone; eleven other findings logged in the Review section for the return.

- 2026-09-06: return-2 question gate — both recommendations taken: AC4's `ffm_batch(manifest = TRUE)` pin drives the batch itself with `ffm_run()` mocked, rather than an end-to-end test gated on `skip_if_no_ffmpeg()`, so the pin runs on the binary-less CI legs too; and the locations-length abort drops `{.arg locations}` for "The locations supplied must name one location for each program.", keeping the caller frame the blame test pins, rather than dropping `call =` or routing the site to the existing argument-naming candidate row.
- 2026-09-06: minor amendment — five return-repair tasks T12-T16 added for the eight findings marked fix-on-return, and the Coverage lines updated together; [O]7 and [O]8/[S-blame]3 stay at maintainer triage where the re-review put them, and [O]9 stays rejected as pre-existing.
- 2026-09-06: T12 — the manifest-caller test now drives `ffm_batch(jobs, .f, manifest = TRUE)` with `ffm_run()` mocked, asserts the batch reached the manifest block (one job run, both manifest versions `NA`), and pins the `tidymedia_probe_timeout` message the batch call raised against the `program_status()`-path message and against `tm_timeout_wording_holds()`. Mutation-checked: renaming the timed-out programs back to their display labels in `R/ffm_manifest.R` reddened this test at its predicate leg (`:134`) as well as the `program_status()` test; source restored, `git diff` clean. The whole-message comparison alone cannot discriminate — both callers read one callee — which is why the predicate leg is what carries the pin.
- 2026-09-06: T13 — the Configuration headline narrowed to a config file that "reads back as something other than one program location", which is what AC1's guard is scoped to, and the blank-line form moved out of the R-error claim: it warned about a missing binary before this branch (`Sys.which("")` is `""`), which the NEWS now says. The directory-at-the-config-path gap went to a ROADMAP candidate row rather than widening AC1 (D-118); `ROADMAP.md` is 28,746 bytes at 58 lines, still over its 24,000-byte budget and still needing `/cairn-triage`.
- 2026-09-06: T14 — `R/cache.R`'s header comment now states D089's four routes in place of D044's two, and `?refresh_ffmpeg_capabilities`'s third bullet carries the carve-out: an `unset_program()` that removed nothing leaves the record alone, one that removed a file before failing discards it. The user-facing list stays three items because it enumerates calls, not routes.
- 2026-09-06: T15 — the two comments T2 falsified now say the abort was the pre-M116 behaviour and why each `tryCatch()` stays (`R/ffprobe.R`, `tests/testthat/test-audio-track-drop.R`); `?find_program`'s unreadable bullet drops the "or missing" form the guard's `is.na()` leg cannot see; and the locations-length abort reads "The locations supplied must name one location for each program." with a comment saying why it is not `{.arg locations}`. Nothing in `tests/`, `vignettes/` or `man/` pinned any of the changed strings; the three affected suites run 341 assertions, 0 failures.
- 2026-09-06: T16 — at the repaired head: `devtools::document()` no diff; `devtools::test()` 0 failures, 13,175 passing, 18 skipped, 10 warnings, all ten the pre-existing dropped-track diagnostic from `warn_dropped_audio()` (read off the log, not sampled: every warning entry is that message, in the two audio-stream suites and `test-ffmpeg.R:178`); `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes, 5m51s.
- 2026-09-06: status → review; all sixteen tasks checked. AC4 is what returned the milestone and T12 is its repair; AC1, AC2 and AC7 are left for re-review to re-measure at this head, since T13-T15 moved NEWS text and doc wording AC7 and AC1 were measured against. [O]7 and [O]8/[S-blame]3 are still at maintainer triage, and [O]9 still rejected.
- 2026-09-06: CI at `f3f0aea` — all ten legs pass (pkgdown, test-coverage, `ubuntu-latest` release/devel/oldrel-1/4.1.0, `macos-latest` release, `windows-latest` release) and both codecov gates pass.
- 2026-09-06: third review returned the milestone to `in-progress` on two triggers. The consistency gate FAILED: `cairn_validate` weight caps, the milestone body at 164 plan-owned lines against the `<150` cap, Tasks the heaviest section at 67 lines for 16 tasks. And AC4 FAILED again, on a different clause of the same instrument sentence: restoring only the manifest-naming sentence reddens three tests and leaves the `program_status()` test — the one the clause names — green at 0 failures, measured in a scratch copy and reached independently by the diff-bug lens. AC1, AC2, AC3, AC5 and AC6 re-verified and ticked at `278ad96`; AC7 not ticked, `NEWS.md`'s headline still claiming one shape more than shipped. Defect return 3, the thrash rule's threshold; ten findings logged in the Review section.
- 2026-09-06: correction — T12's work-log line said the batch-path message is pinned "against the `program_status()`-path message". It is not: the comparison baseline is a bare `tool_versions()` call, the shared callee. The test still discriminates through its wording-predicate leg, which is what carries the pin; the description was wrong.
- 2026-09-06: return disposition, chosen by the maintainer at the return gate with a fourth repair round, parking and a re-plan all offered: narrow the promise. AC4's instrument clause is amended through the gated protocol to name what the suite does rather than wording no test satisfies, the shipped behaviour being unchanged and already verified; the over-cap Tasks section is compressed in the same pass; [O]3, [O]4 and [O]5 are fixed on return; [O]6, [O]7, [O]9 and [O]10 stay at maintainer triage; [O]8 is rejected as already dispositioned.

## Decisions

## Review

Reviewed 2026-09-06 at branch head `9de2766`, PR #120. **Outcome: returned to
`in-progress` under the return floor** — finding [O]1 demonstrates AC2 failing.

### Evidence per criterion

- **AC1 — not ticked.** The behaviour is met: all four `find_*()` exports
  against both malformed forms warn `tidymedia_location_unreadable` carrying
  `tm_program` and `tm_file` and return `NULL`, and `program_status()` over a
  directory holding one malformed file returns four rows with that program's
  `location` and `version` `NA`, the other three identical to the same call
  without the file, raising the warning once
  (`tests/testthat/test-program-location-repair.R`, 178 assertions green). Left
  unticked because the AC2 repair moves this branch's message and the test at
  `:64` that pins it; re-review re-measures at the new head.
- **AC2 — FAILS.** The gone-location warning does not add `unset_program()` as
  the repair. It interpolates `{.fn unset_{program}}`
  (`R/program_management.R:122`), which renders as `unset_ffmpeg()` — a
  function the package does not export (`NAMESPACE` has `unset_program` only;
  the per-program wrappers are a declined candidate row). Rendered and
  measured 2026-09-06. The same wrong spelling is at `:106` on AC1's
  unreadable branch. The `set_program()` half of the criterion is also
  uninstrumented: deleting the `set_program()` advice bullet from the
  gone-location warning left all 178 assertions in the repair suite green,
  because `"set_ffmpeg()"` is a substring of the `unset_ffmpeg()` bullet the
  test also matches. The OS × program crossing and the `windows` + `mediainfo`
  absent-installer cell do pass.
- [x] **AC3.** A four-program call with one stale location, one never-configured
  program and two resolving raises exactly one warning, `tidymedia_location_gone`
  with `tm_program` `"ffmpeg"`; a call with nothing configured raises none
  (`test-program-location-repair.R:258`, `:310`). `?program_status` narrows the
  `NA`-without-warning promise to the never-configured case and `?find_program`
  documents both conditions with their fields.
- [x] **AC4.** The timeout warning names `ffmpeg`/`ffprobe`/`ffplay`/`mediainfo`
  and none of the four display labels on the `program_status()` path, and the
  message from the `ffm_batch(manifest = TRUE)` path compares identical whole
  (`test-tool-versions-report.R:23`, `:50`, `:75`). The mutant clause was
  verified by a real source mutation, not only the modelled one: restoring the
  display-label naming and the manifest sentence in `R/ffm_manifest.R` turned
  the `program_status()`-path test red at `:41` and `:45`. Source restored.
- [x] **AC5.** Both partial forms abort `tidymedia_location_not_removed` with an
  empty memo, and the removed-nothing case aborts with the memo intact
  (`test-program-location-repair.R:184`, `:218`, `:240`).
- [x] **AC6.** The recycling length-1, the length-3-against-4 and the empty-`list()`
  cases all abort `tidymedia_locations_mismatch` carrying `tm_n_programs` and
  `tm_n_locations`; the `NULL` default still answers, and
  `ffm_batch(manifest = TRUE)` still records both versions
  (`test-tool-versions-report.R:134`, `:163`, `:179`, `:197`).
- **AC7 — not ticked.** Measured clean at this head: `devtools::check()` Status
  OK, 0 errors / 0 warnings / 0 notes, 16m57s; `devtools::test()` 0 failures,
  13,156 passing, 18 skipped, 10 warnings (the pre-existing dropped-track
  diagnostic); `devtools::document()` no diff; `NEWS.md` carries the five
  user-visible changes (`tool_versions()` is unexported, so AC6 is not one).
  Left unticked because the AC2 repair changes shipped message text these runs
  covered.

### Consistency gate

`cairn_validate.py` — 16 PASS, 7 advisories OK, no `release window` advisory.
No `DESIGN.md` principle changed, so `cairn_impact.py` was not run. Toolchain
slot: `document()` no diff; `NAMESPACE`/`man/` regenerate clean; `README.Rmd`
untouched by the branch; `pkgdown::check_pkgdown()` no problems; `NEWS.md`
entry present; no new top-level files; `check()` clean. CI on PR #120: all ten
legs pass, both codecov gates pass.

### Independent review — three fresh-context lenses

User-facing tier with executable surface, so the full fan-out ran. Ten
findings, one of them floor-qualifying.

- **[O]1 — both new warnings advise a function that does not exist.**
  `R/program_management.R:106`, `:122`. **Floor return** — demonstrates AC2
  failing. Fix on return.
- **[O]2 — the tests pin that spelling**, so the suite resists the fix:
  `test-program-location-repair.R:64` and `:129`, 13 assertions across four
  programs and three operating systems. Fix on return, with [O]1.
- **[O]3 — the `set_program()`-advice assertions cannot fail**, `"set_ffmpeg()"`
  being a substring of `"unset_ffmpeg()"` (`:65`, `:128`). Confirmed by
  mutation. Fix on return, with [O]1.
- **[O]4 — the AC4 mutation probe asserts a string literal it wrote itself**
  (`test-tool-versions-report.R:130-146`), so no change to `R/ffm_manifest.R`
  can redden it — the defect class `DESIGN.md:125` already records. AC4 holds
  in substance on the real mutation run above. Fix on return.
- **[S-prior]1 — a comment this diff's own change made false.**
  `R/ffm_manifest.R:120-123` still says `program_status()` resolves each program
  "with `find_program()`'s warnings suppressed"; T5 replaced that blanket
  suppression with a handler that lets two classes through. Confirmed. Fix on
  return.
- **[O]5 — `?program_status` names only the `R_user_dir()` config directory**
  for the unreadable case, but the pre-0.2.0 `rappdirs` directory produces it
  too, as the diff's own test at `test-program-location-repair.R:70` proves.
  Confirmed. Fix on return.
- **[O]6 — `?find_program` describes the unreadable condition more narrowly than
  the guard**: the docs say empty or more than one line, the guard also fires on
  `is.na()` and `!nzchar()`. Fix on return.
- **[O]7 — the memo drop fires for `mediainfo`**, whose location cannot affect
  the encoder pool; the effect is one wasted re-probe, but the comment's stated
  reason and D089's census inherit it. Pre-existing on the success path since
  M113, widened here. Maintainer triage at re-review.
- **[S-blame] — no findings.** Traced D088, D089, D044 and M113's intent; the
  narrowing of M113's blanket suppression and the memo reordering are both
  covered by decisions written for this milestone.
- **[S-prior] — otherwise clean.** Each of T2–T7 traced to the M113 or M115
  finding it closes; no prior lesson walked back.

---

## Re-review 2026-09-06 (head `b4cb7a7`, PR #120)

Second review of this milestone, after defect return 1. Master had not moved
(branch 0 behind, 7 ahead), so no merge preceded the evidence. All ten CI legs
pass at this SHA and both codecov gates pass. Every criterion below was
re-measured at this head, including the four the first review ticked, because
the repair moved the code and docs they were measured against.

### Evidence per criterion

- [x] **AC1.** Each of the four `find_*()` exports against each malformed form
  warns `tidymedia_location_unreadable` carrying `tm_program` and `tm_file` and
  returns `NULL`; rendered through `find_program()` under a redirected
  `R_USER_CONFIG_DIR` and an emptied `PATH`, the empty, two-line and blank
  forms all produce the class with `tm_file` naming the config file. On the
  second half: `program_status()` with nothing configured returns four rows,
  all `NA`, and raises zero warnings; with one malformed file (`ffplay`) it
  returns four rows, raises exactly one `tidymedia_location_unreadable` with
  `tm_program` `"ffplay"`, the `ffplay` row is `NA` in `location` and `version`
  (`TRUE`), and the other three rows compare `identical()` to the same call
  without the file (`TRUE`).
- [x] **AC2.** The gone-location warning carries `tidymedia_location_gone` with
  `tm_program` and `tm_location` on all four programs, and now advises calls
  the package actually exports — the defect that returned this milestone.
  Rendered per program: `set_ffmpeg()` / `set_ffprobe()` / `set_ffplay()` /
  `set_mediainfo()` beside `unset_program("<program>")`, checked against
  `getNamespaceExports("tidymedia")`, which lists `unset_program` as the only
  `unset_*` export and all four `set_*` wrappers. The installer offer was
  crossed over three operating systems × four programs, all twelve cells
  measured: `install_on_win()` present on exactly `windows` × the three
  programs `tm_install_registers` lists, absent on `windows` + `mediainfo` and
  on every `darwin` and `linux` cell. Both advice bullets are now
  independently instrumented — deleting either one reddens the repair suite,
  where before `"set_ffmpeg()"` was a substring of the `unset_ffmpeg()` bullet
  and the `set_program()` assertion could not fail.
- [x] **AC3.** A four-program call with one stale location (`ffmpeg`), one
  never-configured program (`mediainfo`) and two resolving raises exactly one
  warning, `tidymedia_location_gone` with `tm_program` `"ffmpeg"`; the
  never-configured program contributes none, and the nothing-configured
  baseline raises zero. `man/program_status.Rd:20` narrows the promise to "A
  program that was never configured and is not installed gets `NA` in both
  columns rather than a warning", and `man/find_program.Rd` documents both
  conditions with their fields.
- **AC4 — FAILS** (corrected: this line first ticked AC4, in error, and the
  tick is withdrawn). The criterion asks that tests "pin the emitted message on
  the `program_status()` path and on the `ffm_batch(manifest = TRUE)` path".
  The first path holds. The second does not: no test in the suite pins the
  timeout message through an `ffm_batch(manifest = TRUE)` call. The test named
  for it (`test-tool-versions-report.R:74`, "reads the same from the manifest
  caller") calls `tool_versions()` twice — the shared callee, not the caller
  path — and its two calls differ only in a `locations` argument the message
  does not read, so `expect_identical` on the two messages is a tautology that
  cannot fail. `grep` over `tests/testthat/` for `tidymedia_probe_timeout`
  returns four sites, all on `tool_versions()` or `count_audio_streams_all()`;
  the file's one real `ffm_batch(manifest = TRUE)` test (`:233`) asserts
  versions, never the warning. The rest of AC4 holds: the spelling and the
  caller-neutral sentence are met, and the mutant clause was verified by a real
  source mutation at this head — restoring the retired manifest-naming sentence
  into `R/ffm_manifest.R` turned four assertions red, including
  `test-tool-versions-report.R:144`, the probe's own leg; source restored,
  `git diff` clean. The first tick was written from the test's own comment
  rather than from what the test calls.
- [x] **AC5.** All four removal shapes measured through the `tm_unlink()` seam
  with the memo seeded to one entry: legacy-left and new-left both abort
  `tidymedia_location_not_removed` with the memo at 0 entries; nothing-removed
  aborts the same class with the memo intact at 1; both-removed returns
  invisibly with the memo at 0.
- [x] **AC6.** The recycling length-1, the length-3-against-4 and the
  empty-`list()` cases each abort `tidymedia_locations_mismatch` carrying
  `tm_n_programs` 4 and `tm_n_locations` 1 / 3 / 0; the `NULL` default still
  returns a list. The two repair suites run 232 assertions, 0 failures.

- **AC7 — not ticked.** The mechanical half is clean at this head:
  `devtools::test()` 0 failures, 13,172 passing, 18 skipped, 10 warnings (9
  located by name to `warn_dropped_audio()`, `R/ffmpeg.R:2721`, the
  pre-existing dropped-track diagnostic); `devtools::document()` no diff;
  `pkgdown::check_pkgdown()` no problems; and all ten CI legs, which run
  `R CMD check` across five platforms, pass at `b4cb7a7`. The criterion's
  `NEWS.md` clause is what fails it: [O]1 shows a false claim about the prior
  behaviour of a blank-line config, and [O]5 a headline broader than what
  shipped. The local `devtools::check()` was not re-run by this review — the
  CI legs cover it, and the return moots it.

### Independent review — three fresh-context lenses

User-facing tier with executable surface, so the full fan-out ran. Twelve
findings across two lenses; one is floor-qualifying.

- **[O]2 — AC4's `ffm_batch(manifest = TRUE)` pin does not exist, and the test
  standing in for it cannot fail.** **Floor return** — demonstrates AC4
  failing, evidenced above. Fix on return.
- **[O]1 — `NEWS.md:117-119` states a false fact about the prior behaviour.**
  The clause added by `eff1d3c` says a file "holding a single blank line made
  `find_ffmpeg()` … fail with an R error". It did not: `Sys.which("")` returns
  `""` (measured), so before this branch a one-blank-line file took the
  stale-location branch and warned. Confirmed. Fix on return.
- **[O]5 — a config path that is a directory is still fatal**, so `NEWS.md:116`'s
  headline ("a remembered program location that cannot be read no longer stops
  the call") is broader than what shipped. Measured at this head: with a
  directory at the config path, `find_ffmpeg()` and `program_status()` both die
  with `cannot open the connection`. Outside AC1's domain, which is scoped to
  what `readLines()` returns, so not a criterion failure. Fix the headline on
  return; the behaviour gap goes to a candidate row rather than widening AC1
  (D-118).
- **[S-blame]1 — `R/cache.R:8-10` restates the two-route memo census that
  D089, written by this milestone, supersedes with four.** The diff fixed the
  user-facing roxygen thirty lines below and left the internal comment.
  Confirmed by read. Fix on return.
- **[S-blame]2 — this milestone's own change falsified two comments on
  unmodified files.** `R/ffprobe.R:211-215` and
  `tests/testthat/test-audio-track-drop.R:150-153` both state as fact that a
  malformed config "makes it abort rather than warn"; T2 is what stopped that
  being true. Confirmed by read. Fix on return.
- **[O]6 — `R/cache.R:47-48` and `man/refresh_ffmpeg_capabilities.Rd` state the
  `unset_program()` discard route without D089's carve-out** — an
  `unset_program()` that removed nothing deliberately does not discard.
  Confirmed by read. Fix on return.
- **[O]3 — `R/program_management.R:110`'s `is.na(location)` leg cannot fire**
  (`readLines()` does not return `NA`), and T10's doc widening promises it:
  `man/find_program.Rd` says "blank **or missing**". The guard's defensive
  shape is fine; the doc claim should shrink to what it can see. Fix on return.
- **[O]4 — `R/ffm_manifest.R:141` names `{.arg locations}` in an abort blamed
  on a frame that has no such argument** when `program_status()` supplies the
  `call`. Reachable only through an internal bug. Fix on return.
- **[O]7 — neither new warning threads `call`**, while the sibling
  `tidymedia_no_remembered_location` in the same file does. Consistency only;
  both are raised from an unexported body reached through four exported doors.
  Maintainer triage at re-review.
- **[O]9 — the `options(tidymedia.timeout = )` assertions pass only because
  testthat sets `cli.condition_width = Inf`**; at 80 columns the message wraps
  mid-token. Pre-existing wording, not introduced here. Rejected as
  pre-existing.
- **[O]8 / [S-blame]3 — the memo drop still fires for `mediainfo`**, restating
  the first review's [O]7. Unchanged by the repair, as the first review's
  routing intended. Maintainer triage at re-review.
- **[S-prior] — no findings.** It traced the archived `## Review` sections
  touching these files and confirmed each of the first return's [O]1-[O]6 and
  [S-prior]1 closed; the probe found no inline PR review comments at all, so
  the thread walk was correctly skipped.

### Consistency gate

`cairn_validate.py` — 16 PASS, exit 0, one advisory (M116 at 11 tasks against
the 10-task split tripwire, the appended repair tasks). No `DESIGN.md`
principle changed, so `cairn_impact.py` was not run. Toolchain slot:
`document()` no diff; `pkgdown::check_pkgdown()` no problems; `README.Rmd`
untouched by the branch; `NEWS.md` entry present but defective per [O]1 and
[O]5; no new top-level files. CI on PR #120 at `b4cb7a7`: all ten legs pass,
both codecov gates pass.

**Outcome: returned to `in-progress` under the return floor.** Defect return 2
for this milestone.

---

## Third review 2026-09-06 (head `278ad96`, PR #120)

Third review, after defect return 2. Master had not moved (branch 0 behind, 16
ahead), so no merge preceded the evidence. Every criterion was re-measured at
this head, the five the second review ticked included, because the return
repair moved shipped message text, help-page wording and `NEWS.md` prose those
measurements rested on.

### Evidence per criterion

- [x] **AC1.** Measured by direct rendering through the four `find_*()` exports
  under a redirected `R_USER_CONFIG_DIR` and an emptied `PATH`: all twelve
  cells — four programs against the empty, two-line and blank forms — warn
  `tidymedia_location_unreadable` with `tm_program` naming the program,
  `tm_file` `identical()` to the config file the lookup read, and a `NULL`
  return. On the second half: `program_status()` with nothing configured
  returns four rows and raises zero warnings; with one malformed file
  (`ffplay`) it returns four rows, raises exactly one warning, that warning is
  `tidymedia_location_unreadable` with `tm_program` `"ffplay"`, the `ffplay`
  row is `NA` in `location` and `version`, and the other three rows are
  `identical()` to the same call without the file.
- [x] **AC2.** All twelve `os` × `program` cells rendered at this head. Every
  one carries `tidymedia_location_gone` with `tm_program` naming the program
  and `tm_location` `identical()` to the remembered path; every one renders
  `set_<program>()` and `unset_program("<program>")`; none renders an
  `unset_<program>` wrapper. `getNamespaceExports("tidymedia")` lists
  `unset_program` as the only `unset_*` export and all four `set_*` wrappers,
  so both advised calls exist. The `install_on_win()` offer appears on exactly
  the three `windows` × `tm_install_registers` cells and is absent on
  `windows` + `mediainfo` and on all eight `darwin`/`linux` cells. The crossing
  is in the suite (`test-program-location-repair.R:145`, `:178`), and both
  advice bullets are independently instrumented: deleting the `set_<program>()`
  bullet from the gone-location warning and deleting the `unset_program()`
  bullet each turn 12 of the repair suite's 195 assertions red, run in a
  scratch copy of the tree so the primary checkout stayed clean.
- [x] **AC3.** A four-program call with one stale location (`ffmpeg`), two
  resolving (`ffprobe`, `ffplay`) and one never configured (`mediainfo`)
  raises exactly one warning at this head: `tidymedia_location_gone` with
  `tm_program` `"ffmpeg"`. The never-configured program contributes none and
  its row is `NA` in both columns; a call with nothing configured raises zero.
  `man/program_status.Rd:20` narrows the promise to "A program that was never
  configured and is not installed gets `NA` in both columns rather than a
  warning", and `man/find_program.Rd:42-48` documents both conditions with
  their fields.
- **AC4 — three clauses met, the fourth turns on a reading of its own wording;
  not ticked.** The spelling clause holds: the rendered message names `ffmpeg`
  and `ffprobe` and no display label, and its `NA` sentence reads "Those
  versions are recorded as NA; raise or remove `options(tidymedia.timeout = )`"
  with no caller named, identically from both callers. Both pins now exist and
  drive the callers they name: `test-tool-versions-report.R:56` calls
  `program_status()`, and `:106` calls
  `ffm_batch(jobs, manifest = TRUE, .f = ...)` with `ffm_run()` mocked, then
  compares the message the batch raised whole against the other path's — the
  repair for return 2's floor finding. The fourth clause asks that "a mutant
  restoring the manifest-naming sentence turns the `program_status()` test
  red", and what it does depends on what that names. Measured in a scratch copy
  at this head: restoring **only** the sentence naming the manifest reddens
  three tests (the manifest-caller pin, the caller-neutral-sentence test and
  the wording probe) and leaves the `program_status()`-path test green at 0
  failures — that test asserts program spellings, which the sentence does not
  touch. Restoring the **whole** retired message — the manifest sentence plus
  the display-label naming, which is how the suite's own retired-mutant literal
  spells it (`:168-173`) — reddens the `program_status()`-path test with 8
  failures. The narrow reading fails the clause; the whole-message reading
  meets it. Not ticked: resolving that ambiguity in the work's favour is the
  reinterpretation review is forbidden, so the disposition goes to the
  maintainer at the gate.
- [x] **AC5.** All four removal shapes measured at this head through the
  `tm_unlink()` seam, with the real memo environment seeded to one entry (a
  call counter cannot say the memo is empty, which is what the criterion
  asks). New-file-left and legacy-file-left both abort
  `tidymedia_location_not_removed` with the memo at 0 entries; nothing-removed
  aborts the same class with the memo intact at 1; both-removed returns with
  no condition and the memo at 0.
- [x] **AC6.** The recycling length-1, the length-3-against-4 and the
  empty-`list()` cases each abort `tidymedia_locations_mismatch` carrying
  `tm_n_programs` 4 and `tm_n_locations` 1 / 3 / 0. The `NULL` default still
  answers (`ffmpeg` and `ffprobe` both `8.1.2` through a mocked probe), and the
  end-to-end `ffm_batch(manifest = TRUE)` test runs and passes at this head
  rather than skipping. The abort now reads "The locations supplied must name
  one location for each program." — the wording change return 2 directed, which
  leaves the class and both fields unchanged.
- **AC7 — not ticked.** The mechanical half is clean at this head:
  `devtools::test()` 0 failures, 13,175 passing, 18 skipped, 10 warnings (all
  ten the pre-existing `warn_dropped_audio()` dropped-track diagnostic, read
  off the log rather than sampled); `devtools::document()` no diff;
  `pkgdown::check_pkgdown()` no problems; and all ten CI legs, which run
  `R CMD check` across five platforms, pass at `f3f0aea` with both codecov
  gates. `devtools::check()` was clean locally at the same code tree (Status
  OK, 0/0/0, 5m51s), and its re-run at this head, which landed after the
  return was recorded, is also clean: Status OK, 0 errors / 0 warnings /
  0 notes, 6m48s. The criterion's `NEWS.md` clause is
  again what fails it: [O]5 below shows the Configuration headline still
  claiming one shape more than shipped.

### Consistency gate

**FAILED.** `cairn_validate.py` — 16 PASS, exit 1, one FAIL: **weight caps**,
`cairn/milestones/M116-program-status-config-repair.md` at 164 plan-owned lines
against the `<150` cap, `shed >=15`; the breakdown names Tasks (67 lines, 16
tasks) as the heaviest section. One advisory, the split tripwire at 16 tasks
against 10. The remedy is the tracking rules' own: compress the single heaviest
plan-owned section in one pass, which is implement-owned work, not a
review-side patch. No `DESIGN.md` principle changed, so `cairn_impact.py` was
not run. Toolchain slot otherwise clean: `document()` no diff;
`pkgdown::check_pkgdown()` no problems; `README.Rmd` untouched by the branch;
`NEWS.md` entry present but defective per [O]5; no new top-level files.

### Independent review — three fresh-context lenses

User-facing tier with executable surface, so the full fan-out ran. Ten findings
from the diff-bug lens; the other two lenses returned none.

- **[O]1 — AC4's mutant clause is not met: the manifest-naming mutant leaves
  the `program_status()` test green.** `test-tool-versions-report.R:47`,
  `R/ffm_manifest.R`. **Floor return** — demonstrates AC4 failing, and reached
  independently by the review's own measurement above. The lens's proposed
  one-line repair is to add the wording predicate to that test. Fix on return.
- **[O]2 — AC4's pin clause is half met on the `program_status()` path**, for
  the same reason: that test pins program spellings, not the emitted message.
  Same repair as [O]1. Fix on return, with [O]1.
- **[O]3 — the manifest test's comparison baseline is not the
  `program_status()` path, though its variable name, its comment and this
  milestone's own T12 work-log line all say it is.**
  `test-tool-versions-report.R:111` is a bare `tool_versions()` call, the
  shared callee. The predicate leg is what carries that test's pin, so the
  test still discriminates; what is wrong is the description. Confirmed by
  read. This is the same describe-vs-call mismatch that produced returns 1 and
  2, and a later reviewer composing evidence from the comment would tick AC4
  in error again. Fix on return.
- **[O]4 — `NEWS.md:117` still claims one shape more than shipped.** A config
  file holding a single whitespace-only line (`"   "`) passes the guard
  (`nzchar("   ")` is `TRUE`) and takes the gone-location branch: measured at
  this head, `find_ffmpeg()` warns `tidymedia_location_gone` saying the binary
  "no longer seems to exist", which is the answering-about-the-wrong-thing the
  headline says no longer happens. `?find_program` is correct — it enumerates
  the empty, multi-line and single-blank-line forms only. Same shape as return
  2's [O]5, which was treated as failing AC7's `NEWS.md` clause. Fix on return.
- **[O]5 — `D-118` is cited in repo records but is not a repo decision.**
  `cairn/ROADMAP.md:18` and two milestone lines cite it; `grep` over
  `cairn/DECISIONS.md` returns zero hits, the repo's entries being `D044`-style.
  It is a cairn *plugin* decision id, cited where a repo reader will look for a
  repo entry. Tracking only. Fix on return.
- **[O]6 — AC6's end-to-end clause is gated on a binary.**
  `test-tool-versions-report.R:249` sits behind `skip_if_no_ffmpeg()`, so that
  half of AC6 is not exercised on the binary-less CI legs; the mocked
  `ffm_batch(manifest = TRUE)` test at `:74` does run everywhere and mitigates
  it. Maintainer triage at re-review.
- **[O]7 — the new `locations` length abort sits above `resolve_timeout()`**
  (`R/ffm_manifest.R:140` against `:180`), so a caller with both an unusable
  timeout option and a mismatched `locations` gets the locations refusal. No
  caller can reach both today. Maintainer triage at re-review.
- **[O]8 — `R/program_management.R:110`'s `is.na(location)` leg cannot fire**,
  restating return 2's [O]3, which was closed on the doc side and left as an
  accepted defensive shape. Re-reported because the lens was told to filter
  nothing. Rejected as already dispositioned.
- **[O]9 — neither new warning threads `call`**, restating return 2's [O]7.
  Unchanged. Maintainer triage at re-review.
- **[O]10 — the memo drop still fires for `mediainfo`**, restating return 2's
  [O]8/[S-blame]3. Unchanged. Maintainer triage at re-review.
- **[S-blame] — no findings.** It traced the modified lines to M44, M67, M113
  and D044/D089 and found nothing this diff silently undoes; it confirmed the
  `{.arg locations}` removal is a reasoned exception to the package's
  convention rather than drift, and that mocking `ffm_run()` follows the
  M69/M70-era pattern in `test-timeout-silence.R` rather than defeating it.
- **[S-prior] — no findings.** It verified every fix-on-return item from both
  earlier passes closed in this diff and every deferred or rejected item left
  as it was. Its GitHub probe returned an empty array, so the per-PR thread
  walk was correctly skipped.

**Outcome: returned to `in-progress`.** Two independent triggers — the
consistency gate's weight-caps failure, and [O]1 under the return floor.
Defect return 3 for this milestone, which is the thrash rule's threshold.

