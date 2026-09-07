<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M116: A broken or stale remembered location is reported, not fatal or silent

- **Status:** review
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

- [ ] AC1: A remembered-location file `readLines()` does not return exactly one
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
- [ ] AC2: `find_program()`'s warning for a remembered location whose binary is
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
- [x] AC4: `tool_versions()`'s `tidymedia_probe_timeout` warning names each
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
- AC4 → T6, T11
- AC5 → T4
- AC6 → T7
- AC7 → T8, T11

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
