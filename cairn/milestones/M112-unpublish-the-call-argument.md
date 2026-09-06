# M112: No export publishes the internal `call` argument, and the duplicate exports are settled

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — an exported signature and the Rd usage line a reader copies from
- **Branch/PR:** `m112-unpublish-the-call-argument` / https://github.com/jmgirard/tidymedia/pull/116

## Goal

Move the threaded `call` argument off `set_program()`'s and
`hardware_encoder()`'s published signatures into an internal implementation,
keeping the blame behaviour M100 and M110 bought, and settle the two pairs of
exported names that resolve to one function.

## Scope

**In:** an internal `tm_set_program(program, location, confirm, call)` with
`set_program()` and the four `set_*()` exports as wrappers over it
(`R/program_management.R:220`); the same shape for `hardware_encoder()`
(`R/ffmpeg.R:3115`); a `cairn/DECISIONS.md` entry superseding M110's local
decision that `call` stays a threaded formal; the disposition of
`ffm`/`ffm_files` (`R/ffm.R:64`) and `mediainfo_summary`/`mediainfo_template`
(`R/mediainfo.R:243`).

**Out:** any change to which frame a refusal names, with one measured
exception: `has_hardware_encoder()`'s wrong-`codec` and wrong-`hardware`
refusals were raised in `hardware_encoder()`'s name, a function the caller
never typed, because the shared body's checkers ran under
`rlang::caller_env()`. Threading `call` into them moves both onto
`has_hardware_encoder()` itself, which is what AC2 asks for; ten cells of
M096's corrupt-limit census change side, and that census is a merge-base
measurement that is read, never rewritten. Every other blame assertion still
holds unchanged. The wider `call`-threading question at other seams stays
where M110 left it. The `tidymedia_program_not_found` naming question → the
unclassed-aborts candidate row.

## Acceptance criteria

- [x] AC1: No object in `getNamespaceExports("tidymedia")` that is a function
      has a formal named `call`. Evidence: the sweep's output naming every
      export it examined and the count, run before and after the change so the
      before-run shows the two hits.
- [x] AC2: Across the seven exports the change touches — `set_program`,
      `set_ffmpeg`, `set_ffprobe`, `set_ffplay`, `set_mediainfo`,
      `hardware_encoder`, `has_hardware_encoder` — an ARGUMENT refusal (a
      wrong-typed `location`, `confirm`, `codec` or `hardware`) and a BODY
      refusal (no such executable; a pair no backend's table holds) each name
      the export the caller typed, at the console and from a wrapper.
      Evidence: a table with one row per export x refusal form, carrying the
      frame each refusal named.
- [x] AC3: For each of `ffm`/`ffm_files` and
      `mediainfo_summary`/`mediainfo_template`, either one name is gone from
      `NAMESPACE`, `_pkgdown.yml` and `man/`, or this file's Decisions section
      records why both stay, citing D014's pre-0.2.0 window. Evidence: the
      NAMESPACE diff or the decision entry.
- [x] AC4: A `cairn/DECISIONS.md` entry supersedes M110's `call`-stays-a-formal
      decision by name, states the internal-implementation mechanism, and says
      what would falsify it. Evidence: the entry.
- [x] AC5: `devtools::document()` produces no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings); a `NEWS.md` entry for
      the signature change.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T4
- AC3 → T5
- AC4 → T6
- AC5 → T7

## Tasks

- [x] T1: Write the AC1 sweep first and see it red — two hits — before any
      source change.
- [x] T2: Extract `tm_set_program(program, location, confirm, call)` from
      `R/program_management.R:220-280`; `set_program()` and the four `set_*()`
      exports become wrappers passing their own frames. The M100 lesson holds:
      pass the threaded `call` into `rlang::arg_match()`, `check_string()` and
      `check_bool()`, not only into the abort sites.
- [x] T3: The same extraction for `hardware_encoder()` (`R/ffmpeg.R:3115`),
      keeping its literal `codec` defaults spelled out in the Rd usage line for
      the RR07 Q2 reason recorded there.
- [x] T4: Build the AC2 table as a test, both refusal forms at all seven
      exports. The existing pinning tests are kept but do not stand alone —
      the M100/M110 lesson is that they stay green under the wrong fix.
- [x] T5: Decide the two duplicate-export pairs. `ffm()` is the spelling every
      vignette, README and `@examples` block uses, so removing it is the larger
      change; `mediainfo_summary()` and `mediainfo_template()` have identical
      signatures. Record the call either way.
- [x] T6: Append the superseding D-entry.
- [x] T7: `document()`, `test()`, `check()`, `NEWS.md`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned three findings against this milestone's draft: the `call` sweep quantified over exports but enumerated `man/*.Rd`, which records no export status; the blame criterion varied location only, not refusal form, against the M100/M110 lesson; and the draft did not cite M110's standing decision at all. The first two were fixed before writing; the third went to the question gate as its own question.
- 2026-09-05: plan gate chose extracting an internal implementation over leaving `call` published, because M110's recorded objection was to re-calling the checkers at four wrappers under D074's siting rule, which an internal implementation does not do — no body is duplicated. Falsified by a refusal whose frame changes under the extraction, which AC2's table is built to catch.
- 2026-09-05: implementation gate chose dropping both duplicate exports — `ffm()` and `mediainfo_summary()` — over keeping either, on the measurement that `ffm_files(` has 234 call sites to a bare `ffm(`'s 9, which reverses T5's premise that removing `ffm()` is the larger change. T5's claim holds only inside `@examples` (26 `ffm(` to 21 `ffm_files(`).
- 2026-09-05: implementation gate waived the deprecation cycle for both alias removals and the `call` removal, under D014's pre-0.2.0 clean-break policy; no `lifecycle` shim.
- 2026-09-05: T1 sweep written and seen red at `tests/testthat/test-exported-call-formal.R:32` naming exactly `hardware_encoder` and `set_program` over a domain of 88 exported functions. Its positive control (a real formal named `call`, from `hardware_encoder_available()`) and its independent domain assertion both pass, so a later green is the two exports losing the formal rather than the detector going blind.
- 2026-09-05: T2 extracted `tm_set_program(program, location, confirm, call)`; `set_program()` and the four `set_*()` exports pass their own frames, and the Rd usage line lost `call = rlang::current_env()`. `install_on_win()`'s internal registration still goes through the exported `set_program()`, so its refusals keep naming that frame. `arg_match()` takes `error_arg = "program"` explicitly, since the internal is reached with a string literal from four of the five callers. The blame suites (`builder-blame-front-door`, `hardware-out-of-table-blame`, `nvenc-probe-blame`, `timeout-refusal-blame`, `program-management`, `nvenc-memo`) all pass; the sweep is down to one hit, `hardware_encoder`.
- 2026-09-05: amendment (substantive, Scope Out) accepted at a mini gate: one refusal changes frame. Threading `call` into `tm_hardware_encoder()`'s checkers, which T2/T3 mandate, moves `has_hardware_encoder()`'s wrong-`codec` and wrong-`hardware` refusals off `hardware_encoder()` — a function the caller never typed — and onto `has_hardware_encoder()`. Ten cells of M096's corrupt-limit census change side (kept 1093→1103, dropped 442→432); the merge-base census is read, not rewritten. No acceptance criterion changed, so no criteria re-audit is owed.
- 2026-09-05: T3 extracted `tm_hardware_encoder(codec, hardware, call)`; `hardware_encoder()` is now a wrapper passing its own frame, and `resolve_hw_encoder()`, `check_hardware_available()` and `hardware_encoder_available()` all reach the table through the internal. The exported wrapper keeps its literal `codec` defaults, so the Rd usage line still spells the four families out (RR07 Q2); `arg_match()` inside the internal takes the tables as explicit `values`, and the existing `test-hardware-backends.R` sweep is what pins the literals to them. The AC1 sweep is now green: 0 hits over 88 exported functions.
- 2026-09-05: T4 added `tests/testthat/test-blame-frame-table.R`: 14 rows (7 exports x argument/body) x 2 call sites, all 28 cells naming the export the caller typed. A second test pins each row to its condition class or message, so a body row that aborted in a checker instead cannot pass on the frame alone. The discriminating control plants the defect class — a refusal left to `rlang::caller_env()` — and shows it comes back naming the calling frame (`thunk`, then `w_leaky`) where the fixed shape names itself, so the agreement between the frame columns and the export column is not vacuous. 16 expectations, all green; the existing pinning suites are untouched and still pass.
- 2026-09-05: T5 removed `ffm()` and `mediainfo_summary()`. Both `export()` lines and both `.Rd` files are gone, `_pkgdown.yml` lost both rows, and every `ffm(` call site in `R/`, `tests/`, `vignettes/` and `README.Rmd` now reads `ffm_files(`. Three recorded fixtures needed handling rather than rewriting: M096's corrupt-limit census (20 rows left the domain, 19 kept and 1 dropped, so 1515/1084/431), the timeout domain lists in `helper-timeout-sweep.R`, and `timeout-valid-baseline.rds`, whose `mediainfo_summary` cell is dropped on read under a `stopifnot()` proving it byte-identical to `mediainfo_template`'s — the same treatment the blob's `has_nvenc` rename already gets. The `ffm is an alias for ffm_files` test is deleted. Suite: FAIL 0, PASS 12603, WARN 10 (unchanged), SKIP 18.
- 2026-09-05: T6 appended D087 to `cairn/DECISIONS.md`, superseding M110's milestone-local decision by name and stating the internal-implementation mechanism, what it rules out, and its falsifier.
- 2026-09-05: T7 added a `NEWS.md` Breaking changes section with two entries — the removed `call` argument, and the two removed duplicate names — and a third saying which function each refusal now names, every claim of which `test-blame-frame-table.R` enforces. `devtools::document()` produces no diff, `pkgdown::check_pkgdown()` reports no problems, and `devtools::check()` is `Status: OK` (0 errors, 0 warnings, 0 notes). Reaching 0 notes needed one carried trivial fix outside this milestone's subject: `inst/WORDLIST` gained `testthat's`, which `tests/spelling.R` had been flagging in a pre-existing NEWS line on master too (measured at `NEWS.md:39` on master, `:63` here). No runtime surface.
- 2026-09-05: plan gate chose a sweep over `getNamespaceExports()` over a grep of `man/*.Rd` usage blocks, because `.Rd` files carry no export status — only 1 of 82 is marked internal — so the grep cannot partition its hits. Falsified by an export whose formals a static sweep cannot read.

- 2026-09-05: step-7 approval: PR #116 approved for merge.
- 2026-09-05: resume: PR #116 OPEN with every criterion evidenced and the approval recorded; re-entered at step 1. `master` unmoved at `819a5299`, so the recorded evidence stands; the step-7 PR-conversation read re-ran and the chip was re-posed and approved.
- 2026-09-05: resume (second): PR #116 still OPEN, `master` still `819a5299`, branch pushed at `3b847ba`; PR-conversation read re-run and the chip re-posed. step-7 approval: PR #116 approved for merge.
- 2026-09-05: resume (third): PR #116 still OPEN, `master` still `819a5299`, branch in sync at `2bdc2a4`; PR-conversation read re-run (no reviews, no review threads, one `codecov[bot]` comment — noted) and the chip re-posed. step-7 approval: PR #116 approved for merge.
- 2026-09-05: CI wait hit the harness ceiling at `1efda8b` (the approval commit restarted the full `pull_request` fleet, which has no `cairn/**` path filter). Fresh `gh pr checks` state at the stop: `pkgdown` pass, the other seven pending; the watcher was stopped with `TaskStop` and nothing left armed. No merge attempted; the approval stands and re-entry is via the session-start resume route.

## Decisions

## Review

Fresh evidence gathered 2026-09-05 on `m112-unpublish-the-call-argument` at
`8390acc`, against `master` at `819a5299`.

**AC1 — no exported function has a formal named `call`. PASS.**
The sweep is `tm_export_formals()` (`tests/testthat/helper-export-formals.R`),
reading `getNamespaceExports("tidymedia")` and keeping the objects that are
functions. Run twice today from a fresh session:

| run | ref | exported functions examined | hits |
|---|---|---|---|
| before | `master` `819a5299`, in a detached worktree | 88 | 2 — `hardware_encoder`, `set_program` |
| after | branch HEAD `8390acc` | 86 | 0 |

The 88→86 drop is the two removed duplicate names (AC3), not a narrowed
domain: the after-run's list still opens at `anonymize_video` and closes at
`with_timeout`, and the sweep's own domain assertion (`nrow > 50`, and
`set_program`/`hardware_encoder`/`ffm_files`/`mediainfo_template` all present)
passes. Its positive control — `hardware_encoder_available()`, which really
does carry a `call` formal — still trips the predicate, so the green is the
two exports losing the argument rather than the detector going blind.

**AC2 — every refusal names the export the caller typed. PASS.**
`tm_blame_table()` (`tests/testthat/test-blame-frame-table.R`) built fresh
today: 7 exports x 2 refusal forms = 14 rows, each measured at the console and
again through one user wrapper, so 28 cells. Every cell names the export.

| export | form | console | wrapper |
|---|---|---|---|
| `set_program` | argument | `set_program` | `set_program` |
| `set_program` | body | `set_program` | `set_program` |
| `set_ffmpeg` | argument | `set_ffmpeg` | `set_ffmpeg` |
| `set_ffmpeg` | body | `set_ffmpeg` | `set_ffmpeg` |
| `set_ffprobe` | argument | `set_ffprobe` | `set_ffprobe` |
| `set_ffprobe` | body | `set_ffprobe` | `set_ffprobe` |
| `set_ffplay` | argument | `set_ffplay` | `set_ffplay` |
| `set_ffplay` | body | `set_ffplay` | `set_ffplay` |
| `set_mediainfo` | argument | `set_mediainfo` | `set_mediainfo` |
| `set_mediainfo` | body | `set_mediainfo` | `set_mediainfo` |
| `hardware_encoder` | argument | `hardware_encoder` | `hardware_encoder` |
| `hardware_encoder` | body | `hardware_encoder` | `hardware_encoder` |
| `has_hardware_encoder` | argument | `has_hardware_encoder` | `has_hardware_encoder` |
| `has_hardware_encoder` | body | `has_hardware_encoder` | `has_hardware_encoder` |

The table's three tests pass (5 + 6 + 5 expectations). The frame columns are
not vacuous: the discriminating control plants the defect — a refusal left to
`rlang::caller_env()` — and it comes back naming `thunk` at the console and
`w_leaky` through the wrapper, where the fixed shape names itself in both.
A second test pins each row to its condition (`tidymedia_program_not_found`
for a `set_*` body row, "must be a single string" for an argument row), so a
body row that aborted in a checker instead cannot pass on the frame alone.

The same table built against `master` `819a5299` returns 13 of 14 rows naming
themselves; the one that does not is `has_hardware_encoder` / argument, which
named `hardware_encoder` there. That is exactly the one exception the Scope
Out clause declares, measured, and no other cell changed side.

**AC3 — both duplicate names are gone. PASS.**
The `master...HEAD` diff removes `export(ffm)` and `export(mediainfo_summary)`
from `NAMESPACE`, the `ffm` and `mediainfo_summary` rows from `_pkgdown.yml`'s
reference index, and `man/ffm.Rd` and `man/mediainfo_summary.Rd` from `man/`
(the two files the diff lists as deleted). AC3's alternative branch — a
recorded decision to keep both — is not taken, so nothing is owed the
Decisions section on this criterion. A grep for either name across `R/`,
`man/`, `tests/`, `vignettes/`, `README.*`, `NAMESPACE` and `_pkgdown.yml`
returns only the recorded-fixture handling the work log describes: the
`mediainfo_summary` cell dropped on read from `timeout-valid-baseline.rds`
under a `stopifnot()` proving it byte-identical to `mediainfo_template`'s, and
the same name inside M096's merge-base census strings, which are read and
never rewritten.

**AC4 — the superseding D-entry. PASS.**
`cairn/DECISIONS.md` gains D087, added by this branch. It names M110's
milestone-local decision as what it supersedes, and states why that decision's
reasoning (D074's objection to duplicating a shared body at five front doors)
does not reach the third option it did not consider. The mechanism is stated:
the shared body moves to an internal taking `call` with no default, and each
export is a wrapper passing `rlang::current_env()` — `tm_set_program()` and
`tm_hardware_encoder()` named as the two seams. It states what it rules out (a
future export publishing `call`, enforced by the AC1 sweep rather than
remembered) and its falsifier: a seam whose blame cannot be threaded without
the caller supplying the environment.

**AC5 — document / test / check / NEWS. PASS.**
`devtools::document()` run today leaves `man/`, `NAMESPACE` and `DESCRIPTION`
clean in `git status` — no diff. `devtools::check()` on the branch:
`Status: OK`, 0 errors / 0 warnings / 0 notes, 11m 1.7s, with the full test
suite running inside it (`Running 'testthat.R' [9m/10m] OK`) and
`spelling.R` OK. `NEWS.md` gains a Breaking changes section with three
entries — the removed `call` argument, which function each refusal now names
(including `has_hardware_encoder()`'s change), and the two removed duplicate
names — none of which mentions a milestone number.

Toolchain consistency gate (the `r-package` profile's `consistency-gate`
slot), run today: `document()` no diff — pass; generated files not hand-edited
— pass, by the same no-diff run; `README.md` in sync with `README.Rmd` — pass,
the branch re-knits it (the diff carries fresh `tempdir()` paths, so
`build_readme()` really ran); `pkgdown::check_pkgdown()` — "No problems
found."; changelog entry for the user-visible change — pass, above; new
top-level files needing `.Rbuildignore` — none added; full `devtools::check()`
clean — pass, above.

Universal cairn-file checks: `cairn_validate.py` exits 0, all checks passed,
`release window` advisory not fired. `cairn/DESIGN.md` carries no IPn/GPn
change on this branch (the file is untouched by the diff), so `cairn_impact.py
--changed` is not owed.

### Independent fresh-context review

Surface tier is user-facing and the diff touches R sources and tests, so the
full three-lens fan-out ran, each lens fresh-context and on its own evidence
base.

**[S] prior-PR-comments.** First run was discarded: it read this milestone
file's in-progress Review section, treated it as prior-review evidence, and
skipped the `gh` probe on that reasoning — a circular base, since that section
is this review's own output. Re-run with the file excluded and the probe
mandatory: the probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`
returned `[]` — the repo has no non-bot inline review comments at all, so the
PR-thread walk is skipped and the archived `## Review` sections plus
`cairn/LESSONS.md` are the whole prior-review base. No findings: the diff
applies the M100/M110 `call`-threading lesson (no default on the internal,
`current_env()` at the wrapper, checkers threaded) rather than regressing it,
and the domain edits follow M096's domain-forcing lesson. One claim in its
report does not survive checking against the implementation — that the new
sweep test "includes a positive-control probe before asserting the negative";
[O]3 below shows that control never runs the sweep, and [O]3 is what this
review acted on.

**[S] blame-history.** No findings. It read `git log`/`git blame` on the
modified lines, D014/D042/D049/D074-D087 and the M096/M100/M110 archives, and
reports that D042's carve-out licenses threading `call` through an internal
signature verbatim; that D087's supersession of M110 is honest, because M110's
own text flagged the third option as unconsidered and named a second seam as
the trigger for a D-entry; that neither removed alias has any recorded design
rationale to undo; and that `tm_corrupt_dropped_master()`, the frozen
`4063faa` merge-base census, is untouched by the diff — the edited helpers are
domain-membership lists, and the `.rds` cell is dropped on read behind an
equality assertion rather than re-recorded.

**[O] diff-bug.** Nine findings, ranked; the reviewer separately confirmed
clean: no leftover `ffm(`/`mediainfo_summary` call sites outside the preserved
census strings, no internal caller still routing through the exported
`hardware_encoder()`, unchanged `error_arg`/partial-matching messages, a
missing `location` still blamed on `set_program()`, and the AC1 sweep
unpolluted by `load_all(export_all = TRUE)`.

**[O]'s findings, as reported.** Verified against the implementation before
triage; the verdict column is this review's, the text the reviewer's.

- **[O]1 `cairn/DESIGN.md:96-105`** — "the Known-issues bullet now states
  something the branch has made false, and DESIGN is untouched by the diff."
  It says a wrong-form `codec` or `hardware` at `has_hardware_encoder()` "is
  refused by the `arg_match()` inside `hardware_encoder()`, the mapper it
  consults", and opens with "Twelve arguments are refused below the verb the
  caller typed". CONFIRMED: AC2's own table measures both refusals naming
  `has_hardware_encoder()` on this branch, so the clause is false and the
  count is ten, not twelve.
- **[O]2 `R/program_management.R:213` vs `:236`** — "the exported `program`
  default and the vocabulary `arg_match()` enforces are now two independent
  literals, and nothing pins them together." CONFIRMED by reading both:
  `set_program()` publishes `c("ffmpeg", "ffprobe", "ffplay", "mediainfo")`
  and `tm_set_program()` carries its own copy, which is the one `arg_match()`
  reads. Adding a fifth program to one and not the other leaves the help page
  and the guard disagreeing with a green suite. The hardware seam avoided this
  by passing the tables as explicit `values` and pinning the exported literal
  in `test-hardware-backends.R`.
- **[O]3 `tests/testthat/test-exported-call-formal.R:31-38`** — "the 'positive
  control' never runs the sweep, so the `has_call` column has no test at all."
  CONFIRMED: the control asserts `"call" %in% names(formals(...))` twice
  without ever calling `tm_export_formals()`, so breaking the helper's
  predicate to `"call" %in% names(x)` (all FALSE) leaves all three tests green.
  AC1 itself still holds — this review's before/after run gave the predicate a
  live control by returning 2 hits at `master` and 0 at HEAD — but the
  committed instrument does not carry one.
- **[O]4 `tests/testthat/helper-timeout-sweep.R:428-430`** — "stale count in a
  docstring the diff edited": "Six members already named themselves; the other
  47" should read 46. CONFIRMED by measurement: the recorded domain is 52, six
  self-naming, forty-six not.
- **[O]5 `R/ffmpeg.R:3047, 3216, 3272, 3300, 3324, 3371`** — "six comments
  still name `hardware_encoder()` as the site of the family-not-in-table
  refusal", which now lives in `tm_hardware_encoder()`. CONFIRMED; the blamed
  frame is still the verb, so nothing user-visible is wrong.
- **[O]6 `R/ffmpeg.R:3113-3118`** — the "Literal defaults, not
  `hardware_codec_families()`/`hardware_backends()`" comment "is orphaned by
  the move": it now sits above a bare delegation while the body it moved to
  passes both tables as explicit `values`. CONFIRMED.
- **[O]7 `cairn/milestones/M112-unpublish-the-call-argument.md:63`** — "AC5 is
  unchecked and has no Review paragraph." True when the reviewer read the
  file; addressed in this same step-3 pass, which is where AC5's evidence and
  tick belong.
- **[O]8 `tests/testthat/helper-timeout-sweep.R:745-747`** — "the new baseline
  remap lacks the presence assertion its sibling carries", so a re-recorded
  blob fails with an opaque `identical(...) is not TRUE` rather than telling
  the maintainer the drop is dead code. CONFIRMED against the `has_nvenc`
  block six lines above, which does assert presence.
- **[O]9 Minor** — `inst/WORDLIST` gaining `testthat's` is an unrelated
  carried fix; `R/mediainfo.R:113-115` leaves a raggedly rewrapped paragraph;
  `test-blame-frame-table.R:11-70` defines its sweep functions in the test
  file while the AC1 sweep went into a `helper-` file.

**Return floor.** None of the nine demonstrates an acceptance criterion
failing: AC1-AC5 are each measured true on this branch today. [O]3 weakens
AC1's committed instrument for the future without falsifying AC1 now, and
[O]1 is a stale architecture record rather than a defect in what the package
does. So no finding returns the milestone; each takes ordinary triage.

**Triage and disposition.** Put to the maintainer at the gate; the chosen
disposition was fix 1-6 and 8 on the branch, reject 9.

- **[O]1 — FIXED.** `cairn/DESIGN.md`'s Known-issues bullet now reads ten
  arguments, drops the false clause, and records that
  `has_hardware_encoder()`'s `codec` and `hardware` were there until D087 and
  what moved them.
- **[O]2 — FIXED.** `tests/testthat/test-program-management.R` gains a test
  pinning `formals(set_program)$program` to `formals(tm_set_program)$program`
  and both to `tm_program_vocabulary`, so the published spelling and the
  vocabulary `arg_match()` enforces cannot drift apart silently. This is the
  hardware seam's pattern applied to the program seam.
- **[O]3 — FIXED.** The control in `test-exported-call-formal.R` now runs
  `tm_export_formals("rlang")` and asserts `abort` comes back with
  `has_call` TRUE and `call` in its formals string, plus that the column is
  not uniformly TRUE. Shown to bite: with the helper's predicate mutated to
  `"call" %in% names(x)` — the defect [O] named — the file goes FAIL 1 / PASS
  8, where before the fix it stayed FAIL 0 / PASS 9.
- **[O]4 — FIXED.** 47 → 46, with a paragraph saying the table's membership
  tracks the recorded domain while each cell's measured blame is frozen, and
  naming `tm_corrupt_dropped_master()` as the census that is never rewritten.
- **[O]5 — FIXED.** All six comments now name `tm_hardware_encoder()`, where
  the abort lives.
- **[O]6 — FIXED.** The comment says the `codec` literal is a published
  spelling pinned to `hardware_codec_families()` by
  `test-hardware-backends.R`, and that the body it delegates to is handed both
  tables as explicit `values` — checked against both files.
- **[O]7 — ADDRESSED** in this same pass; AC5's evidence and tick are above.
- **[O]8 — FIXED.** `stopifnot("mediainfo_summary" %in% names(table))` added
  ahead of the equality assertion, matching the `has_nvenc` remap six lines
  above, with the dead-code note.
- **[O]9 — REJECTED.** The `inst/WORDLIST` line is an out-of-subject carried
  fix already disclosed in the work log and has no runtime surface; the
  rewrapped paragraph and the test-file siting are style points a formatter or
  a linter's remit, which the triage taxonomy rejects.

**PR-conversation read (PR #116).** Empty: no reviews, no conversation
comments, and no review threads (resolved or unresolved), so no disposition is
owed and the blocking rule does not fire.

Re-read at the re-posed gate: still no reviews and no review threads, resolved
or unresolved. One conversation comment has appeared since — `codecov[bot]`,
reporting the patch fully covered and project coverage 98.41% → 98.42%.
conversation: codecov[bot] PR #116 — noted (requests nothing; author `type` is
`Bot`, so the blocking rule does not fire). All ten CI checks pass.

Re-verified after the fixes: `devtools::document()` no diff,
`devtools::check()` `Status: OK` 0/0/0 with the suite running inside it
(`testthat.R [295s/360s]`).

