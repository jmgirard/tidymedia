# M112: No export publishes the internal `call` argument, and the duplicate exports are settled

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — an exported signature and the Rd usage line a reader copies from
- **Branch/PR:** `m112-unpublish-the-call-argument`

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

- [ ] AC1: No object in `getNamespaceExports("tidymedia")` that is a function
      has a formal named `call`. Evidence: the sweep's output naming every
      export it examined and the count, run before and after the change so the
      before-run shows the two hits.
- [ ] AC2: Across the seven exports the change touches — `set_program`,
      `set_ffmpeg`, `set_ffprobe`, `set_ffplay`, `set_mediainfo`,
      `hardware_encoder`, `has_hardware_encoder` — an ARGUMENT refusal (a
      wrong-typed `location`, `confirm`, `codec` or `hardware`) and a BODY
      refusal (no such executable; a pair no backend's table holds) each name
      the export the caller typed, at the console and from a wrapper.
      Evidence: a table with one row per export x refusal form, carrying the
      frame each refusal named.
- [ ] AC3: For each of `ffm`/`ffm_files` and
      `mediainfo_summary`/`mediainfo_template`, either one name is gone from
      `NAMESPACE`, `_pkgdown.yml` and `man/`, or this file's Decisions section
      records why both stay, citing D014's pre-0.2.0 window. Evidence: the
      NAMESPACE diff or the decision entry.
- [ ] AC4: A `cairn/DECISIONS.md` entry supersedes M110's `call`-stays-a-formal
      decision by name, states the internal-implementation mechanism, and says
      what would falsify it. Evidence: the entry.
- [ ] AC5: `devtools::document()` produces no diff; `devtools::test()` and
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
- [ ] T6: Append the superseding D-entry.
- [ ] T7: `document()`, `test()`, `check()`, `NEWS.md`.

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
- 2026-09-05: plan gate chose a sweep over `getNamespaceExports()` over a grep of `man/*.Rd` usage blocks, because `.Rd` files carry no export status — only 1 of 82 is marked internal — so the grep cannot partition its hits. Falsified by an export whose formals a static sweep cannot read.

## Decisions

## Review
