# M110: `set_program()` asks before it remembers a location

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — it changes what five exported functions do and adds an argument to each
- **Branch/PR:** `m110-set-program-confirmation` / https://github.com/jmgirard/tidymedia/pull/114

## Goal

`set_program()` and its four exported wrappers obtain the caller's consent — naming the file they write and the location they record — before creating a directory or writing anything under `tools::R_user_dir("tidymedia", "config")`, refuse rather than assume consent where no one can be asked, and say what they did.

## Scope

**In:** a `confirm = TRUE` argument on `set_program()`, threaded through `set_ffmpeg()`, `set_ffprobe()`, `set_ffplay()` and `set_mediainfo()`, checked with `rlang::check_bool()` at each front door; a prompt built from the resolved config file path and the location as typed, brace-escaped through `tm_cli_escape()`; the consent sited above the `dir.create()` at `R/program_management.R:213`, so a declined or refused call creates nothing and writes nothing; the existing `tm_confirm()` seam, so a session with no one to ask aborts `tidymedia_confirmation_unavailable`; `install_on_win()`'s three registrations passing `confirm = FALSE`, its own prompt having already named all three overwrites; a `TRUE`/`FALSE` return replacing the `@return` M097's review found false; a condition class and a `call` on the no-executable refusal at `:203`; the four in-repo scripted callers and the roxygen example updated; `NEWS.md`, `README.Rmd`, `?set_program`.

**Out:** removing or migrating a remembered location (an `unset_program()`) → candidate row. The rest of the package's unclassed aborts → the existing candidate row for the naming pass; only `set_program()`'s own site is in. A confirmation on any other write → not proposed. macOS and Linux installers → existing candidate row.

## Acceptance criteria

- [x] AC1: A `set_program()` call the caller declines, and one refused for want of anyone to ask, each leave the config directory exactly as they found it and discard nothing. Verified at a redirected `R_USER_CONFIG_DIR` over the cross of four programs by three prior directory states — absent, existing and empty, existing and holding a prior `<program>_location.txt` — asserting directory non-existence directly where it was absent, and otherwise comparing a recursive listing (`all.files = TRUE`) and the bytes of every file in it before and after; a capability memo present before the call is still present after it.
- [x] AC2: For every function `NAMESPACE` exports whose name begins with `set_` — enumerated from `NAMESPACE`'s export entries, not from a hand list — `formals()` reports `confirm` defaulting to `TRUE`; under `confirm = TRUE` in a session with no one to ask the function aborts with condition class `tidymedia_confirmation_unavailable` whose message, ANSI-stripped, contains `confirm = FALSE`; and under `confirm = FALSE` it writes the `<program>_location.txt` of the program it sets and returns `TRUE`.
- [x] AC3: An approved `install_on_win()` run asks for confirmation exactly once, counted at `utils::menu()`, and still registers every program the archive produced.
- [x] AC4: The consent prompt contains, ANSI-stripped and verbatim, the config file path the call would write and the `location` string as the caller typed it — never a path `Sys.which()` resolved it to, which is not what is written. A `location` holding an unmatched `{` and a `location` holding a `{name}` that names a local of the prompt-building frame each appear as typed rather than aborting or interpolating.
- [x] AC5: Each of the `set_*` exports of AC2 returns `TRUE` invisibly where it wrote the location and `FALSE` invisibly where the caller declined, and the `@return` on the Rd page they share states both.
- [x] AC6: For every `set_*` export of AC2, a `location` with no executable at it aborts with condition class `tidymedia_program_not_found` whose `call` field names that export — the direct `set_program()` call included, which is the one reaching the argument's own default.
- [x] AC7: `devtools::test()` clean, `devtools::document()` produces no diff, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T5
- AC2 → T2, T5
- AC3 → T4, T6
- AC4 → T1, T2, T5
- AC5 → T2, T5, T7
- AC6 → T3, T5
- AC7 → T7, T8

## Tasks

- [x] T1: Write `tm_set_details(program, location, dir)` and `tm_set_prompt()` beside `tm_install_details()` (`R/program_management.R:799`), composing the resolved `tm_config_file(program)` and the location as typed through a cli file field and `cli::ansi_strip(cli::format_inline())`, in that helper's shape.
- [x] T2: Add `confirm = TRUE` last in `set_program()`'s signature with `rlang::check_bool()`, and thread it through the four wrappers (`:230`-`:250`); call `tm_confirm()` above the `dir.create()` at `:213`, returning `invisible(FALSE)` on a decline and `invisible(TRUE)` after the write, with `forget_ffmpeg_capabilities()` reached only on the write path.
- [x] T3: Give the `:203` abort class `tidymedia_program_not_found` and a `call`, threaded from the four wrappers and defaulting to `rlang::current_env()` — not `caller_env()`, which leaves a direct call blamed on its caller (the M100 lesson).
- [x] T4: Pass `confirm = FALSE` at `install_on_win()`'s registration loop (`:1354`).
- [x] T5: Tests in `tests/testthat/test-program-management.R` for AC1, AC2, AC4, AC5, AC6. Keep any base-namespace stub out of the tests that compare values — under a `dir.create()` stub `expect_identical()` dies inside waldo (the M108 lesson) — so AC1's byte comparison observes the disk rather than a stub.
- [x] T6: Test for AC3: a full install run counting `utils::menu()` calls, asserting one, over an archive producing fewer than three programs as well as all three.
- [x] T7: Roxygen for `confirm` and the corrected `@return`, then `devtools::document()`; update the four in-repo scripted callers (`test-program-management.R:56`, `test-audio-track-drop.R:151` among them) and the `\dontrun` example at `:195`.
- [x] T8: `NEWS.md` entry naming the new argument, the refusal and the breaking change; `README.Rmd` line 50 plus `devtools::build_readme()`; `devtools::check()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: `cairn_validate`'s sizing tripwire fired at 8 criteria; the merge of the `set_program()`-only refusal criterion into the NAMESPACE-enumerated one removed a real overlap rather than shrinking to fit, leaving 7.
- 2026-09-05: criteria audit ran in full mode (user-facing tier), fresh [O] reader; fourteen findings over six drafted criteria, eleven fixed before writing (AC1's prior-directory-state axis and its vacuous empty-listing comparison, AC2's unpinned refusal position and unstripped message match, AC3's unpinned default plus its instrument-bound "passes to the seam" promise and its undefined file name for `set_program` itself, AC4's mock-dependent ask count and its unsatisfiable all-three-programs clause, AC5's single brace form and unstripped comparison, AC6's four unconstrained wrappers), three posed at the gate.
- 2026-09-05: T3 added `call = rlang::current_env()` as `set_program()`'s last formal, so `confirm` is no longer last in the signature; the four wrappers pass their own frames and each not-found refusal names the export the caller typed. The roxygen for `confirm`, `call` and the corrected `@return` (T7's docs half) landed with it, since `call` is a formal on an exported function.
- 2026-09-05: plan gate chose refusing a non-interactive call over proceeding-with-disclosure, because one contract for "no one can be asked" is what D080 already bought at the installer and a second answer at a second site would need its own entry; the user waived the pre-1.0 deprecation cycle for the break this causes. Falsified by an unattended workflow that must register a location and cannot pass `confirm = FALSE`.
- 2026-09-05: T5 added five tests (AC1, AC2, AC4, AC5, AC6); suite 12586 pass / 0 fail. Seven mutation probes each reddened it: creating the config dir above the consent, `caller_env()` for the blame, dropping the escape-hatch bullet, interpolating the location instead of using a cli field, naming the `Sys.which()` resolution, a visible `FALSE`, and forgetting the capability memo on the decline path.
- 2026-09-05: T6 counts the ask at `utils::menu()` over both a three-program and a two-program archive; suite 12600 pass / 0 fail. Dropping `confirm = FALSE` from the registration loop turns the count into four and reddens both halves.
- 2026-09-05: T7 landed with T3 (the roxygen for `confirm`, `call` and the corrected `@return`, and the second `\dontrun` example); `devtools::document()` produces no diff.
- 2026-09-05: T8 added one `NEWS.md` breaking-changes entry pair, the README note and its knit (only the two `temp_libpath` lines reverted, M089), and `unclassed` to `inst/WORDLIST`, which the spelling check had turned into the run's one NOTE. `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-09-05: plan gate chose showing the location as typed over showing it beside its `Sys.which()` resolution, because the file records the typed string and a prompt naming something else asks consent for a write that will not happen. Falsified by a report of a caller approving a bare program name and being surprised by which binary it later resolved to.
- 2026-09-05: T1 built `tm_set_details()`/`tm_set_prompt()`; the implementation gate chose a cli file field over `tm_cli_escape()` for the prompt's two values, because that is what `tm_install_details()` already uses and doubling the escape prints stray braces. T1's task text amended to name the field.
- 2026-09-05: T4 pulled ahead of T3 — the suite cannot be clean between T2 and T4, since `install_on_win()` would otherwise ask once per registered program.
- 2026-09-05: T2/T4 also updated the `set_program` mock at `test-program-management.R:496` and the two scripted callers (`test-program-management.R:56`, `test-nvenc-memo.R:103`) that T7 names; the other two sites T7 lists are comments, not calls. Suite 12440 pass / 0 fail.
- 2026-09-05: plan gate chose folding the `:203` abort's class and `call` into this milestone over leaving it for the package-wide naming pass, because the front door it sits in is already being rewritten here and one site's event name is decided by D062 without the pass. Falsified by the naming pass settling on a name for this event other than `tidymedia_program_not_found`.

- 2026-09-05: review checkpoint — all seven criteria verified with fresh evidence and ticked, consistency gate passed; PR #114 open as a draft, CI running, three fresh-context reviewers still reporting.

- 2026-09-05: review fix-now — six findings fixed on the branch (argument-check blame, condition fields, the `.Rprofile` NEWS case, AC1's collapsed occupied cell, AC5's inert regex, a ROADMAP row claiming a defect this branch fixed); one routed to the unclassed-aborts candidate row, four rejected with reasons.
- 2026-09-05: step-7 approval: PR #114 approved for merge.
- 2026-09-05: CI wait hit the harness ceiling with R-CMD-check still running on 7219c25 (macos green, five legs in progress); watcher stopped, not merged. Resume at /milestone-review M110, which re-derives the state.

## Decisions

- 2026-09-05 (review): `call` stays a threaded formal on `set_program()`, against D074's siting paragraph, which has each export re-call the shared checker at its own front door rather than thread a `call` argument through an exported builder. D074's mechanism cannot reach this seam: the two refusals whose blame the milestone is about — the no-executable abort and the non-interactive consent refusal — sit below the config-path resolution and the prompt build, so re-calling them at four wrappers would duplicate both bodies. The threading now covers the argument checkers too, so every refusal from every export names the export. Falsified by a second seam wanting the same treatment, at which point the pair is a pattern worth a D-entry of its own rather than two local departures.

## Review

Evidence measured 2026-09-05 at `f52da19` (branch head), macOS, R CMD check 0/0/0. Per-criterion expectation counts come from a `testthat::test_file()` run of `tests/testthat/test-program-management.R` reported per test.

- **AC1 — met.** `a declined or refused set_program() leaves the config directory as it found it`: 80 expectations, 0 failures, over all 24 cells (four programs x three prior directory states {absent, empty, occupied} x two ways consent is withheld {menu answers "no", non-interactive refusal}). Where the directory was absent the test asserts `dir.exists()` is still `FALSE` directly; in the other two states it compares the recursive `all.files = TRUE` listing, the directory listing, and the raw bytes of every file, before against after. A capability memo planted in `.tm_capabilities` before each call is asserted present after it. No stub sits below `tm_confirm()`, so the comparison reads the disk.
- **AC2 — met.** `every exported set_* function takes confirm, refuses, and writes under FALSE`: 32 expectations, 0 failures. The domain is read from `NAMESPACE`'s `export(set_...)` entries (source tree under `devtools::test()`, the installed copy under `R CMD check`) and asserted set-equal to the five exports `set_ffmpeg`, `set_ffplay`, `set_ffprobe`, `set_mediainfo`, `set_program`, so a read returning nothing fails rather than passing vacuously. For each: `formals()$confirm` is `TRUE`; under `confirm = TRUE` with `rlang_interactive = FALSE` the call aborts `tidymedia_confirmation_unavailable`, its ANSI-stripped message contains `confirm = FALSE`, and no config directory exists afterward; under `confirm = FALSE` it returns `TRUE`, writes `<program>_location.txt` holding the stub path, and leaves that one file in the directory. Confirmed independently by hand: `grep -E '^export\(set_' NAMESPACE` lists exactly those five, and `man/set_program.Rd`'s `\usage` shows `confirm = TRUE` on all five signatures.
- **AC3 — met.** `an approved install asks exactly once and still registers what it produced`: 14 expectations, 0 failures. The counter sits on `utils::menu()`, below `tm_confirm()`, so the real seam and the real `set_program()` both run and a second ask would be counted. Over an archive producing all three programs the count is 1 and each of the three `<program>_location.txt` files is read back off disk holding `tm_install_binary()`'s path; over an archive producing only `ffmpeg` and `ffprobe` the count is again 1, those two files hold their paths, and no `ffplay` file exists.
- **AC4 — met.** `the consent prompt names the file it would write and the location as typed`: 15 expectations, 0 failures. The prompt is captured at `menu()`'s `title` and ANSI-stripped. For each of three stub names — `plain`, `a{program}b`, `c{d` — the prompt contains the typed location verbatim and the resolved `tm_config_file("ffmpeg")` verbatim, and the brace form `a{program}b` does not appear as the interpolated `affmpegb` (the unmatched-brace form does not abort). With the stub's directory on `PATH` and the call given the bare basename, the prompt contains `'<basename>'` and does not contain the directory `Sys.which()` would resolve it to.
- **AC5 — met.** `a set_* call returns TRUE or FALSE invisibly, and the Rd page says both`: 8 expectations, 0 failures. `withVisible(set_ffmpeg(stub, confirm = FALSE))` gives `value` `TRUE` and `visible` `FALSE`; `withVisible(set_ffprobe(stub))` against a declining `menu()` gives `value` `FALSE` and `visible` `FALSE`. The `\value{}` block of the shared Rd page matches `TRUE`, `FALSE` and `nvisibl`. Read directly: `man/set_program.Rd` states "Invisibly, \code{TRUE} where the location was written and \code{FALSE} where the caller declined to write it."
- **AC6 — met.** `a location with no executable aborts by name, blaming the export that was typed`: 11 expectations, 0 failures. Over the same five `NAMESPACE`-read exports, a location under a temp dir asserted not to exist aborts `tidymedia_program_not_found` and `rlang::call_name(conditionCall(cnd))` is the export's own name — `set_program` included, the one call reaching `call`'s default of `rlang::current_env()`.
- **AC7 — met.** First run at `f52da19`: `devtools::test()` 12600 pass / 0 fail / 18 skip / 10 warnings (pre-existing binary-probe noise); `devtools::document()` no diff; `devtools::check()` 0 errors / 0 warnings / 0 notes in 6m47.7s. Re-run after the fix-now commit: `devtools::test()` 12614 pass / 0 fail / 18 skip; `devtools::document()` no diff; `devtools::check()` 0 errors / 0 warnings / 0 notes in 6m0.4s. `cairn_validate.py` re-run after the ROADMAP correction: exit 0, all checks passed.

**Consistency gate — passed.** `cairn_validate.py` exit 0, 16 PASS and 7 advisories all OK (no `release window`). No `DESIGN.md` principle changed, so `cairn_impact.py` does not apply. Toolchain checks from the `r-package` profile: `devtools::document()` no diff; `NAMESPACE`, `man/` regenerate clean; `README.md` is the knit of the changed `README.Rmd` lines (the M089 `temp_libpath` quirk is the only knit-time revert, already applied at T8); `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` carries two breaking-changes entries for this work, with no milestone numbers in the user-facing text; no new top-level files, so no `.Rbuildignore` entry is owed; `devtools::check()` 0 errors / 0 warnings / 0 notes.

**Independent review — three fresh-context lenses, user-facing tier.** [O] diff-bug: 10 findings. [S] blame-history: 0 findings (checked the diff's M38/M40, M44, M097, M100, M108 citations against what those commits established, and the `install_on_win()` registration loop against M104's all-or-none invariant). [S] prior-review: 1 finding from the archived `## Review` record; its `gh api .../pulls/comments` probe returned `[]`, so the per-PR thread walk was correctly skipped. No finding demonstrates an acceptance criterion failing, so none reaches the return floor.

Fix-now, committed on the branch before approval:

- **[O]1 — the argument checkers blamed `set_program()`, not the export the caller typed.** `rlang::check_string()`/`check_bool()`/`arg_match()` ran on their own `caller_env()` default, so `set_ffmpeg("/bin/ls", confirm = "yes")` reported `Error in set_program("ffmpeg", location, confirm = confirm, call = rlang::current_env())` — a function the caller never typed, with the internal threading on display. Measured live before the fix. Contradicts the Scope's "checked with `rlang::check_bool()` at each front door" and D076's outcome, and no test covered it. Fixed by passing the already-threaded `call` into all three checkers; all five exports now name themselves. New test `a set_* argument refusal blames the export the caller typed`, 11 expectations.
- **[O]4 — the new condition carried no data fields.** `tidymedia_program_not_found` gave a handler nothing but a cli-formatted message to regex. Added `tm_program` and `tm_location`, the field family D062 names and the three nearest precedents supply. New test `the not-found abort carries the program and the location it refused`, 3 expectations.
- **[O]7 — `.Rprofile` is a third break case the NEWS entry did not name.** A session counts as interactive while `.Rprofile` is sourced, so a `set_ffmpeg()` there now blocks startup on a prompt rather than refusing. NEWS breaking-changes entry extended to name it and point at `confirm = FALSE`.
- **[O]8 — AC1's "occupied" cell held one file, not two, on the `ffprobe` iteration.** The second `tm_write_location()` named `ffprobe` literally, overwriting the first when the loop variable was also `ffprobe`. The comparison still held; the cell was weaker than it read. Now writes to `setdiff(tm_program_vocabulary, program)[[1]]`, so every cell compares a two-file directory.
- **[O]9 — AC5's Rd lookup had an inert alternative.** `grepl("^set_program\\.Rd$|set_program", ...)` — the second alternative subsumes the first. Tightened to the anchor alone, which holds in both `rd_sources()` shapes (`tools::Rd_db()` names its elements by `.Rd` filename too).
- **[S prior-review]1 — a ROADMAP candidate row claimed a defect this branch fixed.** The unclassed-aborts row still read "Absorbs `set_program()`'s own abort (`R/program_management.R:205`: no class, no `call =`)". Corrected in place (current-knowledge file) to record that the site left the row at M110 and to leave the naming question — whether `tidymedia_program_not_found` is the right name for `find_program()`'s two not-found warnings — with the pass.

Follow-up:

- **[O]3 — `tidymedia_program_not_found` may be broader than the event it names.** The class currently marks one thing: a `set_*` argument whose location holds no executable. `find_program()`'s two unclassed not-found warnings (`R/program_management.R:89`, `:96`) are what a handler catching that name would expect. This is the milestone's own logged falsifier for the T3 plan decision, and it is the naming pass's call, not this milestone's; absorbed into the existing unclassed-aborts candidate row rather than filed as a new one (search-first).

Rejected, with reasons:

- **[O]2 — `call` is an exported, documented formal, which D074's siting paragraph rejects.** The tension is real and is recorded as a milestone-local decision below rather than silently carried. Rejected as a change to make here: the not-found abort and the consent refusal sit deep in the shared body, so D074's "re-call the checker at each front door" cannot reach them without duplicating both at four wrappers; the plan gate chose the threading at T3, and `\usage` already shows `call` on `set_program()` alone. [O]1's fix removes the half of the complaint that reached a user.
- **[O]5 — the refusal's bullets order the escape hatch above the details, inverting `install_on_win()`'s.** Pure presentation nitpick on an intentional message; no user is misled by either order.
- **[O]6 — the decline is silent, against the Goal's "say what they did".** AC5 pins the invisible `FALSE` deliberately, and a caller who has just answered "No" to a prompt naming both items has been told. `install_on_win()`'s visible `FALSE` ends a long operation; this ends a question the user just answered.
- **[O]10 — the prompt derives the config file path a second time rather than reusing the write's.** Both derivations are `tm_config_file(program, config_dir)` on the same two values inside one call, so they cannot diverge; restructuring the helper signature at review buys nothing and costs risk.

Post-fix verification: `devtools::test()` 12614 pass / 0 fail / 18 skip; `devtools::document()` no diff; per-criterion counts unchanged except the two new tests (AC-adjacent, not criterion evidence).

**PR conversation (#114), read once before the merge gate:** no reviews, no conversation comments, no unresolved review threads. Nothing to triage; the blocking rule does not fire.
