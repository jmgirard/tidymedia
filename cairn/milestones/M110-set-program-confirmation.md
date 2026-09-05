# M110: `set_program()` asks before it remembers a location

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — it changes what five exported functions do and adds an argument to each
- **Branch/PR:** —

## Goal

`set_program()` and its four exported wrappers obtain the caller's consent — naming the file they write and the location they record — before creating a directory or writing anything under `tools::R_user_dir("tidymedia", "config")`, refuse rather than assume consent where no one can be asked, and say what they did.

## Scope

**In:** a `confirm = TRUE` argument on `set_program()`, threaded through `set_ffmpeg()`, `set_ffprobe()`, `set_ffplay()` and `set_mediainfo()`, checked with `rlang::check_bool()` at each front door; a prompt built from the resolved config file path and the location as typed, brace-escaped through `tm_cli_escape()`; the consent sited above the `dir.create()` at `R/program_management.R:213`, so a declined or refused call creates nothing and writes nothing; the existing `tm_confirm()` seam, so a session with no one to ask aborts `tidymedia_confirmation_unavailable`; `install_on_win()`'s three registrations passing `confirm = FALSE`, its own prompt having already named all three overwrites; a `TRUE`/`FALSE` return replacing the `@return` M097's review found false; a condition class and a `call` on the no-executable refusal at `:203`; the four in-repo scripted callers and the roxygen example updated; `NEWS.md`, `README.Rmd`, `?set_program`.

**Out:** removing or migrating a remembered location (an `unset_program()`) → candidate row. The rest of the package's unclassed aborts → the existing candidate row for the naming pass; only `set_program()`'s own site is in. A confirmation on any other write → not proposed. macOS and Linux installers → existing candidate row.

## Acceptance criteria

- [ ] AC1: A `set_program()` call the caller declines, and one refused for want of anyone to ask, each leave the config directory exactly as they found it and discard nothing. Verified at a redirected `R_USER_CONFIG_DIR` over the cross of four programs by three prior directory states — absent, existing and empty, existing and holding a prior `<program>_location.txt` — asserting directory non-existence directly where it was absent, and otherwise comparing a recursive listing (`all.files = TRUE`) and the bytes of every file in it before and after; a capability memo present before the call is still present after it.
- [ ] AC2: For every function `NAMESPACE` exports whose name begins with `set_` — enumerated from `NAMESPACE`'s export entries, not from a hand list — `formals()` reports `confirm` defaulting to `TRUE`; under `confirm = TRUE` in a session with no one to ask the function aborts with condition class `tidymedia_confirmation_unavailable` whose message, ANSI-stripped, contains `confirm = FALSE`; and under `confirm = FALSE` it writes the `<program>_location.txt` of the program it sets and returns `TRUE`.
- [ ] AC3: An approved `install_on_win()` run asks for confirmation exactly once, counted at `utils::menu()`, and still registers every program the archive produced.
- [ ] AC4: The consent prompt contains, ANSI-stripped and verbatim, the config file path the call would write and the `location` string as the caller typed it — never a path `Sys.which()` resolved it to, which is not what is written. A `location` holding an unmatched `{` and a `location` holding a `{name}` that names a local of the prompt-building frame each appear as typed rather than aborting or interpolating.
- [ ] AC5: Each of the `set_*` exports of AC2 returns `TRUE` invisibly where it wrote the location and `FALSE` invisibly where the caller declined, and the `@return` on the Rd page they share states both.
- [ ] AC6: For every `set_*` export of AC2, a `location` with no executable at it aborts with condition class `tidymedia_program_not_found` whose `call` field names that export — the direct `set_program()` call included, which is the one reaching the argument's own default.
- [ ] AC7: `devtools::test()` clean, `devtools::document()` produces no diff, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T5
- AC2 → T2, T5
- AC3 → T4, T6
- AC4 → T1, T2, T5
- AC5 → T2, T5, T7
- AC6 → T3, T5
- AC7 → T7, T8

## Tasks

- [ ] T1: Write `tm_set_details(program, location, dir)` and `tm_set_prompt()` beside `tm_install_details()` (`R/program_management.R:799`), composing the resolved `tm_config_file(program)` and the location as typed through `tm_cli_escape()` and `cli::ansi_strip(cli::format_inline())`, in that helper's shape.
- [ ] T2: Add `confirm = TRUE` last in `set_program()`'s signature with `rlang::check_bool()`, and thread it through the four wrappers (`:230`-`:250`); call `tm_confirm()` above the `dir.create()` at `:213`, returning `invisible(FALSE)` on a decline and `invisible(TRUE)` after the write, with `forget_ffmpeg_capabilities()` reached only on the write path.
- [ ] T3: Give the `:203` abort class `tidymedia_program_not_found` and a `call`, threaded from the four wrappers and defaulting to `rlang::current_env()` — not `caller_env()`, which leaves a direct call blamed on its caller (the M100 lesson).
- [ ] T4: Pass `confirm = FALSE` at `install_on_win()`'s registration loop (`:1354`).
- [ ] T5: Tests in `tests/testthat/test-program-management.R` for AC1, AC2, AC4, AC5, AC6. Keep any base-namespace stub out of the tests that compare values — under a `dir.create()` stub `expect_identical()` dies inside waldo (the M108 lesson) — so AC1's byte comparison observes the disk rather than a stub.
- [ ] T6: Test for AC3: a full install run counting `utils::menu()` calls, asserting one, over an archive producing fewer than three programs as well as all three.
- [ ] T7: Roxygen for `confirm` and the corrected `@return`, then `devtools::document()`; update the four in-repo scripted callers (`test-program-management.R:56`, `test-audio-track-drop.R:151` among them) and the `\dontrun` example at `:195`.
- [ ] T8: `NEWS.md` entry naming the new argument, the refusal and the breaking change; `README.Rmd` line 50 plus `devtools::build_readme()`; `devtools::check()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: `cairn_validate`'s sizing tripwire fired at 8 criteria; the merge of the `set_program()`-only refusal criterion into the NAMESPACE-enumerated one removed a real overlap rather than shrinking to fit, leaving 7.
- 2026-09-05: criteria audit ran in full mode (user-facing tier), fresh [O] reader; fourteen findings over six drafted criteria, eleven fixed before writing (AC1's prior-directory-state axis and its vacuous empty-listing comparison, AC2's unpinned refusal position and unstripped message match, AC3's unpinned default plus its instrument-bound "passes to the seam" promise and its undefined file name for `set_program` itself, AC4's mock-dependent ask count and its unsatisfiable all-three-programs clause, AC5's single brace form and unstripped comparison, AC6's four unconstrained wrappers), three posed at the gate.
- 2026-09-05: plan gate chose refusing a non-interactive call over proceeding-with-disclosure, because one contract for "no one can be asked" is what D080 already bought at the installer and a second answer at a second site would need its own entry; the user waived the pre-1.0 deprecation cycle for the break this causes. Falsified by an unattended workflow that must register a location and cannot pass `confirm = FALSE`.
- 2026-09-05: plan gate chose showing the location as typed over showing it beside its `Sys.which()` resolution, because the file records the typed string and a prompt naming something else asks consent for a write that will not happen. Falsified by a report of a caller approving a bare program name and being surprised by which binary it later resolved to.
- 2026-09-05: plan gate chose folding the `:203` abort's class and `call` into this milestone over leaving it for the package-wide naming pass, because the front door it sits in is already being rewritten here and one site's event name is decided by D062 without the pass. Falsified by the naming pass settling on a name for this event other than `tidymedia_program_not_found`.

## Decisions

## Review
