# M101: `install_on_win()` asks before it downloads and installs

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m101-install-on-win-confirmation` / https://github.com/jmgirard/tidymedia/pull/105

## Goal

`install_on_win()` obtains the caller's consent — naming the archive it will
fetch, the directory it will unpack into, and the remembered locations it will
overwrite — before it creates, downloads, extracts, or writes anything, and
refuses rather than proceeds where no one can be asked.

## Scope

Surface tier: **user-facing** — an exported function gains an argument and
changes what an existing call does.

**In:** an internal `tm_confirm()` seam; a `confirm =` argument on
`install_on_win()`; the consent check sited above the first write; a refusal
in a non-interactive session; the roxygen correction from candidate (b); a
`NEWS.md` entry and D080.

**Out:**
- Verifying the downloaded archive (checksum or signature) and registering the
  three binaries only where the extraction actually produced them → candidate
  row, promote on a report of a corrupt or short download registering anyway.
- A confirmation on `set_program()`'s own config write → candidate row; it
  writes a caller-supplied path, not a download, and M097 already sanctioned
  the location.
- Installers for macOS or Linux → refused under D001/GP1; nothing has asked.
- Any change to the existing `dir.create`-failed and download-failed returns
  beyond adding the new refusal → stays as it is.

## Acceptance criteria

- [x] AC1: In a session where `rlang::is_interactive()` is `FALSE` and `confirm`
      is left at its default, `install_on_win()` aborts with condition class
      `tidymedia_confirmation_unavailable` (asserted by `expect_error(class =)`)
      and a message naming `confirm = FALSE`; and
      `list.files(r, recursive = TRUE, all.files = TRUE)` and `list.dirs(r)`
      for `r = tm_redirect_data()` and `r = tm_redirect_config()$root` return
      values identical to those captured immediately before the call.
- [x] AC2: With `tm_confirm()` mocked to decline, `install_on_win()` returns
      `FALSE`; mocked bindings for `utils::download.file()`,
      `archive::archive_extract()` and `set_program()` each record zero calls;
      and AC1's two directory snapshots are unchanged across the call.
- [x] AC3: With `tm_confirm()` mocked to accept and `download.file()` mocked to
      return `0L`, `install_on_win(download_url = u, install_dir = d)` returns
      `TRUE`, and the mocks record exactly one `download.file()` whose `url` is
      `u`, one `archive_extract()` whose `dir` is `d`, and three `set_program()`
      calls whose `program` values are `"ffmpeg"`, `"ffprobe"`, `"ffplay"` and
      whose `location` values are the `bin/` paths beneath `d`.
- [x] AC4: The `prompt` string `install_on_win()` hands `tm_confirm()` —
      captured by the mock before formatting, read under
      `withr::local_options(cli.width = 1000)` — contains as fixed substrings
      the resolved download URL, the resolved install directory, and every path
      in the set of `<program>_location.txt` files that AC3's `set_program()`
      record shows the call writes. Asserted three times: for a call naming
      neither argument, for one naming both, and for one whose `install_dir`
      contains a space and a `{` (M44).
- [x] AC5: With `rlang::local_interactive()` in force, `confirm = FALSE`, and
      `tm_confirm()` mocked to abort if called, that mock records no call and
      the `download.file()` / `archive_extract()` / `set_program()` record is
      identical to AC3's.
- [x] AC6: `man/install_on_win.Rd` names the `.7z` format its default
      `download_url` points at; `grep -ci 'zip file\|zip installer\|zip archive'`
      over that file reports 0; and the file documents a `confirm` parameter
      and a `@return` stating what a declined call returns.
- [x] AC7: `Rscript -e 'devtools::test()'` reports 0 failures and 0 errors, and
      `Rscript -e 'devtools::check()'` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T4
- AC2 → T1, T2, T4
- AC3 → T1, T2, T4
- AC4 → T3, T4
- AC5 → T1, T2, T4
- AC6 → T5
- AC7 → T6, T7

## Tasks

- [x] T1: Add internal `tm_confirm(prompt, ..., call)` to
      `R/program_management.R`: under `rlang::is_interactive()` ask via
      `utils::menu(c("Yes", "No"), title = prompt)` and return the answer as a
      logical; otherwise `cli::cli_abort()` with class
      `tidymedia_confirmation_unavailable` and caller-supplied bullets, so the
      seam carries no caller's argument name of its own. Unit-test both
      branches with `utils::menu` mocked (`.package = "utils"`);
      `rlang::local_interactive()` does not move `base::interactive()`, which
      is what `menu()` gates on, so the mock is the only route to the ask.
- [x] T2: Add `confirm = TRUE` to `install_on_win()`'s signature with
      `rlang::check_bool()` at the front door (D080), and call `tm_confirm()`
      above the `dir.create()` at `R/program_management.R:286`, so a declined
      call creates nothing; return `FALSE` on decline.
      (RB tripwire: irreversible-api)
- [x] T3: Compose the prompt from the resolved `download_url`, the resolved
      `install_dir`, and the `tm_config_file()` path of each program the call
      registers, routing every user value through a cli field (M44).
- [x] T4: Tests for AC1–AC5 in `tests/testthat/test-program-management.R`
      beside the M098 block at `:169`, reusing `tm_redirect_config()` /
      `tm_redirect_data()`. Every mock is in-process, so none carries a
      `skip_if` on a binary.
- [x] T5: Roxygen at `R/program_management.R:256-277`: replace "zip" with the
      archive format actually defaulted to, document `confirm`, extend
      `@return`; `devtools::document()`. Also `README.Rmd`'s Windows install
      step, which no longer describes the whole call; `devtools::build_readme()`.
- [x] T6: `NEWS.md` entry for the behaviour change and the new argument.
- [x] T7: Run the profile's verify and consistency-gate checks for AC7.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier) and returned 17 findings, all disposed here and none deferred. Fixed without a question: AC1's ungrammatical snapshot clause and its wrong `tm_redirect_config()` return shape (that helper returns a list, so `$root`), the unpinned condition class, `tm_confirm()`'s missing message slot, AC2's uncovered `dir.create()`, AC3's unpinned `download.file()` return, AC4's hand-listed config paths and its console-width and brace-form gaps, AC5's delegated assertions, AC6's `grep -c 'zip'` (which failed a correct "7zip" and passed a wrong "ZIP installer"), AC7's "Status: OK plus justified NOTEs" contradiction and its review-write-up clause (an instrument property under D-118), and Coverage's missing T1 on AC3 and AC5. Verified at the machine: under `rlang::local_interactive()`, `base::interactive()` stays `FALSE`, so `utils::menu()` refuses — AC2/AC3/AC5 name the mock rather than that helper, and T1 says why.
- 2026-09-02: plan gate chose a `confirm =` argument defaulting to its safe position over a session option carrying the consent, because consent is per-call and an option would let one `options()` call at the top of a script silently authorize every later install; falsified by a caller for whom the consent is genuinely a session-wide setting rather than a per-call answer.
- 2026-09-02: plan gate chose refusing a non-interactive default call over proceeding silently in one, because a scripted install is exactly the case the CRAN policy concern is about and the refusal names its own escape hatch; falsified by a report of the refusal breaking an unattended workflow that has no way to pass `confirm = FALSE`.
- 2026-09-02: T1 — `tm_confirm()` added: `rlang::is_interactive()` gates the ask, `utils::menu()` asks, no-selection (0) counts as a decline, and the refusal takes the caller's own bullets. Both branches tested with `utils::menu` mocked; each shown red against a planted defect (never-refuses; no-selection-as-approval).
- 2026-09-02: implement gate chose `confirm` last in the signature over first or a shorter `ask`, because an existing positional `install_on_win(url)` call keeps its meaning; escalation was offered on the tripwire and declined. Prompt shape chosen as an itemized list naming the URL, the directory, and each remembered-location file.
- 2026-09-02: T2/T3 — `confirm = TRUE` added last in the signature with `rlang::check_bool()`, the consent check sited above `dir.create()`, and a decline returning `FALSE`. The three registrations now loop over one `tm_install_registers` vector, so the prompt cannot promise a different set of writes from the one the call makes.
- 2026-09-02: T4 — AC1–AC5 tested with in-process mocks (`utils::download.file`, `archive::archive_extract`, `set_program`, `tm_confirm`); no binary, no network. Each shown red against a planted defect: consent below the first write, a prompt dropping the overwritten locations, a decline proceeding anyway, and `confirm = FALSE` still asking. The M098 default-install-dir test now passes `confirm = FALSE`, since it is about where the default resolves, not about consent.
- 2026-09-02: T5/T6 — roxygen now says archive rather than zip, names the default's `.7z` format, documents `confirm`, and extends `@return` with what a decline returns; `NEWS.md` gains the Configuration entry. `man/install_on_win.Rd` regenerated by `devtools::document()`.
- 2026-09-02: minor amendment (discovered sub-task, folded into T5): `README.Rmd`'s Windows install step said only to run the function, which after this change is no longer the whole instruction; it now says to confirm the prompt and what the prompt names. `README.md` rebuilt with `devtools::build_readme()`.
- 2026-09-02: T7 — `devtools::test()` 0 failures / 11427 passing / 18 skipped; `devtools::check()` Status: OK (0 errors, 0 warnings, 0 notes); `devtools::document()` no diff; `pkgdown::check_pkgdown()` no problems. Tasks complete, status to review.
- 2026-09-02: D079's letter reaches `confirm`'s default and its toggle exemption does not (both `TRUE` and `FALSE` are members of the set the argument ranges over, and `TRUE` is the on position, not the off one); D080 states that exemption rather than the plan ignoring the rule.

## Decisions

## Review

PR: https://github.com/jmgirard/tidymedia/pull/105 (draft). Reviewed at d25943b.

### Acceptance-criteria evidence
- AC1 verified 2026-09-02 at d25943b. Under `options(rlang_interactive = FALSE)` with `confirm` at its default, `install_on_win()` aborted with condition classes `tidymedia_confirmation_unavailable / rlang_error / error / condition`; the message read "Can't ask for confirmation in a non-interactive session." plus the bullet naming `confirm = FALSE` (`grepl(fixed = TRUE)` TRUE). `list.files(recursive, all.files)` and `list.dirs()` over the redirected data root and config root were `identical()` to the snapshot taken immediately before the call. Run twice: standalone at the console against redirected `R_USER_DATA_DIR`/`R_USER_CONFIG_DIR`, and as the suite's "refuses rather than assume consent" test.
- AC2 verified 2026-09-02. Suite test "a declined install creates, downloads, extracts and registers nothing": with `tm_confirm()` mocked to return `FALSE`, the call returned `FALSE`; the recorders for `utils::download.file()`, `archive::archive_extract()` and `set_program()` each held `list()` (zero calls); both directory snapshots `identical()` across the call.
- AC3 verified 2026-09-02. Suite test "an accepted install downloads, extracts and registers…": returned `TRUE`; exactly one `download.file()` whose `url` was the passed `u`, one `archive_extract()` whose `dir` was the passed `d` (and whose archive was the file the download mock had just written), and three `set_program()` calls with `program` `c("ffmpeg", "ffprobe", "ffplay")` and `location` `file.path(d, "bin", c("ffmpeg.exe", "ffprobe.exe", "ffplay.exe"))`.
- AC4 verified 2026-09-02. Suite test "the prompt names the archive, the directory, and every location it overwrites": three prompts captured by the `tm_confirm()` mock before formatting, under `withr::local_options(cli.width = 1000)` — one call naming neither argument, one naming both, one whose `install_dir` was `"an install {dir}"`. Each prompt contained, as `fixed = TRUE` substrings, its resolved URL, its resolved directory, and each config path read off the call's own `set_program()` record. The brace-bearing directory was reproduced verbatim rather than interpolated (M44). The `<program>_location.txt` filenames AC4 names were confirmed independently at the console: `basename(tm_config_file(p))` for the three programs returned `ffmpeg_location.txt`, `ffprobe_location.txt`, `ffplay_location.txt` (see P1 below, which is about the test's own derivation, not this criterion).
- AC5 verified 2026-09-02, asserted in AC3's block against AC3's own record. Under `rlang::local_interactive()` with `confirm = FALSE` and `tm_confirm()` mocked to `stop()` if reached: the mock recorded no prompt, and the `download.file()` / `archive_extract()` / `set_program()` records were each `identical()` to AC3's.
- AC6 verified 2026-09-02 by reading `man/install_on_win.Rd`. Line 12 names the default as "a \verb{.7z} archive"; `grep -ci 'zip file\|zip installer\|zip archive'` over the file printed `0`; `\item{confirm}` is documented at line 19; the `\value{}` block states "`FALSE` is also what a declined confirmation returns".
- AC7 verified 2026-09-02 on this branch. `Rscript -e 'devtools::test()'` → `[ FAIL 0 | WARN 10 | SKIP 18 | PASS 11427 ]`, exit 0. `Rscript -e 'devtools::check()'` → `Status: OK` (0 errors, 0 warnings, 0 notes).

### Consistency gate

- `cairn_validate.py` exit 0, all checks passed (2026-09-02).
- No principle changed (`Principles touched: —`), so `cairn_impact.py` did not run.
- `r-package` profile `consistency-gate`: `devtools::document()` produced no
  diff (only the milestone file was modified afterward); `NAMESPACE`, `man/`
  and `data/*.rda` unedited by hand; `README.md` rebuilt from `README.Rmd`;
  `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` carries a
  Configuration entry for the behaviour change and the new argument, with no
  milestone number in it; no new top-level files, so no `.Rbuildignore`
  entries were needed; `devtools::check()` `Status: OK`.

### Independent fresh-context review

Full three-lens fan-out (user-facing tier, executable diff). Nine findings.

**[O] diff-bug lens — 8 findings, ranked.**

- O1: `tm_confirm()` gates on `rlang::is_interactive()` but asks with
  `utils::menu()`, which gates on `base::interactive()`. Reproduced at the
  machine: `options(rlang_interactive = TRUE)` in an `Rscript` session gives
  `simpleError: menu() cannot be used non-interactively` — unclassed, with no
  `confirm = FALSE` hint — where the documented contract promises the classed
  refusal.
- O2: the refusal message never names the archive, directory or remembered
  locations the call would have touched; `prompt` is a promise never forced on
  that branch. A caller who sees only "pass `confirm = FALSE`" passes it
  without having been shown what it authorizes.
- O3: `tm_confirm()`'s `...` is used only in the abort, silently dropped on the
  ask branch — the signature promises more than the seam does.
- O4: a decline returns `FALSE`, the same value as the `dir.create` and
  download failures, and prints nothing; an existing wrapper reading `FALSE`
  as failure now misreports a deliberate "No".
- O5: `tm_install_prompt()`'s `line()` helper resolves values through
  `parent.frame()` inside `cli::format_inline()`; verified working at cli
  3.6.6, but a frame off would abort the whole prompt.
- O6: the prompt promises three overwrites the call may not make, since
  `set_program()` aborts where the extracted binary is absent.
- O7: `README.md` carries two lines of temp-library-path churn from
  `build_readme()`, unrelated to this change.
- O8: AC4's `cli.width = 1000` is not load-bearing — `format_inline()` does not
  wrap — and the test comment asserts a wrapping property it does not have.

**[S] blame-history lens — no findings.** Checked the branch against D080,
D079, M097 (the `set_ffmpeg`/`set_ffprobe`/`set_ffplay` calls became a loop
over `tm_install_registers` with identical programs, order, paths and
`forget_ffmpeg_capabilities()` count), M098 (the default-install-dir test's
`confirm = FALSE` is correctly scoped) and M44 (every caller value routed
through a cli field). Nothing undone, resurrected or contradicted.

**[S] prior-review lens — 1 finding.** Probe
`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so
no PR-thread walk; archived `## Review` sections were the evidence.

- P1: AC4's test derives its expected config paths by calling
  `tm_config_file()`, the same helper `tm_install_prompt()` calls — the shape
  M097's review flagged as F1. A wrong `tm_config_file()` moves both sides
  together. Partly fenced by the existing `startsWith(config_files,
  config$new)` assertion, but the `<program>_location.txt` filename AC4 names
  is never pinned.

### Triage
