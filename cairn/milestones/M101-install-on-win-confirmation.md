# M101: `install_on_win()` asks before it downloads and installs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m101-install-on-win-confirmation`

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

- [ ] AC1: In a session where `rlang::is_interactive()` is `FALSE` and `confirm`
      is left at its default, `install_on_win()` aborts with condition class
      `tidymedia_confirmation_unavailable` (asserted by `expect_error(class =)`)
      and a message naming `confirm = FALSE`; and
      `list.files(r, recursive = TRUE, all.files = TRUE)` and `list.dirs(r)`
      for `r = tm_redirect_data()` and `r = tm_redirect_config()$root` return
      values identical to those captured immediately before the call.
- [ ] AC2: With `tm_confirm()` mocked to decline, `install_on_win()` returns
      `FALSE`; mocked bindings for `utils::download.file()`,
      `archive::archive_extract()` and `set_program()` each record zero calls;
      and AC1's two directory snapshots are unchanged across the call.
- [ ] AC3: With `tm_confirm()` mocked to accept and `download.file()` mocked to
      return `0L`, `install_on_win(download_url = u, install_dir = d)` returns
      `TRUE`, and the mocks record exactly one `download.file()` whose `url` is
      `u`, one `archive_extract()` whose `dir` is `d`, and three `set_program()`
      calls whose `program` values are `"ffmpeg"`, `"ffprobe"`, `"ffplay"` and
      whose `location` values are the `bin/` paths beneath `d`.
- [ ] AC4: The `prompt` string `install_on_win()` hands `tm_confirm()` —
      captured by the mock before formatting, read under
      `withr::local_options(cli.width = 1000)` — contains as fixed substrings
      the resolved download URL, the resolved install directory, and every path
      in the set of `<program>_location.txt` files that AC3's `set_program()`
      record shows the call writes. Asserted three times: for a call naming
      neither argument, for one naming both, and for one whose `install_dir`
      contains a space and a `{` (M44).
- [ ] AC5: With `rlang::local_interactive()` in force, `confirm = FALSE`, and
      `tm_confirm()` mocked to abort if called, that mock records no call and
      the `download.file()` / `archive_extract()` / `set_program()` record is
      identical to AC3's.
- [ ] AC6: `man/install_on_win.Rd` names the `.7z` format its default
      `download_url` points at; `grep -ci 'zip file\|zip installer\|zip archive'`
      over that file reports 0; and the file documents a `confirm` parameter
      and a `@return` stating what a declined call returns.
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures and 0 errors, and
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
- [ ] T5: Roxygen at `R/program_management.R:256-277`: replace "zip" with the
      archive format actually defaulted to, document `confirm`, extend
      `@return`; `devtools::document()`.
- [ ] T6: `NEWS.md` entry for the behaviour change and the new argument.
- [ ] T7: Run the profile's verify and consistency-gate checks for AC7.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier) and returned 17 findings, all disposed here and none deferred. Fixed without a question: AC1's ungrammatical snapshot clause and its wrong `tm_redirect_config()` return shape (that helper returns a list, so `$root`), the unpinned condition class, `tm_confirm()`'s missing message slot, AC2's uncovered `dir.create()`, AC3's unpinned `download.file()` return, AC4's hand-listed config paths and its console-width and brace-form gaps, AC5's delegated assertions, AC6's `grep -c 'zip'` (which failed a correct "7zip" and passed a wrong "ZIP installer"), AC7's "Status: OK plus justified NOTEs" contradiction and its review-write-up clause (an instrument property under D-118), and Coverage's missing T1 on AC3 and AC5. Verified at the machine: under `rlang::local_interactive()`, `base::interactive()` stays `FALSE`, so `utils::menu()` refuses — AC2/AC3/AC5 name the mock rather than that helper, and T1 says why.
- 2026-09-02: plan gate chose a `confirm =` argument defaulting to its safe position over a session option carrying the consent, because consent is per-call and an option would let one `options()` call at the top of a script silently authorize every later install; falsified by a caller for whom the consent is genuinely a session-wide setting rather than a per-call answer.
- 2026-09-02: plan gate chose refusing a non-interactive default call over proceeding silently in one, because a scripted install is exactly the case the CRAN policy concern is about and the refusal names its own escape hatch; falsified by a report of the refusal breaking an unattended workflow that has no way to pass `confirm = FALSE`.
- 2026-09-02: T1 — `tm_confirm()` added: `rlang::is_interactive()` gates the ask, `utils::menu()` asks, no-selection (0) counts as a decline, and the refusal takes the caller's own bullets. Both branches tested with `utils::menu` mocked; each shown red against a planted defect (never-refuses; no-selection-as-approval).
- 2026-09-02: implement gate chose `confirm` last in the signature over first or a shorter `ask`, because an existing positional `install_on_win(url)` call keeps its meaning; escalation was offered on the tripwire and declined. Prompt shape chosen as an itemized list naming the URL, the directory, and each remembered-location file.
- 2026-09-02: T2/T3 — `confirm = TRUE` added last in the signature with `rlang::check_bool()`, the consent check sited above `dir.create()`, and a decline returning `FALSE`. The three registrations now loop over one `tm_install_registers` vector, so the prompt cannot promise a different set of writes from the one the call makes.
- 2026-09-02: T4 — AC1–AC5 tested with in-process mocks (`utils::download.file`, `archive::archive_extract`, `set_program`, `tm_confirm`); no binary, no network. Each shown red against a planted defect: consent below the first write, a prompt dropping the overwritten locations, a decline proceeding anyway, and `confirm = FALSE` still asking. The M098 default-install-dir test now passes `confirm = FALSE`, since it is about where the default resolves, not about consent.
- 2026-09-02: D079's letter reaches `confirm`'s default and its toggle exemption does not (both `TRUE` and `FALSE` are members of the set the argument ranges over, and `TRUE` is the on position, not the off one); D080 states that exemption rather than the plan ignoring the rule.

## Decisions

## Review
