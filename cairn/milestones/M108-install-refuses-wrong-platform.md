<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M108: `install_on_win()` refuses on a platform it cannot install for, before it spends anything

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — it changes an exported function's refusal behaviour, its message, and its documented return contract
- **Branch/PR:** `m108-install-refuses-wrong-platform`

## Goal

Make D084's claim that Windows "is the only platform this function runs on"
true at the top of `install_on_win()`, and point a caller elsewhere at the
route their own platform has.

## Scope

**In:** an OS seam whose unmocked value derives from the running host; a
platform gate in `install_on_win()` sited below its argument checks and above
every cost, refusing anything that is not Windows with a classed condition
that names the platform and its route; the roxygen and NEWS updates.

**Out:** a downloader for macOS or Linux → candidate row (GP1: `brew` and
`apt` are one line each and keep FFmpeg updated; each downloader would need
its own source, digest format, and arch matrix). **Out:** naming
`install_on_win()` from `find_program()`'s not-found warning, and a
dispatching `install_ffmpeg()` → the same candidate row. **Out:** the gate
sited above the argument checks → rejected at this gate, work log records it.

## Acceptance criteria

- [ ] AC1: With the OS seam reporting any value other than `windows`,
      `install_on_win()` aborts with a condition of class
      `tidymedia_wrong_platform`, and the abort is raised above all four of
      the calls that spend something — the unverified-source
      `cli::cli_inform()`, the `tm_confirm()` prompt, the `dir.create()` of
      the install directory, and the first `tm_fetch()`. Verified by a test
      run under each of `darwin`, `linux` and a third non-Windows value that
      binds those four calls to stubs which abort if reached.
- [ ] AC2: The message of the caught `tidymedia_wrong_platform` condition —
      its `conditionMessage()`, not any message emitted beside it — names the
      platform the seam reported, names `brew install ffmpeg` under `darwin`
      and `sudo apt-get install ffmpeg` under `linux`, and names
      `set_program()` under every value. Under `darwin` it does not name the
      `apt` route and under `linux` it does not name the `brew` route. The
      condition carries the seam's value in a `tm_platform` field (D062).
- [ ] AC3: With the seam reporting `windows` the gate refuses nothing:
      `install_on_win()` reaches `tm_confirm()` under each of three argument
      shapes — every argument at its default, a caller-supplied `install_dir`,
      and a non-default `download_url` with an `archive_checksum`.
- [ ] AC4: Unmocked, on each of the three CI runners, the seam equals the
      running host and the gate's real verdict follows it: `install_on_win()`
      aborts `tidymedia_wrong_platform` on `macos-latest` and
      `ubuntu-latest`, and reaches `tm_confirm()` on `windows-latest`.
- [ ] AC5: `?install_on_win`'s `@return` lists `tidymedia_wrong_platform`
      among the outcomes that abort and its count reads seven rather than six;
      its `@details` states the call installs on Windows only. `NEWS.md`
      carries a bullet for the new refusal.
- [ ] AC6: `devtools::check()` clean (0 errors / 0 warnings) and
      `devtools::test()` green.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: Add the OS seam — a `tm_os()` internal returning a lowercase
      normalized name derived from `Sys.info()[["sysname"]]` with
      `.Platform$OS.type` as its fallback — beside the other seams in
      `R/program_management.R`, with unit tests pinning its vocabulary.
- [x] T2: Write the failing tests for AC1–AC3 in
      `tests/testthat/test-program-management.R`, mocking `tm_os()` and
      stubbing the four spending calls.
- [x] T3: Add the gate to `install_on_win()` immediately below
      `check_sha256()` (`R/program_management.R:891`) and above the
      `download_url` default, raising `tidymedia_wrong_platform` with the
      `tm_platform` field.
- [x] T4: Add the unmocked per-runner test for AC4, skipping nothing and
      branching on the real host rather than on a mock.
- [x] T5: Update the roxygen block (`@details`, `@return`, `@seealso`), run
      `devtools::document()`, add the `NEWS.md` bullet.
- [x] T6: Append the D-entry recording that the installer surface stays one
      platform and where the gate sits; run `devtools::check()` and the suite.

## Work log

- 2026-09-04: created by /milestone-plan.
- 2026-09-04: criteria audit ran in FULL mode (surface tier user-facing) and returned 11 findings: F1 unbounded "any of the four calls that spend", F2 a deny-list gate satisfying the criteria while FreeBSD still downloads, F3 the seam's vocabulary unfixed, F4 the message unbound to the condition, F5 no inversion between the two platform routes, F6 AC3 at one point of the argument space, F7 the `@return` count left at six, F8 a README clause already true, F9 nothing binding the seam to the real host, F10 the gate's position against the argument checks, F11 D062's `tm_` prefix unpinned. Ten were fixed before the gate and reported in chat; F10 was posed as a gate question. No criterion was changed by the gate's answers.
- 2026-09-04: plan gate chose no macOS or Linux downloader over adding one or both because `brew` and `apt` are a one-line install that also keeps FFmpeg updated, while each downloader carries its own source, digest format (johnvansickle publishes `.md5`, not the SHA-256 D081 bought), arch matrix, and the hardening arc M102-M105 spent four milestones on for one installer; falsified by a report from a macOS or Linux user for whom the package manager route is unavailable or insufficient.
- 2026-09-04: plan gate chose siting the gate BELOW the argument checks over above them because D043 and D036 put a cheap value refusal above an availability check and an argument mistake is worth reporting either way, and the gate still sits above every cost; falsified by a report of a non-Windows caller confused at being asked to fix an argument for a call that cannot work on their machine.
- 2026-09-04: T1: `tm_os()` added beside the install seams, reading `Sys.info()[["sysname"]]` lowercased with `.Platform$OS.type` as the fallback; both sources are arguments so the fallback branch is reachable by a test. Six unit tests pin the vocabulary over the five uname names and bind the seam to the running host; a planted constant-returning seam turns seven of their expectations red. Suite 0 failures, 12196 passing.
- 2026-09-04: T2: the AC1-AC3 tests written and failing for the right reason -- with the seam at `darwin`, `linux` and `freebsd` the call reaches the `tm_confirm()` stub, there being no gate above it yet. The four spending calls are stubbed by one helper (`tm_confirm`, `tm_fetch`, `cli::cli_inform` and base `dir.create`, each aborting with its own name). AC3's Windows control already passes, as a control should. Committed red; T3 turns it green.
- 2026-09-04: T3: the gate added below `check_sha256()` and above the `download_url` default, refusing anything the seam does not report as `windows` with `tidymedia_wrong_platform` carrying `tm_platform`; the two routes live in `tm_install_routes`, looked up single-bracket so an unnamed platform gives NA and gets no package-manager line. With the gate short-circuited the four AC1-AC3 tests and M103's two directory-removal tests go red.
- 2026-09-04: T3: the gate broke 51 existing `install_on_win()` tests, which run on a non-Windows developer host. Added `tm_local_windows()` and called it from `tm_mock_install()` plus the two tests that mock no install; the seam is held at `windows` and nothing else is defeated. M103's AC3 exit census gained a `tidymedia_wrong_platform #1` case, the new exit being a `cli_abort()` above the unpack, so the new refusal is also held to creating no directory.
- 2026-09-04: T3: `tm_forbid_spending()` stubs base `dir.create()` only under `writes = TRUE`: a base-namespace stub is also what waldo uses to build a diff, so any `expect_identical()` under it dies in testthat's reporter. AC1's test, which compares nothing, carries the four-stub claim; AC2's runs on the other three.
- 2026-09-04: T4: `tests/testthat/test-install-platform.R` added -- two tests, no skips, branching on `Sys.info()[["sysname"]]` rather than on a mock. The non-Windows branch has no stub between the call and a download, so a gate that did not fire would fail it by installing FFmpeg. Locally (darwin) the refusal branch runs and passes; the Windows branch is only exercisable on the `windows-latest` CI leg, and its expectations match AC3's mocked control.
- 2026-09-04: T5: `@details` gained a Windows-only paragraph naming the two package-manager routes and `set_program()`; `@return` reads seven rather than six and lists `tidymedia_wrong_platform` first, so its "the last two" clause still points at the same two conditions. `@seealso` already named `set_program()` and is unchanged. `devtools::document()` rewrote `man/install_on_win.Rd`; `NEWS.md` gained a Configuration bullet. Suite 0 failures, 12234 passing -- one run in this task reported a single failure in an FFmpeg execution test that did not reproduce, its console carrying "Interrupted system call".
- 2026-09-04: T6: D086 appended, recording the one-platform installer surface, the gate's siting below the argument checks and above every cost, why the gate is an allow-list, and why the seam has an unmocked test of its own. `devtools::check()` 0 errors / 0 warnings / 0 notes; suite 0 failures.
- 2026-09-04: implement gate chose naming only `set_program()` on a platform that is neither Windows, macOS nor Linux over adding a generic package-manager line, and chose repeating the Homebrew and apt routes in `?install_on_win`'s Details over a Windows-only sentence.
- 2026-09-04: plan gate chose an unmocked per-runner assertion over mocked coverage plus a seam unit test because every other criterion runs through a mock, so a seam never wired to the host would satisfy all of them and ship broken for the one platform the function serves (audit F9); falsified by the three tests proving flaky on a runner for a reason that is not the seam.

## Decisions

- The one-platform installer surface, the gate's siting, and the allow-list: **D086** in `cairn/DECISIONS.md`.

## Review
