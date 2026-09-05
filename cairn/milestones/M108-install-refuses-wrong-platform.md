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
- **Branch/PR:** `m108-install-refuses-wrong-platform` / https://github.com/jmgirard/tidymedia/pull/112

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

- [x] AC1: With the OS seam reporting any value other than `windows`,
      `install_on_win()` aborts with a condition of class
      `tidymedia_wrong_platform`, and the abort is raised above all four of
      the calls that spend something — the unverified-source
      `cli::cli_inform()`, the `tm_confirm()` prompt, the `dir.create()` of
      the install directory, and the first `tm_fetch()`. Verified by a test
      run under each of `darwin`, `linux` and a third non-Windows value that
      binds those four calls to stubs which abort if reached.
- [x] AC2: The message of the caught `tidymedia_wrong_platform` condition —
      its `conditionMessage()`, not any message emitted beside it — names the
      platform the seam reported, names `brew install ffmpeg` under `darwin`
      and `sudo apt-get install ffmpeg` under `linux`, and names
      `set_program()` under every value. Under `darwin` it does not name the
      `apt` route and under `linux` it does not name the `brew` route. The
      condition carries the seam's value in a `tm_platform` field (D062).
- [x] AC3: With the seam reporting `windows` the gate refuses nothing:
      `install_on_win()` reaches `tm_confirm()` under each of three argument
      shapes — every argument at its default, a caller-supplied `install_dir`,
      and a non-default `download_url` with an `archive_checksum`.
- [x] AC4: Unmocked, on each of the three CI runners, the seam equals the
      running host and the gate's real verdict follows it: `install_on_win()`
      aborts `tidymedia_wrong_platform` on `macos-latest` and
      `ubuntu-latest`, and reaches `tm_confirm()` on `windows-latest`.
- [x] AC5: `man/install_on_win.Rd`'s `\value` — the rendering of
      `?install_on_win`'s `@return` — names among the outcomes that abort both
      `tidymedia_wrong_platform` and `tidymedia_confirmation_unavailable`, and
      also every condition class named among the outcomes that abort in
      `git show master:man/install_on_win.Rd`'s `\value`; the number word
      introducing that list states the number of distinct classes the list
      names. Its `\details` states the call installs on Windows only.
      `NEWS.md` carries a bullet for the new refusal.
- [x] AC6: `devtools::check()` clean (0 errors / 0 warnings) and
      `devtools::test()` green.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T1, T4
- AC5 → T5, T7
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
- [x] T7: Rewrite the `@return` enumeration so it keeps every class it already
      named, adds `tidymedia_confirmation_unavailable` beside
      `tidymedia_wrong_platform`, and opens with a number word matching the
      list; run `devtools::document()`. Add a test asserting
      `install_on_win()` aborts `tidymedia_confirmation_unavailable` from its
      own frame when the seam reports `windows` and no one can be asked, so the
      newly documented outcome is pinned rather than recited.

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
- 2026-09-04: review gate directed all six actioned findings fixed. O2 (routeless message), O4 (uname word alone), O1 (siting untested), O5 (a stub never on the path), O6 (a false comment) landed; the O1 test is discriminated by hoisting the gate, which turns it red, and the O5 pair now runs a caller-named source with no digest so the unverified-source stub is reached. O3 is held: fixing it makes the `@return` count eight, which falsifies AC5's "reads seven rather than six" -- routed to the user as an amendment question.
- 2026-09-04: amendment return: AC5 — "its `@return` lists `tidymedia_wrong_platform` among the outcomes that abort and its count reads eight rather than six, `tidymedia_confirmation_unavailable` among them". The criterion pinned a count of seven; eight classed aborts are reachable, the enumeration having omitted `tidymedia_confirmation_unavailable` since before M108. User chose correcting the criterion over waiving it or shipping the under-count. Review stops here; the amendment is the only work convened.
- 2026-09-04: re-audit: AC5 (full) — returned four findings on the drafted replacement: F1 the count clause was self-referential, so a page listing only the two named classes would satisfy it while dropping the six already documented; F2 it presupposed a number word without requiring one; F3 it conflated the roxygen source with the rendered Rd and left the counted span open; F4 nothing binds the newly named class to reachability. F1-F3 were fixed by adopting the reader's alternative wording verbatim at the mini gate; F4 was disposed by adding T7's reachability test rather than accepting.
- 2026-09-04: amendment return: AC5 — "`man/install_on_win.Rd`'s `\value` — the rendering of `?install_on_win`'s `@return` — names among the outcomes that abort both `tidymedia_wrong_platform` and `tidymedia_confirmation_unavailable`, and also every condition class named among the outcomes that abort in `git show master:man/install_on_win.Rd`'s `\value`; the number word introducing that list states the number of distinct classes the list names." This supersedes the draft clause in this milestone's earlier amendment-return line of the same date, which was written before the wording was audited: one return, not two.
- 2026-09-04: T7: `@return` rewritten to keep all six classes it already named, add `tidymedia_confirmation_unavailable` beside `tidymedia_wrong_platform`, and open with "Eight"; `devtools::document()` re-rendered `man/install_on_win.Rd`. Added a test that `install_on_win()` aborts `tidymedia_confirmation_unavailable` from its own frame with the seam at `windows` and nothing mocked below the gate; renaming the class in `tm_confirm()` turns it and four others red. Suite 0 failures, 12251 passing. Status back to review.
- 2026-09-04: review pass 2 re-ran every criterion against head 1800936 after the AC5 amendment: `master` had not moved, all six verified (suite 688 passing over the two M108 files with 0 skips, `devtools::check()` 0/0/0, all ten CI checks green including `windows-latest`), AC5 ticked, `cairn_validate` 16/16 with no advisories fired, and the three-lens fan-out returned seven findings from the diff lens and none from the other two. Defect-return count 0; the AC5 amendment return stays on its own track.

## Decisions

- The one-platform installer surface, the gate's siting, and the allow-list: **D086** in `cairn/DECISIONS.md`.

## Review

### Pass 1 (2026-09-04, branch head f62b082, PR #112)

#### Acceptance-criteria evidence

- **AC1 — verified.** `testthat::test_file("tests/testthat/test-program-management.R")`:
  0 failures, 667 passing. The AC1 test loops the seam over `darwin`, `linux`
  and `freebsd` with `tm_forbid_spending()` binding all four spending calls
  (`tm_confirm`, `tm_fetch`, `cli::cli_inform`, base `dir.create`) to stubs that
  abort by name; each iteration reaches `tidymedia_wrong_platform` instead.
  Discrimination checked by mutation: replacing the gate predicate with `FALSE`
  turned the suite red at 5 failures, three of them naming a reached stub
  (`reached dir.create()`, `reached tm_confirm()`) rather than the class.
- **AC2 — verified.** Messages captured by running the gate under each seam
  value and reading `conditionMessage()` off the caught condition. Under
  `darwin`: names `darwin`, `brew install ffmpeg`, `set_program()`, and not the
  apt route. Under `linux`: names `linux`, `sudo apt-get install ffmpeg`,
  `set_program()`, and not the brew route. Under `freebsd`, `sunos` and `unix`:
  names the platform and `set_program()`, no package-manager line. Every case
  carried `tm_platform` equal to the seam's value, class
  `tidymedia_wrong_platform`.
- **AC3 — verified.** The Windows control passes in the same run: with the seam
  at `windows`, all three argument shapes (defaults, caller-supplied
  `install_dir`, non-default `download_url` with `archive_checksum`) reach the
  `tm_confirm()` stub and return `FALSE`.
- **AC4 — verified on all three runners.** `test-install-platform.R` locally
  (darwin): 4 passing, 0 skips, refusal branch exercised with no stub between
  the call and a download. On PR #112 every CI leg is green:
  `macos-latest (release)`, `windows-latest (release)`, and `ubuntu-latest` at
  release / devel / oldrel-1 / 4.1.0 — the file skips nothing, so each leg ran
  its host's branch. The Windows leg is the one only CI can exercise.
- **AC5 — verified.** `man/install_on_win.Rd` `\value` reads "Seven other
  outcomes abort" and lists `tidymedia_wrong_platform` first; `\details` opens
  "This call installs on Windows only." and names both package-manager routes
  and `set_program()`. `NEWS.md` carries a Configuration bullet for the refusal.
- **AC6 — verified.** `devtools::check()`: 0 errors, 0 warnings, 0 notes
  (20m 37s). `devtools::test()` green via the suite runs above.

#### Consistency gate

`cairn_validate.py` exit 0 — 16 PASS, 7 advisories all OK, no `release window`
flag. No `DESIGN.md` principle changed, so `cairn_impact.py` did not apply.
Toolchain slot: `devtools::document()` produced no diff in `man/` or
`NAMESPACE`; `pkgdown::check_pkgdown()` reports no problems; `README.Rmd` and
`README.md` are untouched by the branch and in sync; `NEWS.md` carries the
entry; the branch adds no new top-level file; `devtools::check()` clean.

#### Independent review — three fresh-context lenses

Surface tier user-facing, so the full three-lens fan-out ran. Findings below
verbatim in substance, most severe first per lens, each with its disposition.

**Diff-bug lens (Opus).** Nine findings.

- **O1 — D086's siting rule is asserted by no test, and its incidental coverage
  vanishes on the Windows leg.** Hoisting the gate above `rlang::check_bool()`
  in a scratch copy produced 9 failures, all from M103's exit census, none from
  M108's own tests; those cases mock no seam, so on `windows-latest` they pass
  and a hoisted gate ships green. Reproduced. Disposition: FIXED on the branch. `test-program-management.R:2818` now pins the gate below the four argument checks by asserting each malformed argument reports its own error rather than the platform refusal; hoisting the gate turns it red.
- **O2 — the refusal message loses its antecedent on a platform with no
  route.** On `freebsd`/`sunos`/`unix` the "Install FFmpeg with …" bullet is
  dropped but the next still opens "Then point tidymedia at it …" — "then"
  names a step that is not there and "it" refers to nothing. Confirmed against
  the messages captured for AC2. Disposition: FIXED on the branch. The advice bullets are written as a pair per platform, so a routeless platform gets one self-contained line naming `set_program()` and no dangling "Then".
- **O3 — `@return`'s enumeration is incomplete and the diff re-counted it
  without noticing.** `install_on_win()` also aborts
  `tidymedia_confirmation_unavailable` (via `tm_confirm()`), so eight classed
  aborts are reachable, not seven. Confirmed by running the call on a
  `windows` seam non-interactively: class
  `tidymedia_confirmation_unavailable`. The omission predates M108, but the
  diff rewrote the sentence, so the wrong count is newly asserted.
  Disposition: AMENDMENT RETURN. The fix makes the count eight, which falsified AC5's "reads seven rather than six". Routed to the user, who chose correcting the criterion; AC5 was amended and T7 rewrote the enumeration. Re-verified in pass 2.
- **O4 — the message names the uname word where every other surface names the
  OS.** A macOS caller reads "running on darwin" while NEWS, `@details` and
  the Rd all say "macOS". AC2-compliant, but nothing links the two.
  Disposition: FIXED on the branch. The message names the OS beside the uname word where `tm_os_names` has one — `darwin (macOS)`, `sunos (Solaris)`.
- **O5 — the `cli_inform` stub in `tm_forbid_spending()` is never on the
  path.** The default `download_url` has a sidecar, so
  `is.null(archive_checksum) && is.null(sidecar_url)` is FALSE and the
  unverified-source notice is skipped; three of the four stubs discriminate,
  not four. Confirmed at `R/program_management.R:989`. The gate is still
  proven above every cost, being proven above `tm_confirm()`, which comes
  first. Disposition: FIXED on the branch. The AC1 test now runs a caller-named source with no digest as well as the default, so the unverified-source notice is on the path; re-measured in pass 2, all four stubs discriminate across the pair.
- **O6 — `test-install-platform.R`'s comment misdescribes a failed gate.** It
  claims a gate that did not fire "would fail this test by trying to install
  FFmpeg"; under testthat `rlang::is_interactive()` is FALSE, so the real
  `tm_confirm()` aborts first and nothing is downloaded. Confirmed by this
  review's own mutation run, which failed with "Can't ask for confirmation in
  a non-interactive session." Disposition: FIXED on the branch. The comment now says a gate that did not fire would fail on the class, the real `tm_confirm()` aborting first in a non-interactive session.
- **O7 — the seam's host-binding assertion is duplicated verbatim** across
  `test-install-platform.R` and `test-program-management.R`. Disposition: REJECTED. The duplication is deliberate: `test-install-platform.R` is the one file that skips nothing on any runner, and its own copy of the host-binding assertion is what makes AC4 self-contained there.
- **O8 — `tm_os()` has no guard for a non-NULL `Sys.info()` lacking
  `sysname`**, which would raise an unclassed subscript error. Not reachable
  on any real platform. Disposition: REJECTED. Not reachable on any real platform — where `Sys.info()` is non-NULL it carries `sysname`.
- **O9 — one roxygen line at 85 columns** (`R/program_management.R:901`), an
  unwrapped straggler from the edit. Disposition: FIXED on the branch. The line is wrapped.

What the lens verified as correct: the gate sits above both argument defaults,
the unverified-source notice, `tm_confirm()`, `dir.create()` and the first
`tm_fetch()`; mutating the allow-list to a deny-list turns the `freebsd`
iteration red, so the allow-list is discriminated; `tm_platform` follows D062;
the route inversion is asserted against `conditionMessage()`; `man/` matches
the roxygen; AC4's file skips nothing and the CI matrix carries all three
runners; `tm_local_windows()` holds only the seam and only below the gate.

**Blame-history lens (Sonnet).** No defects. Verified the gate's siting matches
D036/D043's ordering precedent, that the M102–M105 hardening invariants are
untouched, that the new `tm_os()` mocking in `tm_mock_install()` defeats no
prior milestone's assertion, and that no recorded decision is contradicted. It
raised the same 85-column roxygen line as O9.

**Prior-review lens (Sonnet).** No findings. Checked the archived `## Review`
sections of M097, M098 and M101–M105 against the diff — M101's gate/ask
predicate mismatch, M102's exit census, M103's symlink escape, M104's
`Sys.which()`/`file.info()` disagreement, M105's undiscriminated intersects —
and none is reintroduced; M104's regression test still runs below the gate via
`tm_local_windows()`. The GitHub inline-review probe returned empty, so the PR
walk was correctly skipped.

### Pass 2 (2026-09-04, branch head 1800936, PR #112)

Re-entered at step 1 after the AC5 amendment and T7. `master` had not moved
(branch 0 behind, 10 ahead), so no merge was needed and pass 1's tree is a
strict ancestor of this one. Every criterion is re-executed here against the
current head; pass 1's evidence stands only for the tree it names.

#### Acceptance-criteria evidence

- **AC1 — verified.** `testthat::test_local(filter = "program-management|
  install-platform")`: FAIL 0, WARN 0, SKIP 0, PASS 688. The AC1 test loops the
  seam over `darwin`, `linux` and `freebsd` and, within each, over two source
  shapes — the package default and a caller-named `download_url` with no digest
  — with `tm_forbid_spending()` binding all four spending calls (`tm_confirm`,
  `tm_fetch`, `cli::cli_inform`, base `dir.create`) to stubs that abort by
  name. Every iteration reaches `tidymedia_wrong_platform` instead.
  Discrimination measured by mutation: replacing the gate predicate with
  `FALSE` turned the two files red at 5 failures — `test-install-platform.R:41`,
  the two M103 exit-census cases at `test-program-management.R:1742` and
  `:1759`, and the two M108 cases at `:2809` and `:2851` — the M108 ones failing
  with `reached tm_confirm()`, a stub name rather than a class. The
  `cli::cli_inform` stub was measured separately to be on the path (O5's fix):
  under the same mutation, the caller-named source with no digest fails with
  `reached cli::cli_inform()`, so all four stubs discriminate across the pair.
- **AC2 — verified.** Message text read off the caught condition
  (`conditionMessage()`) with the seam driven through five values. `darwin`:
  "This session is running on darwin (macOS)." plus "Install FFmpeg with
  `brew install ffmpeg`." — the apt route absent. `linux`: "running on linux"
  plus "Install FFmpeg with `sudo apt-get install ffmpeg`." — the brew route
  absent. `freebsd`, `sunos`, `unix`: the platform named, one advice line
  naming `set_program()`, neither package manager. Every value named
  `set_program()`; every condition carried class `tidymedia_wrong_platform` and
  `tm_platform` equal to the seam's value.
- **AC3 — verified.** The Windows control passes in the same green run: seam at
  `windows`, all three argument shapes (defaults; caller-supplied
  `install_dir`; non-default `download_url` with `archive_checksum`) reach the
  `tm_confirm()` stub and return `FALSE`.
- **AC4 — verified on all three runners.** `test-install-platform.R` runs 4
  expectations with 0 skips and asserts the seam equals
  `tolower(Sys.info()[["sysname"]])`; locally (darwin) the refusal branch runs
  unmocked, with nothing between the call and a download but the gate. On PR
  #112 every leg is green — see the merge-gate CI line below for the run this
  head produced.
- **AC6 — verified.** `devtools::check()` on head 1800936: Status OK,
  0 errors / 0 warnings / 0 notes, 17m 0.9s. `devtools::test()` green (counts
  in the work log). All ten CI checks on PR #112 pass at this head, including
  `windows-latest (release)` (14m 35s), `macos-latest (release)` (9m 16s) and
  the four `ubuntu-latest` legs (release / devel / oldrel-1 / 4.1.0) — which is
  also AC4's per-runner evidence, `test-install-platform.R` skipping nothing on
  any of them.
- **AC5 — verified.** Read mechanically out of `man/install_on_win.Rd`'s
  `\value` block and compared with `git show master:man/install_on_win.Rd`'s.
  Master's `\value` names 6 distinct classes; HEAD's names 8 — the same 6 with
  nothing dropped, plus `tidymedia_wrong_platform` and
  `tidymedia_confirmation_unavailable`. The number word introducing the list is
  "Eight", matching the 8 distinct classes the list names. `\details` opens
  "This call installs on Windows only." `NEWS.md` carries a Configuration
  bullet for the refusal naming `tidymedia_wrong_platform` and both
  package-manager routes. T7's reachability test holds the newly documented
  class true of the function rather than recited: with the seam at `windows`
  and nothing mocked below the gate, `install_on_win()` aborts
  `tidymedia_confirmation_unavailable` with `conditionCall()` naming
  `install_on_win`.

#### Consistency gate

Universal: `cairn_validate.py` exit 0 — 16 PASS, 7 advisories OK, the `release
window` advisory not fired. The branch changes no `DESIGN.md` principle, so
`cairn_impact.py` does not apply. Toolchain (`r-package` profile's
`consistency-gate` slot): `devtools::document()` left `man/` and `NAMESPACE`
unchanged; `pkgdown::check_pkgdown()` reports no problems; `README.Rmd` and
`README.md` are untouched by the branch; `NEWS.md` carries the entry for the
user-visible change; the branch adds no top-level file and no exported object,
so no `.Rbuildignore` or `_pkgdown.yml` row is owed; `devtools::check()` clean
(see AC6).

#### Independent review — three fresh-context lenses

Surface tier user-facing, so the full three-lens fan-out ran again on the
current head, each lens with its own evidence base. Findings verbatim in
substance, most severe first per lens, each with its disposition.

**Diff-bug lens (Opus).** Eight findings.

- **P1 — the friendly-OS-name branch is new code that no test discriminates.**
  `tm_os_names` (`R/program_management.R:352-356`) and the `is.na(known)` fork
  in the abort message (`:964`, `:983-987`) were added to answer pass 1's O4,
  but nothing asserts the parenthetical: the AC2 test checks only the uname
  word, `set_program()` and the route inversion, and `test-install-platform.R`
  checks only `host`. Deleting `tm_os_names` and collapsing the message to
  "This session is running on {platform}." leaves the whole suite green — a
  feature-removal that passes. Confirmed independently: `grep -rn
  "macOS\|Solaris\|tm_os_names" tests/` finds no hit in any platform-gate
  test. Coverage rather than criterion failure — AC2 does not require the
  parenthetical. Disposition: TBD at gate.
- **P2 — two of `tm_forbid_spending()`'s four stubs can never fire, so AC1's
  "binds those four calls to stubs which abort if reached" is nominal for half
  of them.** On the default-source path `tm_confirm`'s stub is the first
  spending call reached, and on the caller-named-source path
  `cli::cli_inform`'s is; `dir.create` and `tm_fetch` both sit strictly below
  `tm_confirm` (`R/program_management.R:1043`, `:1063`, `:1075`), so no
  mutation of the gate can reach either stub. The gate's position above the
  first write and the first fetch follows from source order, not from an
  assertion. Confirmed by reading the call order. Disposition: TBD at gate.
- **P3 — this milestone's own AC5 evidence was false of the current head** and
  every pass-1 finding was still recorded "Disposition: TBD at gate" while the
  work log said the gate had directed them fixed. Disposition: ALREADY FIXED
  in this pass, independently of the finding — pass 1's block is now scoped to
  the head it names, its dispositions are recorded, and AC5 is re-verified
  above against head 1800936. (Reported by the lens as two findings, 3 and 4;
  merged here, being one record defect.)
- **P4 — the seam's host-binding assertion is duplicated verbatim** at
  `test-install-platform.R:14` and `test-program-management.R:2707`. Raised in
  pass 1 as O7. Disposition: TBD at gate.
- **P5 — `tm_os()` raises an unclassed subscript error if `Sys.info()` is
  non-NULL but lacks `sysname`** (`R/program_management.R:319-324`). Not
  reachable on any platform R runs on. Raised in pass 1 as O8. Disposition:
  TBD at gate.
- **P6 — `other <- setdiff(unlist(routes), character())`**
  (`test-program-management.R:2846`) is a no-op wrapper around
  `unlist(routes)` that only drops names, but reads as if it excluded
  something. Disposition: TBD at gate.
- **P7 — `tm_install_routes` and `tm_os_names` sit under the
  `# install_on_win() ---` section header** (`R/program_management.R:342`,
  `:352`) while the seam producing the keys they are indexed by lives under
  `# Platform ---` (`:301-324`). Organizational nit. Disposition: TBD at gate.

What the lens verified as correct: the gate sits below all four argument checks
and above both argument defaults, the unverified-source notice, `tm_confirm()`,
`dir.create()` and the first `tm_fetch()`; both message shapes render as AC2
requires and O2's dangling antecedent is genuinely fixed; the allow-list is an
allow-list, discriminated by the `freebsd` iteration; `tm_platform` follows
D062; `tm_confirm()` already passes `call = rlang::caller_env()`, so T7's
`conditionCall` assertion is meaningful; the `\value` list names eight distinct
classes including all six from master and the "except the last two" clause
still points at the same pair; `man/` matches the roxygen; AC4's file skips
nothing and its Windows branch spends no bandwidth; `tm_local_windows()` holds
only the seam and cannot leak between M103 census cases; the D086 siting test
turns red on all four argument shapes when the gate is hoisted, so O1 is fixed;
NEWS's `.exe`-only claim is true of the code and README contradicts nothing;
no added line exceeds 80 columns, so O9 is fixed; no new dependency floor.

**Blame-history lens (Sonnet).** No defects. `tm_os()` is called exactly once
in `R/program_management.R` — the new gate — and nothing else in
`install_on_win()` branches on it, so `tm_local_windows()` holding the seam at
`windows` defeats no assertion M102-M105 made: the `.exe`-only path logic was
already Windows-only by construction. M101's confirmation-refusal test still
asserts what M101 intended; M102's classed-exit census invariant is respected
by the new classed abort; M103's symlink-escape and M104's
`Sys.which()`/`file.info()` fixes are untouched by a seam that does no path
handling. D062's field naming and D036/D043's ordering precedent are applied as
D086 records them. It re-raised the pass-1 O7 and O8 shapes, already carried
above as P4 and P5.

**Prior-review lens (Sonnet).** No findings. The GitHub inline-review probe
(`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned `[]` —
no inline review comments at all, bot or human — so the per-PR walk was
correctly skipped. On the primary surface it read the archived review records
of M101-M105 and M107 plus `LESSONS.md` against the touched files and found
nothing reintroduced: M101's `interactive()` predicate mismatch, M102's
registration-reads-disk defect, M103's symlink escape, M104's `~`-relative path
disagreement and M105's extraction messaging are all untouched by the platform
gate, and M107's check-ordering precedent is the one D086 invokes rather than
one it contradicts. No archived lesson addresses `Sys.info()`,
`.Platform$OS.type`, or single-bracket named-vector lookup, so there is no
precedent here to regress against.
