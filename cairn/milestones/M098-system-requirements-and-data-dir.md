<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M098: DESCRIPTION declares the tools the package interfaces, and the data dir follows policy

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m098-system-requirements-and-data-dir` / https://github.com/jmgirard/tidymedia/pull/103

## Goal

Declare FFmpeg and MediaInfo in `DESCRIPTION`'s `SystemRequirements`, and move
`install_on_win()`'s default install directory to `tools::R_user_dir()`.

## Scope

Surface tier: **user-facing** — it changes the default value of an exported
function's argument and the metadata an installer reads.

**In:** the `SystemRequirements` field; `install_on_win()`'s default
`install_dir` and its `@param` text.

**Out:**
- `install_on_win()`'s download posture — it fetches a 7z from a third-party URL
  and installs it without confirmation, which is what a CRAN reviewer is most
  likely to ask about. Declined at this gate as a separate, larger change to an
  exported function's behavior → ROADMAP candidate row.
- The config directory → M097 (independent; either may land first).
- The release itself → the standing `CRAN readiness` ROADMAP candidate row.

## Acceptance criteria

- [x] AC1 `DESCRIPTION` carries a `SystemRequirements` field naming FFmpeg and
      MediaInfo, each with its project URL.
- [x] AC2 `install_on_win()`'s own default — the value `install_dir` takes when
      the caller passes none — is the `ffmpeg` subdirectory of
      `tools::R_user_dir("tidymedia", "data")`. The subdirectory is preserved,
      not dropped: `archive_extract(strip_components = 1)` unpacks into it and
      the function registers `bin/ffmpeg.exe` beneath it. Asserted by a test
      that redirects `R_USER_DATA_DIR` with `withr::local_envvar()` and reaches
      the function's own default resolution while performing no download.
- [x] AC3 `install_on_win()`'s `@param install_dir` text names the new default
      location, replacing "the user data directory".
- [x] AC4 `devtools::test()` clean, `devtools::document()` produces no diff,
      `devtools::check()` reports 0 errors and 0 warnings with every NOTE
      justified (PROFILE `verify` and `consistency-gate` slots). A URL-check
      NOTE on the new `SystemRequirements` URLs is justified by naming it.
- [x] AC5 `NEWS.md` states the new default install location.

## Tasks

1. Add `SystemRequirements` to `DESCRIPTION`.
2. Extract `install_on_win()`'s default-directory resolution so a test can reach
   it without downloading; move it to `R_user_dir("tidymedia", "data")` keeping
   the `ffmpeg` subdirectory.
3. Write AC2's test under a redirected `R_USER_DATA_DIR`.
4. Update the `@param install_dir` roxygen text; add the `NEWS.md` entry.
5. Run `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T4

## Work log
<!-- owner: implement/review -->

- 2026-08-31 plan: criteria audit ran in FULL mode (surface tier user-facing), fresh-context [O] reader, over the wording split out of M097's draft. It returned two findings on this material: the draft target state was ambiguous between the data dir exactly and today's `ffmpeg` subdirectory (fixed — the subdirectory is preserved, since `archive_extract(strip_components = 1)` unpacks into it), and the stated verification named mocking `tools::R_user_dir()`, which is a locked base-priority namespace testthat cannot mock (fixed — `withr::local_envvar(R_USER_DATA_DIR = )`, verified working). It also flagged that AC2 bound a helper rather than the exported function's own default; AC2 now names the default.
- 2026-08-31 plan: alternative rejected — hardening `install_on_win()`'s download posture in this milestone (interactive confirmation before fetching third-party software). Lost at the question gate as a larger change to an exported function's behavior; it became a ROADMAP candidate row. Falsified by a CRAN reviewer raising it on submission.
- 2026-09-01 implement (T1): `DESCRIPTION` gained `SystemRequirements: FFmpeg (https://ffmpeg.org/), MediaInfo (https://mediaarea.net/en/MediaInfo)`, wrapped over two lines and verified to parse back as one field via `read.dcf()`. Both names are already in `inst/WORDLIST`. Suite clean (0 failures, 18 skipped). AC1 met.
- 2026-09-01 implement (T2, T3): the default install directory moved to `tm_install_dir()` = `file.path(tools::R_user_dir("tidymedia", "data"), "ffmpeg")`, replacing the `rappdirs::user_data_dir()` expression inline in `install_on_win()`'s `NULL` branch; `rappdirs` stays in `Imports` for `tm_legacy_config_dir()`. AC2's test calls the function with no `install_dir` under a `withr::local_envvar(R_USER_DATA_DIR = )` temp root and a `file://` URL naming a file that does not exist, so the default resolves and the directory is created before `download.file()` fails; no network is touched. Measured: `tools::R_user_dir()` interposes an `R` component under that envvar, so the path is `<root>/R/tidymedia/ffmpeg` — the test pins the full set of directories created beneath the root. Negative control run: dropping the `ffmpeg` component from the helper fails the test on exactly that assertion. Suite clean (0 failures, 18 skipped, 11195 passing). AC2 met.
- 2026-09-01 implement (T4): `@param install_dir` now names the `ffmpeg` subdirectory of `tools::R_user_dir("tidymedia", "data")` in place of "the user data directory"; `man/install_on_win.Rd` regenerated by `devtools::document()`. `NEWS.md` gained two entries — the new default install location under Configuration (naming that a passed `install_dir` is unaffected and that an FFmpeg installed by an earlier version keeps working, since its absolute path was recorded at install time and is what `find_ffmpeg()` reads), and the `SystemRequirements` declaration under Requirements. Suite clean. AC3 and AC5 met.
- 2026-09-01 implement (T5): `devtools::check()` **0 errors, 0 warnings, 0 notes** (7m 7s); `devtools::document()` produces no diff; `devtools::test()` clean (0 failures, 10 warnings, 18 skipped, 11195 passing). No URL-check NOTE appeared on the new `SystemRequirements` URLs, so AC4's justification clause has nothing to justify — a local check does not run CRAN's incoming URL check, so `urlchecker::url_check()` was run separately: it fetched all 24 URLs and reported both new ones clean. Its single finding is pre-existing and outside this milestone's scope — `README.md:9` links the lifecycle badge to `www.tidyverse.org/lifecycle/#experimental`, which redirects; it belongs to the standing CRAN readiness row. AC4 met.
- 2026-09-01 review: draft PR #103 opened; `origin/master` level with local `master`, no merge needed. The acceptance-criterion boxes arrived at review already ticked with no Review-section evidence — under AC fencing that is unverified, so they were reset to unticked and are re-ticked one at a time as each fresh evidence line lands.

## Review
<!-- owner: review (exclusive) -->

**Branch/PR:** https://github.com/jmgirard/tidymedia/pull/103 · base `master`
(level with `origin/master` at review time; no merge needed).

### Acceptance-criterion evidence (fresh, 2026-09-01)

- **AC1 — met.** `read.dcf("DESCRIPTION")` returns one record whose
  `SystemRequirements` field reads
  `FFmpeg (https://ffmpeg.org/), MediaInfo (https://mediaarea.net/en/MediaInfo)`;
  both tools are named, each with its project URL, and the two-line wrap parses
  back as a single field.
- **AC3 — met.** `man/install_on_win.Rd`'s `\item{install_dir}` reads "will
  default to the `ffmpeg` subdirectory of `tools::R_user_dir("tidymedia",
  "data")`, the user data directory CRAN policy sanctions"; the phrase "the user
  data directory" no longer stands alone.
- **AC5 — met.** `NEWS.md`'s Configuration section states the new default
  install location, that a passed `install_dir` is unaffected, and that an
  FFmpeg installed by an earlier version keeps working.
- **AC2 — met.** `R/program_management.R` resolves `install_on_win()`'s `NULL`
  branch through `tm_install_dir()` =
  `file.path(tools::R_user_dir("tidymedia", "data"), "ffmpeg")`; the same
  function's later lines extract with `archive_extract(strip_components = 1)`
  into `install_dir` and register `file.path(install_dir, "bin", "ffmpeg.exe")`,
  so the subdirectory is preserved rather than dropped. The test at
  `tests/testthat/test-program-management.R:180` calls `install_on_win()` with
  no `install_dir` under a `withr::local_envvar(R_USER_DATA_DIR = )` temp root
  and a `file://` URL, touching no network; re-run fresh at review, the file's
  55 tests pass with no failures and no skips.
- **AC4 — met.** `devtools::document()` left the working tree clean of
  generated-file changes (the only modified file was this milestone file, being
  edited at the time). `devtools::test()`: 0 failures, 11195 passing, 18 skipped,
  10 warnings. `devtools::check()`: **0 errors, 0 warnings, 0 notes** in
  7m 14.5s — no NOTE to justify, so AC4's URL-check clause has nothing to
  discharge. Toolchain consistency-gate slot also run: `pkgdown::check_pkgdown()`
  reports no problems; `README.Rmd` is unmodified by the branch so `README.md`
  stays in sync; `NEWS.md` (the declared changelog) carries entries for both
  user-visible changes; the branch adds no top-level file, so no
  `.Rbuildignore` entry is owed.

### Consistency gate

`cairn_validate.py` exit 0 — every check PASS, every advisory OK, including
`coverage complete` and `binding criteria`; the `release window` advisory did
not fire. No `DESIGN.md` principle changed, so `cairn_impact.py` was not run.
Toolchain checks (PROFILE `consistency-gate`) recorded under AC4 above.

### Independent review — three lenses, fresh context

Full three-lens fan-out (user-facing tier, executable surface touched).
**[S] blame-history:** no findings — the replaced `rappdirs::user_data_dir()`
line was set once at introduction and changed once here, nothing deliberate is
undone, and no D-entry governs the data directory. **[S] prior-PR-comments:**
no findings; the GitHub inline-comment probe returned empty so the thread walk
was skipped, and M097's archived review findings (F1 helper-vs-default, F3
NEWS-only documentation, F4 Windows envvar collapse, F6 `list.files()`) are each
avoided here. **[O] diff-bug:** the change itself correct; nine findings, all in
the test and the margins. Ranked, with disposition:

1. **Fix now.** The test's final assertion is tautological: it checks
   `bin/ffmpeg.exe` is absent inside a directory `dir.create()`d empty moments
   earlier, so it cannot fail; its comment describes a config-directory write
   the test never inspects.
2. **Fix now.** The test redirects `R_USER_DATA_DIR` but not
   `R_USER_CONFIG_DIR`, dropping half the file's own `tm_redirect_config()`
   convention; if the download ever succeeded, `set_ffmpeg()` and siblings would
   write into the developer's real config directory.
3. **Rejected.** AC2's second clause (extraction and registration beneath the
   subdirectory) is said to be unasserted by the test. AC2's verification clause
   ends "while performing no download", so it never asked the test to assert
   extraction; the code half is verified by inspection and recorded under AC2.
4. **Fix now.** Raw string path comparison where the sibling test at line 63
   normalizes both sides — the Windows-only, locally-green failure shape
   `LESSONS.md` records from M097's review.
5. **Fix now.** `paste0("file:///", <POSIX path>)` yields four slashes on
   Unix, so the test errors partly on a malformed URL; the bare `expect_error()`
   also names no failure, against the failure-identity rule.
6. **Rejected.** The test couples to `dir.create()` running before
   `download.file()`. That ordering is exactly what lets the default resolve
   with no download, and the full-directory-set assertion is the discrimination
   AC2 wants; `dir.exists()` alone would weaken it.
7. **Fix now.** `NEWS.md` does not say that re-running `install_on_win()` now
   writes a second FFmpeg tree and leaves the old one orphaned on disk.
8. **Rejected.** `SystemRequirements` field placement differs from `desc`'s
   canonical order — formatter-class nitpick; it parses and `R CMD check` is
   silent.
9. **Follow-up.** `install_on_win()`'s `@description` and `@param download_url`
   say "zip" while the default is a `.7z` extracted with
   `archive::archive_extract()`. Pre-existing, not introduced by this diff →
   ROADMAP candidate row.

No finding demonstrates an acceptance criterion failing, so the return floor is
not reached and the milestone stays in `review`.

### Fix-now work directed at the gate (2026-09-01)

Findings 1, 2, 4, 5 and 7 were fixed on the branch before the approval marker.
In `tests/testthat/test-program-management.R`: the AC2 test now also calls
`tm_redirect_config()`, so a registration write could not reach the developer's
real config directory; the `file://` URL is built as `"file:///"` plus the path
with its leading separator stripped and backslashes turned to `/`, well-formed
on both families; `expect_error()` now names the failure (`"cannot open URL"`)
rather than accepting any error; the directory-set comparison normalizes both
sides, the Windows separator trap `LESSONS.md` records from M097; and the
tautological `bin/ffmpeg.exe` assertion is replaced by one that the config
directory is still empty — which fails if the three `set_*()` calls ever move
ahead of the download. `NEWS.md` now says re-running `install_on_win()` installs
a second copy in the new location and leaves the old one on disk.

Negative control re-run on the changed assertion: dropping the `ffmpeg`
component from `tm_install_dir()` fails the test at the directory-set
comparison, naming the missing `.../R/tidymedia/ffmpeg` path. Finding 9 was
absorbed into the existing `install_on_win()` ROADMAP candidate row rather than
filed as a new one (search-first).
