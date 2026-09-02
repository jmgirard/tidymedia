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

- [ ] AC1 `DESCRIPTION` carries a `SystemRequirements` field naming FFmpeg and
      MediaInfo, each with its project URL.
- [ ] AC2 `install_on_win()`'s own default — the value `install_dir` takes when
      the caller passes none — is the `ffmpeg` subdirectory of
      `tools::R_user_dir("tidymedia", "data")`. The subdirectory is preserved,
      not dropped: `archive_extract(strip_components = 1)` unpacks into it and
      the function registers `bin/ffmpeg.exe` beneath it. Asserted by a test
      that redirects `R_USER_DATA_DIR` with `withr::local_envvar()` and reaches
      the function's own default resolution while performing no download.
- [ ] AC3 `install_on_win()`'s `@param install_dir` text names the new default
      location, replacing "the user data directory".
- [ ] AC4 `devtools::test()` clean, `devtools::document()` produces no diff,
      `devtools::check()` reports 0 errors and 0 warnings with every NOTE
      justified (PROFILE `verify` and `consistency-gate` slots). A URL-check
      NOTE on the new `SystemRequirements` URLs is justified by naming it.
- [ ] AC5 `NEWS.md` states the new default install location.

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
