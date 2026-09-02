<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M098: DESCRIPTION declares the tools the package interfaces, and the data dir follows policy

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m098-system-requirements-and-data-dir`

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
