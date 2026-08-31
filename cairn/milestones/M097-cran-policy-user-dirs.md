<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M097: A remembered binary location lives where CRAN policy sanctions

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Move `set_program()`'s write path and `find_program()`'s read path to
`tools::R_user_dir("tidymedia", "config")` — the only user config location CRAN
policy sanctions — without losing a location a user set before the change.

## Scope

Surface tier: **user-facing** — it changes the on-disk location where a user's
`set_ffmpeg()` path is remembered, which a consumer of the package relies on.

**In:** the config directory for all four programs (`ffmpeg`, `ffprobe`,
`ffplay`, `mediainfo`); a legacy read fallback; the suite's own config-write
hygiene, which the move silently breaks.

Scope notes, not criteria: `rappdirs` stays in `Imports` because the legacy read
path uses it, so D055's nine measured floors, `data-raw/floor-probes.R` and
`data-raw/Dockerfile.floors` need no change. Keeping it is also what lets the
legacy path be computed by the library that wrote it rather than by a second
copy of its platform layout — the failure a single-platform test cannot catch.

**Out:**
- `SystemRequirements` and `install_on_win()`'s data directory → M098.
- `install_on_win()`'s download posture (a 7z from a third-party URL) → ROADMAP
  candidate row; a separate, larger change to an exported function's behavior.
- Removing `rappdirs` from `Imports` → decided against above, not deferred.
- The release itself → the standing `CRAN readiness` ROADMAP candidate row.
- D014's pre-0.2.0 rename window → M099.

## Acceptance criteria

- [ ] AC1 `set_program()` writes its location file at exactly
      `file.path(tools::R_user_dir("tidymedia", "config"), "<program>_location.txt")`.
      A test redirects `R_USER_CONFIG_DIR` to a temp dir with
      `withr::local_envvar()`, calls `set_program()` for each of the four
      programs its own `arg_match()` vocabulary names, passing an executable
      stub the test creates (`set_program()` aborts unless `Sys.which(location)`
      is non-empty), and asserts **path equality**, not containment: `rappdirs`
      honors `R_USER_CONFIG_DIR` too, so a containment assertion passes against
      the unchanged function.
- [ ] AC2 A location written before this milestone is still returned after it.
      For each of the four programs, under `withr::local_envvar(PATH = "")` so
      the `Sys.which(program)` branch cannot short-circuit — never by mocking
      `Sys.which`, which `find_program()` calls again at
      `R/program_management.R:33` to validate what it read, which would make all
      three states return `NULL` — three states are asserted: a file at the
      legacy path alone returns the legacy location; a file at the
      `R_user_dir()` path alone returns that location; files at both return the
      `R_user_dir()` one. Each file holds an absolute path to an executable
      stub, which `Sys.which()` still resolves under an empty `PATH`. Both
      directories are redirected in-test and asserted to differ, so neither
      library reaches the user's real config dir.
- [ ] AC3 `find_program()`'s stale-location branch fires from the legacy path
      too: a legacy file naming a binary that no longer exists produces the
      existing warning and a `NULL` location.
- [ ] AC4 No test in the suite writes to the user's real config directory.
      `tests/testthat/test-nvenc-memo.R:87-106` redirects `set_program()`'s write
      by mocking `rappdirs::user_config_dir`; once AC1 lands that mock still
      resolves and redirects nothing. The domain is the config-writing call
      sites in `tests/`, enumerated by
      `grep -rn "set_program(\|set_ffmpeg(\|set_ffprobe(\|set_ffplay(\|set_mediainfo(" tests/`;
      every hit runs under a redirected `R_USER_CONFIG_DIR`.
- [ ] AC5 The legacy config layout is computed in exactly one place,
      `tm_legacy_config_dir()`. Evidence: `grep -rn "rappdirs" R/` returns hits
      only inside that helper, plus a reviewer read confirming the helper
      delegates to `rappdirs::user_config_dir()` rather than reconstructing a
      platform layout by hand — the failure mode a string grep cannot see.
- [ ] AC6 `NEWS.md` states that a location set with `set_ffmpeg()` and its
      siblings now lives under `tools::R_user_dir()`, and that a location set
      before the change is still found.
- [ ] AC7 `devtools::test()` clean, `devtools::document()` produces no diff,
      `devtools::check()` reports 0 errors and 0 warnings with every NOTE
      justified (PROFILE `verify` and `consistency-gate` slots).

## Tasks

1. Add `tm_config_dir()` (returning `tools::R_user_dir("tidymedia", "config")`)
   and `tm_legacy_config_dir()` (delegating to `rappdirs::user_config_dir`) to
   `R/program_management.R`.
2. Write AC1's four-program equality test with its executable stub; confirm it
   fails against the unchanged `set_program()`.
3. Move `set_program()`'s write to `tm_config_dir()`.
4. Write AC2's and AC3's tests — four programs x three file states under
   `PATH = ""`, plus the stale-location branch reached from the legacy path.
5. Add the legacy read fallback to `find_program()`, new path preferred.
6. Run AC4's grep; move `test-nvenc-memo.R`'s redirect off the `rappdirs` mock
   onto `R_USER_CONFIG_DIR`; cover every other hit the grep returns.
7. Run AC5's grep and read `tm_legacy_config_dir()`.
8. Add the `NEWS.md` entry.
9. Run `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T4, T5
- AC3 → T4, T5
- AC4 → T6
- AC5 → T1, T7
- AC6 → T8
- AC7 → T9

## Work log
<!-- owner: implement/review -->

- 2026-08-31 plan: criteria audit ran in FULL mode (surface tier user-facing), fresh-context [O] reader, twice — once over the pre-gate draft and again over the final wording. Round 1 returned six findings; round 2 returned ten, including a false green in the draft's own AC1: the probe asserted the written file was *inside* a redirected temp dir, which passes against unchanged code because `rappdirs` honors `R_USER_CONFIG_DIR` too (measured: `/tmp/xcfg/tidymedia` vs `/tmp/xcfg/R/tidymedia`). All ten disposed here — AC1 became path equality; AC2 gained the `PATH = ""` mechanism after the reader showed a `Sys.which` mock makes it unsatisfiable via the re-validation at `R/program_management.R:33`; AC4 is new, covering a latent regression the draft had asserted away; AC5 gained the reviewer read a string grep cannot substitute for; the check bar was relaxed to PROFILE's "NOTEs justified".
- 2026-08-31 plan: alternative rejected — dropping `rappdirs` from `Imports` and reconstructing the legacy layout by hand. Lost at the question gate: it makes D055's nine measured floors stale, breaks `test-nvenc-memo.R:96`'s namespace mock, and puts the legacy path in a second copy of rappdirs' platform rules that a single-platform suite cannot check. Falsified by a report of the retained dependency costing something the fallback does not buy.
- 2026-08-31 plan: alternative rejected — one milestone covering the config dir, `SystemRequirements`, and the data dir together. Lost to the sizing tripwires (9 criteria / 11 tasks); split into M097 and M098 rather than shrunk to fit.
