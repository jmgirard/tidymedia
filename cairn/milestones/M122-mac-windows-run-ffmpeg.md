# M122: macOS and Windows run the package's FFmpeg code

- **Status:** review
- **Priority:** normal
- **Depends on:** M118
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — CI configuration is dev tooling; no external consumer of the repo relies on it
- **Branch/PR:** `m122-mac-windows-run-ffmpeg`

## Goal

The macOS and Windows check legs execute the package's FFmpeg invocation code
instead of only its skip paths.

## Scope

**In:** installing the three binaries on the macos-latest and windows-latest legs of
`.github/workflows/R-CMD-check.yaml`, and fixing what the newly-executing tests find.

**Out:** exercising `install_on_win()` against a real download on CI → it spends
money and time and needs its own decision; a candidate row holds it. Adding legs or
changing the matrix → the six legs stay six. Re-running the dependency floor harness
→ ROADMAP candidate row.

## Acceptance criteria

- [x] AC1: `.github/workflows/R-CMD-check.yaml` installs `ffmpeg`, `ffprobe` and
      `mediainfo` on the macos-latest and windows-latest legs as well as the Linux
      ones. Today `:39-43` is gated `if: runner.os == 'Linux'`.
- [ ] AC2: On the workflow run at this milestone's head commit, the macOS and Windows
      legs each report zero tests skipped for the reasons `ffmpeg binary not
      available`, `ffprobe binary not available` or `mediainfo binary not available`.
- [ ] AC3: All six legs of that run are green.

## Coverage

- AC1 → T1
- AC2 → T1, T3
- AC3 → T2, T3

## Tasks

- [x] T1: Add a macOS install step (`brew install ffmpeg media-info`) and a Windows
      one (Chocolatey or winget), replacing the Linux-only gate at `:39-43`.
- [x] T2: Push and read what reddens. The expected surface is Windows `system2()`
      quoting, `.exe` suffixes and path separators — M113 was the second arc reddened
      by this gap, and `R/program_management.R:416` is the `system2(location, …)` site.
- [x] T3: Fix what the legs find, or, for anything that turns out to be a real
      platform defect rather than a CI defect, split it to its own milestone and
      record the split here rather than growing this one. Last, remove the
      temporary branch entry from the workflow's `push:` list.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in REDUCED mode (declared tier internal), fresh-context [O] reader, asking only the bounded-promise, instrument and proportionality questions. It returned no findings: all three criteria passed every question asked of them.
- 2026-09-07: plan gate chose installing the binaries on the existing two legs over adding dedicated execution-only legs, because the matrix already runs macOS and Windows and the gap is the install step rather than the coverage; falsified by the two legs becoming so slow or flaky that the check matrix stops being usable.
- 2026-09-10: /milestone-implement started; branch `m122-mac-windows-run-ffmpeg` cut from `master` at 46f1b0a (in sync with origin).
- 2026-09-10: implement gate chose a temporary `push:` trigger for this branch (removed in T3's last step; the review PR run is the head-commit evidence) over an early draft PR, and Chocolatey over winget for Windows. T3 wording extended with the trigger removal (minor amendment).
- 2026-09-10: T1 done — `brew install ffmpeg media-info` (macOS) and `choco install ffmpeg mediainfo-cli` (Windows) steps added beside the Linux one; no R code changed, so the verify slot's test run was not owed for this task.
- 2026-09-10: T2 done — push run 34544062828 at ec477dc, all six legs green. Missing-binary skips (ffmpeg/ffprobe/mediainfo reasons) went from 273 on macOS and 264 on Windows at master run 34543833367 (46f1b0a) to 0 on both; PASS 12081→13393 (macOS), 11755→13022 (Windows). Test warnings on both legs are a subset of the Linux release leg's set. Install steps took about 8 s (brew bottles) and 2 min (choco).
- 2026-09-10: T3 done — nothing reddened, so no fix and no split; the temporary `push:` branch entry removed from the workflow. Local `devtools::test()` clean (no R code changed on the branch).
- 2026-09-10: claim audit: not owed — internal tier.
- 2026-09-10: status → review. AC2/AC3 still need the review PR's run at the head commit, since the branch run above predates the trigger removal.
- 2026-09-10: /milestone-review started; no PR for the branch (route d); `master` unmoved since the cut (46f1b0a = origin/master), so no merge or re-test owed before evidence.

## Review

- AC1 (2026-09-10): read at head c3e6d68, `.github/workflows/R-CMD-check.yaml:39-51` has three install steps gated `runner.os == 'Linux'` (apt `ffmpeg mediainfo`), `'macOS'` (`brew install ffmpeg media-info`) and `'Windows'` (`choco install ffmpeg mediainfo-cli --no-progress -y`). At ec477dc (workflow differs from head only in the `push:` branch list) run 34544062828 shows the macOS step `success` on macos-latest and the Windows step `success` on windows-latest. That run's job logs contain 0 lines each for `ffmpeg`/`ffprobe`/`mediainfo binary not available` on both legs; the same grep over master run 34543833367's logs finds all three reasons on both legs (macOS 159/107/7, Windows 150/107/7), so a zero discriminates. Pass.
