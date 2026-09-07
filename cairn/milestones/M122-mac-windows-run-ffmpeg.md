# M122: macOS and Windows run the package's FFmpeg code

- **Status:** planned
- **Priority:** normal
- **Depends on:** M118
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — CI configuration is dev tooling; no external consumer of the repo relies on it
- **Branch/PR:** —

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

- [ ] AC1: `.github/workflows/R-CMD-check.yaml` installs `ffmpeg`, `ffprobe` and
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

- [ ] T1: Add a macOS install step (`brew install ffmpeg media-info`) and a Windows
      one (Chocolatey or winget), replacing the Linux-only gate at `:39-43`.
- [ ] T2: Push and read what reddens. The expected surface is Windows `system2()`
      quoting, `.exe` suffixes and path separators — M113 was the second arc reddened
      by this gap, and `R/program_management.R:416` is the `system2(location, …)` site.
- [ ] T3: Fix what the legs find, or, for anything that turns out to be a real
      platform defect rather than a CI defect, split it to its own milestone and
      record the split here rather than growing this one.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in REDUCED mode (declared tier internal), fresh-context [O] reader, asking only the bounded-promise, instrument and proportionality questions. It returned no findings: all three criteria passed every question asked of them.
- 2026-09-07: plan gate chose installing the binaries on the existing two legs over adding dedicated execution-only legs, because the matrix already runs macOS and Windows and the gap is the install step rather than the coverage; falsified by the two legs becoming so slow or flaky that the check matrix stops being usable.
