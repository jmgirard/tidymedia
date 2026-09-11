# M122: macOS and Windows run the package's FFmpeg code

**Status:** done (2026-09-10, PR #126 https://github.com/jmgirard/tidymedia/pull/126)

**Goal:** The macOS and Windows check legs execute the package's FFmpeg invocation code
instead of only its skip paths.

**Outcome:** `.github/workflows/R-CMD-check.yaml` gains `brew install ffmpeg media-info`
(macOS) and `choco install ffmpeg mediainfo-cli --no-progress -y` (Windows) beside the
Linux apt step, each gated on `runner.os`. No R code changed: nothing reddened once the
binaries were present. On PR run 34546781144 missing-binary skips went from 159/107/7
(macOS) and 150/107/7 (Windows) at master run 34543833367 to 0, fixture-generation skips
0; PASS 13,393 (macOS) and 13,022 (Windows); all six legs green. Both legs run FFmpeg
9.0.1 (Windows: gyan.dev essentials build); the Windows install costs about 2 min a run.

**Decisions:** install on the existing two legs rather than add execution-only legs (plan
gate); Chocolatey over winget, and a temporary branch `push:` trigger removed in T3
(implement gate). No DECISIONS.md entry.

**Review:** three-lens fan-out; blame-history no findings, prior-review no evidence. [O]
raised 10. Fix now, in the record: merge commit 24699d7 of the PR run recorded, and
fixture-generation skip counts added to AC2. Rejected: no install retry or pin,
`MediaInfo.exe` casing, no brew env flags, a mixed-separator path in an expected warning.
Noted: essentials build, no caching, PR tracking. AC2/AC3 evidenced after approval (the PR
is the only trigger). Hygiene corrected LESSONS lines M45, M094, M096/M113 (binary-less legs).
