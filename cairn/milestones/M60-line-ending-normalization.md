# M60: The repo's line endings are normalized once and enforced mechanically

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —

- **Branch/PR:** `m60-line-ending-normalization`

## Goal

End the CRLF anomaly in `R/ffmpeg.R` by normalizing the repo to LF once and
pinning it with `.gitattributes`, so no future edit can rewrite the file
wholesale by accident.

## Scope

**In:** measured state on merged master 2026-08-07 — no `.gitattributes`
exists, `core.autocrlf` is unset, and `R/ffmpeg.R` carries CRLF on all 6288
lines (planned as 5950, re-measured 2026-08-08) *in the stored blob* (`git show HEAD:R/ffmpeg.R`), not merely in the
working tree. Nothing records or enforces this, and the trap has fired twice:
M35 read a 209/36-line change as 4172/3999, M48 read a 126-line change as
11,116.

`R/ffmpeg.R` is **not** the only CRLF file, which both `LESSONS.md` and this
milestone's first draft asserted. `git ls-files -z | xargs -0 grep -lI $'\r'`
returns two tracked text files — `R/ffmpeg.R` and `tidymedia.Rproj` — and
three tracked binaries carry CR bytes that `text=auto` will not and must not
strip (`inst/extdata/sample.mp4`, `tests/testthat/fixtures/probe-baseline.rds`,
`tidymedia_0.0.0.9001.tar.gz`). The `LESSONS.md` line is corrected in place as
part of this milestone (current knowledge, D-045).

The work: add `.gitattributes` with `* text=auto`, renormalize the repo in one
isolated commit that changes nothing else, and add `.git-blame-ignore-revs`
naming that commit so `git blame` stays readable. Retire or trim the
`LESSONS.md` entry the change makes mechanical, per the tracking-rules
retirement criteria (enforcement).

**Out:** any content change to `R/ffmpeg.R` — the normalization commit is
byte-only, and a review that finds a semantic diff in it has found a defect,
not a scope question. Choosing to pin CRLF instead: weighed and rejected at
the plan gate (2026-08-07); it is zero-diff but leaves the file the odd one
out and every scripted edit still has to remember.

## Acceptance criteria

- [ ] AC1 — `.gitattributes` exists at the repo root with `* text=auto`, and
      `.git-blame-ignore-revs` exists naming the normalization commit's full
      SHA.
- [ ] AC2 — No tracked *text* file carries a CR byte, verified by a procedure
      that enumerates the whole tracked set and lets git classify:
      `git ls-files -z | xargs -0 grep -lI $'\r'` returns empty. `-I` skips
      binary files, which is the intended domain — three tracked binaries
      carry CR bytes that must survive. This replaces the file-extension
      hand-list, which would leave any extension it omits stale, and `-lc`,
      which prints a `path:count` line for every file and so can never return
      empty.
- [ ] AC3 — The normalization commit changes line endings and nothing else,
      shown both ways: `git diff --ignore-cr-at-eol <before> <after>` is empty
      over the whole tree, **and** the same diff without that flag is
      non-empty and touches exactly the two text files AC2's command names
      today. The second half is what stops AC3 being satisfied by doing
      nothing.
- [ ] AC4 — `git add --renormalize .` on a clean tree after the change
      produces no further staged diff, so the pinned setting and the stored
      bytes agree on this machine.
- [ ] AC5 — The `LESSONS.md` CRLF entry is corrected and then retired: its
      "only CRLF file" claim is false (AC2's command returns two text files),
      and `.gitattributes` plus AC2's check is the enforcement that retires
      what remains. The entry is deleted outright, or its surviving text
      states in so many words what the enforcement does not cover; a trim that
      changes nothing does not satisfy this.
- [ ] AC6 — The r-package profile's verify slot is clean after normalization:
      `devtools::document()` produces no diff, `devtools::test()` passes, and
      `devtools::check()` reports 0 errors and 0 warnings.
- [ ] AC7 — Both new top-level files carry `.Rbuildignore` entries, per the
      repo's CLAUDE.md convention, verified by inspection rather than by a
      check NOTE: `.Rbuildignore` holds anchored patterns matching
      `.gitattributes` and `.git-blame-ignore-revs`, and the tarball
      `pkgbuild::build()` produces contains neither. A `devtools::check()`
      NOTE cannot verify this — R CMD check does not flag top-level dotfiles,
      and the repo's unignored `.gitignore` raises none today.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T4
- AC3 → T2, T4
- AC4 → T2, T4
- AC5 → T5
- AC6 → T6
- AC7 → T1, T6

## Tasks

- [x] T1 — Add `.gitattributes` (`* text=auto`) and the `.Rbuildignore`
      entries for both new top-level files; commit alone, before any byte
      changes.
- [x] T2 — Run `git add --renormalize .` and commit the result as one isolated
      commit touching nothing else; record the SHA.
- [x] T3 — Assert AC3's two-way diff against the recorded SHA.
- [x] T4 — Assert AC2 and AC4 and commit the evidence.
- [x] T5 — Add `.git-blame-ignore-revs` with the T2 SHA; correct and retire
      the `LESSONS.md` CRLF entry; name in the work log what it graduated, for
      the archive summary review will write.
- [ ] T6 — Run the full verify slot and AC7's tarball inspection on the final
      tree, after every file this milestone adds exists.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: plan gate chose one-time LF normalization over pinning the existing CRLF with `R/ffmpeg.R -text` because pinning is zero-diff but leaves the anomaly and its recurring trap in place, while normalizing pays the 5950-line diff once, deliberately, in a commit whose only purpose is that, with `.git-blame-ignore-revs` covering the blame cost; falsified by a tool in this repo's workflow that requires CRLF in that file, or by the ignore-revs file failing to restore readable blame in the maintainer's own tooling.
- 2026-08-07: gate also rejected `.gitattributes` with `text eol=crlf`, which reads as the conservative option but is not: because CRLF is in the stored blob today, it renormalizes to LF in the repo and rewrites all 5950 lines anyway — the full cost of normalizing with none of the benefit.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on AC1's coverage, AC2 (a `grep -lc` that can never return empty, and an unreachable universal over tracked binaries), AC3 (satisfiable by doing nothing), AC4 (an unbounded claim over contributor platforms), AC5 (a zero-character trim satisfies it; archive summary not checkable at review), and AC7 (a check NOTE that verifies nothing, plus a task-ordering defect). All fixed before commit; AC6 returned clean. The audit also corrected this file's own Scope: `R/ffmpeg.R` is not the only CRLF file.
- 2026-08-08: implement gate — the tracked tarball stays out (its ROADMAP row named M60 as a promote trigger; folding an untracking commit in would dilute a milestone whose product is a bytes-only change), and `.git-blame-ignore-revs` ships with a documented one-time `git config blame.ignoreRevsFile` line in CLAUDE.md rather than an unconfigured file, since local `git blame` does not read the file on its own.
- 2026-08-08: measured before T1 that `* text=auto` cannot mangle the three tracked binaries — git classifies a file binary on a NUL byte in its first 8000, and `sample.mp4`, `probe-baseline.rds` and the tarball carry 64, 29 and 45 respectively; T2's diff re-checks this empirically.
- 2026-08-08: T1 — `.gitattributes` (`* text=auto`) added and both new top-level files given anchored `.Rbuildignore` entries, committed alone before any byte change.
- 2026-08-08: T2 — normalization committed as `482a1d3ee38fd9e38a4659d6f9e29faefa1f306a`, touching `R/ffmpeg.R` (6288/6288) and `tidymedia.Rproj` (18/18) and no third file; the three tracked binaries were left alone, confirming the pre-T1 NUL-byte measurement empirically. The commit deliberately carries no tracking file, which is the one place this milestone departs from tracking-travels-with-code: AC3 asserts the diff touches exactly two files, so a milestone-file update riding along would falsify the criterion it exists to prove. Recorded here instead, one commit later.
- 2026-08-08: `git add --renormalize` stages LF but does NOT rewrite the working tree, so both files still held CR bytes on disk after the commit while `git status` read clean (the checkin filter makes the CRLF working copy and the LF blob compare equal). AC2 reads the working tree, so it would have failed here. Refreshed by deleting both files and `git checkout --`, then verified disk md5 equals blob md5 for each.
- 2026-08-08: T3/T4 — all four assertions pass on `5272eb8..482a1d3`. AC3a `git diff --ignore-cr-at-eol` over the whole tree: empty. AC3b the same diff unflagged: non-empty, exactly the two text files. AC2 `git ls-files -z | xargs -0 grep -lI $'\r'`: empty, with a control run WITHOUT `-I` returning the three binaries, so the empty result is the filter working rather than the sweep finding nothing. AC4 `git add --renormalize .` on the clean tree: no staged diff.
- 2026-08-08: minor amendment — Scope's `5950` corrected to the measured `6288` (the figure was taken 2026-08-07 and the blob is larger); no criterion cites the number and no scope boundary moves. The plan-gate work-log line keeps `5950` as written, being history.
- 2026-08-08: T5 — `.git-blame-ignore-revs` added naming `482a1d3ee38fd9e38a4659d6f9e29faefa1f306a` (verified to resolve to a commit), with the one-time `git config blame.ignoreRevsFile` line in its own header comment and in CLAUDE.md's development conventions, since local `git blame` reads the file only when configured to while GitHub's blame UI reads it unprompted.
- 2026-08-08: T5 — the CRLF lesson (`2026-07-27 (M35; recurred M48, M58, M59)`) is retired by enforcement and deleted outright; its own closing sentence named M60 as the trigger. What it graduated, for the archive summary: the whole of it, into `.gitattributes` — a text-mode round-trip can no longer produce a spurious diff, because git normalizes on checkin, so the diffstat-vigilance habit it taught has nothing left to catch. Two of its claims were also false by this branch's measurements — `R/ffmpeg.R` was not the repo's only CRLF file (`tidymedia.Rproj` was the other) and the file is 6288 lines, not the '~5500' it stated — and deleting the line removes them from readable text, which is what correcting in place exists to achieve. `LESSONS.md` now stands at 48 lines against its 50-line cap.

## Decisions

## Review
