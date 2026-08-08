# M60: The repo's line endings are normalized once and enforced mechanically

**Status:** done (2026-08-08, PR #63 https://github.com/jmgirard/tidymedia/pull/63)

**Goal:** End the CRLF anomaly in `R/ffmpeg.R` by normalizing the repo to LF
once and pinning it with `.gitattributes`, so no future edit rewrites it wholesale.

**Outcome:** `.gitattributes` holds `* text=auto`, so git normalizes on
check-in and a text-mode write can no longer inflate a diff. `R/ffmpeg.R`
(6288 lines) and `tidymedia.Rproj` (18) lost their CR bytes in one isolated
byte-only commit; the three tracked binaries survive on git's NUL-byte
detection. `.git-blame-ignore-revs` names the squash commit
`921a00f3e7f2ab495e9145e12e579e54b6a0207f` and cuts blame misattribution on
`R/ffmpeg.R` from 6288 lines to 376. `CLAUDE.md` documents the one-time
`blame.ignoreRevsFile` config and its `--no-ignore-revs-file` escape hatch.

**Decisions:** none promoted. The tracked root tarball was left out to keep
the milestone bytes-only; its ROADMAP candidate row stands.

**Review:** Three lenses, ten candidates, one at or above 80. F3 (88,
`CLAUDE.md`'s blame config is fatal wherever the ignore-revs file is absent)
fixed on the branch. F2 rejected on re-measurement — the ignore-revs file
restores 94% of blame, so the plan gate's falsifier does not fire. F1 (74, a
squash merge orphans the recorded SHA) confirmed true post-merge and repaired
in hygiene. Graduated: the M35 CRLF lesson, retired whole by enforcement.
