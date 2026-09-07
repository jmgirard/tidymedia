# M120: NEWS.md reads as release notes

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — NEWS.md ships and is the first thing a reader checks
- **Branch/PR:** —

## Goal

The development section of `NEWS.md` reads as one set of release notes rather than
an append-only per-milestone log.

## Scope

**In:** collapsing the 1,968-line development section into one section with each
heading appearing once, covering every export added or removed and every rename
since the 0.1.0 heading.

**Out:** naming a release version number, cutting a release section, or bumping
`DESCRIPTION` → the release walk's act (`/cairn-release`), which this milestone
leaves untouched. Rewriting the four already-released sections → not needed; each
already has no repeated heading.

## Acceptance criteria

- [ ] AC1: `NEWS.md` opens with a single development-version `#` heading, and the
      development section names no release version number.
- [ ] AC2: A sweep over every `#` section of `NEWS.md` finds no `##` heading repeated
      within any one section. Today the development section carries `## New features`
      at lines 3, 267 and 1402, `## Breaking changes` at 48, 523 and 1943, `## Bug
      fixes` at 868, 1845 and 1961, and `## Documentation` at 20 and 1666.
- [ ] AC3: The development section names every export in the symmetric difference
      between `NAMESPACE` at head and `NAMESPACE` at commit `4b04fad9`, the commit
      that introduced the `# tidymedia 0.1.0` heading — 40 added and 14 removed as
      measured 2026-09-07.
- [ ] AC4: It names every rename recorded in `cairn/DECISIONS.md` D014, D077 and D078
      since that commit which is not itself a `NAMESPACE` entry — the argument and
      option renames.
- [ ] AC5: The `verify` slot of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T1, T3
- AC4 → T1, T3
- AC5 → T4

## Tasks

- [ ] T1: Compute the symmetric difference of the two `NAMESPACE`s and read D014,
      D077 and D078 for the renames that are not `NAMESPACE` entries. The removed set
      measured 2026-09-07 is `:=`, `as_label`, `as_name`, `audio_as_mp3`,
      `convert_fractions`, `enquo`, `enquos`, `ffm`, `get_codecs`, `get_encoders`,
      `get_framerate`, `get_samplingrate`, `mediainfo_summary`, `pad_integers` —
      breaking changes for any 0.1.0 caller.
- [ ] T2: Collapse the development section: one occurrence of each `##` heading,
      entries merged under it in reader order rather than milestone order.
- [ ] T3: Check the collapsed section against T1's two lists and fill what is missing.
- [ ] T4: Confirm the four released sections below are untouched; run the profile's
      `verify` slot.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: AC1's "no release version number is named" had no stated domain and was unsatisfiable over the whole file, which holds four real release headings (repaired — scoped to the development section); AC3's one-way set difference named only the 40 additions while AC4's two decision entries recovered some but not all of the 14 removals, leaving `ffm`, `pad_integers`, `convert_fractions`, `mediainfo_summary` and the `get_framerate`/`get_samplingrate` renames covered by neither (repaired — AC3 made symmetric, AC4 given D078 and narrowed to non-`NAMESPACE` renames). AC2 and AC5 passed all six questions clean.
