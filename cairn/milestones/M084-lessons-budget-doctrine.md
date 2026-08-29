# M084: LESSONS gets back under its budget, and its biggest family graduates

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M083
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m084-lessons-budget-doctrine`

## Goal

Bring `cairn/LESSONS.md` under its 20,000-byte budget by graduating the
false-green family whole into a doctrine module and trimming or retiring the
rest.

## Scope

Surface tier: **internal** — the deliverable is the repo's own `cairn/`
tracking records, which no external consumer of the R package reads.

**In:** `cairn/LESSONS.md` is 42,232 bytes against a 20,000 budget and 49 lines
against a 50-line cap, so one more lesson makes it line-illegal too. Its 44 entries at bd4d545 are dominated by one family — "a test or control that reads green for
the wrong reason" — running through roughly fifteen entries and ~20,100 bytes,
already consolidated across M074/M076/M077/M079/M081 inside a single 4,851-byte
line. That family meets the maturation exit (transferable craft, extended well
more than twice, neither other exit applying), so it graduates whole into
`cairn/references/false-greens.md`, whose header states its own line and byte
budget set from the graduated size plus headroom. Classify every remaining entry
against the three exits — enforcement, ownership, maturation — trimming a
partly-covered lesson to its remainder and leaving no line behind for a retired
one. Refresh the hygiene stamp.

**Out:** `cairn/ROADMAP.md` → M083. A second doctrine module for the
front-door-ordering family (lines 21, 33, 41) → candidate row; it has been
extended twice at most, which is the edge of the maturation bar. Any lesson
merely disputed rather than redundant, which is corrected in place, never
retired.

## Acceptance criteria

- [ ] AC1 On the merge commit, `wc -c cairn/LESSONS.md` reports fewer than
      20,000 bytes and `wc -l` fewer than 50 lines.
- [ ] AC2 Each entry `grep '^- ' cairn/LESSONS.md` enumerates at M084's branch
      point (`git merge-base <branch> master`) is, after the merge, in exactly
      one observable state: present in `cairn/LESSONS.md` whole or trimmed, present in
      `cairn/references/false-greens.md`, or absent from both.
- [ ] AC3 `cairn/references/false-greens.md` exists, is listed in
      `cairn/references/INDEX.md`, states a line budget and a byte budget in
      its own header, and `wc -l -c` reports it inside both figures.
- [ ] AC4 No entry AC2 finds in `cairn/references/false-greens.md` is also in
      `cairn/LESSONS.md`: for each, `grep -F` of the first 120 bytes of its
      branch-point text (`cut -c1-120`) over `cairn/LESSONS.md` returns no match;
      T1 records that the key is unique across the enumeration.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T1, T2, T3
- AC3 → T2
- AC4 → T1, T2

## Tasks

- [x] T1 Record the branch-point baseline: `grep '^- ' cairn/LESSONS.md` with
      each entry's bytes and its `cut -c1-120` key, saved to the branch as the
      comparison surface AC2 and AC4 read, and assert the key is unique across
      the enumeration (it is across the 44 entries at bd4d545). Classify every
      entry against the three exits and write the classification into the
      milestone's Decisions section.
- [ ] T2 Author `cairn/references/false-greens.md`: the graduated family under
      themed headings, a header stating its line and byte budget set from the
      graduated size plus stated headroom, and an `INDEX.md` line. Delete every
      graduated entry from `LESSONS.md`, leaving no line behind.
- [ ] T3 Apply the remaining exits: trim each partly-covered lesson to its
      remainder; retire under enforcement or ownership only where the exit's
      test or owning slot is named in T1's classification. Re-measure; stop when
      AC1 clears with headroom.
- [ ] T4 Refresh the `Last hygiene check` stamp with the new byte and line
      figures for both files and the module's own budget.

## Decisions

- 2026-08-28 (T1): the 44 entries at the branch point (`31a8e4f`) classify as 17 `graduate`, 6 `trim`, 21 `keep`. The 17 graduating entries are one family — a test or control that reads green for the wrong reason: a fixture or grid that measures nothing, a skip or gate that never runs, a mock or control the call site swallows, a coverage or non-vacuity floor blind to the loss it exists to catch. They hold 21,171 bytes of entry text and were extended or consolidated across M52, M31/M63/M68, M35, M39, M41/M61/M62/M64, M42, M43, M44/M63/M67, M47, M50, M51/M59, M54/M58, M70, M071, M072/M073/M078 and M074/M076/M077/M079/M081, well past the maturation exit's twice; neither other exit applies, since no test fails on the mistake and no other tracking file's slot owns test-discrimination craft. The 6 `trim` entries each carry one half now covered elsewhere: the FFmpeg-version entry's `.aac`-muxer clause, the error-precedence entry's otherwise-valid-grid clause and the two-pass entry's half-domain clause belong to the graduating family; the front-door-ordering entry's front-door-guard half is enforced by `tests/testthat/test-builder-blame-front-door.R`, which reddens on the verb whose guard is removed; the line-endings entry's blame-config half is owned by `CLAUDE.md`'s development conventions; the timeout-wait entry's limit + 40 s measurement is owned by D056. The remaining 21 keep FFmpeg, R and package-shape facts no exit reaches. Per-entry rows with byte lengths and keys: `cairn/references/lessons-baseline-M084.md`.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in REDUCED mode (internal tier, no RB-tripwire tags). Round 1 returned four findings on this milestone's criteria: AC2 said "the pre-branch commit" without a SHA, AC2/AC4 routed their domain through a work-log disposition ledger (recording instrument, D-120), AC4's "no line of it remains" named no procedure, and AC5 mandated a run showing a test red under a planted defect — a plant matrix plus a recording act, and a demonstration crossing into R sources and testthat inside a milestone whose deliverable is `cairn/` markdown. Round 2 on the revised wording passed AC1/AC2/AC3 and returned two: AC4's "dated opening clause" was not procedurally bounded (two entry pairs share their first 20 characters), and the revised AC5 still bound archive-summary prose rather than the files. AC5 was dropped — AC2 already carries the deliverable-side fact — and AC4's key fixed to `cut -c1-120`, verified unique across all 44 entries.
- 2026-08-28: plan gate chose `cairn/references/false-greens.md` over a new `cairn/doctrine/` directory, because references/ exists with an INDEX and this repo has no doctrine-directory convention; falsified by a second family graduating and the two modules reading as source summaries in the index. Round 2's reader flagged the boundary: the module is doctrine-shaped and the plugin sites its own under `skills/shared/`, so the choice is a repo-local convention, not the plugin's.
- 2026-08-28: plan gate chose one module holding the family whole over splitting it into false-greens plus front-door-ordering, because the maturation exit requires a family to graduate whole and the second group has been extended twice at most; falsified by AC1 failing to clear with the one module out.
- 2026-08-28: T1 — branch cut; ledger `cairn/references/lessons-baseline-M084.md` committed with all 44 entries, their byte lengths and `cut -c1-120` keys (unique: `sort | uniq -d` returns nothing); classification in Decisions.
