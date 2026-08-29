# M083: The ROADMAP gets back under its byte budget

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m083-roadmap-byte-budget`

## Goal

Bring `cairn/ROADMAP.md` under its 24,000-byte budget by entombing the
instrument-findings rows, pruning the already-promoted ones, and compressing
what stays.

## Scope

Surface tier: **internal** — the deliverable is the repo's own `cairn/`
tracking records, which no external consumer of the R package reads.

**In:** `cairn/ROADMAP.md` is 41,509 bytes against a 24,000 budget (50 lines,
under the 60-line cap), and 39,760 of those bytes are its 32 candidate rows.
Apply the tracking-rules remedy in its stated order. (a) Move the five
instrument-findings rows (13,303 bytes) into a new
`cairn/references/instrument-findings.md`, leaving one grouped row pointing at
it. (b) Prune the three struck-through rows already fully promoted,
first folding the retired M44 row's `(a)`/`(b)` promotion conditions into the
M082-leftovers row, which cites them by reference rather than restating them. (c) Compress the widest surviving live rows to a
hook, a promote-on clause and a links trailer, moving measurement narrative to
the D-entry or milestone file each already cites. Refresh the hygiene stamp.

**Out:** `cairn/LESSONS.md` → M084. Any byte-budget checker: budgets are
judgment-checked at hygiene passes by rule, and adding one would be the
checker-regress shape. Retiring any live candidate on its merits — this
milestone changes how rows are stored and worded, never whether an idea stands.

## Acceptance criteria

- [ ] AC1 On the merge commit, `wc -c cairn/ROADMAP.md` reports fewer than
      24,000 bytes and `wc -l` fewer than 60 lines.
- [ ] AC2 Each row `awk '/^## Candidates/,0' cairn/ROADMAP.md | grep '^- '`
      enumerates at M083's branch point (`git merge-base <branch> master`) is,
      after the merge, in exactly one observable state: present in
      `cairn/ROADMAP.md`, present in `cairn/references/instrument-findings.md`,
      or absent from both — and the rows in the third state are exactly those
      the same pipe with `grep '^- ~~'` enumerates at that commit.
- [ ] AC3 `cairn/references/instrument-findings.md` exists and is listed in
      `cairn/references/INDEX.md`, and each row the same pipe with
      `grep -i 'instrument'` enumerates at M083's branch point is present in it
      carrying that row's finding ids and its promote-on clause, and absent
      from `cairn/ROADMAP.md`.
- [ ] AC4 Each row AC2 finds present in `cairn/ROADMAP.md` after the merge
      retains its `— added` trailer, and retains a promote-on clause wherever
      its branch-point text matched `grep 'Promote '`.
- [ ] AC5 `python3 ~/.claude/skills/cairn/scripts/cairn_validate.py` reports
      all 16 checks PASS and all 7 advisories OK.

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T2, T3, T4
- AC3 → T2
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1 Record the branch-point baseline: run AC2–AC4's four enumerations,
      save each row's bytes, its `— added` trailer and its `Promote ` match to
      the branch as the comparison surface AC2–AC4 read, and record the counts.
      (At bd4d545 these were 32 / 3 / 5 / 21; this plan's own commit adds one
      candidate row, so the baseline is re-measured, never carried over.)
- [x] T2 Create `cairn/references/instrument-findings.md` holding AC3's five
      rows verbatim under per-row headings; add its `INDEX.md` line
      under "Working artifacts"; replace the five rows with one grouped
      candidate row pointing at the page.
- [x] T3 Fold the retired M44 row's `(a)`/`(b)` promotion conditions into the
      M082-leftovers row (which says "read them there rather than here"), then
      delete AC2's three struck-through rows. Check the D055-gaps row's stated
      dependence on the retired floor-harness row before deleting that one.
- [ ] T4 Compress the surviving rows over 1,000 bytes at the branch point
      (eleven at 8021df1) to hook + promote-on + trailer, each keeping the citations its
      narrative is moving to. Before dropping a sentence, confirm the entry or
      milestone file it cites actually holds it; anything with no home stays.
      Re-measure after each; stop when AC1 clears with headroom.
- [ ] T5 Refresh the `Last hygiene check` stamp with the new byte figures; run
      `cairn_validate.py`.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in REDUCED mode (internal tier, no RB-tripwire tags). Round 1 returned five findings on this milestone's criteria: AC3's "the five instrument-findings rows" was a recalled count (the case-sensitive grep returns four; `tolower` returns five), AC5 named a `cairn_validate.py` path with no target, and AC2/AC4 routed their domain through a work-log disposition ledger (recording instrument, D-120). Round 2 on the revised wording passed AC1/AC2/AC3/AC5 and returned one finding: AC4's `-- added` literal matches nothing (the file uses an em dash) and its promote-on sub-domain named no procedure. All fixed before writing.
- 2026-08-28: plan gate chose entombing the instrument rows into `cairn/references/` over pruning them outright, because a pruned finding surfaces at no later plan gate; falsified by a hygiene pass finding the page itself over budget with nothing having consulted it.
- 2026-08-28: T1 branch-point baseline at `8021df1`: `cairn/ROADMAP.md` 42,552 bytes / 53 lines; 33 candidate rows, 3 struck, 5 matching `instrument` (13,303 bytes), 22 carrying `Promote `. Frozen in `cairn/references/roadmap-candidates-baseline-M083.md` as a convenience record; the criteria read git and the post-merge files, not that page.
- 2026-08-28: minor amendment to T4 — the branch point has eleven surviving rows over 1,000 bytes, not the nine the plan recorded at `bd4d545`; T4's parenthetical corrected and its citation-check step (the implement gate's choice) written in.
- 2026-08-28: implement gate chose one shared promote-on clause for the grouped instrument row over restating all five (the five stay verbatim on the entombed page), checking each row's citations before dropping narrative over trusting them, and compressing all eleven wide rows rather than stopping at the first pass under 24,000.
- 2026-08-28: T2 — the five instrument rows moved verbatim into `cairn/references/instrument-findings.md` (15,878 bytes, indexed), replaced in `cairn/ROADMAP.md` by one grouped row with a shared promote-on clause; verified each of the five is byte-identical on the page and absent from the ROADMAP. ROADMAP 42,552 → 30,013 bytes, 53 → 49 lines, 33 → 29 candidate rows.
- 2026-08-28: T3 — the retired M44 row's (a)/(b) promotion conditions folded verbatim into the M082-leftovers row, then the three struck-through rows deleted. Checked the D055-gaps row first: it states its own gaps in full and says outright that retiring the carry row does not lose them, and the carry row agreed, so nothing was lost. ROADMAP 30,013 → 27,140 bytes, 49 → 46 lines, 29 → 26 candidate rows.
- 2026-08-28: plan gate chose compressing live rows in place over deferring the whole cut to entombment and pruning, because the arithmetic does not reach 24,000 without it (41,509 less 13,303 less 3,429 leaves ~24,800, before this plan's own additions); falsified by a measured pass where entombment alone clears the budget with headroom.
