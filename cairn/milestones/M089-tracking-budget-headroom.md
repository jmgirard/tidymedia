# M089: The guard-ordering family graduates and the tracking files get headroom

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

`cairn/LESSONS.md` and `cairn/ROADMAP.md` each clear their byte budget by more
than one average entry, reached by graduating the front-door guard-ordering
family into its own doctrine module rather than by raising a figure this repo
does not own.

## Scope

**Surface tier: internal.** Every deliverable is a file under `cairn/`; no
external consumer of the package relies on one.

**In:** the call the standing budget candidate row forces, made and recorded.
The front-door guard-ordering and precedence family — the two `LESSONS.md`
entries dated 2026-07-29 (M41) and 2026-07-30 (M47), 3,219 bytes over 2 lines —
graduates whole into `cairn/references/guard-ordering.md` under the maturation
exit. The ROADMAP absorbs the two candidate rows this milestone promotes and is
compressed to a stated figure. A D-entry records which budgets this repo owns
and what remedy each of the three tracked files got.

**Out:**
- Raising `ROADMAP.md`'s or `LESSONS.md`'s byte budget. Those figures are the
  cairn plugin's (`skills/shared/tracking-rules.md`, "Weight caps": line cap ×
  400); a D-entry in this repo cannot supersede plugin doctrine. Changing them
  is the user's call against the plugin → not a milestone here.
- Raising `references/false-greens.md`'s budget. That one IS repo-owned, set by
  M084 in the module's own header, but the header states the remedy over budget
  is compressing or retiring content, never growth. Superseding that → its own
  milestone, promotable if the module ever breaches 26,000 bytes.
- Any third doctrine module. The remaining `LESSONS.md` families have not met
  the maturation exit's twice-extended test → stays unfiled; propose a row only
  when one does.
- The five M088 findings, the unclassed-abort naming pass, and every other
  candidate row → their existing ROADMAP rows, compressed but not promoted.

## Acceptance criteria

- [ ] AC1 `cairn/references/guard-ordering.md` exists, carries exactly the two
      graduated entries one per line, and its header states a line budget and a
      byte budget set from the graduated size plus headroom the header itself
      states (the maturation exit's rule).
- [ ] AC2 Each of the module's two entry lines is byte-identical to the
      `cairn/LESSONS.md` line it left: `diff` between the module's entry lines
      and those same lines extracted from `cairn/LESSONS.md` at this
      milestone's base commit reports no difference.
- [ ] AC3 `wc -c cairn/LESSONS.md` reports fewer than 17,500 bytes and
      `wc -l` reports 28 or fewer, and `grep -F` for a distinctive phrase from
      each graduated entry finds neither in the file.
- [ ] AC4 `wc -c cairn/ROADMAP.md` reports fewer than 22,000 bytes and `wc -l`
      reports 44 or fewer, measured after this milestone's own row and its
      replacement hygiene stamp are in the file.
- [ ] AC5 `wc -l -c cairn/references/false-greens.md` reports fewer than 60
      lines and fewer than 26,000 bytes, and the figures in the module's own
      header are unchanged from the base commit.
- [ ] AC6 `cairn/DECISIONS.md` gains one entry naming, for each of the three
      tracked files, whether its budget is repo-owned or the plugin's and which
      remedy it got here.
- [ ] AC7 Every ROADMAP candidate row this milestone's diff shows as edited
      (enumerated by `git diff --unified=0` against the base commit) still
      names a promote trigger in its post-change text.
- [ ] AC8 `python3 .../cairn_validate.py` exits clean, and every plan-owned
      section of this file is under the 150-line cap it reports.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T3
- AC4 → T5, T6
- AC5 → T7
- AC6 → T8
- AC7 → T6
- AC8 → T9

## Tasks

- [ ] T1 Confirm the maturation exit for the family: read `cairn/LESSONS.md`
      lines 16 and 23, count the extension/consolidation/correction marks each
      carries, and confirm neither the enforcement nor the ownership exit
      applies. Record the count.
- [ ] T2 Create `cairn/references/guard-ordering.md` — header modelled on
      `false-greens.md`'s (provenance naming M089 and the originating
      milestones, the stated budget with its headroom, the one-entry-per-line
      rule), then the two entries pasted verbatim.
- [ ] T3 Delete those two lines from `cairn/LESSONS.md`; verify byte-identity
      of the moved text by `diff` against the base commit; check `wc -l -c`.
- [ ] T4 Add the module to `cairn/references/INDEX.md` under "Working
      artifacts", in the form the other five rows use.
- [ ] T5 In `cairn/ROADMAP.md`: delete the two candidate rows this milestone
      absorbs (the second-doctrine-module row and the budget row), add the M089
      row, replace the hygiene stamp.
- [ ] T6 Compress the widest remaining candidate rows until `wc -c` is under
      22,000 and `wc -l` is 44 or fewer, checking each edited row after the
      edit that its promote trigger survived.
- [ ] T7 Confirm `cairn/references/false-greens.md` is untouched: `git diff`
      empty for that path, `wc -l -c` under its header's figures.
- [ ] T8 Append the D-entry (next free id) recording the call and the
      repo-owned/plugin-owned split.
- [ ] T9 Run `cairn_validate.py`; fix what it reports; confirm clean.


- 2026-08-29: created by /milestone-plan.
- 2026-08-29: criteria audit ran in REDUCED mode (surface tier internal, no RB-tripwire tag on any criterion or task). Deviation from the skill's fresh-context [O] reader: this session's standing instruction forbids spawning subagents unrequested, so the audit was run inline by the plan author. Returned two findings, both fixed here. (1) Bounded-promise: a draft AC3 read "no lesson is lost in the graduation" — universal over "lessons" as content, which no procedure enumerates; narrowed to byte-identity of two named lines plus a `grep -F` absence check. (2) Proportionality: a draft AC4 read "the ROADMAP never needs compressing at a milestone commit again" — quantified over all future commits, unenumerable and disproportionate to an internal-tier deliverable; narrowed to a byte figure with stated headroom. AC6's D-entry was weighed against D-118/D-120's instrument question and kept as deliverable-bound: this milestone's deliverable IS the call, and the D-entry is the call's durable form, not a record of the milestone's own verification.
- 2026-08-29: plan gate chose graduating the family plus compressing over raising the budgets, because `ROADMAP.md`'s 24,000 and `LESSONS.md`'s 20,000 live in the cairn plugin's `skills/shared/tracking-rules.md` as line cap × 400 and no repo D-entry supersedes plugin doctrine, and because `false-greens.md`'s repo-owned budget is stated by its own header to be met by compressing, never growth; falsified by the user changing the plugin's Weight caps section, or by M084's module header being superseded.
- 2026-08-29: plan gate chose the maturation exit over pruning the stalest `LESSONS.md` entries, because tracking-rules names prune-the-stalest "the last resort" and the family clears the twice-extended test (6 marks measured across the two lines against a requirement of 2); falsified by T1 finding fewer than two extension marks, or by the enforcement or ownership exit turning out to apply.
- 2026-08-29: split tripwires weighed and none fired decisively — 8 criteria and 9 tasks sit at the >~7 / >~10 edges without crossing, and a second milestone would add a ROADMAP row consuming the headroom the first one makes, so this stays one milestone.

## Decisions

## Review
