---
name: milestone
description: Milestone-driven development workflow for tidymedia. Use when planning, implementing, or reviewing a milestone, requesting a Fable review brief, ingesting a Fable review, or auditing project-tracking health. Triggers include "plan a milestone", "work on M01", "review the milestone", "fable brief", "project status".
argument-hint: plan | implement <id> | review <id> | brief <id> <topic> | status
---

# Milestone workflow

You are the orchestrator (Opus) for tidymedia development. All project state
lives in markdown under `project/`. This skill defines the only sanctioned way
to change that state.

## File map

- `project/ROADMAP.md` — milestone index and **the only authority on status**
- `project/milestones/M<NN>-<slug>.md` — one file per planned milestone
- `project/DECISIONS.md` — append-only cross-cutting decisions
- `project/briefs/` — open Fable briefs and their review artifacts
- `project/archive/` — compressed done milestones, resolved brief/review pairs
- Templates: `templates/milestone.md` and `templates/brief.md` in this skill's
  directory

## Non-negotiable rules

1. **Single source of truth.** Status lives in ROADMAP.md. Milestone files
   mirror it in their header; on any conflict ROADMAP.md wins and you fix the
   mirror immediately, before other work.
2. **One `in-progress` milestone at a time.** Never start a second.
3. **Absolute dates only** (YYYY-MM-DD). Never "yesterday" or "last week".
4. **Tracking travels with code.** Every commit that changes code also updates
   the work log / checkboxes in the same commit. Never let tracking drift from
   git history.
5. **Weight caps.** ROADMAP.md < 60 lines. Active milestone file < 150 lines.
   CLAUDE.md < 80 lines. Work-log entries are one line each; summarize, never
   paste command output or subagent transcripts into tracking files.
6. **Append, don't rewrite.** Work logs and DECISIONS.md are append-only.
   Supersede old decisions with new entries; never edit history.
7. **No status in CLAUDE.md.** It describes stable architecture and commands
   only; anything time-varying rots there.

## Git and approval model

- **master accepts only:** docs-only tracking commits (from `plan` and the
  post-merge hygiene pass) and squash-merges of milestone branches. Never
  implement on master.
- Each milestone is implemented on `milestone/M<NN>-<slug>`, branched from
  up-to-date master. Branch commits are cheap per-task checkpoints — the
  squash-merge erases them — so commit freely there. **Nothing reaches
  master without the user's explicit final approval at review.**
- Never force-push; never merge with red or pending CI.

User interaction happens at three gates, and only there:

1. **Plan questions** — before the plan is solidified.
2. **Implementation questions** — after branching, before code is written;
   plus the recap stop when implementation finishes.
3. **Final approval at review** — the only thing that authorizes a merge.

At a question gate, ask a small batch (2–5) of concrete decision questions,
each with a recommendation and brief pros/cons of the options (use the
AskUserQuestion tool where available). Between gates, work autonomously —
never drip questions one at a time mid-work.

## Session start protocol (every subcommand)

Read, in order: `project/ROADMAP.md`, the active/target milestone file, and
`project/DECISIONS.md`. Read nothing else from `project/` unless a step below
requires it. Then check `project/briefs/`: if a `*-review.md` artifact exists
for an open brief, run **brief ingestion** (below) before anything else.

## /milestone plan [title]

1. Confirm no other milestone is `in-progress` (or get user sign-off to plan
   ahead anyway).
2. Investigate first: read the relevant code and DECISIONS.md; draft scope,
   tasks, and the list of genuinely open decisions internally.
3. **Question gate:** pose those open decisions to the user — scope
   boundaries, sequencing, acceptance bar — with recommendations and
   pros/cons. One batched round.
4. With answers in hand, solidify autonomously (no further questions).
   Create `project/milestones/M<NN>-<slug>.md` from `templates/milestone.md`:
   - Acceptance criteria must be **verifiable with evidence** (a test passes,
     `devtools::check()` output, CI status), never vibes ("code is cleaner").
   - Tasks sized to one working session or less, ordered by dependency.
   - Scope must have explicit **Out** items — what this milestone refuses.
5. Update ROADMAP.md (status `planned`, link the file); commit tracking to
   master (docs-only): `plan M<NN>: <title>`. Present the plan and stop.

## /milestone implement <id>

1. Session start protocol. Verify status is `planned`, `in-progress`, or
   `blocked`-with-resolved-brief; set to `in-progress` in ROADMAP.md + header.
2. First implement session only: update master, then
   `git checkout -b milestone/M<NN>-<slug>`. Later sessions resume the
   existing branch.
3. **Question gate:** before writing code, surface the implementation
   choices the plan left open (API shape, naming, dependency picks) with
   recommendations and pros/cons. Skip only if nothing is genuinely open.
4. Then work tasks in order, autonomously. For each task:
   - **Tests first** where feasible (testthat 3e). New/modified user-facing
     errors use `cli::cli_abort()` / rlang checks — never assertthat.
   - After roxygen changes run `devtools::document()`; after README.Rmd
     changes run `devtools::build_readme()`.
   - Gate: `devtools::test()` clean before checking the task off.
   - Checkpoint-commit per task on the branch, including the milestone-file
     update (checkbox + one work-log line). These are squashed away later;
     commit freely.
5. **Delegation policy:**
   - **Sonnet subagents** (`model: "sonnet"`): well-specified, self-contained
     tasks — mechanical migrations, test writing against a spec, broad code
     searches (Explore agent). Give complete specs; verify their diffs
     yourself before committing.
   - **Opus subagents** (`model: "opus"`): design-sensitive implementation or
     an independent review pass. Use when a fresh perspective matters or work
     can proceed in parallel worktrees.
   - **Never Haiku**, for anything, ever.
   - **Never spawn Fable.** Fable input arrives only via the brief protocol.
   - Summarize subagent results into one work-log line; never paste output.
6. If blocked on a technical question that needs stronger review: run
   `/milestone brief` and stop the turn.
7. When all tasks are checked and `devtools::check()` is clean: set status
   `review`, then **stop with a recap**: what changed (file-level summary of
   the branch diff), test/check results, deviations from the plan, and open
   concerns. Do not push or open a PR yet. End the turn by calling
   **`AskUserQuestion`** (not a prose "you can discuss or I can proceed…"
   invitation) so the user gets a selector. Ask a single question — e.g.
   "How should I proceed with M<NN>?" — with these options, recommended first:
   - **Proceed to review** — advance to `/milestone review <id>` (push branch,
     open draft PR, verify criteria, Opus review).
   - **Adjust first** — make changes on the branch before review.
   - **Pause here** — stop; leave the milestone at `review`.
   Honor "Other" free-text as adjustment instructions. Only advance to review
   when the user selects it — the selector is a stop, not an auto-proceed.

## /milestone review <id>

1. Session start protocol. Status must be `review` (or user overrides).
2. Push the branch and open a **draft PR** (`gh pr create --draft`) so CI
   runs in the background while the review proceeds.
3. Verify each acceptance criterion **with fresh evidence** — run the tests,
   run `devtools::check()`. Record results in the Review section. Any
   failure → status back to `in-progress` with a work-log line saying
   exactly what failed; stop.
4. Independent code review: spawn an **Opus subagent** to review the
   milestone's full diff (`git diff master..HEAD`) for correctness and design
   drift against DECISIONS.md. Triage its findings: fix now, spawn follow-up,
   or reject with reason (logged).
5. Update NEWS.md under the development version heading; final checkpoint
   commit on the branch.
6. **Present the review and request final approval:** acceptance-criteria
   evidence, problems found and how each was handled, diffstat, and anything
   the user should eyeball directly. Ask any remaining questions first (with
   recommendations), but end by asking plainly for approval to merge.
7. **On approval — and only then:** mark the PR ready, confirm CI is green
   (poll if pending). If CI is red, fix on the branch, re-verify, and
   re-request approval if the fix was nontrivial. When green:
   `gh pr merge --squash --delete-branch` with a clean summary message.
   If approval is withheld: log the requested changes as tasks, status back
   to `in-progress`, stop.
8. Post-merge, on master: **hygiene pass** — compress the milestone file to
   a ≤25-line summary (goal, outcome, key decisions, PR link) and move it to
   `project/archive/`; update its ROADMAP.md row (status `done`, archive
   path); move resolved brief/review pairs to archive; update "Last hygiene
   check"; verify weight caps. Commit (docs-only): `review M<NN>: done`.
   Suggest (don't start) the next milestone.

## /milestone brief <id> <topic>

For technical questions needing Fable-level review. Fable runs in a **clean
session started manually by the user** — the brief must be fully
self-contained (assume zero conversation context).

1. Create `project/briefs/YYYY-MM-DD-M<NN>-<slug>-brief.md` from
   `templates/brief.md`: background, exact files/lines to examine, numbered
   specific questions (not "thoughts?"), constraints, and the required
   artifact path `.../YYYY-MM-DD-M<NN>-<slug>-review.md`.
2. Set milestone status to `blocked` (work-log line: "blocked on Fable brief
   <file>"). Commit.
3. Tell the user verbatim how to run it, e.g.:
   > Open a fresh session with Fable in the repo root and prompt:
   > `Read project/briefs/<file> and follow its instructions exactly.`
4. **Stop the turn.** Do not proceed past the blocking question.

**Brief ingestion** (automatic at session start when an artifact appears):
read the review artifact; record its answers as dated entries in the
milestone's Decisions (promote cross-cutting ones to DECISIONS.md); apply or
schedule its recommendations as tasks; move the brief+review pair to
`project/archive/`; set status back to `in-progress`; commit.

## /milestone status

Health audit; fix mechanical problems immediately, report the rest:

- ROADMAP.md vs milestone headers consistent; ≤1 `in-progress`.
- Weight caps respected (rule 5).
- Staleness: any `in-progress` milestone with no work-log entry in 14+ days;
  any open brief with no artifact after 7+ days (remind the user to run it).
- Orphans: `done` milestones not archived; review artifacts not ingested;
  milestone files missing from ROADMAP.md or vice versa.
- Uncommitted changes touching `project/`.
- Update "Last hygiene check" in ROADMAP.md.
