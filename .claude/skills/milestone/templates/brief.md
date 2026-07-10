# Fable Brief: <topic>

- **Milestone:** M<NN>
- **Created:** YYYY-MM-DD
- **Required artifact:** `project/briefs/YYYY-MM-DD-M<NN>-<slug>-review.md`

## Instructions for the reviewing session

You are running in a clean session in the tidymedia repository with no prior
context — everything you need is in this brief and the files it points to.
Your job is expert technical review, not implementation:

1. Read the Context, then examine the listed files.
2. Answer every numbered question decisively; when options exist, recommend
   one and say why.
3. **Do not modify any package code.** Illustrative snippets belong inside
   your review artifact only.
4. Write your review to the Required artifact path above, following the
   format at the bottom of this brief, then stop.

## Context

Self-contained background: what the milestone is doing, what problem was hit,
what has been tried or considered. Include relevant excerpts from
project/DECISIONS.md rather than assuming they will be read.

## Files to examine

- `R/<file>.R` — why it is relevant, which functions/lines matter
- `project/DECISIONS.md` — D00X constrains this question because …

## Questions

1. Specific, answerable question — not "any thoughts?"
2. …

## Constraints on the answer

- Recommendations must respect DECISIONS.md (or explicitly argue a decision
  should be superseded, with a proposed new decision entry).
- State confidence (high/medium/low) per answer.
- Flag any additional problems noticed in the examined files, briefly.

## Required artifact format

```markdown
# Fable Review: <topic> (YYYY-MM-DD)

## Answers
Numbered, matching the brief's questions. Decisive, with reasoning and
confidence level.

## Recommendations
Concrete next actions for the orchestrator, in priority order.

## Risks / caveats
What could make these answers wrong; what to watch during implementation.

## Other observations
(optional) Problems noticed outside the questions' scope.
```
