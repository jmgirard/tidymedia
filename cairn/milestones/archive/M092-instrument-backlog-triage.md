# M092: The deferred-findings backlog is triaged and the page retired

**Status:** done (2026-08-30, PR #96 https://github.com/jmgirard/tidymedia/pull/96)

**Goal:** Close the four instrument gaps that could let a defect in shipped behaviour
reach a user, prune the rest of `cairn/references/instrument-findings.md` with each
pruning's reason recorded, and retire the page.

**Outcome:** All 53 still-open findings triaged in one ledger, one row per `## M`
heading, each id classed instrument or runtime and marked close or prune with its own
reason. Four closed, each a gap with a path to a user: `run_with_progress()`'s return
contract gets a binary-free test off `skip_if_no_ffmpeg()`; `assemble_measured()`'s
`tm_row_status` is driven through a real failing Phase 1 row, not a wholesale mock;
the class-pairing probes move from `condition =` to `error =`, so a warning raised
before the abort no longer gets asserted against help topics; and
`holds_multiple_audio()`'s case fold is exercised at the batch site, not the scalar
site alone. The page, its `INDEX.md` line and its ROADMAP row are deleted; three rows
carry forward (M071 F9, M70 O11, M087's topic-vs-site design call). No `R/` change.

**Decisions:** D072.

**Review:** Two passes. Pass 1 returned on `cairn_validate`'s 150-line plan-owned cap
(206); the repair compressed the ledger to 22 lines. Pass 2: three-lens fan-out, 14
findings, none meeting the return floor — five criterion-wording findings held at the
maintainer's decision, five record-accuracy findings fixed at the gate, four rejected.
