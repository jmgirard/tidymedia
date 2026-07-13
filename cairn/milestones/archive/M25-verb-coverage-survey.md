# M25: Verb coverage survey (research-domain gap analysis) — done

**Goal:** Survey the Layer 2 task verbs a behavioral/affective-science
media-preprocessing pipeline needs, diff against what tidymedia exports, and give
every gap a scoped keep/refuse/defer verdict. Research/docs-only — no code.

**Outcome:** Produced `cairn/references/verb-coverage-survey-M25.md` — the 18
current task verbs classified into a 9-stage pipeline (have), the verbs each stage
demands (want), and a gap list. Verdicts: **2 keep** (`sample_frames` fixed-rate
frame sampling; `strip_metadata` de-identification), **3 defer** (region blur —
reconciled with the existing candidate; `burn_timecode`/drawtext; grouped minor
convenience verbs), **5 refuse** (face-tracking, content-based segmentation,
xstack/amix, bare one-filter verbs, subtitles — each citing GP1/D001/D009). Keep +
defer gaps registered as ROADMAP candidate rows; refusals recorded as standing
rationale (not candidates).

**Key points:**
- Anchored on the research domain (not analog-tool parity) by user choice;
  keep/refuse/defer rigor with rationale; research-only stop (no verbs built).
- Bar for a new task verb: it must encode a research task with non-obvious
  defaults/guards, else it belongs to the `ffm_*` builder / Layer 0 (GP1).
- Review: all 6 ACs passed on fresh evidence (have-table = NAMESPACE 18/18);
  `cairn_validate` clean; one send-back fixed (R3 GP1 citation, AC4).

**PR:** https://github.com/jmgirard/tidymedia/pull/27
