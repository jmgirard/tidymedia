# M131: The filter and multi-input pipeline help pages read as plain English

**Status:** done (2026-09-17, PR #135 https://github.com/jmgirard/tidymedia/pull/135)

**Goal:** The M131 help-page domain uses plain English for an R user who does not know FFmpeg.

**Outcome:** The roxygen text in `R/ffm.R` is rewritten under D093, so it changes form, not claims. The pages are `ffm_trim()`, `ffm_crop()`, `ffm_scale()`, `ffm_fps()`, `ffm_drawbox()`, `ffm_loudnorm()`, `ffm_hstack()`, `ffm_vstack()`, `ffm_overlay()` and `ffm_concat()`. The sweep over the 10 pages goes from 14 findings to none. The 6 pages with a glossary stem name the glossary. "Layer-2", "blessed" and "verb" are gone from these pages. The claim audit read 110 claims and put 6 back to the base claim. No test file changed. The ledger is `### M131` in `cairn/references/plain-docs.md`. Four false base claims are the M131 part of the `ffm_*()` help-text follow-up row in `ROADMAP.md`.

**Decisions:** none.

**Review:** One pass with the three-lens fan-out, no returns. All six criteria passed. The [S] blame-history and prior-review lenses found nothing. The [O] lens gave 12 findings. O1 and O2 (`?ffm_concat` wording), O5 and O6 (`?ffm_loudnorm` unclear subject and a split sentence) were fixed at the gate. O3 (a glossary count of 7 that is 6) was fixed in the ledger. O4 and O7 were fixed in the follow-up row. O11 (`linear = FALSE` claim against the FFmpeg default) is item (d) of that row. O8 and O10 (base text kept), O9 (a move that rule 6 allows) and O12 (ledger layout) were rejected. The session stopped after approval, and the resumed review on 2026-09-17 found CI 10/10 green. The PR conversation held one Codecov bot comment, noted, that requests nothing. No lesson added.
