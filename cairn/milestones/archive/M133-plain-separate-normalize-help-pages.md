# M133: The separate-audio-video and normalize-audio help pages read as plain English

**Status:** done (2026-09-17, PR #137 https://github.com/jmgirard/tidymedia/pull/137)

**Goal:** The M133 help-page domain uses plain English for an R user who does not know FFmpeg.

**Outcome:** The roxygen text in `R/ffmpeg.R` is rewritten under D093, so it changes form, not claims. The pages are `separate_audio_video()` and `normalize_audio()`, with the two batch pages. The sweep over the 4 pages goes from 103 findings to none, over 526 sentences at head. All 4 pages name the glossary. "escape hatch", "verb", "scalar", "sibling", "knobs", "transcode", "best-effort" and "fans out" are gone from these pages. The four-condition sentences on the two separate pages became lists. The claim audit read 116 claims and corrected 4. No test file changed, and the four phrases that `test-ffmpeg-exit-condition.R` pins stay, each on one source line. The ledger is `### M133` in `cairn/references/plain-docs.md`. One base claim on `?normalize_audio_batch` is doubtful: that `loudness_range = 7` follows EBU R 128. It is the M133 part of the `ffm_*()` help-text follow-up row in `ROADMAP.md`.

**Decisions:** none.

**Review:** One pass with the three-lens fan-out, no returns. All six criteria passed. The [S] prior-review lens found nothing. The [O] lens gave 6 findings and the [S] blame-history lens gave 4. Five were fixed at the gate. O1: the `@return` of `separate_audio_video_batch()` had lost the `run = TRUE` condition on `verified` and the manifest, through the claim audit's own and/or correction. O2: "Each bullet" became "Each bullet of the warning". O4: "the same row numbers" is back. O5: the "because" is back on the reason the function derives no output paths. O6: the ledger row now records the See Also replacement. O3 (two work log page counts one low, ledger total right) was noted. S1 to S3 (term replacements the plan called for) and S4 (glossary sentences that AC3 requires) were rejected. The CI wait reached its time limit after approval, and the resumed review found CI 10/10 green. The PR conversation held one Codecov bot comment, noted, that requests nothing. No lesson added.
