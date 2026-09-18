# M134: The anonymize, segment, concatenate, compare and picture-in-picture help pages read as plain English

**Status:** done (2026-09-17, PR #138 https://github.com/jmgirard/tidymedia/pull/138)

**Goal:** The M134 help-page domain uses plain English for an R user who does not know FFmpeg.

**Outcome:** The roxygen text in `R/ffmpeg.R` is rewritten under D093, so it changes form, not claims. The pages are `anonymize_video()`, `segment_video()`, `concatenate_videos()`, `compare_videos()` and `picture_in_picture()`, with their five batch pages. The sweep over the 10 pages goes from 56 findings to none, over 621 sentences at head. 9 pages name the glossary, and `concatenate_videos_batch` has no stem. "builder", "blessed", "verb", "scalar", "sibling", "knobs", "toggle", "transcode", "fan-out" and the `(D015)` citations are gone from these pages. The claim audit read 63 claims and corrected none. No test file changed. No base claim was found false. The ledger is `### M134` in `cairn/references/plain-docs.md`.

**Decisions:** none.

**Review:** One pass with the three-lens fan-out, no returns. All six criteria passed. The [S] prior-review lens found no regression. The [S] blame-history lens found no lost caveat and one item, S1, the dropped `(D015)` citations, rejected because the plan called for it. The [O] lens gave 5 findings. O5 was fixed at the gate: the ledger heading now names the head result column. O1 (AC6 unticked before its checks ran) needed no change. O2 (`?segment_video` says "an integer" where the code zero-pads) was rejected at the gate as imprecise, not false. O3 (`color` called an encoding argument, inherited) was noted. O4 (`@seealso` separators) was rejected as style. The CI wait reached its time limit after approval, and the resumed review found CI 10/10 green. The PR conversation held one Codecov bot comment, noted. No lesson added.
