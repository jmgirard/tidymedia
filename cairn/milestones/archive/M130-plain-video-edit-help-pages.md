# M130: The crop, web-format, standardize and strip-metadata help pages read as plain English

**Status:** done (2026-09-14, PR #134 https://github.com/jmgirard/tidymedia/pull/134)

**Goal:** The M130 help-page domain uses plain English for an R user who does not know FFmpeg.

**Outcome:** The roxygen text in `R/ffmpeg.R` is rewritten under D093, so it changes form, not claims. The pages are `crop_video()`, `format_for_web()`, `standardize_video()` and `strip_metadata()`, and their batch pages. The sweep over the 8 pages goes from 48 findings to none. All 8 pages name the glossary. The `?standardize_video` resolution rules are a three-item list. The M129 shared `video_codec` text on `crop_video_batch` stays as it was. No test pinned changed wording, so no test changed. The ledger is `### M130` in `cairn/references/plain-docs.md`. Ten false base claims are the M130 follow-up row in `ROADMAP.md`.

**Decisions:** none.

**Review:** One pass with the three-lens fan-out, no returns. All six criteria passed. The [S] prior-review lens found no prior-review evidence. Six slips the branch added were fixed at the gate and read by a fresh reader. O1: `?strip_metadata` Details made re-encoding read as the only way to remove per-stream data. O2: a dangling "one" in `?crop_video_batch` hardware text. O3: "GPS location" and "the task function for IRB de-identification" narrowed the base claim. O4: an unclear "it" in `?strip_metadata` See Also. O5: "fixes both codecs by identity" lost its meaning. S2: "single-file function" became the existing "single-input form". O6 (source line breaks) and S1 (restore "front door", a maintainer term) were rejected. P1-P5 from the [O] lens and P6 from the fresh reader are pre-existing false claims, added to the follow-up row as (e)-(j). The first CI wait reached its time limit, and the resumed review found CI 10/10 green. No lesson added.
