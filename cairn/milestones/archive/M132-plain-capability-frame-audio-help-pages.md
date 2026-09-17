# M132: The FFmpeg-capability, frame and audio conversion help pages read as plain English

**Status:** done (2026-09-17, PR #136 https://github.com/jmgirard/tidymedia/pull/136)

**Goal:** The M132 help-page domain uses plain English for an R user who does not know FFmpeg.

**Outcome:** The roxygen text in `R/ffmpeg.R` is rewritten under D093, so it changes form, not claims. The pages are `ffmpeg()`, `ffmpeg_codecs()`, `ffmpeg_encoders()`, `hardware_encoder()`, `extract_frame()`, `sample_frames()`, `extract_audio()` and `convert_audio()`, with the four batch pages. The sweep over the 12 pages goes from 45 findings to none, over 481 sentences at head. The 10 pages with a glossary stem name the glossary. "toggle", "verbs", "front door" and "sentinel" are gone from these pages. The claim audit read 118 claims and corrected 3. No test file changed. The inherited `run`, `parallel` and `...` text on `?extract_audio_batch` stayed as it was. The ledger is `### M132` in `cairn/references/plain-docs.md`. One base claim on `?hardware_encoder` is doubtful: a `.webm` output needs an HEVC- or AV1-family codec. It is the M132 part of the `ffm_*()` help-text follow-up row in `ROADMAP.md`.

**Decisions:** none.

**Review:** One pass with the three-lens fan-out, no returns. All six criteria passed. The [S] blame-history lens found nothing. The [O] lens gave 6 findings and the [S] prior-review lens gave 1. Four were fixed at the gate. O1: `?hardware_encoder` said the task functions use the two exported functions, but they call internal ones. O2: the same page said functions "use this page". O3: "the package supports" became "the chosen backend has an encoder for", and the ledger row was corrected. P1: two pronouns in a row on `?convert_audio`. O4 and O5 (same claim as the base) and O6 (AC6 not yet ticked during the read) were rejected. The first O1 fix ran to 29 words and the sweep caught it. The CI wait reached its time limit after approval, and the resumed review found CI 10/10 green. The PR conversation held one Codecov bot comment, noted, that requests nothing. No lesson added.
