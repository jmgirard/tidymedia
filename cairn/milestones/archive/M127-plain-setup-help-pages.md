# M127: ?tidymedia and the setup, metadata, timeout and batch help pages read as plain English

**Status:** done (2026-09-13, PR #131 https://github.com/jmgirard/tidymedia/pull/131)

**Goal:** The M127 help-page domain, including `?tidymedia`, uses plain English for an R user who does not know FFmpeg.

**Outcome:** 28 pages from 13 `R/` files pass the prose sweep, which printed 185 findings at base. `?tidymedia` renders 78 lines. Its timeout detail and class lists moved to `?with_timeout` and `?local_timeout`, and the timings moved to the comment at the top of `R/timeout.R`. The generated `audio_stream` text on 25 task pages is rewritten, and `audio_stream_family_sentence()` gained `after_null`. The `@family` labels are now task functions, pipeline functions and direct command functions. About 30 help claims were corrected against the code in the claim audit and review. No behavior change. Wording tests were re-pinned and none removed. The ledger is `### M127` in `cairn/references/plain-docs.md`.

**Decisions:** Implement gate: the timeout classes live on `?with_timeout`. Only `?find_ffmpeg` explains the 0.1.0 settings location. The sweep parse gaps are not fixed here. Amendment return: AC5 checks only the 27 base identifiers, because no procedure can list reworded facts. The approach change for the rest of the series is D093.

**Review:** Four rounds, each with the three-lens fan-out. Two defect returns: round 1 for a spelling NOTE and a dropped `check_tracks` fact, and round 3 for a glossary stem that a round-2 fix added. One amendment return, on AC5 in round 2. The rounds found 16, 12, 16 and 11 problems. Several fixes added new false claims that the next round found. In round 4, R4-1, R4-2, R4-4 and R4-11 were fixed at the gate, and R4-3 and R4-5 to R4-10 went to a candidate row. CI 10/10 green. No lessons added or retired, because LESSONS is at its byte budget and D093 records the lesson.
