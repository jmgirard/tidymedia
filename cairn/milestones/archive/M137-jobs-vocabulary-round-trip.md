# M137: `ffm_jobs()` lists every container the package itself writes

**Status:** done (2026-09-18, PR #141 https://github.com/jmgirard/tidymedia/pull/141)

**Goal:** A folder of files the package told the user to write becomes a jobs table.

**Outcome:** `media_extensions()` gained `"mka"` in the audio vector and `"ts"` in
the video vector (`R/ffm_jobs.R`). A folder of the multi-track audio output
`separate_audio_video()` recommends is now a jobs table, not a refusal. The no-match
abort in `tm_ffm_jobs()` gained one bullet naming `list.files()`, raised only on a
call that left `extension` unset. Three tests in `tests/testthat/test-ffm-jobs.R`
pin the three vectors, guard that every `multi_audio_extensions` member sits in one
of them, and sweep all nine round trips. One in `test-separate-av-multitrack.R`
writes a `.mka` and lists it back. `@return` and `NEWS.md` say a container able to
hold several audio streams is not always audio here, `.ts` being the case in point.

**Decisions:** M137-1, the round-trip rule: a container the package writes, or names
in its own diagnostics, sits in exactly one `media_extensions()` vector. The lists
stay closed otherwise, and M121-1's `list.files()` fallback stands for the rest.

**Review:** Three-lens fan-out, no returns, nine findings. Three were fixed before
the push and re-verified: a `.ts` overclaim on the help page, a missing comment on
why `ts` is video, unlabelled loop expectations. One went to a candidate row, one
was applied here, four were rejected. Local check 0/0/0, suite 0 failures of 17767,
ten CI checks green. No lesson added.
