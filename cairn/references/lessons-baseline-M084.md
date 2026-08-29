# LESSONS baseline and exit classification (M084)

_The classification ledger M084 committed: every `- ` entry in `cairn/LESSONS.md`
at M084's branch point, with its byte length, its key, and the exit it was
classified under. The key is the entry's first 120 bytes — what `cut -c1-120`
returns here, `cut -c` counting bytes on this platform — with a trailing partial
character dropped so this file stays valid UTF-8; that shortening never affects a
`grep -F` match. The 44 keys are unique, so each identifies one entry. Produced
2026-08-28 by M084 from commit 31a8e4f; the file it enumerates has changed since._

_**Dispositions.** `graduate` — moved whole into `cairn/references/false-greens.md`
under the maturation exit. `trim` — the covered half moved to that module or to the
slot that owns it, the remainder left in `cairn/LESSONS.md`. `keep` — left whole in
`cairn/LESSONS.md`._

**Provenance.** Ingested 2026-08-28 by M084 from `cairn/LESSONS.md` at commit
`31a8e4f`, the branch point of `m084-lessons-budget-doctrine` — a first-hand
record of this repo's own files, not an external source. The rows are what
`grep '^- ' cairn/LESSONS.md` enumerates at that commit, in file order.
Pagination: —.
Extraction: byte lengths and keys were computed from the frozen blob, which git
still holds, so every row is recomputable from its origin at any time and there
is nothing to re-verify against a moving source — observed 2026-08-28.

| # | bytes | disposition | key |
|---|---|---|---|
| 1 | 706 | graduate | `- 2026-08-06 (M52): a parity baseline is only as good as the fixture family under it. Five fixtures built from lavfi sou` |
| 2 | 710 | keep | `- 2026-08-06 (M52): FFprobe's two writers disagree about nested-section keys in more than casing. `default=nw=1` prints ` |
| 3 | 356 | keep | `- 2026-07-13 (M26): a `_batch` verb that auto-derives output paths from the input *basename* silently overwrites when tw` |
| 4 | 541 | keep | `- 2026-08-06 (M52): R's character-based string functions are locale traps on text a process handed back. `strsplit(x, ""` |
| 5 | 1087 | trim | `- 2026-07-30 (M45, consolidating M27): FFmpeg behavior differs by version and the two CI platforms straddle those differ` |
| 6 | 1261 | graduate | `- 2026-07-26 (M31): skip a hardware-encoder execution test on run-time usability, not merely that the encoder is *listed` |
| 7 | 467 | keep | `- 2026-07-26 (M32/M37): a `_batch` per-row override *column* skips the scalar argument's guards (`check_number_whole`/ra` |
| 8 | 402 | keep | `- 2026-07-26 (M34): guarding a `_batch` override column whose documented `NA` means "unset" — R types an all-NA column` |
| 9 | 343 | graduate | `- 2026-07-27 (M35): an execution test proving a stream copy survived must use a source codec that is NOT the output cont` |
| 10 | 347 | keep | `- 2026-07-26 (M36): in a cli message `{.val NULL}` renders the *string* `"NULL"` — quoted, reading as a value to pass ` |
| 11 | 302 | keep | `- 2026-07-26 (M36): when a check is hoisted into a two-pass/batch prelude to fail before an expensive analysis phase, ho` |
| 12 | 346 | keep | `- 2026-07-26 (M37): removing an argument from a `_batch` verb is only half a clean break — the verb's `...` swallows t` |
| 13 | 1090 | keep | `- 2026-07-26 (M38): an `arg_match()` argument compared before it is resolved silently misreads its own default — `hard` |
| 14 | 921 | keep | `- 2026-07-26 (M38/M40): a `cli_abort()` remediation hint must be true under the condition that FIRED the guard, not in g` |
| 15 | 332 | graduate | `- 2026-07-26 (M39): a `_batch` verb's batch-wide argument needs a test naming a NON-default value — asserting only the` |
| 16 | 1477 | trim | `- 2026-07-29 (M41): a front-door guard added only to improve a *message* silently reassigns error PRECEDENCE — every c` |
| 17 | 2127 | graduate | `- 2026-07-29 (M41): a before/after grid proves contract-neutrality only over the dimensions it VARIES — three review r` |
| 18 | 608 | keep | `- 2026-07-30 (M42): `if (!is.null(x)) check_string(x)` and `check_string(x, allow_null = TRUE)` accept identical values ` |
| 19 | 445 | graduate | `- 2026-07-30 (M42): line coverage cannot see an untested *shared* type-guard call — `check_batch_codec_col(jobs, "vide` |
| 20 | 336 | keep | `- 2026-07-26 (M39): adding an override column to a `_batch` verb also falsifies its `@param jobs` prose, which enumerate` |
| 21 | 597 | keep | `- 2026-07-30 (M43): adding an explicit `-map` REPLACES FFmpeg's default stream selection wholesale, and that default car` |
| 22 | 595 | graduate | `- 2026-07-30 (M43): a fixture built to prove a stream-selection change can silently defeat itself, because the ffmpeg co` |
| 23 | 624 | keep | `- 2026-07-30 (M44): `cli::cli_warn()`/`cli_abort()` glue-interpolate EVERY bullet in the calling frame, so a bullet buil` |
| 24 | 2114 | graduate | `- 2026-07-30 (M44): a mock that `stop()`s to prove a code path is never taken proves NOTHING when the call site wraps it` |
| 25 | 566 | keep | `- 2026-07-30 (M46): `-shortest` tracks the shortest stream INCLUDING subtitles, so on a fixture whose `.srt` is shorter ` |
| 26 | 582 | keep | `- 2026-07-30 (M46, trimmed M69): R translates the `system(..., timeout =)` warning, so muffling or detecting a timeout b` |
| 27 | 1247 | keep | `- 2026-07-30 (M47): a verb that STATES its stream selection must tolerate an input that lacks the stream type, because F` |
| 28 | 1998 | trim | `- 2026-07-30 (M47, extends M41; absorbs M45's reshaped-index line; corrected M48): "last in the front door" is not "last` |
| 29 | 565 | graduate | `- 2026-07-30 (M47): a fixture whose property under test COINCIDES with the default makes the test pass with the feature ` |
| 30 | 713 | keep | `- 2026-07-30 (M48): a verb's BRANCHES need not emit the same command shape, and writing about "the verb" hides it — `s` |
| 31 | 557 | graduate | `- 2026-07-31 (M50): re-baselining a rendered form across the suite silently voids NEGATIVE assertions. `expect_false(gre` |
| 32 | 724 | keep | `- 2026-07-31 (M50): a scripted sweep over test files must parse the value with a GRAMMAR, never as "the next token". `[^` |
| 33 | 426 | graduate | `- 2026-07-31 (M50): snapshot expectations are NOT compared under `testthat::test_file()` after `devtools::load_all()` ` |
| 34 | 1785 | graduate | `- 2026-07-31 (M51, extended M59): a test that reads `man/*.Rd` from the source tree SKIPS under `R CMD check`, where tes` |
| 35 | 1045 | keep | `- 2026-08-06 (M53): fanning a NON-EXPORTED function out with furrr is correct in the shipped package and broken under `d` |
| 36 | 1460 | graduate | `- 2026-08-07 (M54, extended M58): a test that reads an error to prove WHICH call is blamed passes for the wrong reason o` |
| 37 | 918 | keep | `- 2026-08-07 (M55): giving a package its `?<pkg>` landing topic has two traps, and the usethis template walks into one o` |
| 38 | 1257 | trim | `- 2026-08-08 (M60): normalizing line endings leaves two things NOT normalized, and both read as success. (a) `git add --` |
| 39 | 765 | trim | `- 2026-08-26 (M69): base R's `timeout =` bounds the WAIT, not the process — it escalates SIGINT at the limit, SIGTERM ` |
| 40 | 669 | graduate | `- 2026-08-26 (M70): a test grid that forces a timeout VERDICT rather than the SPAWN still shells out, so on a runner wit` |
| 41 | 486 | graduate | `- 2026-08-27 (M071): a test that greps the package's own `R/` sources must key its skip on finding a known `.R` file, ne` |
| 42 | 2449 | graduate | `- 2026-08-27 (M072): `skip_on_cran()` cells are evidence only when the runner sets `NOT_CRAN` — `devtools::test()` doe` |
| 43 | 4851 | graduate | `- 2026-08-27 (M074): pinning a dependency version by setting `R_LIBS` PREPENDS a library, it does not isolate one — th` |
| 44 | 746 | trim | `- 2026-08-27 (M075): `normalize_audio()`'s `if (two_pass)` block does NOT return — it falls through into the single-pa` |
