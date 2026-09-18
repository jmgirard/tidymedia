# M137: `ffm_jobs()` lists every container the package itself writes

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — it changes which files an exported function returns
- **Branch/PR:** —

## Goal

A folder of files the package told the user to write becomes a jobs table.

## Scope

**In:** `"mka"` added to `media_extensions("audio")` and `"ts"` added to `media_extensions("video")` (`R/ffm_jobs.R:164`), the two members of `multi_audio_extensions` (`R/ffmpeg.R:664`) that no vector listed. A standing test that keeps the two vocabularies aligned as either grows. A bullet on the no-match abort naming the way to reach a container the scanned set omits, raised only when the caller did not narrow the search. A `@return` sentence and a `NEWS.md` entry disclosing that `.ts` also names TypeScript source files, which are now video rows.

**Out:** widening `extension` to accept a container outside the type's set. The recorded choice is that such a caller uses `list.files()` directly, and reversing it is a separate decision → candidate row "Six deferred `ffm_jobs()` items" (c). The video list's missing `ogv` → the same row. Closing the lists over the extension families FFmpeg declares → rejected at the plan gate, measured below.

## Acceptance criteria

- [ ] AC1: For every element of `multi_audio_extensions` (`R/ffmpeg.R:664`, nine elements, which this milestone does not change), exactly one `media_extensions()` vector holds that element, and given a directory holding one readable file whose name ends in it, `ffm_jobs(directory, type = <the type of that holding vector>, extension = <element>)` and the same call with `extension = NULL` each return a one-row tibble whose `input` cell equals `file.path(normalizePath(directory, winslash = "/", mustWork = TRUE), <that file's basename>)`. The test derives each element's type from the vectors rather than from a hand-written table, so an element no vector holds fails by name. One element is spelled in mixed case and one is read from a subdirectory under `recursive = TRUE`. Covered by a test in `tests/testthat/test-ffm-jobs.R`.
- [ ] AC2: `media_extensions("audio")` is identical to `c("wav", "mp3", "m4a", "aac", "flac", "ogg", "oga", "opus", "wma", "aiff", "aif", "mka")`, `media_extensions("video")` is identical to `c("mp4", "mov", "mkv", "avi", "m4v", "webm", "mpg", "mpeg", "wmv", "flv", "mts", "m2ts", "ts")`, and `media_extensions("image")` is identical to `c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "gif", "webp")`, each expected vector written out in the test rather than derived from the function under test. For every element of `media_types()` the vector is character, non-empty, all lower case, free of duplicates, and disjoint from every other type's vector. Covered by a test in `tests/testthat/test-ffm-jobs.R`.
- [ ] AC3: Every element of `multi_audio_extensions`, read from that vector at test time rather than from a copy, appears in exactly one `media_extensions()` vector, and the failure message names any element that appears in none. Covered by a test in `tests/testthat/test-ffm-jobs.R`.
- [ ] AC4: A file the package wrote is listable by the package. Under `skip_if_no_ffmpeg()`, `separate_audio_video()` writing the three-track input from `make_multitrack_video()` to a `.mka` output inside its own `withr::local_tempdir()` produces a file that `ffm_jobs(<that directory>, type = "audio")` returns as the single row, its `input` cell naming that file. Covered by a test in `tests/testthat/test-separate-av-multitrack.R`.
- [ ] AC5: When no file matches and `extension` was not supplied, `ffm_jobs()` aborts with a message naming the extensions it looked for and naming `list.files()` as the way to reach a container the scanned set omits. When `extension` was supplied, it aborts naming the extensions it looked for and adds no such bullet. Both aborts name `ffm_jobs` as the call. The supplied-`extension` branch is asserted for a type other than `"video"`. Covered by tests in `tests/testthat/test-ffm-jobs.R`.
- [ ] AC6: `devtools::document()` produces a `man/` diff confined to `man/ffm_jobs.Rd`, whose `\value` section states that a `.ts` file that is TypeScript source is returned as a video row. `NEWS.md` names both added containers and that same consequence. `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R man/ffm_jobs.Rd` prints only its sentence-count line and exits 0. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T1
- AC4 → T3
- AC5 → T2
- AC6 → T4, T5

## Tasks

- [ ] T1: Tests first in `tests/testthat/test-ffm-jobs.R`: the three pinned vectors (AC2), the standing alignment guard over `multi_audio_extensions` (AC3), and the nine-element sweep with its mixed-case and `recursive = TRUE` axes (AC1). Then add `"mka"` to the audio vector and `"ts"` to the video vector (`R/ffm_jobs.R:164-173`), each appended last, with a comment naming the rule that admitted it: a container the package writes or names in its own diagnostics is listable. Plant a defect in the guard to see it red before trusting its green.
- [ ] T2: The no-match abort in `tm_ffm_jobs()` (`R/ffm_jobs.R:143-149`): add the `list.files()` bullet, raised only when `extension` was `NULL`, so a caller who already narrowed the search is not told to narrow it. Tests for both branches, one of them on a non-`"video"` type (AC5).
- [ ] T3: The end-to-end round trip in `tests/testthat/test-separate-av-multitrack.R` (AC4), under `skip_if_no_ffmpeg()` and inside its own `withr::local_tempdir()` so the assertion is about the file this test wrote. `tests/testthat/test-ffm-jobs.R` stays binary-free, as its header comment states.
- [ ] T4: The `@return` sentence on the `.ts` collision (`R/ffm_jobs.R:44-55`); the `NEWS.md` entry; `devtools::document()`; the prose sweep over `man/ffm_jobs.Rd` (AC6).
- [ ] T5: `devtools::check()`, `devtools::test()` with no other R session working (LESSONS M124), and `pkgdown::check_pkgdown()` (AC6).

## Work log

- 2026-09-18: created by /milestone-plan, promoting candidate row "Six deferred `ffm_jobs()` items" (c), whose stated trigger is a request naming a container the lists' own neighbours already cover. The row keeps its `ogv` half.
- 2026-09-18: plan gate chose adding the two containers the package writes over widening `extension` into an escape hatch, because the escape hatch reverses M121-1's recorded fallback that such a caller uses `list.files()` directly and widens an exported argument before a first release; falsified by a report of a caller blocked on a container the package neither writes nor names.
- 2026-09-18: plan gate chose the round-trip rule over closing the lists across the extension families FFmpeg's muxers and demuxers declare, measured 2026-09-18 against ffmpeg 9.0.1 by parsing `Common extensions` from `ffmpeg -h muxer=<name>` and `-h demuxer=<name>`: that closure adds 21 extensions to video, 23 to audio and 36 to image, and crosses media types, putting `mka` and `mks` in video and `mov`, `mp4` and `wmv` in audio; falsified by a container the round-trip rule admits that no user wants listed.
- 2026-09-18: plan gate chose including `ts` in the video vector over declining it, accepting that a folder holding TypeScript sources returns them as video rows; falsified by a report of source files reaching FFmpeg from a scanned folder. The cost is disclosed in `@return` and `NEWS.md` per AC6.
- 2026-09-18: criteria audit ran twice in full mode, both on fresh readers. The first read a wider draft carrying the escape hatch and returned 12 findings and 3 gaps; the gate narrowed the scope, so the criteria were rewritten and re-audited. The second returned 12 findings, all disposed here: the element-to-type table became a derivation from the pinned vectors; the standing alignment guard became AC3; the `.ts` cost gained its disclosure clause in AC6; `document()` no longer promises an unchanged `man/`; the end-to-end test moved out of the binary-free file; `mustWork = TRUE`, its own temp directory, the mixed-case and `recursive` axes, the ordered literal vectors and the non-`video` abort branch were all written in. The decision record the first finding asked for is M137-1, written at implement.

## Decisions

## Review
