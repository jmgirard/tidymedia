# M140: A folder at a remembered-location path warns instead of stopping, and `ffmpeg()` quotes its program's location

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — it changes what the exported program lookups, `program_status()` and `ffmpeg()` do
- **Branch/PR:** m140-folder-config-and-spaced-ffmpeg

## Goal

A folder where tidymedia keeps a remembered program location gives a warning that says to delete it, and `ffmpeg()` runs a program whose location holds a space.

## Scope

**In:** `find_program()`, so `find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()`,
`find_mediainfo()` and `program_status()`, warn with `tidymedia_location_unreadable`
when a remembered-location path is a folder, at the current or the pre-0.2.0 path.
The warning says to delete the folder. `ffmpeg()` puts the location in double
quotes, as `ffprobe()` and `mediainfo()` do. `?find_ffmpeg` and `?program_status`
describe the folder case. A decision entry notes how the folder case fits D088.
`NEWS.md` entries.

**Out:** the stricter Windows reading of a remembered location (item (a) of the
"Three gaps in reading or running a remembered location" row), which stays on that
row under D100. `unset_program()` and `set_program()` over a folder at the path,
which go to the new "Folder at a remembered-location path in set and unset" row.
Other characters that need shell quoting (`$`, a backtick, `"`), which stay
unquoted by the double quotes and are not claimed.

## Acceptance criteria

- [ ] AC1: For each program `find_program()` accepts (`ffmpeg`, `ffprobe`,
      `ffplay`, `mediainfo`), when that program's remembered-location path is a
      folder and the program is not found on `PATH`, its lookup returns `NULL`
      with a warning of class `tidymedia_location_unreadable` whose `tm_file`
      field is that path, and raises no error. This holds for a folder at the
      current path, and for a folder at the pre-0.2.0 path while no current file
      exists. A test under `tm_redirect_config()` asserts the return value, the
      class and the field for each program at both paths.
- [ ] AC2: That warning's message contains the folder's path and the word
      "Delete", and does not contain `unset_program`. The AC1 test asserts both
      with `cli.width` set wide enough that the path is not wrapped.
- [ ] AC3: With a folder at `ffmpeg`'s current path, `program_status()` returns
      its table with one row for each of the four programs and raises exactly one
      warning, of class `tidymedia_location_unreadable`, instead of stopping. A
      test asserts the table's `program` column and the one warning's class.
- [ ] AC4: `ffmpeg()` passes the resolved location to the shell inside double
      quotes, as `ffprobe()` and `mediainfo()` do, so a location under a folder
      whose name holds a space runs. A test with a stub `ffmpeg` that uses only
      shell builtins, under such a folder and with the lookup pointed at it,
      asserts that `ffmpeg("-version")` returns the stub's output. The test skips
      on Windows and its skip message says why.
- [ ] AC5: `?find_ffmpeg` and `?program_status` describe the folder case and say
      to delete the folder by hand, and no sentence on either page says that
      `unset_program()` forgets a location in that case.
- [ ] AC6: `NEWS.md`'s development-version section has two entries. One says the
      program lookups and `program_status()` warn instead of stopping when a
      remembered-location path is a folder, and that the warning says to delete
      it. The other says `ffmpeg()`, and so `ffmpeg_codecs()` and
      `ffmpeg_encoders()`, quote the program's location, so a location holding a
      space runs. Neither entry names Windows or any character other than a space.
- [ ] AC7: The profile's verify slot is clean: `devtools::test()` passes and
      `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T4
- AC5 → T3
- AC6 → T5
- AC7 → T5

## Tasks

- [x] T1: Write the folder tests first, beside the existing unreadable tests in
      `tests/testthat/test-program-location-repair.R` (near :315). Under
      `tm_redirect_config()`, loop over `tm_program_vocabulary` with a folder at
      `tm_config_file(program)`, and again with a folder at the legacy path and no
      current file. Assert `NULL`, the class, `tm_file`, and the message (path,
      "Delete", no `unset_program`) under `cli.width = 1000`. Assert
      `program_status()`'s `program` column and exactly one warning with
      `tm_collect_warnings()`. See each red against `master`.
- [x] T2: In `find_program()` (`R/program_management.R:148-171`), test the
      config path for a folder before `readLines()`. A folder gives the
      `tidymedia_location_unreadable` warning with its own message: it names the
      folder, says to delete it, and then to use `set_{program}()`. Keep the class
      and the `tm_program` and `tm_file` fields. Update the comments above it.
- [ ] T3: Rewrite the unreadable bullet on `?find_ffmpeg`
      (`R/program_management.R:944`) and the repair sentence on `?program_status`
      (`:820-823`) for the folder case, then `devtools::document()` (the M128
      lesson: an old roxygen2 writes nothing). Append a `cairn/DECISIONS.md` entry
      noting that D088's audible unreadable warning still applies to a folder,
      though neither `unset_program()` nor `set_program()` clears one, so the
      repair it names is deleting the folder by hand.
- [ ] T4: Write the `ffmpeg()` test first: a stub script that only `echo`es,
      because `tm_redirect_config()` sets `PATH=""`, under a temp folder whose
      name holds a space, with `find_ffmpeg` mocked to that path and
      `skip_on_os("windows")` naming the POSIX stub as the reason. See it red,
      then quote the location at `R/ffmpeg.R:33` as `R/ffprobe.R:30` does.
- [ ] T5: Add the two `NEWS.md` entries under "tidymedia (development version)".
      Run `devtools::test()` with no other R session working (the M124 lesson),
      then `devtools::check()`. At review, quote a CI log from the macOS and
      Linux legs showing AC4's test passed rather than skipped.

## Work log

- 2026-10-17: created by /milestone-plan from the "Three gaps in reading or running a remembered location" candidate row, items (b) and (c).
- 2026-10-17: tier check: milestone — the row as promoted includes item (a), which reverses D100's Windows rule, a design decision.
- 2026-10-17: collision sweep: D100 (M138 promises no stricter Windows reading) surfaced at the question set and kept; D088 gets an annotating entry at T3; M116 and M138 shipped the neighbouring location warnings; no planned milestone overlaps.
- 2026-10-17: inbox sweep: 0 open issues, 0 open PRs.
- 2026-10-17: lessons harvested: M044 (cli interpolates braces, so the folder path goes through `{.file}`), M124 (run the suite alone), M128 (roxygen2 version), and the `PATH=""` that `tm_redirect_config()` sets, which limits the stub to shell builtins.
- 2026-10-17: measured on master: a folder at `tm_config_file("ffmpeg")` makes `find_ffmpeg()` and `program_status()` stop with `cannot open the connection`, and `unset_program()` refuses with write-access advice; `ffmpeg("-version")` on a stub under `my bin/` fails with `sh: .../my: No such file or directory`.
- 2026-10-17: criteria audit (full mode, fresh Opus reader) returned six findings, all taken: AC2 also bars `set_program()` as the only repair and matches a fixed word; AC5 added for the two help pages; AC1 widened to all four programs and both paths; AC6 became a content list; AC4's CI-leg wording moved to T5 as review evidence and the stub limited to builtins; set and unset over a folder sent to a candidate row.
- 2026-10-17: question set: Windows item (a) — leave it for later; set and unset over a folder — candidate row; route — one milestone.
- 2026-10-17: plan gate chose a folder test before `readLines()` over catching any `readLines()` error, because a folder takes advice of its own (delete it by hand) that a generic read error cannot give; falsified by another read failure, such as a file with no read permission, stopping the lookup.
- 2026-10-17: plan gate chose double quotes in `ffmpeg()` over `shQuote()`, because the two sibling direct commands already use double quotes and a matching form keeps the three alike; falsified by a location that the double-quoted form breaks and `shQuote()` would not.
- 2026-10-17: T1 tests written in `test-program-location-repair.R` and seen red on master (`cannot open the connection`); T2 adds a `dir.exists()` guard above `readLines()` in `find_program()` with its own delete-the-folder message; full suite 0 failed, 1818 tests.

## Decisions

## Review
