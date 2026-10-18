# M140: A folder at a remembered-location path warns instead of stopping, and `ffmpeg()` quotes its program's location

**Status:** done (2026-10-18, PR #144 https://github.com/jmgirard/tidymedia/pull/144)

**Goal:** A folder where tidymedia keeps a remembered program location gives a warning that says to delete it, and `ffmpeg()` runs a program whose location holds a space.

**Outcome:** `find_program()` (`R/program_management.R`) tests the chosen config path with `dir.exists()` before
`readLines()`. A folder at the current path, or at the pre-0.2.0 path with no current file, gives
`tidymedia_location_unreadable` (`tm_program`, `tm_file`) naming the folder and saying to delete it, then use
`set_*()`, so the four `find_*()` lookups and `program_status()` no longer stop. `ffmpeg()` (`R/ffmpeg.R`) quotes the
location as `ffprobe()` and `mediainfo()` do, covering `ffmpeg_codecs()` and `ffmpeg_encoders()`. `?find_ffmpeg`,
`?program_status` and two NEWS entries. Tests in `test-program-location-repair.R` and `test-ffmpeg.R` (a POSIX stub
under `my bin/`, skipped on CRAN and Windows); the timeout test's `find_ffmpeg` mock returns an unquoted path.

**Decisions:** D103 annotates D088: the folder case keeps the unreadable class and stays audible, with
delete-by-hand advice. The stricter Windows reading (D100) and `set_program()`/`unset_program()` over a folder
stayed out at the user's choice.

**Review:** One three-lens pass, no returns. 17 findings: 4 fixed (D103 narrowed, the stub test's CRAN skip, two
test assertions), 4 follow-ups (the "Folder at a remembered-location path in set and unset" row, a new "Unreadable
location file" row, the unclassed-aborts row), 9 rejected. Criteria audit took six findings; claim audit read 30
claims, corrected 1. All 11 CI checks green; the space test ran on macOS and Ubuntu, skipped on Windows.
