# M123: Help pages stop naming the internal find_program()

**Status:** done (2026-09-10, PR #127 https://github.com/jmgirard/tidymedia/pull/127)

**Goal:** The shipped help pages send readers to `find_ffmpeg()` and its siblings wherever
they sent them to the internal `find_program()`.

**Outcome:** The find-a-program roxygen block moved off `find_program()` onto `find_ffmpeg()`,
whose definition now comes first among the four wrappers so roxygen names the merged topic
`find_ffmpeg`; `@usage NULL` and the `find_program` alias went with it. `man/find_program.Rd`
became `man/find_ffmpeg.Rd`, `?find_program` opens nothing, and the four "Other program
management functions" lists link `find_ffmpeg()`. Seven roxygen references in
`R/program_management.R` name `find_ffmpeg()` and its siblings (`find_mediainfo()` in
`unset_program()`'s example comment). The NEWS bullet names `?find_ffmpeg` and says
`?find_program` no longer opens the page. `devtools::check()` 0/0/0.

**Decisions:** plan gate (user choices): drop the alias rather than keep `?find_program`
working; no pkgdown redirect; one-time greps over a standing unexported-link test. No
DECISIONS.md entry.

**Review:** three-lens fan-out, 0 returns; blame-history and prior-review no findings. [O]
raised 6. Fixed: NEWS "is now" dropped (0.1.0 already had the `find_ffmpeg` alias).
Follow-up rows: topic name held only by block order; the help text's "current directory"
claim, folded into the remembered-location row. Rejected: two style nits, AC4 wording. All
10 CI checks green after one timed-out wait. No lessons added or retired.
