# M104: `install_on_win()` registers every program or none

**Status:** done (2026-09-03, PR #108 https://github.com/jmgirard/tidymedia/pull/108)

**Goal:** An `install_on_win()` that cannot use a program the archive produced registers nothing at all, and says which program it could not use.

**Outcome:** `tm_usable_binary()` (`R/program_management.R`) asks of one produced path what
`set_program()` asks — `Sys.which()` resolves it — plus that what is there is a file rather than
a directory and is not empty; it never runs the binary. `install_on_win()` partitions the extracted set through it BEFORE
the loop that registers anything: a required program failing aborts `tidymedia_program_unusable`,
blamed on `install_on_win()`, naming every failed program and each full path, with no config file
written and nothing removed from the install directory; a failing `ffplay` draws its own
`cli_inform()` sentence, distinct from the archive-did-not-produce one, and the install still
returns `TRUE`. `tm_install_binary()` now `path.expand()`s the path it builds, so the check and
the `set_program()` call below it ask about one file. `tm_mock_install()` gained `spoil =`,
planting any of four unusable forms per program. Ten tests; `@return` enumerates six aborts.

**Decisions:** D083 (annotates D082).

**Review:** Three-lens fan-out, user-facing tier. Both Sonnet lenses found nothing; the [O] lens
returned eight. F1-F5 fixed on the branch — F1 a real defect the branch introduced, a
`~`-relative `install_dir` refusing a good install because `Sys.which()` and `file.info()`
disagreed about the path, fixed under a regression test confirmed red. F6-F8 (the class a
listed-but-never-created path deserves; no test isolating the `!isdir` clause; the helper's
scalar-only shape) to a candidate row. Six criteria evidenced, ten CI checks green.
