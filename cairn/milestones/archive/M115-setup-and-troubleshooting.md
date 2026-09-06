# M115: A new user can tell whether the setup worked, and fix it when it did not

**Status:** done (2026-09-06, PR #119 https://github.com/jmgirard/tidymedia/pull/119)

**Goal:** Close the getting-started gap: no "did it work?" step after installing the binaries, a macOS route dead-ending off `PATH`, README chunks running unguarded.

**Outcome:** All seven `README.Rmd` install routes end with a `program_status()` check, what a found and a not-found answer look like, and that route's recovery call -- the three FFmpeg routes offering `set_ffprobe()` beside `set_ffmpeg()`, since the two are looked up separately. The macOS manual route names the separate evermeet.cx `ffprobe` download and the `set_*()` step an Applications-folder install needs. `find_program()`'s not-found warning reads its advice from the installer's own seam and list (`tm_os()`, `tm_install_registers`) -- `set_<program>()` everywhere, `install_on_win()` on Windows for what it registers -- class and fields unchanged. `@examplesIf` guards the `find_program` example and the three spawning README chunks name their program in a guard. M114's two chunk sweeps and `build_vignettes_without_binaries.R` (new `readme`/`both` target) now cover `README.Rmd`; the identity sweep asserts its binaries are on `PATH` and aborts on an empty domain.

**Decisions:** none.

**Review:** Three lenses, ten findings, all from the diff-bug lens. Fixed here: the one-program recovery on three FFmpeg routes, the identity sweep's empty-domain blindness, two NEWS wraps. Folded into existing candidate rows at the maintainer's gate choice: the `Sys.which()`-vs-`find_program()` guard gap, the `ffplay` install offer, the un-widened stale-location warning, the negated-guard read, the artifact scan's `#>` dependency. Rejected: the scratch copy's size, the pre-existing `temp_libpath` path. PR #119's only comment was a coverage bot -- noted. M114's chunk-sweep lesson gained an empty-domain clause.
