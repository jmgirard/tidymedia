# M112: No export publishes the internal `call` argument, and the duplicate exports are settled

**Status:** done (2026-09-05, PR #116 https://github.com/jmgirard/tidymedia/pull/116)

**Goal:** Move the threaded `call` argument off `set_program()`'s and `hardware_encoder()`'s published signatures
into an internal implementation, keeping the blame behaviour M100 and M110 bought, and settle the two pairs of
exported names that resolve to one function.

**Outcome:** Internals `tm_set_program(program, location, confirm, call)` and `tm_hardware_encoder(codec, hardware,
call)` hold the shared bodies; the seven exports are wrappers passing `rlang::current_env()`, and the threaded
`call` reaches `arg_match()`/`check_string()`/`check_bool()`, not only the abort sites. `tm_export_formals()`
(`helper-export-formals.R`, used by `test-exported-call-formal.R`) took `getNamespaceExports()` from 2 hits to 0
over 86 exported functions; `test-blame-frame-table.R` measures 7 exports x argument/body x console/wrapper = 28
cells all naming the export typed, with a control that plants the `caller_env()` defect. One frame moved as Scope Out
declared: `has_hardware_encoder()`'s wrong-`codec`/`hardware` refusals now name it rather than the mapper. `ffm()` and
`mediainfo_summary()` are gone from `NAMESPACE`, `_pkgdown.yml` and `man/`; the `.rds` baseline drops its `mediainfo_summary` cell on read behind a byte-identity `stopifnot()`.

**Decisions:** D087 (supersedes M110's milestone-local `call`-stays-a-formal decision); deprecation waived for all
three removals under D014's pre-0.2.0 window.

**Review:** Three-lens fan-out; blame-history and prior-PR-comments clean, diff-bug returned nine. Eight fixed on the
branch — a false DESIGN Known-issues clause, an unpinned duplicate `program` literal, a positive control that never
ran the sweep, a stale 47/46 count, six comments naming the wrong frame, an orphaned comment, AC5's own paragraph, a
missing presence assertion; one rejected as linter/style. At hygiene, LESSONS' M100/M110 `call` line was trimmed to
its uncovered half and the control finding graduated to `references/false-greens.md`.
