# M116: A broken or stale remembered location is reported, not fatal or silent

**Status:** done (2026-09-07, PR #120 https://github.com/jmgirard/tidymedia/pull/120)

**Goal:** Close the six defects M113's review measured in what `program_status()` and its
seam report about a remembered program location.

**Outcome:** `find_program()` guards what it read back before `Sys.which()` tests it,
warning `tidymedia_location_unreadable` (`tm_program`, `tm_file`) and returning `NULL`
where an empty or multi-line file raised R's own `if` error. The stale-location warning
is classed `tidymedia_location_gone` (`tm_program`, `tm_location`), advises
`unset_program("<program>")` beside `set_<program>()`, and derives `install_on_win()`
through a shared `tm_install_bullet()`. `program_status()` selects by the two classes to
raise, not the one to muffle; `unset_program()` drops the FFmpeg capability memo on any
removal that took, above its abort; `tool_versions()` aborts
`tidymedia_locations_mismatch` on a wrong-length `locations` and names timed-out programs
in `program_status()`'s column spelling, caller-neutrally.

**Decisions:** D088 (which `find_program()` warnings `program_status()` surfaces,
superseding M113's blanket suppression); D089 (the memo's four discard routes and its
removed-nothing carve-out). Local: the three condition-class names, chosen at the gate.

**Review:** Four passes; three defect returns, the third hitting the thrash threshold and
dispositioned by narrowing AC4's instrument clause under two fresh-context [O] audits. The
fourth verified all seven criteria; twelve fan-out findings, none floor-qualifying.
