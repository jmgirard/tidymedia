# M128: The input, output and run pipeline help pages read as plain English

**Status:** done (2026-09-14, PR #133 https://github.com/jmgirard/tidymedia/pull/133)

**Goal:** The M128 help-page domain uses plain English for an R user who does not know FFmpeg.

**Outcome:** The roxygen text in `R/ffm.R` is rewritten under D093, so it changes form, not claims. The pages are `ffm_files()`, `ffm_copy()`, `ffm_seek()`, `ffm_map()`, `ffm_drop()`, `ffm_codec()`, `ffm_pixel_format()`, `ffm_output_options()`, `ffm_compile()` and `ffm_run()`. The sweep over the 10 pages goes from 26 findings to none. The six pages with a glossary stem name the glossary. The exit-status detail on `?ffm_run` is two lists in its own section. The `?ffm_drop` title typo "Steams" is fixed. No test pinned changed wording, so no test changed. The ledger is `### M128` in `cairn/references/plain-docs.md`. Six false base claims are the M128 follow-up row in `ROADMAP.md`.

**Decisions:** none. The plan gate chose naming the glossary over defining a term on the page, and about 12 pages per milestone.

**Review:** One pass with the three-lens fan-out, no returns. AC6 first had no `document()` evidence, because roxygen2 8.0.0 stopped it against the 8.1.0 pin. The user chose to install 8.1.0, and all six criteria then passed. Four slips the branch added were fixed at the gate and read by a fresh reader. O1: the verify error "matches" the exit error. O2: yuv420p "works with most players". O3: failed rows "include rows that exited zero". O4: "to stack videos". O7 added `?ffm_seek` "lossless copy" and `?ffm_drop` "from the media file" to the follow-up row as (e) and (f). O8 fixed the ledger's base counts to 14 and 12, and ROADMAP item (d). S2 was rejected, because "Append" already made its claim. CI 10/10 green. One lesson was added. To keep the byte budget, the stalest lesson was pruned: M46, on timeout warning text by locale. A guard on `R/timeout.R` covers most of it.
