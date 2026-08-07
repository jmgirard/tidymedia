# M55: A package landing topic, and a `parallel` enumeration that matches the code

**Status:** done (2026-08-07, PR #58 https://github.com/jmgirard/tidymedia/pull/58)

**Goal:** Give `?tidymedia` a topic to resolve to, and make every vignette line mentioning
`parallel` true of the shipped package.

**Outcome:** `R/tidymedia-package.R` carries a `"_PACKAGE"` sentinel under an `@details` landing body
— three-layer orientation, both metadata back ends kept apart, all four vignettes named — generating
`man/tidymedia-package.Rd` with no `\keyword{internal}`, so `?tidymedia` resolves and the topic holds
a row in the installed `INDEX`; a new top `Package` section in `_pkgdown.yml` catalogues it.
`vignettes/batch.Rmd`'s parallel paragraph states the surface as a rule — `ffm_batch()`, every
`*_batch` verb, `segment_video()`, the five `probe_*()` readers, nothing else — plus what the argument
does NOT do: ignored on a reader handed a `probe` object, and with no `future` plan set it runs one
job at a time and warns. `vignettes/metadata.Rmd` covers it at all. `test-parallel-surface.R` pins the
22-name set off the namespace, `test-package-topic.R` the topic's alias and absent internal keyword.

**Decisions:** none promoted. Two implement-gate calls: deliver AC1's `help(package=)` half by
dropping the usethis `@keywords internal` line rather than narrowing the criterion; and put the
topic in a new top `Package` section over folding it into `Concepts`, whose subject is arguments.

**Review:** One round, no defect return. Thirteen findings from three lenses, one actioned (F2, 85 —
the landing text called the MediaInfo-backed scalar `get_*()` helpers tibble readers). Four
sub-threshold prose findings (78/68/68/65) fixed at the maintainer's direction at the merge gate;
the prior-review lens caught a NEWS splice that had eaten M54's nvenc bullet. Eight logged.
