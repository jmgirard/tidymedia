# Guards the package's landing topic (M55): `?tidymedia` resolves, and the topic
# stays listed in the installed help index.
#
# Both halves are easy to lose by accident. Dropping the roxygen block above the
# `"_PACKAGE"` sentinel makes roxygen skip it entirely and document() deletes
# tidymedia-package.Rd; adding back the `@keywords internal` line that
# usethis::use_package_doc() generates keeps the .Rd but drops the topic from
# the installed INDEX, so help(package = "tidymedia") stops listing it. Neither
# is visible in any other test.
#
# rd_sources() comes from helper-rd.R, which reads the source tree's man/ under
# devtools::test() and the parsed Rd database under R CMD check -- so this guard
# runs in both shapes rather than skipping in CI (LESSONS M51).

test_that("the package has a landing topic reachable as `?tidymedia`", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")

  topic <- rd[names(rd) %in% c("tidymedia-package.Rd", "tidymedia-package")]
  expect_length(topic, 1L)

  # `?tidymedia` works through the alias, not the file name.
  expect_match(topic[[1]], "\\alias{tidymedia}", fixed = TRUE)
})

test_that("the landing topic is not keyworded internal", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")

  topic <- rd[names(rd) %in% c("tidymedia-package.Rd", "tidymedia-package")]
  skip_if(length(topic) != 1L, "no landing topic")

  # \keyword{internal} is what excludes a topic from the installed INDEX, and so
  # from help(package = "tidymedia"). Measured at M55: with the keyword the
  # index carried 104 entries and no tidymedia-package row; without it, the row
  # is there.
  expect_false(grepl("\\keyword{internal}", topic[[1]], fixed = TRUE))
})

test_that("the vignette navigation paragraph is outside every section", {
  # M087 AC5. The paragraph pointing readers at the four vignettes is the
  # landing topic's closing navigation, not part of any one subject; it opened
  # inside \section{Session options} until M087 moved it. \section{} blocks are
  # top-level and come after \details{}, so appearing before the first one is
  # what "outside every section" means in a generated Rd.
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  topic <- rd[names(rd) %in% c("tidymedia-package.Rd", "tidymedia-package")]
  skip_if(length(topic) != 1L, "no landing topic")
  txt <- topic[[1]]

  needle <- "for the guided tour"
  expect_match(txt, needle, fixed = TRUE)
  first_section <- regexpr("\\section{", txt, fixed = TRUE)
  # The topic really has sections: without one, "before the first section" is
  # vacuously true and this guard would pass on an Rd it cannot judge.
  expect_gt(first_section, 0L)
  expect_lt(regexpr(needle, txt, fixed = TRUE), first_section)
})
