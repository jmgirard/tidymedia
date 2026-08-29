# M082: what the six verbs behind the dropped-track check say the check costs,
# and how to switch it off. A documentation guard, read from whichever Rd source
# this run has -- the source tree's man/ under devtools::test(), the installed
# package's parsed Rd database under R CMD check (helper-rd.R), so the guard
# runs against the installed help in the run the release gate uses rather than
# skipping there (M51/M59).


# The domain is walked, not listed ------------------------------------------

# Every exported function whose body reaches the dropped-track diagnostic. A
# walk over the namespace rather than a hand-list, so a seventh verb joining the
# family joins this guard with no line edited here (D059's membership rule).
drop_check_verbs <- function() {
  ns <- asNamespace("tidymedia")
  hits <- Filter(
    function(nm) {
      obj <- get(nm, envir = ns)
      is.function(obj) &&
        grepl("warn_dropped_audio", paste(deparse(body(obj)), collapse = " "),
              fixed = TRUE)
    },
    getNamespaceExports("tidymedia")
  )
  sort(hits)
}

# One verb's Rd text on a single line, or NULL. Both Rd shapes name topics with
# the .Rd suffix. The whitespace is squished because Rd is hard-wrapped at
# roxygen's width, so any sentence long enough to matter here is split across
# lines in some topics and not in others.
rd_for <- function(rd, topic) {
  hit <- rd[sub("\\.Rd$", "", names(rd)) == topic]
  if (!length(hit)) return(NULL)
  trimws(gsub("[[:space:]]+", " ", hit[[1]]))
}

test_that("the walk finds the verbs behind the check and nothing else", {
  # Non-vacuous, and stated independently of the walk that produced it: the six
  # verbs are named here, so a walk that silently emptied would fail rather than
  # make every expectation below pass over nothing.
  expect_setequal(
    drop_check_verbs(),
    c("extract_audio", "convert_audio", "normalize_audio",
      "extract_audio_batch", "convert_audio_batch", "normalize_audio_batch")
  )
})


# What every verb's own help says (AC5) -------------------------------------

test_that("every verb states the check's cost and how to switch it off", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source in this run")
  verbs <- drop_check_verbs()
  expect_gt(length(verbs), 0)
  for (verb in verbs) {
    txt <- rd_for(rd, verb)
    expect_false(is.null(txt), info = verb)
    expect_match(txt, "one FFprobe call per distinct input", fixed = TRUE,
                 info = verb)
    # Both forms, and the session form matched so that the `withr` line cannot
    # stand in for it: "local_options(tidymedia.check_tracks = FALSE)" contains
    # the session form as a substring, so a fixed match on it passed with the
    # session sentence deleted.
    expect_match(txt, "(^|[^_])options\\(tidymedia\\.check_tracks = FALSE\\)",
                 info = verb)
    expect_match(txt, "withr::local_options(tidymedia.check_tracks = FALSE)",
                 fixed = TRUE, info = verb)
  }
})

test_that("only the batch verbs promise the serial front-door sweep", {
  # The silent case: a scalar verb takes one input, so the sentence about
  # probes running serially before the fan-out would be false there.
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source in this run")
  for (verb in drop_check_verbs()) {
    txt <- rd_for(rd, verb)
    batch <- grepl("_batch$", verb)
    says <- grepl("serially at the front door", txt, fixed = TRUE)
    expect_identical(says, batch, info = verb)
  }
})


# The package topic and the release note (AC6) ------------------------------

test_that("the package topic names the loudness verbs among the check's verbs", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source in this run")
  txt <- rd_for(rd, "tidymedia-package")
  expect_false(is.null(txt))
  sentence <- regmatches(
    txt,
    regexpr("The dropped-track check behind.*?unreadable input\\.", txt)
  )
  expect_length(sentence, 1L)
  for (verb in c("extract_audio", "convert_audio", "normalize_audio")) {
    expect_match(sentence, verb, fixed = TRUE)
  }
  # The sentence said separate_audio_video() ran this check until M082; it runs
  # a different one, after a failed run, and the sentence now says so.
  expect_match(sentence, "multi-track diagnostic", fixed = TRUE)
})

test_that("the package topic documents the seam beside the other two", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source in this run")
  txt <- rd_for(rd, "tidymedia-package")
  expect_match(txt, "Session options", fixed = TRUE)
  for (opt in c("tidymedia.timeout", "tidymedia.check_tracks",
                "tidymedia.nvenc_encoders")) {
    expect_match(txt, paste0("options(", opt), fixed = TRUE)
  }
})

test_that("the release note names the option, its default and the cost", {
  # doc_timeout_sources()$news is already the file's CONTENT, in whichever
  # shape this run has NEWS.md -- the repo root under devtools::test(), the
  # installed package root under R CMD check.
  news <- doc_timeout_sources()$news
  skip_if(is.null(news), "NEWS.md not readable in this run")
  squished <- trimws(gsub("[[:space:]]+", " ", news))
  expect_match(squished, "options(tidymedia.check_tracks = FALSE)", fixed = TRUE)
  expect_match(squished, "defaults to TRUE", fixed = TRUE)
  expect_match(squished, "one FFprobe call per distinct input", fixed = TRUE)
})
