# Tests for ffm_jobs(), the directory -> jobs-table export (M121). Every branch
# here is binary-free: the function lists files and builds a tibble, and the
# one ffm_batch() call runs with `run = FALSE`.

# A directory with one file of each shape the selector has to tell apart.
local_media_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(dir, "sub"))
  for (f in c("a.mp4", "b.MOV", "c.wav", "d.png", "notes.txt",
              "sub/deep.mp4")) {
    file.create(file.path(dir, f))
  }
  dir
}

# Raise-and-return, so a refusal's blame frame can be read. Not catch_call():
# that helper forces a `run` argument ffm_jobs() does not have.
catch <- function(expr) tryCatch(expr, error = function(e) e)

# Listing ------------------------------------------------------------------

test_that("ffm_jobs() returns one row per matching file, `input` and nothing else", {
  dir <- local_media_dir()
  jobs <- ffm_jobs(dir, type = "video")

  expect_s3_class(jobs, "tbl_df")
  expect_identical(names(jobs), "input")
  expect_type(jobs$input, "character")
  expect_identical(nrow(jobs), 2L)
  expect_setequal(basename(jobs$input), c("a.mp4", "b.MOV"))
})

test_that("`input` holds full paths that resolve to the listed files", {
  dir <- local_media_dir()
  jobs <- ffm_jobs(dir, type = "video")

  # Discriminating: a bare basename or a path relative to the directory would
  # both fail here from a different working directory.
  withr::with_dir(withr::local_tempdir(), {
    expect_true(all(file.exists(jobs$input)))
  })
  expect_identical(
    normalizePath(jobs$input, winslash = "/", mustWork = TRUE), jobs$input
  )
})

test_that("the extension match is case-insensitive but the returned name is not", {
  dir <- local_media_dir()
  jobs <- ffm_jobs(dir, type = "video")
  # `b.MOV` is matched by the lower-case pattern and returned unchanged.
  expect_true("b.MOV" %in% basename(jobs$input))
})

test_that("each type lists its own category and no other", {
  dir <- local_media_dir()
  expect_setequal(basename(ffm_jobs(dir, type = "audio")$input), "c.wav")
  expect_setequal(basename(ffm_jobs(dir, type = "image")$input), "d.png")
  # notes.txt is in none of the three.
  for (type in c("video", "audio", "image")) {
    expect_false("notes.txt" %in% basename(ffm_jobs(dir, type = type)$input))
  }
})

test_that("recursive = TRUE adds subdirectory files and FALSE is the default", {
  dir <- local_media_dir()
  expect_false("deep.mp4" %in% basename(ffm_jobs(dir, type = "video")$input))
  deep <- ffm_jobs(dir, type = "video", recursive = TRUE)
  expect_identical(nrow(deep), 3L)
  expect_true("deep.mp4" %in% basename(deep$input))
})

test_that("`extension` narrows within the type, with or without a leading dot", {
  dir <- local_media_dir()
  expect_setequal(
    basename(ffm_jobs(dir, type = "video", extension = "mp4")$input), "a.mp4"
  )
  expect_setequal(
    basename(ffm_jobs(dir, type = "video", extension = ".MP4")$input), "a.mp4"
  )
  expect_identical(
    nrow(ffm_jobs(dir, type = "video", extension = c("mp4", "mov"))), 2L
  )
})

# The AC1 hand-off ---------------------------------------------------------

test_that("ffm_batch() consumes the returned table unreshaped, plus a derived output", {
  dir <- local_media_dir()
  jobs <- ffm_jobs(dir, type = "video")
  jobs$output <- file.path(
    tempdir(), paste0(tools::file_path_sans_ext(basename(jobs$input)), ".mp3")
  )

  res <- ffm_batch(jobs, run = FALSE, .f = function(input, output, ...) {
    ffm_files(input, output) |> ffm_drop("video")
  })

  expect_identical(nrow(res), nrow(jobs))
  expect_true("command" %in% names(res))
  # Each compiled command names the row's own input and output -- an `.f` that
  # ignored its arguments would not.
  for (i in seq_len(nrow(res))) {
    expect_true(grepl(basename(jobs$input[[i]]), res$command[[i]], fixed = TRUE))
    expect_true(grepl(basename(jobs$output[[i]]), res$command[[i]], fixed = TRUE))
  }
})

# Refusals -----------------------------------------------------------------

test_that("AC2's three refusals name the frame the caller typed", {
  dir <- local_media_dir()
  empty <- withr::local_tempdir()

  cases <- list(
    missing_dir = catch(ffm_jobs(file.path(dir, "no-such-dir"), type = "video")),
    bad_type    = catch(ffm_jobs(dir, type = "subtitle")),
    no_match    = catch(ffm_jobs(empty, type = "video"))
  )

  for (nm in names(cases)) {
    expect_s3_class(cases[[nm]], "error")
    expect_identical(blamed_verb(cases[[nm]]), "ffm_jobs", info = nm)
  }
  expect_match(conditionMessage(cases$missing_dir), "existing directory")
  expect_match(conditionMessage(cases$bad_type), "video")
  expect_match(conditionMessage(cases$no_match), "No video files")
})

test_that("a file that is not a directory is refused like a missing one", {
  dir <- local_media_dir()
  cnd <- catch(ffm_jobs(file.path(dir, "a.mp4"), type = "video"))
  expect_s3_class(cnd, "error")
  expect_match(conditionMessage(cnd), "existing directory")
})

test_that("the no-match message names the extensions it looked for", {
  empty <- withr::local_tempdir()
  cnd <- catch(ffm_jobs(empty, type = "audio", extension = "flac"))
  expect_match(conditionMessage(cnd), "flac")
  # Only the narrowed set, not the whole audio vocabulary.
  expect_false(grepl("wav", conditionMessage(cnd), fixed = TRUE))
  expect_match(
    conditionMessage(catch(ffm_jobs(empty, type = "video", recursive = TRUE))),
    "subdirectories"
  )
})

test_that("an extension outside the type is refused, naming the offender and the set", {
  dir <- local_media_dir()
  cnd <- catch(ffm_jobs(dir, type = "video", extension = c("mp4", "wav")))
  expect_s3_class(cnd, "error")
  expect_identical(blamed_verb(cnd), "ffm_jobs")
  msg <- conditionMessage(cnd)
  expect_match(msg, "wav")
  expect_match(msg, "mkv")   # the accepted set is listed
})

test_that("every argument-form refusal fires and names ffm_jobs()", {
  dir <- local_media_dir()
  cases <- list(
    directory_not_string = catch(ffm_jobs(c(dir, dir), type = "video")),
    directory_na         = catch(ffm_jobs(NA_character_, type = "video")),
    directory_numeric    = catch(ffm_jobs(1, type = "video")),
    type_missing         = catch(ffm_jobs(dir)),
    type_not_string      = catch(ffm_jobs(dir, type = 1)),
    recursive_not_bool   = catch(ffm_jobs(dir, type = "video", recursive = "yes")),
    recursive_na         = catch(ffm_jobs(dir, type = "video", recursive = NA)),
    extension_numeric    = catch(ffm_jobs(dir, type = "video", extension = 1)),
    extension_empty_vec  = catch(ffm_jobs(dir, type = "video",
                                          extension = character(0))),
    extension_na         = catch(ffm_jobs(dir, type = "video",
                                          extension = NA_character_)),
    extension_empty_str  = catch(ffm_jobs(dir, type = "video", extension = ""))
  )
  for (nm in names(cases)) {
    expect_s3_class(cases[[nm]], "error")
    expect_identical(blamed_verb(cases[[nm]]), "ffm_jobs", info = nm)
  }
  expect_match(conditionMessage(cases$type_missing), "type")
})

# The vocabulary -----------------------------------------------------------

test_that("media_types() and media_extensions() agree and stay lower-case", {
  types <- media_types()
  expect_identical(types, c("video", "audio", "image"))
  for (type in types) {
    ext <- media_extensions(type)
    expect_type(ext, "character")
    expect_gt(length(ext), 0)
    expect_identical(ext, tolower(ext))
    expect_identical(ext, unique(ext))
    # No extension belongs to two categories -- the type selector would not
    # separate them.
    others <- unlist(lapply(setdiff(types, type), media_extensions))
    expect_length(intersect(ext, others), 0)
  }
})
