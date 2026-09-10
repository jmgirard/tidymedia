# Tests for ffm_jobs(), the directory -> jobs-table export (M121). Every branch
# here is binary-free: the function lists files and builds a tibble, and every
# batch call -- the one ffm_batch() call and the fifteen *_batch() verbs --
# runs with `run = FALSE`.

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
  # Stated independently of what ffm_jobs() returned: the two paths the fixture
  # wrote, spelled from the directory's own normalized path. Asserting instead
  # that normalizePath() is idempotent on jobs$input would hold whatever files
  # came back.
  root <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  expect_setequal(jobs$input, file.path(root, c("a.mp4", "b.MOV")))
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
    type_na              = catch(ffm_jobs(dir, type = NA_character_)),
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

test_that("a subdirectory matching the extension pattern is not a row", {
  dir <- local_media_dir()
  dir.create(file.path(dir, "takes.mp4"))

  jobs <- ffm_jobs(dir, type = "video")
  expect_false("takes.mp4" %in% basename(jobs$input))
  expect_false(any(dir.exists(jobs$input)))
  expect_setequal(basename(jobs$input), c("a.mp4", "b.MOV"))

  # And with the directory dropped, recursive = TRUE is a superset of FALSE:
  # before the fix `takes.mp4` was in FALSE only, since list.files(recursive =
  # TRUE) omits directories.
  deep <- ffm_jobs(dir, type = "video", recursive = TRUE)
  expect_true(all(jobs$input %in% deep$input))
  expect_true("deep.mp4" %in% basename(deep$input))
})

test_that("a relative `directory` still yields absolute paths", {
  # AC1's "full paths" clause is promised on every platform, Windows included,
  # so it is tested outside the symbolic-link block that Windows skips.
  dir <- local_media_dir()

  withr::with_dir(dirname(dir), {
    jobs <- ffm_jobs(basename(dir), type = "video")

    expect_setequal(basename(jobs$input), c("a.mp4", "b.MOV"))
    expect_true(all(startsWith(jobs$input, "/") | grepl("^[A-Za-z]:", jobs$input)))
    expect_true(all(file.exists(jobs$input)))
  })
})

test_that("a dangling symbolic link is not a row", {
  # Windows reports a dangling link as existing, so the predicate keeps it
  # there and these expectations are about a platform this package no longer
  # promises them on -- AC1's disclosed carve-out, held by the `ffm_jobs()`
  # candidate row as item (f). file.symlink() succeeds on the runner, so
  # skip_if_not() below never fires and this guard is what keeps the leg green.
  skip_on_os("windows")

  dir <- local_media_dir()
  linked <- file.symlink("../gone.mp4", file.path(dir, "broken.mp4"))
  skip_if_not(isTRUE(linked), "this filesystem does not support symbolic links")

  # list.files() yields the link, dir.exists() is FALSE on it, and
  # normalizePath(mustWork = FALSE) returns an unresolvable path unchanged --
  # so before the fix `broken.mp4` came back as a row, and from a relative
  # `directory` it came back relative too.
  withr::with_dir(dirname(dir), {
    jobs <- ffm_jobs(basename(dir), type = "video")

    expect_false("broken.mp4" %in% basename(jobs$input))
    expect_setequal(basename(jobs$input), c("a.mp4", "b.MOV"))
    expect_true(all(file.exists(jobs$input)))
    expect_false(any(dir.exists(jobs$input)))
    # Absolute however the directory was spelled: the criterion says full
    # paths, and a relative `directory` is what made row 1 relative before.
    expect_true(all(startsWith(jobs$input, "/") | grepl("^[A-Za-z]:", jobs$input)))
  })
})

test_that("a directory holding only a dangling link reports no files", {
  # Windows reports a dangling link as existing, so the predicate keeps it
  # there and these expectations are about a platform this package no longer
  # promises them on -- AC1's disclosed carve-out, held by the `ffm_jobs()`
  # candidate row as item (f). file.symlink() succeeds on the runner, so
  # skip_if_not() below never fires and this guard is what keeps the leg green.
  skip_on_os("windows")

  dir <- withr::local_tempdir()
  linked <- file.symlink("../gone.mp4", file.path(dir, "broken.mp4"))
  skip_if_not(isTRUE(linked), "this filesystem does not support symbolic links")

  # The link is dropped before the zero-match check, so this is a refusal
  # rather than a zero-row tibble.
  cnd <- catch(ffm_jobs(dir, type = "video"))
  expect_s3_class(cnd, "error")
  expect_identical(blamed_verb(cnd), "ffm_jobs")
  expect_match(conditionMessage(cnd), "No video files")
})

test_that("a symbolic link whose target exists is still a row", {
  dir <- local_media_dir()
  target <- file.path(dir, "sub", "deep.mp4")
  linked <- file.symlink(target, file.path(dir, "live.mp4"))
  skip_if_not(isTRUE(linked), "this filesystem does not support symbolic links")

  # Discriminating against a predicate that drops every link rather than only
  # the broken ones: this one resolves, so it belongs in the table.
  jobs <- ffm_jobs(dir, type = "video")
  expect_identical(nrow(jobs), 3L)
  expect_true(all(file.exists(jobs$input)))
})

test_that("a directory holding only extension-named subdirectories reports no files", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "takes.mp4"))
  cnd <- catch(ffm_jobs(dir, type = "video"))
  expect_s3_class(cnd, "error")
  expect_match(conditionMessage(cnd), "No video files")
})

test_that("a multi-value `type` is refused, not silently reduced to its first", {
  dir <- local_media_dir()
  # Both refuse now. The full-set vector used to pass: arg_match() reduces `arg`
  # to its first element whenever `arg` is setequal() to `values`, so any
  # permutation slipped through. The two-element vector always aborted, but
  # from arg_match(), not with the "single string" message asserted here.
  for (val in list(c("video", "audio", "image"), c("audio", "video"))) {
    cnd <- catch(ffm_jobs(dir, type = val))
    expect_s3_class(cnd, "error")
    expect_identical(blamed_verb(cnd), "ffm_jobs")
    expect_match(conditionMessage(cnd), "single string")
  }
})

test_that("`extension` given as a factor is refused, and so is an empty `directory`", {
  dir <- local_media_dir()

  fac <- catch(ffm_jobs(dir, type = "video", extension = factor("mp4")))
  expect_s3_class(fac, "error")
  expect_identical(blamed_verb(fac), "ffm_jobs")
  expect_match(conditionMessage(fac), "character vector")

  empty_dir <- catch(ffm_jobs("", type = "video"))
  expect_s3_class(empty_dir, "error")
  expect_identical(blamed_verb(empty_dir), "ffm_jobs")
  expect_match(conditionMessage(empty_dir), "existing directory")
})

test_that("the extension refusal agrees in number with the offenders it names", {
  dir <- local_media_dir()
  one <- conditionMessage(catch(ffm_jobs(dir, type = "video", extension = "wav")))
  many <- conditionMessage(
    catch(ffm_jobs(dir, type = "video", extension = c("wav", "png")))
  )
  expect_match(one, "is not one of them", fixed = TRUE)
  expect_match(many, "are not among them", fixed = TRUE)
})

test_that("the NEWS entry's six-and-nine split over the *_batch() verbs holds", {
  dir <- local_media_dir()
  jobs <- ffm_jobs(dir, type = "video")

  # The four that need no argument at all.
  expect_no_error(standardize_video_batch(jobs, run = FALSE))
  expect_no_error(normalize_audio_batch(jobs, run = FALSE))
  expect_no_error(format_for_web_batch(jobs, run = FALSE))
  expect_no_error(strip_metadata_batch(jobs, run = FALSE))

  # And the two that take the table unaltered once their argument is supplied
  # -- the pair the entry used to count among the refusers.
  expect_no_error(
    crop_video_batch(jobs, width = 100, height = 100, x = 0, y = 0, run = FALSE)
  )
  expect_no_error(sample_frames_batch(jobs, fps = 1, run = FALSE))

  # The other nine refuse the bare table, each naming its own column. Three
  # name `output`; the rest name what their task needs.
  refusers <- list(
    convert_audio        = list(quote(convert_audio_batch(jobs, run = FALSE)), "output"),
    extract_audio        = list(quote(extract_audio_batch(jobs, run = FALSE)), "output"),
    picture_in_picture   = list(quote(picture_in_picture_batch(jobs, run = FALSE)), "output"),
    anonymize_video      = list(quote(anonymize_video_batch(jobs, run = FALSE)), "regions"),
    compare_videos       = list(quote(compare_videos_batch(jobs, run = FALSE)), "inputs"),
    concatenate_videos   = list(quote(concatenate_videos_batch(jobs, run = FALSE)), "inputs"),
    extract_frame        = list(quote(extract_frame_batch(jobs, run = FALSE)), "timestamp"),
    segment_video        = list(quote(segment_video_batch(jobs, run = FALSE)), "start"),
    separate_audio_video = list(quote(separate_audio_video_batch(jobs, run = FALSE)),
                                "audiofile")
  )
  expect_identical(length(refusers), 9L)
  for (nm in names(refusers)) {
    cnd <- catch(eval(refusers[[nm]][[1]]))
    expect_s3_class(cnd, "error")
    expect_match(conditionMessage(cnd), refusers[[nm]][[2]], info = nm)
  }
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
