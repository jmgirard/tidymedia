# M080: the one-argument `check_*` value predicates refuse NA with a condition
# that names the argument, instead of crashing bare inside a comparison or
# compiling the NA into the command.

test_that("check_dim() refuses NA of every type, naming its argument", {
  # NA_real_ reached `x > 0`, whose NA made `if (!ok)` raise base R's
  # `missing value where TRUE/FALSE needed` with no argument in it (M64 F4);
  # NA_character_ satisfied is_character(n = 1) and passed straight through.
  for (na in list(NA, NA_integer_, NA_real_, NA_character_)) {
    err <- rlang::catch_cnd(check_dim(na, arg = "width"))
    expect_s3_class(err, "rlang_error")
    expect_match(conditionMessage(err), "`width`", fixed = TRUE)
  }
})

test_that("a scalar verb refuses an NA dimension instead of compiling it", {
  f <- withr::local_tempfile(fileext = ".mp4")
  file.create(f)
  o <- withr::local_tempfile(fileext = ".mp4")
  # This returned `-vf "crop=w=NA:h=100:..."` — a command FFmpeg would have
  # rejected at run time, from a call that succeeded at run = FALSE.
  err <- rlang::catch_cnd(
    crop_video(f, o, NA_character_, 100, run = FALSE)
  )
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "`width`", fixed = TRUE)
})
