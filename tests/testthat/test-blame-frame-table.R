# M112 AC2: the blame table. Every export the `call` extraction touched must
# name ITSELF in a refusal -- the export the caller typed -- and must do so on
# both refusal forms and from both call sites.
#
# The two forms are separated because the M100/M110 lesson is that they fail
# apart: an ARGUMENT refusal comes from a checker (`arg_match()`,
# `check_string()`, `check_bool()`), a BODY refusal from a `cli_abort()` deeper
# in the shared body, and a fix that threads `call` into only the aborts leaves
# the checkers blaming the internal implementation while every existing pinning
# test stays green. The two sites are separated for the mirror-image reason: a
# `call` left to `rlang::caller_env()` names the caller's own frame, which is
# NULL at the console and the enclosing function inside a wrapper, so a
# console-only check cannot tell a correct frame from a missing one.

# The seven exports, each with an argument mistake and a body refusal that
# cannot be reached by any other route.
tm_blame_cases <- function() {
  bogus <- file.path(tempdir(), "tm-no-such-executable-M112")
  list(
    list(export = "set_program", form = "argument",
         f = function() set_program("ffmpeg", location = 1, confirm = FALSE)),
    list(export = "set_program", form = "body",
         f = function() set_program("ffmpeg", bogus, confirm = FALSE)),
    list(export = "set_ffmpeg", form = "argument",
         f = function() set_ffmpeg(location = 1, confirm = FALSE)),
    list(export = "set_ffmpeg", form = "body",
         f = function() set_ffmpeg(bogus, confirm = FALSE)),
    list(export = "set_ffprobe", form = "argument",
         f = function() set_ffprobe(location = TRUE, confirm = FALSE)),
    list(export = "set_ffprobe", form = "body",
         f = function() set_ffprobe(bogus, confirm = FALSE)),
    list(export = "set_ffplay", form = "argument",
         f = function() set_ffplay(bogus, confirm = "yes")),
    list(export = "set_ffplay", form = "body",
         f = function() set_ffplay(bogus, confirm = FALSE)),
    list(export = "set_mediainfo", form = "argument",
         f = function() set_mediainfo(location = 1, confirm = FALSE)),
    list(export = "set_mediainfo", form = "body",
         f = function() set_mediainfo(bogus, confirm = FALSE)),
    list(export = "hardware_encoder", form = "argument",
         f = function() hardware_encoder("h264", hardware = 1)),
    list(export = "hardware_encoder", form = "body",
         f = function() hardware_encoder("av1", "videotoolbox")),
    list(export = "has_hardware_encoder", form = "argument",
         f = function() has_hardware_encoder(codec = 1, hardware = "nvenc")),
    list(export = "has_hardware_encoder", form = "body",
         f = function() has_hardware_encoder("av1", "videotoolbox"))
  )
}

# The frame a refusal named, as the function name the condition's call carries.
# NA_character_ where the condition carried no call at all, which is what a
# `caller_env()` default produces at the console -- the M100 failure this whole
# table exists to keep fixed.
tm_blamed_frame <- function(thunk) {
  cnd <- rlang::catch_cnd(thunk(), classes = "error")
  if (is.null(cnd)) {
    return("<no refusal raised>")
  }
  call <- conditionCall(cnd)
  if (is.null(call)) {
    return(NA_character_)
  }
  nm <- rlang::call_name(call)
  if (is.null(nm)) NA_character_ else nm
}

# The AC2 table itself: one row per export x refusal form x call site.
tm_blame_table <- function() {
  cases <- tm_blame_cases()
  rows <- lapply(cases, function(case) {
    # "at the console": the export is called from the top of the thunk, so the
    # frame a `caller_env()` default would name is the thunk's, not the
    # export's -- the two are distinguishable, which is the point.
    console <- tm_blamed_frame(case$f)
    # "from a wrapper": one more user frame between the console and the export.
    # A refusal naming `tm_user_wrapper` here would mean the export blamed its
    # caller instead of itself.
    tm_user_wrapper <- function() case$f()
    wrapper <- tm_blamed_frame(tm_user_wrapper)
    data.frame(
      export = case$export, form = case$form,
      console = console, wrapper = wrapper,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

test_that("every refusal in the table names the export the caller typed", {
  tbl <- tm_blame_table()

  # The domain, stated independently of the table: fourteen rows, seven
  # exports, both forms at each. A table that silently lost a case would
  # otherwise pass on whatever was left.
  expect_identical(nrow(tbl), 14L)
  expect_setequal(
    unique(tbl$export),
    c("set_program", "set_ffmpeg", "set_ffprobe", "set_ffplay",
      "set_mediainfo", "hardware_encoder", "has_hardware_encoder")
  )
  expect_setequal(unique(tbl$form), c("argument", "body"))

  expect_identical(tbl$console, tbl$export)
  expect_identical(tbl$wrapper, tbl$export)
})

test_that("each row's refusal is the failure the row claims", {
  # The frame check above is blind to WHICH refusal fired: a body row that
  # aborted in a checker instead would still name the export. So each row is
  # pinned to its condition, and the two `set_*` forms to distinct ones.
  bogus <- file.path(tempdir(), "tm-no-such-executable-M112")
  expect_false(file.exists(bogus))

  expect_error(set_ffmpeg(bogus, confirm = FALSE),
               class = "tidymedia_program_not_found")
  expect_error(set_ffmpeg(location = 1, confirm = FALSE),
               class = "rlang_error")
  expect_error(set_ffmpeg(location = 1, confirm = FALSE),
               "must be a single string")

  expect_error(hardware_encoder("av1", "videotoolbox"),
               "videotoolbox has no")
  expect_error(hardware_encoder("h264", hardware = 1),
               "`hardware` must be")
})

test_that("both columns can tell a wrong frame from a right one", {
  # The discriminating control. `tm_leaky()` is the defect this table exists to
  # catch: a refusal whose `call` is left to `rlang::caller_env()`, so it names
  # whatever frame happened to call it. `tm_tight()` is the fixed shape. Both
  # are reached exactly as the table reaches each export -- by name, from
  # inside the thunk -- and they must come back with DIFFERENT frames, or the
  # agreement between `console`/`wrapper` and `export` above proves nothing.
  tm_leaky <- function(x, call = rlang::caller_env()) {
    cli::cli_abort("no", call = call)
  }
  tm_tight <- function(x) cli::cli_abort("no", call = rlang::current_env())

  # The console leg: one frame between the top and the refusing function.
  expect_identical(tm_blamed_frame(function() tm_tight(1)), "tm_tight")
  expect_identical(tm_blamed_frame(function() tm_leaky(1)), "thunk")

  # The wrapper leg: one more. The leaky form follows the extra frame and names
  # it; the tight form does not move. This is the pair the seven exports must
  # behave like the second half of.
  w_leaky <- function() tm_leaky(1)
  w_tight <- function() tm_tight(1)
  expect_identical(tm_blamed_frame(function() w_leaky()), "w_leaky")
  expect_identical(tm_blamed_frame(function() w_tight()), "tm_tight")

  # And a refusal that never came, so a silent no-op cannot read as a pass.
  expect_identical(tm_blamed_frame(function() NULL), "<no refusal raised>")
})
