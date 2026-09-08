# M118 -- the three name-resolution skip helpers, and the two hardware-probe
# ones, must skip on CRAN's own submission check and nowhere else.
#
# The assertion is on WHICH skip, never on a bare one: a helper whose binary is
# absent also skips, and reading that as the CRAN skip would pass this file on a
# machine where NOT_CRAN never mattered. testthat::skip() signals a condition of
# class "skip" carrying "Reason: <message>" (measured 2026-09-08), so the reason
# string is what separates the two.

# Run `helper` with NOT_CRAN in the given state and return the skip reason, or
# NA_character_ when it did not skip. `value = NA` unsets the variable.
tm_skip_reason <- function(helper, not_cran) {
  withr::with_envvar(c(NOT_CRAN = not_cran), {
    tryCatch(
      {
        helper()
        NA_character_
      },
      skip = function(cnd) conditionMessage(cnd)
    )
  })
}

cran_helpers <- list(
  skip_if_no_ffmpeg = skip_if_no_ffmpeg,
  skip_if_no_ffprobe = skip_if_no_ffprobe,
  skip_if_no_mediainfo = skip_if_no_mediainfo,
  skip_if_no_nvenc = skip_if_no_nvenc,
  skip_if_no_videotoolbox = skip_if_no_videotoolbox
)

test_that("each skip helper skips FOR CRAN when NOT_CRAN is unset", {
  for (name in names(cran_helpers)) {
    reason <- tm_skip_reason(cran_helpers[[name]], NA)
    expect_match(
      reason, "On CRAN",
      info = paste0(name, " did not skip for CRAN with NOT_CRAN unset")
    )
  }
})

test_that("NOT_CRAN=true lifts the CRAN skip from the three name helpers", {
  # The control half. Without it an "On CRAN" match above would also pass for a
  # helper that skips unconditionally. Only meaningful where the binary is on
  # PATH, so each program is asked separately rather than gating the whole test.
  programs <- c(
    skip_if_no_ffmpeg = "ffmpeg",
    skip_if_no_ffprobe = "ffprobe",
    skip_if_no_mediainfo = "mediainfo"
  )
  present <- programs[nzchar(Sys.which(programs))]
  skip_if(
    length(present) == 0L,
    "none of ffmpeg, ffprobe or mediainfo is on PATH"
  )
  for (name in names(present)) {
    expect_identical(
      tm_skip_reason(cran_helpers[[name]], "true"), NA_character_,
      info = paste0(name, " still skipped with NOT_CRAN=true and the binary on PATH")
    )
  }
})

test_that("the CRAN skip comes before the binary question", {
  # Both conditions hold at once -- on CRAN, and no binary reachable. The helper
  # must report CRAN, which is only true if skip_on_cran() runs first. PATH = ""
  # is how helper-program-config.R:43 makes a program unreachable.
  withr::with_envvar(c(PATH = ""), {
    for (name in names(cran_helpers)) {
      expect_match(
        tm_skip_reason(cran_helpers[[name]], NA), "On CRAN",
        info = paste0(name, " reported the binary, not CRAN, when both applied")
      )
    }
  })
})
