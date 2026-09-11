# M125 AC1/AC2: every export that reaches ffm_batch() refuses two jobs whose
# destinations resolve to one path -- at its own front door, before any program
# starts, on both `run` values -- and accepts the same call once the
# destinations differ. The cells and the stubs are in
# helper-batch-output-collision.R.

test_that("every export reaching ffm_batch() has a collision cell (M125)", {
  domain <- input_guard_verbs()$fanout
  # One fact stated independently of the walk, so an emptied graph cannot
  # certify an empty cell list.
  expect_true(all(c("segment_video", "strip_metadata_batch",
                    "separate_audio_video_batch") %in% domain))
  covered <- unique(vapply(tm_collision_cells(), `[[`, character(1), "verb"))
  expect_setequal(covered, domain)
})

for (cell in tm_collision_cells()) {
  for (run in c(FALSE, TRUE)) {
    local({
      cell <- cell
      run <- run

      test_that(sprintf("%s() refuses a colliding %s before any program starts (run = %s)",
                        cell$verb, cell$form, run), {
        local_collision_files()
        res <- tm_collision_run(cell$verb, cell$bad, run)
        if (!inherits(res$cnd, "error")) {
          fail("the colliding call returned without an error")
          return(invisible())
        }
        expect_false(tm_cnd_has(res$cnd, "tm_m125_batch"),
                     label = "the front door let the call reach ffm_batch()")
        expect_identical(res$spawned, character())
        expect_identical(blamed_verb(res$cnd), cell$verb)
        expect_match(cli::ansi_strip(conditionMessage(res$cnd)), cell$dest,
                     fixed = TRUE)
      })

      test_that(sprintf("%s() accepts distinct destinations for its %s control (run = %s)",
                        cell$verb, cell$form, run), {
        local_collision_files()
        res <- tm_collision_run(cell$verb, cell$ok, run)
        ended_on_stub <- tm_cnd_has(res$cnd, "tm_m125_batch") ||
          tm_cnd_has(res$cnd, "tm_m125_spawn")
        expect(ended_on_stub, paste(
          "the control ended somewhere other than a stub:",
          if (inherits(res$cnd, "condition")) {
            cli::ansi_strip(conditionMessage(res$cnd))
          } else {
            "no error"
          }
        ))
        expect_identical(length(res$spawned) > 0,
                         as.character(run) %in% cell$spawns)
      })
    })
  }
}

# An output column that is not text, or repeats NA, is not a collision: it is
# left to ffm_files()'s own "must be a single string" refusal, as before M125,
# rather than read as a repeated path (M125 review O4).
test_that("a non-text or NA output column is refused for its type, not as a collision (M125)", {
  local_collision_files()
  testthat::local_mocked_bindings(
    find_program = function(program = "ffmpeg", ...) {
      file.path("/nonexistent", program[[1]])
    },
    guard_timeout = function(program, limit, expr, ...) {
      cli::cli_abort("M125 stub: {program} would start here.",
                     class = "tm_m125_spawn")
    },
    .package = "tidymedia"
  )
  outputs <- list(numeric = c(1, 2, 1, 2), missing = c(NA, NA, NA, NA))
  for (nm in names(outputs)) {
    jobs <- tibble::tibble(input = c("a.mp4", "b.mp4", "a.mp4", "b.mp4"),
                           output = outputs[[nm]])
    cnd <- tryCatch(standardize_video_batch(jobs, run = FALSE),
                    error = function(e) e)
    expect_s3_class(cnd, "error")
    msgs <- character()
    while (inherits(cnd, "condition")) {
      msgs <- c(msgs, cli::ansi_strip(conditionMessage(cnd)))
      cnd <- cnd$parent
    }
    expect_false(any(grepl("same output path", msgs, fixed = TRUE)), label = nm)
    expect_true(any(grepl("`output` must be a single string", msgs,
                          fixed = TRUE)), label = nm)
  }
})

# M125 AC3: ffm_batch() called directly -----------------------------------------

# One pipeline per row: `output` as given, and each `opts` cell handed to
# ffm_output_options() as separate strings, so a cell can spell `-f null` as one
# string or as two.
collision_f <- function(input, output, opts = NULL, ...) {
  p <- ffm_files(input, output)
  if (length(opts)) p <- do.call(ffm_output_options, c(list(p), as.list(opts)))
  p
}

# Call ffm_batch() by name with the spawn wrapper stubbed, and report the
# condition it ended on (or its value) and every program a job tried to start.
# Unlike tm_collision_run() nothing here stubs ffm_batch(): it is the function
# under test.
collision_batch_run <- function(jobs, run, ...) {
  spawned <- character()
  testthat::local_mocked_bindings(
    find_program = function(program = "ffmpeg", ...) {
      file.path("/nonexistent", program[[1]])
    },
    guard_timeout = function(program, limit, expr, ...) {
      spawned <<- c(spawned, program)
      cli::cli_abort("M125 stub: {program} would start here.",
                     class = "tm_m125_spawn")
    },
    .package = "tidymedia"
  )
  value <- tryCatch(
    ffm_batch(jobs, collision_f, run = run, ...),
    error = function(e) e
  )
  list(value = value, spawned = spawned)
}

expect_batch_collision <- function(res, dest) {
  if (!inherits(res$value, "error")) {
    fail("ffm_batch() returned without an error")
    return(invisible())
  }
  expect_identical(res$spawned, character())
  expect_identical(blamed_verb(res$value), "ffm_batch")
  msg <- cli::ansi_strip(conditionMessage(res$value))
  expect_match(msg, "same output path", fixed = TRUE)
  expect_match(msg, dest, fixed = TRUE)
  # No row number: the paths are what the caller can act on.
  expect_no_match(msg, "[Rr]ow|[Jj]ob [0-9]")
}

for (run in c(FALSE, TRUE)) {
  local({
    run <- run

    test_that(sprintf("ffm_batch() refuses two pipelines sharing an output before any job runs (run = %s)", run), {
      a <- make_input()
      jobs <- tibble::tibble(input = c(a, a), output = c("o.mp4", "o.mp4"))
      expect_batch_collision(collision_batch_run(jobs, run), "o.mp4")
    })

    test_that(sprintf("ffm_batch() still refuses when a later -f overrides -f null (run = %s)", run), {
      a <- make_input()
      one_string <- tibble::tibble(
        input = c(a, a), output = c("o.mp4", "o.mp4"),
        opts = list("-f null -f mp4", "-f null -f mp4")
      )
      expect_batch_collision(collision_batch_run(one_string, run), "o.mp4")
      separate <- tibble::tibble(
        input = c(a, a), output = c("o.mp4", "o.mp4"),
        opts = list(c("-f null", "-f mp4"), c("-f", "null", "-f mp4"))
      )
      expect_batch_collision(collision_batch_run(separate, run), "o.mp4")
    })

    test_that(sprintf("ffm_batch() refuses the real repeat in a table mixing exempt rows (run = %s)", run), {
      a <- make_input()
      jobs <- tibble::tibble(
        input = rep(a, 6),
        output = c("-", "-", "pipe:1", "pipe:1", "o.mp4", "o.mp4")
      )
      res <- collision_batch_run(jobs, run)
      expect_batch_collision(res, "o.mp4")
      if (inherits(res$value, "error")) {
        msg <- cli::ansi_strip(conditionMessage(res$value))
        expect_no_match(msg, "\"-\"", fixed = TRUE)
        expect_no_match(msg, "pipe:1", fixed = TRUE)
      }
    })
  })
}

test_that("ffm_batch() compiles a table whose repeated outputs write no file (M125)", {
  a <- make_input()
  exempt <- list(
    stdout = tibble::tibble(input = c(a, a), output = c("-", "-")),
    pipe = tibble::tibble(input = c(a, a), output = c("pipe:1", "pipe:1")),
    null_one_string = tibble::tibble(
      input = c(a, a), output = c("x.out", "x.out"),
      opts = list("-f null", "-f null")
    ),
    null_two_strings = tibble::tibble(
      input = c(a, a), output = c("x.out", "x.out"),
      opts = list(c("-f", "null"), c("-an", "-f", "null"))
    ),
    null_after_override = tibble::tibble(
      input = c(a, a), output = c("x.out", "x.out"),
      opts = list("-f mp4 -f null", c("-f mp4", "-f null"))
    )
  )
  for (nm in names(exempt)) {
    res <- collision_batch_run(exempt[[nm]], run = FALSE)
    expect(!inherits(res$value, "error"), paste0(
      nm, ": ", if (inherits(res$value, "error")) {
        cli::ansi_strip(conditionMessage(res$value))
      }
    ))
    expect_identical(res$spawned, character())
  }
  # The control beside them: a real destination repeated in the same shape is
  # refused, so the tables above pass for their outputs and not for their shape.
  res <- collision_batch_run(
    tibble::tibble(input = c(a, a), output = c("x.out", "x.out"),
                   opts = list("-an", "-an")),
    run = FALSE
  )
  expect_batch_collision(res, "x.out")
})

test_that("ffm_batch() runs a table whose repeated outputs write no file (M125)", {
  skip_on_cran()
  skip_if_no_ffmpeg()
  v <- make_test_video()
  jobs <- tibble::tibble(
    input = c(v, v), output = c("-", "-"), opts = list("-f null", "-f null")
  )
  out <- ffm_batch(jobs, collision_f, run = TRUE)
  expect_identical(out$success, c(TRUE, TRUE))
})

test_that("ffm_batch()'s verify-spec refusal still reports before the collision (M125)", {
  a <- make_input()
  jobs <- tibble::tibble(input = c(a, a), output = c("o.mp4", "o.mp4"),
                         bad = c(TRUE, FALSE))
  # One invalid spec rather than two: with two, resolve_batch_verify()'s message
  # fails inside cli on its own `{?s}` and never says what it refuses.
  res <- collision_batch_run(
    jobs, run = TRUE,
    verify = function(bad, ...) if (bad) list() else list(width = 1)
  )
  expect_s3_class(res$value, "error")
  expect_match(cli::ansi_strip(conditionMessage(res$value)),
               "verify", fixed = TRUE)
  expect_no_match(cli::ansi_strip(conditionMessage(res$value)),
                  "same output path", fixed = TRUE)
})
