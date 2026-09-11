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
