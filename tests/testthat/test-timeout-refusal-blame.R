# M094: an invalid `tidymedia.timeout` is refused by the function the caller
# typed.
#
# `resolve_timeout()` has always refused a value base R would mishandle, but it
# was reached from wherever the limit happened to be read first -- so 47 of the
# 53 exports in `tm_timeout_domain()` aborted naming `ffm_run(object)`,
# `ffm_batch(jobs, <the whole deparsed builder>)`, `mediainfo_read(file, inform)`
# or `purrr::map(infile, probe_one)`. `tm_timeout_blame_master()` records that
# state (measured at ae5ff1c); this file is the sweep that closes it.
#
# The domain is COMPUTED (`tm_timeout_domain()`), not recalled, for M70's
# reason: an export that starts reaching a spawn joins the sweep on its own.

test_that("the sweep runs over a non-empty domain with a cell for each member", {
  dir <- withr::local_tempdir()
  dom <- tm_timeout_domain()
  specs <- tm_timeout_call_specs(dir)
  # Guards on the guard: a domain that silently emptied, or a member with no
  # argument cell, would make every expectation below vacuously true.
  expect_gt(length(dom), 0)
  expect_setequal(names(tm_timeout_blame_master()), dom)
  expect_true(all(dom %in% names(specs)))
})

test_that("every domain member blames itself for an invalid limit (AC1, AC2)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  forms <- tm_timeout_bad_forms()
  # The override stays UNSET for this sweep, so `has_nvenc()` takes the branch
  # that reads the limit and is held to the same rule as everything else.
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))

  # `tm_refusal_head()` rather than `tm_blame_head()`: the head alone cannot
  # tell this refusal from any other error raised in the same frame, so a member
  # aborting on something else entirely would read as a pass (review F9). The
  # identity is checked against what the one checker site writes for that value.
  for (name in tm_timeout_domain()) {
    for (form in names(forms)) {
      expect_identical(
        tm_refusal_head(name, specs[[name]], forms[[form]]), name,
        info = paste(name, form)
      )
    }
  }
})

test_that("a set encoder override leaves `has_nvenc()` nothing to refuse", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = "h264_nvenc"))
  for (form in names(tm_timeout_bad_forms())) {
    limit <- tm_timeout_bad_forms()[[form]]
    expect_identical(
      tm_blame_head("has_nvenc", specs$has_nvenc, limit), "<none>",
      info = form
    )
    # And the answer is still the override's, not a stale or spawned one.
    expect_true(
      withr::with_options(list(tidymedia.timeout = limit), has_nvenc("h264"))
    )
  }
})

test_that("the refusal does not wait for `run = TRUE` (AC3)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  carries_run <- Filter(
    function(nm) {
      "run" %in% names(formals(get(nm, envir = asNamespace("tidymedia"))))
    },
    tm_timeout_domain()
  )
  # The asymmetry this closes was measured on master: `extract_audio(run =
  # FALSE)` raised nothing while `extract_audio_batch(run = FALSE)` aborted, so
  # a domain that lost the scalar half would leave the sweep green on the wrong
  # thing.
  expect_true("extract_audio" %in% carries_run)
  expect_true("extract_audio_batch" %in% carries_run)

  for (name in carries_run) {
    for (form in names(tm_timeout_bad_forms())) {
      args <- specs[[name]]
      args$run <- FALSE
      expect_identical(
        tm_blame_head(name, args, tm_timeout_bad_forms()[[form]]), name,
        info = paste(name, "run = FALSE", form)
      )
    }
  }
})

test_that("the refusal does not wait for the fan-out (AC3)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  carries_parallel <- Filter(
    function(nm) {
      "parallel" %in%
        names(formals(get(nm, envir = asNamespace("tidymedia"))))
    },
    tm_timeout_domain()
  )
  expect_gt(length(carries_parallel), 0)

  # No `skip_if_not_installed("furrr")` here on purpose. A machine-independent
  # refusal reports before a machine-dependent one (D036), and `furrr` is what
  # every one of these checks next -- so a correct front door never reaches
  # `check_installed()`, and a cell that DID reach it would fail here rather
  # than skip, which is the point.
  for (name in carries_parallel) {
    for (form in names(tm_timeout_bad_forms())) {
      args <- specs[[name]]
      args$parallel <- TRUE
      expect_identical(
        tm_blame_head(name, args, tm_timeout_bad_forms()[[form]]), name,
        info = paste(name, "parallel = TRUE", form)
      )
    }
  }
})

test_that("one wording reaches every member, from the one checker site (AC4)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL, cli.width = 80))

  for (form in names(tm_timeout_bad_forms())) {
    limit <- tm_timeout_bad_forms()[[form]]
    reference <- tm_resolve_timeout_message(limit)
    # The referent has to say something: an empty or missing reference would
    # make every comparison below pass against nothing.
    expect_match(reference, "must be a whole number")
    for (name in tm_timeout_domain()) {
      cnd <- tm_blame_condition(name, specs[[name]], limit)
      expect_false(is.null(cnd), info = paste(name, form))
      expect_identical(cnd$message, reference, info = paste(name, form))
    }
  }
})

test_that("the FFprobe readers no longer arrive wrapped by purrr (AC4)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(cli.width = 80))
  wrapped <- c("probe_all", "probe_audio", "probe_container", "probe_streams",
               "probe_video", "verify_media")
  # These six are exactly the members `tm_timeout_blame_master()` records as
  # blaming purrr::map() -- read from the recorded table, not retyped, so the
  # two cannot drift.
  master <- tm_timeout_blame_master()
  expect_setequal(wrapped, names(master)[master == "purrr::map"])

  for (name in wrapped) {
    for (form in names(tm_timeout_bad_forms())) {
      cnd <- tm_blame_condition(name, specs[[name]], tm_timeout_bad_forms()[[form]])
      expect_false("purrr_error_indexed" %in% cnd$classes,
                   info = paste(name, form))
      expect_no_match(cnd$message, "In index:", fixed = TRUE)
      expect_no_match(cnd$message, "Caused by error", fixed = TRUE)
    }
  }
})

test_that("the paths one argument cell cannot reach blame themselves too (AC1)", {
  # M094's review found three members still blaming a function the caller never
  # typed, each on a path `tm_timeout_call_specs()`'s single cell does not take:
  # a GPU encode reached `check_nvenc_available()` before the verb's own site
  # (F2), `extract_frame(frame = )` divided by `get_frame_rate()` first (F4),
  # and `normalize_audio_batch(two_pass = TRUE)` returned above its site (F3).
  dir <- withr::local_tempdir()
  variants <- tm_timeout_variant_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  # Guards on the guard: an empty variant table, or one that lost the three
  # axes the review named, would make every expectation below vacuous.
  expect_gt(length(variants), 0)
  expect_true("extract_frame [frame = ]" %in% names(variants))
  expect_true("normalize_audio_batch [two_pass = TRUE]" %in% names(variants))
  expect_gt(sum(grepl("hardware = nvenc", names(variants), fixed = TRUE)), 0)

  for (label in names(variants)) {
    cell <- variants[[label]]
    for (form in names(tm_timeout_bad_forms())) {
      expect_identical(
        tm_refusal_head(cell$name, cell$args, tm_timeout_bad_forms()[[form]]),
        cell$name,
        info = paste(label, form)
      )
    }
  }
})

test_that("a machine with no media binaries gets the same refusal (AC4)", {
  # The failure this closes was invisible on a developer machine and red on CI:
  # a member with no site of its own reached `run_program()`'s `Could not locate`
  # check (R/program_management.R) before `resolve_timeout()`, so the sweep
  # measured the runner's PATH instead of the package (review F8). D036 orders
  # the machine-independent refusal first, and that is what this asserts.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL, cli.width = 80))
  withr::local_envvar(list(PATH = ""))
  # The emptied PATH has to actually hide the binaries, or this leg asserts
  # nothing beyond the sweep above.
  expect_identical(unname(Sys.which("ffmpeg")), "")
  expect_identical(unname(Sys.which("mediainfo")), "")

  for (form in names(tm_timeout_bad_forms())) {
    limit <- tm_timeout_bad_forms()[[form]]
    for (name in tm_timeout_domain()) {
      expect_identical(
        suppressWarnings(tm_refusal_head(name, specs[[name]], limit)), name,
        info = paste(name, form, "PATH = \"\"")
      )
    }
  }
})

test_that("the valid and unset paths are byte-for-byte the pre-change ones (AC5)", {
  # The interception this reading rests on is complete: every function that
  # names a spawn primitive reaches it as an argument of guard_timeout(), which
  # the mock never forces. Asserted rather than assumed, because a spawn added
  # outside that wrapper would make every count below read 0 while a process ran.
  expect_true(tm_spawn_interception_complete())

  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  recorded <- tm_timeout_valid_baseline()
  expect_setequal(names(recorded), tm_timeout_domain())
  # The recorded table must describe calls that actually spawned, or "the spawn
  # count is unchanged" would be a claim about a column of zeros.
  expect_gt(sum(vapply(recorded, function(x) x$unset$spawns, integer(1))), 0)

  for (name in tm_timeout_domain()) {
    expect_identical(tm_spawn_trace(name, specs[[name]], NULL, dir),
                     recorded[[name]]$unset, info = paste(name, "unset"))
    expect_identical(tm_spawn_trace(name, specs[[name]], 30, dir),
                     recorded[[name]]$valid, info = paste(name, "valid"))
  }
})

test_that("the recorded baseline's provenance is read, not just carried (AC5)", {
  # The fixture's `source`/`generator`/`seed`/`recorded` fields were attached and
  # never consulted, so a blob regenerated from the wrong ref -- or by hand --
  # would go on comparing green against the wrong reading (review F10).
  recorded <- tm_timeout_valid_baseline()
  expect_true(tm_provenance_ok(recorded))
  # And the check can say no: a stripped blob, a blob from another ref, and a
  # blob from another generator each fail it.
  expect_false(tm_provenance_ok(structure(list(), provenance = NULL)))
  expect_false(tm_provenance_ok(recorded, ref = "0000000"))
  bad <- recorded
  prov <- attr(bad, "provenance")
  prov$generator <- "by hand"
  attr(bad, "provenance") <- prov
  expect_false(tm_provenance_ok(bad))
})

test_that("an invalid limit reaches no spawn at all (AC5)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  for (name in tm_timeout_domain()) {
    for (form in names(tm_timeout_bad_forms())) {
      trace <- tm_spawn_trace(name, specs[[name]], tm_timeout_bad_forms()[[form]],
                              dir)
      expect_identical(trace$spawns, 0L, info = paste(name, form))
    }
  }
})

test_that("the interception check can say no (AC5)", {
  # The planted defect is the one the check exists to catch: a spawn that is not
  # an argument of guard_timeout(). Without this the check's only falsifier
  # would be deleting it, which certifies nothing.
  outside <- function() system2("ffmpeg", "-version")
  inside <- function() guard_timeout("FFmpeg", 0, system2("ffmpeg", "-version"))
  expect_false(tm_spawn_interception_complete(fns = list(bad = outside)))
  expect_true(tm_spawn_interception_complete(fns = list(good = inside)))
  # And one guarded spawn does not vouch for an unguarded one beside it.
  both <- function() {
    guard_timeout("FFmpeg", 0, system2("ffmpeg", "-version"))
    system2("ffprobe", "-version")
  }
  expect_false(tm_spawn_interception_complete(fns = list(both = both)))
})
