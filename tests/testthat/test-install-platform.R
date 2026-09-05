# M108 AC4. The one test in the suite that asks the machine rather than a mock.
#
# Every other test of the platform gate holds `tm_os()` at a value it chose --
# which is what makes them tests of the gate rather than of the seam, and also
# what would let a seam wired to nothing pass all of them and ship an installer
# that refuses on Windows or downloads on Linux. Nothing here is mocked except
# the confirmation, and that only on the leg where the gate lets execution
# through: without it a green Windows run would download a hundred megabytes.
#
# Nothing here skips, on any platform or on CRAN. The point is that the file
# runs on all three CI legs and asserts a different outcome on each.

test_that("the seam reports the machine this is actually running on", {
  expect_identical(tm_os(), tolower(Sys.info()[["sysname"]]))
})

test_that("the gate's real verdict follows the real platform", {
  host <- tolower(Sys.info()[["sysname"]])

  if (host == "windows") {
    # The windows-latest leg. The gate refuses nothing and execution reaches
    # the consent prompt, which declines: the call returns FALSE having
    # written nothing, and this test spends no bandwidth proving it.
    reached <- FALSE
    testthat::local_mocked_bindings(
      tm_confirm = function(...) {
        reached <<- TRUE
        FALSE
      }
    )
    expect_false(install_on_win())
    expect_true(reached)
  } else {
    # The macos-latest and ubuntu-latest legs, and any developer machine that
    # is not Windows. No stub stands between the call and a download here --
    # the gate is the only thing stopping it, so a gate that did not fire
    # would fail this test by trying to install FFmpeg.
    cnd <- expect_error(install_on_win(), class = "tidymedia_wrong_platform")
    expect_identical(cnd$tm_platform, host)
    expect_match(
      cli::ansi_strip(conditionMessage(cnd)), host,
      fixed = TRUE
    )
  }
})
