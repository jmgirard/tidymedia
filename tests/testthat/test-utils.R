test_that("pad_integers() intuits width from the maximum value", {
  expect_equal(pad_integers(c(1, 10, 100)), c("001", "010", "100"))
})

test_that("pad_integers() honours an explicit width", {
  expect_equal(pad_integers(c(1, 2), width = 2), c("01", "02"))
})

test_that("pad_integers() honours a custom flag", {
  # "-" left-justifies within the intuited width.
  expect_equal(pad_integers(c(1, 20), width = 3, flag = "-"), c("1  ", "20 "))
})

test_that("pad_integers() rejects non-integerish input", {
  expect_error(pad_integers(c(1.5, 2.5)))
})
