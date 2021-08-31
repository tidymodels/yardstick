# ------------------------------------------------------------------------------
# yardstick_table()

test_that("unweighted case is correct", {
  x <- data_altman()$pathology

  expect_identical(
    yardstick_table(x$pathology, x$scan),
    table(x$scan, x$pathology, dnn = c("Prediction", "Truth"))
  )
})

test_that("two level weighted case is correct", {
  # Note, no (truth=y, estimate=y) value, but it appears in the table
  truth <- factor(c("x", "x", "y", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "y", "x", "x"), levels = c("x", "y"))
  case_weights <- c(1, 1, 2, .5)

  result <- yardstick_table(truth, estimate, case_weights = case_weights)

  expect <- as.table(matrix(
    c(1, 1, 2.5, 0),
    nrow = 2,
    ncol = 2,
    dimnames = list(Prediction = c("x", "y"), Truth = c("x", "y"))
  ))

  expect_identical(result, expect)
})

test_that("three level weighted case is correct", {
  truth <- factor(c("x", "x", "y", "y", "z", "z", "z"), levels = c("x", "y", "z"))
  estimate <- factor(c("x", "y", "x", "x", "z", "x", "z"), levels = c("x", "y", "z"))
  case_weights <- c(1, 1, 2, .5, 2, 3, 3)

  result <- yardstick_table(truth, estimate, case_weights = case_weights)

  expect <- as.table(matrix(
    c(1, 1, 0, 2.5, 0, 0, 3, 0, 5),
    nrow = 3,
    ncol = 3,
    dimnames = list(Prediction = c("x", "y", "z"), Truth = c("x", "y", "z"))
  ))

  expect_identical(result, expect)
})

test_that("validates input types", {
  x <- factor(c("x", "y"))

  expect_error(yardstick_table(1, x), "`truth` must be a factor")
  expect_error(yardstick_table(x, 2), "`estimate` must be a factor")
})

test_that("levels must be exactly the same", {
  x <- factor(levels = c("x", "y"))
  y <- factor(levels = c("x"))
  z <- factor(levels = c("y", "x"))

  expect_error(yardstick_table(x, y), "same levels in the same order")
  expect_error(yardstick_table(x, z), "same levels in the same order")
})

test_that("must have at least 2 levels", {
  x <- factor(levels = c("x"))

  expect_error(yardstick_table(x, x), "at least 2 factor levels")
})

test_that("case weights must be numeric", {
  x <- factor(levels = c("x", "y"))

  expect_error(yardstick_table(x, x, case_weights = "x"), "must be a numeric vector")
})

# ------------------------------------------------------------------------------
