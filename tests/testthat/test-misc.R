# ------------------------------------------------------------------------------
# yardstick_table()

test_that("unweighted case is correct", {
  x <- data_altman()$pathology

  expect <- table(Prediction = x$scan, Truth = x$pathology)
  expect <- unclass(expect)
  storage.mode(expect) <- "double"

  expect_identical(
    yardstick_table(x$pathology, x$scan),
    expect
  )
})

test_that("two level weighted case is correct", {
  # Note, no (truth=y, estimate=y) value, but it appears in the table
  truth <- factor(c("x", "x", "y", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "y", "x", "x"), levels = c("x", "y"))
  case_weights <- c(1, 1, 2, .5)

  result <- yardstick_table(truth, estimate, case_weights = case_weights)

  expect <- matrix(
    c(1, 1, 2.5, 0),
    nrow = 2,
    ncol = 2,
    dimnames = list(Prediction = c("x", "y"), Truth = c("x", "y"))
  )

  expect_identical(result, expect)
})

test_that("three level weighted case is correct", {
  truth <- factor(c("x", "x", "y", "y", "z", "z", "z"), levels = c("x", "y", "z"))
  estimate <- factor(c("x", "y", "x", "x", "z", "x", "z"), levels = c("x", "y", "z"))
  case_weights <- c(1, 1, 2, .5, 2, 3, 3)

  result <- yardstick_table(truth, estimate, case_weights = case_weights)

  expect <- matrix(
    c(1, 1, 0, 2.5, 0, 0, 3, 0, 5),
    nrow = 3,
    ncol = 3,
    dimnames = list(Prediction = c("x", "y", "z"), Truth = c("x", "y", "z"))
  )

  expect_identical(result, expect)
})

test_that("validates input types", {
  x <- factor(c("x", "y"))

  expect_snapshot(error = TRUE, yardstick_table(1, x))
  expect_snapshot(error = TRUE, yardstick_table(x, 2))
})

test_that("levels must be exactly the same", {
  x <- factor(levels = c("x", "y"))
  y <- factor(levels = c("x"))
  z <- factor(levels = c("y", "x"))

  expect_snapshot(error = TRUE, yardstick_table(x, y))
  expect_snapshot(error = TRUE, yardstick_table(x, z))
})

test_that("must have at least 2 levels", {
  x <- factor(levels = c("x"))

  expect_snapshot(error = TRUE, yardstick_table(x, x))
})

test_that("case weights must be numeric", {
  x <- factor(levels = c("x", "y"))

  expect_snapshot(error = TRUE, yardstick_table(x, x, case_weights = "x"))
})

# ------------------------------------------------------------------------------
# yardstick_sum()

test_that("`na_remove` only removes NAs present in `x`", {
  # For consistency with `stats::weighted.sum()`

  x <- c(1, NA)
  w <- c(2, 1)

  expect_identical(yardstick_sum(x, case_weights = w), NA_real_)
  expect_identical(yardstick_sum(x, case_weights = w, na_remove = TRUE), 2)

  x <- c(1, 2)
  w <- c(2, NA)

  expect_identical(yardstick_sum(x, case_weights = w), NA_real_)
  expect_identical(yardstick_sum(x, case_weights = w, na_remove = TRUE), NA_real_)
})
