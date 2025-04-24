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
  truth <- factor(
    c("x", "x", "y", "y", "z", "z", "z"),
    levels = c("x", "y", "z")
  )
  estimate <- factor(
    c("x", "y", "x", "x", "z", "x", "z"),
    levels = c("x", "y", "z")
  )
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

test_that("works with hardhat case weights", {
  x <- factor(c("x", "y", "x"), levels = c("x", "y"))
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_table(x, x, case_weights = w),
    yardstick_table(x, x, case_weights = as.integer(w))
  )
})

# ------------------------------------------------------------------------------
# yardstick_mean()

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_mean(x, case_weights = w),
    2 + 4 / 9
  )
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
  expect_identical(
    yardstick_sum(x, case_weights = w, na_remove = TRUE),
    NA_real_
  )
})

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_sum(x, case_weights = w),
    22
  )
})

# ------------------------------------------------------------------------------
# yardstick_sd()

test_that("works with constant inputs", {
  x <- c(1, 1)

  expect_identical(yardstick_sd(x), 0)
  expect_identical(yardstick_sd(x), sd(x))
})

test_that("works with input of size 1", {
  expect_identical(yardstick_sd(0), NA_real_)
  expect_identical(yardstick_sd(0), sd(0))
})

test_that("works with input of size 0", {
  expect_identical(yardstick_sd(double()), NA_real_)
  expect_identical(yardstick_sd(double()), sd(double()))
})

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_sd(x, case_weights = w),
    yardstick_sd(x, case_weights = as.integer(w))
  )
})

# ------------------------------------------------------------------------------
# yardstick_var()

test_that("works with constant inputs", {
  x <- c(1, 1)

  expect_identical(yardstick_var(x), 0)
  expect_identical(yardstick_var(x), var(x))
})

test_that("works with input of size 1", {
  expect_identical(yardstick_var(0), NA_real_)
  expect_identical(yardstick_var(0), var(0))
})

test_that("works with input of size 0", {
  expect_identical(yardstick_var(double()), NA_real_)
  expect_identical(yardstick_var(double()), var(double()))
})

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_var(x, case_weights = w),
    yardstick_var(x, case_weights = as.integer(w))
  )
})

# ------------------------------------------------------------------------------
# yardstick_cov()

test_that("works with constant inputs", {
  x <- c(1, 1)
  y <- c(2, 3)

  expect_identical(yardstick_cov(x, y), 0)
  expect_identical(yardstick_cov(x, y), cov(x, y))

  x <- c(2, 3)
  y <- c(1, 1)

  expect_identical(yardstick_cov(x, y), 0)
  expect_identical(yardstick_cov(x, y), cov(x, y))

  x <- c(1, 1)
  y <- c(1, 1)

  expect_identical(yardstick_cov(x, y), 0)
  expect_identical(yardstick_cov(x, y), cov(x, y))
})

test_that("works with input of size 1", {
  expect_identical(yardstick_cov(0, 0), NA_real_)
  expect_identical(yardstick_cov(0, 0), cov(0, 0))
})

test_that("works with input of size 0", {
  expect_identical(yardstick_cov(double(), double()), NA_real_)
  expect_identical(yardstick_cov(double(), double()), cov(double(), double()))
})

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_cov(x, x, case_weights = w),
    yardstick_cov(x, x, case_weights = as.integer(w))
  )
})

# ------------------------------------------------------------------------------
# yardstick_cor()

test_that("works with constant inputs", {
  expect_snapshot({
    (expect_warning(
      object = out <- yardstick_cor(c(1, 2), c(1, 1)),
      class = "yardstick_warning_correlation_undefined_constant_estimate"
    ))
  })
  expect_identical(out, NA_real_)

  expect_snapshot({
    (expect_warning(
      object = out <- yardstick_cor(c(1, 1), c(1, 2)),
      class = "yardstick_warning_correlation_undefined_constant_truth"
    ))
  })
  expect_identical(out, NA_real_)
})

test_that("warns with input of size 1", {
  expect_snapshot({
    (expect_warning(
      object = out <- yardstick_cor(1, 1),
      class = "yardstick_warning_correlation_undefined_size_zero_or_one"
    ))
  })
  expect_identical(out, NA_real_)
})

test_that("warns with input of size 0", {
  expect_snapshot({
    (expect_warning(
      object = out <- yardstick_cor(double(), double()),
      class = "yardstick_warning_correlation_undefined_size_zero_or_one"
    ))
  })
  expect_identical(out, NA_real_)
})

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    yardstick_cor(x, x, case_weights = w),
    yardstick_cor(x, x, case_weights = as.integer(w))
  )
})

# ------------------------------------------------------------------------------
# weighted_quantile()

test_that("is a weighted variant of `quantile(type = 4)`", {
  x <- 1:20 + 0
  w <- rep(1, times = length(x))

  expect_identical(
    quantile(x, probs = c(0, .25, .5, .75, 1), type = 4, names = FALSE),
    weighted_quantile(x, weights = w, probabilities = c(0, .25, .5, .75, 1))
  )

  x <- rev(0:20 + 0)
  w <- rep(1, times = length(x))

  expect_identical(
    quantile(x, probs = c(0, .25, .5, .75, 1), type = 4, names = FALSE),
    weighted_quantile(x, weights = w, probabilities = c(0, .25, .5, .75, 1))
  )
})

test_that("works with zero values", {
  expect_identical(
    weighted_quantile(numeric(), numeric(), c(.5, .6)),
    c(NA_real_, NA_real_)
  )
})

test_that("works with one value", {
  expect_identical(weighted_quantile(2, 5, c(.5, .6)), c(2, 2))
})

test_that("works with zero percentiles", {
  expect_identical(weighted_quantile(1:5, 1:5, numeric()), numeric())
})

test_that("works with hardhat case weights", {
  x <- 1:3
  w <- hardhat::frequency_weights(c(1, 3, 5))

  expect_identical(
    weighted_quantile(x, w, .5),
    weighted_quantile(x, as.integer(w), .5)
  )
})

test_that("`x` is validated", {
  expect_snapshot(error = TRUE, weighted_quantile("x", 1, .5))
})

test_that("`weights` is validated", {
  expect_snapshot(error = TRUE, weighted_quantile(1, "x", .5))
})

test_that("`x` and `weights` must be the same size", {
  expect_snapshot(error = TRUE, weighted_quantile(1, 1:2, .5))
})

test_that("`probabilities` is validated", {
  expect_snapshot(error = TRUE, weighted_quantile(1, 1, "x"))
})

test_that("`probabilities` must be in [0, 1]", {
  expect_snapshot(error = TRUE, weighted_quantile(1, 1, -1))
  expect_snapshot(error = TRUE, weighted_quantile(1, 1, 2))
})

test_that("`probabilities` can't be missing", {
  expect_snapshot(error = TRUE, weighted_quantile(1, 1, NA))
})

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  cp_estimate <- probably::as_class_pred(two_class_example$predicted, which = 2)

  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  fct_estimate <- two_class_example$predicted
  fct_estimate[2] <- NA

  local_mocked_bindings(
    .package = "rlang",
    detect_installed = function(pkg, ...) {
      FALSE
    }
  )

  expect_snapshot(
    error = TRUE,
    accuracy_vec(fct_truth, cp_estimate)
  )
})
