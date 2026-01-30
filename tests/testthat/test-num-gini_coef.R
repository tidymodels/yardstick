test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  # Reference implementation based on MLmetrics NormalizedGini
  normalized_gini <- function(truth, estimate) {
    gini <- function(truth, order_by) {
      ord <- order(order_by, decreasing = TRUE)
      truth_sorted <- truth[ord]
      n <- length(truth)
      cumulative <- cumsum(truth_sorted)
      gini_sum <- sum(cumulative) / sum(truth)
      gini_sum <- gini_sum - (n + 1) / 2
      gini_sum / n
    }
    gini(truth, estimate) / gini(truth, truth)
  }

  expect_equal(
    gini_coef_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    normalized_gini(ex_dat$obs, ex_dat$pred),
    tolerance = 0.001
  )

  # MLmetrics::NormalizedGini(ex_dat$pred, ex_dat$obs)
  exp <- 0.8464368

  expect_equal(
    gini_coef_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    exp,
    tolerance = 0.001
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    gini_coef_vec(ex_dat$obs, ex_dat$pred),
    gini_coef(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    gini_coef_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  # Reference implementation based on MLmetrics NormalizedGini
  normalized_gini <- function(truth, estimate) {
    gini <- function(truth, order_by) {
      ord <- order(order_by, decreasing = TRUE)
      truth_sorted <- truth[ord]
      n <- length(truth)
      cumulative <- cumsum(truth_sorted)
      gini_sum <- sum(cumulative) / sum(truth)
      gini_sum <- gini_sum - (n + 1) / 2
      gini_sum / n
    }
    gini(truth, estimate) / gini(truth, truth)
  }

  expect_equal(
    gini_coef_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    normalized_gini(ex_dat$obs[-na_ind], ex_dat$pred[-na_ind]),
    tolerance = 0.001
  )
})

test_that("Case weights calculations are correct", {
  # Test that weighted result differs from unweighted
  solubility_test$weights <- read_weights_solubility_test()

  unweighted <- gini_coef(
    solubility_test,
    solubility,
    prediction
  )[[".estimate"]]

  weighted <- gini_coef(
    solubility_test,
    solubility,
    prediction,
    case_weights = weights
  )[[".estimate"]]

  expect_true(weighted != unweighted)
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    gini_coef_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    gini_coef_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    gini_coef_vec(1, 1, na_rm = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(gini_coef)
  range <- metric_range(gini_coef)
  perfect <- ifelse(direction == "minimize", range[1], range[2])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )

  # Perfect ranking - estimate perfectly ranks truth
  df$estimate <- df$truth

  expect_identical(
    gini_coef_vec(df$truth, df$estimate),
    perfect
  )

  # Test that imperfect ranking gives lower than perfect
  df$imperfect <- c(1, 2, 3, 4, 5, 6, 7)
  expect_lt(gini_coef_vec(df$truth, df$imperfect), perfect)
})

test_that("perfect predictions give Gini of 1", {
  truth <- c(10, 20, 30, 40, 50)
  estimate <- truth

  expect_equal(gini_coef_vec(truth, estimate), 1)
})

test_that("inverse predictions give negative Gini", {
  truth <- c(10, 20, 30, 40, 50)
  estimate <- rev(truth)

  expect_lt(gini_coef_vec(truth, estimate), 0)
})

test_that("constant truth returns NA with warning", {
  truth <- rep(5, 10)
  estimate <- 1:10

  expect_snapshot(
    result <- gini_coef_vec(truth, estimate)
  )
  expect_identical(result, NA_real_)
})

test_that("single observation returns NA", {
  expect_identical(
    gini_coef_vec(5, 3),
    NA_real_
  )
})

test_that("zero sum truth returns NA with warning", {
  truth <- c(-2, -1, 0, 1, 2)
  estimate <- c(1, 2, 3, 4, 5)

  expect_snapshot(
    result <- gini_coef_vec(truth, estimate)
  )
  expect_identical(result, NA_real_)
})
