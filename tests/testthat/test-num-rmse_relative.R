test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  truth_range <- max(ex_dat$obs) - min(ex_dat$obs)
  rmse <- sqrt(mean((ex_dat$obs - ex_dat$pred)^2))

  expect_equal(
    rmse_relative_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    rmse / truth_range
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    rmse_relative_vec(ex_dat$obs, ex_dat$pred),
    rmse_relative(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    rmse_relative_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  truth_range <- max(ex_dat$obs, na.rm = TRUE) - min(ex_dat$obs, na.rm = TRUE)
  rmse <- sqrt(mean((ex_dat$obs - ex_dat$pred)^2, na.rm = TRUE))

  expect_equal(
    rmse_relative_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    rmse / truth_range
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  truth <- solubility_test$solubility
  estimate <- solubility_test$prediction
  weights <- solubility_test$weights

  truth_range <- max(truth) - min(truth)
  weighted_mse <- weighted.mean((truth - estimate)^2, weights)
  expected <- sqrt(weighted_mse) / truth_range

  expect_equal(
    rmse_relative_vec(
      truth = truth,
      estimate = estimate,
      case_weights = weights
    ),
    expected
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rmse_relative_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    rmse_relative_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    rmse_relative_vec(1, 1, na_rm = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(rmse_relative)
  range <- metric_range(rmse_relative)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )

  df$estimate <- df$truth
  df$off <- df$truth + 1

  expect_identical(
    rmse_relative_vec(df$truth, df$estimate),
    perfect
  )
  if (direction == "minimize") {
    expect_gt(rmse_relative_vec(df$truth, df$off), perfect)
    expect_lt(rmse_relative_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(rmse_relative_vec(df$truth, df$off), perfect)
    expect_gt(rmse_relative_vec(df$truth, df$off), worst)
  }
})

test_that("rmse_relative() returns Inf when truth range is zero", {
  expect_identical(
    rmse_relative_vec(truth = c(5, 5, 5), estimate = c(4, 5, 6)),
    Inf
  )
})
