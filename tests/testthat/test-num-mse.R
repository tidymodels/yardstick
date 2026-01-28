test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    mse_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean((ex_dat$obs - ex_dat$pred)^2)
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    mse_vec(ex_dat$obs, ex_dat$pred),
    mse(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    mse_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    mse_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean((ex_dat$obs - ex_dat$pred)^2, na.rm = TRUE)
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  truth <- solubility_test$solubility
  estimate <- solubility_test$prediction
  weights <- solubility_test$weights

  expect_identical(
    mse_vec(
      truth = truth,
      estimate = estimate,
      case_weights = weights
    ),
    weighted.mean((truth - estimate)^2, weights)
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    mse_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    mse_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    mse_vec(1, 1, na_rm = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(mse)
  range <- metric_range(mse)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )

  df$estimate <- df$truth
  df$off <- df$truth + 1

  expect_identical(
    mse_vec(df$truth, df$estimate),
    perfect
  )
  if (direction == "minimize") {
    expect_gt(mse_vec(df$truth, df$off), perfect)
    expect_lt(mse_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(mse_vec(df$truth, df$off), perfect)
    expect_gt(mse_vec(df$truth, df$off), worst)
  }
})

test_that("mse() - Integer columns are allowed (#44)", {
  ex_dat <- generate_numeric_test_data()
  ex_dat$obs <- as.integer(ex_dat$obs)

  expect_equal(
    mse_vec(ex_dat$obs, ex_dat$pred),
    mean((ex_dat$obs - ex_dat$pred)^2)
  )
})
