test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  # Note: Uses `quantile(type = 7)` when case weights aren't provided
  expect_equal(
    rpiq_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    stats::IQR(ex_dat$obs) / sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    rpiq_vec(ex_dat$obs, ex_dat$pred),
    rpiq(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    rpiq_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    rpiq_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    stats::IQR(ex_dat$obs[-na_ind]) /
      sqrt(mean((ex_dat$obs[-na_ind] - ex_dat$pred[-na_ind])^2))
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    rpiq_vec(
      truth = solubility_test$solubility,
      estimate = solubility_test$prediction,
      case_weights = solubility_test$weights
    ),
    3.401406885440771965534
  )
})

test_that("works with hardhat case weights", {
  count_results <- data_counts()$basic
  count_results$weights <- c(1, 2, 1, 1, 2, 1)

  df <- count_results

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rpiq_vec(df$count, df$pred, case_weights = imp_wgt)
  )

  expect_no_error(
    rpiq_vec(df$count, df$pred, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    rpiq_vec(1, 1, na_rm = "yes")
  )
})
