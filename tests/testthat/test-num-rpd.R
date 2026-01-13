test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    rpd_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    stats::sd(ex_dat$obs) / (sqrt(mean((ex_dat$obs - ex_dat$pred)^2)))
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  sd <- yardstick_sd(
    solubility_test$solubility,
    case_weights = solubility_test$weights
  )
  rmse <- rmse_vec(
    solubility_test$solubility,
    solubility_test$prediction,
    case_weights = solubility_test$weights
  )
  exp <- sd / rmse

  expect_identical(
    rpd_vec(
      truth = solubility_test$solubility,
      estimate = solubility_test$prediction,
      case_weights = solubility_test$weights
    ),
    exp
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rpd_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    rpd_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    rpd_vec(1, 1, na_rm = "yes")
  )
})
