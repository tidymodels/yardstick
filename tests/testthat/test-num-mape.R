test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    mape_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    100 * mean(abs((ex_dat$obs - ex_dat$pred) / ex_dat$obs))
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()
  zero_solubility <- solubility_test$solubility == 0
  solubility_test_not_zero <- solubility_test[!zero_solubility, ]

  expect_equal(
    mape_vec(
      truth = solubility_test_not_zero$solubility,
      estimate = solubility_test_not_zero$prediction,
      case_weights = solubility_test_not_zero$weights
    ),
    read_pydata("py-mape")$case_weight * 100
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    mape_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    mape_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    mape_vec(1, 1, na_rm = "yes")
  )
})

test_that("mape() - computes expected values when singular `truth` is `0` (#271)", {
  expect_identical(
    mape_vec(truth = 0, estimate = 1),
    Inf
  )

  expect_identical(
    mape_vec(truth = 0, estimate = -1),
    Inf
  )

  expect_identical(
    mape_vec(truth = 0, estimate = 0),
    NaN
  )
})
