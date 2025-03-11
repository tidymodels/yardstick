test_that("Mean Absolute Percentage Error", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    mape(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    100 * mean(abs((ex_dat$obs - ex_dat$pred) / ex_dat$obs))
  )
  expect_equal(
    mape(ex_dat, obs, pred_na)[[".estimate"]],
    100 *
      mean(abs((ex_dat$obs[not_na] - ex_dat$pred[not_na]) / ex_dat$obs[not_na]))
  )
})

test_that("`mape()` computes expected values when singular `truth` is `0` (#271)", {
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

test_that("Weighted results are the same as scikit-learn", {
  solubility_test$weights <- read_weights_solubility_test()
  zero_solubility <- solubility_test$solubility == 0
  solubility_test_not_zero <- solubility_test[!zero_solubility, ]

  expect_equal(
    mape(
      solubility_test_not_zero,
      solubility,
      prediction,
      case_weights = weights
    )[[".estimate"]],
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
