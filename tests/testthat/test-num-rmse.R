test_that("rmse", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    rmse(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rmse(ex_dat, truth = obs, estimate = "pred_na")[[".estimate"]],
    sqrt(mean((ex_dat$obs[not_na] - ex_dat$pred[not_na])^2))
  )
})

test_that("Weighted results are the same as scikit-learn", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_identical(
    rmse(solubility_test, solubility, prediction, case_weights = weights)[[
      ".estimate"
    ]],
    read_pydata("py-rmse")$case_weight
  )
})

test_that("Integer columns are allowed (#44)", {
  ex_dat <- generate_numeric_test_data()
  ex_dat$obs <- as.integer(ex_dat$obs)

  expect_equal(
    rmse(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rmse_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    rmse_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})
