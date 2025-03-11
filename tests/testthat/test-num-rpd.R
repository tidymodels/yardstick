test_that("rpd", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::sd(ex_dat$obs) / (sqrt(mean((ex_dat$obs - ex_dat$pred)^2)))
  )
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::sd(ex_dat$obs[not_na]) /
      (sqrt(mean((ex_dat$obs[not_na] - ex_dat$pred[not_na])^2)))
  )
})

test_that("case weights are applied", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_identical(
    rpd(solubility_test, solubility, prediction, case_weights = weights)[[
      ".estimate"
    ]],
    {
      sd <- yardstick_sd(
        solubility_test$solubility,
        case_weights = solubility_test$weights
      )
      rmse <- rmse_vec(
        solubility_test$solubility,
        solubility_test$prediction,
        case_weights = solubility_test$weights
      )
      sd / rmse
    }
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
