test_that("Symmetric Mean Absolute Percentage Error", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    smape(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    100 *
      mean(
        abs(
          (ex_dat$obs - ex_dat$pred) /
            ((abs(ex_dat$obs) + abs(ex_dat$pred)) / 2)
        )
      )
  )
  expect_equal(
    smape(ex_dat, obs, pred_na)[[".estimate"]],
    100 *
      mean(
        abs(
          (ex_dat$obs[not_na] - ex_dat$pred[not_na]) /
            ((abs(ex_dat$obs[not_na]) + abs(ex_dat$pred[not_na])) / 2)
        )
      )
  )
})

test_that("Weighted results are working", {
  truth <- c(1, 2, 3)
  estimate <- c(2, 4, 3)
  weights <- c(1, 2, 1)

  expect_identical(
    smape_vec(truth, estimate, case_weights = weights),
    50
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    smape_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    smape_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})
