test_that("rpiq", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  # Note: Uses `quantile(type = 7)` when case weights aren't provided
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::IQR(ex_dat$obs) / sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::IQR(ex_dat$obs[not_na]) /
      sqrt(mean((ex_dat$obs[not_na] - ex_dat$pred[not_na])^2))
  )
})

test_that("case weights are applied", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    rpiq(solubility_test, solubility, prediction, case_weights = weights)[[
      ".estimate"
    ]],
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
