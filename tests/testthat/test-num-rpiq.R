test_that('rpiq', {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  # Note: Uses `quantile(type = 7)` when case weights aren't provided
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::IQR(ex_dat$obs) / sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::IQR(ex_dat$obs[not_na]) / sqrt(mean((ex_dat$obs[not_na] - ex_dat$pred[not_na])^2))
  )
})

test_that("case weights are applied", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    rpiq(solubility_test, solubility, prediction, case_weights = weights)[[".estimate"]],
    3.401406885440771965534
  )
})
