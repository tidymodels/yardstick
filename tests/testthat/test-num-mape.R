test_that('Mean Absolute Percentage Error', {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    mape(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    100 * mean(abs((ex_dat$obs - ex_dat$pred)/ex_dat$obs))
  )
  expect_equal(
    mape(ex_dat, obs, pred_na)[[".estimate"]],
    100 * mean(abs((ex_dat$obs[not_na] - ex_dat$pred[not_na])/ex_dat$obs[not_na]))
  )
})

test_that("`0` values for `truth` don't result in infinity (#271)", {
  expect_identical(
    mape_vec(truth = c(1, 0), estimate = c(2, 0)),
    50
  )
})

test_that("Weighted results are the same as scikit-learn", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    mape(solubility_test, solubility, prediction, case_weights = weights)[[".estimate"]],
    read_pydata("py-mape")$case_weight * 100
  )
})
