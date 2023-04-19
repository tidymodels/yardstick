test_that("mean absolute error", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    mae(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    mean(abs(ex_dat$obs - ex_dat$pred))
  )
  expect_equal(
    mae(ex_dat, obs, pred_na)[[".estimate"]],
    mean(abs(ex_dat$obs[not_na] - ex_dat$pred[not_na]))
  )
})

test_that("Weighted results are the same as scikit-learn", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    mae(solubility_test, solubility, prediction, case_weights = weights)[[".estimate"]],
    read_pydata("py-mae")$case_weight
  )
})
