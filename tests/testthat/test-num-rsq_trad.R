test_that("Traditional R^2", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    1 -
      (sum((ex_dat$obs - ex_dat$pred)^2) /
        sum((ex_dat$obs - mean(ex_dat$obs))^2))
  )
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    1 -
      (sum((ex_dat$obs[not_na] - ex_dat$pred[not_na])^2) /
        sum((ex_dat$obs[not_na] - mean(ex_dat$obs[not_na]))^2))
  )
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = rand)[[".estimate"]],
    1 -
      (sum((ex_dat$obs - ex_dat$rand)^2) /
        sum((ex_dat$obs - mean(ex_dat$obs))^2))
  )
  expect_equal(
    rsq_trad(ex_dat, obs, rand_na)[[".estimate"]],
    1 -
      (sum((ex_dat$obs[not_na] - ex_dat$rand[not_na])^2) /
        sum((ex_dat$obs[not_na] - mean(ex_dat$obs[not_na]))^2))
  )
})

test_that("Weighted results are the same as scikit-learn", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    rsq_trad(solubility_test, solubility, prediction, case_weights = weights)[[
      ".estimate"
    ]],
    read_pydata("py-rsq-trad")$case_weight
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rsq_trad_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    rsq_trad_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})
