test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  exp <- 1 -
    (sum((ex_dat$obs - ex_dat$pred)^2) /
      sum((ex_dat$obs - mean(ex_dat$obs))^2))

  expect_equal(
    rsq_trad_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    exp
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    rsq_trad_vec(
      truth = solubility_test$solubility,
      estimate = solubility_test$prediction,
      case_weights = solubility_test$weights
    ),
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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    rsq_trad_vec(1, 1, na_rm = "yes")
  )
})
