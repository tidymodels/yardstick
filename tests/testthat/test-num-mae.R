test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    mae_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean(abs(ex_dat$obs - ex_dat$pred))
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    mae_vec(ex_dat$obs, ex_dat$pred),
    mae(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    mae_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    mae_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean(abs(ex_dat$obs - ex_dat$pred), na.rm = TRUE)
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  expect_equal(
    mae_vec(
      truth = solubility_test$solubility,
      estimate = solubility_test$prediction,
      case_weights = solubility_test$weights
    ),
    read_pydata("py-mae")$case_weight
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    mae_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    mae_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    mae_vec(1, 1, na_rm = "yes")
  )
})
