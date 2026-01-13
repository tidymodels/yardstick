test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  exp <- (ex_dat$obs - ex_dat$pred) / ((abs(ex_dat$obs) + abs(ex_dat$pred)) / 2)
  exp <- 100 * mean(abs(exp))

  expect_equal(
    smape_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    exp
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  exp <- (ex_dat$obs[-na_ind] - ex_dat$pred[-na_ind]) /
    ((abs(ex_dat$obs[-na_ind]) + abs(ex_dat$pred[-na_ind])) / 2)
  exp <- 100 * mean(abs(exp))

  expect_identical(
    smape_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    smape_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    exp
  )
})

test_that("Case weights calculations are correct", {
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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    smape_vec(1, 1, na_rm = "yes")
  )
})
