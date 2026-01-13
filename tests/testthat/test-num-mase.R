test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  truth <- ex_dat$obs
  pred <- ex_dat$pred

  truth_lag <- dplyr::lag(truth, 1L)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-1])
  scaled_error <- (truth - pred) / mae_denom
  exp <- mean(abs(scaled_error))

  expect_equal(
    mase_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    exp
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    mase_vec(ex_dat$obs, ex_dat$pred),
    mase(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  truth <- ex_dat$obs[-na_ind]
  pred <- ex_dat$pred[-na_ind]

  truth_lag <- dplyr::lag(truth, 1L)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-1])
  scaled_error <- (truth - pred) / mae_denom
  exp <- mean(abs(scaled_error))

  expect_identical(
    mase_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    mase_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    exp
  )
})

test_that("Case weights calculations are correct", {
  truth <- c(1, 2, 3)
  estimate <- c(2, 4, 3)
  weights <- c(1, 2, 1)

  expect_identical(
    mase_vec(truth, estimate, case_weights = weights),
    5 / 4
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    mase_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    mase_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    mase_vec(1, 1, na_rm = "yes")
  )
})

test_that("bad argument check", {
  expect_snapshot(
    error = TRUE,
    mase_vec(1, 1, m = "yes")
  )
})

test_that("mase() - errors if m is larger than number of observations", {
  expect_snapshot(
    error = TRUE,
    mase(mtcars, mpg, disp, m = 100)
  )
})

test_that("mase() - m argument works", {
  ex_dat <- generate_numeric_test_data()

  truth <- ex_dat$obs
  pred <- ex_dat$pred

  m <- 2

  truth_lag <- dplyr::lag(truth, m)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-c(1, 2)])
  scaled_error <- (truth - pred) / mae_denom
  exp <- mean(abs(scaled_error))

  expect_equal(
    mase_vec(ex_dat$obs, ex_dat$pred, m = 2),
    exp
  )
})

test_that("mase() - mae_train argument works", {
  ex_dat <- generate_numeric_test_data()

  truth <- ex_dat$obs
  pred <- ex_dat$pred

  mae_train <- 0.5

  mae_denom <- mae_train
  scaled_error <- (truth - pred) / mae_denom
  exp <- mean(abs(scaled_error))

  expect_equal(
    mase_vec(ex_dat$obs, ex_dat$pred, mae_train = mae_train),
    exp
  )
})
