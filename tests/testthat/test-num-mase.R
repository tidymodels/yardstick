test_that("Mean Absolute Scaled Error", {
  ex_dat <- generate_numeric_test_data()

  truth <- ex_dat$obs
  pred <- ex_dat$pred

  truth_lag <- dplyr::lag(truth, 1L)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-1])
  scaled_error <- (truth - pred) / mae_denom
  known_mase <- mean(abs(scaled_error))

  m <- 2

  truth_lag <- dplyr::lag(truth, m)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-c(1, 2)])
  scaled_error <- (truth - pred) / mae_denom
  known_mase_with_m <- mean(abs(scaled_error))

  mae_train <- .5

  mae_denom <- mae_train
  scaled_error <- (truth - pred) / mae_denom
  known_mase_with_mae_train <- mean(abs(scaled_error))

  expect_equal(
    mase(ex_dat, obs, pred)[[".estimate"]],
    known_mase
  )

  expect_equal(
    mase(ex_dat, obs, pred, m = 2)[[".estimate"]],
    known_mase_with_m
  )

  expect_equal(
    mase(ex_dat, obs, pred, mae_train = mae_train)[[".estimate"]],
    known_mase_with_mae_train
  )

  expect_snapshot(
    error = TRUE,
    mase_vec(truth, pred, m = "x")
  )

  expect_snapshot(
    error = TRUE,
    mase_vec(truth, pred, m = -1)
  )

  expect_snapshot(
    error = TRUE,
    mase_vec(truth, pred, m = 1.5)
  )

  expect_snapshot(
    error = TRUE,
    mase_vec(truth, pred, mae_train = -1)
  )

  expect_snapshot(
    error = TRUE,
    mase_vec(truth, pred, mae_train = "x")
  )
})

test_that("Weighted results are working", {
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

test_that("mase() errors if m is larger than number of observations", {
  expect_snapshot(
    error = TRUE,
    mase(mtcars, mpg, disp, m = 100)
  )
})
