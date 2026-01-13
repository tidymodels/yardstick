test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    mpe_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean((ex_dat$obs - ex_dat$pred) / ex_dat$obs) * 100
  )
})

test_that("Case weights calculations are correct", {
  truth <- c(1, 2, 3)
  estimate <- c(2, 4, 3)
  weights <- c(1, 2, 1)

  expect_identical(
    mpe_vec(truth, estimate, case_weights = weights),
    -3 / 4 * 100
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    mpe_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    mpe_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    mpe_vec(1, 1, na_rm = "yes")
  )
})

test_that("mpe() - computes expected values when singular `truth` is `0`", {
  expect_identical(
    mpe_vec(truth = 0, estimate = 1),
    -Inf
  )

  expect_identical(
    mpe_vec(truth = 0, estimate = -1),
    Inf
  )

  expect_identical(
    mpe_vec(truth = 0, estimate = 0),
    NaN
  )
})
