test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    msd_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean(ex_dat$obs - ex_dat$pred)
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    msd_vec(ex_dat$obs, ex_dat$pred),
    msd(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    msd_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_identical(
    msd_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    mean(ex_dat$obs - ex_dat$pred, na.rm = TRUE)
  )
})

test_that("Case weights calculations are correct", {
  truth <- c(1, 2, 3)
  estimate <- c(1, 4, 4)
  weights <- c(0, 1, 2)

  expect_identical(
    msd_vec(truth, estimate, case_weights = weights),
    -4 / 3
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    msd_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    msd_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    msd_vec(1, 1, na_rm = "yes")
  )
})

test_that("msd() - positive and negative errors cancel each other out", {
  expect_identical(msd_vec(c(100, -100), c(0, 0)), 0)
})

test_that("msd() - differences are computed as `truth - estimate`", {
  expect_identical(msd_vec(0, 1), -1)
})
