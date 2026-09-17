test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    rpd_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    stats::sd(ex_dat$obs) / (sqrt(mean((ex_dat$obs - ex_dat$pred)^2)))
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    rpd_vec(ex_dat$obs, ex_dat$pred),
    rpd(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    rpd_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    rpd_vec(truth = ex_dat$obs, estimate = ex_dat$pred),
    stats::sd(ex_dat$obs[-na_ind]) /
      (sqrt(mean((ex_dat$obs[-na_ind] - ex_dat$pred[-na_ind])^2)))
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  sd <- yardstick_sd(
    solubility_test$solubility,
    case_weights = solubility_test$weights
  )
  rmse <- rmse_vec(
    solubility_test$solubility,
    solubility_test$prediction,
    case_weights = solubility_test$weights
  )
  exp <- sd / rmse

  expect_identical(
    rpd_vec(
      truth = solubility_test$solubility,
      estimate = solubility_test$prediction,
      case_weights = solubility_test$weights
    ),
    exp
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rpd_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    rpd_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    rpd_vec(1, 1, na_rm = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(rpd)
  range <- metric_range(rpd)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )

  df$estimate <- df$truth
  df$off <- df$truth + 1

  expect_identical(
    rpd_vec(df$truth, df$estimate),
    perfect
  )
  if (direction == "minimize") {
    expect_gt(rpd_vec(df$truth, df$off), perfect)
    expect_lt(rpd_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(rpd_vec(df$truth, df$off), perfect)
    expect_gt(rpd_vec(df$truth, df$off), worst)
  }
})
