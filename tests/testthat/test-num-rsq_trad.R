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

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    rsq_trad_vec(ex_dat$obs, ex_dat$pred),
    rsq_trad(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  exp <- 1 -
    (sum((ex_dat$obs[-na_ind] - ex_dat$pred[-na_ind])^2) /
      sum((ex_dat$obs[-na_ind] - mean(ex_dat$obs[-na_ind]))^2))

  expect_identical(
    rsq_trad_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

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

test_that("range values are correct", {
  direction <- metric_direction(rsq_trad)
  range <- metric_range(rsq_trad)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )

  df$estimate <- df$truth
  df$off <- df$truth + 1

  expect_identical(
    rsq_trad_vec(df$truth, df$estimate),
    perfect
  )
  if (direction == "minimize") {
    expect_gt(rsq_trad_vec(df$truth, df$off), perfect)
    expect_lt(rsq_trad_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(rsq_trad_vec(df$truth, df$off), perfect)
    expect_gt(rsq_trad_vec(df$truth, df$off), worst)
  }
})
