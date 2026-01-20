test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  delta <- 2
  a <- ex_dat$obs - ex_dat$pred
  exp <- mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))

  expect_equal(
    huber_loss_pseudo_vec(
      truth = ex_dat$obs,
      estimate = ex_dat$pred,
      delta = delta
    ),
    exp
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    huber_loss_pseudo_vec(ex_dat$obs, ex_dat$pred),
    huber_loss_pseudo(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  delta <- 2
  a <- ex_dat$obs[-na_ind] - ex_dat$pred[-na_ind]
  exp <- mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))

  expect_identical(
    huber_loss_pseudo_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )
  expect_equal(
    huber_loss_pseudo_vec(
      truth = ex_dat$obs,
      estimate = ex_dat$pred,
      delta = delta
    ),
    exp
  )
})

test_that("Case weights calculations are correct", {
  truth <- c(1, 2, 3)
  estimate <- c(2, 4, 3)
  weights <- c(1, 2, 1)

  expect_identical(
    huber_loss_pseudo_vec(truth, estimate, case_weights = weights),
    yardstick_mean(sqrt(1 + (truth - estimate)^2) - 1, case_weights = weights)
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    huber_loss_pseudo_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    huber_loss_pseudo_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    huber_loss_pseudo_vec(1, 1, na_rm = "yes")
  )
})

test_that("bad argument check", {
  expect_snapshot(
    error = TRUE,
    huber_loss_pseudo_vec(1, 1, delta = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(huber_loss_pseudo)
  range <- metric_range(huber_loss_pseudo)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )
  df$estimate <- df$truth
  df$off <- df$truth + 1

  expect_equal(
    huber_loss_pseudo_vec(df$truth, df$estimate),
    perfect
  )

  if (direction == "minimize") {
    expect_gt(huber_loss_pseudo_vec(df$truth, df$off), perfect)
    expect_lte(huber_loss_pseudo_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(huber_loss_pseudo_vec(df$truth, df$off), perfect)
    expect_gte(huber_loss_pseudo_vec(df$truth, df$off), worst)
  }
})
