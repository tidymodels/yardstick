test_that("Pseudo-Huber Loss", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  delta <- 2
  expect_equal(
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred", delta = delta)[[
      ".estimate"
    ]],
    {
      a <- ex_dat$obs - ex_dat$pred
      mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))
    }
  )
  expect_equal(
    huber_loss_pseudo(
      ex_dat,
      truth = "obs",
      estimate = "pred_na",
      delta = delta
    )[[".estimate"]],
    {
      a <- ex_dat$obs[not_na] - ex_dat$pred[not_na]
      mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))
    }
  )

  expect_snapshot(
    error = TRUE,
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = -1)
  )

  expect_snapshot(
    error = TRUE,
    huber_loss_pseudo(
      ex_dat,
      truth = "obs",
      estimate = "pred_na",
      delta = c(1, 2)
    )
  )
})

test_that("Weighted results are working", {
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
