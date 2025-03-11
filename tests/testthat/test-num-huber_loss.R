test_that("Huber Loss", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  delta <- 2

  expect_equal(
    huber_loss(ex_dat, truth = "obs", estimate = "pred", delta = delta)[[
      ".estimate"
    ]],
    {
      a <- ex_dat$obs - ex_dat$pred
      mean(
        ifelse(abs(a) <= delta, 0.5 * a^2, delta * (abs(a) - 0.5 * delta))
      )
    }
  )

  expect_equal(
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = delta)[[
      ".estimate"
    ]],
    {
      a <- ex_dat$obs[not_na] - ex_dat$pred[not_na]
      mean(
        ifelse(abs(a) <= delta, 0.5 * a^2, delta * (abs(a) - 0.5 * delta))
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -1)
  )

  expect_snapshot(
    error = TRUE,
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1, 2))
  )
})

test_that("Weighted results are working", {
  truth <- c(1, 2, 3)
  estimate <- c(2, 4, 3)
  weights <- c(1, 2, 1)

  expect_identical(
    huber_loss_vec(truth, estimate, case_weights = weights),
    3.5 / 4
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    huber_loss_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    huber_loss_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})
