test_that("`mpe()` works", {
  set.seed(1812)
  df <- data.frame(obs = rnorm(50))
  df$pred <- .2 + 1.1 * df$obs + rnorm(50, sd = 0.5)

  expect_identical(
    mpe(df, truth = "obs", estimate = "pred")[[".estimate"]],
    mean((df$obs - df$pred) / df$obs) * 100
  )

  ind <- c(10, 20, 30, 40, 50)
  df$pred[ind] <- NA

  expect_identical(
    mpe(df, obs, pred)[[".estimate"]],
    mean((df$obs[-ind] - df$pred[-ind]) / df$obs[-ind]) * 100
  )
})

test_that("`mpe()` computes expected values when singular `truth` is `0`", {
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

test_that("Weighted results are working", {
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
