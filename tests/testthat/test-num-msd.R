test_that("`msd()` works", {
  set.seed(1812)
  df <- data.frame(obs = rnorm(50))
  df$pred <- .2 + 1.1 * df$obs + rnorm(50, sd = 0.5)

  expect_identical(
    msd(df, truth = "obs", estimate = "pred")[[".estimate"]],
    mean(df$obs - df$pred)
  )

  # adding some NA values and check that they are ignored
  ind <- c(10, 20, 30, 40, 50)
  df$pred[ind] <- NA

  expect_identical(
    msd(df, obs, pred)[[".estimate"]],
    mean(df$obs[-ind] - df$pred[-ind])
  )
})

test_that("positive and negative errors cancel each other out", {
  expect_identical(msd_vec(c(100, -100), c(0, 0)), 0)
})

test_that("differences are computed as `truth - estimate`", {
  expect_identical(msd_vec(0, 1), -1)
})

test_that("weighted results are correct", {
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
