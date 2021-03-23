test_that("`msd()` works", {
  set.seed(1812)
  df <- data.frame(obs = rnorm(50))
  df$pred <- .2 + 1.1 * df$obs + rnorm(50, sd = 0.5)

  expect_identical(
    msd(df, truth = "obs", estimate = "pred")[[".estimate"]],
    mean(df$pred - df$obs)
  )

  # adding some NA values and check that they are ignored
  ind <- c(10, 20, 30, 40, 50)
  df$pred[ind] <- NA

  expect_identical(
    msd(df, obs, pred)[[".estimate"]],
    mean(df$pred[-ind] - df$obs[-ind])
  )
})

test_that("positive and negative errors cancel each other out", {
  expect_identical(msd_vec(c(100, -100), c(0, 0)), 0)
})

test_that("differences are computed as `estimate - truth`", {
  expect_identical(msd_vec(1, 0), -1)
})
