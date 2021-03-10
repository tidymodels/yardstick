test_that("`me()` works", {
  set.seed(1812)
  df <- data.frame(obs = rnorm(50))
  df$pred <- .2 + 1.1 * df$obs + rnorm(50, sd = 0.5)

  expect_identical(
    me(df, truth = "obs", estimate = "pred")[[".estimate"]],
    mean((df$obs - df$pred))
  )

  # adding some NA values and check that they are ignored
  ind <- c(10, 20, 30, 40, 50)
  df$pred[ind] <- NA

  expect_identical(
    me(df, obs, pred)[[".estimate"]],
    mean((df$obs[-ind] - df$pred[-ind])),
    'me() should ignore NA values by default.'
  )
})
