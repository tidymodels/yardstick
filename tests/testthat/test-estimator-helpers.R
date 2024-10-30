test_that("get_weights() errors with wrong estimator", {
  expect_snapshot(
    error = TRUE,
    get_weights(mtcars, "wrong")
  )
})
