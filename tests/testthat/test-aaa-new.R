test_that("can create metric functions", {
  fn1 <- new_class_metric(function() 1, "maximize")
  fn2 <- new_prob_metric(function() 1, "maximize")
  fn3 <- new_numeric_metric(function() 1, "minimize")
  fn3zero <- new_numeric_metric(function() 1, "zero")

  expect_identical(class(fn1), c("class_metric", "metric", "function"))
  expect_identical(class(fn2), c("prob_metric", "metric", "function"))
  expect_identical(class(fn3), c("numeric_metric", "metric", "function"))
  expect_identical(class(fn3zero), c("numeric_metric", "metric", "function"))

  expect_identical(attr(fn1, "direction"), "maximize")
  expect_identical(attr(fn2, "direction"), "maximize")
  expect_identical(attr(fn3, "direction"), "minimize")
  expect_identical(attr(fn3zero, "direction"), "zero")
})

test_that("`fn` is validated", {
  expect_error(new_class_metric(1, "maximize"), "must be a function")
})

test_that("`direction` is validated", {
  expect_error(new_class_metric(function() 1, "min"))
})
