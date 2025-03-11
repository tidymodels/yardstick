test_that("can create metric functions", {
  fn1 <- new_class_metric(function() 1, "maximize")
  fn2 <- new_prob_metric(function() 1, "maximize")
  fn3 <- new_numeric_metric(function() 1, "minimize")
  fn3zero <- new_numeric_metric(function() 1, "zero")
  fn4 <- new_dynamic_survival_metric(function() 1, "minimize")
  fn5 <- new_static_survival_metric(function() 1, "minimize")
  fn6 <- new_integrated_survival_metric(function() 1, "minimize")

  expect_identical(class(fn1), c("class_metric", "metric", "function"))
  expect_identical(class(fn2), c("prob_metric", "metric", "function"))
  expect_identical(class(fn3), c("numeric_metric", "metric", "function"))
  expect_identical(class(fn3zero), c("numeric_metric", "metric", "function"))
  expect_identical(
    class(fn4),
    c("dynamic_survival_metric", "metric", "function")
  )
  expect_identical(
    class(fn5),
    c("static_survival_metric", "metric", "function")
  )
  expect_identical(
    class(fn6),
    c("integrated_survival_metric", "metric", "function")
  )

  expect_identical(attr(fn1, "direction"), "maximize")
  expect_identical(attr(fn2, "direction"), "maximize")
  expect_identical(attr(fn3, "direction"), "minimize")
  expect_identical(attr(fn3zero, "direction"), "zero")
  expect_identical(attr(fn4, "direction"), "minimize")
  expect_identical(attr(fn5, "direction"), "minimize")
  expect_identical(attr(fn6, "direction"), "minimize")
})

test_that("`fn` is validated", {
  expect_snapshot(
    error = TRUE,
    new_class_metric(1, "maximize")
  )
})

test_that("`direction` is validated", {
  expect_snapshot(
    error = TRUE,
    new_class_metric(function() 1, "min")
  )
})

test_that("metric print method works", {
  expect_snapshot(rmse)
  expect_snapshot(roc_auc)
  expect_snapshot(demographic_parity(boop))
})
