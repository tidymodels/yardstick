test_that("can create metric functions", {
  class_metric <- new_class_metric(
    function() 1,
    "maximize"
  )
  prob_metric <- new_prob_metric(
    function() 1,
    "maximize"
  )
  numeric_metric <- new_numeric_metric(
    function() 1,
    "minimize"
  )
  numeric_zero_metric <- new_numeric_metric(
    function() 1,
    "zero"
  )
  dynamic_survival_metric <- new_dynamic_survival_metric(
    function() 1,
    "minimize"
  )
  static_survival_metric <- new_static_survival_metric(
    function() 1,
    "minimize"
  )
  integrated_survival_metric <- new_integrated_survival_metric(
    function() 1,
    "minimize"
  )
  linear_pred_survival_metric <- new_linear_pred_survival_metric(
    function() 1,
    "minimize"
  )
  quantile_metric <- new_quantile_metric(
    function() 1,
    "minimize"
  )

  expect_identical(
    class(class_metric),
    c("class_metric", "metric", "function")
  )
  expect_identical(
    class(prob_metric),
    c("prob_metric", "metric", "function")
  )
  expect_identical(
    class(numeric_metric),
    c("numeric_metric", "metric", "function")
  )
  expect_identical(
    class(numeric_zero_metric),
    c("numeric_metric", "metric", "function")
  )
  expect_identical(
    class(dynamic_survival_metric),
    c("dynamic_survival_metric", "metric", "function")
  )
  expect_identical(
    class(static_survival_metric),
    c("static_survival_metric", "metric", "function")
  )
  expect_identical(
    class(linear_pred_survival_metric),
    c("linear_pred_survival_metric", "metric", "function")
  )
  expect_identical(
    class(integrated_survival_metric),
    c("integrated_survival_metric", "metric", "function")
  )
  expect_identical(
    class(quantile_metric),
    c("quantile_metric", "metric", "function")
  )

  expect_identical(attr(class_metric, "direction"), "maximize")
  expect_identical(attr(prob_metric, "direction"), "maximize")
  expect_identical(attr(numeric_metric, "direction"), "minimize")
  expect_identical(attr(numeric_zero_metric, "direction"), "zero")
  expect_identical(attr(dynamic_survival_metric, "direction"), "minimize")
  expect_identical(attr(static_survival_metric, "direction"), "minimize")
  expect_identical(attr(integrated_survival_metric, "direction"), "minimize")
  expect_identical(attr(quantile_metric, "direction"), "minimize")
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

test_that("range argument works", {
  metric_with_range <- new_numeric_metric(
    function() 1,
    "minimize",
    range = c(0, 1)
  )

  expect_identical(metric_range(metric_with_range), c(0, 1))
  expect_null(metric_range(rmse))
})

test_that("range argument is not required", {
  metric_without_range <- new_numeric_metric(
    function() 1,
    "minimize"
  )

  expect_null(metric_range(metric_without_range))
})

test_that("`range` argument is validated", {
  expect_snapshot(
    error = TRUE,
    new_numeric_metric(function() 1, "minimize", range = "bad")
  )
  expect_snapshot(
    error = TRUE,
    new_numeric_metric(function() 1, "minimize", range = c(1, 0))
  )
  expect_snapshot(
    error = TRUE,
    new_numeric_metric(function() 1, "minimize", range = 1)
  )
})
