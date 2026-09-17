test_that("get_metrics() returns metric_set", {
  res <- get_metrics("numeric")

  expect_s3_class(res, c("numeric_metric_set", "metric_set"))

  metrics <- attr(res, "metrics")
  expect_gt(length(metrics), 0)

  expect_snapshot(res)
})

test_that("get_metrics() works with multiple compatible types", {
  res <- get_metrics(c("class", "prob"))

  expect_s3_class(res, c("class_prob_metric_set", "metric_set"))

  metrics <- attr(res, "metrics")
  class_metrics <- get_metrics("class")
  prob_metrics <- get_metrics("prob")

  expect_equal(
    length(metrics),
    length(attr(class_metrics, "metrics")) +
      length(attr(prob_metrics, "metrics"))
  )
})

test_that("get_metrics() errors on empty input", {
  expect_snapshot(error = TRUE, get_metrics())
})

test_that("get_metrics() errors on invalid type", {
  expect_snapshot(error = TRUE, get_metrics("invalid"))
})

test_that("get_metrics() errors on incompatible metric types", {
  expect_snapshot(error = TRUE, get_metrics(c("class", "numeric")))
})
